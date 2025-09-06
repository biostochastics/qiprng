// File: ziggurat_normal.cpp
// --------------------------------------------------------------
#include "ziggurat_normal.hpp"

#include <atomic>      // For debugging counters if re-enabled
#include <functional>  // For std::function
#include <random>      // For std::mt19937, std::normal_distribution
#include <stdexcept>   // For std::runtime_error

// Set to true to enable Rcpp::Rcout debug messages
const bool ZIGGURAT_DEBUG_LOGGING = false;

namespace qiprng {

// Thread cleanup helper class - automatically cleans up TLS resources when thread exits
class ThreadCleanupHelper {
   private:
    // Use std::once_flag for thread-safe cleanup registration
    static std::once_flag cleanup_registration_flag_;
    // Track if this specific thread has performed cleanup
    std::atomic<bool> thread_cleanup_done_{false};

   public:
    ThreadCleanupHelper() = default;
    ~ThreadCleanupHelper() {
        // Use atomic compare-exchange to ensure single cleanup per thread
        bool expected = false;
        if (thread_cleanup_done_.compare_exchange_strong(expected, true,
                                                         std::memory_order_acq_rel)) {
            // Check global cleanup flag with proper memory ordering
            if (!ZigguratNormal::is_cleanup_in_progress()) {
                // Perform thread-local cleanup with proper synchronization
                ZigguratNormal::cleanup_thread_local_resources();
            }
        }
    }

    // Register cleanup for this thread (called once per thread)
    static void register_cleanup() {
        std::call_once(cleanup_registration_flag_, []() {
            // Any one-time global cleanup registration can go here
            // Currently just a placeholder for future use
        });
    }
};

// Initialize static member
std::once_flag ThreadCleanupHelper::cleanup_registration_flag_;

// Initialize static members
std::array<double, ZigguratNormal::ZIGGURAT_TABLE_SIZE> ZigguratNormal::cached_x_table_;
std::array<double, ZigguratNormal::ZIGGURAT_TABLE_SIZE> ZigguratNormal::cached_y_table_;
std::array<uint32_t, ZigguratNormal::ZIGGURAT_TABLE_SIZE> ZigguratNormal::cached_k_table_;
std::once_flag ZigguratNormal::tables_init_flag_;

// Initialize cleanup flag
std::atomic<bool> ZigguratNormal::cleanup_in_progress_(false);

// Initialize thread-local static members
thread_local std::array<double, ZigguratNormal::ZIGGURAT_TABLE_SIZE> ZigguratNormal::tls_x_table_;
thread_local std::array<double, ZigguratNormal::ZIGGURAT_TABLE_SIZE> ZigguratNormal::tls_y_table_;
thread_local std::array<uint32_t, ZigguratNormal::ZIGGURAT_TABLE_SIZE> ZigguratNormal::tls_k_table_;
thread_local bool ZigguratNormal::tls_tables_initialized_ = false;

// Initialize thread-local random cache members
thread_local std::array<double, ZigguratNormal::RANDOM_CACHE_SIZE>
    ZigguratNormal::tls_random_cache_;
thread_local size_t ZigguratNormal::tls_random_cache_pos_ =
    ZigguratNormal::RANDOM_CACHE_SIZE;  // Start empty
thread_local bool ZigguratNormal::tls_random_cache_initialized_ = false;

// Initialize thread-local manager
thread_local ZigguratTLSManager* ZigguratNormal::tls_manager_ = nullptr;

// Initialize thread-local cleanup registration flag
thread_local std::atomic<bool> ZigguratNormal::tls_cleanup_registered_(false);

// Initialize thread-local exiting flag for ZigguratTLSManager
thread_local bool ZigguratTLSManager::thread_exiting_ = false;

void ZigguratNormal::initialize_static_tables() {
    // Use std::call_once for thread-safe one-time initialization
    // This eliminates the double-checked locking antipattern and provides proper memory
    // synchronization
    std::call_once(tables_init_flag_, []() {
        // Constants for Ziggurat tables (Marsaglia and Tsang 2000)
        const double R_zig = 3.6541528853610088;    // x_0, rightmost boundary of rectangles
        const double V_zig = 0.004928673233992336;  // Area under tail R_zig to infinity

        // k_table_[0] is R_zig * pdf(R_zig) / V_zig scaled to uint32
        // x_table_[0] is R_zig
        // y_table_[0] is pdf(R_zig)
        double f = std::exp(-0.5 * R_zig * R_zig);
        cached_k_table_[0] =
            static_cast<uint32_t>((R_zig * f / V_zig) * static_cast<double>(UINT32_MAX));
        cached_x_table_[0] = R_zig;
        cached_y_table_[0] = f;

        // For i = 1 to ZIGGURAT_TABLE_SIZE - 2
        for (int i = 1; i < ZIGGURAT_TABLE_SIZE - 1; ++i) {
            double prev_x = cached_x_table_[i - 1];
            double prev_f = cached_y_table_[i - 1];  // pdf(prev_x)

            cached_x_table_[i] = std::sqrt(-2.0 * std::log(V_zig / prev_x + prev_f));
            cached_y_table_[i] = std::exp(-0.5 * cached_x_table_[i] * cached_x_table_[i]);
            cached_k_table_[i] = static_cast<uint32_t>(
                (cached_x_table_[i - 1] / cached_x_table_[i]) * static_cast<double>(UINT32_MAX));
        }

        // Special case for the last rectangle
        cached_k_table_[ZIGGURAT_TABLE_SIZE - 1] =
            UINT32_MAX;  // Always accept for the base segment
        cached_x_table_[ZIGGURAT_TABLE_SIZE - 1] = 0.0;
        cached_y_table_[ZIGGURAT_TABLE_SIZE - 1] = 1.0;  // pdf(0)

        // std::call_once automatically provides proper memory synchronization
        // All writes in this lambda are guaranteed to be visible to other threads
    });
}

void ZigguratNormal::initialize_tables_original() {
    // If standard normal parameters, use cached tables
    if (mean_ == 0.0 && stddev_ == 1.0) {
        use_cached_tables();
        return;
    }

    // If custom parameters, initialize tables directly in instance
    // Constants for Ziggurat tables (Marsaglia and Tsang 2000)
    const double R_zig = 3.6541528853610088;    // x_0, rightmost boundary of rectangles
    const double V_zig = 0.004928673233992336;  // Area under tail R_zig to infinity

    // k_table_[0] is R_zig * pdf(R_zig) / V_zig scaled to uint32
    // x_table_[0] is R_zig
    // y_table_[0] is pdf(R_zig)
    double f = std::exp(-0.5 * R_zig * R_zig);
    k_table_[0] = static_cast<uint32_t>((R_zig * f / V_zig) * static_cast<double>(UINT32_MAX));
    x_table_[0] = R_zig;
    y_table_[0] = f;

    // For i = 1 to ZIGGURAT_TABLE_SIZE - 2
    for (int i = 1; i < ZIGGURAT_TABLE_SIZE - 1; ++i) {
        // x_i = sqrt(-2 * log(V/x_{i-1} + f_{i-1}))
        double prev_x = x_table_[i - 1];
        double prev_f = y_table_[i - 1];  // pdf(prev_x)

        x_table_[i] = std::sqrt(-2.0 * std::log(V_zig / prev_x + prev_f));
        y_table_[i] = std::exp(-0.5 * x_table_[i] * x_table_[i]);
        k_table_[i] = static_cast<uint32_t>((x_table_[i - 1] / x_table_[i]) *
                                            static_cast<double>(UINT32_MAX));
    }

    // Special case for the last rectangle
    k_table_[ZIGGURAT_TABLE_SIZE - 1] = UINT32_MAX;  // Always accept for the base segment
    x_table_[ZIGGURAT_TABLE_SIZE - 1] = 0.0;
    y_table_[ZIGGURAT_TABLE_SIZE - 1] = 1.0;  // pdf(0)
}

void ZigguratNormal::use_cached_tables() {
    // Ensure the static tables are initialized
    initialize_static_tables();

    // Copy from cached tables
    x_table_ = cached_x_table_;
    y_table_ = cached_y_table_;
    k_table_ = cached_k_table_;
}

void ZigguratNormal::initialize_thread_local_tables() {
    // Skip if cleanup is in progress - acquire ordering already provides necessary synchronization
    if (cleanup_in_progress_.load(std::memory_order_acquire)) {
        return;
    }

    // Register cleanup helper for this thread using proper synchronization
    if (!tls_cleanup_registered_.load(std::memory_order_acquire)) {
        // Ensure the cleanup helper is created exactly once per thread
        static thread_local ThreadCleanupHelper cleanup_helper;
        // Register with the helper's static registration system
        ThreadCleanupHelper::register_cleanup();
        // Set flag with release ordering to ensure visibility
        tls_cleanup_registered_.store(true, std::memory_order_release);
        // Add fence to ensure all writes are visible
        std::atomic_thread_fence(std::memory_order_release);
    }

    // Setup TLS manager if needed
    if (!tls_manager_) {
        tls_manager_ = &ZigguratTLSManager::instance();
        tls_manager_->set_owner(this);
    }

    // Initialize tables if not already done
    if (!tls_tables_initialized_ && tls_manager_->is_valid()) {
        try {
            // Verify we're not in cleanup before copying
            if (cleanup_in_progress_.load(std::memory_order_acquire)) {
                return;
            }

            // Copy the data
            tls_x_table_ = x_table_;
            tls_y_table_ = y_table_;
            tls_k_table_ = k_table_;
            tls_tables_initialized_ = true;

            if (ZIGGURAT_DEBUG_LOGGING) {
                Rcpp::Rcout << "Thread " << std::this_thread::get_id() << " initialized TLS tables"
                            << std::endl;
            }
        } catch (...) {
            if (ZIGGURAT_DEBUG_LOGGING) {
                Rcpp::Rcout << "Exception during TLS table initialization" << std::endl;
            }
            // Leave tables uninitialized on exception - will retry on next call
        }
    }
}

void ZigguratNormal::initialize_random_cache() {
    // Skip if cleanup is in progress - acquire ordering already provides necessary synchronization
    if (cleanup_in_progress_.load(std::memory_order_acquire)) {
        return;
    }

    // Register cleanup helper for this thread using proper synchronization
    if (!tls_cleanup_registered_.load(std::memory_order_acquire)) {
        // Ensure the cleanup helper is created exactly once per thread
        static thread_local ThreadCleanupHelper cleanup_helper;
        // Register with the helper's static registration system
        ThreadCleanupHelper::register_cleanup();
        // Set flag with release ordering to ensure visibility
        tls_cleanup_registered_.store(true, std::memory_order_release);
        // Add fence to ensure all writes are visible
        std::atomic_thread_fence(std::memory_order_release);
    }

    // Setup TLS manager if needed (may not be done if initialize_thread_local_tables wasn't called
    // first)
    if (!tls_manager_) {
        tls_manager_ = &ZigguratTLSManager::instance();
        tls_manager_->set_owner(this);
    }

    // Only proceed if manager is valid
    if (tls_manager_->is_valid() && !tls_random_cache_initialized_) {
        try {
            // Verify we're not in cleanup before initializing
            if (cleanup_in_progress_.load(std::memory_order_acquire)) {
                return;
            }

            // Initialize the cache
            refill_random_cache();
            tls_random_cache_initialized_ = true;

            if (ZIGGURAT_DEBUG_LOGGING) {
                Rcpp::Rcout << "Thread " << std::this_thread::get_id()
                            << " initialized random cache" << std::endl;
            }
        } catch (...) {
            if (ZIGGURAT_DEBUG_LOGGING) {
                Rcpp::Rcout << "Exception during random cache initialization" << std::endl;
            }
            // Leave cache uninitialized on exception - will retry on next call
        }
    }
}

void ZigguratNormal::refill_random_cache() {
    // Use mutex to get a batch of random numbers at once
    {
        std::lock_guard<std::mutex> lock(instance_mutex_);
        for (size_t i = 0; i < RANDOM_CACHE_SIZE; ++i) {
            try {
                double u = uniform_generator_();
                // Ensure value is in valid range
                if (u <= 0.0 || u >= 1.0) {
                    u = 0.5;  // Use a safe default
                }
                tls_random_cache_[i] = u;
            } catch (...) {
                // In case of error, use a default value
                tls_random_cache_[i] = 0.5;
            }
        }
    }
    // Reset cache position
    tls_random_cache_pos_ = 0;
}

double ZigguratNormal::get_cached_uniform() {
    // Skip if cleanup is in progress - return a safe value
    if (cleanup_in_progress_.load(std::memory_order_acquire)) {
        return 0.5;  // Safe fallback value
    }

    // Check TLS manager
    if (!tls_manager_ || !tls_manager_->is_valid()) {
        // TLS manager not initialized or invalid - try to initialize
        initialize_thread_local_tables();

        if (!tls_manager_ || !tls_manager_->is_valid()) {
            return 0.5;  // Safe fallback value if initialization fails
        }
    }

    // Initialize cache if needed
    if (!tls_random_cache_initialized_) {
        initialize_random_cache();

        // Double-check initialization success
        if (!tls_random_cache_initialized_) {
            return 0.5;  // Safe fallback value if initialization fails
        }
    }

    // Refill cache if empty
    if (tls_random_cache_pos_ >= RANDOM_CACHE_SIZE) {
        try {
            refill_random_cache();
        } catch (...) {
            // If refill fails, return a safe value
            return 0.5;
        }
    }

    // Get the next value from the cache with bounds check
    if (tls_random_cache_pos_ < RANDOM_CACHE_SIZE) {
        return tls_random_cache_[tls_random_cache_pos_++];
    } else {
        // This shouldn't happen, but just in case
        return 0.5;
    }
}

double ZigguratNormal::sample_from_tail_thread_safe() {
    double x, y;

    if (ZIGGURAT_DEBUG_LOGGING)
        Rcpp::Rcout << "[Ziggurat Tail TS] Enter. R_zig=" << tls_x_table_[0] << std::endl;

    // Maximum number of attempts to avoid infinite loops
    const int MAX_ATTEMPTS = 100;
    int attempts = 0;

    do {
        attempts++;
        if (attempts > MAX_ATTEMPTS) {
            throw std::runtime_error(
                "ZigguratNormal::sample_from_tail_thread_safe exceeded maximum attempts");
        }

        try {
            // Sample x from exp distribution with rate R_zig, shifted by R_zig
            // x = R_zig + E / R_zig where E ~ Exp(1)
            // E = -log(U1) where U1 ~ Uniform(0,1)
            double u1 = get_cached_uniform();  // Use cached uniform value

            // Calculate exponential value using safe u1
            x = -std::log(u1) / tls_x_table_[0];  // This is E/R_zig

            // Sample y from Exp(1)
            double u2 = get_cached_uniform();  // Use cached uniform value

            // Calculate second exponential value
            y = -std::log(u2);

            // Diagnostic output
            if (ZIGGURAT_DEBUG_LOGGING) {
                Rcpp::Rcout << "[Ziggurat Tail TS] u1=" << u1 << ", u2=" << u2
                            << ", x_exp_part=" << x << ", y_exp=" << y << ", test: 2*y (" << 2 * y
                            << ") vs x*x (" << x * x << ")" << std::endl;
            }

            // Break out of the loop if we've found a valid pair (accept condition)
            if (y + y >= x * x) {
                break;
            }

        } catch (const std::exception& e) {
            if (ZIGGURAT_DEBUG_LOGGING) {
                Rcpp::Rcout << "[Ziggurat Tail TS] Exception: " << e.what() << ". Retrying."
                            << std::endl;
            }
            // In case of exceptions, just retry
        } catch (...) {
            if (ZIGGURAT_DEBUG_LOGGING) {
                Rcpp::Rcout << "[Ziggurat Tail TS] Unknown exception. Retrying." << std::endl;
            }
            // In case of exceptions, just retry
        }

    } while (true);  // Continue until we break out with valid values

    // Calculate final result (R_zig + exponential part)
    double result = tls_x_table_[0] + x;

    if (ZIGGURAT_DEBUG_LOGGING) {
        Rcpp::Rcout << "[Ziggurat Tail TS] Exit. Returning " << result << std::endl;
    }

    return result;
}

double ZigguratNormal::sample_from_tail_original() {
    double x, y;

    if (ZIGGURAT_DEBUG_LOGGING)
        Rcpp::Rcout << "[Ziggurat Tail] Enter. R_zig=" << x_table_[0] << std::endl;

    // Maximum number of attempts to avoid infinite loops
    const int MAX_ATTEMPTS = 100;
    int attempts = 0;

    do {
        attempts++;
        if (attempts > MAX_ATTEMPTS) {
            throw std::runtime_error(
                "ZigguratNormal::sample_from_tail_original exceeded maximum attempts");
        }

        try {
            // Sample x from exp distribution with rate R_zig, shifted by R_zig
            // x = R_zig + E / R_zig where E ~ Exp(1)
            // E = -log(U1) where U1 ~ Uniform(0,1)
            double u1 = uniform_generator_();

            // Ensure u1 is valid for log operation
            int u1_attempts = 0;
            while (u1 <= 0.0 || u1 >= 1.0) {
                if (++u1_attempts > 10) {
                    // After several attempts, use a reasonable default
                    u1 = 0.5;
                    break;
                }
                try {
                    u1 = uniform_generator_();
                } catch (...) {
                    u1 = 0.5;  // Safe default
                    break;
                }
            }

            // Calculate exponential value using safe u1
            x = -std::log(u1) / x_table_[0];  // This is E/R_zig

            // Sample y from Exp(1)
            double u2 = uniform_generator_();

            // Ensure u2 is valid for log operation
            int u2_attempts = 0;
            while (u2 <= 0.0 || u2 >= 1.0) {
                if (++u2_attempts > 10) {
                    // After several attempts, use a reasonable default
                    u2 = 0.5;
                    break;
                }
                try {
                    u2 = uniform_generator_();
                } catch (...) {
                    u2 = 0.5;  // Safe default
                    break;
                }
            }

            // Calculate second exponential value
            y = -std::log(u2);

            // Diagnostic output
            if (ZIGGURAT_DEBUG_LOGGING) {
                Rcpp::Rcout << "[Ziggurat Tail] u1=" << u1 << ", u2=" << u2 << ", x_exp_part=" << x
                            << ", y_exp=" << y << ", test: 2*y (" << 2 * y << ") vs x*x (" << x * x
                            << ")" << std::endl;
            }

            // Break out of the loop if we've found a valid pair (accept condition)
            if (y + y >= x * x) {
                break;
            }

        } catch (const std::exception& e) {
            if (ZIGGURAT_DEBUG_LOGGING) {
                Rcpp::Rcout << "[Ziggurat Tail] Exception: " << e.what() << ". Retrying."
                            << std::endl;
            }
            // In case of exceptions, just retry
        } catch (...) {
            if (ZIGGURAT_DEBUG_LOGGING) {
                Rcpp::Rcout << "[Ziggurat Tail] Unknown exception. Retrying." << std::endl;
            }
            // In case of exceptions, just retry
        }

    } while (true);  // Continue until we break out with valid values

    // Calculate final result (R_zig + exponential part)
    double result = x_table_[0] + x;

    if (ZIGGURAT_DEBUG_LOGGING) {
        Rcpp::Rcout << "[Ziggurat Tail] Exit. Returning " << result << std::endl;
    }

    return result;
}

double ZigguratNormal::generate_internal() {
    if (ZIGGURAT_DEBUG_LOGGING)
        Rcpp::Rcout << "[Ziggurat Gen] Enter." << std::endl;

    uint32_t iz;    // Random integer from bits of uniform
    uint32_t idx;   // Index into tables
    double x_cand;  // Candidate x value

    // Maximum number of attempts to avoid infinite loops
    const int MAX_ATTEMPTS = 1000;
    int attempts = 0;

    while (attempts < MAX_ATTEMPTS) {
        attempts++;

        try {
            // Generate uniform u0 for sign and index
            // Guard against uniform_generator_ returning invalid values
            double u0 = uniform_generator_();
            if (u0 <= 0.0 || u0 >= 1.0) {
                u0 = 0.5;  // Use a safe default if generator fails
            }

            // Convert u0 to a 32-bit integer for table lookup
            iz = static_cast<uint32_t>(u0 * 4294967296.0);  // Multiply by 2^32
            idx = iz & ZIGGURAT_MASK;                       // Get index (0 to TABLE_SIZE-1)

            // Get random sign
            bool sign;
            try {
                double sign_u = uniform_generator_();
                sign = (sign_u < 0.5);
            } catch (...) {
                // If generator fails, use a deterministic approach based on iz
                sign = (iz & 0x80000000) != 0;  // Use top bit of iz
            }

            if (ZIGGURAT_DEBUG_LOGGING)
                Rcpp::Rcout << "[Ziggurat Gen] u0=" << u0 << ", iz=" << iz << ", idx=" << idx
                            << std::endl;

            // Handle tail region (idx == 0)
            if (idx == 0) {
                if (ZIGGURAT_DEBUG_LOGGING)
                    Rcpp::Rcout << "[Ziggurat Gen] idx=0 (tail)." << std::endl;

                try {
                    x_cand = sample_from_tail_original();
                    if (ZIGGURAT_DEBUG_LOGGING)
                        Rcpp::Rcout << "[Ziggurat Gen] Tail returned " << x_cand << std::endl;
                    return sign ? -x_cand : x_cand;
                } catch (...) {
                    // If tail sampling fails, continue to next attempt
                    continue;
                }
            }

            // For regular layers, generate x candidate
            double u1;
            try {
                u1 = uniform_generator_();
                if (u1 <= 0.0 || u1 >= 1.0) {
                    u1 = 0.5;  // Use a safe default if generator fails
                }
            } catch (...) {
                // If generator fails, derive u1 from iz
                u1 = (iz & 0x7FFFFFFF) / static_cast<double>(0x7FFFFFFF);
            }

            x_cand = u1 * x_table_[idx];  // x_cand is in [0, x_table_[idx])

            if (ZIGGURAT_DEBUG_LOGGING)
                Rcpp::Rcout << "[Ziggurat Gen] idx=" << idx << ", u1=" << u1
                            << ", x_table_[idx]=" << x_table_[idx] << ", x_cand=" << x_cand
                            << std::endl;

            // Check if we're in the rectangle part (fast accept)
            if (iz < k_table_[idx]) {
                if (ZIGGURAT_DEBUG_LOGGING)
                    Rcpp::Rcout << "[Ziggurat Gen] Rectangle accept for idx=" << idx
                                << ". x_cand=" << x_cand << std::endl;
                return sign ? -x_cand : x_cand;
            }

            // Handle base strip special case
            if (idx == ZIGGURAT_TABLE_SIZE - 1) {
                if (ZIGGURAT_DEBUG_LOGGING)
                    Rcpp::Rcout << "[Ziggurat Gen] Base strip for idx=" << idx << ". Re-looping."
                                << std::endl;
                continue;
            }

            // Wedge test - need another uniform
            double u2;
            try {
                u2 = uniform_generator_();
                if (u2 <= 0.0 || u2 >= 1.0) {
                    u2 = 0.5;  // Use a safe default if generator fails
                }
            } catch (...) {
                // If generator fails, derive u2 deterministically
                u2 = (((iz >> 8) & 0x7FFFFFFF)) / static_cast<double>(0x7FFFFFFF);
            }

            if (ZIGGURAT_DEBUG_LOGGING) {
                Rcpp::Rcout << "[Ziggurat Gen] Wedge test for idx=" << idx << ". x_cand=" << x_cand
                            << ", u2=" << u2 << std::endl;
                Rcpp::Rcout << "                 y_table_[idx]=" << y_table_[idx]
                            << ", y_table_[idx-1]=" << y_table_[idx - 1] << std::endl;
            }

            // Compute pdf value at the candidate point
            double pdf_x = std::exp(-0.5 * x_cand * x_cand);

            // Compute random height in the wedge
            double y_wedge = y_table_[idx - 1] + u2 * (y_table_[idx] - y_table_[idx - 1]);

            // Accept if point is under the PDF curve
            if (pdf_x > y_wedge) {
                if (ZIGGURAT_DEBUG_LOGGING)
                    Rcpp::Rcout << "[Ziggurat Gen] Wedge accept for idx=" << idx
                                << ". x_cand=" << x_cand << std::endl;
                return sign ? -x_cand : x_cand;
            }

            if (ZIGGURAT_DEBUG_LOGGING)
                Rcpp::Rcout << "[Ziggurat Gen] Wedge reject for idx=" << idx << ". Loop again."
                            << std::endl;
            // If rejected, continue to next attempt

        } catch (const std::exception& e) {
            if (ZIGGURAT_DEBUG_LOGGING)
                Rcpp::Rcout << "[Ziggurat Gen] Exception: " << e.what() << ". Continuing."
                            << std::endl;
            // Continue to next attempt
        } catch (...) {
            if (ZIGGURAT_DEBUG_LOGGING)
                Rcpp::Rcout << "[Ziggurat Gen] Unknown exception. Continuing." << std::endl;
            // Continue to next attempt
        }
    }

    // If we reach here, we've failed to generate a valid sample after MAX_ATTEMPTS
    throw std::runtime_error("ZigguratNormal::generate_internal failed after maximum attempts");
}

// Optimized thread-safe implementation using thread-local tables and random number cache
double ZigguratNormal::generate_internal_thread_safe() {
    if (ZIGGURAT_DEBUG_LOGGING)
        Rcpp::Rcout << "[Ziggurat TS] Enter." << std::endl;

    // Initialize thread-local tables if not yet done
    initialize_thread_local_tables();

    // Initialize random cache if not yet done
    initialize_random_cache();

    uint32_t iz;    // Random integer from bits of uniform
    uint32_t idx;   // Index into tables
    double x_cand;  // Candidate x value

    // Maximum number of attempts to avoid infinite loops
    const int MAX_ATTEMPTS = 1000;
    int attempts = 0;

    while (attempts < MAX_ATTEMPTS) {
        attempts++;

        try {
            // Generate uniform u0 for sign and index - from cache to avoid mutex
            double u0 = get_cached_uniform();

            // Convert u0 to a 32-bit integer for table lookup
            iz = static_cast<uint32_t>(u0 * 4294967296.0);  // Multiply by 2^32
            idx = iz & ZIGGURAT_MASK;                       // Get index (0 to TABLE_SIZE-1)

            // Get random sign - from cache to avoid mutex
            double sign_u = get_cached_uniform();
            bool sign = (sign_u < 0.5);

            if (ZIGGURAT_DEBUG_LOGGING)
                Rcpp::Rcout << "[Ziggurat TS] u0=" << u0 << ", iz=" << iz << ", idx=" << idx
                            << std::endl;

            // Handle tail region (idx == 0)
            if (idx == 0) {
                if (ZIGGURAT_DEBUG_LOGGING)
                    Rcpp::Rcout << "[Ziggurat TS] idx=0 (tail)." << std::endl;

                // Use thread-safe tail sampling that uses the cached values
                double tail_sample = sample_from_tail_thread_safe();

                if (ZIGGURAT_DEBUG_LOGGING)
                    Rcpp::Rcout << "[Ziggurat TS] Tail returned " << tail_sample << std::endl;
                return sign ? -tail_sample : tail_sample;
            }

            // For regular layers, generate x candidate - from cache to avoid mutex
            double u1 = get_cached_uniform();

            // Use thread-local tables for the rest to avoid locks
            x_cand = u1 * tls_x_table_[idx];  // x_cand is in [0, tls_x_table_[idx])

            if (ZIGGURAT_DEBUG_LOGGING)
                Rcpp::Rcout << "[Ziggurat TS] idx=" << idx << ", u1=" << u1
                            << ", tls_x_table_[idx]=" << tls_x_table_[idx] << ", x_cand=" << x_cand
                            << std::endl;

            // Check if we're in the rectangle part (fast accept)
            if (iz < tls_k_table_[idx]) {
                if (ZIGGURAT_DEBUG_LOGGING)
                    Rcpp::Rcout << "[Ziggurat TS] Rectangle accept for idx=" << idx
                                << ". x_cand=" << x_cand << std::endl;
                return sign ? -x_cand : x_cand;
            }

            // Handle base strip special case
            if (idx == ZIGGURAT_TABLE_SIZE - 1) {
                if (ZIGGURAT_DEBUG_LOGGING)
                    Rcpp::Rcout << "[Ziggurat TS] Base strip for idx=" << idx << ". Re-looping."
                                << std::endl;
                continue;
            }

            // Wedge test - need another uniform - from cache to avoid mutex
            double u2 = get_cached_uniform();

            if (ZIGGURAT_DEBUG_LOGGING) {
                Rcpp::Rcout << "[Ziggurat TS] Wedge test for idx=" << idx << ". x_cand=" << x_cand
                            << ", u2=" << u2 << std::endl;
                Rcpp::Rcout << "                 tls_y_table_[idx]=" << tls_y_table_[idx]
                            << ", tls_y_table_[idx-1]=" << tls_y_table_[idx - 1] << std::endl;
            }

            // Compute pdf value at the candidate point
            double pdf_x = std::exp(-0.5 * x_cand * x_cand);

            // Compute random height in the wedge
            double y_wedge =
                tls_y_table_[idx - 1] + u2 * (tls_y_table_[idx] - tls_y_table_[idx - 1]);

            // Accept if point is under the PDF curve
            if (pdf_x > y_wedge) {
                if (ZIGGURAT_DEBUG_LOGGING)
                    Rcpp::Rcout << "[Ziggurat TS] Wedge accept for idx=" << idx
                                << ". x_cand=" << x_cand << std::endl;
                return sign ? -x_cand : x_cand;
            }

            if (ZIGGURAT_DEBUG_LOGGING)
                Rcpp::Rcout << "[Ziggurat TS] Wedge reject for idx=" << idx << ". Loop again."
                            << std::endl;
            // If rejected, continue to next attempt

        } catch (const std::exception& e) {
            if (ZIGGURAT_DEBUG_LOGGING)
                Rcpp::Rcout << "[Ziggurat TS] Exception: " << e.what() << ". Continuing."
                            << std::endl;
            // Continue to next attempt
        } catch (...) {
            if (ZIGGURAT_DEBUG_LOGGING)
                Rcpp::Rcout << "[Ziggurat TS] Unknown exception. Continuing." << std::endl;
            // Continue to next attempt
        }
    }

    // If we reach here, we've failed to generate a valid sample after MAX_ATTEMPTS
    throw std::runtime_error(
        "ZigguratNormal::generate_internal_thread_safe failed after maximum attempts");
}

ZigguratNormal::ZigguratNormal(std::function<double()> uniform_generator, double mean,
                               double stddev, bool thread_safe_mode)
    : uniform_generator_(uniform_generator), mean_(mean), stddev_(stddev),
      is_thread_safe_mode_(thread_safe_mode) {
    // Validate parameters with meaningful error messages
    if (!uniform_generator_) {
        throw std::invalid_argument("ZigguratNormal: Uniform generator cannot be null.");
    }

    if (stddev_ <= 0) {
        throw std::invalid_argument("ZigguratNormal: Standard deviation must be positive.");
    }

    try {
        // Use cached tables if standard parameters, otherwise initialize instance tables
        if (mean_ == 0.0 && stddev_ == 1.0) {
            use_cached_tables();
        } else {
            initialize_tables_original();
        }

        // Initialize thread-local tables if in thread-safe mode
        if (is_thread_safe_mode_.load()) {
            initialize_thread_local_tables();

            // Initialize thread-local resources for this instance
        }
    } catch (const std::exception& e) {
        // Provide more context in the error message
        throw std::runtime_error(std::string("ZigguratNormal: Initialization failed - ") +
                                 e.what());
    } catch (...) {
        throw std::runtime_error("ZigguratNormal: Unknown error during initialization");
    }

    // Don't test the generator during construction - this can cause circular dependencies
    // The generator will be tested the first time it's actually used
}

void ZigguratNormal::set_parameters(double mean, double stddev) {
    if (stddev <= 0) {
        throw std::invalid_argument("ZigguratNormal: Standard deviation must be positive.");
    }

    // In thread-safe mode, need to lock before changing parameters
    std::lock_guard<std::mutex> lock(instance_mutex_);

    // If previously using standard parameters but now using custom ones,
    // or vice versa, we need to reinitialize tables
    bool reinitialize = (mean_ == 0.0 && stddev_ == 1.0) != (mean == 0.0 && stddev == 1.0);

    mean_ = mean;
    stddev_ = stddev;

    if (reinitialize) {
        if (mean_ == 0.0 && stddev_ == 1.0) {
            use_cached_tables();
        } else {
            initialize_tables_original();
        }

        // Reset thread-local initialization flag to force refresh
        tls_tables_initialized_ = false;
    }
}

void ZigguratNormal::set_thread_safe_mode(bool enable) {
    // Set the thread-safe mode flag
    is_thread_safe_mode_.store(enable);

    // Initialize thread-local tables if enabling thread-safe mode
    if (enable) {
        initialize_thread_local_tables();
    }
}

bool ZigguratNormal::is_thread_safe_mode() const {
    return is_thread_safe_mode_.load();
}

double ZigguratNormal::generate() {
    try {
        // Thread exit detection - if we get an exception during thread-safe operation,
        // it might be because the thread is exiting or resources are being cleaned up
        static thread_local bool local_thread_exiting = false;
        // Also check the global cleanup flag
        if (local_thread_exiting || cleanup_in_progress_.load(std::memory_order_acquire)) {
            // Use fallback RNG instead of fixed mean value
            static thread_local std::mt19937 fallback_rng(std::random_device{}());
            std::normal_distribution<double> fallback_dist(mean_, stddev_);
            return fallback_dist(fallback_rng);
        }

        // Choose between thread-safe and normal implementation
        double std_normal_val;
        if (is_thread_safe_mode_.load()) {
            // Handle case where object is being destroyed
            if (!uniform_generator_) {
                // Use fallback RNG instead of fixed mean value
                static thread_local std::mt19937 fallback_rng(std::random_device{}());
                std::normal_distribution<double> fallback_dist(mean_, stddev_);
                return fallback_dist(fallback_rng);
            }

            // Initialize thread-local resources if needed
            if (!tls_tables_initialized_) {
                try {
                    initialize_thread_local_tables();
                    // Verify initialization succeeded
                    if (!tls_tables_initialized_) {
                        // Initialization failed, use fallback
                        static thread_local std::mt19937 fallback_rng(std::random_device{}());
                        std::normal_distribution<double> fallback_dist(mean_, stddev_);
                        return fallback_dist(fallback_rng);
                    }
                } catch (...) {
                    // Use fallback RNG instead of fixed mean value
                    static thread_local std::mt19937 fallback_rng(std::random_device{}());
                    std::normal_distribution<double> fallback_dist(mean_, stddev_);
                    return fallback_dist(fallback_rng);
                }
            }

            // Post-initialization check - verify TLS data is valid
            if (!tls_manager_ || !tls_manager_->is_valid()) {
                // TLS manager is invalid, use fallback
                static thread_local std::mt19937 fallback_rng(std::random_device{}());
                std::normal_distribution<double> fallback_dist(mean_, stddev_);
                return fallback_dist(fallback_rng);
            }

            // Run thread-safe generation with exception handling
            try {
                std_normal_val = generate_internal_thread_safe();
            } catch (...) {
                // If generation fails in thread-safe mode, fall back to simple method
                // This might happen if thread-local resources are in an inconsistent state
                try {
                    std_normal_val = generate_internal();
                } catch (...) {
                    // Use fallback RNG instead of fixed mean value
                    static thread_local std::mt19937 fallback_rng(std::random_device{}());
                    std::normal_distribution<double> fallback_dist(mean_, stddev_);
                    return fallback_dist(fallback_rng);
                }
            }
        } else {
            std_normal_val = generate_internal();
        }

        // Apply mean and stddev transformation
        double result = mean_ + stddev_ * std_normal_val;

        // Validate the result
        if (std::isnan(result) || std::isinf(result)) {
            // Use fallback RNG instead of fixed mean value
            static thread_local std::mt19937 fallback_rng(std::random_device{}());
            std::normal_distribution<double> fallback_dist(mean_, stddev_);
            return fallback_dist(fallback_rng);
        }

        return result;
    } catch (const std::exception& e) {
        static thread_local int warning_count = 0;
        if (warning_count < 5) {
            Rcpp::warning("ZigguratNormal::generate failed: %s", e.what());
            warning_count++;
        }
        // Use fallback RNG instead of fixed mean value
        static thread_local std::mt19937 fallback_rng(std::random_device{}());
        std::normal_distribution<double> fallback_dist(mean_, stddev_);
        return fallback_dist(fallback_rng);
    } catch (...) {
        // Something catastrophic happened - possibly thread exit
        // Mark this thread for cleanup

        // Check if cleanup is already in progress
        if (!cleanup_in_progress_.load(std::memory_order_acquire)) {
            // Clean up thread-local resources to prevent segfaults
            cleanup_thread_local_resources();
        }

        // Use fallback RNG instead of fixed mean value
        static thread_local std::mt19937 fallback_rng(std::random_device{}());
        std::normal_distribution<double> fallback_dist(mean_, stddev_);
        return fallback_dist(fallback_rng);
    }
}

void ZigguratNormal::generate_n(double* buffer, size_t count) {
    // Validate input
    if (!buffer) {
        Rcpp::warning("ZigguratNormal::generate_n called with null buffer");
        return;
    }

    // Thread exit detection - if a previous operation marked this thread as exiting,
    // don't attempt complex multi-threading
    static thread_local bool local_thread_exiting = false;

    try {
        // For small buffers, non-threaded mode, or thread-exit situations, use the simple loop
        if (count < 1000 || !is_thread_safe_mode_.load() || local_thread_exiting) {
            // Fill buffer with generated values
            for (size_t i = 0; i < count; ++i) {
                try {
                    buffer[i] = generate();
                } catch (...) {
                    // If generate fails for an individual value, use mean
                    // Use fallback RNG instead of fixed mean value
                    static thread_local std::mt19937 fallback_rng(std::random_device{}());
                    std::normal_distribution<double> fallback_dist(mean_, stddev_);
                    buffer[i] = fallback_dist(fallback_rng);
                }
            }
            return;
        }

        // Safety check for object validity during multi-threaded operation
        if (!uniform_generator_) {
            // Something is wrong with the generator - fill with mean values
            for (size_t i = 0; i < count; ++i) {
                buffer[i] = mean_;
            }
            return;
        }

        // For larger buffers in thread-safe mode, use multiple threads with proper cleanup

        // Get the hardware concurrency (number of available cores)
        const size_t hardware_threads = std::thread::hardware_concurrency();

        // Calculate optimal thread count (limited to avoid creating too many threads)
        // Use fewer threads to reduce contention and resource usage
        const size_t max_threads = std::min(hardware_threads, size_t(4));     // Reduced from 8
        const size_t thread_count = std::min(max_threads, count / 2000 + 1);  // Increased threshold

        if (thread_count <= 1) {
            // If only one thread would be created, just use the simple loop
            for (size_t i = 0; i < count; ++i) {
                try {
                    buffer[i] = generate();
                } catch (...) {
                    // Use fallback RNG instead of fixed mean value
                    static thread_local std::mt19937 fallback_rng(std::random_device{}());
                    std::normal_distribution<double> fallback_dist(mean_, stddev_);
                    buffer[i] = fallback_dist(fallback_rng);
                }
            }
            return;
        }

        // Calculate chunk size for each thread
        const size_t chunk_size = count / thread_count;
        const size_t remainder = count % thread_count;

        // Create and start threads
        std::vector<std::thread> threads;
        std::atomic<bool> any_thread_failed(false);

        for (size_t t = 0; t < thread_count; ++t) {
            // Calculate start and end for this thread
            size_t start = t * chunk_size + std::min(t, remainder);
            size_t end = start + chunk_size + (t < remainder ? 1 : 0);

            // Create thread with thread-specific exit flag
            threads.push_back(std::thread([this, buffer, start, end, &any_thread_failed]() {
                // Thread-local exit flag for this worker thread
                thread_local bool worker_exiting = false;

                try {
                    // Initialize thread-local tables and cache for this thread
                    // with proper error handling
                    try {
                        initialize_thread_local_tables();
                        initialize_random_cache();
                    } catch (...) {
                        // If initialization fails, mark thread as failed
                        any_thread_failed.store(true);
                        worker_exiting = true;

                        // Fill this thread's portion with mean values
                        for (size_t i_local = start; i_local < end; ++i_local) {
                            buffer[i_local] = mean_;
                        }
                        return;  // Exit this thread's lambda
                    }

                    // Generate values for this chunk
                    for (size_t i_local = start; i_local < end; ++i_local) {
                        // Check if thread is marked as exiting
                        if (worker_exiting) {
                            buffer[i_local] = mean_;
                            continue;
                        }

                        try {
                            // Check if parent object is still valid via thread-safe mode flag
                            if (!is_thread_safe_mode_.load()) {
                                buffer[i_local] = mean_;
                                continue;
                            }

                            // Use the optimized thread-safe implementation with fallback
                            double std_normal_val;
                            try {
                                std_normal_val = generate_internal_thread_safe();
                            } catch (...) {
                                // If thread-safe generation fails, try non-thread-safe
                                try {
                                    std_normal_val = generate_internal();
                                } catch (...) {
                                    // Both methods failed, use mean
                                    buffer[i_local] = mean_;
                                    continue;
                                }
                            }

                            // Apply mean and stddev transformation
                            buffer[i_local] = mean_ + stddev_ * std_normal_val;

                            // Validate the result
                            if (std::isnan(buffer[i_local]) || std::isinf(buffer[i_local])) {
                                buffer[i_local] = mean_;  // Fallback to mean value
                            }
                        } catch (...) {
                            // If generate fails for an individual value, use mean
                            buffer[i_local] = mean_;

                            // If too many failures occur, mark this thread as exiting
                            static thread_local int failure_count = 0;
                            if (++failure_count > 20) {
                                worker_exiting = true;
                            }
                        }
                    }
                } catch (...) {
                    // If this thread fails completely, mark it
                    any_thread_failed.store(true);
                    worker_exiting = true;

                    // Fill this thread's portion with mean values
                    for (size_t i_local = start; i_local < end; ++i_local) {
                        buffer[i_local] = mean_;
                    }
                }

                // Clean up thread-local resources if this thread is marked as exiting
                if (worker_exiting) {
                    cleanup_thread_local_resources();
                }
            }));
        }

        // Safer thread joining with timeout
        for (size_t t = 0; t < threads.size(); ++t) {
            if (threads[t].joinable()) {
                try {
                    // Try joining with a timeout
                    std::chrono::milliseconds timeout(1000);  // 1 second timeout
                    auto thread_finished =
                        std::async(std::launch::async, [&]() { threads[t].join(); });

                    if (thread_finished.wait_for(timeout) != std::future_status::ready) {
                        // Thread didn't finish in time - we have a problem
                        any_thread_failed.store(true);
                        // Cannot safely detach or terminate - we'll have to wait
                        threads[t].join();  // This might block
                    }
                } catch (...) {
                    any_thread_failed.store(true);
                }
            }
        }

        // If any thread failed, log a warning
        if (any_thread_failed.load()) {
            Rcpp::warning(
                "Some threads failed during parallel generation. Results may be affected.");
        }
    } catch (const std::exception& e) {
        // Something went wrong with the overall process
        Rcpp::warning("ZigguratNormal::generate_n failed: %s", e.what());

        // Mark this thread as exiting to prevent further generation attempts
        local_thread_exiting = true;

        // Clean up thread-local resources to prevent segfaults
        cleanup_thread_local_resources();

        // Fill entire buffer with mean value
        for (size_t i = 0; i < count; ++i) {
            buffer[i] = mean_;
        }
    } catch (...) {
        // Unknown error - mark thread as exiting
        local_thread_exiting = true;

        // Clean up thread-local resources
        cleanup_thread_local_resources();

        // Fill with mean values
        for (size_t i = 0; i < count; ++i) {
            buffer[i] = mean_;
        }
    }
}

// Destructor to handle cleanup
ZigguratNormal::~ZigguratNormal() {
    // Set thread-safe mode to false before cleanup to prevent thread races
    is_thread_safe_mode_.store(false);

    if (ZIGGURAT_DEBUG_LOGGING) {
        Rcpp::Rcout << "ZigguratNormal destructor called" << std::endl;
    }
}

// Static method to check if cleanup is in progress
bool ZigguratNormal::is_cleanup_in_progress() {
    return cleanup_in_progress_.load(std::memory_order_acquire);
}

// Static method to prepare for shutdown
void ZigguratNormal::prepare_for_shutdown() {
    // Try to mark cleanup in progress using compare_exchange
    bool expected = false;
    if (!cleanup_in_progress_.compare_exchange_strong(expected, true, std::memory_order_acq_rel)) {
        // Cleanup is already in progress, just return
        return;
    }

    if (ZIGGURAT_DEBUG_LOGGING) {
        Rcpp::Rcout << "ZigguratNormal prepared for shutdown" << std::endl;
    }

    // During shutdown, we ensure all thread-local resources are reset to safe values
    try {
        // We don't have access to each thread's resources directly, but we'll set up flags
        // to ensure any still-running threads will see the shutdown state and act accordingly
    } catch (...) {
        // Suppress any exceptions during shutdown preparation
        if (ZIGGURAT_DEBUG_LOGGING) {
            Rcpp::Rcout << "Exception during shutdown preparation - suppressed" << std::endl;
        }
    }
}

// Static method to clean up thread-local resources
void ZigguratNormal::cleanup_thread_local_resources() {
    // Thread-local flag to prevent recursive cleanup
    thread_local std::atomic<bool> cleanup_in_progress_local{false};

    // Check if cleanup is already in progress for this thread
    bool expected = false;
    if (!cleanup_in_progress_local.compare_exchange_strong(expected, true,
                                                           std::memory_order_acq_rel)) {
        // Cleanup already in progress, avoid recursion
        return;
    }

    // RAII guard to reset the flag when we're done
    struct CleanupGuard {
        std::atomic<bool>& flag;
        ~CleanupGuard() {
            flag.store(false, std::memory_order_release);
            std::atomic_thread_fence(std::memory_order_release);
        }
    } guard{cleanup_in_progress_local};

    // Add memory fence to ensure all previous writes are visible
    std::atomic_thread_fence(std::memory_order_acquire);

    if (ZIGGURAT_DEBUG_LOGGING) {
        Rcpp::Rcout << "Thread " << std::this_thread::get_id() << " starting TLS cleanup"
                    << std::endl;
    }

    try {
        // Reset TLS tables - only do this if they were initialized
        if (tls_tables_initialized_) {
            for (size_t i = 0; i < ZIGGURAT_TABLE_SIZE; ++i) {
                tls_x_table_[i] = 0.0;
                tls_y_table_[i] = 0.0;
                tls_k_table_[i] = 0;
            }
            tls_tables_initialized_ = false;
        }

        // Reset random cache
        if (tls_random_cache_initialized_) {
            for (size_t i = 0; i < RANDOM_CACHE_SIZE; ++i) {
                tls_random_cache_[i] = 0.0;
            }
            tls_random_cache_initialized_ = false;
        }
        tls_random_cache_pos_ = RANDOM_CACHE_SIZE;  // Mark as empty

        // Invalidate the TLS manager if it exists
        if (tls_manager_) {
            tls_manager_->invalidate();
            tls_manager_ = nullptr;
        }

        // Reset the cleanup registration flag for this thread with proper ordering
        tls_cleanup_registered_.store(false, std::memory_order_release);

        // Final memory fence to ensure all cleanup is visible
        std::atomic_thread_fence(std::memory_order_release);

        if (ZIGGURAT_DEBUG_LOGGING) {
            Rcpp::Rcout << "Thread " << std::this_thread::get_id() << " completed TLS cleanup"
                        << std::endl;
        }
    } catch (...) {
        // Suppress any exceptions during cleanup
        if (ZIGGURAT_DEBUG_LOGGING) {
            Rcpp::Rcout << "Exception during thread-local cleanup - suppressed" << std::endl;
        }
    }
}

}  // namespace qiprng
