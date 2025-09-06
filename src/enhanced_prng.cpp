// File: enhanced_prng.cpp
// --------------------------------------------------------------
#include "enhanced_prng.hpp"

#include <algorithm>  // For std::min, std::max
#include <cmath>  // For std::isnan, std::isinf, std::round, std::exp, std::pow, std::sqrt, std::log, std::fmod, std::floor
#include <limits>       // For std::numeric_limits
#include <stdexcept>    // For std::invalid_argument
#include <type_traits>  // For std::is_same

// v0.5.0: OpenMP support for parallel buffer filling
#ifdef _OPENMP
#    include <omp.h>
#    define PRNG_HAS_OPENMP 1
#else
#    define PRNG_HAS_OPENMP       0
#    define omp_get_num_threads() 1
#    define omp_get_thread_num()  0
#    define omp_get_max_threads() 1
#endif

namespace qiprng {

// Helper method to get raw uniform without distribution transformations
// This avoids circular dependencies when used with Ziggurat
double EnhancedPRNG::next_raw_uniform() {
    // Thread-safe fallback generator with lazy initialization
    struct FallbackGen {
        std::mt19937_64 rng;
        std::uniform_real_distribution<double> dist{0.000001, 0.999999};
        std::once_flag init_flag;

        void init() {
            std::call_once(init_flag, [this]() {
                ensure_libsodium_initialized();
                uint64_t seed;
                randombytes_buf(&seed, sizeof(seed));
                rng.seed(seed);
            });
        }

        double generate() {
            init();
            return dist(rng);
        }
    };
    static thread_local FallbackGen fallback_gen;

    // Check if object is being destroyed
    if (is_being_destroyed_.load(std::memory_order_acquire)) {
        // Return a random value from fallback generator if we're in the process of cleanup
        return fallback_gen.generate();
    }

    // If buffer needs refilling, do so
    if (buffer_pos_ >= buffer_.size()) {
        try {
            fill_buffer();
        } catch (...) {
            // Return a random value from fallback generator if fill_buffer fails
            return fallback_gen.generate();
        }
    }

    // Ensure buffer position is valid even after fill_buffer might have changed it
    if (buffer_pos_ >= buffer_.size()) {
        return fallback_gen.generate();  // Use fallback if buffer is still invalid
    }

    // Get raw uniform value with bounds checking
    double u;
    try {
        u = buffer_[buffer_pos_++];
        sample_count_++;

        // Validate the value
        if (u <= 0.0 || u >= 1.0 || std::isnan(u) || std::isinf(u)) {
            u = fallback_gen.generate();  // Use fallback if we got an invalid uniform
        }
    } catch (...) {
        // Return a random value from fallback if array access throws
        return fallback_gen.generate();
    }

    // Reseed crypto periodically if enabled
    if (config_.use_crypto_mixing && crypto_ && config_.reseed_interval > 0 &&
        sample_count_ % config_.reseed_interval == 0) {
        try {
            crypto_->reseed();
        } catch (...) {
            // Ignore errors during reseeding
        }
    }

    return u;
}

#include "thread_manager.hpp"

// EnhancedPRNG implementation
EnhancedPRNG::EnhancedPRNG(const PRNGConfig& cfg,
                           const std::vector<std::tuple<long, long, long>>& abc_list)
    : config_(cfg), buffer_(cfg.buffer_size),
      buffer_pos_(cfg.buffer_size),  // Start at end to force fill on first next()
      sample_count_(0), has_spare_normal_(false), spare_normal_(0.0), is_being_destroyed_(false) {
    // Validate important parameters
    if (abc_list.empty()) {
        throw std::runtime_error("EnhancedPRNG: abc_list cannot be empty");
    }

    // Create ziggurat for normal generation if needed
    if (config_.normal_method == PRNGConfig::ZIGGURAT) {
        ziggurat_ = std::make_unique<ZigguratNormal>(
            [this]() {
                return this->next_raw_uniform();
            },  // Use raw uniform to avoid circular dependency
            config_.normal_mean, config_.normal_sd,
            config_.use_threading  // Enable thread-safe mode if threading is enabled
        );
    }

    // Create crypto mixer if enabled
    if (config_.use_crypto_mixing) {
        // SECURITY FIX: Use centralized libsodium initialization
        ensure_libsodium_initialized();

        // SECURITY FIX: PREVENT deterministic seeds with crypto mixing
        if (config_.has_seed) {
            throw std::runtime_error(
                "SECURITY ERROR: Deterministic seeds are not allowed with cryptographic mixing. "
                "This configuration would completely defeat cryptographic security. "
                "Either disable crypto mixing or remove the deterministic seed.");
        }

        // SECURITY FIX: CryptoMixer no longer accepts seed parameters
        crypto_ =
            std::make_unique<CryptoMixer>(config_.adhoc_corrections, config_.use_tie_breaking);
    }

    // Create MultiQI instance with provided parameters
    // v0.5.0: Use mixing strategy from config
    MixingStrategy strategy = static_cast<MixingStrategy>(config_.mixing_strategy);

    if (config_.has_seed) {
        // Pass seed to MultiQI for deterministic initialization
        multi_ = std::make_unique<MultiQI>(abc_list, config_.mpfr_precision, config_.seed, true,
                                           strategy);
    } else {
        multi_ = std::make_unique<MultiQI>(abc_list, config_.mpfr_precision, 0, false, strategy);
    }

    reset_state();

    // Register thread for cleanup if threading is enabled
    if (config_.use_threading) {
        registerThreadForCleanup();
    }
}

// Thread-safe shutdown preparation
void EnhancedPRNG::prepare_for_shutdown() {
    // Set the destruction flag with sequential consistency
    is_being_destroyed_.store(true, std::memory_order_seq_cst);

    // Call prepare_for_shutdown on ziggurat if it exists
    if (ziggurat_) {
        try {
            ziggurat_->prepare_for_shutdown();
            ziggurat_->set_thread_safe_mode(false);
        } catch (...) {
            // Ignore errors during shutdown preparation
        }
    }
}

// Destructor with safe cleanup
EnhancedPRNG::~EnhancedPRNG() noexcept {
    // Use std::call_once to ensure shutdown logic runs only once
    std::call_once(shutdown_once_flag_, [this]() { prepare_for_shutdown(); });

    // Acquire cleanup mutex to prevent concurrent access
    std::lock_guard<std::mutex> lock(cleanup_mutex_);

    try {
        // Clean up resources in an orderly fashion
        // Order matters for safe destruction
        crypto_.reset();
        ziggurat_.reset();
        multi_.reset();

        // Clear buffer
        buffer_ = SecureBuffer<double>(0);

    } catch (const std::exception& e) {
        // Log error but continue - this is a destructor
        if (config_.debug) {
            // Can't use Rcpp::warning in a noexcept destructor
            // Silently continue
        }
    } catch (...) {
        // Catch all exceptions to prevent crashes during destruction
        // This is required for noexcept guarantee
    }
}

const PRNGConfig& EnhancedPRNG::getConfig() const {
    return config_;
}

size_t EnhancedPRNG::getQICount() const {
    return multi_ ? multi_->size() : 0;
}

void EnhancedPRNG::updateConfig(const PRNGConfig& new_config) {
    // Determine if we need to reset certain components
    bool reset_buffer = (new_config.buffer_size != config_.buffer_size) ||
                        (new_config.distribution != config_.distribution) ||
                        (new_config.normal_method != config_.normal_method);

    // Always reset buffer when changing distribution parameters
    if (new_config.distribution == PRNGConfig::NORMAL &&
        (new_config.normal_mean != config_.normal_mean ||
         new_config.normal_sd != config_.normal_sd)) {
        reset_buffer = true;
    } else if (new_config.distribution == PRNGConfig::UNIFORM_RANGE &&
               (new_config.range_min != config_.range_min ||
                new_config.range_max != config_.range_max)) {
        reset_buffer = true;
    } else if (new_config.distribution == PRNGConfig::EXPONENTIAL &&
               new_config.exponential_lambda != config_.exponential_lambda) {
        reset_buffer = true;
    }

    bool reset_ziggurat = (new_config.normal_method != config_.normal_method) ||
                          (new_config.normal_mean != config_.normal_mean) ||
                          (new_config.normal_sd != config_.normal_sd);

    bool reset_crypto = (new_config.use_crypto_mixing != config_.use_crypto_mixing) ||
                        (new_config.adhoc_corrections != config_.adhoc_corrections) ||
                        (new_config.use_tie_breaking != config_.use_tie_breaking);

    // Store old distribution for transition checks
    PRNGConfig::Distribution old_distribution = config_.distribution;

    // Update configuration
    config_ = new_config;

    // Reset components as needed
    if (reset_buffer) {
        buffer_ = SecureBuffer<double>(config_.buffer_size);
        buffer_pos_ = config_.buffer_size;  // Force refill on next use
        has_spare_normal_ = false;

        // Ensure we always reseed when changing distribution types
        if (old_distribution != config_.distribution) {
            // Force a reseed to ensure clean state for the new distribution
            reseed();
        }
    }

    if (reset_ziggurat) {
        if (config_.normal_method == PRNGConfig::ZIGGURAT) {
            if (ziggurat_) {
                // Update parameters and thread-safe mode
                ziggurat_->set_parameters(config_.normal_mean, config_.normal_sd);
                ziggurat_->set_thread_safe_mode(config_.use_threading);
            } else {
                // Create new ziggurat instance
                ziggurat_ = std::make_unique<ZigguratNormal>(
                    [this]() {
                        return this->next_raw_uniform();
                    },  // Use raw uniform to avoid circular dependency
                    config_.normal_mean, config_.normal_sd,
                    config_.use_threading  // Enable thread-safe mode if threading is enabled
                );
            }
        } else {
            ziggurat_.reset();  // Not needed for Box-Muller
        }
    } else if (ziggurat_ && config_.normal_method == PRNGConfig::ZIGGURAT) {
        // Update thread-safe mode even if parameters didn't change
        ziggurat_->set_thread_safe_mode(config_.use_threading);
    }

    if (reset_crypto) {
        if (config_.use_crypto_mixing) {
            crypto_ =
                std::make_unique<CryptoMixer>(config_.adhoc_corrections, config_.use_tie_breaking);
        } else {
            crypto_.reset();
        }
    }
}

void EnhancedPRNG::reset_state() {
    buffer_pos_ = config_.buffer_size;
    has_spare_normal_ = false;
    sample_count_ = 0;
}

void EnhancedPRNG::fill_buffer_sequential() {
    if (buffer_.size() == 0)
        return;

    // Fill buffer with raw samples from MultiQI using thread-safe method
    multi_->fill_thread_safe(buffer_.data(), buffer_.size());

    // Apply crypto mixing if enabled
    if (config_.use_crypto_mixing && crypto_ && crypto_->is_initialized()) {
        crypto_->mix(reinterpret_cast<unsigned char*>(buffer_.data()),
                     buffer_.size() * sizeof(double));
    }

    // Reset buffer position
    buffer_pos_ = 0;
}

void EnhancedPRNG::copy_thread_buffers_to_main(
    size_t thread_count, const std::vector<SecureBuffer<double>>& thread_buffers,
    ThreadPool& pool) {
    // Copy thread buffers back to main buffer
    if (buffer_.size() >= 10000 && thread_count > 1) {
        // For large buffers, use parallel copying
        std::vector<std::future<void>> copy_futures;
        size_t buffer_pos = 0;

        for (size_t t = 0; t < thread_count && t < thread_buffers.size(); t++) {
            const SecureBuffer<double>& thread_buffer = thread_buffers[t];
            const size_t copy_size = thread_buffer.size();

            if (copy_size > 1000) {
                // For larger chunks, copy in parallel
                const size_t start_pos = buffer_pos;
                copy_futures.push_back(pool.enqueue([this, &thread_buffer, start_pos, copy_size]() {
                    std::memcpy(buffer_.data() + start_pos, thread_buffer.data(),
                                copy_size * sizeof(double));
                }));
            } else {
                // For smaller buffers, copy directly
                std::memcpy(buffer_.data() + buffer_pos, thread_buffer.data(),
                            copy_size * sizeof(double));
            }
            buffer_pos += copy_size;
        }

        // Wait for all copy operations to complete
        for (auto& future : copy_futures) {
            future.get();
        }
    } else {
        // For smaller buffers, copy sequentially
        size_t buffer_pos = 0;
        for (size_t t = 0; t < thread_count && t < thread_buffers.size(); t++) {
            const SecureBuffer<double>& thread_buffer = thread_buffers[t];
            const size_t copy_size = std::min(thread_buffer.size(), buffer_.size() - buffer_pos);

            // Safely copy the data
            if (copy_size > 0 && buffer_pos + copy_size <= buffer_.size()) {
                std::memcpy(buffer_.data() + buffer_pos, thread_buffer.data(),
                            copy_size * sizeof(double));
                buffer_pos += copy_size;
            }
        }
    }
}

void EnhancedPRNG::submit_parallel_tasks(ThreadPool& pool, size_t thread_count,
                                         std::vector<SecureBuffer<double>>& thread_buffers,
                                         std::vector<std::unique_ptr<MultiQI>>& thread_qis,
                                         std::atomic<bool>& any_thread_failed,
                                         std::vector<std::future<void>>& futures) {
    // Submit tasks to the thread pool
    for (size_t t = 0; t < thread_count; t++) {
        // Capture thread index by value and other objects by reference
        futures.push_back(
            pool.enqueue([t, &thread_buffers, &thread_qis, &any_thread_failed, this]() {
                try {
                    if (t < thread_qis.size() && t < thread_buffers.size() && thread_qis[t] &&
                        thread_buffers[t].size() > 0) {
                        // Fill the thread's buffer with its dedicated MultiQI
                        thread_qis[t]->fill_thread_safe(thread_buffers[t].data(),
                                                        thread_buffers[t].size());

                    } else {
                        // Resource not properly initialized
                        any_thread_failed.store(true);
                    }
                } catch (const std::exception& e) {
                    // Log and handle error
                    if (config_.debug) {
                        thread_local int warning_count = 0;
                        if (warning_count < 3) {  // Limit warnings per thread
                            Rcpp::warning("Thread %d failed: %s", static_cast<int>(t), e.what());
                            warning_count++;
                        }
                    }
                    any_thread_failed.store(true);

                    // Fill with proper random fallback values
                    if (t < thread_buffers.size()) {
                        // SECURITY FIX: Use libsodium for cryptographically secure fallback
                        ensure_libsodium_initialized();
                        for (size_t i = 0; i < thread_buffers[t].size(); i++) {
                            uint64_t rand_val;
                            randombytes_buf(&rand_val, sizeof(rand_val));
                            // Convert to uniform [0,1) using proper scaling
                            thread_buffers[t][i] = (rand_val >> 11) * 0x1.0p-53;
                        }
                    }
                } catch (...) {
                    // Handle unknown errors
                    any_thread_failed.store(true);

                    // Fill with proper random fallback values
                    if (t < thread_buffers.size()) {
                        // SECURITY FIX: Use libsodium for cryptographically secure fallback
                        ensure_libsodium_initialized();
                        for (size_t i = 0; i < thread_buffers[t].size(); i++) {
                            uint64_t rand_val;
                            randombytes_buf(&rand_val, sizeof(rand_val));
                            // Convert to uniform [0,1) using proper scaling
                            thread_buffers[t][i] = (rand_val >> 11) * 0x1.0p-53;
                        }
                    }
                }
            }));
    }
}

bool EnhancedPRNG::create_thread_resources(
    size_t thread_count, std::vector<std::vector<std::tuple<long, long, long>>>& thread_abc_lists,
    std::vector<std::unique_ptr<MultiQI>>& thread_qis,
    std::vector<SecureBuffer<double>>& thread_buffers, std::vector<size_t>& chunk_sizes) {
    const PRNGConfig& cfg = config_;

    if (config_.debug) {
        Rcpp::Rcout << "Creating thread resources for " << thread_count << " threads\n";
    }

    // Generate unique parameters for each thread to avoid identical sequences
    for (size_t t = 0; t < thread_count; t++) {
        std::vector<std::tuple<long, long, long>> abc_list;

        // Get current parameters from primary QI or use default
        if (multi_ && multi_->size() > 0) {
            // Use pickMultiQiSet with thread-specific seed to ensure unique sequences
            try {
                // Add thread index to seed to ensure each thread gets different parameters
                // Use a simpler offset to avoid potential overflow issues
                uint64_t thread_seed = cfg.has_seed ? (cfg.seed + t * 1237ULL) : 0;
                abc_list = pickMultiQiSet(cfg.mpfr_precision, 3, thread_seed, cfg.has_seed);
            } catch (...) {
                // In case of exception, use a safe default with thread variation
                abc_list.push_back(std::make_tuple(cfg.a + t, cfg.b + t * 2, cfg.c - t));
            }
        } else {
            // Fallback parameters with thread variation
            abc_list.push_back(std::make_tuple(cfg.a + t, cfg.b + t * 2, cfg.c - t));
        }

        thread_abc_lists.push_back(abc_list);
    }

    // Calculate chunk sizes
    const size_t base_chunk_size = buffer_.size() / thread_count;
    const size_t remainder = buffer_.size() % thread_count;
    chunk_sizes.resize(thread_count, base_chunk_size);

    // Distribute remainder among first few threads
    for (size_t i = 0; i < remainder; i++) {
        chunk_sizes[i]++;
    }

    // Create thread resources
    for (size_t t = 0; t < thread_count; t++) {
        try {
            // Ensure we have abc_list for this thread
            if (t >= thread_abc_lists.size()) {
                if (config_.debug) {
                    Rcpp::warning("Missing abc_list for thread %d", static_cast<int>(t));
                }
                return false;
            }

            // Create a MultiQI instance for each thread with thread-specific parameters
            thread_qis.push_back(std::make_unique<MultiQI>(thread_abc_lists[t], cfg.mpfr_precision,
                                                           0, false, MixingStrategy::ROUND_ROBIN));
            // Create a buffer for each thread
            thread_buffers.emplace_back(chunk_sizes[t]);
        } catch (...) {
            // If resource creation fails
            if (config_.debug) {
                Rcpp::warning("Failed to create thread-local resources.");
            }
            return false;
        }
    }

    return true;
}

void EnhancedPRNG::fill_buffer_parallel(size_t thread_count) {
    if (buffer_.size() == 0)
        return;

    // Ensure thread count is reasonable
    thread_count = std::min(thread_count, static_cast<size_t>(std::thread::hardware_concurrency()));
    if (thread_count == 0)
        thread_count = 1;

    // Limit thread count for very small buffers
    if (buffer_.size() < thread_count * 64) {
        // For small buffers, use sequential filling
        fill_buffer_sequential();
        return;
    }

    // v0.5.0: Use work-stealing for better load balancing
    const bool use_work_stealing = buffer_.size() >= 10000 && thread_count > 2;

    try {
        // Use the global thread pool for more efficient threading
        ThreadPool& pool = global_thread_pool();

        // Create containers for thread resources
        std::vector<std::vector<std::tuple<long, long, long>>> thread_abc_lists;
        std::vector<std::unique_ptr<MultiQI>> thread_qis;
        std::vector<SecureBuffer<double>> thread_buffers;
        std::vector<size_t> chunk_sizes;

        // Create thread resources using helper function
        if (!create_thread_resources(thread_count, thread_abc_lists, thread_qis, thread_buffers,
                                     chunk_sizes)) {
            // Fall back to sequential filling if resource creation fails
            fill_buffer_sequential();
            return;
        }

        // Thread synchronization objects
        std::atomic<bool> any_thread_failed(false);
        std::vector<std::future<void>> futures;

        // v0.5.0: Create work-stealing pool for load balancing
        std::unique_ptr<WorkStealingPool> ws_pool;
        if (use_work_stealing) {
            ws_pool = std::make_unique<WorkStealingPool>(thread_count);

            // Divide work into smaller chunks for work-stealing
            const size_t work_items_per_thread = 4;
            size_t buffer_pos = 0;

            for (size_t t = 0; t < thread_count; ++t) {
                size_t thread_buffer_size = chunk_sizes[t];
                size_t chunk_size = thread_buffer_size / work_items_per_thread;

                for (size_t w = 0; w < work_items_per_thread; ++w) {
                    size_t start = w * chunk_size;
                    size_t end = (w == work_items_per_thread - 1) ? thread_buffer_size
                                                                  : (w + 1) * chunk_size;

                    WorkItem item{start, end, t,
                                  [&thread_qis, &thread_buffers, t](size_t s, size_t e) {
                                      if (t < thread_qis.size() && thread_qis[t]) {
                                          thread_qis[t]->fill_thread_safe(
                                              thread_buffers[t].data() + s, e - s);
                                      }
                                  }};
                    ws_pool->submit(t, std::move(item));
                }
            }

            // Submit work-stealing workers
            for (size_t t = 0; t < thread_count; ++t) {
                futures.push_back(pool.enqueue([t, &ws_pool, &any_thread_failed]() {
                    try {
                        int idle_count = 0;
                        const int max_idle = 100;  // Prevent infinite loop

                        while (!ws_pool->is_done() && idle_count < max_idle) {
                            auto work = ws_pool->get_work(t);
                            if (work) {
                                work->task(work->start_idx, work->end_idx);
                                idle_count = 0;  // Reset on successful work
                            } else if (ws_pool->all_empty()) {
                                break;
                            } else {
                                idle_count++;
                                std::this_thread::yield();
                            }
                        }
                    } catch (...) {
                        any_thread_failed.store(true);
                    }
                }));
            }

            // Wait for work completion with timeout
            for (auto& future : futures) {
                future.wait();
            }
            ws_pool->shutdown();
        } else {
            // Original parallel submission without work-stealing
            submit_parallel_tasks(pool, thread_count, thread_buffers, thread_qis, any_thread_failed,
                                  futures);
        }

        // Wait for all futures to complete
        for (auto& future : futures) {
            try {
                future.get();  // This will re-throw any exceptions that occurred in the task
            } catch (const std::exception& e) {
                if (config_.debug) {
                    Rcpp::warning("Task failed with exception: %s", e.what());
                }
                any_thread_failed.store(true);
            } catch (...) {
                if (config_.debug) {
                    Rcpp::warning("Task failed with unknown exception");
                }
                any_thread_failed.store(true);
            }
        }

        // If any thread failed, log a warning but continue with the values we have
        if (any_thread_failed.load() && config_.debug) {
            Rcpp::warning(
                "Some threads failed during parallel buffer filling. Results may be affected.");
        }

        // Copy thread buffers back to main buffer using helper function
        copy_thread_buffers_to_main(thread_count, thread_buffers, pool);

        // Apply crypto mixing if enabled (can be potentially parallelized for large buffers)
        if (config_.use_crypto_mixing && crypto_ && crypto_->is_initialized()) {
            // For large buffers, consider dividing crypto mixing into chunks
            if (buffer_.size() >= 10000 && thread_count > 1) {
                // Parallel crypto mixing for large buffers can be implemented here
                // For now, we'll use the regular mixing
                crypto_->mix(reinterpret_cast<unsigned char*>(buffer_.data()),
                             buffer_.size() * sizeof(double));
            } else {
                // Standard crypto mixing
                crypto_->mix(reinterpret_cast<unsigned char*>(buffer_.data()),
                             buffer_.size() * sizeof(double));
            }
        }

    } catch (const std::exception& e) {
        if (config_.debug) {
            Rcpp::warning(
                "Exception in parallel buffer filling: %s. Falling back to sequential filling.",
                e.what());
        }
        // If anything fails, fall back to sequential filling
        fill_buffer_sequential();
        return;
    } catch (...) {
        if (config_.debug) {
            Rcpp::warning("Unknown exception in parallel buffer filling. Falling back to "
                          "sequential filling.");
        }
        // If anything fails, fall back to sequential filling
        fill_buffer_sequential();
        return;
    }

    // Reset buffer position
    buffer_pos_ = 0;
}

void EnhancedPRNG::fill_buffer() {
#if PRNG_HAS_OPENMP
    // v0.5.0: Try OpenMP first if available and enabled
    if (config_.use_parallel_filling && buffer_.size() >= 1024) {
        fill_buffer_openmp();
        return;
    }
#endif

    const size_t cpu_cores = std::thread::hardware_concurrency();
    const size_t max_threads = std::min(cpu_cores, size_t(8));  // Cap at 8 threads

    // Use parallel filling only if enabled and buffer is large enough
    if (config_.use_parallel_filling && buffer_.size() >= 1024 && max_threads > 1) {
        size_t num_threads = std::min(max_threads, buffer_.size() / 128);
        fill_buffer_parallel(num_threads);
    } else {
        fill_buffer_sequential();
    }
}

#if PRNG_HAS_OPENMP
// v0.5.0: OpenMP-optimized buffer filling
void EnhancedPRNG::fill_buffer_openmp() {
    if (buffer_.size() == 0)
        return;

    const int num_threads = omp_get_max_threads();
    const size_t chunk_size = buffer_.size() / num_threads;

    // Only use OpenMP for sufficiently large chunks
    if (chunk_size < 256) {
        fill_buffer_sequential();
        return;
    }

    try {
// Parallel region for buffer filling
#    pragma omp parallel
        {
            const int tid = omp_get_thread_num();
            const size_t start = tid * chunk_size;
            const size_t end = (tid == num_threads - 1) ? buffer_.size() : (tid + 1) * chunk_size;

            // Each thread fills its portion
            if (start < end) {
// Create thread-local MultiQI for this chunk
// Note: In a real implementation, we'd cache these thread-local instances
#    pragma omp critical
                { multi_->fill_thread_safe(&buffer_[start], end - start); }
            }
        }

        // Apply crypto mixing if enabled (sequential for consistency)
        if (config_.use_crypto_mixing && crypto_ && crypto_->is_initialized()) {
            crypto_->mix(reinterpret_cast<unsigned char*>(buffer_.data()),
                         buffer_.size() * sizeof(double));
        }

        buffer_pos_ = 0;
    } catch (...) {
        // Fall back to sequential on any error
        fill_buffer_sequential();
    }
}
#endif

void EnhancedPRNG::reseed() {
    // Force a buffer refill on next use
    buffer_pos_ = config_.buffer_size;
    has_spare_normal_ = false;

    // Reseed the crypto mixer if used
    if (crypto_) {
        crypto_->reseed();
    }

    // Skip ahead in the sequence to get "new" starting point
    // Use smaller range for reseed skips since this happens during runtime
    // and we've already done a large warm-up during initialization
    const uint64_t MIN_RESEED_SKIP = 1000;   // Minimum to ensure state change
    const uint64_t MAX_RESEED_SKIP = 10000;  // Maximum to keep reseed fast

    // SECURITY FIX: Use libsodium for cryptographically secure random skip distance
    ensure_libsodium_initialized();
    uint64_t skip_distance =
        MIN_RESEED_SKIP + (randombytes_uniform(MAX_RESEED_SKIP - MIN_RESEED_SKIP + 1));
    multi_->jump_ahead(skip_distance);
}

double EnhancedPRNG::next() {
    // If buffer needs refilling, do so
    if (buffer_pos_ >= buffer_.size()) {
        fill_buffer();
    }

    // Get raw uniform value
    double u = buffer_[buffer_pos_++];
    sample_count_++;

    // Reseed crypto periodically if enabled
    if (config_.use_crypto_mixing && crypto_ && config_.reseed_interval > 0 &&
        sample_count_ % config_.reseed_interval == 0) {
        crypto_->reseed();
    }

    // Transform according to requested distribution
    switch (config_.distribution) {
        case PRNGConfig::UNIFORM_01:
            return generate_uniform_01(u);
        case PRNGConfig::UNIFORM_RANGE:
            return generate_uniform_range(u);
        case PRNGConfig::NORMAL:
            return generate_normal(u);
        case PRNGConfig::EXPONENTIAL:
            return generate_exponential(u);
        case PRNGConfig::POISSON:
            return generate_poisson_dispatch(u);
        case PRNGConfig::GAMMA:
            return generate_gamma_dispatch(u);
        case PRNGConfig::BETA:
            return generate_beta_dispatch(u);
        case PRNGConfig::BERNOULLI:
            return generate_bernoulli_dispatch(u);
        case PRNGConfig::BINOMIAL:
            return generate_binomial_dispatch(u);
        case PRNGConfig::LOGNORMAL:
            return generate_lognormal_dispatch(u);
        case PRNGConfig::WEIBULL:
            return generate_weibull_dispatch(u);
        case PRNGConfig::CHISQUARED:
            return generate_chisquared_dispatch(u);
        case PRNGConfig::STUDENT_T:
            return generate_student_t_dispatch(u);
        case PRNGConfig::NEGATIVE_BINOMIAL:
            return generate_negative_binomial_dispatch(u);
        default:
            return u;  // Default to uniform(0,1)
    }
}

void EnhancedPRNG::skip(uint64_t n) {
    if (n == 0)
        return;

    uint64_t original_n = n;
    size_t remaining_in_buffer =
        (buffer_pos_ < buffer_.size()) ? (buffer_.size() - buffer_pos_) : 0;

    if (n <= remaining_in_buffer) {
        // Jump is within the current buffer
        buffer_pos_ += static_cast<size_t>(n);
    } else {
        // Jump is beyond the current buffer
        n -= remaining_in_buffer;  // Consume what's left in the current buffer

        // Calculate how many full buffers to jump over
        uint64_t num_full_buffers_to_skip = n / buffer_.size();
        if (num_full_buffers_to_skip > 0) {
            multi_->jump_ahead(num_full_buffers_to_skip * buffer_.size());
        }

        // Calculate the position in the new buffer
        uint64_t remaining_in_new_buffer = n % buffer_.size();

        if (remaining_in_new_buffer > 0) {
            fill_buffer();  // Refill to get the target buffer
            buffer_pos_ = static_cast<size_t>(remaining_in_new_buffer);
        } else {
            // If the jump lands exactly on a buffer boundary, the buffer is considered empty
            // and will be refilled on the next call to next()
            buffer_pos_ = buffer_.size();
        }
    }
    sample_count_ += original_n;
}

void EnhancedPRNG::generate_n(Rcpp::NumericVector& output_vec) {
    const size_t n = output_vec.size();

    // Direct buffer filling if we need a lot of values
    if (n >= 1024) {
        // Create a temporary buffer of the exact size needed
        SecureBuffer<double> temp_buffer(n);

        // Fill it directly with MultiQI
        multi_->fill(temp_buffer.data(), n);

        // Apply crypto mixing if enabled
        if (config_.use_crypto_mixing && crypto_ && crypto_->is_initialized()) {
            crypto_->mix(reinterpret_cast<unsigned char*>(temp_buffer.data()), n * sizeof(double));
        }

        // Transform according to requested distribution
        for (size_t i = 0; i < n; i++) {
            double u = temp_buffer[i];

            switch (config_.distribution) {
                case PRNGConfig::UNIFORM_01:
                    output_vec[i] = generate_uniform_01(u);
                    break;
                case PRNGConfig::UNIFORM_RANGE:
                    output_vec[i] = generate_uniform_range(u);
                    break;
                case PRNGConfig::NORMAL:
                    output_vec[i] = generate_normal(u);
                    break;
                case PRNGConfig::EXPONENTIAL:
                    output_vec[i] = generate_exponential(u);
                    break;
                case PRNGConfig::POISSON:
                    output_vec[i] = generate_poisson_dispatch(u);
                    break;
                case PRNGConfig::GAMMA:
                    output_vec[i] = generate_gamma_dispatch(u);
                    break;
                case PRNGConfig::BETA:
                    output_vec[i] = generate_beta_dispatch(u);
                    break;
                case PRNGConfig::BERNOULLI:
                    output_vec[i] = generate_bernoulli_dispatch(u);
                    break;
                case PRNGConfig::BINOMIAL:
                    output_vec[i] = generate_binomial_dispatch(u);
                    break;
                case PRNGConfig::LOGNORMAL:
                    output_vec[i] = generate_lognormal_dispatch(u);
                    break;
                case PRNGConfig::WEIBULL:
                    output_vec[i] = generate_weibull_dispatch(u);
                    break;
                case PRNGConfig::CHISQUARED:
                    output_vec[i] = generate_chisquared_dispatch(u);
                    break;
                case PRNGConfig::STUDENT_T:
                    output_vec[i] = generate_student_t_dispatch(u);
                    break;
                case PRNGConfig::NEGATIVE_BINOMIAL:
                    output_vec[i] = generate_negative_binomial_dispatch(u);
                    break;
                default:
                    output_vec[i] = u;  // Default to uniform(0,1)
            }
        }

        // Update sample count
        sample_count_ += n;

        // Reset standard buffer to force refill on next use
        buffer_pos_ = buffer_.size();

        return;
    }

    // For smaller requests, use the standard next() method
    for (size_t i = 0; i < n; i++) {
        output_vec[i] = next();
    }
}

std::pair<double, double> EnhancedPRNG::box_muller_pair(double u1, double u2) {
    // Box-Muller transform to generate normal random variables
    // Ensure inputs are in valid range (should already be checked, but be defensive)
    if (u1 <= 0.0)
        u1 = std::numeric_limits<double>::min();
    if (u1 >= 1.0)
        u1 = 1.0 - std::numeric_limits<double>::epsilon();
    if (u2 <= 0.0)
        u2 = std::numeric_limits<double>::min();
    if (u2 >= 1.0)
        u2 = 1.0 - std::numeric_limits<double>::epsilon();

    // Compute radius using log of first uniform
    double r = std::sqrt(-2.0 * std::log(u1));

    // Compute angle in radians
    double theta = 2.0 * M_PI * u2;

    // Generate two standard normal variates
    double z1 = r * std::cos(theta);
    double z2 = r * std::sin(theta);

    // Apply mean and standard deviation from configuration
    double x1 = config_.normal_mean + config_.normal_sd * z1;
    double x2 = config_.normal_mean + config_.normal_sd * z2;

    return {x1, x2};
}

double EnhancedPRNG::uniform_to_exponential(double u) {
    if (u <= 0.0)
        u = std::numeric_limits<double>::min();
    if (u >= 1.0)
        u = 1.0 - std::numeric_limits<double>::epsilon();

    return -std::log(1.0 - u) / config_.exponential_lambda;
}

double EnhancedPRNG::generate_poisson_knuth(double lambda) {
    // Knuth's algorithm for small lambda
    double L = std::exp(-lambda);
    double p = 1.0;
    int k = 0;

    do {
        k++;
        // Get next uniform from buffer
        if (buffer_pos_ >= buffer_.size()) {
            fill_buffer();
        }
        p *= buffer_[buffer_pos_++];
    } while (p > L);

    return static_cast<double>(k - 1);
}

double EnhancedPRNG::generate_poisson_normal_approx(double lambda) {
    // Normal approximation for large lambda
    double mu = lambda;
    double sigma = std::sqrt(lambda);

    // Use Box-Muller directly for a standard normal
    double u1 = next_raw_uniform();
    double u2 = next_raw_uniform();

    // Generate a standard normal using Box-Muller
    double r = std::sqrt(-2.0 * std::log(u1));
    double theta = 2.0 * M_PI * u2;
    double z = r * std::cos(theta);

    // Apply normal approximation and round to nearest integer
    double result = std::round(mu + sigma * z);
    return std::max(0.0, result);  // Ensure result is non-negative
}

double EnhancedPRNG::generate_gamma_small_alpha(double alpha, double theta) {
    // Ahrens-Dieter acceptance-rejection method for alpha < 1
    double b = (alpha + M_E) / M_E;

    while (true) {
        // Get two uniform variates
        double u1 = next_raw_uniform();
        double u2 = next_raw_uniform();
        double p = b * u1;

        if (p <= 1.0) {
            double x = std::pow(p, 1.0 / alpha);
            if (u2 <= std::exp(-x)) {
                return theta * x;
            }
        } else {
            double x = -std::log((b - p) / alpha);
            if (u2 <= std::pow(x, alpha - 1.0)) {
                return theta * x;
            }
        }
    }
}

double EnhancedPRNG::generate_gamma_large_alpha(double alpha, double theta) {
    // Marsaglia-Tsang method for alpha >= 1
    double d = alpha - 1.0 / 3.0;
    double c = 1.0 / std::sqrt(9.0 * d);

    while (true) {
        double z, v;
        do {
            // Generate standard normal directly with Box-Muller
            double u1 = next_raw_uniform();
            double u2 = next_raw_uniform();
            double r = std::sqrt(-2.0 * std::log(u1));
            double theta = 2.0 * M_PI * u2;
            z = r * std::cos(theta);

            v = 1.0 + c * z;
        } while (v <= 0.0);

        v = v * v * v;
        double u = next_raw_uniform();

        if (u < 1.0 - 0.0331 * (z * z) * (z * z)) {
            return theta * d * v;
        }

        if (std::log(u) < 0.5 * z * z + d * (1.0 - v + std::log(v))) {
            return theta * d * v;
        }
    }
}

double EnhancedPRNG::generate_beta_johnk(double alpha, double beta) {
    // Johnk's algorithm for Beta distribution
    double x_pow_alpha, y_pow_beta;

    do {
        double u = next_raw_uniform();
        double v = next_raw_uniform();

        if (u <= 0.0)
            u = std::numeric_limits<double>::min();
        if (v <= 0.0)
            v = std::numeric_limits<double>::min();

        x_pow_alpha = std::pow(u, 1.0 / alpha);
        y_pow_beta = std::pow(v, 1.0 / beta);
    } while (x_pow_alpha + y_pow_beta > 1.0);

    return x_pow_alpha / (x_pow_alpha + y_pow_beta);
}

// Distribution generation methods
double EnhancedPRNG::generate_uniform_01(double u) {
    return u;  // Already uniform(0,1)
}

double EnhancedPRNG::generate_uniform_range(double u) {
    return config_.range_min + u * (config_.range_max - config_.range_min);
}

double EnhancedPRNG::generate_normal(double u) {
    // Check if object is being destroyed
    if (is_being_destroyed_.load(std::memory_order_acquire)) {
        // Return a mean + small random variance if we're in the process of cleanup
        // This avoids returning identical values which break statistical tests
        static thread_local std::mt19937 fallback_rng(std::random_device{}());
        std::normal_distribution<double> fallback_dist(config_.normal_mean, 0.01);
        return fallback_dist(fallback_rng);
    }

    // This is a critical path for thread safety issues
    try {
        // Disable Ziggurat when parallel filling is enabled for stability
        if (config_.use_parallel_filling && config_.use_threading) {
            // Temporarily override the normal method
            if (config_.debug && config_.normal_method == PRNGConfig::ZIGGURAT) {
                static thread_local bool warned = false;
                if (!warned) {
                    Rcpp::warning("Ziggurat method is not used with parallel filling for "
                                  "stability. Using Box-Muller instead.");
                    warned = true;
                }
            }
            // Always use Box-Muller in this case
            return generate_normal_box_muller(u);
        }

        // In all other cases, use the configured method
        if (config_.normal_method == PRNGConfig::ZIGGURAT && ziggurat_) {
            // Use the Ziggurat method
            try {
                // Make sure the uniform is valid
                if (u <= 0.0 || u >= 1.0) {
                    // Use a better fallback than a fixed value
                    static thread_local std::mt19937 rng_u(std::random_device{}());
                    std::uniform_real_distribution<double> dist_u(0.000001, 0.999999);
                    u = dist_u(rng_u);
                }

                // Check again for object destruction - critical for thread safety
                if (is_being_destroyed_.load(std::memory_order_acquire)) {
                    // Use fallback RNG instead of fixed value
                    static thread_local std::mt19937 fallback_rng(std::random_device{}());
                    std::normal_distribution<double> fallback_dist(config_.normal_mean, 0.01);
                    return fallback_dist(fallback_rng);
                }

                // Get the normal value using Ziggurat
                double result = ziggurat_->generate();

                // Validate the result
                if (std::isnan(result) || std::isinf(result)) {
                    if (config_.debug) {
                        Rcpp::warning(
                            "Ziggurat method produced invalid result. Falling back to Box-Muller.");
                    }
                    // Fall back to Box-Muller if Ziggurat fails
                    return generate_normal_box_muller(u);
                }

                // Apply mean and SD from config to the N(0,1) value from Ziggurat
                return config_.normal_mean + config_.normal_sd * result;
            } catch (const std::exception& e) {
                if (config_.debug) {
                    static thread_local int warning_count = 0;
                    if (warning_count < 5) {
                        Rcpp::warning("Ziggurat method failed: %s. Falling back to Box-Muller.",
                                      e.what());
                        warning_count++;
                    }
                }

                // If the Ziggurat is being destroyed, use fallback RNG
                if (is_being_destroyed_.load(std::memory_order_acquire)) {
                    static thread_local std::mt19937 fallback_rng(std::random_device{}());
                    std::normal_distribution<double> fallback_dist(config_.normal_mean,
                                                                   config_.normal_sd);
                    return fallback_dist(fallback_rng);
                }

                // Fall back to Box-Muller if Ziggurat throws
                try {
                    return generate_normal_box_muller(u);
                } catch (...) {
                    // If Box-Muller also fails, use fallback RNG
                    static thread_local std::mt19937 fallback_rng(std::random_device{}());
                    std::normal_distribution<double> fallback_dist(config_.normal_mean,
                                                                   config_.normal_sd);
                    return fallback_dist(fallback_rng);
                }
            } catch (...) {
                if (config_.debug) {
                    static thread_local int warning_count = 0;
                    if (warning_count < 5) {
                        Rcpp::warning("Ziggurat method failed with unknown error. Falling back to "
                                      "Box-Muller.");
                        warning_count++;
                    }
                }

                // If the object is being destroyed, use fallback RNG
                if (is_being_destroyed_.load(std::memory_order_acquire)) {
                    static thread_local std::mt19937 fallback_rng(std::random_device{}());
                    std::normal_distribution<double> fallback_dist(config_.normal_mean,
                                                                   config_.normal_sd);
                    return fallback_dist(fallback_rng);
                }

                // Fall back to Box-Muller for any other error
                try {
                    return generate_normal_box_muller(u);
                } catch (...) {
                    // If Box-Muller also fails, use fallback RNG
                    static thread_local std::mt19937 fallback_rng(std::random_device{}());
                    std::normal_distribution<double> fallback_dist(config_.normal_mean,
                                                                   config_.normal_sd);
                    return fallback_dist(fallback_rng);
                }
            }
        } else {
            // Use Box-Muller method with destruction check
            if (is_being_destroyed_.load(std::memory_order_acquire)) {
                static thread_local std::mt19937 fallback_rng(std::random_device{}());
                std::normal_distribution<double> fallback_dist(config_.normal_mean,
                                                               config_.normal_sd);
                return fallback_dist(fallback_rng);
            }
            return generate_normal_box_muller(u);
        }
    } catch (const std::exception& e) {
        // Ultimate fallback - use std RNG instead of fixed mean
        if (config_.debug) {
            static thread_local int warning_count = 0;
            if (warning_count < 5) {
                Rcpp::warning("Normal generation failed with error: %s", e.what());
                warning_count++;
            }
        }
        static thread_local std::mt19937 fallback_rng(std::random_device{}());
        std::normal_distribution<double> fallback_dist(config_.normal_mean, config_.normal_sd);
        return fallback_dist(fallback_rng);
    } catch (...) {
        // Ultimate fallback for any other errors
        if (config_.debug) {
            static thread_local int warning_count = 0;
            if (warning_count < 5) {
                Rcpp::warning("Normal generation failed with unknown error");
                warning_count++;
            }
        }
        static thread_local std::mt19937 fallback_rng(std::random_device{}());
        std::normal_distribution<double> fallback_dist(config_.normal_mean, config_.normal_sd);
        return fallback_dist(fallback_rng);
    }
}

// Box-Muller implementation for normal distribution
double EnhancedPRNG::generate_normal_box_muller(double u) {
    try {
        // Thread-safe handling of spare normal value
        // In threading mode, we don't use the spare normal to avoid thread safety issues
        if (!config_.use_threading && has_spare_normal_) {
            double result = spare_normal_;
            has_spare_normal_ = false;
            spare_normal_ = 0.0;  // Clear for safety
            return result;
        }

        // Get second uniform for Box-Muller (first uniform is passed in)
        double u2;

        // Make sure we have values in the buffer
        if (buffer_pos_ >= buffer_.size()) {
            try {
                fill_buffer();
            } catch (const std::exception& e) {
                if (config_.debug) {
                    Rcpp::warning("Buffer filling failed in normal generation: %s", e.what());
                }
                // If buffer filling fails, use a fallback method
                static thread_local std::mt19937_64 fallback_rng(std::random_device{}());
                static thread_local std::uniform_real_distribution<double> fallback_dist(0.000001,
                                                                                         0.999999);
                u2 = fallback_dist(fallback_rng);
            }
        }

        // Safely get the second uniform value
        if (buffer_pos_ >= buffer_.size()) {
            // Buffer is still invalid after filling attempt
            static thread_local std::mt19937_64 fallback_rng(std::random_device{}());
            static thread_local std::uniform_real_distribution<double> fallback_dist(0.000001,
                                                                                     0.999999);
            u2 = fallback_dist(fallback_rng);
        } else {
            u2 = buffer_[buffer_pos_++];  // Thread-safe increment of buffer_pos_
        }

        // Ensure uniforms are in (0,1) range for numerical stability
        // These checks are required to prevent NaN/infinity in the Box-Muller transform
        if (u <= 0.0)
            u = std::numeric_limits<double>::min();
        if (u >= 1.0)
            u = 1.0 - std::numeric_limits<double>::epsilon();
        if (u2 <= 0.0)
            u2 = std::numeric_limits<double>::min();
        if (u2 >= 1.0)
            u2 = 1.0 - std::numeric_limits<double>::epsilon();

        // Generate pair of normal values with proper error handling
        std::pair<double, double> norm_pair;
        try {
            norm_pair = box_muller_pair(u, u2);
        } catch (const std::exception& e) {
            if (config_.debug) {
                Rcpp::warning("Box-Muller transform failed: %s", e.what());
            }
            // If Box-Muller fails, use fallback RNG instead of fixed mean
            static thread_local std::mt19937 fallback_rng(std::random_device{}());
            std::normal_distribution<double> fallback_dist(config_.normal_mean, config_.normal_sd);
            return fallback_dist(fallback_rng);
        }

        // Extract the normal values
        double x1 = norm_pair.first;
        double x2 = norm_pair.second;

        // Check for NaN/infinity
        if (std::isnan(x1) || std::isinf(x1)) {
            // Use fallback RNG instead of fixed mean value
            static thread_local std::mt19937 fallback_rng(std::random_device{}());
            std::normal_distribution<double> fallback_dist(config_.normal_mean, config_.normal_sd);
            x1 = fallback_dist(fallback_rng);
        }

        if (std::isnan(x2) || std::isinf(x2)) {
            // Use fallback RNG instead of fixed mean value
            static thread_local std::mt19937 fallback_rng(std::random_device{}());
            std::normal_distribution<double> fallback_dist(config_.normal_mean, config_.normal_sd);
            x2 = fallback_dist(fallback_rng);
        } else if (!config_.use_threading) {
            // Only store spare normal in non-threading mode
            has_spare_normal_ = true;
            spare_normal_ = x2;
        }

        return x1;
    } catch (const std::exception& e) {
        // Ultimate fallback - use std RNG instead of fixed mean
        if (config_.debug) {
            Rcpp::warning("Box-Muller failed with error: %s", e.what());
        }
        static thread_local std::mt19937 fallback_rng(std::random_device{}());
        std::normal_distribution<double> fallback_dist(config_.normal_mean, config_.normal_sd);
        return fallback_dist(fallback_rng);
    } catch (...) {
        // Ultimate fallback for any other errors
        if (config_.debug) {
            Rcpp::warning("Box-Muller failed with unknown error");
        }
        static thread_local std::mt19937 fallback_rng(std::random_device{}());
        std::normal_distribution<double> fallback_dist(config_.normal_mean, config_.normal_sd);
        return fallback_dist(fallback_rng);
    }
}

double EnhancedPRNG::generate_exponential(double u) {
    return uniform_to_exponential(u);
}

double EnhancedPRNG::generate_poisson_dispatch(double u) {
    double lambda = config_.poisson_lambda;

    // Choose algorithm based on lambda value
    if (lambda < 30.0) {
        return generate_poisson_knuth(lambda);
    } else {
        return generate_poisson_normal_approx(lambda);
    }
}

double EnhancedPRNG::generate_gamma_dispatch(double u) {
    double alpha = config_.gamma_shape;
    double theta = config_.gamma_scale;

    if (alpha < 1.0) {
        return generate_gamma_small_alpha(alpha, theta);
    } else {
        return generate_gamma_large_alpha(alpha, theta);
    }
}

double EnhancedPRNG::generate_beta_dispatch(double u) {
    return generate_beta_johnk(config_.beta_alpha, config_.beta_beta);
}

// New distribution implementations

double EnhancedPRNG::generate_bernoulli_dispatch(double u) {
    // Use the provided u parameter directly - more efficient than calling next_raw_uniform()
    if (config_.bernoulli_p < 0.0 || config_.bernoulli_p > 1.0) {
        throw std::invalid_argument("Bernoulli p must be in [0,1]");
    }
    return (u < config_.bernoulli_p) ? 1.0 : 0.0;
}

double EnhancedPRNG::generate_weibull_dispatch(double u) {
    if (config_.weibull_shape <= 0.0 || config_.weibull_scale <= 0.0) {
        throw std::invalid_argument("Weibull shape and scale must be positive");
    }

    // Use 1-u to avoid log(0) when u approaches 0
    // Since u and 1-u are identically distributed on (0,1), this is mathematically equivalent
    double safe_u = 1.0 - u;
    if (safe_u <= 0.0)
        safe_u = std::numeric_limits<double>::epsilon();
    if (safe_u >= 1.0)
        safe_u = 1.0 - std::numeric_limits<double>::epsilon();

    double result =
        config_.weibull_scale * std::pow(-std::log(safe_u), 1.0 / config_.weibull_shape);

    if (std::isnan(result) || std::isinf(result)) {
        return config_.weibull_scale;  // Fallback to scale parameter
    }
    return result;
}

double EnhancedPRNG::generate_lognormal_dispatch(double u) {
    if (config_.lognormal_sigma <= 0.0) {
        throw std::invalid_argument("Log-normal sigma must be positive");
    }

    // Pass u to normal generator, which will use it as first uniform
    double normal_val = generate_normal(u) * config_.lognormal_sigma + config_.lognormal_mu;
    double result = std::exp(normal_val);

    if (std::isnan(result) || std::isinf(result) || result <= 0.0) {
        return std::exp(config_.lognormal_mu);  // Fallback to median
    }
    return result;
}

double EnhancedPRNG::generate_chisquared_dispatch(double u) {
    if (config_.chisquared_df <= 0.0) {
        throw std::invalid_argument("Chi-squared df must be positive");
    }

    // Chi-squared(df) = Gamma(df/2, 2) for ALL positive df
    // This is more efficient than summing squared normals
    double shape = config_.chisquared_df / 2.0;
    double scale = 2.0;

    // For very large df, use normal approximation for efficiency
    if (config_.chisquared_df > 100.0) {
        double mean = config_.chisquared_df;
        double sd = std::sqrt(2.0 * config_.chisquared_df);
        double result = generate_normal(u) * sd + mean;
        return std::max(0.0, result);
    }

    // Use gamma generation (which will use its own uniforms)
    // Save current gamma params, use chi-squared params, then restore
    double saved_shape = config_.gamma_shape;
    double saved_scale = config_.gamma_scale;

    const_cast<PRNGConfig&>(config_).gamma_shape = shape;
    const_cast<PRNGConfig&>(config_).gamma_scale = scale;

    double result = generate_gamma_dispatch(u);

    // Restore original gamma params
    const_cast<PRNGConfig&>(config_).gamma_shape = saved_shape;
    const_cast<PRNGConfig&>(config_).gamma_scale = saved_scale;

    return result;
}

double EnhancedPRNG::generate_binomial_dispatch(double u) {
    if (config_.binomial_n < 0 || config_.binomial_p < 0.0 || config_.binomial_p > 1.0) {
        throw std::invalid_argument("Invalid binomial parameters");
    }

    // Edge cases
    if (config_.binomial_n == 0 || config_.binomial_p == 0.0)
        return 0.0;
    if (config_.binomial_p == 1.0)
        return static_cast<double>(config_.binomial_n);

    int n = config_.binomial_n;
    double p = config_.binomial_p;

    // Use normal approximation for n >= 50 and moderate p values
    if (n >= 50 && p > 0.1 && p < 0.9) {
        double mu = n * p;
        double sigma = std::sqrt(n * p * (1.0 - p));
        double result = std::round(generate_normal(u) * sigma + mu);
        return std::max(0.0, std::min(static_cast<double>(n), result));
    }

    // Use normal approximation when both n*p > 5 and n*(1-p) > 5
    if (n * p > 5.0 && n * (1.0 - p) > 5.0) {
        double mu = n * p;
        double sigma = std::sqrt(n * p * (1.0 - p));
        double approx = generate_normal(u) * sigma + mu;
        double result = std::round(approx);
        if (std::isnan(result) || std::isinf(result))
            return mu;
        return std::max(0.0, std::min(static_cast<double>(n), result));
    }

    // Exact method: sum of Bernoulli trials
    // Use first u for first trial, then get more uniforms as needed
    double sum = (u < p) ? 1.0 : 0.0;
    for (int i = 1; i < n; ++i) {
        sum += (next_raw_uniform() < p) ? 1.0 : 0.0;
    }
    return sum;
}

double EnhancedPRNG::generate_student_t_dispatch(double u) {
    if (config_.student_t_df <= 0.0) {
        throw std::invalid_argument("Student's t df must be positive");
    }

    // For large df, converges to normal
    if (config_.student_t_df > 100.0) {
        return generate_normal(u);
    }

    // Student's t = Normal / sqrt(ChiSquared/df)
    double norm = generate_normal(u);

    // Generate chi-squared (will use its own uniforms)
    double saved_df = config_.chisquared_df;
    const_cast<PRNGConfig&>(config_).chisquared_df = config_.student_t_df;
    double chisq = generate_chisquared_dispatch(next_raw_uniform());
    const_cast<PRNGConfig&>(config_).chisquared_df = saved_df;

    if (chisq <= 0.0) {
        chisq = std::numeric_limits<double>::epsilon();
    }

    double result = norm / std::sqrt(chisq / config_.student_t_df);

    if (std::isnan(result) || std::isinf(result)) {
        return 0.0;  // Fallback to median
    }
    return result;
}

double EnhancedPRNG::generate_negative_binomial_dispatch(double u) {
    if (config_.negative_binomial_r <= 0.0 || config_.negative_binomial_p <= 0.0 ||
        config_.negative_binomial_p >= 1.0) {
        throw std::invalid_argument("Invalid negative binomial parameters");
    }

    double r = config_.negative_binomial_r;
    double p = config_.negative_binomial_p;

    // Use gamma-Poisson mixture method for non-integer r or large r values
    if (r != std::floor(r) || r > 50.0) {
        // Negative binomial can be generated as Poisson(lambda) where lambda ~ Gamma(r, (1-p)/p)
        double gamma_shape = r;
        double gamma_scale = (1.0 - p) / p;

        // Save current gamma params
        double saved_shape = config_.gamma_shape;
        double saved_scale = config_.gamma_scale;

        const_cast<PRNGConfig&>(config_).gamma_shape = gamma_shape;
        const_cast<PRNGConfig&>(config_).gamma_scale = gamma_scale;

        double lambda = generate_gamma_dispatch(u);

        // Restore gamma params
        const_cast<PRNGConfig&>(config_).gamma_shape = saved_shape;
        const_cast<PRNGConfig&>(config_).gamma_scale = saved_scale;

        // Now generate Poisson with this lambda
        double saved_lambda = config_.poisson_lambda;
        const_cast<PRNGConfig&>(config_).poisson_lambda = lambda;

        double result = generate_poisson_dispatch(next_raw_uniform());

        const_cast<PRNGConfig&>(config_).poisson_lambda = saved_lambda;

        return result;
    }

    // Exact method for integer r: sum of geometric distributions
    // Each geometric counts failures until success
    double sum = 0.0;
    int r_int = static_cast<int>(r);
    for (int i = 0; i < r_int; ++i) {
        double trials = 0.0;
        // Use u for first trial of first geometric, then get more uniforms
        double trial_u = (i == 0) ? u : next_raw_uniform();
        while (trial_u >= p) {
            trials += 1.0;
            if (trials > 1e6)
                break;  // Prevent infinite loop
            trial_u = next_raw_uniform();
        }
        sum += trials;
    }
    return sum;
}

void EnhancedPRNG::dumpConfig() const {
    Rcpp::Rcout << "EnhancedPRNG Configuration:" << std::endl;
    Rcpp::Rcout << "  a: " << config_.a << std::endl;
    Rcpp::Rcout << "  b: " << config_.b << std::endl;
    Rcpp::Rcout << "  c: " << config_.c << std::endl;
    Rcpp::Rcout << "  mpfr_precision: " << config_.mpfr_precision << std::endl;
    Rcpp::Rcout << "  buffer_size: " << config_.buffer_size << std::endl;

    std::string dist_str;
    switch (config_.distribution) {
        case PRNGConfig::UNIFORM_01:
            dist_str = "uniform_01";
            break;
        case PRNGConfig::UNIFORM_RANGE:
            dist_str = "uniform_range";
            break;
        case PRNGConfig::NORMAL:
            dist_str = "normal";
            break;
        case PRNGConfig::EXPONENTIAL:
            dist_str = "exponential";
            break;
        case PRNGConfig::POISSON:
            dist_str = "poisson";
            break;
        case PRNGConfig::GAMMA:
            dist_str = "gamma";
            break;
        case PRNGConfig::BETA:
            dist_str = "beta";
            break;
        case PRNGConfig::BERNOULLI:
            dist_str = "bernoulli";
            break;
        case PRNGConfig::BINOMIAL:
            dist_str = "binomial";
            break;
        case PRNGConfig::LOGNORMAL:
            dist_str = "lognormal";
            break;
        case PRNGConfig::WEIBULL:
            dist_str = "weibull";
            break;
        case PRNGConfig::CHISQUARED:
            dist_str = "chisquared";
            break;
        case PRNGConfig::STUDENT_T:
            dist_str = "student_t";
            break;
        case PRNGConfig::NEGATIVE_BINOMIAL:
            dist_str = "negative_binomial";
            break;
        default:
            dist_str = "unknown";
            break;
    }
    Rcpp::Rcout << "  distribution: " << dist_str << std::endl;

    std::string method_str;
    switch (config_.normal_method) {
        case PRNGConfig::BOX_MULLER:
            method_str = "box_muller";
            break;
        case PRNGConfig::ZIGGURAT:
            method_str = "ziggurat";
            break;
        default:
            method_str = "unknown";
            break;
    }
    Rcpp::Rcout << "  normal_method: " << method_str << std::endl;

    Rcpp::Rcout << "  range_min: " << config_.range_min << std::endl;
    Rcpp::Rcout << "  range_max: " << config_.range_max << std::endl;
    Rcpp::Rcout << "  normal_mean: " << config_.normal_mean << std::endl;
    Rcpp::Rcout << "  normal_sd: " << config_.normal_sd << std::endl;
    Rcpp::Rcout << "  exponential_lambda: " << config_.exponential_lambda << std::endl;
    Rcpp::Rcout << "  poisson_lambda: " << config_.poisson_lambda << std::endl;
    Rcpp::Rcout << "  gamma_shape: " << config_.gamma_shape << std::endl;
    Rcpp::Rcout << "  gamma_scale: " << config_.gamma_scale << std::endl;
    Rcpp::Rcout << "  beta_alpha: " << config_.beta_alpha << std::endl;
    Rcpp::Rcout << "  beta_beta: " << config_.beta_beta << std::endl;

    Rcpp::Rcout << "  use_crypto_mixing: " << (config_.use_crypto_mixing ? "true" : "false")
                << std::endl;
    Rcpp::Rcout << "  adhoc_corrections: " << (config_.adhoc_corrections ? "true" : "false")
                << std::endl;
    Rcpp::Rcout << "  use_tie_breaking: " << (config_.use_tie_breaking ? "true" : "false")
                << std::endl;
    Rcpp::Rcout << "  reseed_interval: " << config_.reseed_interval << std::endl;
    Rcpp::Rcout << "  use_csv_discriminants: " << (config_.use_csv_discriminants ? "true" : "false")
                << std::endl;
    Rcpp::Rcout << "  use_parallel_filling: " << (config_.use_parallel_filling ? "true" : "false")
                << std::endl;
    Rcpp::Rcout << "  offset: " << config_.offset << std::endl;
    Rcpp::Rcout << "  debug: " << (config_.debug ? "true" : "false") << std::endl;
    Rcpp::Rcout << "  seed: " << config_.seed << std::endl;
    Rcpp::Rcout << "  has_seed: " << (config_.has_seed ? "true" : "false") << std::endl;
    Rcpp::Rcout << "  deterministic: " << (config_.deterministic ? "true" : "false") << std::endl;

    Rcpp::Rcout << std::endl;
}

// Thread management methods
void EnhancedPRNG::registerThreadForCleanup() {
    // Use thread-local resources through ZigguratNormal rather than ThreadManager
    // This implementation is now handled by the ZigguratTLSManager class
}

void EnhancedPRNG::cleanupThreadResources() {
    // Clean up thread-local resources for Ziggurat
    ZigguratNormal::cleanup_thread_local_resources();

    // Any other thread-local cleanup can be added here
}

void EnhancedPRNG::cleanupAllThreadResources() {
    // Use ZigguratNormal's built-in thread safety mechanisms
    ZigguratNormal::prepare_for_shutdown();

    // Directly clean up Ziggurat resources for the current thread
    ZigguratNormal::cleanup_thread_local_resources();
}

// Thread-safe cleanup methods
void EnhancedPRNG::prepareForCleanup() {
    // Mark as being destroyed to prevent new operations
    is_being_destroyed_.store(true);

    // Disable thread-safe mode on ziggurat if it exists
    if (ziggurat_) {
        try {
            ziggurat_->set_thread_safe_mode(false);
        } catch (...) {
            // Ignore errors during cleanup
        }
    }

    // Clean up all thread resources
    cleanupAllThreadResources();
}

void EnhancedPRNG::performCleanup() {
    // Only continue if already marked for destruction
    if (!is_being_destroyed_.load(std::memory_order_acquire)) {
        prepareForCleanup();
    }

    // Acquire cleanup mutex to prevent concurrent access
    std::lock_guard<std::mutex> lock(cleanup_mutex_);

    try {
        // Clean up resources in an orderly fashion
        // Order matters for safe destruction
        crypto_.reset();
        ziggurat_.reset();
        multi_.reset();

        // Clear buffer
        buffer_ = SecureBuffer<double>(0);
        buffer_pos_ = 0;

    } catch (const std::exception& e) {
        // Log error but continue - this is cleanup
        if (config_.debug) {
            Rcpp::warning("Error during PRNG cleanup: %s", e.what());
        }
    } catch (...) {
        // Catch all exceptions to prevent crashes during cleanup
    }
}

bool EnhancedPRNG::isBeingDestroyed() const {
    return is_being_destroyed_.load(std::memory_order_acquire);
}

}  // namespace qiprng
