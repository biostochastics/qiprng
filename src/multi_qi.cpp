// File: multi_qi.cpp
// --------------------------------------------------------------
#include "multi_qi.hpp"

#include <algorithm>  // For std::min, std::max
#include <cmath>      // For std::sin
#include <cstring>    // For memcpy
#include <numeric>    // For std::accumulate
#include <random>     // For random QI generation
#include <stdexcept>  // For std::runtime_error

#include "precision_utils.hpp"  // For high-precision constants and safe conversions
#include "simd_operations.hpp"  // v0.5.0: SIMD vectorization

// v0.5.0: OpenMP support for parallel generation
#ifdef _OPENMP
#    include <omp.h>
#    define QIPRNG_HAS_OPENMP 1
#else
#    define QIPRNG_HAS_OPENMP     0
#    define omp_get_num_threads() 1
#    define omp_get_thread_num()  0
#    define omp_get_max_threads() 1
#endif

namespace qiprng {

// Thread-safe fallback PRNG with lazy initialization to prevent races
struct ThreadLocalPRNG {
    std::mt19937_64 rng;
    std::uniform_real_distribution<double> dist{0.0, 1.0};
    std::once_flag init_flag;

    double generate() {
        std::call_once(init_flag, [this]() {
            std::random_device rd;
            rng.seed(rd());
        });
        return dist(rng);
    }
};

thread_local ThreadLocalPRNG tl_fallback;

// Thread-local cache to reduce lock contention
thread_local std::vector<double> tl_cache;
thread_local size_t tl_cache_pos = 0;
// Optimal cache size determined through benchmarking:
// - Small enough to fit in L1 cache (32KB typical)
// - Large enough to amortize lock acquisition overhead
// - Power of 2 for memory alignment and efficient indexing
// - 256 * 8 bytes = 2KB buffer, leaves room for other L1 data
const size_t CACHE_SIZE = 256;  // Batch size for reducing lock frequency

// Enhanced constructor with mixing strategy
MultiQI::MultiQI(const std::vector<std::tuple<long, long, long>>& abc_list, int mpfr_prec,
                 uint64_t seed, bool has_seed, MixingStrategy strategy)
    : idx_(0), mixing_strategy_(strategy), mix_counter_(0) {
    if (abc_list.empty()) {
        throw std::runtime_error("MultiQI: abc_list cannot be empty for initialization.");
    }
    qis_.reserve(abc_list.size());
    for (const auto& abc : abc_list) {
        long A, B, C;
        std::tie(A, B, C) = abc;
        // QuadraticIrrational constructor throws on error
        qis_.push_back(std::make_unique<QuadraticIrrational>(A, B, C, mpfr_prec, seed, has_seed));
    }

    // Initialize mixing buffers
    mixing_buffer_.resize(std::min(size_t(16), qis_.size()));

    // Initialize default weights for averaging
    weights_.resize(qis_.size(), 1.0 / qis_.size());
}

// Dynamic QI generation constructor for v0.5.0
MultiQI::MultiQI(size_t num_qis, int mpfr_prec, uint64_t seed, bool has_seed,
                 MixingStrategy strategy)
    : idx_(0), mixing_strategy_(strategy), mix_counter_(0) {
    if (num_qis == 0) {
        throw std::runtime_error("MultiQI: num_qis must be positive.");
    }

    qis_.reserve(num_qis);

    // Generate QIs with discriminant D = k^2 + 1 pattern
    std::mt19937_64 rng(seed);
    std::uniform_int_distribution<long> k_dist(1, 1000);

    for (size_t i = 0; i < num_qis; ++i) {
        long k = k_dist(rng);
        long discriminant_target = k * k + 1;

        // Generate a, b, c such that b^2 - 4ac ≈ discriminant_target
        // Using pattern: a = 1, c = -(D-b^2)/4, b = random odd
        long a = 1;
        long b = 2 * k_dist(rng) + 1;  // Ensure odd for better properties
        long c = -(discriminant_target - b * b) / 4;

        // Verify discriminant is positive and square-free
        long long disc = static_cast<long long>(b) * b - 4LL * a * c;
        if (disc > 0) {
            // Additional validation: ensure discriminant is actually square-free
            if (QuadraticIrrational::is_square_free(disc)) {
                qis_.push_back(
                    std::make_unique<QuadraticIrrational>(a, b, c, mpfr_prec, seed + i, has_seed));
            } else {
                // Try alternative parameters with guaranteed square-free discriminant
                // Use prime-based generation for better properties
                long p = 2 * k + 1;  // Odd value
                while (!QuadraticIrrational::is_square_free(p * p + 4)) {
                    p += 2;
                }
                qis_.push_back(
                    std::make_unique<QuadraticIrrational>(1, p, -1, mpfr_prec, seed + i, has_seed));
            }
        } else {
            // Fallback to a known good configuration with square-free discriminant = 5
            qis_.push_back(
                std::make_unique<QuadraticIrrational>(1, 1, -1, mpfr_prec, seed + i, has_seed));
        }
    }

    // Initialize mixing buffers
    mixing_buffer_.resize(std::min(size_t(16), qis_.size()));

    // Initialize default weights
    weights_.resize(qis_.size(), 1.0 / qis_.size());
}

// Helper method: Get value from cache if available
double MultiQI::getFromCache() {
    if (tl_cache_pos < tl_cache.size()) {
        return tl_cache[tl_cache_pos++];
    }
    return std::numeric_limits<double>::quiet_NaN();  // Indicates cache miss
}

// Helper method: Generate a single value with error handling
double MultiQI::generateSingleValue(QuadraticIrrational* qi) {
    if (!qi) {
        return tl_fallback.generate();
    }

    try {
        return qi->next();
    } catch (...) {
        return tl_fallback.generate();
    }
}

// Helper method: Refill cache with reduced lock duration
void MultiQI::refillCache() {
    // Prepare cache memory without lock
    if (tl_cache.capacity() < CACHE_SIZE) {
        tl_cache.reserve(CACHE_SIZE);  // One-time allocation
    }
    tl_cache.resize(0);  // Keep capacity but clear elements

    // Copy QI pointers and current index under minimal lock
    std::vector<QuadraticIrrational*> local_qis;
    size_t local_idx;
    {
        std::lock_guard<std::mutex> lock(mutex_);

        if (qis_.empty()) {
            // Fill cache with fallback values
            for (size_t i = 0; i < CACHE_SIZE; ++i) {
                tl_cache.push_back(tl_fallback.generate());
            }
            tl_cache_pos = 0;
            return;
        }

        // Copy pointers for lock-free generation
        local_qis.reserve(qis_.size());
        for (const auto& qi : qis_) {
            local_qis.push_back(qi.get());
        }
        local_idx = idx_;
    }

    // Generate values without holding lock
    for (size_t i = 0; i < CACHE_SIZE && !local_qis.empty(); ++i) {
        if (local_idx >= local_qis.size()) {
            local_idx = 0;
        }

        double val = generateSingleValue(local_qis[local_idx]);
        tl_cache.push_back(val);
        local_idx = (local_idx + 1) % local_qis.size();
    }

    // Update shared index with atomic operation for cache coherency
    // Use atomic exchange to ensure visibility across threads
    {
        std::lock_guard<std::mutex> lock(mutex_);
        // Memory barrier ensures all threads see consistent state
        std::atomic_thread_fence(std::memory_order_release);
        idx_ = local_idx;
        std::atomic_thread_fence(std::memory_order_acquire);
    }

    tl_cache_pos = 0;
}

double MultiQI::next() {
    // Try cache first (lock-free fast path)
    double cached = getFromCache();
    if (!std::isnan(cached)) {
        return cached;
    }

    // Cache miss - refill and return first value
    try {
        refillCache();
        return tl_cache.empty() ? tl_fallback.generate() : tl_cache[tl_cache_pos++];
    } catch (...) {
        // Ultimate fallback for any exception
        return tl_fallback.generate();
    }
}

void MultiQI::skip(uint64_t n) {
    jump_ahead(n);
}

void MultiQI::jump_ahead(uint64_t n) {
    std::lock_guard<std::mutex> lock(mutex_);
    if (qis_.empty() || n == 0) {
        return;
    }

    size_t num_qis = qis_.size();
    uint64_t num_full_jumps = n / num_qis;
    uint64_t remaining_jump = n % num_qis;

    // Each QI advances by the number of full rotations
    if (num_full_jumps > 0) {
        for (auto& qi : qis_) {
            qi->jump_ahead_optimized(num_full_jumps);
        }
    }

    // Handle the remaining jumps by advancing the next `remaining_jump` QIs one by one
    for (uint64_t i = 0; i < remaining_jump; ++i) {
        size_t current_qi_index = (idx_ + i) % num_qis;
        qis_[current_qi_index]->jump_ahead_optimized(1);
    }

    // Update the index to the new position
    idx_ = (idx_ + remaining_jump) % num_qis;
}

size_t MultiQI::size() const {
    return qis_.size();
}

void MultiQI::fill(double* buffer, size_t fill_size) {
    // Non-thread-safe version - uses the thread-safe next() but releases the lock between calls
    if (qis_.empty()) {
        // Fallback for empty state
        for (size_t i = 0; i < fill_size; ++i) {
            buffer[i] = 0.5;
        }
        return;
    }

    // Fill buffer using next() which has its own lock
    for (size_t i = 0; i < fill_size; i++) {
        buffer[i] = next();
    }
}

void MultiQI::fill_thread_safe(double* buffer, size_t fill_size) {
    // Safety check for null buffer
    if (!buffer) {
        return;
    }

    // Hold the lock for the entire operation - more efficient for large fills
    try {
        std::lock_guard<std::mutex> lock(mutex_);

        if (qis_.empty()) {
            // Fallback for empty state
            for (size_t i = 0; i < fill_size; ++i) {
                buffer[i] = 0.5;
            }
            return;
        }

        // Make sure index is valid
        if (idx_ >= qis_.size()) {
            idx_ = 0;
        }

        // v0.5.0: Use SIMD for batch operations when possible
        const bool use_simd = simd::is_simd_available() &&
                              fill_size >= simd::get_optimal_batch_size() &&
                              mixing_strategy_ == MixingStrategy::XOR_MIX;

        if (use_simd && qis_.size() >= 2) {
            // SIMD-optimized XOR mixing for large buffers
            size_t batch_size = simd::get_optimal_batch_size();
            size_t num_batches = fill_size / batch_size;

            // Temporary buffers for SIMD operations
            std::vector<double> temp1(batch_size);
            std::vector<double> temp2(batch_size);

            for (size_t batch = 0; batch < num_batches; ++batch) {
                // Fill temp buffers from two QIs
                for (size_t i = 0; i < batch_size; ++i) {
                    temp1[i] = qis_[0]->next();
                    temp2[i] = qis_[1]->next();
                }

                // Use SIMD XOR mixing
                simd::xor_mix_batch(buffer + batch * batch_size, temp1.data(), temp2.data(),
                                    batch_size);
            }

            // Handle remainder without SIMD
            size_t remainder_start = num_batches * batch_size;
            for (size_t i = remainder_start; i < fill_size; ++i) {
                buffer[i] = next();  // Use regular mixing for remainder
            }
        } else {
            // Non-SIMD path: use appropriate mixing strategy
            if (mixing_strategy_ == MixingStrategy::ROUND_ROBIN) {
                // Original round-robin implementation
                for (size_t i = 0; i < fill_size; i++) {
                    try {
                        // Check if the QI pointer is valid
                        if (qis_[idx_]) {
                            buffer[i] = qis_[idx_]->next();
                        } else {
                            buffer[i] = 0.5;  // Fallback for null QI
                        }
                    } catch (...) {
                        // If next() throws, use fallback
                        buffer[i] = 0.5;
                    }

                    // Safely advance the index
                    idx_ = (idx_ + 1) % qis_.size();
                }
            } else {
// Use mixed generation for other strategies
// Note: mutex is already held, so we need to call the internal mixing logic

// DEBUG: Print information
#ifdef DEBUG_MIXING
                Rcpp::Rcout << "DEBUG: Non-round-robin mixing, fill_size=" << fill_size
                            << ", qis_.size()=" << qis_.size()
                            << ", mixing_buffer_.size()=" << mixing_buffer_.size()
                            << ", strategy=" << static_cast<int>(mixing_strategy_) << std::endl;
#endif

                for (size_t i = 0; i < fill_size; i++) {
                    // Collect values for mixing - only use the first mix_size elements
                    size_t mix_size = std::min(mixing_buffer_.size(), qis_.size());
                    std::vector<double> temp_values(mix_size);

#ifdef DEBUG_MIXING
                    if (i == 0) {
                        Rcpp::Rcout << "DEBUG: mix_size=" << mix_size << ", idx_=" << idx_
                                    << std::endl;
                    }
#endif

                    for (size_t j = 0; j < mix_size; ++j) {
                        size_t qi_idx = (idx_ + j) % qis_.size();
                        try {
                            if (qis_[qi_idx]) {
                                temp_values[j] = qis_[qi_idx]->next();
                            } else {
                                temp_values[j] = 0.5;
                            }
                        } catch (...) {
                            // If next() throws, use fallback
                            temp_values[j] = 0.5;
                        }
                    }

                    // Advance index
                    idx_ = (idx_ + mix_size) % qis_.size();

                    // Apply mixing strategy using only the values we collected
                    switch (mixing_strategy_) {
                        case MixingStrategy::XOR_MIX:
                            buffer[i] = mix_xor(temp_values);
                            break;
                        case MixingStrategy::AVERAGING:
                            buffer[i] = mix_averaging(temp_values);
                            break;
                        case MixingStrategy::MODULAR_ADD:
                            buffer[i] = mix_modular_add(temp_values);
                            break;
                        case MixingStrategy::CASCADE_MIX:
                            buffer[i] = mix_cascade(temp_values);
                            break;
                        default:
                            buffer[i] = temp_values[0];
                    }
                }
            }
        }
    } catch (...) {
        // Ultimate fallback - fill with constant value
        for (size_t i = 0; i < fill_size; ++i) {
            buffer[i] = 0.5;
        }
    }
}

// Enhanced next with mixing strategy
double MultiQI::next_mixed() {
    if (mixing_strategy_ == MixingStrategy::ROUND_ROBIN) {
        return next();  // Use original round-robin
    }

    std::lock_guard<std::mutex> lock(mutex_);

    if (qis_.empty()) {
        return 0.5;
    }

    // Collect values for mixing
    size_t mix_size = std::min(mixing_buffer_.size(), qis_.size());
    for (size_t i = 0; i < mix_size; ++i) {
        size_t qi_idx = (idx_ + i) % qis_.size();
        if (qis_[qi_idx]) {
            mixing_buffer_[i] = qis_[qi_idx]->next();
        } else {
            mixing_buffer_[i] = 0.5;
        }
    }

    // Advance index
    idx_ = (idx_ + mix_size) % qis_.size();
    mix_counter_++;

    // Apply selected mixing strategy
    double result;
    switch (mixing_strategy_) {
        case MixingStrategy::XOR_MIX:
            result = mix_xor(mixing_buffer_);
            break;
        case MixingStrategy::AVERAGING:
            result = mix_averaging(mixing_buffer_);
            break;
        case MixingStrategy::MODULAR_ADD:
            result = mix_modular_add(mixing_buffer_);
            break;
        case MixingStrategy::CASCADE_MIX:
            result = mix_cascade(mixing_buffer_);
            break;
        default:
            result = mixing_buffer_[0];
    }

    return result;
}

// XOR-based mixing for bit diffusion
double MultiQI::mix_xor(const std::vector<double>& values) {
    if (values.empty())
        return 0.5;

    // For single value, we need to mix it with something to avoid poor randomness
    if (values.size() == 1) {
        // Mix with a prime-based transform to ensure good bit diffusion
        uint64_t mantissa = extract_mantissa(values[0]);
        mantissa ^= (mantissa << 13);
        mantissa ^= (mantissa >> 7);
        mantissa ^= (mantissa << 17);
        return static_cast<double>(mantissa) / static_cast<double>(0x000FFFFFFFFFFFFFULL);
    }

    uint64_t combined = 0;
    for (double val : values) {
        combined ^= extract_mantissa(val);
    }

    // Convert back to double in [0,1)
    // FIX: Use mantissa mask instead of UINT64_MAX for correct scaling
    return static_cast<double>(combined) / static_cast<double>(0x000FFFFFFFFFFFFFULL);
}

// Weighted averaging for smooth distribution
double MultiQI::mix_averaging(const std::vector<double>& values) {
    if (values.empty())
        return 0.5;

    double sum = 0.0;
    size_t count = std::min(values.size(), weights_.size());

    for (size_t i = 0; i < count; ++i) {
        sum += values[i] * weights_[i];
    }

    // Ensure result is in [0,1)
    return std::max(0.0, std::min(sum, 0.999999999999));
}

// Modular addition for entropy combining
double MultiQI::mix_modular_add(const std::vector<double>& values) {
    if (values.empty())
        return 0.5;

    double sum = 0.0;
    for (double val : values) {
        sum += val;
    }

    // Take fractional part to ensure [0,1)
    sum = std::fmod(sum, 1.0);
    if (sum < 0)
        sum += 1.0;

    return sum;
}

// Cascaded mixing for maximum entropy
double MultiQI::mix_cascade(const std::vector<double>& values) {
    if (values.empty())
        return 0.5;
    if (values.size() == 1)
        return values[0];

    // First pass: XOR mixing for bit diffusion
    uint64_t xor_result = 0;
    for (double val : values) {
        xor_result ^= extract_mantissa(val);
    }

    // Second pass: Modular addition with XOR result
    double sum = static_cast<double>(xor_result) / static_cast<double>(UINT64_MAX);
    for (double val : values) {
        sum = std::fmod(sum + val * 0.5, 1.0);
    }

    // Third pass: Apply non-linear transformation
    sum = std::sin(sum * 2.0 * M_PI) * 0.5 + 0.5;

    return std::max(0.0, std::min(sum, 0.999999999999));
}

// Extract mantissa bits from double
uint64_t MultiQI::extract_mantissa(double value) {
    uint64_t bits;
    std::memcpy(&bits, &value, sizeof(double));
    return bits & 0x000FFFFFFFFFFFFFULL;  // Extract 52-bit mantissa
}

// Combine two mantissas into a double
double MultiQI::combine_mantissas(uint64_t m1, uint64_t m2) {
    uint64_t combined = m1 ^ m2;
    return static_cast<double>(combined) / static_cast<double>(0x000FFFFFFFFFFFFFULL);
}

// Batch fill with mixing
void MultiQI::fill_mixed(double* buffer, size_t fill_size) {
    if (!buffer || fill_size == 0)
        return;

    for (size_t i = 0; i < fill_size; ++i) {
        buffer[i] = next_mixed();
    }
}

// v0.5.0: Parallel fill using OpenMP
void MultiQI::fill_parallel(double* buffer, size_t fill_size) {
    if (!buffer || fill_size == 0)
        return;

#if QIPRNG_HAS_OPENMP
    // Determine optimal number of threads
    int num_threads = omp_get_max_threads();
    size_t chunk_size = fill_size / num_threads;

    // Only use parallel execution for sufficiently large buffers
    if (fill_size >= 10000 && chunk_size >= 1000) {
        // Create thread-local QI instances for parallel generation
        std::vector<std::unique_ptr<MultiQI>> thread_qis(num_threads);

// Initialize thread-local QIs with different seeds
#    pragma omp parallel
        {
            int tid = omp_get_thread_num();

            // Create a copy of QI parameters with thread-specific seed
            std::vector<std::tuple<long, long, long>> abc_list;
            {
                std::lock_guard<std::mutex> lock(mutex_);
                for (const auto& qi : qis_) {
                    // Extract parameters (this is a simplification - in real impl,
                    // we'd need getter methods in QuadraticIrrational)
                    abc_list.push_back(std::make_tuple(1, 1 + tid * 2, -1));
                }
            }

// Each thread gets its own MultiQI with unique seed
#    pragma omp critical
            {
                thread_qis[tid] =
                    std::make_unique<MultiQI>(abc_list, 256, tid * 1000000, true, mixing_strategy_);
            }
        }

// Parallel generation
#    pragma omp parallel for schedule(static)
        for (size_t i = 0; i < fill_size; ++i) {
            int tid = omp_get_thread_num();
            buffer[i] = thread_qis[tid]->next();
        }
    } else {
        // Fall back to sequential generation for small buffers
        fill_mixed(buffer, fill_size);
    }
#else
    // No OpenMP available, use sequential generation
    fill_mixed(buffer, fill_size);
#endif
}

// Set mixing strategy
void MultiQI::set_mixing_strategy(MixingStrategy strategy) {
    std::lock_guard<std::mutex> lock(mutex_);
    mixing_strategy_ = strategy;
}

// Set weights for averaging
void MultiQI::set_weights(const std::vector<double>& weights) {
    std::lock_guard<std::mutex> lock(mutex_);

    if (weights.size() != qis_.size()) {
        throw std::invalid_argument("Weights vector size must match number of QIs");
    }

    // Normalize weights
    double sum = std::accumulate(weights.begin(), weights.end(), 0.0);
    if (sum <= 0) {
        throw std::invalid_argument("Weights must sum to positive value");
    }

    weights_.resize(weights.size());
    for (size_t i = 0; i < weights.size(); ++i) {
        weights_[i] = weights[i] / sum;
    }
}

// Add a new QI dynamically
void MultiQI::add_qi(long a, long b, long c, int mpfr_prec) {
    std::lock_guard<std::mutex> lock(mutex_);

    qis_.push_back(std::make_unique<QuadraticIrrational>(a, b, c, mpfr_prec));

    // Update weights
    weights_.resize(qis_.size(), 1.0 / qis_.size());
}

// Remove a QI by index
void MultiQI::remove_qi(size_t index) {
    std::lock_guard<std::mutex> lock(mutex_);

    if (index >= qis_.size()) {
        throw std::out_of_range("QI index out of range");
    }

    if (qis_.size() <= 1) {
        throw std::runtime_error("Cannot remove last QI");
    }

    qis_.erase(qis_.begin() + index);

    // Update weights
    weights_.resize(qis_.size(), 1.0 / qis_.size());

    // Adjust current index if needed
    if (idx_ >= qis_.size()) {
        idx_ = 0;
    }
}

// Regenerate QIs to target count
void MultiQI::regenerate_qis(size_t target_count) {
    if (target_count == 0) {
        throw std::invalid_argument("Target count must be positive");
    }

    std::lock_guard<std::mutex> lock(mutex_);

    // Get current precision from first QI if available
    int mpfr_prec = 256;  // Default

    // Clear current QIs
    qis_.clear();
    qis_.reserve(target_count);

    // Generate new QIs with diverse discriminants
    std::random_device rd;
    std::mt19937_64 rng(rd());
    std::uniform_int_distribution<long> param_dist(1, 10000);

    for (size_t i = 0; i < target_count; ++i) {
        long k = param_dist(rng);
        long a = 1;
        long b = 2 * param_dist(rng) + 1;
        long c = -(k * k + 1 - b * b) / 4;

        qis_.push_back(std::make_unique<QuadraticIrrational>(a, b, c, mpfr_prec));
    }

    // Reset state
    idx_ = 0;
    weights_.resize(qis_.size(), 1.0 / qis_.size());
    mixing_buffer_.resize(std::min(size_t(16), qis_.size()));
}

// Estimate entropy of the ensemble
double MultiQI::estimate_entropy() const {
    if (qis_.empty())
        return 0.0;

    // Simplified entropy estimate based on:
    // 1. Number of QIs
    // 2. Mixing strategy complexity
    // 3. CFE period lengths

    double base_entropy = std::log2(static_cast<double>(qis_.size()));

    double strategy_factor = 1.0;
    switch (mixing_strategy_) {
        case MixingStrategy::CASCADE_MIX:
            strategy_factor = 3.0;
            break;
        case MixingStrategy::XOR_MIX:
        case MixingStrategy::MODULAR_ADD:
            strategy_factor = 2.0;
            break;
        case MixingStrategy::AVERAGING:
            strategy_factor = 1.5;
            break;
        default:
            strategy_factor = 1.0;
    }

    // Estimate based on CFE periods (if computed)
    double period_entropy = 0.0;
    size_t computed_count = 0;
    for (const auto& qi : qis_) {
        if (qi && qi->has_computed_cfe()) {
            period_entropy += std::log2(static_cast<double>(qi->get_cfe_period()));
            computed_count++;
        }
    }

    if (computed_count > 0) {
        period_entropy /= computed_count;
    } else {
        period_entropy = 10.0;  // Assume moderate period length
    }

    return base_entropy * strategy_factor + period_entropy;
}

}  // namespace qiprng
