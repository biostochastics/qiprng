// Lock-free optimized MultiQI implementation
// --------------------------------------------------------------
#include "multi_qi_optimized.hpp"

#include <algorithm>
#include <cmath>
#include <cstring>
#include <numeric>
#include <random>
#include <stdexcept>
#ifdef _OPENMP
#    include <omp.h>
#else
#    define omp_get_thread_num()  0
#    define omp_get_num_threads() 1
#endif

namespace qiprng {

// Global shutdown flag - prevents access to resources after cleanup starts
std::atomic<bool> g_shutdown_in_progress{false};

void mark_shutdown_started() {
    g_shutdown_in_progress.store(true, std::memory_order_release);
}

void mark_shutdown_complete() {
    g_shutdown_in_progress.store(false, std::memory_order_release);
}

bool is_shutdown_in_progress() {
    return g_shutdown_in_progress.load(std::memory_order_acquire);
}

// Thread-local storage definitions
thread_local MultiQIOptimized::ThreadLocalData MultiQIOptimized::tl_data;
std::atomic<size_t> MultiQIOptimized::thread_counter_{0};

// Reset thread counter for deterministic behavior
void MultiQIOptimized::reset_thread_counter() {
    if (is_shutdown_in_progress())
        return;  // Don't reset during shutdown
    thread_counter_.store(0, std::memory_order_relaxed);
    // Also reset thread-local data to ensure clean state
    tl_data = ThreadLocalData();
}

// Cleanup thread-local data explicitly before shutdown
void MultiQIOptimized::cleanup_thread_local_data() {
    // Clear all QI instances in thread-local storage
    if (tl_data.qis) {
        tl_data.qis->clear();
        tl_data.qis.reset();
    }
    tl_data.cache.clear();
    tl_data.cache.shrink_to_fit();
    tl_data.initialized = false;
    tl_data.idx = 0;
    tl_data.cache_pos = 0;
}

// Reset for fresh start after shutdown
void MultiQIOptimized::reset_after_shutdown() {
    thread_counter_.store(0, std::memory_order_relaxed);
    tl_data = ThreadLocalData();
}

// Performance metrics - wrapped in struct with safe access
namespace perf {

static std::atomic<Metrics*> metrics_ptr{nullptr};
static std::once_flag metrics_init_flag;
static std::atomic<bool> metrics_destroyed{false};

Metrics& get_global_metrics() {
    std::call_once(metrics_init_flag, []() {
        static Metrics static_metrics;
        metrics_ptr.store(&static_metrics, std::memory_order_release);
    });
    return *metrics_ptr.load(std::memory_order_acquire);
}

void mark_metrics_destroyed() {
    metrics_destroyed.store(true, std::memory_order_release);
}

bool are_metrics_available() {
    return !metrics_destroyed.load(std::memory_order_acquire) &&
           !g_shutdown_in_progress.load(std::memory_order_acquire);
}

// Legacy global_metrics - now redirects to safe accessor
// We keep the name for backward compatibility but it should not be accessed directly
Metrics global_metrics;  // This is now just a placeholder, actual metrics go through
                         // get_global_metrics()

}  // namespace perf

// Constructor from parameter list
MultiQIOptimized::MultiQIOptimized(const std::vector<std::tuple<long, long, long>>& abc_list,
                                   int mpfr_prec, uint64_t seed, bool has_seed,
                                   MixingStrategy strategy)
    : mixing_strategy_(strategy), mix_counter_(0) {
    if (abc_list.empty()) {
        throw std::runtime_error("MultiQIOptimized: abc_list cannot be empty.");
    }

    // Create shared configuration
    config_ = std::make_shared<Config>();
    config_->abc_params = abc_list;
    config_->mpfr_precision = mpfr_prec;
    config_->base_seed = seed;
    config_->has_seed = has_seed;
    config_->strategy = strategy;

    // Initialize mixing buffers
    mixing_buffer_.resize(std::min(size_t(16), abc_list.size()));
    weights_.resize(abc_list.size(), 1.0 / abc_list.size());
}

// Dynamic constructor
MultiQIOptimized::MultiQIOptimized(size_t num_qis, int mpfr_prec, uint64_t seed, bool has_seed,
                                   MixingStrategy strategy)
    : mixing_strategy_(strategy), mix_counter_(0) {
    if (num_qis == 0) {
        throw std::runtime_error("MultiQIOptimized: num_qis must be positive.");
    }

    config_ = std::make_shared<Config>();
    config_->mpfr_precision = mpfr_prec;
    config_->base_seed = seed;
    config_->has_seed = has_seed;
    config_->strategy = strategy;

    // Generate QI parameters dynamically
    std::mt19937_64 rng(seed);
    std::uniform_int_distribution<long> k_dist(1, 1000);

    for (size_t i = 0; i < num_qis; ++i) {
        long k = k_dist(rng);
        long discriminant_target = k * k + 1;
        long a = 1;
        long b = 2 * k_dist(rng) + 1;
        long c = -(discriminant_target - b * b) / 4;

        long long disc = static_cast<long long>(b) * b - 4LL * a * c;
        if (disc > 0) {
            config_->abc_params.push_back({a, b, c});
        } else {
            config_->abc_params.push_back({1, 1, -1});
        }
    }

    mixing_buffer_.resize(std::min(size_t(16), num_qis));
    weights_.resize(num_qis, 1.0 / num_qis);
}

// Ensure thread-local data is initialized
void MultiQIOptimized::ensure_initialized() const {
    if (is_shutdown_in_progress()) {
        throw std::runtime_error("MultiQIOptimized used during shutdown");
    }

    if (!tl_data.initialized) {
        // Get unique thread ID
        size_t thread_id = thread_counter_.fetch_add(1, std::memory_order_relaxed);
        if (perf::are_metrics_available()) {
            perf::get_global_metrics().thread_initializations++;
        }

        // Derive thread-specific seed using golden ratio
        constexpr uint64_t GOLDEN_PRIME = 0x9E3779B97F4A7C15ULL;
        uint64_t thread_seed = config_->base_seed + thread_id * GOLDEN_PRIME;

        // Create thread-local QI instances
        tl_data.qis = std::make_unique<std::vector<std::unique_ptr<QuadraticIrrational>>>();
        tl_data.qis->reserve(config_->abc_params.size());

        for (const auto& params : config_->abc_params) {
            long a, b, c;
            std::tie(a, b, c) = params;
            tl_data.qis->push_back(std::make_unique<QuadraticIrrational>(
                a, b, c, config_->mpfr_precision, thread_seed++, true));
        }

        tl_data.idx = 0;
        tl_data.cache.reserve(CACHE_SIZE);
        tl_data.cache_pos = 0;
        tl_data.initialized = true;
    }
}

// Generate single value
double MultiQIOptimized::generate_single() const {
    ensure_initialized();

    if (tl_data.idx >= tl_data.qis->size()) {
        tl_data.idx = 0;
    }

    double val = (*tl_data.qis)[tl_data.idx]->next();
    tl_data.idx = (tl_data.idx + 1) % tl_data.qis->size();

    return val;
}

// Refill thread-local cache (lock-free)
void MultiQIOptimized::refill_cache() const {
    ensure_initialized();

    tl_data.cache.clear();
    tl_data.cache.reserve(CACHE_SIZE);

    for (size_t i = 0; i < CACHE_SIZE; ++i) {
        tl_data.cache.push_back(generate_single());
    }

    tl_data.cache_pos = 0;
    if (perf::are_metrics_available()) {
        perf::get_global_metrics().cache_misses++;
    }
}

// Main generation method (lock-free via thread-local storage)
double MultiQIOptimized::next() {
    if (is_shutdown_in_progress()) {
        return 0.5;  // Safe fallback during shutdown
    }

    ensure_initialized();

    // Check cache first
    if (tl_data.cache_pos < tl_data.cache.size()) {
        if (perf::are_metrics_available()) {
            perf::get_global_metrics().cache_hits++;
            perf::get_global_metrics().samples_generated++;
        }
        return tl_data.cache[tl_data.cache_pos++];
    }

    // Refill cache and return first value
    refill_cache();
    if (perf::are_metrics_available()) {
        perf::get_global_metrics().samples_generated++;
    }
    return tl_data.cache[tl_data.cache_pos++];
}

// Mixed generation
double MultiQIOptimized::next_mixed() {
    if (is_shutdown_in_progress()) {
        return 0.5;  // Safe fallback during shutdown
    }

    ensure_initialized();

    size_t mix_size = std::min(mixing_buffer_.size(), tl_data.qis->size());
    std::vector<double> values;
    values.reserve(mix_size);

    for (size_t i = 0; i < mix_size; ++i) {
        values.push_back(generate_single());
    }

    mix_counter_++;

    switch (mixing_strategy_) {
        case MixingStrategy::XOR_MIX:
            return mix_xor(values);
        case MixingStrategy::AVERAGING:
            return mix_averaging(values);
        case MixingStrategy::MODULAR_ADD:
            return mix_modular_add(values);
        case MixingStrategy::CASCADE_MIX:
            return mix_cascade(values);
        default:
            return values[0];
    }
}

// Batch fill - no locks!
void MultiQIOptimized::fill(double* buffer, size_t fill_size) {
    for (size_t i = 0; i < fill_size; ++i) {
        buffer[i] = next();
    }
}

// Thread-safe fill (same as regular fill now - no mutex needed!)
void MultiQIOptimized::fill_thread_safe(double* buffer, size_t fill_size) {
    fill(buffer, fill_size);
}

// Mixed batch fill
void MultiQIOptimized::fill_mixed(double* buffer, size_t fill_size) {
    for (size_t i = 0; i < fill_size; ++i) {
        buffer[i] = next_mixed();
    }
}

// Parallel fill - each thread works independently
void MultiQIOptimized::fill_parallel(double* buffer, size_t fill_size) {
#ifdef _OPENMP
#    pragma omp parallel
    {
        int thread_id = omp_get_thread_num();
        int num_threads = omp_get_num_threads();
        size_t chunk_size = fill_size / num_threads;
        size_t start = thread_id * chunk_size;
        size_t end = (thread_id == num_threads - 1) ? fill_size : (thread_id + 1) * chunk_size;

        // Each thread fills independently - no contention!
        for (size_t i = start; i < end; ++i) {
            buffer[i] = next();
        }
    }
#else
    fill(buffer, fill_size);
#endif
}

// Skip ahead
void MultiQIOptimized::skip(uint64_t n) {
    if (is_shutdown_in_progress()) {
        return;  // No-op during shutdown
    }

    ensure_initialized();
    for (auto& qi : *tl_data.qis) {
        qi->skip(n);
    }
    tl_data.cache.clear();
    tl_data.cache_pos = 0;
}

// Jump ahead
void MultiQIOptimized::jump_ahead(uint64_t n) {
    skip(n);
}

// Get size
size_t MultiQIOptimized::size() const {
    if (tl_data.initialized && tl_data.qis) {
        return tl_data.qis->size();
    }
    return config_->abc_params.size();
}

// Set mixing strategy
void MultiQIOptimized::set_mixing_strategy(MixingStrategy strategy) {
    mixing_strategy_ = strategy;
}

// Set weights
void MultiQIOptimized::set_weights(const std::vector<double>& weights) {
    if (weights.size() != size()) {
        throw std::runtime_error("Weights size must match number of QIs");
    }
    weights_ = weights;
}

// Mixing implementations
double MultiQIOptimized::mix_xor(const std::vector<double>& values) const {
    uint64_t result = 0;
    for (double val : values) {
        result ^= extract_mantissa(val);
    }
    // Use mantissa mask (52 bits) for proper scaling to preserve full precision
    return static_cast<double>(result) / static_cast<double>(0x000FFFFFFFFFFFFFULL);
}

double MultiQIOptimized::mix_averaging(const std::vector<double>& values) const {
    double sum = 0.0;
    for (size_t i = 0; i < values.size(); ++i) {
        sum += values[i] * weights_[i % weights_.size()];
    }
    return sum;
}

double MultiQIOptimized::mix_modular_add(const std::vector<double>& values) const {
    double sum = 0.0;
    for (double val : values) {
        sum += val;
    }
    return std::fmod(sum, 1.0);
}

double MultiQIOptimized::mix_cascade(const std::vector<double>& values) const {
    if (values.empty())
        return 0.0;

    double result = values[0];
    for (size_t i = 1; i < values.size(); ++i) {
        uint64_t m1 = extract_mantissa(result);
        uint64_t m2 = extract_mantissa(values[i]);
        result = combine_mantissas(m1, m2);
    }
    return result;
}

// Extract mantissa bits
uint64_t MultiQIOptimized::extract_mantissa(double value) const {
    uint64_t bits;
    std::memcpy(&bits, &value, sizeof(bits));
    return bits & 0x000FFFFFFFFFFFFFULL;
}

// Combine mantissas using SplitMix64 constant for better statistical properties
double MultiQIOptimized::combine_mantissas(uint64_t m1, uint64_t m2) const {
    // Use golden ratio-based constant from SplitMix64 (better avalanche than Java LCG)
    uint64_t combined = (m1 * 0x9e3779b97f4a7c15ULL + m2) & 0x000FFFFFFFFFFFFFULL;
    combined |= 0x3FF0000000000000ULL;
    double result;
    std::memcpy(&result, &combined, sizeof(result));
    return result - 1.0;
}

// Estimate entropy
double MultiQIOptimized::estimate_entropy() const {
    double base_entropy = std::log2(static_cast<double>(size()));
    double strategy_factor = 1.0;

    switch (mixing_strategy_) {
        case MixingStrategy::CASCADE_MIX:
            strategy_factor = 1.5;
            break;
        case MixingStrategy::XOR_MIX:
            strategy_factor = 1.3;
            break;
        case MixingStrategy::MODULAR_ADD:
            strategy_factor = 1.2;
            break;
        case MixingStrategy::AVERAGING:
            strategy_factor = 1.1;
            break;
        default:
            break;
    }

    return base_entropy * strategy_factor;
}

// Clone
std::unique_ptr<MultiQIOptimized> MultiQIOptimized::clone() const {
    return std::make_unique<MultiQIOptimized>(config_->abc_params, config_->mpfr_precision,
                                              config_->base_seed, config_->has_seed,
                                              mixing_strategy_);
}

// Reseed
void MultiQIOptimized::reseed() {
    tl_data.initialized = false;
    tl_data.cache.clear();
    tl_data.cache_pos = 0;
    mix_counter_ = 0;
}

// Reset thread-local state
void MultiQIOptimized::reset_thread_local() {
    tl_data = ThreadLocalData();
}

}  // namespace qiprng
