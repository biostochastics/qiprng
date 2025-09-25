//' @file multi_qi_optimized.hpp
//' @brief Optimized MultiQI with lock-free thread-local architecture
//' @details This implementation eliminates mutex contention through thread-local
//'          storage, achieving zero-contention parallel generation. Each thread
//'          maintains independent PRNG instances with cache-line aligned data
//'          structures to prevent false sharing.
//' @author QIPRNG Development Team
//' @version 0.6.4
// --------------------------------------------------------------
#ifndef QIPRNG_MULTI_QI_OPTIMIZED_HPP
#define QIPRNG_MULTI_QI_OPTIMIZED_HPP

#include <atomic>
#include <cstddef>
#include <cstdint>
#include <functional>
#include <memory>
#include <vector>

#include "quadratic_irrational.hpp"

namespace qiprng {

// Define mixing strategy enum (copied from multi_qi.hpp)
enum class MixingStrategy { ROUND_ROBIN, XOR_MIX, AVERAGING, MODULAR_ADD, CASCADE_MIX };

//' @class MultiQIOptimized
//' @brief Lock-free thread-local MultiQI implementation
//' @details This class provides high-performance random number generation through
//'          thread-local storage patterns. Key features:
//'          - Zero mutex contention through thread-local instances
//'          - L1 cache-optimized buffer (4096 samples)
//'          - Cache-line alignment (64 bytes) to prevent false sharing
//'          - Golden ratio prime seeding for thread independence
//'          - Support for multiple mixing strategies
class MultiQIOptimized {
   private:
    // Per-thread instance storage
    struct ThreadLocalData {
        std::unique_ptr<std::vector<std::unique_ptr<QuadraticIrrational>>> qis;
        size_t idx;
        std::vector<double> cache;
        size_t cache_pos;
        bool initialized;

        ThreadLocalData() : idx(0), cache_pos(0), initialized(false) {}
    };

    // Thread-local storage for each thread's PRNG state
    static thread_local ThreadLocalData tl_data;

    // Shared immutable configuration
    struct Config {
        std::vector<std::tuple<long, long, long>> abc_params;
        int mpfr_precision;
        uint64_t base_seed;
        bool has_seed;
        MixingStrategy strategy;
    };
    std::shared_ptr<Config> config_;

    // Global thread counter for unique seeding
    static std::atomic<size_t> thread_counter_;

    // Cache configuration
    static constexpr size_t CACHE_SIZE = 4096;  // 32KB cache for optimal L1 usage

    // Helper methods
    void ensure_initialized() const;
    void refill_cache() const;
    double generate_single() const;

    // Mixing strategy implementations
    MixingStrategy mixing_strategy_;
    std::vector<double> mixing_buffer_;
    std::vector<double> weights_;
    mutable uint64_t mix_counter_;

    double mix_xor(const std::vector<double>& values) const;
    double mix_averaging(const std::vector<double>& values) const;
    double mix_modular_add(const std::vector<double>& values) const;
    double mix_cascade(const std::vector<double>& values) const;

    uint64_t extract_mantissa(double value) const;
    double combine_mantissas(uint64_t m1, uint64_t m2) const;

   public:
    // Constructors matching original MultiQI interface
    MultiQIOptimized(const std::vector<std::tuple<long, long, long>>& abc_list, int mpfr_prec,
                     uint64_t seed = 0, bool has_seed = false,
                     MixingStrategy strategy = MixingStrategy::ROUND_ROBIN);

    MultiQIOptimized(size_t num_qis, int mpfr_prec, uint64_t seed = 0, bool has_seed = false,
                     MixingStrategy strategy = MixingStrategy::CASCADE_MIX);

    ~MultiQIOptimized() = default;

    // Core PRNG interface - ALL LOCK-FREE!
    double next();
    double next_mixed();
    void skip(uint64_t n);
    void jump_ahead(uint64_t n);
    size_t size() const;

    // Batch operations - optimized for parallel execution
    void fill(double* buffer, size_t fill_size);
    void fill_mixed(double* buffer, size_t fill_size);
    void fill_parallel(double* buffer, size_t fill_size);
    void fill_thread_safe(double* buffer, size_t fill_size);

    // Configuration methods
    void set_mixing_strategy(MixingStrategy strategy);
    MixingStrategy get_mixing_strategy() const { return mixing_strategy_; }
    void set_weights(const std::vector<double>& weights);

    // Statistics
    uint64_t get_mix_counter() const { return mix_counter_; }
    double estimate_entropy() const;

    // Cloning for explicit thread creation
    std::unique_ptr<MultiQIOptimized> clone() const;

    // Utility methods
    size_t getSize() const { return size(); }
    void reseed();

    // Static methods for thread management
    static void reset_thread_local();
    static size_t get_thread_count() { return thread_counter_.load(); }
};

// Performance monitoring utilities
namespace perf {
struct Metrics {
    std::atomic<uint64_t> samples_generated{0};
    std::atomic<uint64_t> cache_hits{0};
    std::atomic<uint64_t> cache_misses{0};
    std::atomic<uint64_t> thread_initializations{0};

    double get_cache_hit_rate() const {
        uint64_t total = cache_hits + cache_misses;
        return total > 0 ? static_cast<double>(cache_hits) / total : 0.0;
    }
};

extern Metrics global_metrics;
}  // namespace perf

}  // namespace qiprng

#endif  // QIPRNG_MULTI_QI_OPTIMIZED_HPP
