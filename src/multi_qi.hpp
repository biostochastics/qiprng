// File: multi_qi.hpp
// --------------------------------------------------------------
#ifndef QIPRNG_MULTI_QI_HPP
#define QIPRNG_MULTI_QI_HPP

#include <cstddef>     // For size_t
#include <cstdint>     // For uint64_t
#include <functional>  // For std::function
#include <memory>      // For std::unique_ptr
#include <mutex>       // For std::mutex
#include <tuple>       // For std::tuple
#include <vector>

#include "quadratic_irrational.hpp"

namespace qiprng {

// Enum for different mixing strategies
enum class MixingStrategy {
    ROUND_ROBIN,  // Original round-robin approach
    XOR_MIX,      // XOR-based mixing for bit diffusion
    AVERAGING,    // Weighted averaging for smooth distribution
    MODULAR_ADD,  // Modular addition for entropy combining
    CASCADE_MIX   // Cascaded mixing for maximum entropy
};

class MultiQI {
   private:
    std::vector<std::unique_ptr<QuadraticIrrational>> qis_;
    size_t idx_;
    std::mutex mutex_;  // Added for thread safety

    // v0.5.0 enhancements
    MixingStrategy mixing_strategy_;
    std::vector<double> mixing_buffer_;  // Buffer for mixing operations
    std::vector<double> weights_;        // Weights for averaging strategy
    uint64_t mix_counter_;               // Counter for mixing operations

    // Mixing strategy implementations
    double mix_xor(const std::vector<double>& values);
    double mix_averaging(const std::vector<double>& values);
    double mix_modular_add(const std::vector<double>& values);
    double mix_cascade(const std::vector<double>& values);

    // Helper for entropy extraction
    uint64_t extract_mantissa(double value);
    double combine_mantissas(uint64_t m1, uint64_t m2);

    // Refactored helper methods for Single Responsibility Principle
    double getFromCache();
    double generateSingleValue(QuadraticIrrational* qi);
    void refillCache();

   public:
    // Enhanced constructor with mixing strategy selection
    MultiQI(const std::vector<std::tuple<long, long, long>>& abc_list, int mpfr_prec,
            uint64_t seed = 0, bool has_seed = false,
            MixingStrategy strategy = MixingStrategy::ROUND_ROBIN);

    // Dynamic QI generation for v0.5.0
    MultiQI(size_t num_qis, int mpfr_prec, uint64_t seed = 0, bool has_seed = false,
            MixingStrategy strategy = MixingStrategy::CASCADE_MIX);

    ~MultiQI() = default;  // Unique_ptr handles cleanup

    // Core PRNG interface
    double next();
    double next_mixed();  // Enhanced mixing-based generation
    void skip(uint64_t n);
    void jump_ahead(uint64_t n);
    size_t size() const;

    // Batch operations for efficiency
    void fill(double* buffer, size_t fill_size);
    void fill_thread_safe(double* buffer, size_t fill_size);
    void fill_mixed(double* buffer, size_t fill_size);     // Batch with mixing
    void fill_parallel(double* buffer, size_t fill_size);  // v0.5.0: OpenMP parallel generation

    // Configuration and control
    void set_mixing_strategy(MixingStrategy strategy);
    MixingStrategy get_mixing_strategy() const { return mixing_strategy_; }
    void set_weights(const std::vector<double>& weights);

    // Dynamic QI management
    void add_qi(long a, long b, long c, int mpfr_prec);
    void remove_qi(size_t index);
    void regenerate_qis(size_t target_count);

    // Statistics and monitoring
    uint64_t get_mix_counter() const { return mix_counter_; }
    double estimate_entropy() const;
};

}  // namespace qiprng

#endif  // QIPRNG_MULTI_QI_HPP