// File: enhanced_prng.hpp
// --------------------------------------------------------------
// Purpose: Main PRNG implementation using quadratic irrational numbers
// Author: Sergey Kornilov
// Version: 0.6.2
//
// This file implements the core pseudo-random number generator based on
// quadratic irrational numbers with continued fraction expansion (CFE).
// The generator provides:
//   - High-precision computation via MPFR (24-10000 bits)
//   - Hardware acceleration through SIMD (AVX2/NEON)
//   - Parallel generation with OpenMP
//   - Cryptographic mixing via ChaCha20
//   - Support for 14+ statistical distributions
//   - Thread-safe operation with configurable buffer sizes
//
// Mathematical basis: x_{n+1} = (a·x_n² + b·x_n + c) mod 1
// where the discriminant b² - 4ac must be a non-perfect square
// --------------------------------------------------------------
#ifndef QIPRNG_ENHANCED_PRNG_HPP
#define QIPRNG_ENHANCED_PRNG_HPP

#include <Rcpp.h>  // For Rcpp::Rcout, Rcpp::warning

#include <atomic>  // For std::atomic in parallel fill
#include <memory>  // For std::unique_ptr
#include <mutex>   // For std::mutex in parallel fill
#include <string>
#include <thread>  // For std::thread, std::hardware_concurrency
#include <vector>

#include "crypto_mixer.hpp"
#include "multi_qi_optimized.hpp"  // Use optimized lock-free version
#include "precision_utils.hpp"     // For high-precision constants and safe conversions
#include "prng_common.hpp"         // For SecureBuffer
#include "prng_config.hpp"
#include "prng_utils.hpp"           // For pickMultiQiSet and other utilities
#include "simd_operations.hpp"      // For SIMD vectorization
#include "thread_pool.hpp"          // For thread pool implementation
#include "work_stealing_queue.hpp"  // For work-stealing load balancing
#include "ziggurat_normal.hpp"

namespace qiprng {

// No forward declarations needed

/**
 * Enhanced Pseudo-Random Number Generator using Quadratic Irrationals
 *
 * This class implements a high-quality PRNG based on the ergodic properties
 * of quadratic irrational numbers. The generator uses continued fraction
 * expansion (CFE) to produce uniformly distributed random numbers with
 * excellent statistical properties.
 *
 * Key Features:
 * - Multiple quadratic irrationals (MultiQI) for ensemble generation
 * - Cryptographic mixing via ChaCha20 for enhanced security
 * - Parallel buffer filling with OpenMP optimization
 * - SIMD vectorization for performance
 * - Support for 14+ statistical distributions
 * - Thread-safe operation with mutex protection
 *
 * Thread Safety:
 * - All public methods are thread-safe via internal synchronization
 * - For heavy parallel workloads, enable use_threading in configuration
 * - Thread-local storage used for OpenMP parallel filling
 *
 * Performance:
 * - Single-threaded: ~8.18M values/second
 * - Near-linear scaling with multiple threads
 * - O(1) memory usage with buffering
 * - Sub-122ns latency per value
 */
class EnhancedPRNG {
   private:
    PRNGConfig config_;
    std::unique_ptr<MultiQIOptimized> multi_;  // Using optimized lock-free version
    std::unique_ptr<CryptoMixer> crypto_;
    std::unique_ptr<ZigguratNormal> ziggurat_;
    SecureBuffer<double> buffer_;
    size_t buffer_pos_;
    uint64_t sample_count_;  // Changed to uint64_t for larger counts

    bool has_spare_normal_;
    double spare_normal_;

    // Thread management
    std::atomic<bool> is_being_destroyed_;
    std::mutex cleanup_mutex_;
    std::once_flag shutdown_once_flag_;  // Ensure shutdown logic runs only once

    // Reset internal state (buffer position, counts)
    void reset_state();

    // Thread-safe shutdown preparation
    void prepare_for_shutdown();

    // Parallel buffer filling using thread pool
    // Complexity: O(buffer_size / thread_count) per thread
    void fill_buffer_parallel(size_t thread_count);

    // Sequential buffer filling for single-threaded mode
    // Complexity: O(buffer_size)
    void fill_buffer_sequential();
#ifdef _OPENMP
    // OpenMP-optimized filling with thread-local caching (v0.6.2)
    // Uses thread-local MultiQI instances for better cache locality
    void fill_buffer_openmp();

    // Clean up thread-local MultiQI caches
    static void cleanup_thread_caches();
#endif

    // Decides which fill method to use based on configuration
    void fill_buffer();

    // Helper methods for parallel filling (refactoring)
    bool create_thread_resources(
        size_t thread_count,
        std::vector<std::vector<std::tuple<long, long, long>>>& thread_abc_lists,
        std::vector<std::unique_ptr<MultiQIOptimized>>& thread_qis,
        std::vector<SecureBuffer<double>>& thread_buffers, std::vector<size_t>& chunk_sizes);
    void submit_parallel_tasks(ThreadPool& pool, size_t thread_count,
                               std::vector<SecureBuffer<double>>& thread_buffers,
                               std::vector<std::unique_ptr<MultiQIOptimized>>& thread_qis,
                               std::atomic<bool>& any_thread_failed,
                               std::vector<std::future<void>>& futures);
    void copy_thread_buffers_to_main(size_t thread_count,
                                     const std::vector<SecureBuffer<double>>& thread_buffers,
                                     ThreadPool& pool);

    // Gets a raw uniform [0,1) without distribution transformations
    // Thread-safe via buffer management
    double next_raw_uniform();

    std::pair<double, double> box_muller_pair(double u1, double u2);
    double uniform_to_exponential(double u);
    double generate_poisson_knuth(double lambda);                   // For small lambda
    double generate_poisson_normal_approx(double lambda);           // For large lambda
    double generate_gamma_small_alpha(double alpha, double theta);  // alpha < 1
    double generate_gamma_large_alpha(double alpha, double theta);  // alpha >= 1
    double generate_beta_johnk(double alpha, double beta);

    // Make these callable from next() via a switch
    double generate_uniform_01(double u);
    double generate_uniform_range(double u);
    double generate_normal(double u);             // u is first uniform, may need more from buffer
    double generate_normal_box_muller(double u);  // Box-Muller implementation
    double generate_exponential(double u);
    double generate_poisson_dispatch(double u);  // u is first uniform
    double generate_gamma_dispatch(double u);    // u is first uniform
    double generate_beta_dispatch(double u);     // u is first uniform

    // New distribution dispatch methods
    double generate_bernoulli_dispatch(double u);
    double generate_binomial_dispatch(double u);
    double generate_lognormal_dispatch(double u);
    double generate_weibull_dispatch(double u);
    double generate_chisquared_dispatch(double u);
    double generate_student_t_dispatch(double u);
    double generate_negative_binomial_dispatch(double u);

    // Thread management methods
    static void registerThreadForCleanup();
    static void cleanupThreadResources();

   public:
    /**
     * Construct a new Enhanced PRNG
     * @param cfg Configuration parameters including precision, distribution, etc.
     * @param abc_list List of (a,b,c) coefficient tuples for quadratic irrationals
     * @throws std::invalid_argument if coefficients don't satisfy b²-4ac > 0
     * @throws std::runtime_error if MPFR initialization fails
     */
    EnhancedPRNG(const PRNGConfig& cfg, const std::vector<std::tuple<long, long, long>>& abc_list);

    /**
     * Destructor - ensures clean shutdown of threads and resources
     * Thread-safe with proper synchronization
     */
    ~EnhancedPRNG() noexcept;

    // Get current configuration (read-only)
    const PRNGConfig& getConfig() const;

    // Get number of quadratic irrationals in ensemble
    size_t getQICount() const;

    // Update configuration (distribution, parameters, etc.)
    // Thread-safe but may cause temporary performance impact
    void updateConfig(const PRNGConfig& new_config);

    /**
     * Generate next random value according to configured distribution
     * @return Random value from the specified distribution
     * Thread-safe via internal synchronization
     * Complexity: O(1) amortized (may trigger buffer refill)
     */
    double next();

    /**
     * Fill an R vector with random values
     * @param output_vec Pre-allocated Rcpp::NumericVector to fill
     * More efficient than repeated next() calls for large n
     * Complexity: O(n)
     */
    void generate_n(Rcpp::NumericVector& output_vec);

    /**
     * Skip ahead n steps in the sequence
     * @param n Number of values to skip
     * Uses O(log n) jump-ahead algorithm for efficiency
     */
    void skip(uint64_t n);

    /**
     * Reseed with fresh entropy
     * Reinitializes all quadratic irrationals and crypto mixer
     */
    void reseed();

    // Dump current configuration to console (for debugging)
    void dumpConfig() const;

    // Thread-safe cleanup
    void prepareForCleanup();
    void performCleanup();
    bool isBeingDestroyed() const;

    // Utility functions
    static void cleanupAllThreadResources();
};

}  // namespace qiprng

#endif  // QIPRNG_ENHANCED_PRNG_HPP
