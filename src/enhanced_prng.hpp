// File: enhanced_prng.hpp
// --------------------------------------------------------------
#ifndef QIPRNG_ENHANCED_PRNG_HPP
#define QIPRNG_ENHANCED_PRNG_HPP

#include "prng_config.hpp"
#include "prng_common.hpp" // For SecureBuffer
#include "multi_qi.hpp"
#include "crypto_mixer.hpp"
#include "ziggurat_normal.hpp"
#include "prng_utils.hpp"    // For pickMultiQiSet and other utilities
#include "thread_pool.hpp"   // For thread pool implementation
#include "work_stealing_queue.hpp"  // For work-stealing load balancing
#include "simd_operations.hpp"      // For SIMD vectorization

#include <Rcpp.h> // For Rcpp::Rcout, Rcpp::warning
#include <memory> // For std::unique_ptr
#include <vector>
#include <string>
#include <atomic>  // For std::atomic in parallel fill
#include <thread>  // For std::thread, std::hardware_concurrency
#include <mutex>   // For std::mutex in parallel fill
#include "precision_utils.hpp"  // For high-precision constants and safe conversions

namespace qiprng {

// No forward declarations needed

class EnhancedPRNG {
private:
    PRNGConfig config_;
    std::unique_ptr<MultiQI> multi_;
    std::unique_ptr<CryptoMixer> crypto_;
    std::unique_ptr<ZigguratNormal> ziggurat_;
    SecureBuffer<double> buffer_;
    size_t buffer_pos_;
    uint64_t sample_count_; // Changed to uint64_t for larger counts

    bool has_spare_normal_;
    double spare_normal_;
    
    // Thread management
    std::atomic<bool> is_being_destroyed_;
    std::mutex cleanup_mutex_;
    std::once_flag shutdown_once_flag_;  // Ensure shutdown logic runs only once

    void reset_state();
    void prepare_for_shutdown();       // Thread-safe shutdown preparation
    void fill_buffer_parallel(size_t thread_count);
    void fill_buffer_sequential(); // Renamed for clarity
    #ifdef _OPENMP
    void fill_buffer_openmp();     // v0.5.0: OpenMP-optimized filling
    #endif
    void fill_buffer();            // Decides which fill method to use
    
    // Helper methods for parallel filling (refactoring)
    bool create_thread_resources(size_t thread_count,
                                std::vector<std::vector<std::tuple<long, long, long>>>& thread_abc_lists,
                                std::vector<std::unique_ptr<MultiQI>>& thread_qis,
                                std::vector<SecureBuffer<double>>& thread_buffers,
                                std::vector<size_t>& chunk_sizes);
    void submit_parallel_tasks(ThreadPool& pool,
                              size_t thread_count,
                              std::vector<SecureBuffer<double>>& thread_buffers,
                              std::vector<std::unique_ptr<MultiQI>>& thread_qis,
                              std::atomic<bool>& any_thread_failed,
                              std::vector<std::future<void>>& futures);
    void copy_thread_buffers_to_main(size_t thread_count,
                                    const std::vector<SecureBuffer<double>>& thread_buffers,
                                    ThreadPool& pool);
    
    double next_raw_uniform();     // Gets a raw uniform without distribution transformations

    std::pair<double,double> box_muller_pair(double u1, double u2);
    double uniform_to_exponential(double u);
    double generate_poisson_knuth(double lambda); // For small lambda
    double generate_poisson_normal_approx(double lambda); // For large lambda
    double generate_gamma_small_alpha(double alpha, double theta); // alpha < 1
    double generate_gamma_large_alpha(double alpha, double theta); // alpha >= 1
    double generate_beta_johnk(double alpha, double beta);

    // Make these callable from next() via a switch
    double generate_uniform_01(double u);
    double generate_uniform_range(double u);
    double generate_normal(double u); // u is first uniform, may need more from buffer
    double generate_normal_box_muller(double u); // Box-Muller implementation
    double generate_exponential(double u);
    double generate_poisson_dispatch(double u); // u is first uniform
    double generate_gamma_dispatch(double u);   // u is first uniform
    double generate_beta_dispatch(double u);    // u is first uniform
    
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
    EnhancedPRNG(const PRNGConfig& cfg,
                 const std::vector<std::tuple<long, long, long>>& abc_list);
    ~EnhancedPRNG() noexcept;

    const PRNGConfig& getConfig() const;
    size_t getQICount() const;
    void updateConfig(const PRNGConfig& new_config);

    double next();
    void generate_n(Rcpp::NumericVector& output_vec); // Fills an Rcpp vector
    void skip(uint64_t n); // Changed to uint64_t
    void reseed();
    void dumpConfig() const;
    
    // Thread-safe cleanup
    void prepareForCleanup();
    void performCleanup();
    bool isBeingDestroyed() const;
    
    // Utility functions
    static void cleanupAllThreadResources();
};

} // namespace qiprng

#endif // QIPRNG_ENHANCED_PRNG_HPP