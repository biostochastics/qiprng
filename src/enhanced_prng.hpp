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

#include <Rcpp.h> // For Rcpp::Rcout, Rcpp::warning
#include <memory> // For std::unique_ptr
#include <vector>
#include <string>
#include <atomic>  // For std::atomic in parallel fill
#include <thread>  // For std::thread, std::hardware_concurrency
#include <mutex>   // For std::mutex in parallel fill

#ifndef M_PI // Ensure M_PI is defined
#define M_PI 3.14159265358979323846
#endif

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

    void reset_state();
    void fill_buffer_parallel(size_t thread_count);
    void fill_buffer_sequential(); // Renamed for clarity
    void fill_buffer();            // Decides which fill method to use
    
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
    
    // Thread management methods
    static void registerThreadForCleanup();
    static void cleanupThreadResources();


public:
    EnhancedPRNG(const PRNGConfig& cfg,
                 const std::vector<std::tuple<long, long, long>>& abc_list);
    ~EnhancedPRNG();

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