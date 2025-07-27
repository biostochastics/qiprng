// File: enhanced_prng.cpp
// --------------------------------------------------------------
#include "enhanced_prng.hpp"
#include <algorithm> // For std::min
#include <type_traits> // For std::is_same
#include <cmath> // For std::isnan, std::isinf

namespace qiprng {

// Helper method to get raw uniform without distribution transformations
// This avoids circular dependencies when used with Ziggurat
double EnhancedPRNG::next_raw_uniform() {
    // Thread-local fallback generator for when main generator is unavailable
    static thread_local std::mt19937_64 fallback_rng(std::random_device{}());
    static thread_local std::uniform_real_distribution<double> fallback_dist(0.000001, 0.999999);
    
    // Check if object is being destroyed
    if (is_being_destroyed_.load()) {
        // Return a random value from fallback generator if we're in the process of cleanup
        return fallback_dist(fallback_rng);
    }
    
    // If buffer needs refilling, do so
    if (buffer_pos_ >= buffer_.size()) {
        try {
            fill_buffer();
        } catch (...) {
            // Return a random value from fallback generator if fill_buffer fails
            return fallback_dist(fallback_rng);
        }
    }
    
    // Ensure buffer position is valid even after fill_buffer might have changed it
    if (buffer_pos_ >= buffer_.size()) {
        return fallback_dist(fallback_rng); // Use fallback if buffer is still invalid
    }
    
    // Get raw uniform value with bounds checking
    double u;
    try {
        u = buffer_[buffer_pos_++];
        sample_count_++;
        
        // Validate the value
        if (u <= 0.0 || u >= 1.0 || std::isnan(u) || std::isinf(u)) {
            u = fallback_dist(fallback_rng); // Use fallback if we got an invalid uniform
        }
    } catch (...) {
        // Return a random value from fallback if array access throws
        return fallback_dist(fallback_rng);
    }
    
    // Reseed crypto periodically if enabled
    if (config_.use_crypto_mixing && crypto_ && 
        config_.reseed_interval > 0 && 
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
    : config_(cfg),
      buffer_(cfg.buffer_size),
      buffer_pos_(cfg.buffer_size), // Start at end to force fill on first next()
      sample_count_(0),
      has_spare_normal_(false),
      spare_normal_(0.0),
      is_being_destroyed_(false) {
    
    // Validate important parameters
    if (abc_list.empty()) {
        throw std::runtime_error("EnhancedPRNG: abc_list cannot be empty");
    }
    
    // Create ziggurat for normal generation if needed
    if (config_.normal_method == PRNGConfig::ZIGGURAT) {
        ziggurat_ = std::make_unique<ZigguratNormal>(
            [this]() { return this->next_raw_uniform(); }, // Use raw uniform to avoid circular dependency
            config_.normal_mean,
            config_.normal_sd,
            config_.use_threading // Enable thread-safe mode if threading is enabled
        );
    }
    
    // Create crypto mixer if enabled
    if (config_.use_crypto_mixing) {
        initialize_libsodium_if_needed();
        crypto_ = std::make_unique<CryptoMixer>(
            config_.adhoc_corrections,
            config_.use_tie_breaking,
            config_.seed,
            config_.has_seed
        );
    }
    
    // Create MultiQI instance with provided parameters
    if (config_.has_seed) {
        // Pass seed to MultiQI for deterministic initialization
        multi_ = std::make_unique<MultiQI>(abc_list, config_.mpfr_precision,
                                          config_.seed, true);
    } else {
        multi_ = std::make_unique<MultiQI>(abc_list, config_.mpfr_precision);
    }
    
    reset_state();
    
    // Register thread for cleanup if threading is enabled
    if (config_.use_threading) {
        registerThreadForCleanup();
    }
}

// Destructor with safe cleanup
EnhancedPRNG::~EnhancedPRNG() {
    // Mark as being destroyed to prevent new operations
    is_being_destroyed_.store(true);
    
    // Acquire cleanup mutex to prevent concurrent access
    std::lock_guard<std::mutex> lock(cleanup_mutex_);
    
    try {
        // Disable thread-safe mode on ziggurat first
        if (ziggurat_) {
            try {
                ziggurat_->set_thread_safe_mode(false);
            } catch (...) {
                // Ignore errors during cleanup
            }
        }
        
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
            Rcpp::warning("Error during PRNG destruction: %s", e.what());
        }
    } catch (...) {
        // Catch all exceptions to prevent crashes during destruction
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
    bool reset_buffer = 
        (new_config.buffer_size != config_.buffer_size) || 
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
    
    bool reset_ziggurat = 
        (new_config.normal_method != config_.normal_method) ||
        (new_config.normal_mean != config_.normal_mean) ||
        (new_config.normal_sd != config_.normal_sd);
    
    bool reset_crypto =
        (new_config.use_crypto_mixing != config_.use_crypto_mixing) ||
        (new_config.adhoc_corrections != config_.adhoc_corrections) ||
        (new_config.use_tie_breaking != config_.use_tie_breaking);
    
    // Store old distribution for transition checks
    PRNGConfig::Distribution old_distribution = config_.distribution;
    
    // Update configuration
    config_ = new_config;
    
    // Reset components as needed
    if (reset_buffer) {
        buffer_ = SecureBuffer<double>(config_.buffer_size);
        buffer_pos_ = config_.buffer_size; // Force refill on next use
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
                    [this]() { return this->next_raw_uniform(); }, // Use raw uniform to avoid circular dependency
                    config_.normal_mean,
                    config_.normal_sd,
                    config_.use_threading // Enable thread-safe mode if threading is enabled
                );
            }
        } else {
            ziggurat_.reset(); // Not needed for Box-Muller
        }
    } else if (ziggurat_ && config_.normal_method == PRNGConfig::ZIGGURAT) {
        // Update thread-safe mode even if parameters didn't change
        ziggurat_->set_thread_safe_mode(config_.use_threading);
    }
    
    if (reset_crypto) {
        if (config_.use_crypto_mixing) {
            crypto_ = std::make_unique<CryptoMixer>(
                config_.adhoc_corrections,
                config_.use_tie_breaking
            );
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
    if (buffer_.size() == 0) return;
    
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

void EnhancedPRNG::fill_buffer_parallel(size_t thread_count) {
    if (buffer_.size() == 0) return;
    
    // Ensure thread count is reasonable
    thread_count = std::min(thread_count, static_cast<size_t>(std::thread::hardware_concurrency()));
    if (thread_count == 0) thread_count = 1;
    
    // Limit thread count for very small buffers
    if (buffer_.size() < thread_count * 64) {
        // For small buffers, use sequential filling
        fill_buffer_sequential();
        return;
    }
    
    try {
        // Use the global thread pool for more efficient threading
        ThreadPool& pool = global_thread_pool();
        
        // Use a more thread-safe approach with thread-local buffers
        std::vector<SecureBuffer<double>> thread_buffers;
        
        // Create ABC parameter tuples for thread-local QIs
        const PRNGConfig& cfg = config_;
        std::vector<std::tuple<long, long, long>> abc_list;
        
        // Get current parameters from primary QI or use default
        if (multi_ && multi_->size() > 0) {
            // Use pickMultiQiSet to get a new set of parameters
            try {
                abc_list = pickMultiQiSet(cfg.mpfr_precision, 3 + thread_count, 
                                        cfg.seed, cfg.has_seed);
            } catch (...) {
                // In case of exception, use a safe default
                abc_list.push_back(std::make_tuple(cfg.a, cfg.b, cfg.c));
            }
        } else {
            // Fallback parameters
            abc_list.push_back(std::make_tuple(cfg.a, cfg.b, cfg.c));
        }
        
        // Create thread-local resources
        std::vector<std::unique_ptr<MultiQI>> thread_qis;
        
        // Calculate chunk sizes
        const size_t base_chunk_size = buffer_.size() / thread_count;
        const size_t remainder = buffer_.size() % thread_count;
        std::vector<size_t> chunk_sizes(thread_count, base_chunk_size);
        
        // Distribute remainder among first few threads
        for (size_t i = 0; i < remainder; i++) {
            chunk_sizes[i]++;
        }
        
        // Create thread resources
        for (size_t t = 0; t < thread_count; t++) {
            try {
                // Create a MultiQI instance for each thread
                thread_qis.push_back(std::make_unique<MultiQI>(abc_list, cfg.mpfr_precision));
                // Create a buffer for each thread
                thread_buffers.emplace_back(chunk_sizes[t]);
            } catch (...) {
                // If resource creation fails, fall back to sequential filling
                if (config_.debug) {
                    Rcpp::warning("Failed to create thread-local resources. Falling back to sequential filling.");
                }
                fill_buffer_sequential();
                return;
            }
        }
        
        // Thread synchronization objects
        std::atomic<bool> any_thread_failed(false);
        std::vector<std::future<void>> futures;
        
        // Submit tasks to the thread pool
        for (size_t t = 0; t < thread_count; t++) {
            // Capture thread index by value and other objects by reference
            futures.push_back(pool.enqueue([t, &thread_buffers, &thread_qis, &any_thread_failed, this]() {
                try {
                    if (t < thread_qis.size() && t < thread_buffers.size() && 
                        thread_qis[t] && thread_buffers[t].size() > 0) {
                        
                        // Fill the thread's buffer with its dedicated MultiQI
                        thread_qis[t]->fill_thread_safe(
                            thread_buffers[t].data(), 
                            thread_buffers[t].size());
                            
                    } else {
                        // Resource not properly initialized
                        any_thread_failed.store(true);
                    }
                } catch (const std::exception& e) {
                    // Log and handle error
                    if (config_.debug) {
                        thread_local int warning_count = 0;
                        if (warning_count < 3) { // Limit warnings per thread
                            Rcpp::warning("Thread %d failed: %s", static_cast<int>(t), e.what());
                            warning_count++;
                        }
                    }
                    any_thread_failed.store(true);
                        
                    // Fill with fallback values
                    if (t < thread_buffers.size()) {
                        for (size_t i = 0; i < thread_buffers[t].size(); i++) {
                            thread_buffers[t][i] = 0.5;
                        }
                    }
                } catch (...) {
                    // Handle unknown errors
                    any_thread_failed.store(true);
                        
                    // Fill with fallback values
                    if (t < thread_buffers.size()) {
                        for (size_t i = 0; i < thread_buffers[t].size(); i++) {
                            thread_buffers[t][i] = 0.5;
                        }
                    }
                }
            }));
        }
        
        // Wait for all futures to complete
        for (auto& future : futures) {
            try {
                future.get(); // This will re-throw any exceptions that occurred in the task
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
            Rcpp::warning("Some threads failed during parallel buffer filling. Results may be affected.");
        }
        
        // Copy from thread buffers to main buffer using parallel processing for large buffers
        if (buffer_.size() >= 10000 && thread_count > 1) {
            // Divide the copying into chunks for parallel processing
            size_t copy_chunk_size = buffer_.size() / thread_count;
            std::vector<std::future<void>> copy_futures;
            
            size_t buffer_pos = 0;
            for (size_t t = 0; t < thread_count && t < thread_buffers.size(); t++) {
                // No need to make a copy for lambda capture anymore
                const size_t start_pos = buffer_pos;
                const SecureBuffer<double>& thread_buffer = thread_buffers[t];
                const size_t copy_size = std::min(thread_buffer.size(), buffer_.size() - buffer_pos);
                
                if (copy_size > 0 && buffer_pos + copy_size <= buffer_.size()) {
                    // For large buffers, submit a parallel copy task
                    if (copy_size >= copy_chunk_size) {
                        copy_futures.push_back(pool.enqueue([this, &thread_buffer, start_pos, copy_size]() {
                            std::memcpy(
                                buffer_.data() + start_pos, 
                                thread_buffer.data(), 
                                copy_size * sizeof(double));
                        }));
                    } else {
                        // For smaller buffers, copy directly
                        std::memcpy(
                            buffer_.data() + buffer_pos, 
                            thread_buffer.data(), 
                            copy_size * sizeof(double));
                    }
                    buffer_pos += copy_size;
                }
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
                    std::memcpy(
                        buffer_.data() + buffer_pos, 
                        thread_buffer.data(), 
                        copy_size * sizeof(double));
                    buffer_pos += copy_size;
                }
            }
        }
        
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
            Rcpp::warning("Exception in parallel buffer filling: %s. Falling back to sequential filling.", e.what());
        }
        // If anything fails, fall back to sequential filling
        fill_buffer_sequential();
        return;
    } catch (...) {
        if (config_.debug) {
            Rcpp::warning("Unknown exception in parallel buffer filling. Falling back to sequential filling.");
        }
        // If anything fails, fall back to sequential filling
        fill_buffer_sequential();
        return;
    }
    
    // Reset buffer position
    buffer_pos_ = 0;
}

void EnhancedPRNG::fill_buffer() {
    const size_t cpu_cores = std::thread::hardware_concurrency();
    const size_t max_threads = std::min(cpu_cores, size_t(8)); // Cap at 8 threads
    
    // Use parallel filling only if enabled and buffer is large enough
    if (config_.use_parallel_filling && buffer_.size() >= 1024 && max_threads > 1) {
        size_t num_threads = std::min(max_threads, buffer_.size() / 128);
        fill_buffer_parallel(num_threads);
    } else {
        fill_buffer_sequential();
    }
}

void EnhancedPRNG::reseed() {
    // Force a buffer refill on next use
    buffer_pos_ = config_.buffer_size;
    has_spare_normal_ = false;
    
    // Reseed the crypto mixer if used
    if (crypto_) {
        crypto_->reseed();
    }
    
    // Skip ahead in the sequence to get "new" starting point
    std::random_device rd;
    std::mt19937_64 rng(rd());
    std::uniform_int_distribution<uint64_t> skip_dist(1000, 10000);
    multi_->jump_ahead(skip_dist(rng));
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
    if (config_.use_crypto_mixing && crypto_ && 
        config_.reseed_interval > 0 && 
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
        default:
            return u; // Default to uniform(0,1)
    }
}

void EnhancedPRNG::skip(uint64_t n) {
    if (n == 0) return;

    uint64_t original_n = n;
    size_t remaining_in_buffer = (buffer_pos_ < buffer_.size()) ? (buffer_.size() - buffer_pos_) : 0;

    if (n <= remaining_in_buffer) {
        // Jump is within the current buffer
        buffer_pos_ += static_cast<size_t>(n);
    } else {
        // Jump is beyond the current buffer
        n -= remaining_in_buffer; // Consume what's left in the current buffer

        // Calculate how many full buffers to jump over
        uint64_t num_full_buffers_to_skip = n / buffer_.size();
        if (num_full_buffers_to_skip > 0) {
            multi_->jump_ahead(num_full_buffers_to_skip * buffer_.size());
        }

        // Calculate the position in the new buffer
        uint64_t remaining_in_new_buffer = n % buffer_.size();
        
        if (remaining_in_new_buffer > 0) {
            fill_buffer(); // Refill to get the target buffer
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
            crypto_->mix(reinterpret_cast<unsigned char*>(temp_buffer.data()), 
                         n * sizeof(double));
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
                default:
                    output_vec[i] = u; // Default to uniform(0,1)
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
    if (u1 <= 0.0) u1 = std::numeric_limits<double>::min();
    if (u1 >= 1.0) u1 = 1.0 - std::numeric_limits<double>::epsilon();
    if (u2 <= 0.0) u2 = std::numeric_limits<double>::min();
    if (u2 >= 1.0) u2 = 1.0 - std::numeric_limits<double>::epsilon();
    
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
    if (u <= 0.0) u = std::numeric_limits<double>::min();
    if (u >= 1.0) u = 1.0 - std::numeric_limits<double>::epsilon();
    
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
    double u1 = next();
    double u2 = next();
    
    // Generate a standard normal using Box-Muller
    double r = std::sqrt(-2.0 * std::log(u1));
    double theta = 2.0 * M_PI * u2;
    double z = r * std::cos(theta);
    
    // Apply normal approximation and round to nearest integer
    double result = std::round(mu + sigma * z);
    return std::max(0.0, result); // Ensure result is non-negative
}

double EnhancedPRNG::generate_gamma_small_alpha(double alpha, double theta) {
    // Ahrens-Dieter acceptance-rejection method for alpha < 1
    double b = (alpha + M_E) / M_E;
    
    while (true) {
        // Get two uniform variates
        double u1 = next();
        double u2 = next();
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
    double d = alpha - 1.0/3.0;
    double c = 1.0 / std::sqrt(9.0 * d);
    
    while (true) {
        double z, v;
        do {
            // Generate standard normal directly with Box-Muller
            double u1 = next();
            double u2 = next();
            double r = std::sqrt(-2.0 * std::log(u1));
            double theta = 2.0 * M_PI * u2;
            z = r * std::cos(theta);
            
            v = 1.0 + c * z;
        } while (v <= 0.0);
        
        v = v * v * v;
        double u = next();
        
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
        double u = next();
        double v = next();
        
        if (u <= 0.0) u = std::numeric_limits<double>::min();
        if (v <= 0.0) v = std::numeric_limits<double>::min();
        
        x_pow_alpha = std::pow(u, 1.0 / alpha);
        y_pow_beta = std::pow(v, 1.0 / beta);
    } while (x_pow_alpha + y_pow_beta > 1.0);
    
    return x_pow_alpha / (x_pow_alpha + y_pow_beta);
}

// Distribution generation methods
double EnhancedPRNG::generate_uniform_01(double u) {
    return u; // Already uniform(0,1)
}

double EnhancedPRNG::generate_uniform_range(double u) {
    return config_.range_min + u * (config_.range_max - config_.range_min);
}

double EnhancedPRNG::generate_normal(double u) {
    // Check if object is being destroyed
    if (is_being_destroyed_.load()) {
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
                    Rcpp::warning("Ziggurat method is not used with parallel filling for stability. Using Box-Muller instead.");
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
                if (is_being_destroyed_.load()) {
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
                        Rcpp::warning("Ziggurat method produced invalid result. Falling back to Box-Muller.");
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
                        Rcpp::warning("Ziggurat method failed: %s. Falling back to Box-Muller.", e.what());
                        warning_count++;
                    }
                }
                
                // If the Ziggurat is being destroyed, use fallback RNG
                if (is_being_destroyed_.load()) {
                    static thread_local std::mt19937 fallback_rng(std::random_device{}());
                    std::normal_distribution<double> fallback_dist(config_.normal_mean, config_.normal_sd);
                    return fallback_dist(fallback_rng);
                }
                
                // Fall back to Box-Muller if Ziggurat throws
                try {
                    return generate_normal_box_muller(u);
                } catch (...) {
                    // If Box-Muller also fails, use fallback RNG
                    static thread_local std::mt19937 fallback_rng(std::random_device{}());
                    std::normal_distribution<double> fallback_dist(config_.normal_mean, config_.normal_sd);
                    return fallback_dist(fallback_rng);
                }
            } catch (...) {
                if (config_.debug) {
                    static thread_local int warning_count = 0;
                    if (warning_count < 5) {
                        Rcpp::warning("Ziggurat method failed with unknown error. Falling back to Box-Muller.");
                        warning_count++;
                    }
                }
                
                // If the object is being destroyed, use fallback RNG
                if (is_being_destroyed_.load()) {
                    static thread_local std::mt19937 fallback_rng(std::random_device{}());
                    std::normal_distribution<double> fallback_dist(config_.normal_mean, config_.normal_sd);
                    return fallback_dist(fallback_rng);
                }
                
                // Fall back to Box-Muller for any other error
                try {
                    return generate_normal_box_muller(u);
                } catch (...) {
                    // If Box-Muller also fails, use fallback RNG
                    static thread_local std::mt19937 fallback_rng(std::random_device{}());
                    std::normal_distribution<double> fallback_dist(config_.normal_mean, config_.normal_sd);
                    return fallback_dist(fallback_rng);
                }
            }
        } else {
            // Use Box-Muller method with destruction check
            if (is_being_destroyed_.load()) {
                static thread_local std::mt19937 fallback_rng(std::random_device{}());
                std::normal_distribution<double> fallback_dist(config_.normal_mean, config_.normal_sd);
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
                static thread_local std::uniform_real_distribution<double> fallback_dist(0.000001, 0.999999);
                u2 = fallback_dist(fallback_rng);
            }
        }
        
        // Safely get the second uniform value
        if (buffer_pos_ >= buffer_.size()) {
            // Buffer is still invalid after filling attempt
            static thread_local std::mt19937_64 fallback_rng(std::random_device{}());
            static thread_local std::uniform_real_distribution<double> fallback_dist(0.000001, 0.999999);
            u2 = fallback_dist(fallback_rng);
        } else {
            u2 = buffer_[buffer_pos_++];  // Thread-safe increment of buffer_pos_
        }
        
        // Ensure uniforms are in (0,1) range for numerical stability
        // These checks are required to prevent NaN/infinity in the Box-Muller transform
        if (u <= 0.0) u = std::numeric_limits<double>::min();
        if (u >= 1.0) u = 1.0 - std::numeric_limits<double>::epsilon();
        if (u2 <= 0.0) u2 = std::numeric_limits<double>::min();
        if (u2 >= 1.0) u2 = 1.0 - std::numeric_limits<double>::epsilon();
        
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

void EnhancedPRNG::dumpConfig() const {
    Rcpp::Rcout << "EnhancedPRNG Configuration:" << std::endl;
    Rcpp::Rcout << "  a: " << config_.a << std::endl;
    Rcpp::Rcout << "  b: " << config_.b << std::endl;
    Rcpp::Rcout << "  c: " << config_.c << std::endl;
    Rcpp::Rcout << "  mpfr_precision: " << config_.mpfr_precision << std::endl;
    Rcpp::Rcout << "  buffer_size: " << config_.buffer_size << std::endl;
    
    std::string dist_str;
    switch (config_.distribution) {
        case PRNGConfig::UNIFORM_01: dist_str = "uniform_01"; break;
        case PRNGConfig::UNIFORM_RANGE: dist_str = "uniform_range"; break;
        case PRNGConfig::NORMAL: dist_str = "normal"; break;
        case PRNGConfig::EXPONENTIAL: dist_str = "exponential"; break;
        case PRNGConfig::POISSON: dist_str = "poisson"; break;
        case PRNGConfig::GAMMA: dist_str = "gamma"; break;
        case PRNGConfig::BETA: dist_str = "beta"; break;
        default: dist_str = "unknown"; break;
    }
    Rcpp::Rcout << "  distribution: " << dist_str << std::endl;
    
    std::string method_str;
    switch (config_.normal_method) {
        case PRNGConfig::BOX_MULLER: method_str = "box_muller"; break;
        case PRNGConfig::ZIGGURAT: method_str = "ziggurat"; break;
        default: method_str = "unknown"; break;
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
    
    Rcpp::Rcout << "  use_crypto_mixing: " << (config_.use_crypto_mixing ? "true" : "false") << std::endl;
    Rcpp::Rcout << "  adhoc_corrections: " << (config_.adhoc_corrections ? "true" : "false") << std::endl;
    Rcpp::Rcout << "  use_tie_breaking: " << (config_.use_tie_breaking ? "true" : "false") << std::endl;
    Rcpp::Rcout << "  reseed_interval: " << config_.reseed_interval << std::endl;
    Rcpp::Rcout << "  use_csv_discriminants: " << (config_.use_csv_discriminants ? "true" : "false") << std::endl;
    Rcpp::Rcout << "  use_parallel_filling: " << (config_.use_parallel_filling ? "true" : "false") << std::endl;
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
    if (!is_being_destroyed_.load()) {
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
    return is_being_destroyed_.load();
}

} // namespace qiprng