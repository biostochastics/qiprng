# CodeConCat Output

# AI-Friendly Code Summary

This document contains a structured representation of a codebase, organized for AI analysis.

## Repository Structure

```
Total code files: 10
Documentation files: 0
Total functions found: 0
Average function length: 0 lines

File types:
- cpp: 10 files
```

## Potential Entry Points

No common entry point files detected.

## Key Files

Key files based on available annotations:

- `/Users/biostochastics/Development/GitHub/qiprng/src/enhanced_prng.cpp`: No declarations found
- `/Users/biostochastics/Development/GitHub/qiprng/src/RcppExports.cpp`: No declarations found
- `/Users/biostochastics/Development/GitHub/qiprng/src/prng_utils.cpp`: No declarations found
- `/Users/biostochastics/Development/GitHub/qiprng/src/rcpp_exports.cpp`: No declarations found
- `/Users/biostochastics/Development/GitHub/qiprng/src/test_thread_safety.cpp`: No declarations found
- `/Users/biostochastics/Development/GitHub/qiprng/src/prng_warnings.cpp`: No declarations found
- `/Users/biostochastics/Development/GitHub/qiprng/src/ziggurat_normal.cpp`: No declarations found
- `/Users/biostochastics/Development/GitHub/qiprng/src/quadratic_irrational.cpp`: No declarations found
- `/Users/biostochastics/Development/GitHub/qiprng/src/crypto_mixer.cpp`: No declarations found
- `/Users/biostochastics/Development/GitHub/qiprng/src/multi_qi.cpp`: No declarations found

## Code Organization

The code is organized into sections, each prefixed with clear markers:

- Directory markers show file organization
- File headers contain metadata and imports
- Annotations provide context about code purpose
- Documentation sections contain project documentation

## Navigation

- Each file begins with '---FILE:' followed by its path
- Each section is clearly delimited with markdown headers
- Code blocks are formatted with appropriate language tags

---
Begin code content below:

## Directory Structure

```
[INFO] Directory tree would be displayed here.

```

---

## File Index

```
/Users/biostochastics/Development/GitHub/qiprng/src/enhanced_prng.cpp
/Users/biostochastics/Development/GitHub/qiprng/src/RcppExports.cpp
/Users/biostochastics/Development/GitHub/qiprng/src/prng_utils.cpp
/Users/biostochastics/Development/GitHub/qiprng/src/rcpp_exports.cpp
/Users/biostochastics/Development/GitHub/qiprng/src/test_thread_safety.cpp
/Users/biostochastics/Development/GitHub/qiprng/src/prng_warnings.cpp
/Users/biostochastics/Development/GitHub/qiprng/src/ziggurat_normal.cpp
/Users/biostochastics/Development/GitHub/qiprng/src/quadratic_irrational.cpp
/Users/biostochastics/Development/GitHub/qiprng/src/crypto_mixer.cpp
/Users/biostochastics/Development/GitHub/qiprng/src/multi_qi.cpp
```

## Files

## File: /Users/biostochastics/Development/GitHub/qiprng/src/enhanced_prng.cpp

### Summary

No declarations found

**Tags**: `cpp`

```cpp
## File: /Users/biostochastics/Development/GitHub/qiprng/src/enhanced_prng.cpp
```cpp
// File: enhanced_prng.cpp
// --------------------------------------------------------------
#include "enhanced_prng.hpp"
#include <algorithm> // For std::min, std::max
#include <type_traits> // For std::is_same
#include <cmath> // For std::isnan, std::isinf, std::round, std::exp, std::pow, std::sqrt, std::log, std::fmod, std::floor
#include <limits> // For std::numeric_limits
#include <stdexcept> // For std::invalid_argument

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

void EnhancedPRNG::copy_thread_buffers_to_main(size_t thread_count,
                                              const std::vector<SecureBuffer<double>>& thread_buffers,
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
}

void EnhancedPRNG::submit_parallel_tasks(ThreadPool& pool,
                                        size_t thread_count,
                                        std::vector<SecureBuffer<double>>& thread_buffers,
                                        std::vector<std::unique_ptr<MultiQI>>& thread_qis,
                                        std::atomic<bool>& any_thread_failed,
                                        std::vector<std::future<void>>& futures) {
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

                // Fill with proper random fallback values
                if (t < thread_buffers.size()) {
                    // Use a simple thread-safe approach for fallback
                    std::random_device rd;
                    std::mt19937_64 rng(rd());
                    std::uniform_real_distribution<double> dist(0.0, 1.0);
                    for (size_t i = 0; i < thread_buffers[t].size(); i++) {
                        thread_buffers[t][i] = dist(rng);
                    }
                }
            } catch (...) {
                // Handle unknown errors
                any_thread_failed.store(true);

                // Fill with proper random fallback values
                if (t < thread_buffers.size()) {
                    // Use a simple thread-safe approach for fallback
                    std::random_device rd;
                    std::mt19937_64 rng(rd());
                    std::uniform_real_distribution<double> dist(0.0, 1.0);
                    for (size_t i = 0; i < thread_buffers[t].size(); i++) {
                        thread_buffers[t][i] = dist(rng);
                    }
                }
            }
        }));
    }
}

bool EnhancedPRNG::create_thread_resources(size_t thread_count,
                                          std::vector<std::vector<std::tuple<long, long, long>>>& thread_abc_lists,
                                          std::vector<std::unique_ptr<MultiQI>>& thread_qis,
                                          std::vector<SecureBuffer<double>>& thread_buffers,
                                          std::vector<size_t>& chunk_sizes) {
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
                abc_list = pickMultiQiSet(cfg.mpfr_precision, 3,
                                        thread_seed, cfg.has_seed);
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
            thread_qis.push_back(std::make_unique<MultiQI>(thread_abc_lists[t], cfg.mpfr_precision));
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

        // Create containers for thread resources
        std::vector<std::vector<std::tuple<long, long, long>>> thread_abc_lists;
        std::vector<std::unique_ptr<MultiQI>> thread_qis;
        std::vector<SecureBuffer<double>> thread_buffers;
        std::vector<size_t> chunk_sizes;

        // Create thread resources using helper function
        if (!create_thread_resources(thread_count, thread_abc_lists, thread_qis,
                                    thread_buffers, chunk_sizes)) {
            // Fall back to sequential filling if resource creation fails
            fill_buffer_sequential();
            return;
        }

        // Thread synchronization objects
        std::atomic<bool> any_thread_failed(false);
        std::vector<std::future<void>> futures;

        // Submit parallel tasks using helper function
        submit_parallel_tasks(pool, thread_count, thread_buffers, thread_qis,
                            any_thread_failed, futures);

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
    // Use smaller range for reseed skips since this happens during runtime
    // and we've already done a large warm-up during initialization
    const uint64_t MIN_RESEED_SKIP = 1000;   // Minimum to ensure state change
    const uint64_t MAX_RESEED_SKIP = 10000;  // Maximum to keep reseed fast

    std::random_device rd;
    std::mt19937_64 rng(rd());
    std::uniform_int_distribution<uint64_t> skip_dist(MIN_RESEED_SKIP, MAX_RESEED_SKIP);
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
    double u1 = next_raw_uniform();
    double u2 = next_raw_uniform();

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
    double d = alpha - 1.0/3.0;
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
    if (safe_u <= 0.0) safe_u = std::numeric_limits<double>::epsilon();
    if (safe_u >= 1.0) safe_u = 1.0 - std::numeric_limits<double>::epsilon();

    double result = config_.weibull_scale * std::pow(-std::log(safe_u), 1.0 / config_.weibull_shape);

    if (std::isnan(result) || std::isinf(result)) {
        return config_.weibull_scale; // Fallback to scale parameter
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
        return std::exp(config_.lognormal_mu); // Fallback to median
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
    if (config_.binomial_n == 0 || config_.binomial_p == 0.0) return 0.0;
    if (config_.binomial_p == 1.0) return static_cast<double>(config_.binomial_n);

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
        if (std::isnan(result) || std::isinf(result)) return mu;
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
        return 0.0; // Fallback to median
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
            if (trials > 1e6) break; // Prevent infinite loop
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
        case PRNGConfig::UNIFORM_01: dist_str = "uniform_01"; break;
        case PRNGConfig::UNIFORM_RANGE: dist_str = "uniform_range"; break;
        case PRNGConfig::NORMAL: dist_str = "normal"; break;
        case PRNGConfig::EXPONENTIAL: dist_str = "exponential"; break;
        case PRNGConfig::POISSON: dist_str = "poisson"; break;
        case PRNGConfig::GAMMA: dist_str = "gamma"; break;
        case PRNGConfig::BETA: dist_str = "beta"; break;
        case PRNGConfig::BERNOULLI: dist_str = "bernoulli"; break;
        case PRNGConfig::BINOMIAL: dist_str = "binomial"; break;
        case PRNGConfig::LOGNORMAL: dist_str = "lognormal"; break;
        case PRNGConfig::WEIBULL: dist_str = "weibull"; break;
        case PRNGConfig::CHISQUARED: dist_str = "chisquared"; break;
        case PRNGConfig::STUDENT_T: dist_str = "student_t"; break;
        case PRNGConfig::NEGATIVE_BINOMIAL: dist_str = "negative_binomial"; break;
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
```

```

---

## File: /Users/biostochastics/Development/GitHub/qiprng/src/RcppExports.cpp

### Summary
No declarations found

**Tags**: `cpp`

```cpp
## File: /Users/biostochastics/Development/GitHub/qiprng/src/RcppExports.cpp
```cpp
// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// initialize_libsodium_
void initialize_libsodium_();
RcppExport SEXP _qiprng_initialize_libsodium_() {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    initialize_libsodium_();
    return R_NilValue;
END_RCPP
}
// createPRNG_
void createPRNG_(Rcpp::List rcfg);
RcppExport SEXP _qiprng_createPRNG_(SEXP rcfgSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List >::type rcfg(rcfgSEXP);
    createPRNG_(rcfg);
    return R_NilValue;
END_RCPP
}
// updatePRNG_
void updatePRNG_(Rcpp::List rcfg);
RcppExport SEXP _qiprng_updatePRNG_(SEXP rcfgSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List >::type rcfg(rcfgSEXP);
    updatePRNG_(rcfg);
    return R_NilValue;
END_RCPP
}
// getPRNGConfig_
Rcpp::List getPRNGConfig_();
RcppExport SEXP _qiprng_getPRNGConfig_() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(getPRNGConfig_());
    return rcpp_result_gen;
END_RCPP
}
// dumpPRNGConfig_
void dumpPRNGConfig_();
RcppExport SEXP _qiprng_dumpPRNGConfig_() {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    dumpPRNGConfig_();
    return R_NilValue;
END_RCPP
}
// generatePRNG_
Rcpp::NumericVector generatePRNG_(int n);
RcppExport SEXP _qiprng_generatePRNG_(SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    rcpp_result_gen = Rcpp::wrap(generatePRNG_(n));
    return rcpp_result_gen;
END_RCPP
}
// reseedPRNG_
void reseedPRNG_();
RcppExport SEXP _qiprng_reseedPRNG_() {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    reseedPRNG_();
    return R_NilValue;
END_RCPP
}
// cleanup_prng_
void cleanup_prng_();
RcppExport SEXP _qiprng_cleanup_prng_() {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    cleanup_prng_();
    return R_NilValue;
END_RCPP
}
// skipPRNG_
void skipPRNG_(int n);
RcppExport SEXP _qiprng_skipPRNG_(SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    skipPRNG_(n);
    return R_NilValue;
END_RCPP
}
// jumpAheadPRNG_
void jumpAheadPRNG_(double n);
RcppExport SEXP _qiprng_jumpAheadPRNG_(SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type n(nSEXP);
    jumpAheadPRNG_(n);
    return R_NilValue;
END_RCPP
}
// suppress_mpfr_warnings_
bool suppress_mpfr_warnings_();
RcppExport SEXP _qiprng_suppress_mpfr_warnings_() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(suppress_mpfr_warnings_());
    return rcpp_result_gen;
END_RCPP
}
// set_mpfr_warnings_
void set_mpfr_warnings_(bool show_warnings);
RcppExport SEXP _qiprng_set_mpfr_warnings_(SEXP show_warningsSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< bool >::type show_warnings(show_warningsSEXP);
    set_mpfr_warnings_(show_warnings);
    return R_NilValue;
END_RCPP
}
// cleanupPRNG_ThreadSafe_
bool cleanupPRNG_ThreadSafe_();
RcppExport SEXP _qiprng_cleanupPRNG_ThreadSafe_() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(cleanupPRNG_ThreadSafe_());
    return rcpp_result_gen;
END_RCPP
}
// cleanupPRNG_Final_
bool cleanupPRNG_Final_();
RcppExport SEXP _qiprng_cleanupPRNG_Final_() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(cleanupPRNG_Final_());
    return rcpp_result_gen;
END_RCPP
}
// test_choose_discriminant
Rcpp::List test_choose_discriminant(int thread_count, int iterations);
RcppExport SEXP _qiprng_test_choose_discriminant(SEXP thread_countSEXP, SEXP iterationsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type thread_count(thread_countSEXP);
    Rcpp::traits::input_parameter< int >::type iterations(iterationsSEXP);
    rcpp_result_gen = Rcpp::wrap(test_choose_discriminant(thread_count, iterations));
    return rcpp_result_gen;
END_RCPP
}
// check_discriminants_unique
bool check_discriminants_unique(Rcpp::List discriminant_lists);
RcppExport SEXP _qiprng_check_discriminants_unique(SEXP discriminant_listsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List >::type discriminant_lists(discriminant_listsSEXP);
    rcpp_result_gen = Rcpp::wrap(check_discriminants_unique(discriminant_lists));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_qiprng_initialize_libsodium_", (DL_FUNC) &_qiprng_initialize_libsodium_, 0},
    {"_qiprng_createPRNG_", (DL_FUNC) &_qiprng_createPRNG_, 1},
    {"_qiprng_updatePRNG_", (DL_FUNC) &_qiprng_updatePRNG_, 1},
    {"_qiprng_getPRNGConfig_", (DL_FUNC) &_qiprng_getPRNGConfig_, 0},
    {"_qiprng_dumpPRNGConfig_", (DL_FUNC) &_qiprng_dumpPRNGConfig_, 0},
    {"_qiprng_generatePRNG_", (DL_FUNC) &_qiprng_generatePRNG_, 1},
    {"_qiprng_reseedPRNG_", (DL_FUNC) &_qiprng_reseedPRNG_, 0},
    {"_qiprng_cleanup_prng_", (DL_FUNC) &_qiprng_cleanup_prng_, 0},
    {"_qiprng_skipPRNG_", (DL_FUNC) &_qiprng_skipPRNG_, 1},
    {"_qiprng_jumpAheadPRNG_", (DL_FUNC) &_qiprng_jumpAheadPRNG_, 1},
    {"_qiprng_suppress_mpfr_warnings_", (DL_FUNC) &_qiprng_suppress_mpfr_warnings_, 0},
    {"_qiprng_set_mpfr_warnings_", (DL_FUNC) &_qiprng_set_mpfr_warnings_, 1},
    {"_qiprng_cleanupPRNG_ThreadSafe_", (DL_FUNC) &_qiprng_cleanupPRNG_ThreadSafe_, 0},
    {"_qiprng_cleanupPRNG_Final_", (DL_FUNC) &_qiprng_cleanupPRNG_Final_, 0},
    {"_qiprng_test_choose_discriminant", (DL_FUNC) &_qiprng_test_choose_discriminant, 2},
    {"_qiprng_check_discriminants_unique", (DL_FUNC) &_qiprng_check_discriminants_unique, 1},
    {NULL, NULL, 0}
};

RcppExport void R_init_qiprng(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}

```

```

---

## File: /Users/biostochastics/Development/GitHub/qiprng/src/prng_utils.cpp

### Summary
No declarations found

**Tags**: `cpp`

```cpp
## File: /Users/biostochastics/Development/GitHub/qiprng/src/prng_utils.cpp
```cpp
// File: prng_utils.cpp
// --------------------------------------------------------------
#include "prng_utils.hpp"
#include "enhanced_prng.hpp" // For accessing g_prng->getConfig(), t_prng->getConfig()
#include "deterministic_rng.hpp" // For DeterministicRNGFactory
#include <sodium.h>
#include <fstream>
#include <sstream>
#include <algorithm> // For std::min, std::shuffle, std::abs
#include <set>       // For std::set in chooseUniqueDiscriminant
#include <chrono>    // For std::chrono in chooseUniqueDiscriminant fallback
#include <numeric>   // For std::iota
#include <limits>    // For std::numeric_limits

// Define globals that were declared extern in prng_utils.hpp
namespace qiprng {

// LibSodium initialization
std::atomic<bool> sodium_initialized_flag(false); // Definition
static std::once_flag sodium_init_flag;

// Global PRNG state
bool g_use_threading = false; // Default to false
std::mutex g_prng_mutex;
thread_local std::unique_ptr<qiprng::EnhancedPRNG> t_prng = nullptr;
std::unique_ptr<qiprng::EnhancedPRNG> g_prng = nullptr;

// Discriminant utilities
std::mutex g_disc_mutex;
std::unordered_set<long long> g_used_discriminants;

// CSV Discriminant utilities
std::vector<std::tuple<long, long, long, long long>> g_csv_discriminants;
std::mutex g_csv_disc_mutex;
bool g_csv_discriminants_loaded = false;

// This ensures the global `sodium_initialized` used by CryptoMixer is this one.
bool sodium_initialized = false; // Will be set by Rcpp export `initialize_libsodium_`

} // namespace qiprng

// LibSodium initialization
void qiprng::initialize_libsodium_if_needed() {
    std::call_once(sodium_init_flag, []() {
        if (sodium_init() < 0) {
            Rcpp::warning("Failed to initialize libsodium. Cryptographic operations may fail or be insecure.");
            sodium_initialized_flag.store(false);
        } else {
            sodium_initialized_flag.store(true);
        }
    });
}

// Thread-local random engine
std::mt19937_64& qiprng::getThreadLocalEngine() {
    static thread_local std::unique_ptr<std::mt19937_64> t_engine_ptr;
    if (!t_engine_ptr) {
        std::random_device rd;
        std::array<std::uint32_t, std::mt19937_64::state_size / 2> seeds; // Correct seeding
        for(size_t i = 0; i < seeds.size(); ++i) seeds[i] = rd();

        // Incorporate thread ID for better seed diversity across threads
        auto tid_hash = std::hash<std::thread::id>()(std::this_thread::get_id());
        seeds[0] ^= static_cast<uint32_t>(tid_hash & 0xFFFFFFFFULL);
        if (seeds.size() > 1) {
             seeds[1] ^= static_cast<uint32_t>((tid_hash >> 32) & 0xFFFFFFFFULL);
        }
        std::seed_seq seq(seeds.begin(), seeds.end());
        t_engine_ptr = std::make_unique<std::mt19937_64>(seq);
    }
    return *t_engine_ptr;
}

// Deterministic thread-local random engine
std::mt19937_64& qiprng::getDeterministicThreadLocalEngine(uint64_t seed) {
    static thread_local std::unique_ptr<std::mt19937_64> t_det_engine_ptr;
    static thread_local uint64_t t_last_seed = 0;

    if (!t_det_engine_ptr || t_last_seed != seed) {
        // Get unique thread ID
        std::hash<std::thread::id> hasher;
        size_t thread_id = hasher(std::this_thread::get_id());

        // Create deterministic RNG for this thread
        t_det_engine_ptr = std::make_unique<std::mt19937_64>(
            DeterministicRNGFactory::create_for_thread(seed, thread_id)
        );
        t_last_seed = seed;
    }

    return *t_det_engine_ptr;
}

// Multi-QI parameter set selection
std::vector<std::tuple<long, long, long>> qiprng::pickMultiQiSet(int precision, int count,
                                                                 uint64_t seed, bool has_seed) {
    std::vector<std::tuple<long, long, long>> result;

    // First, try to load and use excellent discriminants from CSV
    loadCSVDiscriminants();

    std::vector<std::tuple<long, long, long>> baseParams;

    // If CSV discriminants are loaded, use them; otherwise fallback to hardcoded values
    if (g_csv_discriminants_loaded && !g_csv_discriminants.empty()) {
        // Convert CSV discriminants (a,b,c,discriminant) to baseParams (a,b,c)
        for (const auto& disc : g_csv_discriminants) {
            baseParams.emplace_back(std::get<0>(disc), std::get<1>(disc), std::get<2>(disc));
        }
    } else {
        // Fallback to hardcoded excellent discriminants if CSV loading fails
        baseParams = {
            {1, 1, -1}, {2, 3, -1}, {1, 2, -1}, {1, 3, -2}, {2, 5, -3},
            {3, 5, -2}, {1, 4, -3}, {2, 7, -4}, {3, 7, -4}, {1, 5, -6},
            {2, 9, -10}, {3, 11, -10}, {4, 9, -5}, {5, 11, -6},
            {6, 13, -7}, {7, 15, -8}
        };
    }

    if (count <= 0) {
        return result; // Return empty if count is not positive
    }

    if (count <= static_cast<int>(baseParams.size())) {
        result.insert(result.end(), baseParams.begin(), baseParams.begin() + count);
    } else {
        result = baseParams;
        int remaining = count - baseParams.size();
        std::vector<int> primes = {2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53};

        // Get appropriate RNG based on whether we have a seed
        std::mt19937_64& rng = has_seed
            ? getDeterministicThreadLocalEngine(seed)
            : getThreadLocalEngine();

        for (int i = 0; i < remaining; i++) {
            size_t baseIdx = i % baseParams.size();
            int prime1_idx = i % primes.size();
            int prime2_idx = (i + 1 + (rng() % (primes.size()-1) )) % primes.size(); // Add some randomness to prime2 selection
            if(prime1_idx == prime2_idx) prime2_idx = (prime2_idx + 1) % primes.size();

            auto baseParam = baseParams[baseIdx];
            long a = std::get<0>(baseParam);
            long b = std::get<1>(baseParam);
            long c = std::get<2>(baseParam);

            // Apply more varied tweaks
            std::uniform_int_distribution<int> tweak_dist_small(-2, 2);
            std::uniform_int_distribution<int> tweak_dist_large(-primes[prime2_idx]/2, primes[prime2_idx]/2);

            a += tweak_dist_small(rng);
            b += primes[prime1_idx] + tweak_dist_large(rng);
            c += tweak_dist_small(rng);

            if (a == 0) a = (rng() % 2 == 0) ? 1 : -1; // Ensure a is not zero
            if (c == 0) c = (rng() % 2 == 0) ? -1 : 1; // Ensure c is not zero

            // Ensure discriminant b^2 - 4ac > 0
            if ((a > 0 && c > 0) || (a < 0 && c < 0)) { // a and c have same sign
                c = -c; // Flip sign of c to make -4ac positive
            }

            long long current_disc = static_cast<long long>(b) * b - 4LL * static_cast<long long>(a) * c;
            if (current_disc <= 0) {
                // Make |b| large relative to |ac|
                b = static_cast<long>(std::sqrt(std::abs(4.0 * a * c)) + 5 + (rng()%10));
                if (rng()%2 == 0 && b != 0) b = -b; // Randomize sign of b
                if (b==0) b = (rng()%2 == 0) ? 5 : -5; // Ensure b is not zero if sqrt was 0

                // Re-ensure a and c have opposite signs for safety
                if (a > 0) c = std::abs(c) * -1; else if (a < 0) c = std::abs(c); else a = 1, c = -1;
                if (c == 0) c = (a > 0) ? -1 : 1; // Final check for c

                current_disc = static_cast<long long>(b) * b - 4LL * static_cast<long long>(a) * c;
                if (current_disc <= 0) { // Ultimate fallback
                    a = std::get<0>(baseParams[i % baseParams.size()]);
                    b = std::get<1>(baseParams[i % baseParams.size()]);
                    c = std::get<2>(baseParams[i % baseParams.size()]);
                }
            }
            result.push_back(std::make_tuple(a, b, c));
        }
    }
    return result;
}

// CSV discriminant loading
void qiprng::loadCSVDiscriminants() {
    // Use a static flag to avoid potential race conditions with the mutex
    static std::atomic<bool> load_attempt_in_progress(false);

    // First quick check without lock
    if (g_csv_discriminants_loaded) {
        return;
    }

    // Try to set the in-progress flag
    bool expected = false;
    if (!load_attempt_in_progress.compare_exchange_strong(expected, true)) {
        // Another thread is already loading, just wait
        int timeout_ms = 1000; // 1 second timeout
        for (int i = 0; i < timeout_ms/10; i++) {
            if (g_csv_discriminants_loaded) {
                return; // Successfully loaded by another thread
            }
            std::this_thread::sleep_for(std::chrono::milliseconds(10));
        }
        // If we get here, the other thread might be stuck
        return; // Give up waiting
    }

    // We got the flag, now get the mutex
    std::lock_guard<std::mutex> lock(g_csv_disc_mutex);

    // Double-check after acquiring the lock
    if (g_csv_discriminants_loaded) {
        load_attempt_in_progress.store(false);
        return;
    }

    // Proceed with loading the discriminants
    try {
        std::vector<std::tuple<long, long, long, long long>> temp_discriminants;

        // Try different possible paths for the excellent discriminants CSV file
        std::vector<std::string> possible_paths = {
            "inst/extdata/excellent_discriminants.csv",
            "../inst/extdata/excellent_discriminants.csv",
            "analysis/data/excellent_discriminants.csv",
            "../analysis/data/excellent_discriminants.csv",
            "../../analysis/data/excellent_discriminants.csv",
            "excellent_discriminants.csv",
            "discriminants.csv",
            "../discriminants.csv",
            "../../discriminants.csv"
        };

        std::ifstream file;
        std::string used_path;

        for (const auto& path : possible_paths) {
            file.open(path);
            if (file.is_open()) {
                used_path = path;
                break;
            }
            file.clear(); // Clear any error flags
        }

        if (!file.is_open()) {
            Rcpp::warning("Could not open excellent_discriminants.csv file in any standard location. Will use generated discriminants.");

            // Generate default discriminants instead
            for (int i = 0; i < 100; i++) {
                long a = 1 + (i % 10);
                long b = 3 + 2 * (i % 8);
                long c = -1 - (i % 5);
                long long disc = static_cast<long long>(b) * b - 4LL * static_cast<long long>(a) * c;
                if (disc > 0) {
                    temp_discriminants.emplace_back(a, b, c, disc);
                }
            }

            g_csv_discriminants.swap(temp_discriminants);
            Rcpp::Rcout << "Created " << g_csv_discriminants.size() << " default discriminants." << std::endl;
            g_csv_discriminants_loaded = true;
            load_attempt_in_progress.store(false);
            return;
        }

        std::string header;
        if (!std::getline(file, header)) {
            Rcpp::warning("Error reading CSV header from %s.", used_path.c_str());
            g_csv_discriminants_loaded = true;
            load_attempt_in_progress.store(false);
            return;
        }

        std::string line;
        size_t line_number = 1;
        while (std::getline(file, line)) {
            line_number++;
            if (line.empty() || line[0] == '#') continue;

            std::stringstream ss(line);
            std::string token;
            std::vector<std::string> tokens;
            while (std::getline(ss, token, ',')) {
                tokens.push_back(token);
            }

            if (tokens.size() < 4) {
                Rcpp::warning("Skipping line %d in %s: insufficient values (expected 4, got %d)",
                            (int)line_number, used_path.c_str(), (int)tokens.size());
                continue;
            }

            try {
                long a = std::stol(tokens[0]);
                long b = std::stol(tokens[1]);
                long c = std::stol(tokens[2]);
                long long discriminant = std::stoll(tokens[3]);

                if (a == 0) {
                    Rcpp::warning("Skipping line %d from CSV: 'a' parameter cannot be zero", (int)line_number);
                    continue;
                }
                long long calculated_disc = static_cast<long long>(b) * b - 4LL * static_cast<long long>(a) * c;
                if (discriminant != calculated_disc) {
                    Rcpp::warning("Skipping line %d from CSV: provided discriminant %lld does not match calculated %lld for a=%ld, b=%ld, c=%ld",
                                (int)line_number, discriminant, calculated_disc, a,b,c);
                    continue;
                }
                if (discriminant <= 0) {
                    Rcpp::warning("Skipping line %d from CSV: discriminant %lld must be positive", (int)line_number, discriminant);
                    continue;
                }
                temp_discriminants.emplace_back(a, b, c, discriminant);
            } catch (const std::invalid_argument& e) {
                Rcpp::warning("Skipping line %d from CSV: invalid numeric format (%s)", (int)line_number, e.what());
            } catch (const std::out_of_range& e) {
                Rcpp::warning("Skipping line %d from CSV: number out of range (%s)", (int)line_number, e.what());
            }
        }

        if (!temp_discriminants.empty()) {
            g_csv_discriminants.swap(temp_discriminants);
            Rcpp::Rcout << "Loaded " << g_csv_discriminants.size() << " discriminants from CSV file." << std::endl;
        } else {
            Rcpp::warning("No valid discriminants found in CSV file or file was empty. Generating default discriminants.");

            // Generate default discriminants instead
            for (int i = 0; i < 100; i++) {
                long a = 1 + (i % 10);
                long b = 3 + 2 * (i % 8);
                long c = -1 - (i % 5);
                long long disc = static_cast<long long>(b) * b - 4LL * static_cast<long long>(a) * c;
                if (disc > 0) {
                    g_csv_discriminants.emplace_back(a, b, c, disc);
                }
            }

            if (!g_csv_discriminants.empty()) {
                Rcpp::Rcout << "Generated " << g_csv_discriminants.size() << " default discriminants." << std::endl;
            } else {
                Rcpp::warning("Failed to generate default discriminants. Thread safety issues may occur.");
            }
        }

        // Always mark as loaded, even if we failed, to avoid repeated attempts
        g_csv_discriminants_loaded = true;

        // Reset the in-progress flag
        load_attempt_in_progress.store(false);

    } catch (const std::exception& e) {
        Rcpp::warning("Exception during CSV discriminants loading: %s", e.what());
        g_csv_discriminants_loaded = true;
        load_attempt_in_progress.store(false);
    } catch (...) {
        Rcpp::warning("Unknown exception during CSV discriminants loading");
        g_csv_discriminants_loaded = true;
        load_attempt_in_progress.store(false);
    }
}

// Discriminant selection
long long qiprng::chooseUniqueDiscriminant(long min_value, long max_value) {
    initialize_libsodium_if_needed(); // Ensure libsodium is ready if used by PRNG for config

    PRNGConfig current_config; // Default config
    bool use_csv = PRNGDefaults::use_csv_discriminants; // Default

    // Safely try to get current PRNG's config - thread-safe access
    if (g_use_threading) {
        if (t_prng) {
            current_config = t_prng->getConfig(); // Thread-local access is safe
        }
    } else {
        std::lock_guard<std::mutex> lock(g_prng_mutex);
        if (g_prng) {
            current_config = g_prng->getConfig();
        }
    }
    use_csv = current_config.use_csv_discriminants;

    // Get appropriate RNG based on whether we have a seed
    std::mt19937_64& rng = current_config.has_seed
        ? getDeterministicThreadLocalEngine(current_config.seed)
        : getThreadLocalEngine();

    // First approach: Use CSV discriminants if configured
    if (use_csv) {
        // Make sure CSV discriminants are loaded - this has its own thread safety
        loadCSVDiscriminants();

        // Create a local copy of CSV discriminants to work with outside the lock
        std::vector<std::tuple<long,long,long,long long>> local_csv_copy;
        {
            std::lock_guard<std::mutex> csv_lock(g_csv_disc_mutex);
            if(!g_csv_discriminants.empty()) {
                local_csv_copy = g_csv_discriminants;
            }
        }

        if (!local_csv_copy.empty()) {
            if (current_config.has_seed) {
                // Use seeded shuffle for deterministic but randomized selection
                std::mt19937_64 shuffle_rng(current_config.seed);
                std::shuffle(local_csv_copy.begin(), local_csv_copy.end(), shuffle_rng);
            } else {
                // Original random shuffle
                std::shuffle(local_csv_copy.begin(), local_csv_copy.end(), rng);
            }

            for (const auto& entry : local_csv_copy) {
                long long disc_candidate = std::get<3>(entry);
                long csv_a = std::get<0>(entry);
                long csv_b = std::get<1>(entry);
                long csv_c = std::get<2>(entry);

                // Critical section: Check and update discriminant usage
                bool is_new_discriminant = false;
                {
                    std::lock_guard<std::mutex> disc_lock(g_disc_mutex);
                    if (g_used_discriminants.find(disc_candidate) == g_used_discriminants.end()) {
                        g_used_discriminants.insert(disc_candidate);
                        is_new_discriminant = true;
                    }
                }

                // If we found a new discriminant, update config and return it
                if (is_new_discriminant) {
                    // Update the PRNG's a,b,c with the ones from CSV
                    if (g_use_threading && t_prng) {
                        PRNGConfig cfg = t_prng->getConfig();
                        cfg.a = csv_a; cfg.b = csv_b; cfg.c = csv_c;
                        t_prng->updateConfig(cfg);
                    } else if (!g_use_threading && g_prng) {
                        std::lock_guard<std::mutex> prng_lock(g_prng_mutex);
                        PRNGConfig cfg = g_prng->getConfig();
                        cfg.a = csv_a; cfg.b = csv_b; cfg.c = csv_c;
                        g_prng->updateConfig(cfg);
                    }
                    return disc_candidate;
                }
            }

            // If we get here, all CSV discriminants are used
            if (current_config.debug) {
                Rcpp::warning("All unique discriminants from CSV have been used. Falling back to random generation.");
            }
        } else if (g_csv_discriminants_loaded) {
            // CSV was loaded but was empty or all entries were invalid
            if (current_config.debug) {
                Rcpp::warning("CSV discriminants were requested but list is empty or failed to load. Falling back to random generation.");
            }
        }
    }

    // Second approach: Generate random discriminants
    std::uniform_int_distribution<long long> dist(min_value, max_value);
    int max_attempts = 1000;

    for (int attempt = 0; attempt < max_attempts; ++attempt) {
        long long candidate = dist(rng);
        if (candidate <= 0) continue;

        // Basic square-free check
        bool is_sf = true;
        long limit = static_cast<long>(std::sqrt(static_cast<double>(candidate)));
        if (limit > 1000) limit = 1000; // Cap for performance

        for (long f = 2; f <= limit; ++f) {
            if (candidate % (f * f) == 0) {
                is_sf = false;
                break;
            }
        }

        if (!is_sf) continue;

        // Critical section: Check and update discriminant usage
        bool is_new_discriminant = false;
        {
            std::lock_guard<std::mutex> lock(g_disc_mutex);
            if (g_used_discriminants.find(candidate) == g_used_discriminants.end()) {
                g_used_discriminants.insert(candidate);
                is_new_discriminant = true;
            }
        }

        if (is_new_discriminant) {
            // Calculate a, b, c values for this discriminant
            try {
                auto abc = makeABCfromDelta(candidate);
                long a = std::get<0>(abc);
                long b = std::get<1>(abc);
                long c = std::get<2>(abc);

                // Update the PRNG with these values
                if (g_use_threading && t_prng) {
                    PRNGConfig cfg = t_prng->getConfig();
                    cfg.a = a; cfg.b = b; cfg.c = c;
                    t_prng->updateConfig(cfg);
                } else if (!g_use_threading && g_prng) {
                    std::lock_guard<std::mutex> prng_lock(g_prng_mutex);
                    PRNGConfig cfg = g_prng->getConfig();
                    cfg.a = a; cfg.b = b; cfg.c = c;
                    g_prng->updateConfig(cfg);
                }

                return candidate;
            } catch (const std::exception& e) {
                // Log error but continue looking
                if (current_config.debug) {
                    Rcpp::warning("Failed to compute a,b,c for discriminant %lld: %s", candidate, e.what());
                }
                continue;
            }
        }
    }

    // Fallback if we couldn't find a good discriminant
    long long fallback_disc = 41; // Default value (from 2,5,-2)
    try {
        auto abc = makeABCfromDelta(fallback_disc);
        long a = std::get<0>(abc);
        long b = std::get<1>(abc);
        long c = std::get<2>(abc);

        // Update the PRNG with these values
        if (g_use_threading && t_prng) {
            PRNGConfig cfg = t_prng->getConfig();
            cfg.a = a; cfg.b = b; cfg.c = c;
            t_prng->updateConfig(cfg);
        } else if (!g_use_threading && g_prng) {
            std::lock_guard<std::mutex> prng_lock(g_prng_mutex);
            PRNGConfig cfg = g_prng->getConfig();
            cfg.a = a; cfg.b = b; cfg.c = c;
            g_prng->updateConfig(cfg);
        }
    } catch (...) {
        // Ultimate fallback
        if (g_use_threading && t_prng) {
            PRNGConfig cfg = t_prng->getConfig();
            cfg.a = 2; cfg.b = 5; cfg.c = -2;
            t_prng->updateConfig(cfg);
        } else if (!g_use_threading && g_prng) {
            std::lock_guard<std::mutex> prng_lock(g_prng_mutex);
            PRNGConfig cfg = g_prng->getConfig();
            cfg.a = 2; cfg.b = 5; cfg.c = -2;
            g_prng->updateConfig(cfg);
        }
    }

    return fallback_disc;
}

// Generate a,b,c values from discriminant
std::tuple<long, long, long> qiprng::makeABCfromDelta(long long Delta) {
    // Return a default fallback for non-positive Delta
    if (Delta <= 0) {
        return {1, 5, -1}; // Safe default fallback
    }

    const int MAX_TRIES = 50; // Reduced attempts for better performance
    int total_tries = 0;

    try {
        // Use a thread-local PRNG for reproducibility within a thread but diversity between threads
        std::mt19937_64& rng = getThreadLocalEngine();

        // Choose a random a value with a preference for small absolute values
        std::vector<long> a_choices = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 15};
        // Randomize sign 50% of the time
        for (size_t i = 0; i < a_choices.size(); ++i) {
            if (rng() % 2 == 0) {
                a_choices[i] = -a_choices[i];
            }
        }

        // Prefer small values for performance
        std::shuffle(a_choices.begin(), a_choices.end(), rng);
        std::sort(a_choices.begin(), a_choices.end(), [](long a, long b) {
            return std::abs(a) < std::abs(b);
        });

        // Try each a value to find a suitable b and c
        for (long a_val : a_choices) {
            if (total_tries >= MAX_TRIES) {
                break; // Prevent runaway loops
            }
            if (a_val == 0) continue; // Skip a=0

            // More efficient algorithm using mathematical properties
            // Calculate b using quadratic residues approach
            long b_val = static_cast<long>(std::sqrt(static_cast<double>(Delta)));
            if (Delta > 4) {
                // Find nearest square above Delta
                long long nearest_square = static_cast<long long>(b_val + 1);
                nearest_square = nearest_square * nearest_square;
                if (nearest_square > Delta) {
                    b_val = static_cast<long>(std::sqrt(static_cast<double>(nearest_square)));
                }
            }

            // Try a small range around the calculated b value
            for (long b_offset = 0; b_offset < 10 && total_tries < MAX_TRIES; ++b_offset, ++total_tries) {
                long b_try = (b_offset % 2 == 0) ? b_val + b_offset/2 : b_val - (b_offset+1)/2;

                // Skip b=0 unless appropriate condition is met
                if (b_try == 0 && Delta % (4LL * a_val) != 0) continue;

                // Use safe computation for b^2 to avoid overflow
                // Check if b^2 would overflow a long long
                long long b_ll = static_cast<long long>(b_try);
                if (b_ll > 0 && b_ll > std::numeric_limits<long long>::max() / b_ll) {
                    continue; // Would overflow, skip this value
                }
                if (b_ll < 0 && b_ll < std::numeric_limits<long long>::min() / b_ll) {
                    continue; // Would overflow, skip this value
                }

                long long b_squared = b_ll * b_ll;

                // Compute numerator and denominator for c
                long long num = b_squared - Delta;
                long long den = 4LL * a_val;

                // Skip invalid cases
                if (den == 0) continue;  // Should not happen with a_choices

                // Check if we can get an integer c
                if (num % den == 0) {
                    long long c_ll = num / den;

                    // Check if c fits in a long
                    if (c_ll >= std::numeric_limits<long>::min() &&
                        c_ll <= std::numeric_limits<long>::max()) {

                        long c_val = static_cast<long>(c_ll);

                        // Skip c=0 unless appropriate condition is met
                        if (c_val == 0 && b_squared != Delta) continue;

                        // Verify the result actually gives the correct discriminant
                        // Check for overflow in 4*a*c calculation
                        long long a_ll = static_cast<long long>(a_val);
                        long long c_ll = static_cast<long long>(c_val);

                        // First check 4*a for overflow
                        if (a_ll > 0 && 4LL > std::numeric_limits<long long>::max() / a_ll) {
                            continue; // Would overflow
                        }

                        long long four_a = 4LL * a_ll;

                        // Then check four_a * c for overflow
                        bool will_overflow = false;
                        if (four_a > 0 && c_ll > 0) {
                            if (four_a > std::numeric_limits<long long>::max() / c_ll) {
                                will_overflow = true;
                            }
                        } else if (four_a < 0 && c_ll < 0) {
                            if (four_a < std::numeric_limits<long long>::max() / c_ll) {
                                will_overflow = true;
                            }
                        } else if (four_a > 0 && c_ll < 0) {
                            if (c_ll < std::numeric_limits<long long>::min() / four_a) {
                                will_overflow = true;
                            }
                        } else if (four_a < 0 && c_ll > 0) {
                            if (four_a < std::numeric_limits<long long>::min() / c_ll) {
                                will_overflow = true;
                            }
                        }

                        if (will_overflow) {
                            continue;  // Skip if multiplication would overflow
                        }

                        long long four_ac = four_a * c_ll;
                        long long disc = b_squared - four_ac;

                        if (disc == Delta) {
                            return {a_val, b_val, c_val};
                        }
                    }
                }
            }
        }

        // Fallback if no suitable a,b,c found
        long fallback_a = 1;

        // Calculate a reasonable b value for the fallback
        long fallback_b;
        if (Delta <= 4) {
            fallback_b = static_cast<long>(std::sqrt(static_cast<double>(Delta + 100)));
        } else {
            fallback_b = static_cast<long>(std::sqrt(static_cast<double>(Delta)));
        }

        // Choose c to satisfy b^2 - 4ac = Delta
        long long b_squared = static_cast<long long>(fallback_b) * fallback_b;
        long fallback_c = static_cast<long>((b_squared - Delta) / (4 * fallback_a));

        // Verify result
        long long test_disc = static_cast<long long>(fallback_b) * fallback_b -
                              4LL * static_cast<long long>(fallback_a) * fallback_c;

        if (test_disc == Delta) {
            return {fallback_a, fallback_b, fallback_c};
        }

        // Ultimate fallback with nice round numbers
        return {1, 5, -1}; // This gives 41
    } catch (...) {
        // Even safer fallback in case of any errors
        return {1, 5, -1};
    }
}
```

```

---

## File: /Users/biostochastics/Development/GitHub/qiprng/src/rcpp_exports.cpp

### Summary
No declarations found

**Tags**: `cpp`

```cpp
## File: /Users/biostochastics/Development/GitHub/qiprng/src/rcpp_exports.cpp
```cpp
// File: rcpp_exports.cpp
// --------------------------------------------------------------
#include <Rcpp.h>
#include "enhanced_prng.hpp"
#include "prng_utils.hpp"
#include "ziggurat_normal.hpp"

using namespace Rcpp;
using namespace qiprng;

// [[Rcpp::export(".initialize_libsodium_")]]
void initialize_libsodium_() {
    static bool already_initialized = false;
    if (already_initialized) {
        return;
    }

    int ret = sodium_init();
    if (ret < 0) {
        // sodium_init() returns -1 on error, 0 on success, 1 if already initialized.
        throw std::runtime_error("Failed to initialize libsodium. The library may be unusable or insecure.");
    }

    // Set the global flag
    qiprng::sodium_initialized = true;
    already_initialized = true;
    qiprng::sodium_initialized_flag.store(true);

    Rcpp::Rcout << "Libsodium initialized successfully. Return code: " << ret << std::endl;
}

// Helper function to validate PRNGConfig parameters
void validatePRNGConfig(const PRNGConfig& cfg) {
    // Validate discriminant (b^2 - 4ac > 0)
    long long disc = static_cast<long long>(cfg.b) * cfg.b -
                     4LL * static_cast<long long>(cfg.a) * cfg.c;
    if (disc <= 0) {
        throw std::invalid_argument("Invalid config: discriminant must be positive (b^2 - 4ac > 0)");
    }

    // Validate a is not zero
    if (cfg.a == 0) {
        throw std::invalid_argument("Invalid config: parameter 'a' cannot be zero");
    }

    // Validate buffer size
    if (cfg.buffer_size == 0) {
        throw std::invalid_argument("Invalid config: buffer_size must be greater than 0");
    }

    // Validate range parameters
    if (cfg.range_min >= cfg.range_max) {
        throw std::invalid_argument("Invalid config: range_min must be less than range_max");
    }

    // Validate distribution-specific parameters
    if (cfg.distribution == PRNGConfig::NORMAL && cfg.normal_sd <= 0) {
        throw std::invalid_argument("Invalid config: normal_sd must be positive");
    }

    if (cfg.distribution == PRNGConfig::EXPONENTIAL && cfg.exponential_lambda <= 0) {
        throw std::invalid_argument("Invalid config: exponential_lambda must be positive");
    }

    if (cfg.distribution == PRNGConfig::POISSON && cfg.poisson_lambda <= 0) {
        throw std::invalid_argument("Invalid config: poisson_lambda must be positive");
    }

    if (cfg.distribution == PRNGConfig::GAMMA) {
        if (cfg.gamma_shape <= 0) {
            throw std::invalid_argument("Invalid config: gamma_shape must be positive");
        }
        if (cfg.gamma_scale <= 0) {
            throw std::invalid_argument("Invalid config: gamma_scale must be positive");
        }
    }

    if (cfg.distribution == PRNGConfig::BETA) {
        if (cfg.beta_alpha <= 0) {
            throw std::invalid_argument("Invalid config: beta_alpha must be positive");
        }
        if (cfg.beta_beta <= 0) {
            throw std::invalid_argument("Invalid config: beta_beta must be positive");
        }
    }

    // Validate MPFR precision
    if (cfg.mpfr_precision < MPFR_PREC_MIN || cfg.mpfr_precision > MPFR_PREC_MAX) {
        throw std::invalid_argument("Invalid config: mpfr_precision out of valid range");
    }
}

// Helper to parse Rcpp::List to PRNGConfig
PRNGConfig parsePRNGConfig(Rcpp::List rcfg) {
    PRNGConfig cfg;

    // Parse configuration from R list with type checking and defaults
    if (rcfg.containsElementNamed("a")) {
        SEXP a_sexp = rcfg["a"];
        if (TYPEOF(a_sexp) == INTSXP || TYPEOF(a_sexp) == REALSXP) {
            cfg.a = Rcpp::as<long>(a_sexp);
        }
    }

    if (rcfg.containsElementNamed("b")) {
        SEXP b_sexp = rcfg["b"];
        if (TYPEOF(b_sexp) == INTSXP || TYPEOF(b_sexp) == REALSXP) {
            cfg.b = Rcpp::as<long>(b_sexp);
        }
    }

    if (rcfg.containsElementNamed("c")) {
        SEXP c_sexp = rcfg["c"];
        if (TYPEOF(c_sexp) == INTSXP || TYPEOF(c_sexp) == REALSXP) {
            cfg.c = Rcpp::as<long>(c_sexp);
        }
    }

    if (rcfg.containsElementNamed("mpfr_precision")) {
        SEXP prec_sexp = rcfg["mpfr_precision"];
        if (TYPEOF(prec_sexp) == INTSXP || TYPEOF(prec_sexp) == REALSXP) {
            cfg.mpfr_precision = Rcpp::as<unsigned int>(prec_sexp);
        }
    }

    if (rcfg.containsElementNamed("buffer_size")) {
        SEXP buf_sexp = rcfg["buffer_size"];
        if (TYPEOF(buf_sexp) == INTSXP || TYPEOF(buf_sexp) == REALSXP) {
            cfg.buffer_size = Rcpp::as<size_t>(buf_sexp);
        }
    }

    if (rcfg.containsElementNamed("distribution")) {
        SEXP dist_sexp = rcfg["distribution"];
        if (TYPEOF(dist_sexp) == INTSXP) {
            int dist_int = Rcpp::as<int>(dist_sexp);
            if (dist_int >= 0 && dist_int <= 13) { // Check valid distribution enum range (14 distributions)
                cfg.distribution = static_cast<PRNGConfig::Distribution>(dist_int);
            }
        } else if (TYPEOF(dist_sexp) == STRSXP) {
            std::string dist_str = Rcpp::as<std::string>(dist_sexp);
            if (dist_str == "uniform_01") cfg.distribution = PRNGConfig::UNIFORM_01;
            else if (dist_str == "uniform_range") cfg.distribution = PRNGConfig::UNIFORM_RANGE;
            else if (dist_str == "normal") cfg.distribution = PRNGConfig::NORMAL;
            else if (dist_str == "exponential") cfg.distribution = PRNGConfig::EXPONENTIAL;
            else if (dist_str == "poisson") cfg.distribution = PRNGConfig::POISSON;
            else if (dist_str == "gamma") cfg.distribution = PRNGConfig::GAMMA;
            else if (dist_str == "beta") cfg.distribution = PRNGConfig::BETA;
            else if (dist_str == "bernoulli") cfg.distribution = PRNGConfig::BERNOULLI;
            else if (dist_str == "binomial") cfg.distribution = PRNGConfig::BINOMIAL;
            else if (dist_str == "lognormal") cfg.distribution = PRNGConfig::LOGNORMAL;
            else if (dist_str == "weibull") cfg.distribution = PRNGConfig::WEIBULL;
            else if (dist_str == "chisquared") cfg.distribution = PRNGConfig::CHISQUARED;
            else if (dist_str == "student_t") cfg.distribution = PRNGConfig::STUDENT_T;
            else if (dist_str == "negative_binomial") cfg.distribution = PRNGConfig::NEGATIVE_BINOMIAL;
        }
    }

    if (rcfg.containsElementNamed("normal_method")) {
        SEXP method_sexp = rcfg["normal_method"];
        if (TYPEOF(method_sexp) == INTSXP) {
            int method_int = Rcpp::as<int>(method_sexp);
            if (method_int >= 0 && method_int <= 1) { // Check valid method enum range
                cfg.normal_method = static_cast<PRNGConfig::NormalMethod>(method_int);
            }
        } else if (TYPEOF(method_sexp) == STRSXP) {
            std::string method_str = Rcpp::as<std::string>(method_sexp);
            if (method_str == "box_muller") cfg.normal_method = PRNGConfig::BOX_MULLER;
            else if (method_str == "ziggurat") cfg.normal_method = PRNGConfig::ZIGGURAT;
        }
    }

    if (rcfg.containsElementNamed("range_min")) {
        SEXP min_sexp = rcfg["range_min"];
        if (TYPEOF(min_sexp) == REALSXP) {
            cfg.range_min = Rcpp::as<double>(min_sexp);
        }
    }

    if (rcfg.containsElementNamed("range_max")) {
        SEXP max_sexp = rcfg["range_max"];
        if (TYPEOF(max_sexp) == REALSXP) {
            cfg.range_max = Rcpp::as<double>(max_sexp);
        }
    }

    if (rcfg.containsElementNamed("normal_mean")) {
        SEXP mean_sexp = rcfg["normal_mean"];
        if (TYPEOF(mean_sexp) == REALSXP) {
            cfg.normal_mean = Rcpp::as<double>(mean_sexp);
        }
    }

    if (rcfg.containsElementNamed("normal_sd")) {
        SEXP sd_sexp = rcfg["normal_sd"];
        if (TYPEOF(sd_sexp) == REALSXP) {
            cfg.normal_sd = Rcpp::as<double>(sd_sexp);
        }
    }

    if (rcfg.containsElementNamed("exponential_lambda")) {
        SEXP lambda_sexp = rcfg["exponential_lambda"];
        if (TYPEOF(lambda_sexp) == REALSXP) {
            cfg.exponential_lambda = Rcpp::as<double>(lambda_sexp);
        }
    }

    // New distribution parameters
    if (rcfg.containsElementNamed("poisson_lambda")) {
        SEXP lambda_sexp = rcfg["poisson_lambda"];
        if (TYPEOF(lambda_sexp) == REALSXP) {
            cfg.poisson_lambda = Rcpp::as<double>(lambda_sexp);
        }
    }

    if (rcfg.containsElementNamed("gamma_shape")) {
        SEXP shape_sexp = rcfg["gamma_shape"];
        if (TYPEOF(shape_sexp) == REALSXP) {
            cfg.gamma_shape = Rcpp::as<double>(shape_sexp);
        }
    }

    if (rcfg.containsElementNamed("gamma_scale")) {
        SEXP scale_sexp = rcfg["gamma_scale"];
        if (TYPEOF(scale_sexp) == REALSXP) {
            cfg.gamma_scale = Rcpp::as<double>(scale_sexp);
        }
    }

    if (rcfg.containsElementNamed("beta_alpha")) {
        SEXP alpha_sexp = rcfg["beta_alpha"];
        if (TYPEOF(alpha_sexp) == REALSXP) {
            cfg.beta_alpha = Rcpp::as<double>(alpha_sexp);
        }
    }

    if (rcfg.containsElementNamed("beta_beta")) {
        SEXP beta_sexp = rcfg["beta_beta"];
        if (TYPEOF(beta_sexp) == REALSXP) {
            cfg.beta_beta = Rcpp::as<double>(beta_sexp);
        }
    }

    // New distribution parameters
    if (rcfg.containsElementNamed("bernoulli_p")) {
        SEXP p_sexp = rcfg["bernoulli_p"];
        if (TYPEOF(p_sexp) == REALSXP) {
            cfg.bernoulli_p = Rcpp::as<double>(p_sexp);
        }
    }

    if (rcfg.containsElementNamed("binomial_n")) {
        SEXP n_sexp = rcfg["binomial_n"];
        if (TYPEOF(n_sexp) == INTSXP || TYPEOF(n_sexp) == REALSXP) {
            cfg.binomial_n = Rcpp::as<int>(n_sexp);
        }
    }

    if (rcfg.containsElementNamed("binomial_p")) {
        SEXP p_sexp = rcfg["binomial_p"];
        if (TYPEOF(p_sexp) == REALSXP) {
            cfg.binomial_p = Rcpp::as<double>(p_sexp);
        }
    }

    if (rcfg.containsElementNamed("lognormal_mu")) {
        SEXP mu_sexp = rcfg["lognormal_mu"];
        if (TYPEOF(mu_sexp) == REALSXP) {
            cfg.lognormal_mu = Rcpp::as<double>(mu_sexp);
        }
    }

    if (rcfg.containsElementNamed("lognormal_sigma")) {
        SEXP sigma_sexp = rcfg["lognormal_sigma"];
        if (TYPEOF(sigma_sexp) == REALSXP) {
            cfg.lognormal_sigma = Rcpp::as<double>(sigma_sexp);
        }
    }

    if (rcfg.containsElementNamed("weibull_shape")) {
        SEXP shape_sexp = rcfg["weibull_shape"];
        if (TYPEOF(shape_sexp) == REALSXP) {
            cfg.weibull_shape = Rcpp::as<double>(shape_sexp);
        }
    }

    if (rcfg.containsElementNamed("weibull_scale")) {
        SEXP scale_sexp = rcfg["weibull_scale"];
        if (TYPEOF(scale_sexp) == REALSXP) {
            cfg.weibull_scale = Rcpp::as<double>(scale_sexp);
        }
    }

    if (rcfg.containsElementNamed("chisquared_df")) {
        SEXP df_sexp = rcfg["chisquared_df"];
        if (TYPEOF(df_sexp) == INTSXP || TYPEOF(df_sexp) == REALSXP) {
            cfg.chisquared_df = Rcpp::as<int>(df_sexp);
        }
    }

    if (rcfg.containsElementNamed("student_t_df")) {
        SEXP df_sexp = rcfg["student_t_df"];
        if (TYPEOF(df_sexp) == INTSXP || TYPEOF(df_sexp) == REALSXP) {
            cfg.student_t_df = Rcpp::as<int>(df_sexp);
        }
    }

    if (rcfg.containsElementNamed("negative_binomial_r")) {
        SEXP r_sexp = rcfg["negative_binomial_r"];
        if (TYPEOF(r_sexp) == INTSXP || TYPEOF(r_sexp) == REALSXP) {
            cfg.negative_binomial_r = Rcpp::as<int>(r_sexp);
        }
    }

    if (rcfg.containsElementNamed("negative_binomial_p")) {
        SEXP p_sexp = rcfg["negative_binomial_p"];
        if (TYPEOF(p_sexp) == REALSXP) {
            cfg.negative_binomial_p = Rcpp::as<double>(p_sexp);
        }
    }

    // Advanced options
    if (rcfg.containsElementNamed("use_crypto_mixing")) {
        SEXP crypto_sexp = rcfg["use_crypto_mixing"];
        if (TYPEOF(crypto_sexp) == LGLSXP) {
            cfg.use_crypto_mixing = Rcpp::as<bool>(crypto_sexp);
        }
    }

    if (rcfg.containsElementNamed("adhoc_corrections")) {
        SEXP adhoc_sexp = rcfg["adhoc_corrections"];
        if (TYPEOF(adhoc_sexp) == LGLSXP) {
            cfg.adhoc_corrections = Rcpp::as<bool>(adhoc_sexp);
        }
    }

    if (rcfg.containsElementNamed("use_tie_breaking")) {
        SEXP tie_sexp = rcfg["use_tie_breaking"];
        if (TYPEOF(tie_sexp) == LGLSXP) {
            cfg.use_tie_breaking = Rcpp::as<bool>(tie_sexp);
        }
    }

    if (rcfg.containsElementNamed("reseed_interval")) {
        SEXP interval_sexp = rcfg["reseed_interval"];
        if (TYPEOF(interval_sexp) == INTSXP || TYPEOF(interval_sexp) == REALSXP) {
            cfg.reseed_interval = Rcpp::as<unsigned long>(interval_sexp);
        }
    }

    if (rcfg.containsElementNamed("use_csv_discriminants")) {
        SEXP csv_sexp = rcfg["use_csv_discriminants"];
        if (TYPEOF(csv_sexp) == LGLSXP) {
            cfg.use_csv_discriminants = Rcpp::as<bool>(csv_sexp);
        }
    }

    if (rcfg.containsElementNamed("use_parallel_filling")) {
        SEXP parallel_sexp = rcfg["use_parallel_filling"];
        if (TYPEOF(parallel_sexp) == LGLSXP) {
            cfg.use_parallel_filling = Rcpp::as<bool>(parallel_sexp);
        }
    }

    if (rcfg.containsElementNamed("use_threading")) {
        SEXP threading_sexp = rcfg["use_threading"];
        if (TYPEOF(threading_sexp) == LGLSXP) {
            cfg.use_threading = Rcpp::as<bool>(threading_sexp);
        }
    }

    if (rcfg.containsElementNamed("offset")) {
        SEXP offset_sexp = rcfg["offset"];
        if (TYPEOF(offset_sexp) == INTSXP || TYPEOF(offset_sexp) == REALSXP) {
            cfg.offset = Rcpp::as<size_t>(offset_sexp);
        }
    }

    if (rcfg.containsElementNamed("debug")) {
        SEXP debug_sexp = rcfg["debug"];
        if (TYPEOF(debug_sexp) == LGLSXP) {
            cfg.debug = Rcpp::as<bool>(debug_sexp);
        }
    }

    if (rcfg.containsElementNamed("seed")) {
        SEXP seed_sexp = rcfg["seed"];
        if (TYPEOF(seed_sexp) == INTSXP || TYPEOF(seed_sexp) == REALSXP) {
            cfg.seed = Rcpp::as<uint64_t>(seed_sexp);
        }
    }

    if (rcfg.containsElementNamed("has_seed")) {
        SEXP has_seed_sexp = rcfg["has_seed"];
        if (TYPEOF(has_seed_sexp) == LGLSXP) {
            cfg.has_seed = Rcpp::as<bool>(has_seed_sexp);
        }
    }

    if (rcfg.containsElementNamed("deterministic")) {
        SEXP deterministic_sexp = rcfg["deterministic"];
        if (TYPEOF(deterministic_sexp) == LGLSXP) {
            cfg.deterministic = Rcpp::as<bool>(deterministic_sexp);
        }
    }

    // Validate the configuration before returning
    validatePRNGConfig(cfg);

    return cfg;
}

// Helper to convert PRNGConfig to Rcpp::List
Rcpp::List PRNGConfigToList(const PRNGConfig& cfg) {
    Rcpp::List result;

    result["a"] = cfg.a;
    result["b"] = cfg.b;
    result["c"] = cfg.c;
    result["mpfr_precision"] = cfg.mpfr_precision;
    result["buffer_size"] = static_cast<int>(cfg.buffer_size);

    // Convert distribution enum to string for R
    std::string dist_str;
    switch (cfg.distribution) {
        case PRNGConfig::UNIFORM_01: dist_str = "uniform_01"; break;
        case PRNGConfig::UNIFORM_RANGE: dist_str = "uniform_range"; break;
        case PRNGConfig::NORMAL: dist_str = "normal"; break;
        case PRNGConfig::EXPONENTIAL: dist_str = "exponential"; break;
        case PRNGConfig::POISSON: dist_str = "poisson"; break;
        case PRNGConfig::GAMMA: dist_str = "gamma"; break;
        case PRNGConfig::BETA: dist_str = "beta"; break;
        case PRNGConfig::BERNOULLI: dist_str = "bernoulli"; break;
        case PRNGConfig::BINOMIAL: dist_str = "binomial"; break;
        case PRNGConfig::LOGNORMAL: dist_str = "lognormal"; break;
        case PRNGConfig::WEIBULL: dist_str = "weibull"; break;
        case PRNGConfig::CHISQUARED: dist_str = "chisquared"; break;
        case PRNGConfig::STUDENT_T: dist_str = "student_t"; break;
        case PRNGConfig::NEGATIVE_BINOMIAL: dist_str = "negative_binomial"; break;
        default: dist_str = "unknown"; break;
    }
    result["distribution"] = dist_str;

    // Convert normal method enum to string for R
    std::string method_str;
    switch (cfg.normal_method) {
        case PRNGConfig::BOX_MULLER: method_str = "box_muller"; break;
        case PRNGConfig::ZIGGURAT: method_str = "ziggurat"; break;
        default: method_str = "unknown"; break;
    }
    result["normal_method"] = method_str;

    result["range_min"] = cfg.range_min;
    result["range_max"] = cfg.range_max;
    result["normal_mean"] = cfg.normal_mean;
    result["normal_sd"] = cfg.normal_sd;
    result["exponential_lambda"] = cfg.exponential_lambda;

    // New distribution parameters
    result["poisson_lambda"] = cfg.poisson_lambda;
    result["gamma_shape"] = cfg.gamma_shape;
    result["gamma_scale"] = cfg.gamma_scale;
    result["beta_alpha"] = cfg.beta_alpha;
    result["beta_beta"] = cfg.beta_beta;

    // Additional new distribution parameters
    result["bernoulli_p"] = cfg.bernoulli_p;
    result["binomial_n"] = cfg.binomial_n;
    result["binomial_p"] = cfg.binomial_p;
    result["lognormal_mu"] = cfg.lognormal_mu;
    result["lognormal_sigma"] = cfg.lognormal_sigma;
    result["weibull_shape"] = cfg.weibull_shape;
    result["weibull_scale"] = cfg.weibull_scale;
    result["chisquared_df"] = cfg.chisquared_df;
    result["student_t_df"] = cfg.student_t_df;
    result["negative_binomial_r"] = cfg.negative_binomial_r;
    result["negative_binomial_p"] = cfg.negative_binomial_p;

    // Advanced settings
    result["use_crypto_mixing"] = cfg.use_crypto_mixing;
    result["adhoc_corrections"] = cfg.adhoc_corrections;
    result["use_tie_breaking"] = cfg.use_tie_breaking;
    result["reseed_interval"] = static_cast<int>(cfg.reseed_interval);
    result["use_csv_discriminants"] = cfg.use_csv_discriminants;
    result["use_parallel_filling"] = cfg.use_parallel_filling;
    result["use_threading"] = cfg.use_threading;
    result["offset"] = static_cast<int>(cfg.offset);
    result["debug"] = cfg.debug;
    result["seed"] = static_cast<double>(cfg.seed);
    result["has_seed"] = cfg.has_seed;
    result["deterministic"] = cfg.deterministic;

    return result;
}

// [[Rcpp::export(".createPRNG_")]]
void createPRNG_(Rcpp::List rcfg) {
    try {
        // First make sure libsodium is initialized
        if (!qiprng::sodium_initialized) {
            initialize_libsodium_();
        }

        PRNGConfig cfg = parsePRNGConfig(rcfg);

    // Validate critical parameters
    if (cfg.a == 0) {
        throw std::invalid_argument("Parameter 'a' cannot be 0 in quadratic irrational generator");
    }

    long long discriminant = static_cast<long long>(cfg.b) * cfg.b - 4LL * static_cast<long long>(cfg.a) * cfg.c;

    if (discriminant <= 0) {
        throw std::invalid_argument("Discriminant (b^2 - 4ac) must be positive");
    }

    // Make sure libsodium is initialized
    if (!qiprng::sodium_initialized) {
        Rcpp::stop("Libsodium not initialized before PRNG creation - this should not happen");
    }

    // Update global threading flag based on config
    g_use_threading = cfg.use_threading;

    if (g_use_threading) {
        // Clean up any existing thread-local PRNG
        t_prng.reset();

        // For thread safety, we choose a slightly different a,b,c for each thread
        long long disc = chooseUniqueDiscriminant();

        // Use std::tie instead of C++17 structured bindings
        std::tuple<long, long, long> abc_vals = makeABCfromDelta(disc);
        long a_val = std::get<0>(abc_vals);
        long b_val = std::get<1>(abc_vals);
        long c_val = std::get<2>(abc_vals);

        // Override with thread-specific a,b,c unless user explicitly provided them
        if (!rcfg.containsElementNamed("a") && !rcfg.containsElementNamed("b") && !rcfg.containsElementNamed("c")) {
            cfg.a = a_val;
            cfg.b = b_val;
            cfg.c = c_val;
        }

        // Select quadratic irrationals based on precision and create thread-local PRNG
        int num_qis = cfg.mpfr_precision < 64 ? 2 : (cfg.mpfr_precision < 128 ? 3 : 5);
        std::vector<std::tuple<long, long, long>> abc_list = pickMultiQiSet(cfg.mpfr_precision, num_qis,
                                                                           cfg.seed, cfg.has_seed);
        t_prng = std::make_unique<EnhancedPRNG>(cfg, abc_list);

        if (cfg.debug) {
            t_prng->dumpConfig();
        }
    } else {
        std::lock_guard<std::mutex> lock(g_prng_mutex);
        // Clean up any existing global PRNG
        g_prng.reset();

        // Select quadratic irrationals based on precision and create global PRNG
        int num_qis = cfg.mpfr_precision < 64 ? 2 : (cfg.mpfr_precision < 128 ? 3 : 5);
        std::vector<std::tuple<long, long, long>> abc_list = pickMultiQiSet(cfg.mpfr_precision, num_qis,
                                                                           cfg.seed, cfg.has_seed);
        g_prng = std::make_unique<EnhancedPRNG>(cfg, abc_list);

        if (cfg.debug) {
            g_prng->dumpConfig();
        }
    }

    } catch (const std::exception& e) {
        // Just rethrow for R error handling
        throw;
    } catch (...) {
        // Just rethrow any unknown exceptions
        throw;
    }
}

// [[Rcpp::export(".updatePRNG_")]]
void updatePRNG_(Rcpp::List rcfg) {
    PRNGConfig cfg = parsePRNGConfig(rcfg);

    // Check if threading mode has changed
    bool was_threading = g_use_threading;
    g_use_threading = cfg.use_threading;

    // If threading mode changed, handle appropriately
    if (was_threading != g_use_threading) {
        // If switching from non-threading to threading
        if (g_use_threading) {
            // Create a new thread-local PRNG based on the global one if possible
            if (g_prng) {
                std::lock_guard<std::mutex> lock(g_prng_mutex);
                // Copy the global PRNG config and update with our specific changes
                PRNGConfig new_cfg = g_prng->getConfig();
                // Apply changes from the provided config
                if (rcfg.containsElementNamed("a")) new_cfg.a = cfg.a;
                if (rcfg.containsElementNamed("b")) new_cfg.b = cfg.b;
                if (rcfg.containsElementNamed("c")) new_cfg.c = cfg.c;
                // Ensure threading is enabled
                new_cfg.use_threading = true;

                // Get the MultiQI set for our PRNG
                int num_qis = new_cfg.mpfr_precision < 64 ? 2 : (new_cfg.mpfr_precision < 128 ? 3 : 5);
                std::vector<std::tuple<long, long, long>> abc_list = pickMultiQiSet(new_cfg.mpfr_precision, num_qis,
                                                                                   new_cfg.seed, new_cfg.has_seed);

                // Create the thread-local PRNG
                t_prng = std::make_unique<EnhancedPRNG>(new_cfg, abc_list);
            }
            // Otherwise the thread-local PRNG will be created on the next call to createPRNG_
        }
        // If switching from threading to non-threading
        else {
            // Create a new global PRNG based on the thread-local one if possible
            if (t_prng) {
                // Copy the thread-local PRNG config and update with our specific changes
                PRNGConfig new_cfg = t_prng->getConfig();
                // Apply changes from the provided config
                if (rcfg.containsElementNamed("a")) new_cfg.a = cfg.a;
                if (rcfg.containsElementNamed("b")) new_cfg.b = cfg.b;
                if (rcfg.containsElementNamed("c")) new_cfg.c = cfg.c;
                // Ensure threading is disabled
                new_cfg.use_threading = false;

                // Get the MultiQI set for our PRNG
                int num_qis = new_cfg.mpfr_precision < 64 ? 2 : (new_cfg.mpfr_precision < 128 ? 3 : 5);
                std::vector<std::tuple<long, long, long>> abc_list = pickMultiQiSet(new_cfg.mpfr_precision, num_qis,
                                                                                   new_cfg.seed, new_cfg.has_seed);

                // Create the global PRNG
                std::lock_guard<std::mutex> lock(g_prng_mutex);
                g_prng = std::make_unique<EnhancedPRNG>(new_cfg, abc_list);
            }
            // Otherwise the global PRNG will be created on the next call to createPRNG_
        }
    }

    // Update the appropriate PRNG with the new config
    if (g_use_threading) {
        if (!t_prng) {
            throw std::runtime_error("Cannot update PRNG: No active PRNG in current thread");
        }
        t_prng->updateConfig(cfg);
    } else {
        std::lock_guard<std::mutex> lock(g_prng_mutex);
        if (!g_prng) {
            throw std::runtime_error("Cannot update PRNG: No active global PRNG");
        }
        g_prng->updateConfig(cfg);
    }
}

// [[Rcpp::export(".getPRNGConfig_")]]
Rcpp::List getPRNGConfig_() {
    if (g_use_threading) {
        if (!t_prng) {
            throw std::runtime_error("Cannot get config: No active PRNG in current thread");
        }
        const PRNGConfig& cfg = t_prng->getConfig();
        return PRNGConfigToList(cfg);
    } else {
        std::lock_guard<std::mutex> lock(g_prng_mutex);
        if (!g_prng) {
            throw std::runtime_error("Cannot get config: No active global PRNG");
        }
        const PRNGConfig& cfg = g_prng->getConfig();
        return PRNGConfigToList(cfg);
    }
}

// [[Rcpp::export(".dumpPRNGConfig_")]]
void dumpPRNGConfig_() {
    if (g_use_threading) {
        if (!t_prng) {
            throw std::runtime_error("Cannot dump config: No active PRNG in current thread");
        }
        t_prng->dumpConfig();
    } else {
        std::lock_guard<std::mutex> lock(g_prng_mutex);
        if (!g_prng) {
            throw std::runtime_error("Cannot dump config: No active global PRNG");
        }
        g_prng->dumpConfig();
    }
}

// [[Rcpp::export(".generatePRNG_")]]
Rcpp::NumericVector generatePRNG_(int n) {
    if (n <= 0) {
        return Rcpp::NumericVector(0);
    }

    Rcpp::NumericVector result(n);

    if (g_use_threading) {
        if (!t_prng) {
            throw std::runtime_error("Cannot generate: No active PRNG in current thread");
        }
        t_prng->generate_n(result);
    } else {
        std::lock_guard<std::mutex> lock(g_prng_mutex);
        if (!g_prng) {
            throw std::runtime_error("Cannot generate: No active global PRNG");
        }
        g_prng->generate_n(result);
    }

    return result;
}

// [[Rcpp::export(".reseedPRNG_")]]
void reseedPRNG_() {
    if (g_use_threading) {
        if (!t_prng) {
            throw std::runtime_error("Cannot reseed: No active PRNG in current thread");
        }
        t_prng->reseed();
    } else {
        std::lock_guard<std::mutex> lock(g_prng_mutex);
        if (!g_prng) {
            throw std::runtime_error("Cannot reseed: No active global PRNG");
        }
        g_prng->reseed();
    }
}

// Flag to prevent multiple cleanups
static std::atomic<bool> cleanup_in_progress(false);

// [[Rcpp::export(".cleanup_prng_")]]
void cleanup_prng_() {
    // Safeguard against recursive or reentrant calls using compare_exchange
    // This is more robust than a simple exchange
    bool expected = false;
    if (!cleanup_in_progress.compare_exchange_strong(expected, true, std::memory_order_acq_rel)) {
        // Cleanup already in progress, just return
        Rcpp::Rcout << "Cleanup already in progress, skipping duplicate call" << std::endl;
        return;
    }

    try {
        // Mark any thread-local ZigguratTLSManager instances as exiting
        try {
            ZigguratTLSManager::mark_thread_exiting();
        } catch (...) {
            // Ignore any errors - this is just a best effort
        }

        // First, prepare ZigguratNormal for shutdown (before we destroy any PRNGs)
        // This prevents segfaults from trying to access thread-local resources
        ZigguratNormal::prepare_for_shutdown();

        // Call the EnhancedPRNG cleanup method to handle any thread-local resources
        EnhancedPRNG::cleanupAllThreadResources();

        // Disable threading mode
        bool was_threading = g_use_threading;
        g_use_threading = false;

        try {
            // Clear the PRNGs safely with multiple safeguards
            if (was_threading) {
                if (t_prng) {
                    // Disable thread-safe mode first to prevent new resource access
                    t_prng->prepareForCleanup();
                    t_prng = nullptr;  // Use nullptr instead of reset() to avoid potential issues
                }
            } else {
                try {
                    std::lock_guard<std::mutex> lock(g_prng_mutex);
                    if (g_prng) {
                        // Disable thread-safe mode first to prevent new resource access
                        g_prng->prepareForCleanup();
                        g_prng = nullptr;  // Use nullptr instead of reset() to avoid potential issues
                    }
                } catch (...) {
                    // If mutex locking fails, try a more aggressive approach
                    g_prng = nullptr;
                }
            }
        } catch (...) {
            // Suppress any exceptions during PRNG cleanup
            Rcpp::Rcout << "Exception during PRNG pointer cleanup - suppressed" << std::endl;
        }

        // Clean up ZigguratNormal resources one more time
        try {
            ZigguratNormal::cleanup_thread_local_resources();
        } catch (...) {
            // Suppress any exceptions during ziggurat cleanup
            Rcpp::Rcout << "Exception during ziggurat cleanup - suppressed" << std::endl;
        }

        // Reset flag after successful cleanup
        cleanup_in_progress.store(false, std::memory_order_release);
        Rcpp::Rcout << "PRNG cleanup completed successfully" << std::endl;
    } catch (...) {
        // Always suppress exceptions in cleanup functions
        cleanup_in_progress.store(false, std::memory_order_release);  // Reset flag even on error
        Rcpp::warning("Exception occurred during PRNG cleanup");
    }
}

// [[Rcpp::export(".skipPRNG_")]]
void skipPRNG_(int n) {
    if (n <= 0) return;

    if (g_use_threading) {
        if (!t_prng) {
            throw std::runtime_error("Cannot skip: No active PRNG in current thread");
        }
        t_prng->skip(static_cast<uint64_t>(n));
    } else {
        std::lock_guard<std::mutex> lock(g_prng_mutex);
        if (!g_prng) {
            throw std::runtime_error("Cannot skip: No active global PRNG");
        }
        g_prng->skip(static_cast<uint64_t>(n));
    }
}

// [[Rcpp::export(".jumpAheadPRNG_")]]
void jumpAheadPRNG_(double n) {
    if (n <= 0) return;

    if (g_use_threading) {
        if (!t_prng) {
            throw std::runtime_error("Cannot jump ahead: No active PRNG in current thread");
        }
        t_prng->skip(static_cast<uint64_t>(n));
    } else {
        std::lock_guard<std::mutex> lock(g_prng_mutex);
        if (!g_prng) {
            throw std::runtime_error("Cannot jump ahead: No active global PRNG");
        }
        g_prng->skip(static_cast<uint64_t>(n));
    }
}

// [[Rcpp::export(".suppress_mpfr_warnings_")]]
bool suppress_mpfr_warnings_() {
    return qiprng::suppress_mpfr_warnings.load();
}

// [[Rcpp::export(".set_mpfr_warnings_")]]
void set_mpfr_warnings_(bool show_warnings) {
    qiprng::suppress_mpfr_warnings.store(!show_warnings);
}

// [[Rcpp::export(".cleanupPRNG_ThreadSafe_")]]
bool cleanupPRNG_ThreadSafe_() {
    try {
        // Properly cleanup resources in a thread-safe manner
        if (g_use_threading) {
            if (!t_prng) {
                Rcpp::warning("No active PRNG in current thread to clean up");
                return true; // Nothing to clean up is not an error
            }

            // Prepare for cleanup (disable thread-safe mode, etc)
            t_prng->prepareForCleanup();

            // Perform actual cleanup
            t_prng->performCleanup();

            // Clear the pointer
            t_prng = nullptr;
        } else {
            std::lock_guard<std::mutex> lock(g_prng_mutex);
            if (!g_prng) {
                Rcpp::warning("No active global PRNG to clean up");
                return true; // Nothing to clean up is not an error
            }

            // Prepare for cleanup (disable thread-safe mode, etc)
            g_prng->prepareForCleanup();

            // Perform actual cleanup
            g_prng->performCleanup();

            // Clear the pointer
            g_prng = nullptr;
        }

        // Clean up all thread resources, not just the current thread
        EnhancedPRNG::cleanupAllThreadResources();

        return true;
    } catch (const std::exception& e) {
        Rcpp::warning("Error during thread-safe PRNG cleanup: %s", e.what());
        return false;
    } catch (...) {
        Rcpp::warning("Unknown error during thread-safe PRNG cleanup");
        return false;
    }
}

// [[Rcpp::export(".cleanupPRNG_Final_")]]
bool cleanupPRNG_Final_() {
    try {
        // This is the final cleanup that should be called after all other cleanups

        // Clear thread-local and global PRNG pointers (don't invoke destructors)
        t_prng = nullptr;
        {
            std::lock_guard<std::mutex> lock(g_prng_mutex);
            g_prng = nullptr;
        }

        // Clean up all thread resources one more time
        EnhancedPRNG::cleanupAllThreadResources();

        // Reset global flags
        g_use_threading = false;
        cleanup_in_progress.store(false, std::memory_order_release);

        return true;
    } catch (...) {
        // We don't even try to log errors in the final cleanup
        return false;
    }
}
```

```

---

## File: /Users/biostochastics/Development/GitHub/qiprng/src/test_thread_safety.cpp

### Summary
No declarations found

**Tags**: `cpp`

```cpp
## File: /Users/biostochastics/Development/GitHub/qiprng/src/test_thread_safety.cpp
```cpp
#include <Rcpp.h>
#include "prng_common.hpp"
#include "prng_utils.hpp"
#include "enhanced_prng.hpp"

// Forward declaration using namespace
namespace qiprng {
    void loadCSVDiscriminants();
}

// [[Rcpp::export]]
Rcpp::List test_choose_discriminant(int thread_count = 4, int iterations = 10) {
    Rcpp::Rcout << "\nTesting with " << thread_count << " threads and "
                << iterations << " iterations" << std::endl;

    // Initialize vector to store results from each thread
    std::vector<std::vector<long long>> results(thread_count);

    // Initialize libsodium first
    qiprng::initialize_libsodium_if_needed();

    // Make sure CSV discriminants are loaded first in main thread
    // This prevents race conditions when multiple threads try to load it
    qiprng::loadCSVDiscriminants();

    // Create threads
    std::vector<std::thread> threads;

    // Starting barrier
    std::atomic<int> ready_count(0);
    std::atomic<bool> start_flag(false);

    // Clear the global used discriminants set before starting test
    {
        std::lock_guard<std::mutex> lock(qiprng::g_disc_mutex);
        qiprng::g_used_discriminants.clear();
    }

    Rcpp::Rcout << "Checking CSV discriminants are loaded..." << std::endl;
    size_t csv_size = 0;
    {
        std::lock_guard<std::mutex> lock(qiprng::g_csv_disc_mutex);
        csv_size = qiprng::g_csv_discriminants.size();
    }
    Rcpp::Rcout << "CSV discriminants size: " << csv_size << std::endl;

    // Force CSV discriminants to be generated if empty
    if (csv_size == 0) {
        Rcpp::Rcout << "CSV discriminants is empty. Generating default values." << std::endl;

        {
            std::lock_guard<std::mutex> lock(qiprng::g_csv_disc_mutex);
            for (int i = 0; i < 100; i++) {
                long a = 1 + (i % 10);
                long b = 3 + 2 * (i % 8);
                long c = -1 - (i % 5);
                long long disc = static_cast<long long>(b) * b - 4LL * static_cast<long long>(a) * c;
                if (disc > 0) {
                    qiprng::g_csv_discriminants.emplace_back(a, b, c, disc);
                }
            }
            Rcpp::Rcout << "Generated " << qiprng::g_csv_discriminants.size() << " default discriminants" << std::endl;
        }
    }

    // Launch threads with additional safety and debug
    for (int t = 0; t < thread_count; t++) {
        Rcpp::Rcout << "Creating thread " << t << std::endl;
        try {
            threads.push_back(std::thread([t, iterations, &results, &ready_count, &start_flag]() {
                try {
                    Rcpp::Rcout << "Thread " << t << " starting" << std::endl;

                    // Wait for all threads to be ready
                    ready_count++;
                    while (!start_flag.load()) {
                        std::this_thread::yield();
                    }

                    Rcpp::Rcout << "Thread " << t << " running" << std::endl;

                    // Choose discriminants
                    results[t].reserve(iterations);
                    for (int i = 0; i < iterations; i++) {
                        try {
                            // Use a wider range to avoid collisions between threads
                            long long discriminant = qiprng::chooseUniqueDiscriminant(5, 10000000);

                            if (discriminant > 0) {
                                Rcpp::Rcout << "Thread " << t << " selected discriminant "
                                            << i << ": " << discriminant << std::endl;
                                results[t].push_back(discriminant);
                            } else {
                                Rcpp::Rcout << "Thread " << t << " got invalid discriminant: "
                                            << discriminant << std::endl;
                                results[t].push_back(-1); // Error indicator
                            }
                        } catch (const std::exception& e) {
                            Rcpp::Rcout << "Thread " << t << " encountered error in iteration "
                                        << i << ": " << e.what() << std::endl;
                            results[t].push_back(-1); // Error indicator
                        } catch (...) {
                            Rcpp::Rcout << "Thread " << t << " encountered unknown error in iteration "
                                        << i << std::endl;
                            results[t].push_back(-1); // Error indicator
                        }
                    }

                    Rcpp::Rcout << "Thread " << t << " completed with "
                                << results[t].size() << " discriminants" << std::endl;

                } catch (const std::exception& e) {
                    Rcpp::Rcout << "Thread " << t << " failed with exception: "
                                << e.what() << std::endl;
                } catch (...) {
                    Rcpp::Rcout << "Thread " << t << " failed with unknown exception" << std::endl;
                }
            }));
        } catch (const std::exception& e) {
            Rcpp::Rcout << "Failed to create thread " << t << ": " << e.what() << std::endl;
        } catch (...) {
            Rcpp::Rcout << "Failed to create thread " << t << " with unknown error" << std::endl;
        }
    }

    // Wait for all threads to be ready
    while (ready_count.load() < thread_count) {
        std::this_thread::sleep_for(std::chrono::milliseconds(10));
    }

    // Start all threads at once
    start_flag.store(true);

    // Wait for all threads to complete
    for (auto& t : threads) {
        if (t.joinable()) {
            t.join();
        }
    }

    // Print summary of results
    int total_count = 0;
    for (int t = 0; t < thread_count; t++) {
        total_count += results[t].size();
        Rcpp::Rcout << "Thread " << t << " generated "
                    << results[t].size() << " discriminants" << std::endl;
    }
    Rcpp::Rcout << "Total discriminants: " << total_count << std::endl;

    // Convert results to R list
    Rcpp::List r_results(thread_count);
    for (int t = 0; t < thread_count; t++) {
        r_results[t] = Rcpp::NumericVector(results[t].begin(), results[t].end());
    }

    return r_results;
}

// [[Rcpp::export]]
bool check_discriminants_unique(Rcpp::List discriminant_lists) {
    std::unordered_set<long long> all_discriminants;

    for (int i = 0; i < discriminant_lists.size(); i++) {
        Rcpp::NumericVector discr_vec = discriminant_lists[i];

        for (int j = 0; j < discr_vec.size(); j++) {
            long long d = static_cast<long long>(discr_vec[j]);
            if (d == -1) continue; // Skip error indicators

            if (all_discriminants.find(d) != all_discriminants.end()) {
                // Found duplicate
                Rcpp::Rcout << "Duplicate discriminant found: " << d << std::endl;
                return false;
            }

            all_discriminants.insert(d);
        }
    }

    Rcpp::Rcout << "All discriminants are unique (" << all_discriminants.size() << " total)" << std::endl;
    return true;
}
```

```

---

## File: /Users/biostochastics/Development/GitHub/qiprng/src/prng_warnings.cpp

### Summary
No declarations found

**Tags**: `cpp`

```cpp
## File: /Users/biostochastics/Development/GitHub/qiprng/src/prng_warnings.cpp
```cpp
#include "prng_common.hpp"
#include <atomic>

namespace qiprng {

// Initialize global warning suppression flag as atomic for thread-safety
std::atomic<bool> suppress_mpfr_warnings{false};

// Initialize thread-local warning counter
thread_local int mpfr_warning_count = 0;

} // namespace qiprng
```

```

---

## File: /Users/biostochastics/Development/GitHub/qiprng/src/ziggurat_normal.cpp

### Summary
No declarations found

**Tags**: `cpp`

```cpp
## File: /Users/biostochastics/Development/GitHub/qiprng/src/ziggurat_normal.cpp
```cpp
// File: ziggurat_normal.cpp
// --------------------------------------------------------------
#include "ziggurat_normal.hpp"
#include <stdexcept>    // For std::runtime_error
#include <atomic>       // For debugging counters if re-enabled
#include <random>       // For std::mt19937, std::normal_distribution
#include <functional>   // For std::function

// Set to true to enable Rcpp::Rcout debug messages
const bool ZIGGURAT_DEBUG_LOGGING = false;


namespace qiprng {

// Initialize static members
std::array<double, ZigguratNormal::ZIGGURAT_TABLE_SIZE> ZigguratNormal::cached_x_table_;
std::array<double, ZigguratNormal::ZIGGURAT_TABLE_SIZE> ZigguratNormal::cached_y_table_;
std::array<uint32_t, ZigguratNormal::ZIGGURAT_TABLE_SIZE> ZigguratNormal::cached_k_table_;
std::atomic<bool> ZigguratNormal::tables_initialized_(false);
std::mutex ZigguratNormal::tables_mutex_;

// Initialize cleanup flag
std::atomic<bool> ZigguratNormal::cleanup_in_progress_(false);

// Initialize thread-local static members
thread_local std::array<double, ZigguratNormal::ZIGGURAT_TABLE_SIZE> ZigguratNormal::tls_x_table_;
thread_local std::array<double, ZigguratNormal::ZIGGURAT_TABLE_SIZE> ZigguratNormal::tls_y_table_;
thread_local std::array<uint32_t, ZigguratNormal::ZIGGURAT_TABLE_SIZE> ZigguratNormal::tls_k_table_;
thread_local bool ZigguratNormal::tls_tables_initialized_ = false;

// Initialize thread-local random cache members
thread_local std::array<double, ZigguratNormal::RANDOM_CACHE_SIZE> ZigguratNormal::tls_random_cache_;
thread_local size_t ZigguratNormal::tls_random_cache_pos_ = ZigguratNormal::RANDOM_CACHE_SIZE; // Start empty
thread_local bool ZigguratNormal::tls_random_cache_initialized_ = false;

// Initialize thread-local manager
thread_local ZigguratTLSManager* ZigguratNormal::tls_manager_ = nullptr;

// Initialize thread-local exiting flag for ZigguratTLSManager
thread_local bool ZigguratTLSManager::thread_exiting_ = false;

void ZigguratNormal::initialize_static_tables() {
    // Double-checked locking pattern for thread safety
    if (!tables_initialized_.load(std::memory_order_acquire)) {
        std::lock_guard<std::mutex> lock(tables_mutex_);
        if (!tables_initialized_.load(std::memory_order_relaxed)) {
            // Constants for Ziggurat tables (Marsaglia and Tsang 2000)
            const double R_zig = 3.6541528853610088; // x_0, rightmost boundary of rectangles
            const double V_zig = 0.004928673233992336; // Area under tail R_zig to infinity

            // k_table_[0] is R_zig * pdf(R_zig) / V_zig scaled to uint32
            // x_table_[0] is R_zig
            // y_table_[0] is pdf(R_zig)
            double f = std::exp(-0.5 * R_zig * R_zig);
            cached_k_table_[0] = static_cast<uint32_t>((R_zig * f / V_zig) * static_cast<double>(UINT32_MAX));
            cached_x_table_[0] = R_zig;
            cached_y_table_[0] = f;

            // For i = 1 to ZIGGURAT_TABLE_SIZE - 2
            for (int i = 1; i < ZIGGURAT_TABLE_SIZE - 1; ++i) {
                double prev_x = cached_x_table_[i-1];
                double prev_f = cached_y_table_[i-1]; // pdf(prev_x)

                cached_x_table_[i] = std::sqrt(-2.0 * std::log(V_zig / prev_x + prev_f));
                cached_y_table_[i] = std::exp(-0.5 * cached_x_table_[i] * cached_x_table_[i]);
                cached_k_table_[i] = static_cast<uint32_t>((cached_x_table_[i-1] / cached_x_table_[i]) * static_cast<double>(UINT32_MAX));
            }

            // Special case for the last rectangle
            cached_k_table_[ZIGGURAT_TABLE_SIZE-1] = UINT32_MAX; // Always accept for the base segment
            cached_x_table_[ZIGGURAT_TABLE_SIZE-1] = 0.0;
            cached_y_table_[ZIGGURAT_TABLE_SIZE-1] = 1.0; // pdf(0)

            // Make sure all writes are visible to other threads
            tables_initialized_.store(true, std::memory_order_release);
        }
    }
}

void ZigguratNormal::initialize_tables_original() {
    // If standard normal parameters, use cached tables
    if (mean_ == 0.0 && stddev_ == 1.0) {
        use_cached_tables();
        return;
    }

    // If custom parameters, initialize tables directly in instance
    // Constants for Ziggurat tables (Marsaglia and Tsang 2000)
    const double R_zig = 3.6541528853610088; // x_0, rightmost boundary of rectangles
    const double V_zig = 0.004928673233992336; // Area under tail R_zig to infinity

    // k_table_[0] is R_zig * pdf(R_zig) / V_zig scaled to uint32
    // x_table_[0] is R_zig
    // y_table_[0] is pdf(R_zig)
    double f = std::exp(-0.5 * R_zig * R_zig);
    k_table_[0] = static_cast<uint32_t>((R_zig * f / V_zig) * static_cast<double>(UINT32_MAX));
    x_table_[0] = R_zig;
    y_table_[0] = f;

    // For i = 1 to ZIGGURAT_TABLE_SIZE - 2
    for (int i = 1; i < ZIGGURAT_TABLE_SIZE -1; ++i) {
        // x_i = sqrt(-2 * log(V/x_{i-1} + f_{i-1}))
        double prev_x = x_table_[i-1];
        double prev_f = y_table_[i-1]; // pdf(prev_x)

        x_table_[i] = std::sqrt(-2.0 * std::log(V_zig / prev_x + prev_f));
        y_table_[i] = std::exp(-0.5 * x_table_[i] * x_table_[i]);
        k_table_[i] = static_cast<uint32_t>((x_table_[i-1] / x_table_[i]) * static_cast<double>(UINT32_MAX));
    }

    // Special case for the last rectangle
    k_table_[ZIGGURAT_TABLE_SIZE-1] = UINT32_MAX; // Always accept for the base segment
    x_table_[ZIGGURAT_TABLE_SIZE-1] = 0.0;
    y_table_[ZIGGURAT_TABLE_SIZE-1] = 1.0; // pdf(0)
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
    // Skip if cleanup is in progress
    if (cleanup_in_progress_.load(std::memory_order_acquire)) {
        return;
    }

    // Setup TLS manager if needed
    if (!tls_manager_) {
        tls_manager_ = &ZigguratTLSManager::instance();
        tls_manager_->set_owner(this);
    }

    // Initialize tables if not already done
    if (!tls_tables_initialized_ && tls_manager_->is_valid()) {
        try {
            // Copy the data
            tls_x_table_ = x_table_;
            tls_y_table_ = y_table_;
            tls_k_table_ = k_table_;
            tls_tables_initialized_ = true;

            if (ZIGGURAT_DEBUG_LOGGING) {
                Rcpp::Rcout << "Thread " << std::this_thread::get_id()
                          << " initialized TLS tables" << std::endl;
            }
        }
        catch (...) {
            if (ZIGGURAT_DEBUG_LOGGING) {
                Rcpp::Rcout << "Exception during TLS table initialization" << std::endl;
            }
            // Leave tables uninitialized on exception - will retry on next call
        }
    }
}

void ZigguratNormal::initialize_random_cache() {
    // Skip if cleanup is in progress
    if (cleanup_in_progress_.load(std::memory_order_acquire)) {
        return;
    }

    // Setup TLS manager if needed (may not be done if initialize_thread_local_tables wasn't called first)
    if (!tls_manager_) {
        tls_manager_ = &ZigguratTLSManager::instance();
        tls_manager_->set_owner(this);
    }

    // Only proceed if manager is valid
    if (tls_manager_->is_valid() && !tls_random_cache_initialized_) {
        try {
            // Initialize the cache
            refill_random_cache();
            tls_random_cache_initialized_ = true;

            if (ZIGGURAT_DEBUG_LOGGING) {
                Rcpp::Rcout << "Thread " << std::this_thread::get_id()
                          << " initialized random cache" << std::endl;
            }
        }
        catch (...) {
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
                    u = 0.5; // Use a safe default
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
        return 0.5; // Safe fallback value
    }

    // Check TLS manager
    if (!tls_manager_ || !tls_manager_->is_valid()) {
        // TLS manager not initialized or invalid - try to initialize
        initialize_thread_local_tables();

        if (!tls_manager_ || !tls_manager_->is_valid()) {
            return 0.5; // Safe fallback value if initialization fails
        }
    }

    // Initialize cache if needed
    if (!tls_random_cache_initialized_) {
        initialize_random_cache();

        // Double-check initialization success
        if (!tls_random_cache_initialized_) {
            return 0.5; // Safe fallback value if initialization fails
        }
    }

    // Refill cache if empty
    if (tls_random_cache_pos_ >= RANDOM_CACHE_SIZE) {
        try {
            refill_random_cache();
        }
        catch (...) {
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

    if (ZIGGURAT_DEBUG_LOGGING) Rcpp::Rcout << "[Ziggurat Tail TS] Enter. R_zig=" << tls_x_table_[0] << std::endl;

    // Maximum number of attempts to avoid infinite loops
    const int MAX_ATTEMPTS = 100;
    int attempts = 0;

    do {
        attempts++;
        if (attempts > MAX_ATTEMPTS) {
            throw std::runtime_error("ZigguratNormal::sample_from_tail_thread_safe exceeded maximum attempts");
        }

        try {
            // Sample x from exp distribution with rate R_zig, shifted by R_zig
            // x = R_zig + E / R_zig where E ~ Exp(1)
            // E = -log(U1) where U1 ~ Uniform(0,1)
            double u1 = get_cached_uniform(); // Use cached uniform value

            // Calculate exponential value using safe u1
            x = -std::log(u1) / tls_x_table_[0]; // This is E/R_zig

            // Sample y from Exp(1)
            double u2 = get_cached_uniform(); // Use cached uniform value

            // Calculate second exponential value
            y = -std::log(u2);

            // Diagnostic output
            if (ZIGGURAT_DEBUG_LOGGING) {
                Rcpp::Rcout << "[Ziggurat Tail TS] u1=" << u1 << ", u2=" << u2
                            << ", x_exp_part=" << x << ", y_exp=" << y
                            << ", test: 2*y (" << 2*y << ") vs x*x (" << x*x << ")" << std::endl;
            }

            // Break out of the loop if we've found a valid pair (accept condition)
            if (y + y >= x * x) {
                break;
            }

        } catch (const std::exception& e) {
            if (ZIGGURAT_DEBUG_LOGGING) {
                Rcpp::Rcout << "[Ziggurat Tail TS] Exception: " << e.what() << ". Retrying." << std::endl;
            }
            // In case of exceptions, just retry
        } catch (...) {
            if (ZIGGURAT_DEBUG_LOGGING) {
                Rcpp::Rcout << "[Ziggurat Tail TS] Unknown exception. Retrying." << std::endl;
            }
            // In case of exceptions, just retry
        }

    } while (true); // Continue until we break out with valid values

    // Calculate final result (R_zig + exponential part)
    double result = tls_x_table_[0] + x;

    if (ZIGGURAT_DEBUG_LOGGING) {
        Rcpp::Rcout << "[Ziggurat Tail TS] Exit. Returning " << result << std::endl;
    }

    return result;
}

double ZigguratNormal::sample_from_tail_original() {
    double x, y;

    if (ZIGGURAT_DEBUG_LOGGING) Rcpp::Rcout << "[Ziggurat Tail] Enter. R_zig=" << x_table_[0] << std::endl;

    // Maximum number of attempts to avoid infinite loops
    const int MAX_ATTEMPTS = 100;
    int attempts = 0;

    do {
        attempts++;
        if (attempts > MAX_ATTEMPTS) {
            throw std::runtime_error("ZigguratNormal::sample_from_tail_original exceeded maximum attempts");
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
                    u1 = 0.5; // Safe default
                    break;
                }
            }

            // Calculate exponential value using safe u1
            x = -std::log(u1) / x_table_[0]; // This is E/R_zig

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
                    u2 = 0.5; // Safe default
                    break;
                }
            }

            // Calculate second exponential value
            y = -std::log(u2);

            // Diagnostic output
            if (ZIGGURAT_DEBUG_LOGGING) {
                Rcpp::Rcout << "[Ziggurat Tail] u1=" << u1 << ", u2=" << u2
                            << ", x_exp_part=" << x << ", y_exp=" << y
                            << ", test: 2*y (" << 2*y << ") vs x*x (" << x*x << ")" << std::endl;
            }

            // Break out of the loop if we've found a valid pair (accept condition)
            if (y + y >= x * x) {
                break;
            }

        } catch (const std::exception& e) {
            if (ZIGGURAT_DEBUG_LOGGING) {
                Rcpp::Rcout << "[Ziggurat Tail] Exception: " << e.what() << ". Retrying." << std::endl;
            }
            // In case of exceptions, just retry
        } catch (...) {
            if (ZIGGURAT_DEBUG_LOGGING) {
                Rcpp::Rcout << "[Ziggurat Tail] Unknown exception. Retrying." << std::endl;
            }
            // In case of exceptions, just retry
        }

    } while (true); // Continue until we break out with valid values

    // Calculate final result (R_zig + exponential part)
    double result = x_table_[0] + x;

    if (ZIGGURAT_DEBUG_LOGGING) {
        Rcpp::Rcout << "[Ziggurat Tail] Exit. Returning " << result << std::endl;
    }

    return result;
}


double ZigguratNormal::generate_internal() {
    if (ZIGGURAT_DEBUG_LOGGING) Rcpp::Rcout << "[Ziggurat Gen] Enter." << std::endl;

    uint32_t iz;     // Random integer from bits of uniform
    uint32_t idx;    // Index into tables
    double x_cand;   // Candidate x value

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
                u0 = 0.5; // Use a safe default if generator fails
            }

            // Convert u0 to a 32-bit integer for table lookup
            iz = static_cast<uint32_t>(u0 * 4294967296.0); // Multiply by 2^32
            idx = iz & ZIGGURAT_MASK; // Get index (0 to TABLE_SIZE-1)

            // Get random sign
            bool sign;
            try {
                double sign_u = uniform_generator_();
                sign = (sign_u < 0.5);
            } catch (...) {
                // If generator fails, use a deterministic approach based on iz
                sign = (iz & 0x80000000) != 0; // Use top bit of iz
            }

            if (ZIGGURAT_DEBUG_LOGGING) Rcpp::Rcout << "[Ziggurat Gen] u0=" << u0 << ", iz=" << iz << ", idx=" << idx << std::endl;

            // Handle tail region (idx == 0)
            if (idx == 0) {
                if (ZIGGURAT_DEBUG_LOGGING) Rcpp::Rcout << "[Ziggurat Gen] idx=0 (tail)." << std::endl;

                try {
                    x_cand = sample_from_tail_original();
                    if (ZIGGURAT_DEBUG_LOGGING) Rcpp::Rcout << "[Ziggurat Gen] Tail returned " << x_cand << std::endl;
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
                    u1 = 0.5; // Use a safe default if generator fails
                }
            } catch (...) {
                // If generator fails, derive u1 from iz
                u1 = (iz & 0x7FFFFFFF) / static_cast<double>(0x7FFFFFFF);
            }

            x_cand = u1 * x_table_[idx]; // x_cand is in [0, x_table_[idx])

            if (ZIGGURAT_DEBUG_LOGGING) Rcpp::Rcout << "[Ziggurat Gen] idx=" << idx << ", u1=" << u1
                << ", x_table_[idx]=" << x_table_[idx] << ", x_cand=" << x_cand << std::endl;

            // Check if we're in the rectangle part (fast accept)
            if (iz < k_table_[idx]) {
                if (ZIGGURAT_DEBUG_LOGGING)
                    Rcpp::Rcout << "[Ziggurat Gen] Rectangle accept for idx=" << idx << ". x_cand=" << x_cand << std::endl;
                return sign ? -x_cand : x_cand;
            }

            // Handle base strip special case
            if (idx == ZIGGURAT_TABLE_SIZE - 1) {
                if (ZIGGURAT_DEBUG_LOGGING)
                    Rcpp::Rcout << "[Ziggurat Gen] Base strip for idx=" << idx << ". Re-looping." << std::endl;
                continue;
            }

            // Wedge test - need another uniform
            double u2;
            try {
                u2 = uniform_generator_();
                if (u2 <= 0.0 || u2 >= 1.0) {
                    u2 = 0.5; // Use a safe default if generator fails
                }
            } catch (...) {
                // If generator fails, derive u2 deterministically
                u2 = (((iz >> 8) & 0x7FFFFFFF)) / static_cast<double>(0x7FFFFFFF);
            }

            if (ZIGGURAT_DEBUG_LOGGING) {
                Rcpp::Rcout << "[Ziggurat Gen] Wedge test for idx=" << idx
                    << ". x_cand=" << x_cand << ", u2=" << u2 << std::endl;
                Rcpp::Rcout << "                 y_table_[idx]=" << y_table_[idx]
                    << ", y_table_[idx-1]=" << y_table_[idx-1] << std::endl;
            }

            // Compute pdf value at the candidate point
            double pdf_x = std::exp(-0.5 * x_cand * x_cand);

            // Compute random height in the wedge
            double y_wedge = y_table_[idx-1] + u2 * (y_table_[idx] - y_table_[idx-1]);

            // Accept if point is under the PDF curve
            if (pdf_x > y_wedge) {
                if (ZIGGURAT_DEBUG_LOGGING)
                    Rcpp::Rcout << "[Ziggurat Gen] Wedge accept for idx=" << idx << ". x_cand=" << x_cand << std::endl;
                return sign ? -x_cand : x_cand;
            }

            if (ZIGGURAT_DEBUG_LOGGING)
                Rcpp::Rcout << "[Ziggurat Gen] Wedge reject for idx=" << idx << ". Loop again." << std::endl;
            // If rejected, continue to next attempt

        } catch (const std::exception& e) {
            if (ZIGGURAT_DEBUG_LOGGING)
                Rcpp::Rcout << "[Ziggurat Gen] Exception: " << e.what() << ". Continuing." << std::endl;
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
    if (ZIGGURAT_DEBUG_LOGGING) Rcpp::Rcout << "[Ziggurat TS] Enter." << std::endl;

    // Initialize thread-local tables if not yet done
    initialize_thread_local_tables();

    // Initialize random cache if not yet done
    initialize_random_cache();

    uint32_t iz;     // Random integer from bits of uniform
    uint32_t idx;    // Index into tables
    double x_cand;   // Candidate x value

    // Maximum number of attempts to avoid infinite loops
    const int MAX_ATTEMPTS = 1000;
    int attempts = 0;

    while (attempts < MAX_ATTEMPTS) {
        attempts++;

        try {
            // Generate uniform u0 for sign and index - from cache to avoid mutex
            double u0 = get_cached_uniform();

            // Convert u0 to a 32-bit integer for table lookup
            iz = static_cast<uint32_t>(u0 * 4294967296.0); // Multiply by 2^32
            idx = iz & ZIGGURAT_MASK; // Get index (0 to TABLE_SIZE-1)

            // Get random sign - from cache to avoid mutex
            double sign_u = get_cached_uniform();
            bool sign = (sign_u < 0.5);

            if (ZIGGURAT_DEBUG_LOGGING) Rcpp::Rcout << "[Ziggurat TS] u0=" << u0 << ", iz=" << iz << ", idx=" << idx << std::endl;

            // Handle tail region (idx == 0)
            if (idx == 0) {
                if (ZIGGURAT_DEBUG_LOGGING) Rcpp::Rcout << "[Ziggurat TS] idx=0 (tail)." << std::endl;

                // Use thread-safe tail sampling that uses the cached values
                double tail_sample = sample_from_tail_thread_safe();

                if (ZIGGURAT_DEBUG_LOGGING) Rcpp::Rcout << "[Ziggurat TS] Tail returned " << tail_sample << std::endl;
                return sign ? -tail_sample : tail_sample;
            }

            // For regular layers, generate x candidate - from cache to avoid mutex
            double u1 = get_cached_uniform();

            // Use thread-local tables for the rest to avoid locks
            x_cand = u1 * tls_x_table_[idx]; // x_cand is in [0, tls_x_table_[idx])

            if (ZIGGURAT_DEBUG_LOGGING) Rcpp::Rcout << "[Ziggurat TS] idx=" << idx << ", u1=" << u1
                << ", tls_x_table_[idx]=" << tls_x_table_[idx] << ", x_cand=" << x_cand << std::endl;

            // Check if we're in the rectangle part (fast accept)
            if (iz < tls_k_table_[idx]) {
                if (ZIGGURAT_DEBUG_LOGGING)
                    Rcpp::Rcout << "[Ziggurat TS] Rectangle accept for idx=" << idx << ". x_cand=" << x_cand << std::endl;
                return sign ? -x_cand : x_cand;
            }

            // Handle base strip special case
            if (idx == ZIGGURAT_TABLE_SIZE - 1) {
                if (ZIGGURAT_DEBUG_LOGGING)
                    Rcpp::Rcout << "[Ziggurat TS] Base strip for idx=" << idx << ". Re-looping." << std::endl;
                continue;
            }

            // Wedge test - need another uniform - from cache to avoid mutex
            double u2 = get_cached_uniform();

            if (ZIGGURAT_DEBUG_LOGGING) {
                Rcpp::Rcout << "[Ziggurat TS] Wedge test for idx=" << idx
                    << ". x_cand=" << x_cand << ", u2=" << u2 << std::endl;
                Rcpp::Rcout << "                 tls_y_table_[idx]=" << tls_y_table_[idx]
                    << ", tls_y_table_[idx-1]=" << tls_y_table_[idx-1] << std::endl;
            }

            // Compute pdf value at the candidate point
            double pdf_x = std::exp(-0.5 * x_cand * x_cand);

            // Compute random height in the wedge
            double y_wedge = tls_y_table_[idx-1] + u2 * (tls_y_table_[idx] - tls_y_table_[idx-1]);

            // Accept if point is under the PDF curve
            if (pdf_x > y_wedge) {
                if (ZIGGURAT_DEBUG_LOGGING)
                    Rcpp::Rcout << "[Ziggurat TS] Wedge accept for idx=" << idx << ". x_cand=" << x_cand << std::endl;
                return sign ? -x_cand : x_cand;
            }

            if (ZIGGURAT_DEBUG_LOGGING)
                Rcpp::Rcout << "[Ziggurat TS] Wedge reject for idx=" << idx << ". Loop again." << std::endl;
            // If rejected, continue to next attempt

        } catch (const std::exception& e) {
            if (ZIGGURAT_DEBUG_LOGGING)
                Rcpp::Rcout << "[Ziggurat TS] Exception: " << e.what() << ". Continuing." << std::endl;
            // Continue to next attempt
        } catch (...) {
            if (ZIGGURAT_DEBUG_LOGGING)
                Rcpp::Rcout << "[Ziggurat TS] Unknown exception. Continuing." << std::endl;
            // Continue to next attempt
        }
    }

    // If we reach here, we've failed to generate a valid sample after MAX_ATTEMPTS
    throw std::runtime_error("ZigguratNormal::generate_internal_thread_safe failed after maximum attempts");
}


ZigguratNormal::ZigguratNormal(std::function<double()> uniform_generator,
                               double mean, double stddev,
                               bool thread_safe_mode)
    : uniform_generator_(uniform_generator),
      mean_(mean),
      stddev_(stddev),
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
        throw std::runtime_error(std::string("ZigguratNormal: Initialization failed - ") + e.what());
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
                } catch (...) {
                    // Use fallback RNG instead of fixed mean value
                    static thread_local std::mt19937 fallback_rng(std::random_device{}());
                    std::normal_distribution<double> fallback_dist(mean_, stddev_);
                    return fallback_dist(fallback_rng);
                }
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
        const size_t max_threads = std::min(hardware_threads, size_t(4)); // Reduced from 8
        const size_t thread_count = std::min(max_threads, count / 2000 + 1); // Increased threshold

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
                        return; // Exit this thread's lambda
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
                                buffer[i_local] = mean_; // Fallback to mean value
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
                    std::chrono::milliseconds timeout(1000); // 1 second timeout
                    auto thread_finished = std::async(std::launch::async, [&]() {
                        threads[t].join();
                    });

                    if (thread_finished.wait_for(timeout) != std::future_status::ready) {
                        // Thread didn't finish in time - we have a problem
                        any_thread_failed.store(true);
                        // Cannot safely detach or terminate - we'll have to wait
                        threads[t].join(); // This might block
                    }
                } catch (...) {
                    any_thread_failed.store(true);
                }
            }
        }

        // If any thread failed, log a warning
        if (any_thread_failed.load()) {
            Rcpp::warning("Some threads failed during parallel generation. Results may be affected.");
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
    // Try to mark cleanup in progress using compare_exchange to prevent multiple cleanups
    bool expected = false;
    if (!cleanup_in_progress_.compare_exchange_strong(expected, true, std::memory_order_acq_rel)) {
        // Cleanup is already in progress by another thread, don't duplicate effort
        if (ZIGGURAT_DEBUG_LOGGING) {
            Rcpp::Rcout << "Thread " << std::this_thread::get_id()
                       << " skipping cleanup as it's already in progress" << std::endl;
        }
        return;
    }

    try {
        // Reset all thread-local variables to a clean state
        tls_tables_initialized_ = false;
        tls_random_cache_initialized_ = false;
        tls_random_cache_pos_ = RANDOM_CACHE_SIZE;  // Mark as empty

        // Invalidate the TLS manager if it exists
        if (tls_manager_) {
            tls_manager_->invalidate();
            tls_manager_ = nullptr;
        }

        if (ZIGGURAT_DEBUG_LOGGING) {
            Rcpp::Rcout << "Thread " << std::this_thread::get_id()
                       << " cleaned up ZigguratNormal resources" << std::endl;
        }

        // Reset TLS tables - only do this if they were initialized
        if (tls_tables_initialized_) {
            for (size_t i = 0; i < ZIGGURAT_TABLE_SIZE; ++i) {
                tls_x_table_[i] = 0.0;
                tls_y_table_[i] = 0.0;
                tls_k_table_[i] = 0;
            }
        }

        if (ZIGGURAT_DEBUG_LOGGING) {
            Rcpp::Rcout << "ZigguratNormal thread-local resources cleaned up" << std::endl;
        }
    } catch (...) {
        // Suppress any exceptions during cleanup
        if (ZIGGURAT_DEBUG_LOGGING) {
            Rcpp::Rcout << "Exception during thread-local cleanup - suppressed" << std::endl;
        }
    }

    // Keep the cleanup flag set - we don't reset it because once we've cleaned up,
    // we want to prevent any new access attempts
}

} // namespace qiprng
```

```

---

## File: /Users/biostochastics/Development/GitHub/qiprng/src/quadratic_irrational.cpp

### Summary
No declarations found

**Tags**: `cpp`

```cpp
## File: /Users/biostochastics/Development/GitHub/qiprng/src/quadratic_irrational.cpp
```cpp
// File: quadratic_irrational.cpp
// --------------------------------------------------------------
#include "quadratic_irrational.hpp"
#include "deterministic_rng.hpp"
#include <limits> // For std::numeric_limits
#include <cstdlib> // For std::getenv

#ifndef M_PI // Ensure M_PI is defined
#define M_PI 3.14159265358979323846
#endif


namespace qiprng {

// Single iteration with improved error handling
void QuadraticIrrational::step_once() {
    if (!value_ || !next_ || !temp_ || !temp2_ || !root_ ||
        !value_->is_valid() || !next_->is_valid() || !temp_->is_valid() ||
        !temp2_->is_valid() || !root_->is_valid()) {
        throw std::runtime_error("QuadraticIrrational: Invalid MPFR state in step_once");
    }

    int ret = 0;

    int op_ret = 0;

    op_ret = mpfr_mul(*temp_->get(), *value_->get(), *value_->get(), MPFR_RNDN);
    ret |= op_ret;
    check_mpfr_result(op_ret, "multiplication");
    if (mpfr_nan_p(*temp_->get())) {
        throw std::runtime_error("QuadraticIrrational: NaN result in multiplication (x_n^2)");
    }

    op_ret = mpfr_mul_si(*next_->get(), *temp_->get(), a_, MPFR_RNDN);
    ret |= op_ret;
    check_mpfr_result(op_ret, "multiply_scalar");
    if (mpfr_nan_p(*next_->get())) {
        throw std::runtime_error("QuadraticIrrational: NaN result in a*x_n^2 calculation");
    }

    op_ret = mpfr_mul_si(*temp_->get(), *value_->get(), b_, MPFR_RNDN);
    ret |= op_ret;
    check_mpfr_result(op_ret, "multiply_scalar");
    if (mpfr_nan_p(*temp_->get())) {
        throw std::runtime_error("QuadraticIrrational: NaN result in b*x_n calculation");
    }

    op_ret = mpfr_add(*next_->get(), *next_->get(), *temp_->get(), MPFR_RNDN);
    ret |= op_ret;
    check_mpfr_result(op_ret, "addition");
    if (mpfr_nan_p(*next_->get())) {
        throw std::runtime_error("QuadraticIrrational: NaN result in addition (a*x_n^2 + b*x_n)");
    }

    op_ret = mpfr_add_si(*next_->get(), *next_->get(), c_, MPFR_RNDN);
    ret |= op_ret;
    check_mpfr_result(op_ret, "add_scalar");
    if (mpfr_nan_p(*next_->get())) {
        throw std::runtime_error("QuadraticIrrational: NaN result after adding constant c");
    }

    op_ret = mpfr_frac(*next_->get(), *next_->get(), MPFR_RNDN);
    ret |= op_ret;
    check_mpfr_result(op_ret, "fractional");
    if (mpfr_nan_p(*next_->get())) {
        throw std::runtime_error("QuadraticIrrational: NaN result in fractional computation");
    }

    if (mpfr_sgn(*next_->get()) < 0) {
        op_ret = mpfr_add_ui(*next_->get(), *next_->get(), 1, MPFR_RNDN);
        ret |= op_ret;
        check_mpfr_result(op_ret, "add_unsigned");
        if (mpfr_nan_p(*next_->get())) {
            throw std::runtime_error("QuadraticIrrational: NaN result in positivity enforcement");
        }
    }
    mpfr_swap(*value_->get(), *next_->get());

    // Only issue a single warning for the entire operation if any part was inexact
    if (ret != 0) {
        check_mpfr_result(1, "step_once operation", false);
    }
}

QuadraticIrrational::QuadraticIrrational(long a, long b, long c, mpfr_prec_t prec,
                                       uint64_t seed, bool has_seed)
    : a_(a), b_(b), c_(c) {
    // Validate parameters more comprehensively
    if (a == 0) {
        throw std::invalid_argument("QuadraticIrrational: 'a' parameter cannot be zero");
    }

    if (prec < MPFR_PREC_MIN || prec > MPFR_PREC_MAX) {
        throw std::invalid_argument("QuadraticIrrational: Invalid precision value");
    }

    // Use __int128 for safe discriminant calculation when available
    long long disc_ll;

#ifdef __SIZEOF_INT128__
    // Use 128-bit integers for overflow-safe calculation
    __int128 b_128 = static_cast<__int128>(b_);
    __int128 a_128 = static_cast<__int128>(a_);
    __int128 c_128 = static_cast<__int128>(c_);
    __int128 disc_128 = b_128 * b_128 - 4 * a_128 * c_128;

    // Check if result fits in long long
    if (disc_128 > std::numeric_limits<long long>::max() ||
        disc_128 < std::numeric_limits<long long>::min()) {
        throw std::runtime_error("QuadraticIrrational: discriminant exceeds long long range");
    }
    disc_ll = static_cast<long long>(disc_128);
#else
    // Fallback to overflow checking for platforms without __int128
    const long long MAX_SAFE_LONG = std::numeric_limits<long>::max();

    // Check if b can safely be squared
    if (std::abs(static_cast<long long>(b)) > static_cast<long long>(std::sqrt(static_cast<double>(MAX_SAFE_LONG)))) {
        throw std::runtime_error("QuadraticIrrational: 'b' parameter is too large, would cause overflow in discriminant calculation");
    }

    // Check for the 4*a*c calculation
    if (std::abs(a) > MAX_SAFE_LONG / 4 || (a != 0 && std::abs(c) > MAX_SAFE_LONG / (4 * std::abs(a)))) {
        throw std::runtime_error("QuadraticIrrational: 'a' and 'c' parameters would cause overflow in 4*a*c calculation");
    }

    // Now safely calculate discriminant
    long long b_ll = static_cast<long long>(b_);
    long long a_ll = static_cast<long long>(a_);
    long long c_ll = static_cast<long long>(c_);
    long long b_squared = b_ll * b_ll;
    long long four_ac = 4LL * a_ll * c_ll;
    disc_ll = b_squared - four_ac;
#endif

    // Check for non-positive discriminant
    if (disc_ll <= 0) {
         Rcpp::Rcerr << "a=" << a_ << ", b=" << b_ << ", c=" << c_ << ", disc=" << disc_ll << std::endl;
        throw std::runtime_error("QuadraticIrrational: non-positive discriminant");
    }

    // Check if discriminant is too large for safe square root calculation
    if (disc_ll > static_cast<long long>(std::pow(2.0, 53))) {
        Rcpp::warning("QuadraticIrrational: discriminant %lld is very large, potential precision issues", disc_ll);
    }

    try {
        value_ = std::make_unique<MPFRWrapper>(prec);
        root_ = std::make_unique<MPFRWrapper>(prec);
        next_ = std::make_unique<MPFRWrapper>(prec);
        temp_ = std::make_unique<MPFRWrapper>(prec);
        temp2_ = std::make_unique<MPFRWrapper>(prec);

        if (!value_->is_valid() || !root_->is_valid() || !next_->is_valid() ||
            !temp_->is_valid() || !temp2_->is_valid()) {
            throw std::runtime_error("QuadraticIrrational: Failed to initialize one or more MPFR wrappers");
        }

        int op_ret = 0;

        // MPFR may not have mpfr_set_sj for signed long long, use mpfr_set_si with casting
        op_ret = mpfr_set_si(*root_->get(), static_cast<long>(disc_ll), MPFR_RNDN);
        check_mpfr_result(op_ret, "set_discriminant");

        op_ret = mpfr_sqrt(*root_->get(), *root_->get(), MPFR_RNDN);
        // It's normal for sqrt to be inexact, so suppress this specific warning
        if (op_ret != 0 && !suppress_mpfr_warnings.load()) {
            static thread_local int sqrt_warning_count = 0;
            if (sqrt_warning_count < 1) { // Only show once per thread
                Rcpp::warning("Some inexact results in square root operations are normal and expected");
                sqrt_warning_count++;
            }
        }
        if (mpfr_nan_p(*root_->get())) throw std::runtime_error("QuadraticIrrational: sqrt(disc) resulted in NaN");

        op_ret = mpfr_set_si(*value_->get(), b_, MPFR_RNDN);
        check_mpfr_result(op_ret, "set_b_value");

        op_ret = mpfr_add(*value_->get(), *value_->get(), *root_->get(), MPFR_RNDN);
        check_mpfr_result(op_ret, "add_sqrt_disc");
        if (mpfr_nan_p(*value_->get())) throw std::runtime_error("QuadraticIrrational: (b+sqrt(disc)) resulted in NaN");

        if (a_ == 0) throw std::logic_error("QuadraticIrrational: 'a' is zero before division (should have been caught)"); // Should be caught earlier

        op_ret = mpfr_div_si(*value_->get(), *value_->get(), 2 * a_, MPFR_RNDN);
        check_mpfr_result(op_ret, "div_by_2a");
        if (mpfr_nan_p(*value_->get())) throw std::runtime_error("QuadraticIrrational: division by 2a resulted in NaN");

        op_ret = mpfr_frac(*value_->get(), *value_->get(), MPFR_RNDN);
        check_mpfr_result(op_ret, "initial_frac");
        if (mpfr_nan_p(*value_->get())) throw std::runtime_error("QuadraticIrrational: initial frac() resulted in NaN");

        // Warm-up period for quadratic irrational sequences
        // Literature suggests that nonlinear PRNGs benefit from an initial "burn-in" period
        // to ensure the sequence has moved away from potentially predictable initial states.
        //
        // For quadratic recurrences:
        // - Minimum of sqrt(period) steps recommended (Knuth, TAOCP Vol 2)
        // - For cryptographic applications, 10x the state size is common practice
        // - Our quadratic irrationals have effectively infinite period, so we use
        //   empirically chosen values that balance security and performance
        //
        // Range [10000, 100000] chosen because:
        // - 10,000 minimum ensures at least 10^4 nonlinear iterations
        // - 100,000 maximum prevents excessive initialization time
        // - Random selection within range prevents timing-based state inference
        const uint64_t MIN_WARMUP_ITERATIONS = 10000;   // ~10^4 ensures good mixing
        const uint64_t MAX_WARMUP_ITERATIONS = 100000;  // ~10^5 upper bound for performance

        // Determine skip amount based on whether seed is provided
        uint64_t skip_amt;
        if (has_seed) {
            // Deterministic skip based on seed and parameters
            auto det_rng = DeterministicRNGFactory::create(seed,
                std::to_string(a) + "_" + std::to_string(b) + "_" + std::to_string(c));
            std::uniform_int_distribution<uint64_t> skip_dist(MIN_WARMUP_ITERATIONS, MAX_WARMUP_ITERATIONS);
            skip_amt = skip_dist(det_rng);
        } else {
            // Original random behavior
            std::random_device rd;
            std::mt19937_64 rng(rd());
            std::uniform_int_distribution<uint64_t> skip_dist(MIN_WARMUP_ITERATIONS, MAX_WARMUP_ITERATIONS);
            skip_amt = skip_dist(rng);
        }

        // Perform the initial warm-up skip
        for (uint64_t i = 0; i < skip_amt; i++) {
            step_once();
        }

    } catch (const std::bad_alloc& e) {
        throw std::runtime_error(std::string("QuadraticIrrational: Memory allocation failed during construction: ") + e.what());
    } catch (const std::exception& e) {
        throw std::runtime_error(std::string("QuadraticIrrational: Initialization failed: ") + e.what());
    }
}


double QuadraticIrrational::next() {
    step_once();
    double val = mpfr_get_d(*value_->get(), MPFR_RNDN);
    if (std::isnan(val) || std::isinf(val)) {
         Rcpp::warning("QuadraticIrrational::next() produced NaN/Inf. Returning 0.5.");
         return 0.5; // A fallback value
    }
    return val;
}

void QuadraticIrrational::skip(uint64_t n) {
    jump_ahead(n);
}


void QuadraticIrrational::jump_ahead(uint64_t n) {
    if (!value_ || !next_ || !temp_ || !temp2_ || !root_ ||
        !value_->is_valid() || !next_->is_valid() || !temp_->is_valid() ||
        !temp2_->is_valid() || !root_->is_valid()) {
        throw std::runtime_error("QuadraticIrrational: Invalid MPFR state at the beginning of jump_ahead");
    }

    if (n == 0) {
        return; // No jump needed
    }

    // For large jumps, we can process in blocks to improve cache efficiency
    const uint64_t BLOCK_SIZE = 1024;

    if (n < BLOCK_SIZE) {
        // For small jumps, just iterate
        for (uint64_t i = 0; i < n; i++) {
            step_once();
        }
    } else {
        // For large jumps, process in blocks
        uint64_t full_blocks = n / BLOCK_SIZE;
        uint64_t remainder = n % BLOCK_SIZE;

        // Process full blocks
        for (uint64_t block = 0; block < full_blocks; block++) {
            for (uint64_t i = 0; i < BLOCK_SIZE; i++) {
                step_once();
            }
        }

        // Process remainder
        for (uint64_t i = 0; i < remainder; i++) {
            step_once();
        }
    }
}

size_t QuadraticIrrational::size() const {
    return 1;
}

void QuadraticIrrational::fill(double* buffer, size_t count) {
    for (size_t i = 0; i < count; i++) {
        buffer[i] = next();
    }
}

} // namespace qiprng
```

```

---

## File: /Users/biostochastics/Development/GitHub/qiprng/src/crypto_mixer.cpp

### Summary
No declarations found

**Tags**: `cpp`

```cpp
## File: /Users/biostochastics/Development/GitHub/qiprng/src/crypto_mixer.cpp
```cpp
// File: crypto_mixer.cpp
// --------------------------------------------------------------
#include "crypto_mixer.hpp"
#include "prng_utils.hpp" // For getThreadLocalEngine and sodium_initialized extern
#include <cstring>      // For std::memcpy
#include <cmath>        // For std::fmod, std::isnan, std::nextafter, std::floor
#include <limits>       // For std::numeric_limits
#include <algorithm>    // For std::clamp


namespace qiprng {

// Definition for the extern declared in crypto_mixer.hpp (and prng_utils.hpp)
// bool sodium_initialized; // This should be defined in prng_utils.cpp

void CryptoMixer::secure_random(unsigned char* buf, size_t len) {
    if (!buf || len == 0) {
        throw std::invalid_argument("CryptoMixer: Invalid buffer for secure random generation");
    }

    if (has_seed_) {
        // Use deterministic generation when seed is provided
        std::uniform_int_distribution<unsigned int> dist(0, 255);
        for (size_t i = 0; i < len; ++i) {
            buf[i] = static_cast<unsigned char>(dist(det_rng_));
        }
    } else if (qiprng::sodium_initialized) { // Check global flag
        randombytes_buf(buf, len);
    } else {
        // Libsodium not initialized - this is a critical security error
        // Throw an exception instead of using insecure fallback
        throw std::runtime_error("CryptoMixer: Libsodium not initialized, cannot generate secure random bytes. "
                                "Please ensure libsodium is properly initialized before using crypto features.");
    }
}

CryptoMixer::CryptoMixer(bool adhoc_corrections, bool use_tie_breaking,
                         uint64_t seed, bool has_seed)
    : key_(crypto_stream_chacha20_KEYBYTES),
      nonce_(crypto_stream_chacha20_NONCEBYTES),
      adhoc_corrections_(adhoc_corrections),
      use_tie_breaking_(use_tie_breaking),
      initialized_(false),
      seed_(seed),
      has_seed_(has_seed),
      det_rng_(has_seed ? seed : std::random_device{}()) {
    if (!qiprng::sodium_initialized) {
         Rcpp::warning("CryptoMixer: Libsodium not initialized at construction. Crypto mixing may be insecure or fail.");
        // Do not throw here, allow construction but it won't be secure until libsodium is init'd
        // and reseed() is successfully called.
        return; // initialized_ remains false
    }
    try {
        reseed(); // This will set initialized_ to true on success
    } catch (const std::exception& e) {
        Rcpp::warning("CryptoMixer: Failed to initialize crypto mixer during construction: %s. Will remain uninitialized.", e.what());
        initialized_ = false; // Explicitly ensure it's false on failure
    }
}

CryptoMixer::~CryptoMixer() {
    // SecureBuffer's destructor handles zeroing its memory
}

void CryptoMixer::reseed() {
    if (!qiprng::sodium_initialized) {
        initialized_ = false;
        throw std::runtime_error("CryptoMixer: Reseed failed because libsodium is not initialized.");
    }
    try {
        secure_random(key_.data(), key_.size());
        secure_random(nonce_.data(), nonce_.size());
        initialized_ = true;
    } catch (const std::exception& e) {
        initialized_ = false;
        throw std::runtime_error(std::string("CryptoMixer: Reseed failed: ") + e.what());
    }
}

bool CryptoMixer::is_initialized() const {
    return initialized_ && qiprng::sodium_initialized;
}

bool CryptoMixer::mix(unsigned char* data, size_t len) {
    if (!is_initialized()) { // Relies on is_initialized() checking the global sodium_initialized flag
        Rcpp::warning("CryptoMixer: Not initialized or libsodium is not ready, skipping mix operation.");
        // Try to re-initialize on demand if libsodium is now ready
        if (qiprng::sodium_initialized && !initialized_) {
            try {
                reseed();
                 Rcpp::Rcout << "CryptoMixer: Re-initialized successfully during mix()." << std::endl;
            } catch (const std::exception& e) {
                Rcpp::warning("CryptoMixer: Failed to re-initialize during mix: %s. Skipping mix.", e.what());
                return false;
            }
        } else if (!qiprng::sodium_initialized) {
            return false; // Libsodium still not ready
        }
        if(!initialized_) return false; // If reseed failed or wasn't attempted
    }

    if (!data || len == 0) {
        Rcpp::warning("CryptoMixer: Invalid data buffer provided to mix()");
        return false;
    }
    if (len % sizeof(double) != 0) {
        Rcpp::warning("CryptoMixer: Buffer length not aligned to double size");
        return false;
    }

    try {
        size_t num_doubles = len / sizeof(double);
        double* doubles = reinterpret_cast<double*>(data);

        SecureBuffer<unsigned char> random_bytes_buf(len); // Corrected name
        secure_random(random_bytes_buf.data(), len); // Use the member function that checks sodium_initialized

        double prev_val = (num_doubles > 0) ? doubles[0] : 0.0; // Initialize reasonably

        if (!adhoc_corrections_) { // partial modular addition
            for (size_t i = 0; i < num_doubles; i++) {
                 if (i * sizeof(double) + sizeof(uint64_t) > random_bytes_buf.size()) { // Check against random_bytes_buf
                    Rcpp::warning("CryptoMixer: Random buffer access out of bounds in modular addition path.");
                    return false;
                }
                uint64_t crypto_bits_val = 0; // Renamed variable
                std::memcpy(&crypto_bits_val, &random_bytes_buf[i * sizeof(double)], sizeof(uint64_t));
                crypto_bits_val = (crypto_bits_val & MANTISSA_MASK) | ONE_BITS;
                double crypto_uniform;
                std::memcpy(&crypto_uniform, &crypto_bits_val, sizeof(double)); // Safe reinterpret_cast alternative
                crypto_uniform -= 1.0;

                // Simplified normalization to [0,1) range
                double mixed_val = doubles[i] + crypto_uniform;

                // Normalize to [0,1) range using a single approach
                mixed_val = mixed_val - std::floor(mixed_val);
                if (mixed_val < 0.0) mixed_val += 1.0;

                // Safety checks for numeric stability
                if (std::isnan(mixed_val) || std::isinf(mixed_val)) {
                    Rcpp::warning("CryptoMixer: Invalid result in mixing, using fallback value 0.5");
                    mixed_val = 0.5;
                }

                // Ensure result is strictly in [0, 1)
                if (mixed_val >= 1.0) {
                    mixed_val = std::nextafter(1.0, 0.0);
                }

                doubles[i] = mixed_val;

                if (use_tie_breaking_ && i > 0 && doubles[i] == prev_val) {
                     try {
                        // Use smaller epsilon to avoid underflow issues
                        static thread_local std::uniform_real_distribution<double> tiny_dist(-1e-15, 1e-15);
                        double eps = tiny_dist(qiprng::getThreadLocalEngine());
                        double new_val = doubles[i] + eps;
                        // Manual clamp implementation for C++14 compatibility
                        const double min_val = std::nextafter(0.0, 1.0);
                        const double max_val = std::nextafter(1.0, 0.0);
                        if (new_val < min_val) {
                            doubles[i] = min_val;
                        } else if (new_val > max_val) {
                            doubles[i] = max_val;
                        } else {
                            doubles[i] = new_val;
                        }
                     } catch (const std::exception& e) {
                        Rcpp::warning("CryptoMixer: Tie breaking failed in modular path: %s", e.what());
                     }
                }
                prev_val = doubles[i];
            }
        } else { // partial averaging approach
            for (size_t i = 0; i < num_doubles; i++) {
                if (i * sizeof(double) + sizeof(uint64_t) > random_bytes_buf.size()) { // Check against random_bytes_buf
                     Rcpp::warning("CryptoMixer: Random buffer access out of bounds in averaging path.");
                    return false;
                }
                uint64_t random_bits_val = 0; // Renamed variable
                std::memcpy(&random_bits_val, &random_bytes_buf[i * sizeof(double)], sizeof(uint64_t));
                uint64_t bits_val = (random_bits_val & MANTISSA_MASK) | ONE_BITS; // Renamed variable
                double crypto_uniform;
                std::memcpy(&crypto_uniform, &bits_val, sizeof(double)); // Safe reinterpret_cast alternative
                crypto_uniform -= 1.0;

                if (std::isnan(crypto_uniform) || std::isnan(doubles[i])) {
                    Rcpp::warning("CryptoMixer: NaN detected in averaging, using fallback 0.5");
                    doubles[i] = 0.5;
                } else {
                    doubles[i] = 0.5 * doubles[i] + 0.5 * crypto_uniform;
                }

                if (use_tie_breaking_ && i > 0 && doubles[i] == prev_val) {
                    try {
                        static thread_local std::uniform_real_distribution<double> tiny_dist(-1e-14, 1e-14);
                        double eps = tiny_dist(qiprng::getThreadLocalEngine()); // Fully qualify
                        doubles[i] += eps;
                    } catch (const std::exception& e) {
                        Rcpp::warning("CryptoMixer: Tie breaking failed in averaging path: %s", e.what());
                    }
                }
                if (doubles[i] >= 1.0) doubles[i] = std::nextafter(1.0, 0.0); // Clamping after potential tie-breaking
                if (doubles[i] < 0.0)  doubles[i] = std::nextafter(0.0, 1.0);

                prev_val = doubles[i];
            }
        }
        return true;
    } catch (const std::exception& e) {
        Rcpp::warning("CryptoMixer: Unexpected error during mixing: %s", e.what());
        return false;
    }
}

} // namespace qiprng
```

```

---

## File: /Users/biostochastics/Development/GitHub/qiprng/src/multi_qi.cpp

### Summary
No declarations found

**Tags**: `cpp`

```cpp
## File: /Users/biostochastics/Development/GitHub/qiprng/src/multi_qi.cpp
```cpp
// File: multi_qi.cpp
// --------------------------------------------------------------
#include "multi_qi.hpp"
#include <stdexcept> // For std::runtime_error

namespace qiprng {

MultiQI::MultiQI(const std::vector<std::tuple<long, long, long>>& abc_list, int mpfr_prec,
                 uint64_t seed, bool has_seed)
    : idx_(0) {
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
}

double MultiQI::next() {
    // Critical section with robust error handling
    try {
        std::lock_guard<std::mutex> lock(mutex_);

        // Safety check
        if (qis_.empty()) {
            // Empty state fallback - shouldn't happen in normal operation
            return 0.5;
        }

        // Check index bounds
        if (idx_ >= qis_.size()) {
            idx_ = 0; // Reset to valid index
        }

        // Get the current QuadraticIrrational
        QuadraticIrrational* current_qi = qis_[idx_].get();

        // Safety check for null pointer
        if (!current_qi) {
            // Advance index and return fallback value
            idx_ = (idx_ + 1) % qis_.size();
            return 0.5;
        }

        // Get next value and advance index
        double val;
        try {
            val = current_qi->next();
        } catch (...) {
            // If next() throws, use fallback
            val = 0.5;
        }

        // Safely advance the index
        idx_ = (idx_ + 1) % qis_.size();

        return val;
    } catch (...) {
        // Ultimate fallback for any exception
        return 0.5;
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
            qi->jump_ahead(num_full_jumps);
        }
    }

    // Handle the remaining jumps by advancing the next `remaining_jump` QIs one by one
    for (uint64_t i = 0; i < remaining_jump; ++i) {
        size_t current_qi_index = (idx_ + i) % num_qis;
        qis_[current_qi_index]->jump_ahead(1);
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

        // Fill buffer directly with values from the QIs in sequence
        for (size_t i = 0; i < fill_size; i++) {
            try {
                // Check if the QI pointer is valid
                if (qis_[idx_]) {
                    buffer[i] = qis_[idx_]->next();
                } else {
                    buffer[i] = 0.5; // Fallback for null QI
                }
            } catch (...) {
                // If next() throws, use fallback
                buffer[i] = 0.5;
            }

            // Safely advance the index
            idx_ = (idx_ + 1) % qis_.size();
        }
    } catch (...) {
        // Ultimate fallback - fill with constant value
        for (size_t i = 0; i < fill_size; ++i) {
            buffer[i] = 0.5;
        }
    }
}

} // namespace qiprng
```

```

---


<!-- Token Count -->
<!-- Total CodeConCat output tokens (cl100k_base): 53,308 -->
