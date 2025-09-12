// File: enhanced_prng_refactored.hpp
// --------------------------------------------------------------
// Refactored modular EnhancedPRNG implementation
// Part of Task #21: Refactor EnhancedPRNG for Better Modularity
#ifndef QIPRNG_ENHANCED_PRNG_REFACTORED_HPP
#define QIPRNG_ENHANCED_PRNG_REFACTORED_HPP

#include <Rcpp.h>

#include <atomic>
#include <memory>
#include <mutex>
#include <string>
#include <vector>

#include "buffer_manager.hpp"
#include "crypto_mixer.hpp"
#include "distribution_generator.hpp"
#include "multi_qi.hpp"
#include "prng_config.hpp"

namespace qiprng {

// Adapter to make BufferManager work as UniformSource
class BufferUniformSource : public UniformSource {
   private:
    BufferManager& buffer_;

   public:
    explicit BufferUniformSource(BufferManager& buffer) : buffer_(buffer) {}

    double next() override { return buffer_.next(); }
};

// Statistics tracker component
class StatisticsTracker {
   private:
    std::atomic<uint64_t> sample_count_;
    std::atomic<uint64_t> refill_count_;
    std::atomic<uint64_t> skip_count_;
    mutable std::mutex stats_mutex_;

   public:
    StatisticsTracker() : sample_count_(0), refill_count_(0), skip_count_(0) {}

    void recordSample() { sample_count_++; }
    void recordRefill() { refill_count_++; }
    void recordSkip(uint64_t n) { skip_count_ += n; }

    uint64_t getSampleCount() const { return sample_count_.load(); }
    uint64_t getRefillCount() const { return refill_count_.load(); }
    uint64_t getSkipCount() const { return skip_count_.load(); }

    void reset() {
        sample_count_ = 0;
        refill_count_ = 0;
        skip_count_ = 0;
    }

    void dumpStatistics() const {
        std::lock_guard<std::mutex> lock(stats_mutex_);
        Rcpp::Rcout << "=== PRNG Statistics ===" << std::endl;
        Rcpp::Rcout << "Samples generated: " << sample_count_.load() << std::endl;
        Rcpp::Rcout << "Buffer refills: " << refill_count_.load() << std::endl;
        Rcpp::Rcout << "Values skipped: " << skip_count_.load() << std::endl;
    }
};

// Configuration manager component
class ConfigurationManager {
   private:
    PRNGConfig config_;
    mutable std::mutex config_mutex_;

   public:
    explicit ConfigurationManager(const PRNGConfig& config) : config_(config) {}

    PRNGConfig getConfig() const {
        std::lock_guard<std::mutex> lock(config_mutex_);
        return config_;
    }

    void updateConfig(const PRNGConfig& new_config) {
        std::lock_guard<std::mutex> lock(config_mutex_);
        config_ = new_config;
    }

    bool isParallelEnabled() const {
        std::lock_guard<std::mutex> lock(config_mutex_);
        return config_.use_parallel_filling;
    }

    size_t getBufferSize() const {
        std::lock_guard<std::mutex> lock(config_mutex_);
        return config_.buffer_size;
    }

    std::string getDistribution() const {
        std::lock_guard<std::mutex> lock(config_mutex_);
        return config_.distribution;
    }
};

// Main refactored PRNG class using composition
class EnhancedPRNGRefactored {
   private:
    // Core components
    std::unique_ptr<ConfigurationManager> config_manager_;
    std::unique_ptr<MultiQI> multi_qi_;
    std::unique_ptr<CryptoMixer> crypto_mixer_;
    std::unique_ptr<BufferManager> buffer_manager_;
    std::unique_ptr<DistributionGenerator> distribution_;
    std::unique_ptr<BufferUniformSource> uniform_source_;
    std::unique_ptr<StatisticsTracker> stats_;

    // Thread safety
    std::atomic<bool> is_being_destroyed_;
    std::mutex cleanup_mutex_;
    std::once_flag shutdown_once_flag_;

    // Initialize components based on configuration
    void initializeComponents(const PRNGConfig& config,
                              const std::vector<std::tuple<long, long, long>>& abc_list) {
        // Configuration manager
        config_manager_ = std::make_unique<ConfigurationManager>(config);

        // Statistics tracker
        stats_ = std::make_unique<StatisticsTracker>();

        // Multi-QI generator
        multi_qi_ = std::make_unique<MultiQI>(abc_list, config);

        // Crypto mixer (optional)
        if (config.use_entropy_injection) {
            crypto_mixer_ =
                std::make_unique<CryptoMixer>(config.adhoc_corrections, config.use_tie_breaking);
        }

        // Buffer manager
        buffer_manager_ = std::make_unique<BufferManager>(config.buffer_size, multi_qi_.get(),
                                                          crypto_mixer_.get());

        // Set appropriate fill strategy
        if (config.use_parallel_filling) {
            buffer_manager_->setFillStrategy(
                std::make_unique<ParallelFillStrategy>(config.thread_count));
        }
#ifdef _OPENMP
        else if (config.use_openmp) {
            buffer_manager_->setFillStrategy(std::make_unique<OpenMPFillStrategy>());
        }
#endif

        // Uniform source adapter
        uniform_source_ = std::make_unique<BufferUniformSource>(*buffer_manager_);

        // Distribution generator
        distribution_ = DistributionFactory::create(config);

        // Initial buffer fill
        buffer_manager_->refill();
        stats_->recordRefill();
    }

   public:
    EnhancedPRNGRefactored(const PRNGConfig& config,
                           const std::vector<std::tuple<long, long, long>>& abc_list)
        : is_being_destroyed_(false) {
        initializeComponents(config, abc_list);
    }

    ~EnhancedPRNGRefactored() noexcept {
        std::call_once(shutdown_once_flag_, [this]() {
            is_being_destroyed_ = true;
            // Cleanup in reverse order of initialization
            distribution_.reset();
            uniform_source_.reset();
            buffer_manager_.reset();
            crypto_mixer_.reset();
            multi_qi_.reset();
            stats_.reset();
            config_manager_.reset();
        });
    }

    // Main generation interface
    double next() {
        if (is_being_destroyed_) {
            throw std::runtime_error("PRNG is being destroyed");
        }

        double value = distribution_->generate(*uniform_source_);
        stats_->recordSample();
        return value;
    }

    void generate_n(Rcpp::NumericVector& output) {
        size_t n = output.size();
        for (size_t i = 0; i < n; ++i) {
            output[i] = next();
        }
    }

    // Advanced operations
    void skip(uint64_t n) {
        multi_qi_->skip(n);
        buffer_manager_->reset();  // Invalidate buffer after skip
        stats_->recordSkip(n);
    }

    void reseed() {
        multi_qi_->reseed();
        if (crypto_mixer_) {
            crypto_mixer_->reseed();
        }
        buffer_manager_->refill();
        distribution_->reset();
        stats_->recordRefill();
    }

    // Configuration management
    void updateConfig(const PRNGConfig& new_config) {
        config_manager_->updateConfig(new_config);

        // Recreate distribution if needed
        if (new_config.distribution != config_manager_->getDistribution()) {
            distribution_ = DistributionFactory::create(new_config);
        }

        // Update buffer strategy if parallel setting changed
        if (new_config.use_parallel_filling) {
            buffer_manager_->setFillStrategy(
                std::make_unique<ParallelFillStrategy>(new_config.thread_count));
        } else {
            buffer_manager_->setFillStrategy(std::make_unique<SequentialFillStrategy>());
        }
    }

    PRNGConfig getConfig() const { return config_manager_->getConfig(); }

    // Diagnostics
    void dumpStatistics() const { stats_->dumpStatistics(); }

    void dumpConfig() const {
        auto config = config_manager_->getConfig();
        Rcpp::Rcout << "=== PRNG Configuration ===" << std::endl;
        Rcpp::Rcout << "Distribution: " << config.distribution << std::endl;
        Rcpp::Rcout << "Buffer size: " << config.buffer_size << std::endl;
        Rcpp::Rcout << "Parallel filling: " << config.use_parallel_filling << std::endl;
        Rcpp::Rcout << "Thread count: " << config.thread_count << std::endl;
        Rcpp::Rcout << "Crypto mixing: " << config.use_entropy_injection << std::endl;
    }

    // Utility
    size_t getQICount() const { return multi_qi_ ? multi_qi_->getSize() : 0; }

    bool isBeingDestroyed() const { return is_being_destroyed_.load(); }
};

}  // namespace qiprng

#endif  // QIPRNG_ENHANCED_PRNG_REFACTORED_HPP
