// File: enhanced_prng.cpp
// --------------------------------------------------------------
#include <Rcpp.h>
#include <vector>
#include <memory>
#include <array>
#include <string>
#include <map>
#include <mutex>
#include <functional>
#include <cmath>
#include <stdexcept>
#include <algorithm>
#include <cstring>

// For MPFR high-precision
#include <mpfr.h>

// For crypto operations
#include <sodium.h>

#include <random>
#include <chrono>

#include "prng_common.hpp"

using namespace qiprng;

// ---------------------------------------------------------------------
// PRNGConfig structure with newly added fields
struct PRNGConfig {
    enum Distribution {
        UNIFORM_01,
        UNIFORM_RANGE,
        NORMAL,
        EXPONENTIAL
    };
    
    // Core parameters
    long a = 2;
    long b = 1;
    long c = -1;
    int mpfr_precision = 53;
    size_t buffer_size = 1024;

    // Distribution parameters
    Distribution distribution = UNIFORM_01;
    double range_min = 0.0;
    double range_max = 1.0;
    double normal_mean = 0.0;
    double normal_sd = 1.0;
    double exponential_lambda = 1.0;

    // Crypto parameters
    bool use_crypto_mixing = false;
    unsigned long reseed_interval = 0;

    // Additional: skip the first 'offset' draws
    size_t offset = 0;

    // Debug flag
    bool debug = false;
};

// ---------------------------------------------------------------------
// Standardized discriminant check function (positive discriminant)
bool has_positive_discriminant(long a, long b, long c) {
    // We want b^2 - 4ac > 0
    return (static_cast<long long>(b) * b) > (4LL * a * c);
}

// ---------------------------------------------------------------------
// Square-free check for the discriminant
bool is_square_free(long d) {
    // Negative or zero is automatically not square-free for our usage
    if (d <= 0) return false;

    // Quick check: factor out perfect squares up to sqrt(d)

    for (long factor = 2; factor * factor <= d; ++factor) {
        long sq = factor * factor;
        if (d % sq == 0) {
            return false;
        }
    }
    return true;
}

// Improved CryptoMixer with proper buffer management
class CryptoMixer {
private:
    std::vector<unsigned char> key;
    std::vector<unsigned char> nonce;
    
    void secure_random(unsigned char* buf, size_t len) {
        randombytes_buf(buf, len);
    }

public:
    CryptoMixer() {
        if (sodium_init() < 0) {
            throw std::runtime_error("Failed to initialize libsodium");
        }
        
        // Initialize with secure key and nonce
        key.resize(crypto_stream_chacha20_KEYBYTES);
        nonce.resize(crypto_stream_chacha20_NONCEBYTES);
        reseed();
    }
    
    ~CryptoMixer() {
        // Secure zeroing of sensitive data
        if (!key.empty()) sodium_memzero(key.data(), key.size());
        if (!nonce.empty()) sodium_memzero(nonce.data(), nonce.size());
    }
    
    void reseed() {
        secure_random(key.data(), key.size());
        secure_random(nonce.data(), nonce.size());
    }
    
    bool mix(unsigned char* data, size_t length) {
        // Create a temporary buffer for the keystream
        std::vector<unsigned char> keystream(length);
        
        // Generate keystream
        if (crypto_stream_chacha20(keystream.data(), length, nonce.data(), key.data()) != 0) {
            return false;
        }
        
        // XOR the data with keystream
        for (size_t i = 0; i < length; i++) {
            data[i] ^= keystream[i];
        }
        
        // Secure zeroing of sensitive buffer
        sodium_memzero(keystream.data(), keystream.size());
        return true;
    }
    
    // Delete copy and move operations
    CryptoMixer(const CryptoMixer&) = delete;
    CryptoMixer& operator=(const CryptoMixer&) = delete;
    CryptoMixer(CryptoMixer&&) = delete;
    CryptoMixer& operator=(CryptoMixer&&) = delete;
};

// ---------------------------------------------------------------------
// QuadraticIrrational: MPFR-based class for core recurrence
class QuadraticIrrational {
    long a_, b_, c_;
    std::unique_ptr<MPFRWrapper> root_;   // √D
    std::unique_ptr<MPFRWrapper> value_;  // Current value
    std::unique_ptr<MPFRWrapper> next_;   // Next value
    std::unique_ptr<MPFRWrapper> temp_;   // Temporary

public:
    QuadraticIrrational(long a, long b, long c, mpfr_prec_t prec = PRNGDefaults::mpfr_precision)
        : a_(a), b_(b), c_(c),
          root_(std::make_unique<MPFRWrapper>(prec)),
          value_(std::make_unique<MPFRWrapper>(prec)),
          next_(std::make_unique<MPFRWrapper>(prec)),
          temp_(std::make_unique<MPFRWrapper>(prec)) {
        
        // Calculate D = b² - 4ac
        long long d = static_cast<long long>(b_) * b_ - 4LL * a_ * c_;
        if (d <= 0) {
            throw std::runtime_error("Invalid quadratic parameters: discriminant must be positive");
        }

        // Optionally check if it's square-free:
        // (If you want to forcibly skip non-square-free, do it here.)
        if (!is_square_free(static_cast<long>(d))) {
            throw std::runtime_error("Discriminant is not square-free; reject these parameters if desired.");
        }

        // Calculate √D
        mpfr_set_si(*root_->get(), d, MPFR_RNDN);
        mpfr_sqrt(*root_->get(), *root_->get(), MPFR_RNDN);

        // Initialize x₀ = fractional part of [(-b + √D)/(2a)]
        mpfr_set_si(*value_->get(), -b_, MPFR_RNDN);
        mpfr_add(*value_->get(), *value_->get(), *root_->get(), MPFR_RNDN);
        mpfr_div_si(*value_->get(), *value_->get(), 2 * a_, MPFR_RNDN);

        // fractional part
        mpfr_floor(*temp_->get(), *value_->get());
        mpfr_sub(*value_->get(), *value_->get(), *temp_->get(), MPFR_RNDN);
        if (mpfr_sgn(*value_->get()) < 0) {
            mpfr_add_ui(*value_->get(), *value_->get(), 1, MPFR_RNDN);
        }

        // Initialize next_ and temp_ to 0
        mpfr_set_zero(*next_->get(), 1);
        mpfr_set_zero(*temp_->get(), 1);
    }

    // Move only
    QuadraticIrrational(QuadraticIrrational&&) = default;
    QuadraticIrrational& operator=(QuadraticIrrational&&) = default;

    double next() {
        // x_{n+1} = frac(a*x_n^2 + b*x_n + c)
        mpfr_mul(*temp_->get(), *value_->get(), *value_->get(), MPFR_RNDN); // x_n^2
        mpfr_mul_si(*next_->get(), *temp_->get(), a_, MPFR_RNDN);           // a*x_n^2

        mpfr_mul_si(*temp_->get(), *value_->get(), b_, MPFR_RNDN);          // b*x_n
        mpfr_add(*next_->get(), *next_->get(), *temp_->get(), MPFR_RNDN);

        mpfr_add_si(*next_->get(), *next_->get(), c_, MPFR_RNDN);

        // fractional part
        mpfr_floor(*temp_->get(), *next_->get());
        mpfr_sub(*next_->get(), *next_->get(), *temp_->get(), MPFR_RNDN);
        if (mpfr_sgn(*next_->get()) < 0) {
            mpfr_add_ui(*next_->get(), *next_->get(), 1, MPFR_RNDN);
        }

        mpfr_swap(*value_->get(), *next_->get());
        return mpfr_get_d(*value_->get(), MPFR_RNDN);
    }
};

// ---------------------------------------------------------------------
// EnhancedPRNG class
class EnhancedPRNG {
private:
    QuadraticIrrational qi_;
    PRNGConfig config_;
    SecureBuffer<double> buffer_;
    size_t buffer_pos_;
    std::unique_ptr<CryptoMixer> crypto_mixer;
    size_t sample_count_;
    bool has_spare_normal_;
    double spare_normal_;
    bool offset_applied_;
    size_t skipped_;

    void fill_buffer() {
        for (size_t i = 0; i < buffer_.size(); ++i) {
            buffer_[i] = qi_.next();
        }
        if (config_.use_crypto_mixing && crypto_mixer) {
            if (!mix_buffer()) {
                throw std::runtime_error("Crypto mixing failed");
            }
        }
        buffer_pos_ = 0;
    }

    double nextUniformRaw() {
        if (!offset_applied_) {
            while (skipped_ < config_.offset) {
                qi_.next();
                ++skipped_;
            }
            offset_applied_ = true;
        }

        if (buffer_pos_ >= buffer_.size()) {
            fill_buffer();
        }
        return buffer_[buffer_pos_++];
    }

    double transform_uniform_range() {
        return config_.range_min + nextUniformRaw() * (config_.range_max - config_.range_min);
    }

    double transform_normal(double u1, double u2) {
        if (has_spare_normal_) {
            has_spare_normal_ = false;
            return config_.normal_mean + config_.normal_sd * spare_normal_;
        }
        const double two_pi = 2.0 * M_PI;
        double r = std::sqrt(-2.0 * std::log(u1));
        double theta = two_pi * u2;
        double z1 = r * std::cos(theta);
        spare_normal_ = r * std::sin(theta);
        has_spare_normal_ = true;
        return config_.normal_mean + config_.normal_sd * z1;
    }

    double transform_exponential() {
        double u;
        do {
            u = nextUniformRaw();
        } while (u <= 0.0);
        return -std::log(u) / config_.exponential_lambda;
    }

    bool mix_buffer() {
        if (!crypto_mixer) return true;
        SecureBuffer<unsigned char> temp(buffer_.size() * sizeof(double));
        std::memcpy(temp.data(), buffer_.data(), temp.size());
        if (!crypto_mixer->mix(temp.data(), temp.size())) {
            return false;
        }
        std::memcpy(buffer_.data(), temp.data(), temp.size());
        return true;
    }

public:
    EnhancedPRNG(const PRNGConfig& config) 
        : qi_(config.a, config.b, config.c, config.mpfr_precision),
          config_(config),
          buffer_(config.buffer_size),
          buffer_pos_(config.buffer_size),
          sample_count_(0),
          has_spare_normal_(false),
          spare_normal_(0.0),
          offset_applied_(false),
          skipped_(0) {
        
        if (config_.use_crypto_mixing) {
            crypto_mixer = std::make_unique<CryptoMixer>();
        }
    }

    const PRNGConfig& getConfig() const {
        return config_;
    }

    void updateConfig(const PRNGConfig& new_config) {
        if (!has_positive_discriminant(new_config.a, new_config.b, new_config.c)) {
            throw std::runtime_error("Invalid quadratic parameters: discriminant must be positive");
        }
        
        long long new_d = static_cast<long long>(new_config.b) * new_config.b
                          - 4LL * new_config.a * new_config.c;
        if (new_d <= 0) {
            throw std::runtime_error("Discriminant is not positive, can't update config.");
        }
        if (!is_square_free(static_cast<long>(new_d))) {
            throw std::runtime_error("Discriminant is not square-free; rejecting updated config.");
        }

        bool core_changed = (new_config.a != config_.a ||
                           new_config.b != config_.b ||
                           new_config.c != config_.c ||
                           new_config.mpfr_precision != config_.mpfr_precision);

        config_ = new_config;

        if (core_changed) {
            qi_ = QuadraticIrrational(config_.a, config_.b, config_.c, config_.mpfr_precision);
        }

        if (buffer_.size() != config_.buffer_size) {
            buffer_.resize(config_.buffer_size);
            buffer_pos_ = buffer_.size();
        }

        offset_applied_ = false;
        skipped_ = 0;

        if (config_.use_crypto_mixing) {
            if (!crypto_mixer) {
                crypto_mixer = std::make_unique<CryptoMixer>();
            }
        } else {
            crypto_mixer.reset();
        }

        has_spare_normal_ = false;
    }

    double next() {
        if (config_.reseed_interval > 0 && sample_count_ >= config_.reseed_interval) {
            reseed();
        }
        
        double result = 0.0;
        switch (config_.distribution) {
            case PRNGConfig::UNIFORM_01:
                result = nextUniformRaw();
                break;
            case PRNGConfig::UNIFORM_RANGE:
                result = transform_uniform_range();
                break;
            case PRNGConfig::NORMAL: {
                double u1 = 0.0, u2 = 0.0;
                do {
                    u1 = nextUniformRaw();
                } while (u1 <= 0.0);
                u2 = nextUniformRaw();
                result = transform_normal(u1, u2);
                break;
            }
            case PRNGConfig::EXPONENTIAL:
                result = transform_exponential();
                break;
            default:
                throw std::runtime_error("Invalid distribution type");
        }

        sample_count_++;
        return result;
    }

    void reseed() {
        if (crypto_mixer) {
            crypto_mixer->reseed();
        }
        sample_count_ = 0;
        buffer_pos_ = buffer_.size();
        has_spare_normal_ = false;
        offset_applied_ = false;
        skipped_ = 0;
    }
};

// ---------------------------------------------------------------------
// Global PRNG instance with thread safety
static std::mutex g_prng_mutex;
static std::unique_ptr<EnhancedPRNG> g_prng;

// Helper function to parse R config list into PRNGConfig
PRNGConfig parse_r_config(const Rcpp::List& cfg) {
    PRNGConfig config;
    
    // Core parameters
    if (cfg.containsElementNamed("a")) config.a = Rcpp::as<long>(cfg["a"]);
    if (cfg.containsElementNamed("b")) config.b = Rcpp::as<long>(cfg["b"]);
    if (cfg.containsElementNamed("c")) config.c = Rcpp::as<long>(cfg["c"]);
    if (cfg.containsElementNamed("mpfr_precision")) config.mpfr_precision = Rcpp::as<int>(cfg["mpfr_precision"]);
    if (cfg.containsElementNamed("buffer_size")) config.buffer_size = Rcpp::as<size_t>(cfg["buffer_size"]);
    
    // Distribution parameters
    if (cfg.containsElementNamed("distribution")) {
        std::string dist = Rcpp::as<std::string>(cfg["distribution"]);
        if (dist == "uniform_01") config.distribution = PRNGConfig::UNIFORM_01;
        else if (dist == "uniform_range") config.distribution = PRNGConfig::UNIFORM_RANGE;
        else if (dist == "normal") config.distribution = PRNGConfig::NORMAL;
        else if (dist == "exponential") config.distribution = PRNGConfig::EXPONENTIAL;
        else throw std::runtime_error("Invalid distribution type");
    }
    
    if (cfg.containsElementNamed("range_min")) config.range_min = Rcpp::as<double>(cfg["range_min"]);
    if (cfg.containsElementNamed("range_max")) config.range_max = Rcpp::as<double>(cfg["range_max"]);
    if (cfg.containsElementNamed("normal_mean")) config.normal_mean = Rcpp::as<double>(cfg["normal_mean"]);
    if (cfg.containsElementNamed("normal_sd")) config.normal_sd = Rcpp::as<double>(cfg["normal_sd"]);
    if (cfg.containsElementNamed("exponential_lambda")) config.exponential_lambda = Rcpp::as<double>(cfg["exponential_lambda"]);
    
    // Crypto parameters
    if (cfg.containsElementNamed("use_crypto_mixing")) config.use_crypto_mixing = Rcpp::as<bool>(cfg["use_crypto_mixing"]);
    if (cfg.containsElementNamed("reseed_interval")) config.reseed_interval = Rcpp::as<unsigned long>(cfg["reseed_interval"]);
    
    // Additional offset parameter
    if (cfg.containsElementNamed("offset")) config.offset = Rcpp::as<size_t>(cfg["offset"]);

    // Debug flag
    if (cfg.containsElementNamed("debug")) config.debug = Rcpp::as<bool>(cfg["debug"]);
    
    return config;
}

// [[Rcpp::export(.createPRNG_)]]
void createPRNG_(Rcpp::List cfg) {
    std::lock_guard<std::mutex> lock(g_prng_mutex);
    g_prng = std::make_unique<EnhancedPRNG>(parse_r_config(cfg));
}

// [[Rcpp::export(.updatePRNG_)]]
void updatePRNG_(Rcpp::List cfg) {
    std::lock_guard<std::mutex> lock(g_prng_mutex);
    if (!g_prng) throw std::runtime_error("PRNG not initialized");
    g_prng->updateConfig(parse_r_config(cfg));
}

// [[Rcpp::export(.getPRNGConfig_)]]
Rcpp::List getPRNGConfig_() {
    std::lock_guard<std::mutex> lock(g_prng_mutex);
    if (!g_prng) throw std::runtime_error("PRNG not initialized");
    
    const PRNGConfig& config = g_prng->getConfig();
    Rcpp::List result;
    
    // Core
    result["a"] = config.a;
    result["b"] = config.b;
    result["c"] = config.c;
    result["mpfr_precision"] = config.mpfr_precision;
    result["buffer_size"] = config.buffer_size;
    
    // Dist
    switch (config.distribution) {
        case PRNGConfig::UNIFORM_01:
            result["distribution"] = "uniform_01";
            break;
        case PRNGConfig::UNIFORM_RANGE:
            result["distribution"] = "uniform_range";
            break;
        case PRNGConfig::NORMAL:
            result["distribution"] = "normal";
            break;
        case PRNGConfig::EXPONENTIAL:
            result["distribution"] = "exponential";
            break;
    }
    result["range_min"] = config.range_min;
    result["range_max"] = config.range_max;
    result["normal_mean"] = config.normal_mean;
    result["normal_sd"] = config.normal_sd;
    result["exponential_lambda"] = config.exponential_lambda;
    
    // Crypto
    result["use_crypto_mixing"] = config.use_crypto_mixing;
    result["reseed_interval"] = config.reseed_interval;

    // Additional offset
    result["offset"] = config.offset;

    // Debug
    result["debug"] = config.debug;
    
    return result;
}

// [[Rcpp::export(.generatePRNG_)]]
Rcpp::NumericVector generatePRNG_(int n) {
    std::lock_guard<std::mutex> lock(g_prng_mutex);
    if (!g_prng) throw std::runtime_error("PRNG not initialized");
    
    Rcpp::NumericVector result(n);
    for (int i = 0; i < n; i++) {
        result[i] = g_prng->next();
    }
    return result;
}

// [[Rcpp::export(.reseedPRNG_)]]
void reseedPRNG_() {
    std::lock_guard<std::mutex> lock(g_prng_mutex);
    if (!g_prng) throw std::runtime_error("PRNG not initialized");
    g_prng->reseed();
}

// [[Rcpp::export(.cleanup_prng_)]]
void cleanup_prng_() {
    std::lock_guard<std::mutex> lock(g_prng_mutex);
    g_prng.reset();
}