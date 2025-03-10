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
// SecureVector for sensitive data handling
template<typename T>
class SecureVector {
private:
    std::vector<T> data;

public:
    SecureVector() = default;
    explicit SecureVector(size_t n) : data(n) {}

    void resize(size_t n) {
        clear();
        data.resize(n);
    }

    void clear() {
        if (!data.empty()) {
            volatile T* ptr = data.data();
            for (size_t i = 0; i < data.size(); ++i) {
                ptr[i] = T(0);
            }
        }
        data.clear();
    }

    ~SecureVector() {
        clear();
    }

    size_t size() const { return data.size(); }
    T& operator[](size_t i) { return data[i]; }
    const T& operator[](size_t i) const { return data[i]; }
};

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

    // Debug flag
    bool debug = false;
};

// Forward declarations
class QuadraticIrrational;

// Standardized discriminant check function
bool has_positive_discriminant(long a, long b, long c) {
    // b^2 - 4ac where c is negative, so we use -c
    return (b * b) > (4 * a * (-c));
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
    std::unique_ptr<MPFRWrapper> temp_;   // Temporary value for calculations

public:
    QuadraticIrrational(long a, long b, long c, mpfr_prec_t prec = PRNGDefaults::mpfr_precision)
        : a_(a), b_(b), c_(c),
          root_(std::make_unique<MPFRWrapper>(prec)),
          value_(std::make_unique<MPFRWrapper>(prec)),
          next_(std::make_unique<MPFRWrapper>(prec)),
          temp_(std::make_unique<MPFRWrapper>(prec)) {
        
        // Calculate D = b² - 4ac
        // For quadratic irrational, we need c to be negative for positive discriminant
        long d = b_ * b_ - 4 * a_ * c_;
        if (d <= 0) {
            throw std::runtime_error("Invalid quadratic parameters: discriminant must be positive");
        }

        // Calculate √D
        mpfr_set_si(*root_->get(), d, MPFR_RNDN);
        mpfr_sqrt(*root_->get(), *root_->get(), MPFR_RNDN);

        // Initialize x₀ = (-b + √D)/(2a)
        mpfr_set_si(*value_->get(), -b_, MPFR_RNDN);
        mpfr_add(*value_->get(), *value_->get(), *root_->get(), MPFR_RNDN);
        mpfr_div_si(*value_->get(), *value_->get(), 2 * a_, MPFR_RNDN);
        mpfr_frac(*value_->get(), *value_->get(), MPFR_RNDN);

        // Initialize next_ and temp_ to 0
        mpfr_set_zero(*next_->get(), 1);
        mpfr_set_zero(*temp_->get(), 1);
    }

    // Allow move operations
    QuadraticIrrational(QuadraticIrrational&&) = default;
    QuadraticIrrational& operator=(QuadraticIrrational&&) = default;

    double next() {
        // Calculate next value: ax² + bx + c (mod 1)
        mpfr_mul(*temp_->get(), *value_->get(), *value_->get(), MPFR_RNDN);  // x²
        mpfr_mul_si(*temp_->get(), *temp_->get(), a_, MPFR_RNDN);         // ax²
        
        mpfr_mul_si(*next_->get(), *value_->get(), b_, MPFR_RNDN);        // bx
        mpfr_add(*next_->get(), *next_->get(), *temp_->get(), MPFR_RNDN);   // ax² + bx
        
        mpfr_add_si(*next_->get(), *next_->get(), c_, MPFR_RNDN);         // ax² + bx + c
        mpfr_frac(*next_->get(), *next_->get(), MPFR_RNDN);               // mod 1
        
        // Update current value
        mpfr_swap(*value_->get(), *next_->get());
        
        // Convert to double and return, with fallback for invalid values
        double result = mpfr_get_d(*value_->get(), MPFR_RNDN);
        if (!std::isfinite(result)) {
            // If we get an invalid value, reset to initial state and return 0.5
            mpfr_set_si(*value_->get(), -b_, MPFR_RNDN);
            mpfr_add(*value_->get(), *value_->get(), *root_->get(), MPFR_RNDN);
            mpfr_div_si(*value_->get(), *value_->get(), 2 * a_, MPFR_RNDN);
            mpfr_frac(*value_->get(), *value_->get(), MPFR_RNDN);
            return 0.5;
        }
        return result;
    }
};

// ---------------------------------------------------------------------
// EnhancedPRNG class
class EnhancedPRNG {
private:
    QuadraticIrrational qi_;
    PRNGConfig config_;
    SecureBuffer<double> buffer_;
    mutable size_t buffer_pos_;  // Mutable since it changes in const methods
    std::unique_ptr<CryptoMixer> crypto_mixer;
    size_t sample_count_;
    bool has_spare_normal_;
    double spare_normal_;

    // Get next raw uniform value
    double nextUniformRaw() const {
        if (buffer_pos_ >= buffer_.size()) {
            const_cast<EnhancedPRNG*>(this)->fill_buffer();  // Mutable operation
        }
        return buffer_[buffer_pos_++];
    }

    // Fill buffer with new random values
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

    // Transform uniform to range
    double transform_uniform_range() const {
        return config_.range_min + nextUniformRaw() * (config_.range_max - config_.range_min);
    }

    // Transform uniform to normal using Box-Muller
    double transform_normal(double u1, double u2) const {
        if (has_spare_normal_) {
            const_cast<EnhancedPRNG*>(this)->has_spare_normal_ = false;
            return config_.normal_mean + config_.normal_sd * spare_normal_;
        }

        // Box-Muller transform
        const double two_pi = 2.0 * M_PI;
        double r = std::sqrt(-2.0 * std::log(u1));
        double theta = two_pi * u2;
        
        // Generate both normal variates
        double z1 = r * std::cos(theta);
        const_cast<EnhancedPRNG*>(this)->spare_normal_ = r * std::sin(theta);
        const_cast<EnhancedPRNG*>(this)->has_spare_normal_ = true;
        
        return config_.normal_mean + config_.normal_sd * z1;
    }

    // Transform uniform to exponential
    double transform_exponential() const {
        double u;
        do {
            u = nextUniformRaw();
        } while (u <= 0.0);  // Ensure u is positive for log
        
        return -std::log(u) / config_.exponential_lambda;
    }

    // Type-safe crypto mixing
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
          buffer_pos_(buffer_.size()),  // Force buffer fill on first use
          sample_count_(0),
          has_spare_normal_(false),
          spare_normal_(0.0) {
        
        if (config.use_crypto_mixing) {
            crypto_mixer = std::make_unique<CryptoMixer>();
        }
    }
    
    // Get current configuration
    const PRNGConfig& getConfig() const {
        return config_;
    }
    
    // Update configuration
    void updateConfig(const PRNGConfig& new_config) {
        // Validate new configuration
        if (!has_positive_discriminant(new_config.a, new_config.b, new_config.c)) {
            throw std::runtime_error("Invalid quadratic parameters: discriminant must be positive");
        }
        
        // Check if core parameters changed
        bool core_changed = (new_config.a != config_.a || 
                           new_config.b != config_.b || 
                           new_config.c != config_.c ||
                           new_config.mpfr_precision != config_.mpfr_precision);
        
        // Update configuration
        config_ = new_config;
        
        // If core parameters changed, recreate quadratic irrational
        if (core_changed) {
            qi_ = QuadraticIrrational(config_.a, config_.b, config_.c, config_.mpfr_precision);
        }
        
        // Update buffer size if needed
        if (buffer_.size() != config_.buffer_size) {
            buffer_.resize(config_.buffer_size);
            buffer_pos_ = buffer_.size();  // Force buffer fill on next use
        }
        
        // Update crypto mixer
        if (config_.use_crypto_mixing) {
            if (!crypto_mixer) {
                crypto_mixer = std::make_unique<CryptoMixer>();
            }
        } else {
            crypto_mixer.reset();
        }
        
        // Reset spare normal
        has_spare_normal_ = false;
    }
    
    // Generate next random number
    double next() {
        // Check if we need to reseed
        if (config_.reseed_interval > 0 && sample_count_ >= config_.reseed_interval) {
            reseed();
        }
        
        // Generate value based on distribution
        double result;
        switch (config_.distribution) {
            case PRNGConfig::UNIFORM_01:
                result = nextUniformRaw();
                break;
            case PRNGConfig::UNIFORM_RANGE:
                result = transform_uniform_range();
                break;
            case PRNGConfig::NORMAL: {
                double u1, u2;
                do {
                    u1 = nextUniformRaw();
                } while (u1 <= 0.0);  // Ensure u1 is positive for log
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
    
    // Reseed the PRNG
    void reseed() {
        if (crypto_mixer) {
            crypto_mixer->reseed();
        }
        sample_count_ = 0;
        buffer_pos_ = buffer_.size();  // Force buffer fill on next use
        has_spare_normal_ = false;     // Reset spare normal
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
    
    // Core parameters
    result["a"] = config.a;
    result["b"] = config.b;
    result["c"] = config.c;
    result["mpfr_precision"] = config.mpfr_precision;
    result["buffer_size"] = config.buffer_size;
    
    // Distribution parameters
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
    
    // Crypto parameters
    result["use_crypto_mixing"] = config.use_crypto_mixing;
    result["reseed_interval"] = config.reseed_interval;
    
    // Debug flag
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