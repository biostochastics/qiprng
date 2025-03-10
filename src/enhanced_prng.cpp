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
    size_t buffer_pos_;
    std::unique_ptr<CryptoMixer> crypto_mixer;
    size_t sample_count_;
    
    // Added: Store spare normal for Box-Muller optimization
    bool has_spare_normal_;
    double spare_normal_;

    static void log_debug(const char* format, ...) {
        if (Rcpp::Function("getOption")("qiprng.debug")) {
            va_list args;
            va_start(args, format);
            REprintf("[qiprng] ");
            Rprintf(format, args);
            REprintf("\n");
            va_end(args);
        }
    }

    // Internal method for raw uniform values without distribution transforms
    double nextUniformRaw() {
        if (buffer_pos_ >= buffer_.size()) {
            fill_buffer();
        }
        return buffer_[buffer_pos_++];
    }

    void fill_buffer() {
        // Fill buffer with raw values from quadratic irrational
        for (size_t i = 0; i < buffer_.size(); ++i) {
            buffer_[i] = qi_.next();
            
            // Ensure values are strictly in (0,1)
            if (buffer_[i] <= 0.0 || buffer_[i] >= 1.0) {
                if (config_.debug) {
                    log_debug("Warning: Generated value %g outside (0,1), adjusting\n", buffer_[i]);
                }
                // Use a small epsilon to avoid extreme values
                const double epsilon = 1e-12;  // Smaller epsilon for better precision
                buffer_[i] = std::max(epsilon, std::min(1.0 - epsilon, buffer_[i]));
            }
        }
        
        // Apply cryptographic mixing if enabled
        if (crypto_mixer) {
            // Convert doubles to bytes for mixing
            std::vector<unsigned char> bytes(buffer_.size() * sizeof(double));
            std::memcpy(bytes.data(), buffer_.data(), bytes.size());
            
            // Mix the bytes
            crypto_mixer->mix(bytes.data(), bytes.size());
            
            // Convert back to doubles
            std::memcpy(buffer_.data(), bytes.data(), bytes.size());
            
            // Re-ensure values are in (0,1) after mixing
            for (size_t i = 0; i < buffer_.size(); ++i) {
                const double epsilon = 1e-12;  // Smaller epsilon for better precision
                buffer_[i] = std::max(epsilon, std::min(1.0 - epsilon, buffer_[i]));
            }
        }
        
        buffer_pos_ = 0;
        
        // Check if we need to reseed
        sample_count_ += buffer_.size();
        if (config_.reseed_interval > 0 && sample_count_ >= config_.reseed_interval) {
            reseed();
        }
    }

    double transform_normal(double u1, double u2) {
        // Ensure u1 and u2 are strictly in (0,1)
        const double epsilon = 1e-6;
        u1 = std::max(epsilon, std::min(1.0 - epsilon, u1));
        u2 = std::max(epsilon, std::min(1.0 - epsilon, u2));
        
        // Box-Muller transform
        double z = std::sqrt(-2.0 * std::log(u1)) * std::cos(2.0 * M_PI * u2);
        
        // Scale and shift to desired mean and standard deviation
        double result = config_.normal_mean + config_.normal_sd * z;
        
        if (config_.debug) {
            log_debug("Normal: u1=%g, u2=%g, z=%g, mean=%g, sd=%g, result=%g\n",
                    u1, u2, z, config_.normal_mean, config_.normal_sd, result);
        }
        
        return result;
    }

    double transform_normal_pair(double u1, double u2, double& second) {
        // Ensure u1 and u2 are strictly in (0,1)
        const double epsilon = 1e-6;
        u1 = std::max(epsilon, std::min(1.0 - epsilon, u1));
        u2 = std::max(epsilon, std::min(1.0 - epsilon, u2));
        
        // Box-Muller transform
        double z1 = std::sqrt(-2.0 * std::log(u1)) * std::cos(2.0 * M_PI * u2);
        double z2 = std::sqrt(-2.0 * std::log(u1)) * std::sin(2.0 * M_PI * u2);
        
        // Scale and shift to desired mean and standard deviation
        double result1 = config_.normal_mean + config_.normal_sd * z1;
        double result2 = config_.normal_mean + config_.normal_sd * z2;
        
        // Check for valid results
        if (!std::isfinite(result1)) {
            result1 = config_.normal_mean;  // Fallback to mean
        }
        
        if (!std::isfinite(result2)) {
            result2 = config_.normal_mean;  // Fallback to mean
        }
        
        second = result2;
        return result1;
    }

    double transform_exponential() {
        double u = nextUniformRaw();  // Use raw uniform values
        return -std::log(1.0 - u) / config_.exponential_lambda;
    }

    double transform_uniform_range() {
        return config_.range_min + nextUniformRaw() * (config_.range_max - config_.range_min);
    }

    // Public interface uses distribution transforms
    double next() {
        switch (config_.distribution) {
            case PRNGConfig::UNIFORM_01:
                return nextUniformRaw();
            case PRNGConfig::UNIFORM_RANGE:
                return config_.range_min + nextUniformRaw() * (config_.range_max - config_.range_min);
            case PRNGConfig::NORMAL: {
                if (has_spare_normal_) {
                    has_spare_normal_ = false;
                    return spare_normal_;
                }
                
                // Box-Muller transform using both variates
                double u1, u2;
                do {
                    u1 = nextUniformRaw();
                    u2 = nextUniformRaw();
                } while (u1 <= 0.0);  // Ensure u1 is positive for log
                
                const double two_pi = 2.0 * M_PI;
                double r = std::sqrt(-2.0 * std::log(u1));
                double theta = two_pi * u2;
                
                // Generate both normal variates
                double z1 = r * std::cos(theta);
                double z2 = r * std::sin(theta);
                
                // Scale and shift both values
                z1 = config_.normal_mean + config_.normal_sd * z1;
                z2 = config_.normal_mean + config_.normal_sd * z2;
                
                // Save second normal for next call
                spare_normal_ = z2;
                has_spare_normal_ = true;
                
                return z1;
            }
            case PRNGConfig::EXPONENTIAL: {
                double u;
                do {
                    u = nextUniformRaw();
                } while (u <= 0.0);  // Ensure u is positive for log
                
                return -std::log(u) / config_.exponential_lambda;
            }
            default:
                throw std::runtime_error("Invalid distribution type");
        }
    }

    static void validate_config(const PRNGConfig& cfg) {
        // MPFR precision
        if (cfg.mpfr_precision < 2 || cfg.mpfr_precision > 10000) {
            throw std::runtime_error("Invalid MPFR precision");
        }

        // Buffer size
        if (cfg.buffer_size <= 0) {
            throw std::runtime_error("Invalid buffer size");
        }

        // Quadratic parameters
        if (cfg.a <= 0) {
            throw std::runtime_error("Parameter 'a' must be positive");
        }
        if (cfg.c >= 0) {
            throw std::runtime_error("Parameter 'c' must be negative");
        }
        if (!has_positive_discriminant(cfg.a, cfg.b, cfg.c)) {
            throw std::runtime_error("Invalid quadratic parameters: discriminant must be positive");
        }

        // Distribution parameters
        if (cfg.distribution == PRNGConfig::UNIFORM_RANGE) {
            if (cfg.range_max <= cfg.range_min) {
                throw std::runtime_error("Invalid range: max must be greater than min");
            }
        } else if (cfg.distribution == PRNGConfig::NORMAL) {
            if (cfg.normal_sd <= 0) {
                throw std::runtime_error("Normal distribution standard deviation must be positive");
            }
        } else if (cfg.distribution == PRNGConfig::EXPONENTIAL) {
            if (cfg.exponential_lambda <= 0) {
                throw std::runtime_error("Exponential distribution lambda must be positive");
            }
        }
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
          buffer_pos_(config.buffer_size),  // Force initial fill
          sample_count_(0),
          has_spare_normal_(false),
          spare_normal_(0.0) {
        
        validate_config(config);
        
        if (config.use_crypto_mixing) {
            crypto_mixer = std::make_unique<CryptoMixer>();
        }
        
        // Initial buffer fill
        fill_buffer();
    }

    void updateConfig(const PRNGConfig& new_cfg) {
        validate_config(new_cfg);
        
        // Update core parameters if they changed
        if (new_cfg.a != config_.a || new_cfg.b != config_.b || 
            new_cfg.c != config_.c || new_cfg.mpfr_precision != config_.mpfr_precision) {
            qi_ = QuadraticIrrational(new_cfg.a, new_cfg.b, new_cfg.c, new_cfg.mpfr_precision);
        }
        
        // Update buffer size if needed
        if (new_cfg.buffer_size != config_.buffer_size) {
            buffer_.resize(new_cfg.buffer_size);
            buffer_pos_ = buffer_.size();  // Force refill
        }
        
        // Update crypto mixing
        if (new_cfg.use_crypto_mixing != config_.use_crypto_mixing) {
            if (new_cfg.use_crypto_mixing) {
                crypto_mixer = std::make_unique<CryptoMixer>();
            } else {
                crypto_mixer.reset();
            }
        }
        
        config_ = new_cfg;
    }

    void reseed() {
        // Reset the quadratic irrational sequence
        qi_ = QuadraticIrrational(config_.a, config_.b, config_.c, config_.mpfr_precision);
        
        // Reset the crypto mixer if used
        if (crypto_mixer) {
            crypto_mixer->reseed();
        }
        
        // Force buffer refill
        buffer_pos_ = buffer_.size();
        sample_count_ = 0;
    }

    Rcpp::NumericVector generate(int n) {
        if (n <= 0) {
            throw std::runtime_error("Invalid n (must be positive).");
        }

        Rcpp::NumericVector result(n);
        
        for (int i = 0; i < n; i++) {
            // Check if we need to reseed
            if (config_.reseed_interval > 0 && sample_count_ >= config_.reseed_interval) {
                reseed();
            }
            
            // Generate value based on distribution
            switch (config_.distribution) {
                case PRNGConfig::UNIFORM_01:
                    result[i] = nextUniformRaw();
                    break;
                case PRNGConfig::UNIFORM_RANGE:
                    result[i] = transform_uniform_range();
                    break;
                case PRNGConfig::NORMAL:
                    result[i] = transform_normal(nextUniformRaw(), nextUniformRaw());
                    break;
                case PRNGConfig::EXPONENTIAL:
                    result[i] = transform_exponential();
                    break;
                default:
                    throw std::runtime_error("Invalid distribution type");
            }
            
            sample_count_++;
        }
        
        return result;
    }

    void parseConfig(const Rcpp::List& config) {
        // Set defaults first
        config_.a = PRNGDefaults::a;
        config_.b = PRNGDefaults::b;
        config_.c = PRNGDefaults::c;
        config_.buffer_size = PRNGDefaults::buffer_size;
        config_.mpfr_precision = PRNGDefaults::mpfr_precision;
        config_.normal_mean = PRNGDefaults::normal_mean;
        config_.normal_sd = PRNGDefaults::normal_sd;
        config_.range_min = PRNGDefaults::range_min;
        config_.range_max = PRNGDefaults::range_max;
        config_.exponential_lambda = PRNGDefaults::exponential_lambda;
        config_.use_crypto_mixing = false;
        config_.reseed_interval = 0;  // 0 means no automatic reseeding
        config_.debug = false;
        
        // Parse distribution
        if (config.containsElementNamed("distribution")) {
            std::string dist_str = Rcpp::as<std::string>(config["distribution"]);
            if (dist_str == "uniform_01") {
                config_.distribution = PRNGConfig::UNIFORM_01;
            } else if (dist_str == "uniform_range") {
                config_.distribution = PRNGConfig::UNIFORM_RANGE;
            } else if (dist_str == "normal") {
                config_.distribution = PRNGConfig::NORMAL;
            } else if (dist_str == "exponential") {
                config_.distribution = PRNGConfig::EXPONENTIAL;
            } else {
                throw std::invalid_argument("Unknown distribution: " + dist_str);
            }
        }

        // Debug mode flag
        if (config.containsElementNamed("debug")) {
            config_.debug = Rcpp::as<bool>(config["debug"]);
        }
        
        // Parse normal distribution parameters
        if (config.containsElementNamed("normal_mean")) {
            double mean = Rcpp::as<double>(config["normal_mean"]);
            if (!std::isfinite(mean)) {
                throw std::invalid_argument("normal_mean must be a finite number");
            }
            config_.normal_mean = mean;
            if (config_.debug) {
                log_debug("Setting normal_mean to %g\n", config_.normal_mean);
            }
        }
        
        if (config.containsElementNamed("normal_sd")) {
            double sd = Rcpp::as<double>(config["normal_sd"]);
            if (!std::isfinite(sd) || sd <= 0) {
                throw std::invalid_argument("normal_sd must be a positive finite number");
            }
            config_.normal_sd = sd;
            if (config_.debug) {
                log_debug("Setting normal_sd to %g\n", config_.normal_sd);
            }
        }
        
        // Parse uniform range parameters
        if (config.containsElementNamed("range_min")) {
            double min = Rcpp::as<double>(config["range_min"]);
            if (!std::isfinite(min)) {
                throw std::invalid_argument("range_min must be a finite number");
            }
            config_.range_min = min;
        }
        
        if (config.containsElementNamed("range_max")) {
            double max = Rcpp::as<double>(config["range_max"]);
            if (!std::isfinite(max)) {
                throw std::invalid_argument("range_max must be a finite number");
            }
            if (max <= config_.range_min) {
                throw std::invalid_argument("range_max must be greater than range_min");
            }
            config_.range_max = max;
        }
        
        // Parse exponential distribution parameters
        if (config.containsElementNamed("exponential_lambda")) {
            double lambda = Rcpp::as<double>(config["exponential_lambda"]);
            if (!std::isfinite(lambda) || lambda <= 0) {
                throw std::invalid_argument("exponential_lambda must be a positive finite number");
            }
            config_.exponential_lambda = lambda;
        }
        
        // Parse PRNG parameters
        if (config.containsElementNamed("a")) {
            config_.a = Rcpp::as<long>(config["a"]);
        }
        
        if (config.containsElementNamed("b")) {
            config_.b = Rcpp::as<long>(config["b"]);
        }
        
        if (config.containsElementNamed("c")) {
            config_.c = Rcpp::as<long>(config["c"]);
        }
        
        // Parse buffer size
        if (config.containsElementNamed("buffer_size")) {
            size_t buffer_size = Rcpp::as<size_t>(config["buffer_size"]);
            if (buffer_size == 0) {
                throw std::invalid_argument("buffer_size must be positive");
            }
            config_.buffer_size = buffer_size;
        }
        
        // Parse MPFR precision
        if (config.containsElementNamed("mpfr_precision")) {
            mpfr_prec_t prec = Rcpp::as<mpfr_prec_t>(config["mpfr_precision"]);
            if (prec < MPFR_PREC_MIN || prec > MPFR_PREC_MAX) {
                throw std::invalid_argument("mpfr_precision must be between MPFR_PREC_MIN and MPFR_PREC_MAX");
            }
            config_.mpfr_precision = prec;
        }
        
        // Parse crypto mixing flag
        if (config.containsElementNamed("use_crypto_mixing")) {
            config_.use_crypto_mixing = Rcpp::as<bool>(config["use_crypto_mixing"]);
        }
        
        // Parse reseed interval
        if (config.containsElementNamed("reseed_interval")) {
            config_.reseed_interval = Rcpp::as<unsigned long>(config["reseed_interval"]);
        }
        
        // Print config if debug mode is enabled
        if (config_.debug) {
            log_debug("Configuration:\n");
            log_debug("  a = %ld\n", config_.a);
            log_debug("  b = %ld\n", config_.b);
            log_debug("  c = %ld\n", config_.c);
            log_debug("  buffer_size = %zu\n", config_.buffer_size);
            log_debug("  mpfr_precision = %ld\n", (long)config_.mpfr_precision);
            log_debug("  distribution = %d\n", static_cast<int>(config_.distribution));
            log_debug("  normal_mean = %g\n", config_.normal_mean);
            log_debug("  normal_sd = %g\n", config_.normal_sd);
            log_debug("  range_min = %g\n", config_.range_min);
            log_debug("  range_max = %g\n", config_.range_max);
            log_debug("  exponential_lambda = %g\n", config_.exponential_lambda);
            log_debug("  use_crypto_mixing = %d\n", config_.use_crypto_mixing);
            log_debug("  reseed_interval = %zu\n", config_.reseed_interval);
        }
    }

    // Get config (for internal use only)
    const PRNGConfig& getConfig() const { return config_; }
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
        else throw std::runtime_error("Unknown distribution type");
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
    PRNGConfig config = parse_r_config(cfg);
    g_prng = std::make_unique<EnhancedPRNG>(config);
}

// [[Rcpp::export(.updatePRNG_)]]
void updatePRNG_(Rcpp::List cfg) {
    std::lock_guard<std::mutex> lock(g_prng_mutex);
    if (!g_prng) {
        throw std::runtime_error("PRNG not initialized");
    }
    PRNGConfig config = parse_r_config(cfg);
    g_prng = std::make_unique<EnhancedPRNG>(config);
}

// [[Rcpp::export(.generatePRNG_)]]
Rcpp::NumericVector generatePRNG_(int n) {
    std::lock_guard<std::mutex> lock(g_prng_mutex);
    if (!g_prng) {
        Rcpp::stop("PRNG not created yet. Call createPRNG(...) first.");
    }
    return g_prng->generate(n);
}

// [[Rcpp::export(.reseedPRNG_)]]
void reseedPRNG_() {
    std::lock_guard<std::mutex> lock(g_prng_mutex);
    if (!g_prng) {
        Rcpp::stop("PRNG not created yet. Call createPRNG(...) first.");
    }
    g_prng->reseed();
}

// [[Rcpp::export(.cleanup_prng_)]]
void cleanup_prng_() {
    std::lock_guard<std::mutex> lock(g_prng_mutex);
    g_prng.reset();
}