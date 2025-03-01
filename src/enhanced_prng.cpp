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

// For SHA-256 (OpenSSL) or AES
#include <openssl/evp.h>
#include <openssl/sha.h>

// libsodium for randombytes
#include <sodium.h>

// For AES CTR
#include <openssl/aes.h>
#include <openssl/evp.h>

#include <random>
#include <chrono>
#include <fstream>

#include "prng_common.hpp"

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
    bool use_hardware_rng = false;
    bool use_threading = false;
    unsigned long reseed_interval = 0;
    std::string hash_algorithm = "sha256";
    int key_size = 256;
};

// Forward declarations
class QuadraticIrrational;

// ---------------------------------------------------------------------
// Minimal hardware RNG detection code
class HardwareRNG {
private:
    std::ifstream urandom;
    bool is_open;

public:
    HardwareRNG() : is_open(false) {
        #if defined(__linux__) || defined(__APPLE__)
        urandom.open("/dev/urandom", std::ios::in | std::ios::binary);
        is_open = urandom.good();
        #endif
    }

    ~HardwareRNG() {
        if (is_open) {
            urandom.close();
        }
    }

    bool available() const {
        return is_open;
    }

    bool generate(uint64_t* value) {
        if (!is_open) {
            return false;
        }

        unsigned char buffer[8];
        urandom.read(reinterpret_cast<char*>(buffer), sizeof(buffer));
        if (!urandom.good()) {
            return false;
        }

        *value = 0;
        for (int i = 0; i < 8; i++) {
            *value |= static_cast<uint64_t>(buffer[i]) << (i * 8);
        }
        return true;
    }
};

// Standardized discriminant check function
bool has_positive_discriminant(long a, long b, long c) {
    // b^2 - 4ac where c is negative, so we use -c
    return (b * b) > (4 * a * (-c));
}

// Improved CryptoMixer with proper buffer management
class CryptoMixer {
private:
    EVP_CIPHER_CTX* ctx;
    std::vector<unsigned char> key;
    std::vector<unsigned char> iv;
    
    void secure_random(unsigned char* buf, size_t len) {
        if (RAND_bytes(buf, len) != 1) {
            throw std::runtime_error("Failed to generate secure random bytes");
        }
    }

public:
    CryptoMixer() : ctx(EVP_CIPHER_CTX_new()) {
        if (!ctx) {
            throw std::runtime_error("Failed to create cipher context");
        }
        
        // Initialize with secure key and IV
        key.resize(32);  // AES-256 key size
        iv.resize(16);   // AES block size
        reseed();
    }
    
    ~CryptoMixer() {
        if (ctx) {
            EVP_CIPHER_CTX_free(ctx);
        }
        // Secure zeroing of sensitive data
        if (!key.empty()) sodium_memzero(key.data(), key.size());
        if (!iv.empty()) sodium_memzero(iv.data(), iv.size());
    }
    
    void reseed() {
        secure_random(key.data(), key.size());
        secure_random(iv.data(), iv.size());
        
        if (EVP_EncryptInit_ex(ctx, EVP_aes_256_ctr(), nullptr, key.data(), iv.data()) != 1) {
            throw std::runtime_error("Failed to initialize cipher");
        }
    }
    
    bool mix(unsigned char* data, size_t length) {
        // Allocate output buffer with proper padding
        std::vector<unsigned char> outbuf(length + EVP_MAX_BLOCK_LENGTH);
        int outlen = 0, finalLen = 0;
        
        // Perform encryption
        if (!EVP_EncryptUpdate(ctx, outbuf.data(), &outlen, data, length) ||
            !EVP_EncryptFinal_ex(ctx, outbuf.data() + outlen, &finalLen)) {
            return false;
        }
        
        // Copy back with bounds checking
        std::memcpy(data, outbuf.data(), std::min(length, static_cast<size_t>(outlen + finalLen)));
        
        // Secure zeroing of sensitive buffer
        sodium_memzero(outbuf.data(), outbuf.size());
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
private:
    MPFRWrapper disc_;    // Discriminant
    MPFRWrapper root_;    // Square root of discriminant
    MPFRWrapper value_;   // Current value
    MPFRWrapper next_;    // Next value
    MPFRWrapper temp_;    // Temporary value for calculations
    
    const long a_;
    const long b_;
    const long c_;
    
public:
    QuadraticIrrational(long a, long b, long c, mpfr_prec_t prec)
        : disc_(prec), root_(prec), value_(prec), next_(prec), temp_(prec),
          a_(a), b_(b), c_(c) {
        // Initialize discriminant
        mpfr_set_si(disc_.get(), b * b - 4 * a * (-c), MPFR_RNDN);
        if (mpfr_cmp_ui(disc_.get(), 0) <= 0) {
            throw std::runtime_error("Invalid parameters: discriminant must be positive");
        }
        
        // Calculate square root
        mpfr_sqrt(root_.get(), disc_.get(), MPFR_RNDN);
        
        // Set initial value to irrational number
        mpfr_set_si(value_.get(), b, MPFR_RNDN);
        mpfr_add(value_.get(), value_.get(), root_.get(), MPFR_RNDN);
        mpfr_div_si(value_.get(), value_.get(), 2 * a, MPFR_RNDN);
        
        // Ensure value is in [0,1)
        mpfr_frac(value_.get(), value_.get(), MPFR_RNDN);
    }
    
    // Delete copy constructor and assignment
    QuadraticIrrational(const QuadraticIrrational&) = delete;
    QuadraticIrrational& operator=(const QuadraticIrrational&) = delete;
    
    // Move operations
    QuadraticIrrational(QuadraticIrrational&&) = default;
    QuadraticIrrational& operator=(QuadraticIrrational&&) = default;
    
    double next() {
        // Calculate next value: ax² + bx + c (mod 1)
        mpfr_mul(temp_.get(), value_.get(), value_.get(), MPFR_RNDN);  // x²
        mpfr_mul_si(temp_.get(), temp_.get(), a_, MPFR_RNDN);         // ax²
        
        mpfr_mul_si(next_.get(), value_.get(), b_, MPFR_RNDN);        // bx
        mpfr_add(next_.get(), next_.get(), temp_.get(), MPFR_RNDN);   // ax² + bx
        
        mpfr_add_si(next_.get(), next_.get(), c_, MPFR_RNDN);         // ax² + bx + c
        mpfr_frac(next_.get(), next_.get(), MPFR_RNDN);               // mod 1
        
        // Update current value
        mpfr_swap(value_.get(), next_.get());
        
        // Convert to double and return
        return mpfr_get_d(value_.get(), MPFR_RNDN);
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

    // Internal method for raw uniform values without distribution transforms
    double nextUniformRaw() {
        if (buffer_pos_ >= buffer_.size()) {
            fill_buffer();
        }
        return buffer_[buffer_pos_++];
    }

    void fill_buffer() {
        for (size_t i = 0; i < buffer_.size(); ++i) {
            buffer_[i] = qi_.next();
        }

        if (config_.use_crypto_mixing && crypto_mixer) {
            mix_buffer();
            // Normalize to [0,1] after mixing
            for (auto& val : buffer_) {
                val = std::fabs(val);
                val = val - std::floor(val);  // Ensure [0,1)
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
        // Handle edge case for u1 near zero
        const double epsilon = std::numeric_limits<double>::epsilon();
        if (u1 < epsilon) {
            u1 = epsilon;
        }
        
        // Box-Muller transform with improved numerical stability
        const double two_pi = 2.0 * M_PI;
        const double r = std::sqrt(-2.0 * std::log(u1));
        const double theta = two_pi * u2;
        
        // Use sincos for better precision and performance
        #ifdef __GNUC__
            double sin_theta, cos_theta;
            __builtin_sincos(theta, &sin_theta, &cos_theta);
            return r * cos_theta;
        #else
            return r * std::cos(theta);
        #endif
    }

    double transform_normal_pair(double u1, double u2, double& second) {
        const double epsilon = std::numeric_limits<double>::epsilon();
        if (u1 < epsilon) {
            u1 = epsilon;
        }
        
        const double two_pi = 2.0 * M_PI;
        const double r = std::sqrt(-2.0 * std::log(u1));
        const double theta = two_pi * u2;
        
        #ifdef __GNUC__
            double sin_theta, cos_theta;
            __builtin_sincos(theta, &sin_theta, &cos_theta);
            second = r * sin_theta;
            return r * cos_theta;
        #else
            second = r * std::sin(theta);
            return r * std::cos(theta);
        #endif
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
        double raw = nextUniformRaw();
        
        switch (config_.distribution) {
            case PRNGConfig::UNIFORM_01:
                return raw;
            case PRNGConfig::UNIFORM_RANGE:
                return config_.range_min + raw * (config_.range_max - config_.range_min);
            case PRNGConfig::NORMAL:
                return transform_normal(nextUniformRaw(), nextUniformRaw());
            case PRNGConfig::EXPONENTIAL:
                return -std::log(1.0 - raw) / config_.exponential_lambda;
            default:
                throw std::runtime_error("Unknown distribution type");
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
    EnhancedPRNG(const PRNGConfig& cfg)
        : qi_(cfg.a, cfg.b, cfg.c, cfg.mpfr_precision),
          config_(cfg),
          buffer_(cfg.buffer_size),
          buffer_pos_(cfg.buffer_size),  // Force initial fill
          sample_count_(0)
    {
        validate_config(cfg);
        if (cfg.use_crypto_mixing) {
            crypto_mixer = std::make_unique<CryptoMixer>();
        }
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
};

// ---------------------------------------------------------------------
// Global PRNG instance with thread safety
static std::mutex g_prng_mutex;
static std::unique_ptr<EnhancedPRNG> g_prng;

// parseConfig
PRNGConfig parseConfig(Rcpp::List cfg) {
    PRNGConfig config;

    // Core parameters
    if (cfg.containsElementNamed("a")) config.a = Rcpp::as<long>(cfg["a"]);
    if (cfg.containsElementNamed("b")) config.b = Rcpp::as<long>(cfg["b"]);
    if (cfg.containsElementNamed("c")) config.c = Rcpp::as<long>(cfg["c"]);
    if (cfg.containsElementNamed("mpfr_precision")) config.mpfr_precision = Rcpp::as<int>(cfg["mpfr_precision"]);
    if (cfg.containsElementNamed("buffer_size")) config.buffer_size = Rcpp::as<size_t>(cfg["buffer_size"]);
    
    // Boolean flags
    if (cfg.containsElementNamed("use_crypto_mixing")) config.use_crypto_mixing = Rcpp::as<bool>(cfg["use_crypto_mixing"]);
    if (cfg.containsElementNamed("use_hardware_rng")) config.use_hardware_rng = Rcpp::as<bool>(cfg["use_hardware_rng"]);
    if (cfg.containsElementNamed("use_threading")) config.use_threading = Rcpp::as<bool>(cfg["use_threading"]);
    
    // Crypto parameters
    if (cfg.containsElementNamed("reseed_interval")) config.reseed_interval = Rcpp::as<unsigned long>(cfg["reseed_interval"]);
    if (cfg.containsElementNamed("hash_algorithm")) config.hash_algorithm = Rcpp::as<std::string>(cfg["hash_algorithm"]);
    if (cfg.containsElementNamed("key_size")) config.key_size = Rcpp::as<int>(cfg["key_size"]);
    
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
    
    return config;
}

// [[Rcpp::export(.createPRNG_)]]
void createPRNG_(Rcpp::List cfg) {
    std::lock_guard<std::mutex> lock(g_prng_mutex);
    PRNGConfig config = parseConfig(cfg);
    g_prng = std::make_unique<EnhancedPRNG>(config);
}

// [[Rcpp::export(.updatePRNG_)]]
void updatePRNG_(Rcpp::List cfg) {
    std::lock_guard<std::mutex> lock(g_prng_mutex);
    if (!g_prng) {
        Rcpp::stop("PRNG not created yet. Call createPRNG(...) first.");
    }
    PRNGConfig config = parseConfig(cfg);
    g_prng->updateConfig(config);
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