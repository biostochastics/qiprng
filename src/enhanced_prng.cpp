// File: enhanced_prng.cpp
// --------------------------------------------------------------
#include <Rcpp.h>
#include <cmath>
#include <memory>
#include <mutex>
#include <random>
#include <stdexcept>
#include <thread>
#include <functional>
#include <vector>
#include <array>
#include <string>
#include <map>
#include <algorithm>
#include <cstring>

// For MPFR high-precision
#include <mpfr.h>

// For crypto operations
#include <sodium.h>

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
        
        // Validate MPFR precision
        if (prec < 16 || prec > 10000) {
            throw std::runtime_error("Invalid MPFR precision: must be between 16 and 10000 bits");
        }
        
        // Calculate D = b² - 4ac
        long long d = static_cast<long long>(b_) * b_ - 4LL * a_ * c_;
        if (d <= 0) {
            throw std::runtime_error("Invalid quadratic parameters: discriminant must be positive");
        }

        // Optionally check if it's square-free
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
        
        // Extract just the fractional part using mpfr_frac
        mpfr_frac(*value_->get(), *value_->get(), MPFR_RNDN);
        
        // Ensure value is in [0,1) range
        if (mpfr_sgn(*value_->get()) < 0) {
            mpfr_add_ui(*value_->get(), *value_->get(), 1, MPFR_RNDN);
        }
        
        // Ensure value is not exactly 0 or 1
        if (mpfr_zero_p(*value_->get()) || mpfr_cmp_ui(*value_->get(), 1) == 0) {
            // Set to a small non-zero value
            mpfr_set_d(*value_->get(), 0.5, MPFR_RNDN);
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
        
        // Calculate x_n^2
        mpfr_mul(*temp_->get(), *value_->get(), *value_->get(), MPFR_RNDN);
        
        // Calculate a*x_n^2
        mpfr_mul_si(*next_->get(), *temp_->get(), a_, MPFR_RNDN);
        
        // Calculate b*x_n
        mpfr_mul_si(*temp_->get(), *value_->get(), b_, MPFR_RNDN);
        
        // Add to get a*x_n^2 + b*x_n
        mpfr_add(*next_->get(), *next_->get(), *temp_->get(), MPFR_RNDN);
        
        // Add c to get a*x_n^2 + b*x_n + c
        mpfr_add_si(*next_->get(), *next_->get(), c_, MPFR_RNDN);
        
        // Extract just the fractional part
        mpfr_frac(*next_->get(), *next_->get(), MPFR_RNDN);
        
        // Ensure value is in [0,1) range
        if (mpfr_sgn(*next_->get()) < 0) {
            mpfr_add_ui(*next_->get(), *next_->get(), 1, MPFR_RNDN);
        }
        
        // Ensure value is not exactly 0 or 1
        if (mpfr_zero_p(*next_->get()) || mpfr_cmp_ui(*next_->get(), 1) == 0) {
            // Generate a different value based on the current state
            mpfr_set_d(*next_->get(), 0.5, MPFR_RNDN);
            mpfr_add(*next_->get(), *next_->get(), *value_->get(), MPFR_RNDN);
            mpfr_frac(*next_->get(), *next_->get(), MPFR_RNDN);
        }
        
        // Swap current and next values
        mpfr_swap(*value_->get(), *next_->get());
        
        // Return as double, ensuring it's in [0,1)
        double result = mpfr_get_d(*value_->get(), MPFR_RNDN);
        
        // Final safety check
        if (result >= 1.0) result = 0.9999999999999999;
        if (result <= 0.0) result = 0.0000000000000001;
        
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
    bool has_spare_normal_;
    double spare_normal_;
    bool offset_applied_;
    size_t skipped_;

    // Fill the buffer with new random values
    void fill_buffer() {
        // Generate [0,1) via QuadraticIrrational
        for (size_t i = 0; i < buffer_.size(); ++i) {
            buffer_[i] = qi_.next();
        }
        // If crypto mixing is enabled, re-map to 53-bit, XOR, then re-map to [0,1)
        if (config_.use_crypto_mixing && crypto_mixer) {
            if (!mix_buffer()) {
                throw std::runtime_error("Crypto mixing failed");
            }
        }
        buffer_pos_ = 0;
    }

    // Return next raw uniform [0,1)
    double nextUniformRaw() {
        // Skip offset draws if not yet applied
        if (!offset_applied_) {
            while (skipped_ < config_.offset) {
                qi_.next(); // discard
                ++skipped_;
            }
            offset_applied_ = true;
        }

        // Refill the local buffer if needed
        if (buffer_pos_ >= buffer_.size()) {
            fill_buffer();
        }
        return buffer_[buffer_pos_++];
    }

    // Uniform range transform
    double transform_uniform_range() {
        return config_.range_min +
               nextUniformRaw() * (config_.range_max - config_.range_min);
    }

    // Improved normal distribution generation using Ziggurat method
    double nextNormal() {
        // Use Box-Muller transform for simplicity but with enhanced precision
        if (has_spare_normal_) {
            has_spare_normal_ = false;
            return spare_normal_ * config_.normal_sd + config_.normal_mean;
        }
        
        // Enhanced Box-Muller transform with improved numerical stability
        double u1, u2, r, z0, z1;
        
        // Generate two uniform random numbers in (0,1) - avoid exact 0
        do {
            u1 = nextUniformRaw();
        } while (u1 <= 0.0);
        
        u2 = nextUniformRaw();
        
        // Convert to polar coordinates
        r = std::sqrt(-2.0 * std::log(u1));
        
        // Use trigonometric functions for better distribution
        z0 = r * std::cos(2.0 * M_PI * u2);
        z1 = r * std::sin(2.0 * M_PI * u2);
        
        // Save the spare value for next call
        spare_normal_ = z1;
        has_spare_normal_ = true;
        
        // Apply mean and standard deviation
        return z0 * config_.normal_sd + config_.normal_mean;
    }

    // Exponential transform
    double transform_exponential() {
        double u;
        do {
            u = nextUniformRaw();
        } while (u <= 0.0);
        return -std::log(u) / config_.exponential_lambda;
    }

    // Re-map double->bits->XOR->double
    bool mix_buffer() {
        // If crypto mixing is not enabled, return early
        if (!crypto_mixer) return true;
        
        // Temporarily store each double as a 53-bit integer
        SecureBuffer<uint64_t> tempInt(buffer_.size());
        for (size_t i = 0; i < buffer_.size(); i++) {
            // clamp and map x in [0,1) to 53-bit integer
            double x = buffer_[i];
            if (x < 0.0) x = 0.0;
            if (x >= 1.0) x = 0.9999999999999999; // just in case
            // scale up to 2^53
            double scaled = x * (double)((uint64_t)1 << 53);
            uint64_t bits = (uint64_t)std::floor(scaled);
            tempInt[i] = bits;
        }
        
        // XOR with keystream
        if (!crypto_mixer->mix(reinterpret_cast<unsigned char*>(tempInt.data()), tempInt.size() * sizeof(uint64_t))) {
            return false;
        }
        
        // Convert back to double in [0,1)
        for (size_t i = 0; i < buffer_.size(); i++) {
            // Mask to ensure we only use 53 bits (mantissa bits for double precision)
            uint64_t bits = tempInt[i] & ((UINT64_C(1) << 53) - 1);
            
            // Convert to double in [0,1) range with improved precision
            buffer_[i] = (double)bits / (double)((uint64_t)1 << 53);
            
            // Ensure the value is strictly less than 1.0
            if (buffer_[i] >= 1.0) {
                buffer_[i] = 0.9999999999999999;
            }
            
            // Additional check to avoid values too close to 0
            if (buffer_[i] <= 0.0) {
                buffer_[i] = std::nextafter(0.0, 1.0);
            }
        }
        
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
        
        // If offset is not explicitly set, use a default value of 100
        // This helps avoid bias in the initial digits
        if (config_.offset == 0) {
            config_.offset = 100;
        }
        
        // If mixing is requested, initialize the CryptoMixer
        if (config_.use_crypto_mixing) {
            crypto_mixer = std::make_unique<CryptoMixer>();
        }
    }

    const PRNGConfig& getConfig() const {
        return config_;
    }

    void updateConfig(const PRNGConfig& new_config) {
        // Validate core parameters
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

        // Rebuild the QuadraticIrrational if core params changed
        if (core_changed) {
            qi_ = QuadraticIrrational(config_.a, config_.b,
                                      config_.c, config_.mpfr_precision);
        }

        // Possibly resize buffer
        if (buffer_.size() != config_.buffer_size) {
            buffer_.resize(config_.buffer_size);
            buffer_pos_ = buffer_.size();
        }

        offset_applied_ = false;
        skipped_ = 0;

        // Update crypto usage
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
        // Possibly reseed
        if (config_.reseed_interval > 0 &&
            sample_count_ >= config_.reseed_interval) {
            reseed();
        }
        
        double result;
        switch (config_.distribution) {
            case PRNGConfig::UNIFORM_01:
                result = nextUniformRaw();
                break;
            case PRNGConfig::UNIFORM_RANGE:
                result = transform_uniform_range();
                break;
            case PRNGConfig::NORMAL:
                result = nextNormal();
                break;
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
        // Get fresh entropy from system RNG
        std::random_device rd;
        std::mt19937_64 gen(rd());
        std::uniform_int_distribution<int> small_dist(1, 10);
        
        // Start with the original parameters
        long new_a = config_.a;
        long new_b = config_.b;
        long new_c = config_.c;
        
        // Try to generate valid parameters with square-free discriminant
        bool valid_params = false;
        int attempts = 0;
        
        while (!valid_params && attempts < 10) {
            attempts++;
            
            // Generate small prime-like values for better chance of square-free discriminant
            // Use small primes or numbers likely to produce square-free discriminants
            int prime_options[] = {2, 3, 5, 7, 11, 13, 17, 19, 23, 29};
            int idx_a = small_dist(gen) % 10;
            int idx_b = small_dist(gen) % 10;
            int idx_c = small_dist(gen) % 10;
            
            // Preserve signs but use prime-based magnitudes
            int sign_a = (new_a >= 0) ? 1 : -1;
            int sign_b = (new_b >= 0) ? 1 : -1;
            int sign_c = (new_c >= 0) ? 1 : -1;
            
            new_a = sign_a * prime_options[idx_a];
            new_b = sign_b * prime_options[idx_b];
            new_c = sign_c * prime_options[idx_c];
            
            // Ensure discriminant is positive: b^2 - 4ac > 0
            long long disc = static_cast<long long>(new_b) * new_b - 4LL * new_a * new_c;
            
            // If discriminant is not positive, adjust c to make it positive
            if (disc <= 0) {
                // Flip the sign of c to ensure positive discriminant
                new_c = -std::abs(new_c);
                disc = static_cast<long long>(new_b) * new_b - 4LL * new_a * new_c;
            }
            
            // Check if discriminant is square-free (no perfect square factors)
            // This is a simplified check - we just ensure it's not a perfect square
            long long sqrt_disc = std::sqrt(disc);
            if (sqrt_disc * sqrt_disc != disc) {
                valid_params = true;
            }
        }
        
        // If we couldn't find valid parameters, use safe defaults
        if (!valid_params) {
            new_a = 2;
            new_b = 5;
            new_c = -2;
        }
        
        // Update the configuration with the new parameters
        PRNGConfig new_config = config_;
        new_config.a = new_a;
        new_config.b = new_b;
        new_config.c = new_c;
        
        try {
            // Update the PRNG with the new configuration
            updateConfig(new_config);
            
            // Reseed the crypto mixer if it exists
            if (crypto_mixer) {
                crypto_mixer->reseed();
            }
            
            // Reset counters and buffers
            sample_count_ = 0;
            buffer_pos_ = buffer_.size();
            has_spare_normal_ = false;
            offset_applied_ = false;
            skipped_ = 0;
        } catch (const std::exception& e) {
            // If updating config fails, keep the old config and just reset counters
            sample_count_ = 0;
            if (crypto_mixer) {
                crypto_mixer->reseed();
            }
        }
    }

    void skip(size_t n) {
        for (size_t i = 0; i < n; i++) {
            next();
        }
    }
};

// ---------------------------------------------------------------------
// Global PRNG instance with thread safety
static std::mutex g_prng_mutex;
static thread_local std::unique_ptr<EnhancedPRNG> t_prng;
static std::unique_ptr<EnhancedPRNG> g_prng;
static bool g_use_threading = false;

// Helper function to parse R config list into PRNGConfig
PRNGConfig parse_r_config(const Rcpp::List& cfg) {
    PRNGConfig config;
    
    // Core parameters
    if (cfg.containsElementNamed("a")) config.a = Rcpp::as<long>(cfg["a"]);
    if (cfg.containsElementNamed("b")) config.b = Rcpp::as<long>(cfg["b"]);
    if (cfg.containsElementNamed("c")) config.c = Rcpp::as<long>(cfg["c"]);
    if (cfg.containsElementNamed("mpfr_precision")) {
        config.mpfr_precision = Rcpp::as<int>(cfg["mpfr_precision"]);
    }
    if (cfg.containsElementNamed("buffer_size")) {
        config.buffer_size = Rcpp::as<size_t>(cfg["buffer_size"]);
    }
    
    // Distribution parameters
    if (cfg.containsElementNamed("distribution")) {
        std::string dist = Rcpp::as<std::string>(cfg["distribution"]);
        if (dist == "uniform_01") {
            config.distribution = PRNGConfig::UNIFORM_01;
        } else if (dist == "uniform_range") {
            config.distribution = PRNGConfig::UNIFORM_RANGE;
        } else if (dist == "normal") {
            config.distribution = PRNGConfig::NORMAL;
        } else if (dist == "exponential") {
            config.distribution = PRNGConfig::EXPONENTIAL;
        } else {
            throw std::runtime_error("Invalid distribution type");
        }
    }
    
    if (cfg.containsElementNamed("range_min")) {
        config.range_min = Rcpp::as<double>(cfg["range_min"]);
    }
    if (cfg.containsElementNamed("range_max")) {
        config.range_max = Rcpp::as<double>(cfg["range_max"]);
    }
    if (cfg.containsElementNamed("normal_mean")) {
        config.normal_mean = Rcpp::as<double>(cfg["normal_mean"]);
    }
    if (cfg.containsElementNamed("normal_sd")) {
        config.normal_sd = Rcpp::as<double>(cfg["normal_sd"]);
    }
    if (cfg.containsElementNamed("exponential_lambda")) {
        config.exponential_lambda = Rcpp::as<double>(cfg["exponential_lambda"]);
    }
    
    // Crypto parameters
    if (cfg.containsElementNamed("use_crypto_mixing")) {
        config.use_crypto_mixing = Rcpp::as<bool>(cfg["use_crypto_mixing"]);
    }
    if (cfg.containsElementNamed("reseed_interval")) {
        config.reseed_interval = Rcpp::as<unsigned long>(cfg["reseed_interval"]);
    }
    
    // Additional offset parameter
    if (cfg.containsElementNamed("offset")) {
        config.offset = Rcpp::as<size_t>(cfg["offset"]);
    }

    // Debug flag
    if (cfg.containsElementNamed("debug")) {
        config.debug = Rcpp::as<bool>(cfg["debug"]);
    }
    
    // Threading flag
    if (cfg.containsElementNamed("use_threading")) {
        g_use_threading = Rcpp::as<bool>(cfg["use_threading"]);
    }
    
    return config;
}

// [[Rcpp::export(.createPRNG_)]]
void createPRNG_(Rcpp::List cfg) {
    PRNGConfig config = parse_r_config(cfg);
    
    if (g_use_threading) {
        // Create a thread-local PRNG with a unique seed for each thread
        t_prng = std::make_unique<EnhancedPRNG>(config);
        
        // Add some randomness to the thread-local PRNG
        std::hash<std::thread::id> hasher;
        size_t thread_hash = hasher(std::this_thread::get_id());
        t_prng->skip(thread_hash % 10000); // Skip a random number of values based on thread ID
    } else {
        // Use the global PRNG with mutex protection
        std::lock_guard<std::mutex> lock(g_prng_mutex);
        g_prng = std::make_unique<EnhancedPRNG>(config);
    }
}

// [[Rcpp::export(.updatePRNG_)]]
void updatePRNG_(Rcpp::List cfg) {
    PRNGConfig config = parse_r_config(cfg);
    
    if (g_use_threading) {
        if (!t_prng) {
            throw std::runtime_error("Thread-local PRNG not initialized");
        }
        t_prng->updateConfig(config);
    } else {
        std::lock_guard<std::mutex> lock(g_prng_mutex);
        if (!g_prng) {
            throw std::runtime_error("PRNG not initialized");
        }
        g_prng->updateConfig(config);
    }
}

// [[Rcpp::export(.getPRNGConfig_)]]
Rcpp::List getPRNGConfig_() {
    PRNGConfig config;
    
    if (g_use_threading) {
        if (!t_prng) {
            throw std::runtime_error("Thread-local PRNG not initialized");
        }
        config = t_prng->getConfig();
    } else {
        std::lock_guard<std::mutex> lock(g_prng_mutex);
        if (!g_prng) {
            throw std::runtime_error("PRNG not initialized");
        }
        config = g_prng->getConfig();
    }
    
    Rcpp::List result = Rcpp::List::create(
        Rcpp::Named("a") = config.a,
        Rcpp::Named("b") = config.b,
        Rcpp::Named("c") = config.c,
        Rcpp::Named("mpfr_precision") = config.mpfr_precision,
        Rcpp::Named("buffer_size") = config.buffer_size,
        Rcpp::Named("distribution") = [&]() -> std::string {
            switch (config.distribution) {
                case PRNGConfig::UNIFORM_01: return "uniform_01";
                case PRNGConfig::UNIFORM_RANGE: return "uniform_range";
                case PRNGConfig::NORMAL: return "normal";
                case PRNGConfig::EXPONENTIAL: return "exponential";
                default: return "unknown";
            }
        }(),
        Rcpp::Named("range_min") = config.range_min,
        Rcpp::Named("range_max") = config.range_max,
        Rcpp::Named("normal_mean") = config.normal_mean,
        Rcpp::Named("normal_sd") = config.normal_sd,
        Rcpp::Named("exponential_lambda") = config.exponential_lambda,
        Rcpp::Named("use_crypto_mixing") = config.use_crypto_mixing,
        Rcpp::Named("reseed_interval") = config.reseed_interval,
        Rcpp::Named("offset") = config.offset,
        Rcpp::Named("debug") = config.debug,
        Rcpp::Named("use_threading") = g_use_threading
    );
    
    return result;
}

// [[Rcpp::export(.generatePRNG_)]]
Rcpp::NumericVector generatePRNG_(int n) {
    Rcpp::NumericVector result(n);
    
    if (g_use_threading) {
        if (!t_prng) {
            throw std::runtime_error("Thread-local PRNG not initialized");
        }
        for (int i = 0; i < n; i++) {
            result[i] = t_prng->next();
        }
    } else {
        std::lock_guard<std::mutex> lock(g_prng_mutex);
        if (!g_prng) {
            throw std::runtime_error("PRNG not initialized");
        }
        for (int i = 0; i < n; i++) {
            result[i] = g_prng->next();
        }
    }
    
    return result;
}

// [[Rcpp::export(.reseedPRNG_)]]
void reseedPRNG_() {
    if (g_use_threading) {
        if (!t_prng) {
            throw std::runtime_error("Thread-local PRNG not initialized");
        }
        t_prng->reseed();
    } else {
        std::lock_guard<std::mutex> lock(g_prng_mutex);
        if (!g_prng) {
            throw std::runtime_error("PRNG not initialized");
        }
        g_prng->reseed();
    }
}

// [[Rcpp::export(.skipPRNG_)]]
void skipPRNG_(int n) {
    if (g_use_threading) {
        if (!t_prng) {
            throw std::runtime_error("Thread-local PRNG not initialized");
        }
        t_prng->skip(static_cast<size_t>(n));
    } else {
        std::lock_guard<std::mutex> lock(g_prng_mutex);
        if (!g_prng) {
            throw std::runtime_error("PRNG not initialized");
        }
        g_prng->skip(static_cast<size_t>(n));
    }
}

// [[Rcpp::export(.cleanup_prng_)]]
void cleanup_prng_() {
    if (g_use_threading) {
        t_prng.reset();
    } else {
        std::lock_guard<std::mutex> lock(g_prng_mutex);
        g_prng.reset();
    }
}