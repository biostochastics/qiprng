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
#include <unordered_set>
#include <cstring>

// For MPFR high-precision
#include <mpfr.h>

// For crypto operations
#include <sodium.h>

#include <chrono>

#include "prng_common.hpp"

using namespace qiprng;

// ---------------------------------------------------------------------
// Forward declarations of classes
class QuadraticIrrational;
class MultiQI;
class CryptoMixer;
class EnhancedPRNG;

// ---------------------------------------------------------------------
// Forward-declared helper functions
long long chooseUniqueDiscriminant(long min_value = 1000, long max_value = 999999);
std::tuple<long,long,long> makeABCfromDelta(long long Delta);

// ---------------------------------------------------------------------
// Global flags/objects for threaded vs non-threaded
static bool g_use_threading = false;
static std::mutex g_prng_mutex;  // protects the global PRNG
static thread_local std::unique_ptr<EnhancedPRNG> t_prng; // thread-local PRNG
static std::unique_ptr<EnhancedPRNG> g_prng;              // global PRNG

// Global registry of used discriminants
static std::mutex g_disc_mutex;
static std::unordered_set<long long> g_used_discriminants;

// ---------------------------------------------------------------------
// PRNGConfig structure with newly added fields
struct PRNGConfig {
    enum Distribution {
        UNIFORM_01,
        UNIFORM_RANGE,
        NORMAL,
        EXPONENTIAL
    };
    
    // Quadratic parameters (initial or fallback)
    long a = 2;
    long b = 5;
    long c = -2;
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
    unsigned long reseed_interval = 1000;

    // Additional offset
    size_t offset = 0;

    // Debug
    bool debug = false;
};

// ---------------------------------------------------------------------
// QuadraticIrrational: single MPFR-based Qi
class QuadraticIrrational {
private:
    long a_, b_, c_;
    std::unique_ptr<MPFRWrapper> root_;
    std::unique_ptr<MPFRWrapper> value_;
    std::unique_ptr<MPFRWrapper> next_;
    std::unique_ptr<MPFRWrapper> temp_;

public:
    QuadraticIrrational(long a, long b, long c, mpfr_prec_t prec)
        : a_(a), b_(b), c_(c),
          root_(std::make_unique<MPFRWrapper>(prec)),
          value_(std::make_unique<MPFRWrapper>(prec)),
          next_(std::make_unique<MPFRWrapper>(prec)),
          temp_(std::make_unique<MPFRWrapper>(prec))
    {
        long long disc = (long long)b_ * b_ - 4LL * a_ * c_;
        if (disc <= 0) {
            throw std::runtime_error("QuadraticIrrational: non-positive discriminant");
        }

        // Compute sqrt(disc)
        mpfr_set_si(*root_->get(), disc, MPFR_RNDN);
        mpfr_sqrt(*root_->get(), *root_->get(), MPFR_RNDN);

        // x0 = frac( (-b + sqrt(disc)) / (2a) )
        mpfr_set_si(*value_->get(), -b_, MPFR_RNDN);
        mpfr_add(*value_->get(), *value_->get(), *root_->get(), MPFR_RNDN);
        mpfr_div_si(*value_->get(), *value_->get(), 2*a_, MPFR_RNDN);

        // fractional part
        mpfr_frac(*value_->get(), *value_->get(), MPFR_RNDN);
        if (mpfr_sgn(*value_->get()) < 0) {
            mpfr_add_ui(*value_->get(), *value_->get(), 1, MPFR_RNDN);
        }

        mpfr_set_zero(*next_->get(), 1);
        mpfr_set_zero(*temp_->get(), 1);
    }

    double next() {
        // x_{n+1} = frac(a*x_n^2 + b*x_n + c)
        mpfr_mul(*temp_->get(), *value_->get(), *value_->get(), MPFR_RNDN);
        mpfr_mul_si(*next_->get(), *temp_->get(), a_, MPFR_RNDN);
        mpfr_mul_si(*temp_->get(), *value_->get(), b_, MPFR_RNDN);
        mpfr_add(*next_->get(), *next_->get(), *temp_->get(), MPFR_RNDN);
        mpfr_add_si(*next_->get(), *next_->get(), c_, MPFR_RNDN);

        mpfr_frac(*next_->get(), *next_->get(), MPFR_RNDN);
        if (mpfr_sgn(*next_->get()) < 0) {
            mpfr_add_ui(*next_->get(), *next_->get(), 1, MPFR_RNDN);
        }
        mpfr_swap(*value_->get(), *next_->get());
        return mpfr_get_d(*value_->get(), MPFR_RNDN); // in [0,1)
    }
};

// ---------------------------------------------------------------------
// MultiQI: a set of QIs, round-robin calls to next()
class MultiQI {
private:
    std::vector<QuadraticIrrational> qis_;
    size_t idx_;

public:
    MultiQI(const std::vector<std::tuple<long,long,long>>& abc_list, int mpfr_prec)
        : idx_(0)
    {
        for (auto &abc : abc_list) {
            long A,B,C;
            std::tie(A,B,C) = abc;
            qis_.emplace_back(A,B,C, mpfr_prec);
        }
    }

    double next() {
        if (qis_.empty()) {
            return 0.5;
        }
        double val = qis_[idx_].next();
        idx_ = (idx_+1) % qis_.size();
        return val;
    }

    size_t size() const { return qis_.size(); }
};

// ---------------------------------------------------------------------
// CryptoMixer: ChaCha20-based mixing
class CryptoMixer {
private:
    SecureBuffer<unsigned char> key_;
    SecureBuffer<unsigned char> nonce_;

    void secure_random(unsigned char* buf, size_t len) {
        randombytes_buf(buf, len);
    }
public:
    CryptoMixer() 
        : key_(crypto_stream_chacha20_KEYBYTES),
          nonce_(crypto_stream_chacha20_NONCEBYTES)
    {
        if (sodium_init() < 0) {
            throw std::runtime_error("Failed to init libsodium");
        }
        reseed();
    }
    
    ~CryptoMixer() = default;
    
    void reseed() {
        secure_random(key_.data(), key_.size());
        secure_random(nonce_.data(), nonce_.size());
    }
    
    bool mix(unsigned char* data, size_t len) {
        try {
            // Calculate the number of doubles in the buffer
            size_t num_doubles = len / sizeof(double);
            double* doubles = reinterpret_cast<double*>(data);
            
            // 1. Convert doubles [0,1) to 53-bit integers
            SecureBuffer<uint64_t> tempInt(num_doubles);
            for (size_t i = 0; i < num_doubles; i++) {
                double x = doubles[i];
                // Ensure x is in [0,1)
                if (x <= 0.0) x = 0.0;
                if (x >= 1.0) x = 0.9999999999999999;
                
                // Convert to 53-bit integer (2^53 is the largest integer exactly representable in a double)
                uint64_t bits = (uint64_t)std::floor(x * (double)((uint64_t)1 << 53));
                tempInt[i] = bits;
            }
            
            // 2. Create a keystream for XOR
            SecureBuffer<unsigned char> keystream(tempInt.size() * sizeof(uint64_t));
            if (crypto_stream_chacha20(keystream.data(), keystream.size(), nonce_.data(), key_.data()) != 0) {
                return false;
            }
            
            // 3. XOR the integers with the keystream
            unsigned char* tempIntPtr = reinterpret_cast<unsigned char*>(tempInt.data());
            for (size_t i = 0; i < keystream.size(); i++) {
                tempIntPtr[i] ^= keystream[i];
            }
            
            // 4. Generate a second keystream for jitter to completely eliminate ties
            SecureBuffer<unsigned char> jitter_stream(num_doubles * sizeof(uint64_t));
            // Increment nonce for different keystream
            for (size_t i = 0; i < nonce_.size(); i++) {
                if (++nonce_[i] != 0) break;
            }
            if (crypto_stream_chacha20(jitter_stream.data(), jitter_stream.size(), nonce_.data(), key_.data()) != 0) {
                return false;
            }
            
            // 5. Convert back to doubles in [0,1) range with slightly increased jitter
            for (size_t i = 0; i < num_doubles; i++) {
                // Mask to ensure we have only 53 bits
                uint64_t bits = (tempInt[i] & ((1ULL << 53) - 1));
                
                // Create a very small jitter using the second keystream
                uint64_t* jitter_ptr = reinterpret_cast<uint64_t*>(jitter_stream.data() + i * sizeof(uint64_t));
                uint64_t jitter_bits = *jitter_ptr;
                
                // Use a tiny jitter to break ties - increased from 1e-16 to 5e-16
                double jitter = (double)jitter_bits / (double)(1ULL << 63);
                
                // Convert to double in [0,1) with minimal jitter
                doubles[i] = (double)bits / (double)((uint64_t)1 << 53) + jitter * 5e-16;
                
                // Extra safety checks - more relaxed clamping
                if (doubles[i] >= 1.0) doubles[i] = 0.9999999999999999;
                if (doubles[i] <= 0.0) doubles[i] = 0.0000000000000001;
            }
            
            // 6. Instead of sorting, check for duplicates locally with improved jitter and window
            const size_t WINDOW_SIZE = 200; // Increased from 50 to 200
            std::unordered_set<uint64_t> recent_values;
            
            for (size_t i = 0; i < num_doubles; i++) {
                // Convert to bits for precision comparison
                uint64_t bits = (uint64_t)(doubles[i] * (double)((uint64_t)1 << 53));
                
                // If this is a duplicate within our recent window, adjust it slightly
                if (recent_values.find(bits) != recent_values.end()) {
                    // Use a much larger adjustment to minimize distribution bias
                    // Get a new random bit from the jitter stream for this position
                    uint64_t extra_jitter = jitter_stream[(i * 13) % jitter_stream.size()]; // Use a different prime offset
                    
                    // Apply a tiny adjustment (either up or down based on parity)
                    if (extra_jitter & 1) {
                        // Add small jitter up - increased from 5e-17 to 1e-15
                        doubles[i] += 1e-15;
                        if (doubles[i] >= 1.0) doubles[i] = 0.9999999999999999;
                    } else {
                        // Add small jitter down - increased from 5e-17 to 1e-15
                        doubles[i] -= 1e-15;
                        if (doubles[i] <= 0.0) doubles[i] = 0.0000000000000001;
                    }
                }
                
                // Update the sliding window of recent values
                // Recalculate bits after possible adjustment
                bits = (uint64_t)(doubles[i] * (double)((uint64_t)1 << 53));
                recent_values.insert(bits);
                
                // Remove oldest value when window gets too large
                if (recent_values.size() > WINDOW_SIZE) {
                    if (i >= WINDOW_SIZE) {
                        uint64_t old_bits = (uint64_t)(doubles[i - WINDOW_SIZE] * (double)((uint64_t)1 << 53));
                        recent_values.erase(old_bits);
                    }
                }
            }
            
            // Increment nonce for next use
            for (size_t i = 0; i < nonce_.size(); i++) {
                if (++nonce_[i] != 0) break;
            }
            
            return true;
        } catch (const std::exception&) {
            return false;
        }
    }
};

// ---------------------------------------------------------------------
// EnhancedPRNG: uses MultiQI + distribution transforms + crypto + offset
class EnhancedPRNG {
private:
    PRNGConfig config_;
    std::unique_ptr<MultiQI> multi_;

    SecureBuffer<double> buffer_;
    size_t buffer_pos_;

    std::unique_ptr<CryptoMixer> crypto_;

    size_t sample_count_;

    bool has_spare_normal_;
    double spare_normal_;

    bool offset_applied_;
    size_t skipped_;

    void reset_state() {
        // Reset the PRNG state
        has_spare_normal_ = false;
        spare_normal_ = 0.0;
        
        // Generate a new seed
        SecureBuffer<unsigned char> seed_buffer(32);
        randombytes_buf(seed_buffer.data(), seed_buffer.size());
        
        // Initialize the mixer with the new seed
        if (crypto_) {
            crypto_->reseed();
        }
        
        // Reset the counter
        sample_count_ = 0;
        
        // Fully refresh internal buffer
        buffer_pos_ = buffer_.size();
        
        // Use a moderate random component for thread isolation
        // Get random bytes for skipping, but keep it reasonable
        SecureBuffer<unsigned char> random_bytes(sizeof(uint16_t));
        randombytes_buf(random_bytes.data(), random_bytes.size());
        uint16_t random_component = *reinterpret_cast<uint16_t*>(random_bytes.data());
        
        // Add thread-specific component with moderate randomization
        size_t thread_hash = std::hash<std::thread::id>()(std::this_thread::get_id());
        uint64_t thread_offset = thread_hash % 10000; // More moderate thread-specific component
        
        // Fixed offset from config
        uint64_t fixed_offset = config_.offset;
        
        // Combine offsets with a moderate random component
        uint64_t random_offset = random_component % 20000; // More moderate random component
        uint64_t total_offset = fixed_offset + thread_offset + random_offset;
        
        // Apply a reasonable cap to avoid excessive skipping that could
        // push the generator into atypical phases
        if (total_offset > 50000) {
            total_offset = 50000;
        }
        
        // Skip ahead to reduce correlation, but not excessively
        for (uint64_t i = 0; i < total_offset; i++) {
            nextUniformRaw();
        }
    }

    // Fill buffer
    void fill_buffer() {
        for (size_t i=0; i<buffer_.size(); i++) {
            buffer_[i] = multi_->next();
        }
        if (config_.use_crypto_mixing && crypto_) {
            if (!mix_buffer()) {
                throw std::runtime_error("Crypto mixing failed in fill_buffer()");
            }
        }
        buffer_pos_ = 0;
    }

    bool mix_buffer() {
        if (!crypto_) return true;
        SecureBuffer<unsigned char> tmp(buffer_.size()*sizeof(double));
        std::memcpy(tmp.data(), buffer_.data(), tmp.size());
        if (!crypto_->mix(tmp.data(), tmp.size())) {
            return false;
        }
        std::memcpy(buffer_.data(), tmp.data(), tmp.size());
        return true;
    }

    double nextUniformRaw() {
        if (!offset_applied_) {
            while (skipped_ < config_.offset) {
                multi_->next();
                skipped_++;
            }
            offset_applied_ = true;
        }
        if (buffer_pos_ >= buffer_.size()) {
            fill_buffer();
        }
        return buffer_[buffer_pos_++];
    }

    double transform_uniform_range() {
        return config_.range_min + nextUniformRaw()*(config_.range_max - config_.range_min);
    }

    double transform_exponential() {
        // Standard inverse transform sampling for exponential distribution
        // with mean = 1/lambda
        
        // Get a uniform value in (0,1)
        double u = nextUniformRaw();
        
        // Apply minimal clamping to avoid log(0)
        // Use extremely small clamp to preserve the distribution
        const double eps = 1e-300;
        
        // Only apply minimal clamp to avoid numerical issues
        if (u <= 0.0) u = eps;
        else if (u >= 1.0) u = 1.0 - eps;
        
        // For exponential distribution with parameter lambda (mean = 1/lambda):
        // F^(-1)(u) = -ln(1-u)/lambda or equivalently -ln(u)/lambda
        // We use u directly for better numerical stability
        double result = -std::log(u) / config_.exponential_lambda;
        
        // Minimal validation only for non-finite values
        if (!std::isfinite(result) || result <= 0.0) {
            // If invalid, return exactly the expected mean
            result = 1.0 / config_.exponential_lambda;
        }
        
        return result;
    }

    double nextNormal() {
        // Box-Muller transform optimized for mean=0, sd=1 standard normal
        
        // If we have a spare normal, use it
        if (has_spare_normal_) {
            has_spare_normal_ = false;
            // Apply the mean and SD transformation correctly
            return spare_normal_ * config_.normal_sd + config_.normal_mean;
        }
        
        // Generate two uniform random numbers in (0,1)
        double u1 = nextUniformRaw();
        double u2 = nextUniformRaw();
        
        // Apply minimal clamping to just avoid log(0) and preserve extreme tails
        // Use extremely small clamp values to preserve the distribution tails
        const double eps = 1e-300;
        
        // For u1 - determines radius in Box-Muller
        // Only apply the minimal necessary clamping to avoid numerical issues
        if (u1 <= 0.0) u1 = eps;
        else if (u1 >= 1.0) u1 = 1.0 - eps;
        
        // For u2 - only clamp if exactly 0 or 1, which should be extremely rare
        if (u2 <= 0.0) u2 = eps;
        else if (u2 >= 1.0) u2 = 1.0 - eps;
        
        // Apply Box-Muller transform to get standard normal variates
        double radius = std::sqrt(-2.0 * std::log(u1));
        double theta = 2.0 * M_PI * u2;
        
        // Generate two standard normal variates with mean=0, sd=1
        double z0 = radius * std::cos(theta);
        double z1 = radius * std::sin(theta);
        
        // Only validate for non-finite values, otherwise preserve the distribution
        if (!std::isfinite(z0)) z0 = 0.0;
        if (!std::isfinite(z1)) z1 = 0.0;
        
        // Save z1 for later use
        spare_normal_ = z1;
        has_spare_normal_ = true;
        
        // Return z0 transformed to the desired mean and standard deviation
        return z0 * config_.normal_sd + config_.normal_mean;
    }

public:
    EnhancedPRNG(const PRNGConfig& config, const std::vector<std::tuple<long,long,long>>& abc_list)
      : config_(config),
        multi_(std::make_unique<MultiQI>(abc_list, config.mpfr_precision)),
        buffer_(config.buffer_size),
        buffer_pos_(config.buffer_size),
        sample_count_(0),
        has_spare_normal_(false),
        spare_normal_(0.0),
        offset_applied_(false),
        skipped_(0)
    {
        if (config_.use_crypto_mixing) {
            crypto_ = std::make_unique<CryptoMixer>();
        }
        
        // Set a large default offset if not specified
        if (config_.offset == 0) {
            // default offset - increased to 100000 to reduce correlation
            config_.offset = 100000;
        }
        
        reset_state();
    }

    ~EnhancedPRNG() = default;

    const PRNGConfig& getConfig() const { return config_; }
    size_t getQICount() const { return multi_ ? multi_->size() : 0; }

    void updateConfig(const PRNGConfig& new_config) {
        // Check if distribution is changing
        bool distribution_changed = (config_.distribution != new_config.distribution);
        bool params_changed = (config_.normal_mean != new_config.normal_mean || 
                              config_.normal_sd != new_config.normal_sd ||
                              config_.exponential_lambda != new_config.exponential_lambda ||
                              config_.range_min != new_config.range_min ||
                              config_.range_max != new_config.range_max);
        
        // Update the configuration
        config_ = new_config;
        
        // If crypto mixing setting changed, update the crypto mixer
        if (config_.use_crypto_mixing && !crypto_) {
            crypto_ = std::make_unique<CryptoMixer>();
            crypto_->reseed(); // Make sure we have a fresh cryptographic state
        } else if (!config_.use_crypto_mixing && crypto_) {
            crypto_.reset();
        }
        
        // Always fully clear the buffer when changing distributions or params
        if (distribution_changed || params_changed) {
            // Clear internal state
            has_spare_normal_ = false;
            spare_normal_ = 0.0;
            buffer_pos_ = buffer_.size(); // Force buffer refresh
            
            // Get truly random bytes for skipping - but much less aggressive
            SecureBuffer<unsigned char> random_bytes(sizeof(uint16_t));
            randombytes_buf(random_bytes.data(), random_bytes.size());
            uint16_t random_component = *reinterpret_cast<uint16_t*>(random_bytes.data());
            
            // Use a much smaller random skip to avoid pushing the generator into atypical phases
            // Just enough to decorrelate without excessive skipping
            uint64_t random_skip = 1000 + (random_component % 5000);
            
            // Skip ahead to ensure independence between different distributions
            for (uint64_t i = 0; i < random_skip; i++) {
                double temp = nextUniformRaw();
                (void)temp; // Prevent unused variable warning
            }
            
            // After skipping, reseed the crypto mixer if it exists
            if (crypto_) {
                crypto_->reseed();
            }
        }
        
        // Even if distribution didn't change, reset state to ensure proper initialization
        // This is important for thread safety
        reset_state();
    }

    double next() {
        if (config_.reseed_interval > 0 && sample_count_ >= config_.reseed_interval) {
            reseed();
        }

        double val=0.0;
        switch (config_.distribution) {
        case PRNGConfig::UNIFORM_01:
            val = nextUniformRaw();
            break;
        case PRNGConfig::UNIFORM_RANGE:
            val = transform_uniform_range();
            break;
        case PRNGConfig::NORMAL:
            val = nextNormal();
            break;
        case PRNGConfig::EXPONENTIAL:
            val = transform_exponential();
            break;
        default:
            throw std::runtime_error("Unknown distribution type in EnhancedPRNG::next()");
        }
        sample_count_++;
        return val;
    }

    void skip(size_t n) {
        for (size_t i=0; i<n; i++) {
            next();
        }
    }

    void reseed() {
        // Build new param set
        std::vector<std::tuple<long,long,long>> params;
        int howMany = config_.debug ? 4 : 8;  // Increased from 2/4 to 4/8
        for (int i=0; i<howMany; i++) {
            long long d = chooseUniqueDiscriminant();
            auto abc = makeABCfromDelta(d);
            params.push_back(abc);
        }
        multi_ = std::make_unique<MultiQI>(params, config_.mpfr_precision);

        if (crypto_) {
            crypto_->reseed();
        }
        sample_count_ = 0;
        buffer_pos_ = buffer_.size();
        has_spare_normal_ = false;
        offset_applied_ = false;
        skipped_ = 0;
        reset_state();
    }
};

// ---------------------------------------------------------------------
// Helper to parse R config list into PRNGConfig
PRNGConfig parse_r_config(Rcpp::List rcfg) {
    PRNGConfig cfg;
    
    if (rcfg.containsElementNamed("a")) cfg.a = Rcpp::as<long>(rcfg["a"]);
    if (rcfg.containsElementNamed("b")) cfg.b = Rcpp::as<long>(rcfg["b"]);
    if (rcfg.containsElementNamed("c")) cfg.c = Rcpp::as<long>(rcfg["c"]);
    if (rcfg.containsElementNamed("mpfr_precision")) cfg.mpfr_precision = Rcpp::as<int>(rcfg["mpfr_precision"]);
    if (rcfg.containsElementNamed("buffer_size")) cfg.buffer_size = Rcpp::as<size_t>(rcfg["buffer_size"]);
    
    if (rcfg.containsElementNamed("distribution")) {
        std::string dist = Rcpp::as<std::string>(rcfg["distribution"]);
        if (dist == "uniform_01") cfg.distribution = PRNGConfig::UNIFORM_01;
        else if (dist == "uniform_range") cfg.distribution = PRNGConfig::UNIFORM_RANGE;
        else if (dist == "normal") cfg.distribution = PRNGConfig::NORMAL;
        else if (dist == "exponential") cfg.distribution = PRNGConfig::EXPONENTIAL;
        else throw std::runtime_error("Unknown distribution: " + dist);
    }
    
    if (rcfg.containsElementNamed("range_min")) cfg.range_min = Rcpp::as<double>(rcfg["range_min"]);
    if (rcfg.containsElementNamed("range_max")) cfg.range_max = Rcpp::as<double>(rcfg["range_max"]);
    if (rcfg.containsElementNamed("normal_mean")) cfg.normal_mean = Rcpp::as<double>(rcfg["normal_mean"]);
    if (rcfg.containsElementNamed("normal_sd")) cfg.normal_sd = Rcpp::as<double>(rcfg["normal_sd"]);
    if (rcfg.containsElementNamed("exponential_lambda")) cfg.exponential_lambda = Rcpp::as<double>(rcfg["exponential_lambda"]);
    
    if (rcfg.containsElementNamed("use_crypto_mixing")) cfg.use_crypto_mixing = Rcpp::as<bool>(rcfg["use_crypto_mixing"]);
    if (rcfg.containsElementNamed("reseed_interval")) cfg.reseed_interval = Rcpp::as<unsigned long>(rcfg["reseed_interval"]);
    
    if (rcfg.containsElementNamed("offset")) cfg.offset = Rcpp::as<size_t>(rcfg["offset"]);
    if (rcfg.containsElementNamed("debug")) cfg.debug = Rcpp::as<bool>(rcfg["debug"]);
    
    return cfg;
}

// ---------------------------------------------------------------------
// chooseUniqueDiscriminant(...) and makeABCfromDelta(...)

long long chooseUniqueDiscriminant(long min_value, long max_value) {
    std::random_device rd;
    std::mt19937_64 gen(rd());
    std::uniform_int_distribution<long long> dist(min_value, max_value);

    const int MAX_ATTEMPTS = 1000;
    for (int attempt=0; attempt<MAX_ATTEMPTS; attempt++) {
        long long candidate = dist(gen);
        if (candidate <= 0) continue;

        // Quick check for approximate square-free
        // Full check:
        bool is_sf = true;
        for (long f=2; (long long)f*f <= candidate; f++) {
            if (candidate % (f*f) == 0) {
                is_sf=false; break;
            }
        }
        if (!is_sf) continue;

        {
            std::lock_guard<std::mutex> lock(g_disc_mutex);
            if (g_used_discriminants.find(candidate) == g_used_discriminants.end()) {
                g_used_discriminants.insert(candidate);
                return candidate;
            }
        }
    }
    // fallback
    std::uniform_int_distribution<long long> fallback_dist(1000000, 9999999);
    long long fallback = fallback_dist(gen);
    
    // ensure square-free
    bool done = false;
    while (!done) {
        bool is_sf = true;
        for (long f=2; (long long)f*f <= fallback; f++) {
            if (fallback % (f*f) == 0) {
                is_sf=false; break;
            }
        }
        if (!is_sf) {
            fallback = fallback_dist(gen);
        } else {
            done=true;
        }
    }
    {
        std::lock_guard<std::mutex> lock(g_disc_mutex);
        g_used_discriminants.insert(fallback);
    }
    return fallback;
}

// Attempt to solve for a=1 or a=2, b up to 200
std::tuple<long,long,long> makeABCfromDelta(long long Delta) {
    for (long b=1; b<200; b++) {
        long long num = (long long)b*b - Delta;
        if (num % 4 == 0) {
            long c = (long)(num/4);
            long long disc = (long long)b*b - 4LL*1*c;
            if (disc == Delta) {
                return std::make_tuple(1L,b,c);
            }
        }
    }
    for (long b=1; b<200; b++) {
        long long num = (long long)b*b - 2LL*Delta;
        if (num % 8 == 0) {
            long c = (long)(num/8);
            long long disc = (long long)b*b - 4LL*2*c;
            if (disc == Delta) {
                return std::make_tuple(2L,b,c);
            }
        }
    }
    // fallback
    return std::make_tuple(2L,5L,-2L);
}

// For picking e.g. 4 Qi parameters
static std::vector<std::tuple<long,long,long>> pickMultiQiSet(int mpfr_prec, int howMany=4) {
    // Different ranges for variety
    std::vector<std::tuple<long,long,long>> result;
    const std::vector<std::pair<long,long>> ranges = {
        {1000,9999}, {10000,99999}, {100000,999999}, {1000000,9999999}
    };
    for (int i=0; i<howMany; i++) {
        size_t idx = std::min((size_t)i, ranges.size()-1);
        long long d = chooseUniqueDiscriminant(ranges[idx].first, ranges[idx].second);
        auto abc = makeABCfromDelta(d);
        result.push_back(abc);
    }
    return result;
}

// Rcpp exports
// [[Rcpp::export(".createPRNG_")]]
void createPRNG_(Rcpp::List rcfg) {
    PRNGConfig config = parse_r_config(rcfg);
    
    // Validate configuration
    if (config.mpfr_precision < 24 || config.mpfr_precision > 10000) {
        throw std::runtime_error("Invalid MPFR precision: must be between 24 and 10000 bits");
    }
    
    int howMany = config.debug ? 4 : 8;  // Increased from 2/4 to 4/8
    auto qiParams = pickMultiQiSet(config.mpfr_precision, howMany);

    if (g_use_threading) {
        t_prng = std::make_unique<EnhancedPRNG>(config, qiParams);
    } else {
        std::lock_guard<std::mutex> lock(g_prng_mutex);
        g_prng = std::make_unique<EnhancedPRNG>(config, qiParams);
    }
}

// [[Rcpp::export(".updatePRNG_")]]
void updatePRNG_(Rcpp::List rcfg) {
    PRNGConfig newC = parse_r_config(rcfg);
    
    // Validate configuration
    if (newC.mpfr_precision < 24 || newC.mpfr_precision > 10000) {
        throw std::runtime_error("Invalid MPFR precision: must be between 24 and 10000 bits");
    }
    
    if (g_use_threading) {
        if (!t_prng) throw std::runtime_error("Thread-local PRNG not init");
        t_prng->updateConfig(newC);
    } else {
        std::lock_guard<std::mutex> lock(g_prng_mutex);
        if (!g_prng) throw std::runtime_error("Global PRNG not init");
        g_prng->updateConfig(newC);
    }
}

// [[Rcpp::export(".getPRNGConfig_")]]
Rcpp::List getPRNGConfig_() {
    PRNGConfig conf;
    size_t qi_count = 0;

    if (g_use_threading) {
        if (!t_prng) throw std::runtime_error("Thread-local PRNG not init");
        conf = t_prng->getConfig();
        qi_count = t_prng->getQICount();
    } else {
        std::lock_guard<std::mutex> lock(g_prng_mutex);
        if (!g_prng) throw std::runtime_error("Global PRNG not init");
        conf = g_prng->getConfig();
        qi_count = g_prng->getQICount();
    }
    std::string dist_name;
    switch (conf.distribution) {
    case PRNGConfig::UNIFORM_01: dist_name="uniform_01"; break;
    case PRNGConfig::UNIFORM_RANGE: dist_name="uniform_range"; break;
    case PRNGConfig::NORMAL: dist_name="normal"; break;
    case PRNGConfig::EXPONENTIAL: dist_name="exponential"; break;
    }
    Rcpp::List out = Rcpp::List::create(
        Rcpp::Named("a")=conf.a,
        Rcpp::Named("b")=conf.b,
        Rcpp::Named("c")=conf.c,
        Rcpp::Named("mpfr_precision")=conf.mpfr_precision,
        Rcpp::Named("buffer_size")=conf.buffer_size,
        Rcpp::Named("distribution")=dist_name,
        Rcpp::Named("range_min")=conf.range_min,
        Rcpp::Named("range_max")=conf.range_max,
        Rcpp::Named("normal_mean")=conf.normal_mean,
        Rcpp::Named("normal_sd")=conf.normal_sd,
        Rcpp::Named("exponential_lambda")=conf.exponential_lambda,
        Rcpp::Named("use_crypto_mixing")=conf.use_crypto_mixing,
        Rcpp::Named("reseed_interval")=conf.reseed_interval,
        Rcpp::Named("offset")=conf.offset,
        Rcpp::Named("debug")=conf.debug,
        Rcpp::Named("qi_count")=qi_count
    );
    return out;
}

// [[Rcpp::export(".generatePRNG_")]]
Rcpp::NumericVector generatePRNG_(int n) {
    Rcpp::NumericVector vec(n);
    if (g_use_threading) {
        if (!t_prng) throw std::runtime_error("Thread-local PRNG not init");
        for (int i=0; i<n; i++) {
            vec[i] = t_prng->next();
        }
    } else {
        std::lock_guard<std::mutex> lock(g_prng_mutex);
        if (!g_prng) throw std::runtime_error("Global PRNG not init");
        for (int i=0; i<n; i++) {
            vec[i] = g_prng->next();
        }
    }
    return vec;
}

// [[Rcpp::export(".reseedPRNG_")]]
void reseedPRNG_() {
    if (g_use_threading) {
        if (!t_prng) throw std::runtime_error("Thread-local PRNG not init");
        t_prng->reseed();
    } else {
        std::lock_guard<std::mutex> lock(g_prng_mutex);
        if (!g_prng) throw std::runtime_error("Global PRNG not init");
        g_prng->reseed();
    }
}

// [[Rcpp::export(".cleanup_prng_")]]
void cleanup_prng_() {
    {
        // also clear the global discriminant set
        std::lock_guard<std::mutex> lock(g_disc_mutex);
        g_used_discriminants.clear();
    }
    if (g_use_threading) {
        t_prng.reset();
    } else {
        std::lock_guard<std::mutex> lock(g_prng_mutex);
        g_prng.reset();
    }
}

// [[Rcpp::export(".skipPRNG_")]]
void skipPRNG_(int n) {
    if (g_use_threading) {
        if (!t_prng) throw std::runtime_error("Thread-local PRNG not init");
        t_prng->skip(static_cast<size_t>(n));
    } else {
        std::lock_guard<std::mutex> lock(g_prng_mutex);
        if (!g_prng) throw std::runtime_error("Global PRNG not init");
        g_prng->skip(static_cast<size_t>(n));
    }
}