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
#include <set>
#include <limits>
#include <atomic>

// For MPFR high-precision
#include <mpfr.h>

// For crypto operations
#include <sodium.h>

#include <chrono>

// If you have your own header with SecureBuffer, MPFRWrapper, etc.:
#include "prng_common.hpp"

using namespace qiprng;

// ---------------------------------------------------------------------
// Forward declarations
long long chooseUniqueDiscriminant(long min_value = 1000, long max_value = 999999);
std::tuple<long,long,long> makeABCfromDelta(long long Delta);
class QuadraticIrrational;
class MultiQI;
class CryptoMixer;
class EnhancedPRNG;

// ---------------------------------------------------------------------
// pickMultiQiSet
//
// A small set of known (a,b,c) with positive discriminants. We still
// rely on random skipping and random parameter tweaks so that each
// new PRNG instance is different.
std::vector<std::tuple<long, long, long>> pickMultiQiSet(int precision, int count) {
    std::vector<std::tuple<long, long, long>> result;
    
    // These are known good quadratic irrationals with positive discriminants
    // Each tuple is (a, b, c) where discriminant = b^2 - 4ac must be positive
    std::vector<std::tuple<long, long, long>> baseParams = {
        {1, 1, -1},      // disc = 5
        {2, 3, -1},      // disc = 17
        {1, 2, -1},      // disc = 8
        {1, 3, -2},      // disc = 17
        {2, 5, -3},      // disc = 49
        {3, 5, -2},      // disc = 49
        {1, 4, -3},      // disc = 28
        {2, 7, -4},      // disc = 81
        {3, 7, -4},      // disc = 97
        {1, 5, -6},      // disc = 49
        {2, 9, -10},     // disc = 161
        {3, 11, -10},    // disc = 241
        {4, 9, -5},      // disc = 161
        {5, 11, -6},     // disc = 241
        {6, 13, -7},     // disc = 337
        {7, 15, -8}      // disc = 449
    };
    
    // If user requests up to 16 QIs, just use that many from baseParams
    if (count <= static_cast<int>(baseParams.size())) {
        result.insert(result.end(), baseParams.begin(), baseParams.begin() + count);
    } else {
        // Use all base parameters
        result = baseParams;
        
        // Generate additional parameters by adding small prime-based offsets
        int remaining = count - baseParams.size();
        std::vector<int> primes = {2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53};
        
        for (int i = 0; i < remaining; i++) {
            size_t baseIdx = i % baseParams.size();
            int prime1 = primes[i % primes.size()];
            int prime2 = primes[(i + 1) % primes.size()];
            
            auto baseParam = baseParams[baseIdx];
            long a = std::get<0>(baseParam);
            long b = std::get<1>(baseParam);
            long c = std::get<2>(baseParam);
            
            a += (prime1 % 2); // small tweak
            b += prime2;       // bigger tweak
            if (c >= 0) c = -1; // ensure negative
            long long disc = (long long)b * b - 4LL * a * c;
            if (disc <= 0) {
                b += std::abs(4*a*c) + 1;
            }
            result.push_back(std::make_tuple(a, b, c));
        }
    }
    return result;
}

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
// PRNGConfig structure
struct PRNGConfig {
    enum Distribution {
        UNIFORM_01,
        UNIFORM_RANGE,
        NORMAL,
        EXPONENTIAL
    };
    
    // Quadratic parameters
    long a = 2;
    long b = 5;
    long c = -2;
    unsigned int mpfr_precision = 53;
    size_t buffer_size = 1024;

    // Distribution parameters
    Distribution distribution = UNIFORM_01;
    double range_min = 0.0;
    double range_max = 1.0;
    double normal_mean = 0.0;
    double normal_sd = 1.0;
    double exponential_lambda = 1.0;

    // Crypto & advanced
    bool use_crypto_mixing = false;
    bool adhoc_corrections  = false;  // default false
    bool use_tie_breaking = true;     // tie-break by default
    unsigned long reseed_interval = 1000;

    // Additional offset
    size_t offset = 0;

    // Debug
    bool debug = false;
};

// ---------------------------------------------------------------------
// Thread-local tie-breaker engine
static thread_local std::unique_ptr<std::mt19937_64> t_gen;
static std::mt19937_64& getThreadLocalEngine() {
    if (!t_gen) {
        std::random_device rd;
        std::array<std::uint32_t, 8> seeds;
        for (auto &x : seeds) x = rd();
        // Combine with thread ID
        auto tid = std::hash<std::thread::id>()(std::this_thread::get_id());
        seeds[0] ^= (std::uint32_t)(tid & 0xFFFFFFFFull);
        seeds[1] ^= (std::uint32_t)(tid >> 32);
        std::seed_seq seq(seeds.begin(), seeds.end());
        t_gen = std::make_unique<std::mt19937_64>(seq);
    }
    return *t_gen;
}

// ---------------------------------------------------------------------
// QuadraticIrrational: single MPFR-based Qi
class QuadraticIrrational {
private:
    long a_, b_, c_;
    std::unique_ptr<MPFRWrapper> value_, root_;
    std::unique_ptr<MPFRWrapper> next_;
    std::unique_ptr<MPFRWrapper> temp_;
    std::unique_ptr<MPFRWrapper> temp2_;

    // Single iteration
    void step_once() {
        // x_{n+1} = frac(a*x_n^2 + b*x_n + c)
        mpfr_mul(*temp_->get(), *value_->get(), *value_->get(), MPFR_RNDN);        
        mpfr_mul_si(*next_->get(), *temp_->get(), a_, MPFR_RNDN);                  
        mpfr_mul_si(*temp_->get(), *value_->get(), b_, MPFR_RNDN);                 
        mpfr_add(*next_->get(), *next_->get(), *temp_->get(), MPFR_RNDN);          
        mpfr_add_si(*next_->get(), *next_->get(), c_, MPFR_RNDN);
        mpfr_frac(*next_->get(), *next_->get(), MPFR_RNDN);

        // enforce positivity
        if (mpfr_sgn(*next_->get()) < 0) {
            mpfr_add_ui(*next_->get(), *next_->get(), 1, MPFR_RNDN);
        }
        mpfr_swap(*value_->get(), *next_->get());
    }

public:
    QuadraticIrrational(long a, long b, long c, mpfr_prec_t prec)
      : a_(a), b_(b), c_(c),
        value_(std::make_unique<MPFRWrapper>(prec)),
        root_(std::make_unique<MPFRWrapper>(prec)),
        next_(std::make_unique<MPFRWrapper>(prec)),
        temp_(std::make_unique<MPFRWrapper>(prec)),
        temp2_(std::make_unique<MPFRWrapper>(prec))
    {
        long long disc = (long long)b_*b_ - 4LL*a_*c_;
        if (disc <= 0) {
            throw std::runtime_error("QuadraticIrrational: non-positive discriminant");
        }
        // sqrt(disc)
        mpfr_set_si(*root_->get(), disc, MPFR_RNDN);
        mpfr_sqrt(*root_->get(), *root_->get(), MPFR_RNDN);

        // initial value = frac( (b + sqrt(disc)) / (2*a) )
        mpfr_set_si(*value_->get(), b_, MPFR_RNDN);
        mpfr_add(*value_->get(), *value_->get(), *root_->get(), MPFR_RNDN);
        mpfr_div_si(*value_->get(), *value_->get(), 2*a_, MPFR_RNDN);
        mpfr_frac(*value_->get(), *value_->get(), MPFR_RNDN);

        // Random skip to ensure different initial states
        // (avoid skipping huge amounts for speed; 10k is enough to decorrelate)
        std::random_device rd;
        std::mt19937_64 rng(rd());
        std::uniform_int_distribution<uint64_t> skip_dist(0, 10000);
        uint64_t skip_amt = skip_dist(rng);
        for (uint64_t i = 0; i < skip_amt; i++) {
            step_once();
        }
    }

    // Return next fraction in [0,1).
    double next() {
        step_once();
        return mpfr_get_d(*value_->get(), MPFR_RNDN);
    }

    // Simple skip
    void skip(uint64_t n) {
        for (uint64_t i = 0; i < n; i++) {
            step_once();
        }
    }
};

// ---------------------------------------------------------------------
// MultiQI
//
// Maintains a round-robin over multiple Qi sequences, each has its own state.
class MultiQI {
private:
    std::vector<QuadraticIrrational> qis_;
    size_t idx_;
public:
    MultiQI(const std::vector<std::tuple<long,long,long>>& abc_list, int mpfr_prec)
      : idx_(0)
    {
        qis_.reserve(abc_list.size());
        for (auto &abc : abc_list) {
            long A,B,C;
            std::tie(A,B,C) = abc;
            qis_.emplace_back(A, B, C, mpfr_prec);
        }
    }

    double next() {
        if (qis_.empty()) {
            return 0.5;
        }
        double val = qis_[idx_].next();
        idx_ = (idx_ + 1) % qis_.size();
        return val;
    }

    void skip(uint64_t n) {
        // naive skip by calling next() repeatedly
        for (uint64_t i = 0; i < n; i++) {
            next();
        }
    }

    size_t size() const { return qis_.size(); }

    void fill(double* buffer, size_t size) {
        for (size_t i = 0; i < size; i++) {
            buffer[i] = next();
        }
    }
};

// ---------------------------------------------------------------------
// CryptoMixer
//
// Adds cryptographic randomness to the uniform [0,1) values from Qi.
// The 'mix' uses partial random addition mod 1.  Also does tie-breaking.
class CryptoMixer {
private:
    SecureBuffer<unsigned char> key_;
    SecureBuffer<unsigned char> nonce_;
    bool adhoc_corrections_;
    bool use_tie_breaking_;

    void secure_random(unsigned char* buf, size_t len) {
        randombytes_buf(buf, len);
    }

public:
    static constexpr uint64_t MANTISSA_MASK = 0x000FFFFFFFFFFFFFULL;
    static constexpr uint64_t ONE_BITS      = 0x3FF0000000000000ULL;

    CryptoMixer(bool adhoc_corrections, bool use_tie_breaking)
      : key_(crypto_stream_chacha20_KEYBYTES),
        nonce_(crypto_stream_chacha20_NONCEBYTES),
        adhoc_corrections_(adhoc_corrections),
        use_tie_breaking_(use_tie_breaking)
    {
        reseed();
    }

    ~CryptoMixer() {
        // SecureBuffer auto-cleans
    }

    void reseed() {
        secure_random(key_.data(), key_.size());
        secure_random(nonce_.data(), nonce_.size());
    }

    bool mix(unsigned char* data, size_t len) {
        if (!data || len == 0 || len % sizeof(double) != 0) {
            return false;
        }
        size_t num_doubles = len / sizeof(double);
        double* doubles = reinterpret_cast<double*>(data);

        SecureBuffer<unsigned char> random_bytes(len);
        randombytes_buf(random_bytes.data(), len);

        // We'll keep "adhoc_corrections_" as an alternative approach
        if (!adhoc_corrections_) {
            // partial modular addition approach
            double prev_val = -1.0; // track last for tie detection
            for (size_t i = 0; i < num_doubles; i++) {
                // Convert random bits to double in [0,1)
                uint64_t crypto_bits = 0;
                std::memcpy(&crypto_bits, &random_bytes[i * sizeof(double)], sizeof(uint64_t));
                crypto_bits = (crypto_bits & MANTISSA_MASK) | ONE_BITS;
                double crypto_uniform = *reinterpret_cast<double*>(&crypto_bits) - 1.0;

                // Add mod 1
                doubles[i] = std::fmod(doubles[i] + crypto_uniform, 1.0);
                if (doubles[i] < 0.0) {
                    doubles[i] += 1.0;
                }

                // If tie-breaking is on, and the new value is exactly the same as the previous,
                // nudge it by a small random epsilon
                if (use_tie_breaking_) {
                    if (i > 0 && doubles[i] == prev_val) {
                        static thread_local std::uniform_real_distribution<double> tiny_dist(-1e-14, 1e-14);
                        double eps = tiny_dist(getThreadLocalEngine());
                        doubles[i] += eps;
                        // clamp to (0,1)
                        if (doubles[i] >= 1.0) doubles[i] = std::nextafter(1.0, 0.0);
                        if (doubles[i] < 0.0)  doubles[i] = std::nextafter(0.0, 1.0);
                    }
                }
                prev_val = doubles[i];
            }
        } else {
            // older partial averaging approach
            double prev_val = -1.0;
            for (size_t i = 0; i < num_doubles; i++) {
                uint64_t random_bits = 0;
                std::memcpy(&random_bits, &random_bytes[i * sizeof(double)], sizeof(uint64_t));
                uint64_t bits = (random_bits & MANTISSA_MASK) | ONE_BITS;
                double crypto_uniform = *reinterpret_cast<double*>(&bits) - 1.0;

                doubles[i] = 0.5 * doubles[i] + 0.5 * crypto_uniform;

                // tie break
                if (use_tie_breaking_ && i > 0 && doubles[i] == prev_val) {
                    static thread_local std::uniform_real_distribution<double> tiny_dist(-1e-14, 1e-14);
                    double eps = tiny_dist(getThreadLocalEngine());
                    doubles[i] += eps;
                }
                if (doubles[i] >= 1.0) doubles[i] = 1.0 - 1e-15;
                if (doubles[i] < 0.0)  doubles[i] = 1e-15;

                prev_val = doubles[i];
            }
        }
        return true;
    }
};

// ---------------------------------------------------------------------
// EnhancedPRNG
class EnhancedPRNG {
private:
    PRNGConfig config_;
    std::unique_ptr<MultiQI> multi_;
    std::unique_ptr<CryptoMixer> crypto_;
    SecureBuffer<double> buffer_;
    size_t buffer_pos_;
    size_t sample_count_;

    // For normal distribution using Box–Muller
    bool has_spare_normal_;
    double spare_normal_;

    bool offset_applied_;
    size_t skipped_;

    // Force refill of buffer on next use
    void reset_state() {
        buffer_.resize(config_.buffer_size);
        buffer_pos_ = buffer_.size(); // trigger refill
        has_spare_normal_ = false;
        spare_normal_ = 0.0;
        sample_count_ = 0;
        skipped_ = 0;

        offset_applied_ = false;
        if (crypto_) {
            crypto_->reseed();
        }
        if (config_.offset > 0 && multi_) {
            multi_->skip(config_.offset);
            offset_applied_ = true;
        }
    }

    // Refill buffer with base uniform(0,1), then optionally cryptomix
    void fill_buffer() {
        if (!multi_) {
            throw std::runtime_error("MultiQI not initialized");
        }
        multi_->fill(buffer_.data(), buffer_.size());
        buffer_pos_ = 0;

        if (config_.use_crypto_mixing && crypto_) {
            crypto_->mix(reinterpret_cast<unsigned char*>(buffer_.data()),
                         buffer_.size() * sizeof(double));
        }
    }

    // Standard Box-Muller to produce two normals from (u1,u2)
    // We'll return z0, store z1 in spare.
    std::pair<double,double> box_muller_pair(double u1, double u2) {
        const double epsilon = 1e-14; // avoid log(0)
        if (u1 < epsilon)  u1 = epsilon;
        if (u1 > 1.0 - epsilon) u1 = 1.0 - epsilon;
        if (u2 < epsilon)  u2 = epsilon;
        if (u2 > 1.0 - epsilon) u2 = 1.0 - epsilon;

        double r = std::sqrt(-2.0 * std::log(u1));
        double theta = 2.0 * M_PI * u2;
        double z0 = r * std::cos(theta);
        double z1 = r * std::sin(theta);
        // apply mean & sd
        z0 = z0 * config_.normal_sd + config_.normal_mean;
        z1 = z1 * config_.normal_sd + config_.normal_mean;
        return {z0, z1};
    }

    // Transform uniform -> exponential
    double uniform_to_exponential(double u) {
        const double epsilon = 1e-14;
        if (u < epsilon)  u = epsilon;
        if (u > 1.0 - epsilon) u = 1.0 - epsilon;
        return -std::log(u) / config_.exponential_lambda;
    }

public:
    EnhancedPRNG(const PRNGConfig& cfg,
                 const std::vector<std::tuple<long,long,long>>& abc_list)
      : config_(cfg),
        multi_(std::make_unique<MultiQI>(abc_list, config_.mpfr_precision)),
        buffer_(config_.buffer_size),
        buffer_pos_(config_.buffer_size),
        sample_count_(0),
        has_spare_normal_(false),
        spare_normal_(0.0),
        offset_applied_(false),
        skipped_(0)
    {
        // optionally create CryptoMixer
        if (config_.use_crypto_mixing) {
            crypto_ = std::make_unique<CryptoMixer>(config_.adhoc_corrections, config_.use_tie_breaking);
        }
        reset_state();
    }

    ~EnhancedPRNG() = default;

    const PRNGConfig& getConfig() const {
        return config_;
    }

    size_t getQICount() const {
        return multi_ ? multi_->size() : 0;
    }

    // Update config in place, resetting if distribution or key parameters changed
    void updateConfig(const PRNGConfig& new_config) {
        PRNGConfig old_config = config_;
        bool distribution_changed = (old_config.distribution != new_config.distribution);
        bool params_changed = (old_config.a != new_config.a ||
                              old_config.b != new_config.b ||
                              old_config.c != new_config.c);
        bool range_changed = (old_config.range_min != new_config.range_min ||
                             old_config.range_max != new_config.range_max);
        bool normal_changed = (old_config.normal_mean != new_config.normal_mean ||
                               old_config.normal_sd   != new_config.normal_sd);
        bool exp_changed = (old_config.exponential_lambda != new_config.exponential_lambda);

        config_ = new_config;

        // Re-init crypto mixer if needed
        if (config_.use_crypto_mixing && !crypto_) {
            crypto_ = std::make_unique<CryptoMixer>(config_.adhoc_corrections, config_.use_tie_breaking);
            crypto_->reseed();
        } else if (!config_.use_crypto_mixing && crypto_) {
            crypto_.reset();
        } else if (crypto_ &&
                   (config_.adhoc_corrections != old_config.adhoc_corrections ||
                    config_.use_tie_breaking  != old_config.use_tie_breaking)) {
            // re-create
            crypto_ = std::make_unique<CryptoMixer>(config_.adhoc_corrections, config_.use_tie_breaking);
            crypto_->reseed();
        }

        // If main distribution or parameters changed, recreate MultiQI
        if (distribution_changed || params_changed || range_changed ||
            normal_changed || exp_changed)
        {
            // We'll reset the entire state, forcing a new sequence
            reset_state();
            buffer_pos_ = buffer_.size(); // force refill
        }
    }

    // Main sample function
    double next() {
        // Reseed if we've hit the interval
        if (config_.reseed_interval > 0 && sample_count_ >= config_.reseed_interval) {
            reseed();
            sample_count_ = 0;
        }

        if (buffer_pos_ >= buffer_.size()) {
            fill_buffer();
        }
        double u = buffer_[buffer_pos_++];
        sample_count_++;

        switch(config_.distribution) {
        case PRNGConfig::UNIFORM_01:
            return u;

        case PRNGConfig::UNIFORM_RANGE:
            return config_.range_min + (config_.range_max - config_.range_min)*u;

        case PRNGConfig::NORMAL: {
            if (!has_spare_normal_) {
                // get next uniform for the second normal
                double u2 = (buffer_pos_ >= buffer_.size()) ?
                    (fill_buffer(), buffer_[buffer_pos_++])
                    : buffer_[buffer_pos_++];
                sample_count_++;
                // produce two normals
                auto pairz = box_muller_pair(u, u2);
                has_spare_normal_ = true;
                spare_normal_ = pairz.second;
                return pairz.first;
            } else {
                has_spare_normal_ = false;
                return spare_normal_;
            }
        }

        case PRNGConfig::EXPONENTIAL:
            return uniform_to_exponential(u);

        default:
            throw std::runtime_error("Unknown distribution type");
        }
    }

    // Skip n draws
    void skip(size_t n) {
        size_t diff = (buffer_pos_ < buffer_.size())
                        ? (buffer_.size() - buffer_pos_)
                        : 0;
        if (n <= diff) {
            // just move pointer
            buffer_pos_ += n;
        } else {
            n -= diff;
            size_t fullbuf = n / buffer_.size();
            size_t remainder = n % buffer_.size();

            // skip in the underlying Qi
            if (multi_) {
                multi_->skip(fullbuf * buffer_.size());
            }
            // refill
            buffer_pos_ = buffer_.size();
            if (remainder > 0) {
                fill_buffer();
                buffer_pos_ = remainder;
            }
        }
        skipped_ += n;
    }

    // Reseed => re-randomize Qi with pickMultiQiSet(...) again + cryptomixer
    void reseed() {
        if (crypto_) {
            crypto_->reseed();
        }
        if (multi_) {
            int howMany = config_.debug ? 16 : 32;
            auto new_params = pickMultiQiSet(config_.mpfr_precision, howMany);

            // For a new random twist, let's also apply some random perturbation
            // to each param set so that repeated calls to reseed() differ:
            {
                static std::random_device srd;
                static std::mt19937_64 sgen(srd());
                std::uniform_int_distribution<long> shift_dist(1, 99999);

                for (auto &p : new_params) {
                    long shiftA = shift_dist(sgen) % 10;   // small tweak
                    long shiftB = shift_dist(sgen) % 500;  // bigger tweak
                    std::get<0>(p) += shiftA;
                    std::get<1>(p) += shiftB;
                    if (std::get<2>(p) >= 0) {
                        std::get<2>(p) = -1; 
                    }
                }
            }

            multi_ = std::make_unique<MultiQI>(new_params, config_.mpfr_precision);
        }
        reset_state();
    }

    // Debug print
    void dumpConfig() const {
        Rcpp::Rcout << "PRNG Configuration:\n";
        Rcpp::Rcout << "-------------------\n";
        Rcpp::Rcout << "abc: (" << config_.a << ", " << config_.b << ", " << config_.c << ")\n";
        Rcpp::Rcout << "MPFR precision: " << config_.mpfr_precision << " bits\n";
        Rcpp::Rcout << "Distribution: ";
        switch(config_.distribution) {
            case PRNGConfig::UNIFORM_01:
                Rcpp::Rcout << "uniform(0,1)\n"; break;
            case PRNGConfig::UNIFORM_RANGE:
                Rcpp::Rcout << "uniform(" << config_.range_min 
                            << ", " << config_.range_max << ")\n";
                break;
            case PRNGConfig::NORMAL:
                Rcpp::Rcout << "normal(mean=" << config_.normal_mean
                            << ", sd=" << config_.normal_sd << ")\n";
                break;
            case PRNGConfig::EXPONENTIAL:
                Rcpp::Rcout << "exponential(lambda=" << config_.exponential_lambda 
                            << ") => mean=" << (1.0 / config_.exponential_lambda) << "\n";
                break;
        }
        Rcpp::Rcout << "Use crypto mixing: " << (config_.use_crypto_mixing ? "yes" : "no") << "\n";
        Rcpp::Rcout << "Ad-hoc corrections: " << (config_.adhoc_corrections ? "yes" : "no") << "\n";
        Rcpp::Rcout << "Use tie-breaking: " << (config_.use_tie_breaking ? "yes" : "no") << "\n";
        Rcpp::Rcout << "Reseed interval: " << config_.reseed_interval << "\n";
        Rcpp::Rcout << "Offset: " << config_.offset << "\n";
        Rcpp::Rcout << "Buffer size: " << config_.buffer_size << "\n";
        Rcpp::Rcout << "QI count: " << (multi_ ? multi_->size() : 0) << "\n";
        Rcpp::Rcout << "-------------------\n";
    }
};

// ---------------------------------------------------------------------
// Rcpp interface

static PRNGConfig parseFromR(Rcpp::List rcfg) {
    PRNGConfig config;

    auto parse_flag = [&](const char* name, bool &field) {
        if (rcfg.containsElementNamed(name)) {
            field = Rcpp::as<bool>(rcfg[name]);
        }
    };
    auto parse_long_ = [&](const char* name, long &field) {
        if (rcfg.containsElementNamed(name)) {
            field = Rcpp::as<long>(rcfg[name]);
        }
    };
    auto parse_uint_ = [&](const char* name, unsigned int &field) {
        if (rcfg.containsElementNamed(name)) {
            field = Rcpp::as<unsigned int>(rcfg[name]);
        }
    };
    auto parse_size_ = [&](const char* name, size_t &field) {
        if (rcfg.containsElementNamed(name)) {
            field = Rcpp::as<size_t>(rcfg[name]);
        }
    };
    auto parse_ulong_ = [&](const char* name, unsigned long &field) {
        if (rcfg.containsElementNamed(name)) {
            field = Rcpp::as<unsigned long>(rcfg[name]);
        }
    };
    auto parse_double_ = [&](const char* name, double &field) {
        if (rcfg.containsElementNamed(name)) {
            field = Rcpp::as<double>(rcfg[name]);
        }
    };

    parse_long_("a", config.a);
    parse_long_("b", config.b);
    parse_long_("c", config.c);
    parse_uint_("mpfr_precision", config.mpfr_precision);
    if (rcfg.containsElementNamed("distribution")) {
        std::string dist = Rcpp::as<std::string>(rcfg["distribution"]);
        if (dist == "uniform_01") config.distribution = PRNGConfig::UNIFORM_01;
        else if (dist == "uniform_range") config.distribution = PRNGConfig::UNIFORM_RANGE;
        else if (dist == "normal") config.distribution = PRNGConfig::NORMAL;
        else if (dist == "exponential") config.distribution = PRNGConfig::EXPONENTIAL;
        else
            throw std::runtime_error("Unknown distribution: " + dist);
    }
    parse_double_("range_min", config.range_min);
    parse_double_("range_max", config.range_max);
    parse_double_("normal_mean", config.normal_mean);
    parse_double_("normal_sd", config.normal_sd);
    parse_double_("exponential_lambda", config.exponential_lambda);
    parse_flag("use_crypto_mixing", config.use_crypto_mixing);
    parse_flag("adhoc_corrections", config.adhoc_corrections);
    parse_flag("use_tie_breaking", config.use_tie_breaking);
    parse_ulong_("reseed_interval", config.reseed_interval);
    parse_size_("buffer_size", config.buffer_size);
    parse_size_("offset", config.offset);
    parse_flag("debug", config.debug);

    if (config.mpfr_precision < 24 || config.mpfr_precision > 10000) {
        throw std::runtime_error("Invalid MPFR precision: must be 24..10000 bits");
    }
    return config;
}

// [[Rcpp::export(".createPRNG_")]]
void createPRNG_(Rcpp::List rcfg) {
    PRNGConfig config = parseFromR(rcfg);
    int howMany = config.debug ? 16 : 32;
    auto qiParams = pickMultiQiSet(config.mpfr_precision, howMany);

    // If user is in multi-thread mode, create a thread-local PRNG
    if (g_use_threading) {
        std::thread::id this_id = std::this_thread::get_id();
        size_t thread_hash = std::hash<std::thread::id>()(this_id);

        // Additional randomization for the QI parameters
        std::random_device rd;
        std::mt19937_64 rng(rd());
        std::uniform_int_distribution<long> shift_dist(1, 99999);

        for (size_t i = 0; i < qiParams.size(); i++) {
            auto &p = qiParams[i];
            long a = std::get<0>(p);
            long b = std::get<1>(p);
            long c = std::get<2>(p);

            // Simple random tweak
            a += (shift_dist(rng) % 7);
            b += (shift_dist(rng) % 200);
            if (c >= 0) c = -1;

            // Also incorporate thread_hash
            if (thread_hash & 1) {
                a += 5;
                b += 11;
            } else {
                a += 3;
                b += 7;
            }
            qiParams[i] = std::make_tuple(a, b, c);
        }

        t_prng = std::make_unique<EnhancedPRNG>(config, qiParams);
        // Force fresh reseed
        t_prng->reseed();
    } else {
        // Single global
        std::lock_guard<std::mutex> lock(g_prng_mutex);

        // Also randomize parameters so multiple createPRNG_ calls differ
        {
            static std::random_device srd;
            static std::mt19937_64 sgen(srd());
            std::uniform_int_distribution<long> shift_dist(1, 99999);

            for (auto &p : qiParams) {
                long a = std::get<0>(p);
                long b = std::get<1>(p);
                long c = std::get<2>(p);

                a += (shift_dist(sgen) % 5);
                b += (shift_dist(sgen) % 200);
                if (c >= 0) c = -1;

                p = std::make_tuple(a, b, c);
            }
        }

        g_prng = std::make_unique<EnhancedPRNG>(config, qiParams);
    }
}

// [[Rcpp::export(".updatePRNG_")]]
void updatePRNG_(Rcpp::List rcfg) {
    PRNGConfig newC = parseFromR(rcfg);

    if (g_use_threading) {
        if (!t_prng) {
            throw std::runtime_error("Thread-local PRNG not init");
        }
        t_prng->updateConfig(newC);
    } else {
        std::lock_guard<std::mutex> lock(g_prng_mutex);
        if (!g_prng) {
            throw std::runtime_error("Global PRNG not init");
        }
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
    switch(conf.distribution) {
    case PRNGConfig::UNIFORM_01:    dist_name = "uniform_01";     break;
    case PRNGConfig::UNIFORM_RANGE: dist_name = "uniform_range";  break;
    case PRNGConfig::NORMAL:        dist_name = "normal";         break;
    case PRNGConfig::EXPONENTIAL:   dist_name = "exponential";    break;
    }

    return Rcpp::List::create(
        Rcpp::Named("a") = conf.a,
        Rcpp::Named("b") = conf.b,
        Rcpp::Named("c") = conf.c,
        Rcpp::Named("mpfr_precision") = conf.mpfr_precision,
        Rcpp::Named("buffer_size") = conf.buffer_size,
        Rcpp::Named("distribution") = dist_name,
        Rcpp::Named("range_min") = conf.range_min,
        Rcpp::Named("range_max") = conf.range_max,
        Rcpp::Named("normal_mean") = conf.normal_mean,
        Rcpp::Named("normal_sd") = conf.normal_sd,
        Rcpp::Named("exponential_lambda") = conf.exponential_lambda,
        Rcpp::Named("use_crypto_mixing") = conf.use_crypto_mixing,
        Rcpp::Named("adhoc_corrections") = conf.adhoc_corrections,
        Rcpp::Named("use_tie_breaking") = conf.use_tie_breaking,
        Rcpp::Named("reseed_interval") = conf.reseed_interval,
        Rcpp::Named("offset") = conf.offset,
        Rcpp::Named("debug") = conf.debug,
        Rcpp::Named("qi_count") = (int)qi_count
    );
}

// [[Rcpp::export(".dumpPRNGConfig_")]]
void dumpPRNGConfig_() {
    if (g_use_threading) {
        if (t_prng) {
            t_prng->dumpConfig();
        } else {
            Rcpp::Rcout << "Thread-local PRNG not initialized\n";
        }
    } else {
        std::lock_guard<std::mutex> lock(g_prng_mutex);
        if (g_prng) {
            g_prng->dumpConfig();
        } else {
            Rcpp::Rcout << "Global PRNG not initialized\n";
        }
    }
}

// [[Rcpp::export(".generatePRNG_")]]
Rcpp::NumericVector generatePRNG_(int n) {
    Rcpp::NumericVector vec(n);

    if (g_use_threading) {
        if (!t_prng) throw std::runtime_error("Thread-local PRNG not init");
        for (int i = 0; i < n; i++) {
            vec[i] = t_prng->next();
        }
    } else {
        std::lock_guard<std::mutex> lock(g_prng_mutex);
        if (!g_prng) throw std::runtime_error("Global PRNG not init");
        for (int i = 0; i < n; i++) {
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

// [[Rcpp::export(".jumpAheadPRNG_")]]
void jumpAheadPRNG_(double n) {
    if (n <= 0) return;
    uint64_t steps = static_cast<uint64_t>(n);
    if (g_use_threading) {
        if (!t_prng) throw std::runtime_error("Thread-local PRNG not init");
        t_prng->skip(steps);
    } else {
        std::lock_guard<std::mutex> lock(g_prng_mutex);
        if (!g_prng) throw std::runtime_error("Global PRNG not init");
        g_prng->skip(steps);
    }
}

// ---------------------------------------------------------------------
// chooseUniqueDiscriminant(...) and makeABCfromDelta(...)

// Not heavily used in the tests, but kept for completeness.
long long chooseUniqueDiscriminant(long min_value, long max_value) {
    std::random_device rd;
    std::mt19937_64 gen(rd());
    std::uniform_int_distribution<long long> dist(min_value, max_value);

    const int MAX_ATTEMPTS = 8000;
    for (int attempt = 0; attempt < MAX_ATTEMPTS; attempt++) {
        long long candidate = dist(gen);
        if (candidate <= 0) continue;

        // Check if square-free up to some limit
        bool is_sf = true;
        long upper = (long)(std::sqrt((long double)candidate)) + 1;
        for (long f = 2; f <= std::min(upper, 1000L); f++) {
            if (candidate % (f*f) == 0) {
                is_sf = false;
                break;
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
    static const std::vector<long long> known_good = {
        1234567, 7654321, 9987643, 1237893, 9876541,
        2345671, 7543217, 9876541, 1235711, 2468101,
        8765431, 9876549, 1293749, 2345671, 8765431
    };
    std::uniform_int_distribution<size_t> idx_dist(0, known_good.size() - 1);
    long long fallback = known_good[idx_dist(gen)];
    {
        std::lock_guard<std::mutex> lock(g_disc_mutex);
        g_used_discriminants.insert(fallback);
    }
    return fallback;
}

std::tuple<long,long,long> makeABCfromDelta(long long Delta) {
    static const std::vector<long> a_vals = {1,2,3,5,7,11};
    for (long a : a_vals) {
        for (long b = 1; b < 300; b++) {
            long long num = (long long)b*b - 4LL*a*Delta;
            if (num % (4*a) == 0) {
                long c = (long)(num / (4*a));
                long long dcheck = (long long)b*b - 4LL*a*c;
                if (dcheck == Delta) {
                    return {a,b,c};
                }
            }
        }
    }
    // fallback
    static const std::vector<std::tuple<long,long,long>> fallbackABC = {
        {2,5,-3}, {3,7,-2}, {5,11,-2}, {1,13,-11}
    };
    std::random_device rd;
    std::mt19937 gen(rd());
    std::uniform_int_distribution<size_t> idxd(0, fallbackABC.size() - 1);
    return fallbackABC[idxd(gen)];
}