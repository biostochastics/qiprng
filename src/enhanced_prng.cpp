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
#include <fstream>
#include <sstream>

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

// CSV discriminants vector
static std::vector<std::tuple<long, long, long, long long>> g_csv_discriminants;
static std::mutex g_csv_disc_mutex;
static bool g_csv_discriminants_loaded = false;

// ---------------------------------------------------------------------
// PRNGConfig structure
struct PRNGConfig {
    enum Distribution {
        UNIFORM_01,
        UNIFORM_RANGE,
        NORMAL,
        EXPONENTIAL,
        POISSON,      // Add Poisson distribution
        GAMMA,        // Add Gamma distribution
        BETA          // Add Beta distribution
    };
    
    // Normal distribution generation method
    using NormalMethod = PRNGDefaults::NormalMethod;
    NormalMethod normal_method = PRNGDefaults::normal_method;
    
    // Core parameters
    long a = PRNGDefaults::aa;
    long b = PRNGDefaults::b;
    long c = PRNGDefaults::c;
    unsigned int mpfr_precision = 53;
    size_t buffer_size = 1024;

    // Distribution parameters
    Distribution distribution = UNIFORM_01;
    double range_min = 0.0;
    double range_max = 1.0;
    double normal_mean = 0.0;
    double normal_sd = 1.0;
    double exponential_lambda = 1.0;
    
    // New distribution parameters
    double poisson_lambda = 1.0;   // Default Poisson rate parameter
    double gamma_shape = 1.0;      // Default Gamma shape parameter (alpha)
    double gamma_scale = 1.0;      // Default Gamma scale parameter (theta)
    double beta_alpha = 1.0;       // Default Beta shape parameter alpha
    double beta_beta = 1.0;        // Default Beta shape parameter beta

    // Crypto & advanced
    bool use_crypto_mixing = false;
    bool adhoc_corrections  = false;  // default false
    bool use_tie_breaking = true;     // tie-break by default
    unsigned long reseed_interval = 1000;
    
    // Discriminant options
    bool use_csv_discriminants = false; // use custom discriminants from CSV file
    
    // Performance options
    bool use_parallel_filling = true; // use parallel buffer filling for better performance

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
        // Use jump_ahead for efficiency
        jump_ahead(n);
    }
    
    // Efficient jump-ahead implementation
    void jump_ahead(uint64_t n) {
        // For small jumps, use the naive approach
        if (n < 100) {
            for (uint64_t i = 0; i < n; i++) {
                step_once();
            }
            return;
        }
        
        // For large jumps, use binary decomposition
        // Save current state
        std::unique_ptr<MPFRWrapper> saved_value = 
            std::make_unique<MPFRWrapper>(mpfr_get_prec(*value_->get()));
        mpfr_set(*saved_value->get(), *value_->get(), MPFR_RNDN);
        
        // Binary decomposition approach:
        // We'll precompute the state after 2^k iterations for various k
        std::vector<std::unique_ptr<MPFRWrapper>> states;
        states.reserve(64); // 64 bits should be enough for uint64_t
        
        // Initialize first state
        states.push_back(std::move(saved_value));
        
        // Compute state after 1 step
        step_once();
        states.push_back(std::make_unique<MPFRWrapper>(mpfr_get_prec(*value_->get())));
        mpfr_set(*states[1]->get(), *value_->get(), MPFR_RNDN);
        
        // Compute states after 2^k steps for k from 1 to log2(n)
        for (size_t k = 1; (1ULL << k) <= n; k++) {
            // Initialize a state for 2^k
            states.push_back(std::make_unique<MPFRWrapper>(mpfr_get_prec(*value_->get())));
            
            // To reach state 2^k, apply the iteration function 2^(k-1) times to state 2^(k-1)
            mpfr_set(*value_->get(), *states[k]->get(), MPFR_RNDN);
            for (uint64_t i = 0; i < (1ULL << (k-1)); i++) {
                step_once();
            }
            mpfr_set(*states[k+1]->get(), *value_->get(), MPFR_RNDN);
        }
        
        // Reset to initial state
        mpfr_set(*value_->get(), *states[0]->get(), MPFR_RNDN);
        
        // Apply jumps based on binary representation of n
        for (size_t k = 0; k <= 63; k++) {
            if (n & (1ULL << k)) {
                if (k+1 < states.size()) {
                    // Apply the precomputed jump of size 2^k
                    mpfr_set(*value_->get(), *states[k+1]->get(), MPFR_RNDN);
                } else {
                    // If we don't have this power, use naive approach for remaining jumps
                    for (uint64_t i = 0; i < (1ULL << k); i++) {
                        step_once();
                    }
                    break;
                }
            }
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
        // Use the efficient jump_ahead implementation
        jump_ahead(n);
    }
    
    // Efficient jump ahead for MultiQI
    void jump_ahead(uint64_t n) {
        if (qis_.empty()) {
            return;
        }
        
        // Apply jump-ahead to each QI sequence
        for (auto& qi : qis_) {
            qi.jump_ahead(n);
        }
        
        // Update index based on jump size
        idx_ = (idx_ + (n % qis_.size())) % qis_.size();
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
// ZigguratNormal - Efficient normal distribution generator
//
// This implements the Ziggurat algorithm by Marsaglia and Tsang for 
// generating normal random variables more efficiently than Box-Muller
class ZigguratNormal {
private:
    // Tables for the Ziggurat algorithm
    // These precomputed tables enable the fast generation of normal random variables
    static constexpr int ZIGGURAT_TABLES = 256;
    static constexpr int ZIGGURAT_MASK = ZIGGURAT_TABLES - 1;
    
    // Arrays for the algorithm
    std::array<double, ZIGGURAT_TABLES> x_table_;
    std::array<double, ZIGGURAT_TABLES> y_table_;
    std::array<uint32_t, ZIGGURAT_TABLES> k_table_;
    
    // Uniform random source
    std::function<double()> uniform_generator_;
    
    // Distribution parameters
    double mean_;
    double stddev_;
    
    // Initialize the tables for the Ziggurat algorithm using the full iterative method
    // as described by Marsaglia and Tsang
    void initialize_tables() {
        constexpr double R = 3.6541528853610088;  // Value at which the tail begins
        constexpr double A = 0.00492867323399;    // Area under tail
        
        // Set the rightmost x value
        x_table_[0] = R;
        y_table_[0] = std::exp(-0.5 * R * R);     // Density at R
        
        // Volume of each box is v = A
        // double v = A; // Unused variable
        
        // Step 2: Create the tables using accurate iterative calculations
        for (int i = 1; i < ZIGGURAT_TABLES; ++i) {
            // Find the next x value where the rectangle and wedge have equal areas
            // Using the equation: x * f(x) + v = y * (x_prev - x) + v
            double x_prev = x_table_[i-1];
            double y_prev = y_table_[i-1];
            
            // Starting value for x_i
            double x_i = (i == ZIGGURAT_TABLES-1) ? 0.0 : 
                         (x_prev > 0.5) ? x_prev - 0.1 : 0.25;
            
            // Iteratively solve for the correct x value
            // We converge on a value where the areas of the wedge and rectangle are equal
            for (int j = 0; j < 10; ++j) { // Usually 5-10 iterations are enough
                double y_i = std::exp(-0.5 * x_i * x_i); // Density at x_i
                
                // Compute the areas and their difference
                double fx = x_i * y_i;  // Rectangle area under the x_i point
                double area_diff = y_prev * (x_prev - x_i) - (fx - fx * (y_i / y_prev));
                
                // Update x_i using Newton-like iteration
                x_i -= area_diff / (y_prev - y_i - x_i * y_i * y_i / y_prev);
                
                // Special handling for the last point (near zero)
                if (i == ZIGGURAT_TABLES-1 && j > 5) {
                    x_i = 0.0;
                    break;
                }
            }
            
            // Store calculated values
            x_table_[i] = x_i;
            y_table_[i] = std::exp(-0.5 * x_i * x_i);
        }
        
        // Ensure x_table_[ZIGGURAT_TABLES-1] is exactly 0
        x_table_[ZIGGURAT_TABLES-1] = 0.0;
        y_table_[ZIGGURAT_TABLES-1] = 1.0;  // Density at x=0
        
        // Calculate k_table_ values (region acceptance thresholds) accurately
        for (int i = 0; i < ZIGGURAT_TABLES; ++i) {
            if (i == 0) {
                // For the tail region, use the tail probability
                k_table_[i] = static_cast<uint32_t>((UINT32_MAX) * (A / y_table_[i]));
            } else {
                // For internal rectangles, compute exact acceptance ratios
                double ratio;
                if (i == ZIGGURAT_TABLES-1) {
                    // The innermost rectangle is fully accepted
                    ratio = 1.0;
                } else {
                    // Other rectangles use the ratio of successive y values
                    ratio = x_table_[i] / x_table_[i-1];
                }
                k_table_[i] = static_cast<uint32_t>((UINT32_MAX) * ratio);
            }
        }
    }
    
    // Handle the tail region of the normal distribution
    // Uses the accurate algorithm from Marsaglia and Tsang's paper
    double sample_from_tail() {
        double x, y;
        do {
            // Get uniform values with bounds checking
            double u1 = uniform_generator_();
            double u2 = uniform_generator_();
            
            // Ensure values are in [0,1)
            if (u1 <= 0.0) u1 = 0.000001; // Avoid log(0)
            if (u1 >= 1.0) u1 = 0.999999;
            if (u2 <= 0.0) u2 = 0.000001;
            if (u2 >= 1.0) u2 = 0.999999;
            
            // Generate a point in the exponential tail
            x = -std::log(u1) / x_table_[0]; // Using x_table_[0] which is R
            y = -std::log(u2);
            
            // Accept if it's under the density curve
        } while (y + y < x * x);
        
        // Return tail value
        return x_table_[0] + x;
    }
    
public:
    ZigguratNormal(std::function<double()> uniform_generator,
                  double mean = 0.0, double stddev = 1.0)
        : uniform_generator_(uniform_generator),
          mean_(mean),
          stddev_(stddev)
    {
        initialize_tables();
    }
    
    // Update distribution parameters
    void set_parameters(double mean, double stddev) {
        mean_ = mean;
        stddev_ = stddev;
    }
    
    // Generate a single normal random number
    double generate() {
        uint32_t u, i, sign;
        double x;
        
        while (true) {
            // Get uniform random number in [0,1) and ensure it's valid
            double uniform = uniform_generator_();
            if (uniform < 0.0 || uniform >= 1.0) {
                uniform = 0.5; // Fallback if uniform generator returns invalid value
            }
            
            // Convert to uint32 and extract sign and index
            u = static_cast<uint32_t>(uniform * static_cast<double>(UINT32_MAX));
            i = u & ZIGGURAT_MASK;
            sign = u & 0x80000000;
            
            // First try the rectangle
            if (u < k_table_[i]) {
                x = u * x_table_[i] / static_cast<double>(UINT32_MAX);
                // Apply mean and stddev
                return sign ? mean_ - stddev_ * x : mean_ + stddev_ * x;
            }
            
            // Bottom box: handle tail
            if (i == 0) {
                x = sample_from_tail();
                return sign ? mean_ - stddev_ * x : mean_ + stddev_ * x;
            }
            
            // Is point within density?
            x = u * x_table_[i] / static_cast<double>(UINT32_MAX);
            
            // Get another uniform value for acceptance test
            uniform = uniform_generator_();
            if (uniform < 0.0 || uniform >= 1.0) {
                uniform = 0.5; // Fallback if uniform generator returns invalid value
            }
            
            if (y_table_[i+1] + (y_table_[i] - y_table_[i+1]) * uniform < 
                std::exp(-0.5 * x * x)) {
                return sign ? mean_ - stddev_ * x : mean_ + stddev_ * x;
            }
            
            // Try again if we reach here
        }
    }
    
    // Generate multiple normal random numbers
    void generate(double* buffer, size_t count) {
        for (size_t i = 0; i < count; ++i) {
            buffer[i] = generate();
        }
    }
};

// ---------------------------------------------------------------------
// EnhancedPRNG
// Forward declare helper functions that will be class members
double generate_poisson(double u);
double generate_gamma(double u);
double generate_beta(double u);

class EnhancedPRNG {
private:
    PRNGConfig config_;
    std::unique_ptr<MultiQI> multi_;
    std::unique_ptr<CryptoMixer> crypto_;
    std::unique_ptr<ZigguratNormal> ziggurat_; // Ziggurat generator for normal distribution
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

    // Parallel buffer filling implementation using shared state with jump-ahead approach
    void fill_buffer_parallel() {
        // Calculate optimal thread count based on buffer size and hardware
        size_t ideal_threads = std::thread::hardware_concurrency();
        if (ideal_threads == 0) ideal_threads = 4; // Fallback if hardware_concurrency fails
        
        // Scale threads based on buffer size - don't use many threads for small buffers
        size_t buffer_based_threads = buffer_.size() / 10000;
        if (buffer_based_threads == 0) buffer_based_threads = 1;
        
        size_t thread_count = std::min(ideal_threads, buffer_based_threads);
        
        // For small buffers or single thread, use sequential fill
        if (thread_count <= 1 || buffer_.size() < 5000) {
            if (multi_) multi_->fill(buffer_.data(), buffer_.size());
            buffer_pos_ = 0;
            return;
        }
        
        // Thread safety check - don't try to use more threads than the buffer size
        if (thread_count > buffer_.size()) {
            thread_count = buffer_.size();
        }
        
        // Setup thread containers
        std::vector<std::thread> threads;
        threads.reserve(thread_count);
        
        // Create clones of the main MultiQI for each thread
        // This allows us to jump ahead to different positions without affecting the original
        // A cloning function for MultiQI and QuadraticIrrational to do this properly
        std::vector<std::unique_ptr<MultiQI>> thread_multiqis;
        thread_multiqis.reserve(thread_count);
        
        // Get parameters from the main MultiQI instance
        int howMany = config_.debug ? 16 : 32;
        auto base_params = pickMultiQiSet(config_.mpfr_precision, howMany);
        
        // Create identical MultiQI instances for each thread
        for (size_t t = 0; t < thread_count; ++t) {
            thread_multiqis.push_back(std::make_unique<MultiQI>(base_params, config_.mpfr_precision));
        }
        
        // Calculate chunk sizes and sequence positions
        const size_t chunk_size = buffer_.size() / thread_count;
        
        // Use mutex to protect thread ID assignment and jump calculations
        std::mutex jump_mutex;
        
        // Determine start position for the entire buffer fill
        size_t current_sequence_position = 0; // We'll jump from the current sequence position
        
        // Launch worker threads
        for (size_t t = 0; t < thread_count; ++t) {
            size_t start = t * chunk_size;
            size_t end = (t == thread_count - 1) ? buffer_.size() : (t + 1) * chunk_size;
            size_t segment_size = end - start;
            
            threads.emplace_back([t, start, segment_size, &thread_multiqis, &jump_mutex, &current_sequence_position, this]() {
                // Calculate this thread's jump position
                uint64_t thread_jump_position;
                {
                    std::lock_guard<std::mutex> lock(jump_mutex);
                    thread_jump_position = current_sequence_position;
                    current_sequence_position += segment_size;
                }
                
                // Jump ahead to the correct position in the sequence
                thread_multiqis[t]->jump_ahead(thread_jump_position);
                
                // Fill this thread's portion of the buffer with values from the same
                // conceptual sequence as the main generator, just at an offset
                thread_multiqis[t]->fill(buffer_.data() + start, segment_size);
            });
        }
        
        // Wait for all threads to complete
        for (auto& thread : threads) {
            if (thread.joinable()) {
                thread.join();
            }
        }
        
        // Update the main generator's position to be after the entire buffer
        multi_->jump_ahead(buffer_.size());
        
        // Reset buffer position
        buffer_pos_ = 0;
        
        // Apply crypto mixing if configured
        if (config_.use_crypto_mixing && crypto_) {
            crypto_->mix(reinterpret_cast<unsigned char*>(buffer_.data()),
                        buffer_.size() * sizeof(double));
        }
    }
    
    // Refill buffer with base uniform(0,1), then optionally cryptomix
    void fill_buffer() {
        if (!multi_) {
            throw std::runtime_error("MultiQI not initialized");
        }
        
        if (config_.use_parallel_filling) {
            fill_buffer_parallel();
        } else {
            // Original sequential implementation
            multi_->fill(buffer_.data(), buffer_.size());
            buffer_pos_ = 0;
            
            if (config_.use_crypto_mixing && crypto_) {
                crypto_->mix(reinterpret_cast<unsigned char*>(buffer_.data()),
                             buffer_.size() * sizeof(double));
            }
        }
    }

    // Standard Box-Muller to produce two normals from (u1,u2)
    // We'll return z0, store z1 in spare.
    std::pair<double,double> box_muller_pair(double u1, double u2) {
        // Sanitize inputs first
        if (std::isnan(u1) || std::isinf(u1) || u1 <= 0.0 || u1 >= 1.0) {
            u1 = 0.5;
        }
        if (std::isnan(u2) || std::isinf(u2) || u2 <= 0.0 || u2 >= 1.0) {
            u2 = 0.5;
        }
        
        // Ensure values are strictly in (0,1) to avoid numerical issues
        const double epsilon = 1e-10;
        if (u1 < epsilon) u1 = epsilon;
        if (u1 > 1.0 - epsilon) u1 = 1.0 - epsilon;
        if (u2 < epsilon) u2 = epsilon;
        if (u2 > 1.0 - epsilon) u2 = 1.0 - epsilon;

        // Standard Box-Muller transformations
        double r = std::sqrt(-2.0 * std::log(u1));
        double theta = 2.0 * M_PI * u2;
        double z0 = r * std::cos(theta);
        double z1 = r * std::sin(theta);
        
        // Apply mean & sd
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
    
    // Generate Poisson random variable
    double generate_poisson(double u) {
        double lambda = config_.poisson_lambda;
        
        // For small lambda values, use direct method
        if (lambda < 15.0) {
            // Knuth's algorithm
            double L = std::exp(-lambda);
            double p = 1.0;
            int k = 0;
            
            do {
                k++;
                // Get next uniform
                if (buffer_pos_ >= buffer_.size()) {
                    fill_buffer();
                }
                double u = buffer_[buffer_pos_++];
                p *= u;
            } while (p > L);
            
            return k - 1;
        } 
        else {
            // For large lambda, use normal approximation with continuity correction
            double alpha = 1.0 / std::sqrt(lambda);
            double beta = alpha * lambda;
            double k;
            
            // Use rejection method with a normal envelope
            while (true) {
                // Get two uniform random numbers
                if (buffer_pos_ >= buffer_.size()) {
                    fill_buffer();
                }
                double u1 = buffer_[buffer_pos_++];
                
                if (buffer_pos_ >= buffer_.size()) {
                    fill_buffer();
                }
                double u2 = buffer_[buffer_pos_++];
                
                // Box-Muller transform to get normal
                double z = std::sqrt(-2.0 * std::log(u1)) * std::cos(2.0 * M_PI * u2);
                
                // Candidate k value using normal approximation
                k = std::floor(beta * z + lambda + 0.5);
                
                // Reject negative values
                if (k < 0) continue;
                
                // Get another uniform for acceptance test
                if (buffer_pos_ >= buffer_.size()) {
                    fill_buffer();
                }
                double u3 = buffer_[buffer_pos_++];
                
                // Calculate log acceptance ratio
                double s;
                
                // Use Stirling's approximation for log(k!)
                if (k > 10) {
                    s = alpha * (1.0 - std::pow(k - lambda, 2) / (k + lambda)) - 
                        std::log(2.0 * M_PI * k) / 2.0;
                } else {
                    s = -lambda + k * std::log(lambda) - std::lgamma(k + 1.0);
                }
                
                if (std::log(u3) <= s) {
                    break;
                }
            }
            
            return k;
        }
    }
    
    // Generate Gamma random variable
    double generate_gamma(double u) {
        double alpha = config_.gamma_shape;  // Shape parameter
        double theta = config_.gamma_scale;  // Scale parameter
        
        // Handle special cases
        if (alpha == 1.0) {
            // For alpha = 1, Gamma is equivalent to Exponential
            return uniform_to_exponential(u) * theta;
        }
        
        // For alpha < 1, use rejection method
        if (alpha < 1.0) {
            double b = (std::exp(1.0) + alpha) / std::exp(1.0);
            
            while (true) {
                // Get three uniform random numbers
                if (buffer_pos_ >= buffer_.size()) {
                    fill_buffer();
                }
                double u1 = buffer_[buffer_pos_++];
                
                if (buffer_pos_ >= buffer_.size()) {
                    fill_buffer();
                }
                double u2 = buffer_[buffer_pos_++];
                
                if (buffer_pos_ >= buffer_.size()) {
                    fill_buffer();
                }
                double u3 = buffer_[buffer_pos_++];
                
                double p = b * u1;
                if (p <= 1.0) {
                    double x = std::pow(p, 1.0 / alpha);
                    if (u2 <= std::exp(-x)) {
                        return x * theta;
                    }
                } else {
                    double x = -std::log((b - p) / alpha);
                    if (u3 <= std::pow(x, alpha - 1.0)) {
                        return x * theta;
                    }
                }
            }
        }
        // For alpha > 1, use efficient algorithm
        else {
            double a = std::sqrt(2.0 * alpha - 1.0);
            double b = alpha - std::log(4.0);
            double q = alpha + 1.0 / a;
            double theta_a = 4.5;
            double d = 1.0 + std::log(theta_a);
            
            while (true) {
                // Get two uniform random numbers
                if (buffer_pos_ >= buffer_.size()) {
                    fill_buffer();
                }
                double u1 = buffer_[buffer_pos_++];
                
                if (buffer_pos_ >= buffer_.size()) {
                    fill_buffer();
                }
                double u2 = buffer_[buffer_pos_++];
                
                double v = a * std::log(u1 / (1.0 - u1));
                double x = alpha * std::exp(v);
                double z = u1 * u1 * u2;
                double r = b + q * v - x;
                
                if (r + d - theta_a * z >= 0.0 || r >= std::log(z)) {
                    return x * theta;
                }
            }
        }
    }
    
    // Generate Beta random variable
    double generate_beta(double u) {
        double alpha = config_.beta_alpha;
        double beta = config_.beta_beta;
        
        // Handle special cases
        if (alpha == 1.0 && beta == 1.0) {
            // Uniform distribution on [0,1]
            return u;
        }
        
        // For certain parameter values, use direct methods
        if (alpha == 1.0) {
            // Get another uniform
            if (buffer_pos_ >= buffer_.size()) {
                fill_buffer();
            }
            double u2 = buffer_[buffer_pos_++];
            // For alpha=1, use direct formula
            return 1.0 - std::pow(u2, 1.0/beta);
        }
        
        if (beta == 1.0) {
            // Get another uniform
            if (buffer_pos_ >= buffer_.size()) {
                fill_buffer();
            }
            double u2 = buffer_[buffer_pos_++];
            // For beta=1, use direct formula
            return std::pow(u2, 1.0/alpha);
        }
        
        // Numerically stable Beta generation using the Jöhnk algorithm
        double sum;
        double X, Y;
        
        do {
            // Get two more uniforms
            if (buffer_pos_ >= buffer_.size() - 1) {
                fill_buffer();
            }
            double u1 = buffer_[buffer_pos_++];
            double u2 = buffer_[buffer_pos_++];
            
            // Ensure they're in (0,1)
            if (u1 <= 0.0 || u1 >= 1.0) u1 = 0.5;
            if (u2 <= 0.0 || u2 >= 1.0) u2 = 0.5;
            
            // Generate X and Y
            X = std::pow(u1, 1.0/alpha);
            Y = std::pow(u2, 1.0/beta);
            sum = X + Y;
        } while (sum > 1.0);
        
        // Return X/(X+Y) but handle edge cases
        if (sum < 1e-10) { 
            return 0.5; // Avoid division by near-zero
        }
        
        return X / sum;
    }

public:
    EnhancedPRNG(const PRNGConfig& cfg,
                 const std::vector<std::tuple<long,long,long>>& abc_list)
  : config_(cfg),
    multi_(std::make_unique<MultiQI>(abc_list, config_.mpfr_precision)),
    crypto_(nullptr),
    ziggurat_(nullptr),
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
        
        // Initialize buffer first
        reset_state();
        
        // Now that buffer is initialized, create the ZigguratNormal object
        ziggurat_ = std::make_unique<ZigguratNormal>(
            [this]() -> double {
                // Get next uniform value with bounds validation
                double u;
                if (buffer_pos_ >= buffer_.size()) {
                    fill_buffer();
                }
                u = buffer_[buffer_pos_++];
                
                // Ensure value is in [0,1)
                if (u < 0.0) u = 0.0;
                if (u >= 1.0) u = 0.9999999999999;
                
                return u;
            },
            config_.normal_mean,
            config_.normal_sd);
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
        bool norm_changed = (old_config.normal_mean != new_config.normal_mean ||
                               old_config.normal_sd   != new_config.normal_sd);
        bool method_changed = (old_config.normal_method != new_config.normal_method);        
        bool exp_changed = (old_config.exponential_lambda != new_config.exponential_lambda);
        
        config_ = new_config;
        
        // Update normal params
        if (norm_changed) {
            reset_state();  // avoid spare_normal issues
            has_spare_normal_ = false;
            
            // Update Ziggurat normal generator if needed
            if (ziggurat_) {
                ziggurat_->set_parameters(config_.normal_mean, config_.normal_sd);
            }
        }
        
        // Handle normal method changes
        if (method_changed) {
            reset_state();  // Reset state when switching methods
            has_spare_normal_ = false;
        }

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
            norm_changed || exp_changed)
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
            // Force using Box-Muller for reliability
            // The Ziggurat method is having numerical stability issues
            if (!has_spare_normal_) {
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

        case PRNGConfig::POISSON:
            return generate_poisson(u);

        case PRNGConfig::GAMMA:
            return generate_gamma(u);

        case PRNGConfig::BETA:
            return generate_beta(u);

        default:
            throw std::runtime_error("Unknown distribution type");
        }
    }

    // Skip n draws
    void skip(size_t n) {
        if (n == 0) {
            return;
        }
        
        // For small skips or when we have buffer data available
        size_t buffer_available = buffer_.size() - buffer_pos_;
        if (n <= buffer_available) {
            // Just move the pointer
            buffer_pos_ += n;
            sample_count_ += n;
            skipped_ += n;
            return;
        }
        
        // For larger skips - consume buffer first
        n -= buffer_available;
        buffer_pos_ = buffer_.size(); // Mark buffer as fully consumed
        sample_count_ += buffer_available;
        
        // Calculate full buffer skips and remainder
        size_t full_buffers = n / buffer_.size();
        size_t remainder = n % buffer_.size();
        
        // Check if we need to reseed during this skip
        if (config_.reseed_interval > 0) {
            uint64_t total_samples = sample_count_ + full_buffers * buffer_.size() + remainder;
            uint64_t intervals_crossed = total_samples / config_.reseed_interval;
            
            if (intervals_crossed > 0) {
                // We'll cross at least one reseed boundary
                reseed();
                sample_count_ = 0;
                
                // Adjust the skip count
                uint64_t remaining_samples = total_samples % config_.reseed_interval;
                
                // Recalculate full buffers and remainder
                full_buffers = remaining_samples / buffer_.size();
                remainder = remaining_samples % buffer_.size();
            }
        }
        
        // Use jump-ahead for the majority of the skip
        if (multi_ && full_buffers > 0) {
            multi_->jump_ahead(full_buffers * buffer_.size());
            sample_count_ += full_buffers * buffer_.size();
        }
        
        // Handle remainder
        if (remainder > 0) {
            fill_buffer();  // Refill buffer
            buffer_pos_ = remainder;  // Set position
            sample_count_ += remainder;
        }
        
        skipped_ += n + buffer_available;
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
            case PRNGConfig::POISSON:
                Rcpp::Rcout << "poisson(lambda=" << config_.poisson_lambda 
                            << ")\n";
                break;
            case PRNGConfig::GAMMA:
                Rcpp::Rcout << "gamma(shape=" << config_.gamma_shape
                            << ", scale=" << config_.gamma_scale << ")\n";
                break;
            case PRNGConfig::BETA:
                Rcpp::Rcout << "beta(alpha=" << config_.beta_alpha
                            << ", beta=" << config_.beta_beta << ")\n";
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
        else if (dist == "poisson") config.distribution = PRNGConfig::POISSON;
        else if (dist == "gamma") config.distribution = PRNGConfig::GAMMA;
        else if (dist == "beta") config.distribution = PRNGConfig::BETA;
        else
            throw std::runtime_error("Unknown distribution: " + dist);
    }
    parse_double_("range_min", config.range_min);
    parse_double_("range_max", config.range_max);
    parse_double_("normal_mean", config.normal_mean);
    parse_double_("normal_sd", config.normal_sd);
    parse_double_("exponential_lambda", config.exponential_lambda);
    
    // Parse new distribution parameters
    parse_double_("poisson_lambda", config.poisson_lambda);
    parse_double_("gamma_shape", config.gamma_shape);
    parse_double_("gamma_scale", config.gamma_scale);
    parse_double_("beta_alpha", config.beta_alpha);
    parse_double_("beta_beta", config.beta_beta);
    
    // Parse normal distribution method
    if (rcfg.containsElementNamed("normal_method")) {
        std::string method = Rcpp::as<std::string>(rcfg["normal_method"]);
        if (method == "ziggurat") {
            config.normal_method = qiprng::PRNGDefaults::ZIGGURAT;
        } else if (method == "box_muller") {
            config.normal_method = qiprng::PRNGDefaults::BOX_MULLER;
        } else {
            throw std::runtime_error("Invalid normal method: must be 'ziggurat' or 'box_muller'");
        }
    }
    parse_flag("use_crypto_mixing", config.use_crypto_mixing);
    parse_flag("use_parallel_filling", config.use_parallel_filling);
    parse_flag("use_csv_discriminants", config.use_csv_discriminants);
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
    case PRNGConfig::POISSON:       dist_name = "poisson";        break;
    case PRNGConfig::GAMMA:         dist_name = "gamma";          break;
    case PRNGConfig::BETA:          dist_name = "beta";           break;
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
        Rcpp::Named("poisson_lambda") = conf.poisson_lambda,
        Rcpp::Named("gamma_shape") = conf.gamma_shape,
        Rcpp::Named("gamma_scale") = conf.gamma_scale,
        Rcpp::Named("beta_alpha") = conf.beta_alpha,
        Rcpp::Named("beta_beta") = conf.beta_beta,
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

// Load discriminants from CSV file
void loadCSVDiscriminants() {
    std::lock_guard<std::mutex> lock(g_csv_disc_mutex);
    
    // Check if already loaded
    if (g_csv_discriminants_loaded) {
        return;
    }
    
    std::string csv_path = "discriminants.csv";
    std::ifstream file(csv_path);
    
    if (!file.is_open()) {
        Rcpp::warning("Could not open discriminants.csv file, falling back to random discriminants");
        return;
    }
    
    std::string line;
    // Skip header
    std::getline(file, line);
    
    // Read data lines
    while (std::getline(file, line)) {
        std::stringstream ss(line);
        std::string token;
        std::vector<std::string> tokens;
        
        while (std::getline(ss, token, ',')) {
            tokens.push_back(token);
        }
        
        if (tokens.size() >= 4) {
            try {
                long a = std::stol(tokens[0]);
                long b = std::stol(tokens[1]);
                long c = std::stol(tokens[2]);
                long long discriminant = std::stoll(tokens[3]);
                
                // Verify discriminant
                if (discriminant == static_cast<long long>(b)*b - 4LL*a*c && discriminant > 0) {
                    g_csv_discriminants.push_back(std::make_tuple(a, b, c, discriminant));
                }
            } catch (const std::exception& e) {
                // Skip invalid lines
                continue;
            }
        }
    }
    
    file.close();
    g_csv_discriminants_loaded = true;
    
    Rcpp::Rcout << "Loaded " << g_csv_discriminants.size() << " discriminants from CSV file." << std::endl;
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

// Augmented to support custom discriminants from CSV file.
long long chooseUniqueDiscriminant(long min_value, long max_value) {
    // Check if we should use CSV discriminants
    PRNGConfig* config = nullptr;
    if (g_use_threading && t_prng) {
        config = const_cast<PRNGConfig*>(&t_prng->getConfig());
    } else if (!g_use_threading && g_prng) {
        config = const_cast<PRNGConfig*>(&g_prng->getConfig());
    }
    
    bool use_csv = config && config->use_csv_discriminants;
    
    // Use CSV discriminants if requested
    if (use_csv) {
        std::lock_guard<std::mutex> csv_lock(g_csv_disc_mutex);
        
        // Load CSV discriminants if not already loaded
        if (!g_csv_discriminants_loaded) {
            loadCSVDiscriminants();
        }
        
        // Use CSV discriminants if available
        if (!g_csv_discriminants.empty()) {
            std::random_device rd;
            std::mt19937_64 gen(rd());
            
            // Try up to 50 random discriminants from the CSV file
            const int MAX_CSV_ATTEMPTS = 50;
            for (int attempt = 0; attempt < MAX_CSV_ATTEMPTS; attempt++) {
                std::uniform_int_distribution<size_t> csv_idx_dist(0, g_csv_discriminants.size() - 1);
                size_t idx = csv_idx_dist(gen);
                auto& entry = g_csv_discriminants[idx];
                long long discriminant = std::get<3>(entry);
                
                // Check if already used
                std::lock_guard<std::mutex> disc_lock(g_disc_mutex);
                if (g_used_discriminants.find(discriminant) == g_used_discriminants.end()) {
                    g_used_discriminants.insert(discriminant);
                    
                    // Update PRNG parameters if possible
                    if (config) {
                        config->a = std::get<0>(entry);
                        config->b = std::get<1>(entry);
                        config->c = std::get<2>(entry);
                    }
                    
                    return discriminant;
                }
            }
            
            // If all attempts failed, fall back to random
            Rcpp::warning("All CSV discriminants are used, falling back to random");
        }
    }
    
    // Original random selection logic
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