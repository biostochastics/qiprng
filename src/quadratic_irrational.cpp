// File: quadratic_irrational.cpp
// --------------------------------------------------------------
#include "quadratic_irrational.hpp"

#include <cstdlib>  // For std::getenv
#include <limits>   // For std::numeric_limits

#include "deterministic_rng.hpp"
#include "precision_utils.hpp"  // For high-precision constants and safe conversions

namespace qiprng {

// Matrix power using binary exponentiation for O(log n) complexity
Matrix2x2 Matrix2x2::power(uint64_t n) const {
    if (n == 0) {
        return Matrix2x2();  // Identity matrix
    }

    Matrix2x2 result(1, 0, 0, 1);  // Identity
    Matrix2x2 base = *this;

    while (n > 0) {
        if (n & 1) {
            result = result * base;
        }
        base = base * base;
        n >>= 1;
    }

    return result;
}

// Check if a number is square-free (no repeated prime factors)
bool QuadraticIrrational::is_square_free(long long n) {
    if (n <= 1)
        return false;

    // Check for divisibility by small primes squared
    const long long small_primes[] = {2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31};
    for (long long p : small_primes) {
        long long p_squared = p * p;
        if (n % p_squared == 0) {
            return false;
        }
        // Remove all factors of p
        while (n % p == 0) {
            n /= p;
        }
    }

    // Check remaining factor
    if (n > 1) {
        // If n is still > 1, it's either prime or has larger factors
        // Check if it's a perfect square

        // SECURITY FIX: Prevent integer overflow in sqrt_n * sqrt_n
        double sqrt_n_dbl = std::sqrt(static_cast<double>(n));

        // Check if sqrt_n would overflow when squared
        if (sqrt_n_dbl <= std::sqrt(static_cast<double>(LLONG_MAX))) {
            long long sqrt_n = static_cast<long long>(sqrt_n_dbl);
            if (sqrt_n * sqrt_n == n) {
                return false;
            }
        }
    }

    return true;
}

// Custom hash function for pair<long, long>
struct PairHash {
    std::size_t operator()(const std::pair<long, long>& p) const {
        // Combine hashes of the two elements
        std::size_t h1 = std::hash<long>{}(p.first);
        std::size_t h2 = std::hash<long>{}(p.second);
        // Use a simple combining function
        return h1 ^ (h2 << 1);
    }
};

// Compute CFE period using Gauss-Legendre algorithm with hash table for O(L) detection
void QuadraticIrrational::compute_cfe_period() {
    if (cfe_computed_) {
        return;  // Already computed
    }

    // Initialize for Gauss-Legendre recurrence
    // P_0 = 0, Q_0 = 1, P_1 = a_0, Q_1 = discriminant - P_1^2
    long long D = discriminant_;
    long a_0 = static_cast<long>(std::sqrt(static_cast<double>(D)));

    // Hash table for period detection: key = (P_n, Q_n), value = index
    std::unordered_map<std::pair<long, long>, size_t, PairHash> seen;

    cfe_coefficients_.clear();
    cfe_coefficients_.push_back(a_0);

    long P_n = a_0;
    long Q_n = D - a_0 * a_0;

    if (Q_n == 0) {
        // Perfect square, no period
        cfe_period_length_ = 0;
        cfe_computed_ = true;
        return;
    }

    size_t index = 0;
    seen[{P_n, Q_n}] = index++;

    // Maximum iterations to prevent infinite loops - scale with discriminant
    // Based on theoretical bounds for continued fraction periods
    const size_t BASE_MAX_PERIOD = 100000;          // Base maximum period length for safety
    const size_t DISCRIMINANT_SCALING_FACTOR = 10;  // Scaling factor based on Lagrange's theorem
    // Period length is O(sqrt(D) * log(D)) in worst case, we use conservative estimate
    const size_t MAX_PERIOD =
        std::max(BASE_MAX_PERIOD, static_cast<size_t>(DISCRIMINANT_SCALING_FACTOR *
                                                      std::sqrt(static_cast<double>(D))));

    while (index < MAX_PERIOD) {
        // Gauss-Legendre recurrence formulas
        // a_n = floor((a_0 + P_n) / Q_n)
        long a_n = (a_0 + P_n) / Q_n;
        cfe_coefficients_.push_back(a_n);

        // P_{n+1} = a_n * Q_n - P_n
        long P_next = a_n * Q_n - P_n;

        // Q_{n+1} = (D - P_{n+1}^2) / Q_n
        // Use safe overflow checking
#ifdef __SIZEOF_INT128__
        // Use 128-bit arithmetic for intermediate calculations on 64-bit systems
        __int128 P_next_128 = static_cast<__int128>(P_next);
        __int128 P_next_squared = P_next_128 * P_next_128;
        if (P_next_squared > static_cast<__int128>(D)) {
            throw std::runtime_error("QuadraticIrrational: Overflow in CFE computation");
        }
        long Q_next = (D - static_cast<long long>(P_next_squared)) / Q_n;
#else
        // Fallback: Check for overflow before multiplication
        double P_next_abs = std::abs(static_cast<double>(P_next));
        if (P_next_abs > std::sqrt(static_cast<double>(D))) {
            throw std::runtime_error("QuadraticIrrational: Overflow in CFE computation");
        }
        long long P_next_squared = static_cast<long long>(P_next) * P_next;
        long Q_next = (D - P_next_squared) / Q_n;
#endif

        // Check for period
        auto state = std::make_pair(P_next, Q_next);
        auto it = seen.find(state);
        if (it != seen.end()) {
            // Found period!
            cfe_period_length_ = index - it->second;

            // Remove pre-period coefficients if any
            if (it->second > 0) {
                cfe_coefficients_.erase(cfe_coefficients_.begin(),
                                        cfe_coefficients_.begin() + it->second);
            }

            // Keep only the period
            cfe_coefficients_.resize(cfe_period_length_);

            cfe_computed_ = true;

            // Store final state for potential optimizations
            P_n_ = P_next;
            Q_n_ = Q_next;

            return;
        }

        seen[state] = index++;
        P_n = P_next;
        Q_n = Q_next;
    }

    // If we reach here, period is too long or doesn't exist
    // Use exception for consistency in error handling
    cfe_period_length_ = 0;
    cfe_computed_ = true;
    throw std::runtime_error("QuadraticIrrational: CFE period exceeds maximum length " +
                             std::to_string(MAX_PERIOD));
}

// Enhanced step_once using Gauss-Legendre insights for optimization
void QuadraticIrrational::step_once_gauss_legendre() {
    // This is an optimized version that can leverage CFE period knowledge
    // For now, fall back to regular step_once, but this can be enhanced
    // to use matrix methods when CFE is computed
    step_once();
}

// Single iteration with improved error handling
void QuadraticIrrational::step_once() {
    if (!value_ || !next_ || !temp_ || !temp2_ || !root_ || !value_->is_valid() ||
        !next_->is_valid() || !temp_->is_valid() || !temp2_->is_valid() || !root_->is_valid()) {
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

QuadraticIrrational::QuadraticIrrational(long a, long b, long c, mpfr_prec_t prec, uint64_t seed,
                                         bool has_seed)
    : a_(a), b_(b), c_(c), discriminant_(0), mpfr_prec_(prec), cfe_period_length_(0),
      cfe_computed_(false), P_n_(0), Q_n_(1) {
    // Validate parameters more comprehensively
    if (a == 0) {
        throw std::invalid_argument("QuadraticIrrational: 'a' parameter cannot be zero");
    }

    if (prec < MPFR_PREC_MIN || prec > MPFR_PREC_MAX) {
        throw std::invalid_argument("QuadraticIrrational: Invalid precision value");
    }

    // Use safe discriminant calculation with overflow protection
    long long disc_ll;
    std::string error_msg;

    if (!safe_calculate_discriminant(a_, b_, c_, disc_ll, error_msg)) {
        throw std::runtime_error("QuadraticIrrational: " + error_msg);
    }

    // Cache discriminant for CFE computation
    discriminant_ = disc_ll;

    // Check for non-positive discriminant
    if (disc_ll <= 0) {
        Rcpp::Rcerr << "a=" << a_ << ", b=" << b_ << ", c=" << c_ << ", disc=" << disc_ll
                    << std::endl;
        throw std::runtime_error("QuadraticIrrational: non-positive discriminant");
    }

    // Check if discriminant is square-free for quality PRNG (v0.5.0 enhancement)
    if (!is_square_free(disc_ll)) {
        // Use exception for critical quality issues that affect PRNG correctness
        throw std::invalid_argument(
            "QuadraticIrrational: discriminant " + std::to_string(disc_ll) +
            " is not square-free. This significantly affects PRNG quality. "
            "Please use different parameters (a, b, c) that produce a square-free discriminant.");
    }

    // Check if discriminant is too large for safe square root calculation
    if (disc_ll > static_cast<long long>(std::pow(2.0, 53))) {
        Rcpp::warning(
            "QuadraticIrrational: discriminant %lld is very large, potential precision issues",
            disc_ll);
    }

    try {
        value_ = std::make_unique<MPFRWrapper>(prec);
        root_ = std::make_unique<MPFRWrapper>(prec);
        next_ = std::make_unique<MPFRWrapper>(prec);
        temp_ = std::make_unique<MPFRWrapper>(prec);
        temp2_ = std::make_unique<MPFRWrapper>(prec);

        if (!value_->is_valid() || !root_->is_valid() || !next_->is_valid() || !temp_->is_valid() ||
            !temp2_->is_valid()) {
            throw std::runtime_error(
                "QuadraticIrrational: Failed to initialize one or more MPFR wrappers");
        }

        int op_ret = 0;

        // FIX: Validate discriminant fits in long before casting
        if (disc_ll > LONG_MAX || disc_ll < LONG_MIN) {
            // Use string conversion for large discriminants
            std::string disc_str = std::to_string(disc_ll);
            op_ret = mpfr_set_str(*root_->get(), disc_str.c_str(), 10, MPFR_RNDN);
        } else {
            op_ret = mpfr_set_si(*root_->get(), static_cast<long>(disc_ll), MPFR_RNDN);
        }
        check_mpfr_result(op_ret, "set_discriminant");

        op_ret = mpfr_sqrt(*root_->get(), *root_->get(), MPFR_RNDN);
        // It's normal for sqrt to be inexact, so suppress this specific warning
        if (op_ret != 0 && !suppress_mpfr_warnings.load()) {
            static thread_local int sqrt_warning_count = 0;
            if (sqrt_warning_count < 1) {  // Only show once per thread
                Rcpp::warning(
                    "Some inexact results in square root operations are normal and expected");
                sqrt_warning_count++;
            }
        }
        if (mpfr_nan_p(*root_->get()))
            throw std::runtime_error("QuadraticIrrational: sqrt(disc) resulted in NaN");

        op_ret = mpfr_set_si(*value_->get(), b_, MPFR_RNDN);
        check_mpfr_result(op_ret, "set_b_value");

        op_ret = mpfr_add(*value_->get(), *value_->get(), *root_->get(), MPFR_RNDN);
        check_mpfr_result(op_ret, "add_sqrt_disc");
        if (mpfr_nan_p(*value_->get()))
            throw std::runtime_error("QuadraticIrrational: (b+sqrt(disc)) resulted in NaN");

        if (a_ == 0)
            throw std::logic_error("QuadraticIrrational: 'a' is zero before division (should have "
                                   "been caught)");  // Should be caught earlier

        op_ret = mpfr_div_si(*value_->get(), *value_->get(), 2 * a_, MPFR_RNDN);
        check_mpfr_result(op_ret, "div_by_2a");
        if (mpfr_nan_p(*value_->get()))
            throw std::runtime_error("QuadraticIrrational: division by 2a resulted in NaN");

        op_ret = mpfr_frac(*value_->get(), *value_->get(), MPFR_RNDN);
        check_mpfr_result(op_ret, "initial_frac");
        if (mpfr_nan_p(*value_->get()))
            throw std::runtime_error("QuadraticIrrational: initial frac() resulted in NaN");

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
            auto det_rng = DeterministicRNGFactory::create(
                seed, std::to_string(a) + "_" + std::to_string(b) + "_" + std::to_string(c));
            std::uniform_int_distribution<uint64_t> skip_dist(MIN_WARMUP_ITERATIONS,
                                                              MAX_WARMUP_ITERATIONS);
            skip_amt = skip_dist(det_rng);
        } else {
            // Original random behavior
            std::random_device rd;
            std::mt19937_64 rng(rd());
            std::uniform_int_distribution<uint64_t> skip_dist(MIN_WARMUP_ITERATIONS,
                                                              MAX_WARMUP_ITERATIONS);
            skip_amt = skip_dist(rng);
        }

        // Perform the initial warm-up skip
        for (uint64_t i = 0; i < skip_amt; i++) {
            step_once();
        }

    } catch (const std::bad_alloc& e) {
        throw std::runtime_error(
            std::string("QuadraticIrrational: Memory allocation failed during construction: ") +
            e.what());
    } catch (const std::exception& e) {
        throw std::runtime_error(std::string("QuadraticIrrational: Initialization failed: ") +
                                 e.what());
    }
}

double QuadraticIrrational::next() {
    // Use fast path for precision 53 if parameters are safe
    static bool use_fast_path = std::getenv("QIPRNG_FAST_PATH") != nullptr;

    if (use_fast_path && mpfr_prec_ == 53 && std::abs(static_cast<double>(a_)) < 1e100 &&
        std::abs(static_cast<double>(b_)) < 1e100 && std::abs(static_cast<double>(c_)) < 1e100) {
        // Fast double precision path
        double x = mpfr_get_d(*value_->get(), MPFR_RNDN);
        double x2 = x * x;
        double result = std::fma(static_cast<double>(a_), x2,
                                 std::fma(static_cast<double>(b_), x, static_cast<double>(c_)));
        result = result - std::floor(result);
        if (result < 0.0)
            result += 1.0;
        mpfr_set_d(*value_->get(), result, MPFR_RNDN);
        return result;
    }

    // Fall back to MPFR implementation
    step_once();
    // Use safe conversion with extended precision intermediates
    double val = precision::safe_mpfr_to_double(*value_->get(), true);
    if (std::isnan(val) || std::isinf(val)) {
        Rcpp::warning("QuadraticIrrational::next() produced NaN/Inf. Returning 0.5.");
        return 0.5;  // A fallback value
    }
    return val;
}

void QuadraticIrrational::skip(uint64_t n) {
    jump_ahead_optimized_v2(n);
}

void QuadraticIrrational::jump_ahead(uint64_t n) {
    if (!value_ || !next_ || !temp_ || !temp2_ || !root_ || !value_->is_valid() ||
        !next_->is_valid() || !temp_->is_valid() || !temp2_->is_valid() || !root_->is_valid()) {
        throw std::runtime_error(
            "QuadraticIrrational: Invalid MPFR state at the beginning of jump_ahead");
    }

    if (n == 0) {
        return;  // No jump needed
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

// Optimized O(log n) jump-ahead using matrix exponentiation (v0.5.0 enhancement)
/**
 * @brief Jumps ahead in the PRNG sequence by n steps in O(log n) time.
 *
 * This function uses matrix exponentiation to efficiently jump ahead in the sequence.
 * The recurrence relation of the quadratic irrational number can be represented by a 2x2 matrix.
 * To jump ahead by n steps, we compute the n-th power of this matrix and apply the resulting
 * transformation to the current state.
 *
 * The function first computes the CFE period of the number if it has not been computed yet.
 * If the jump is much larger than the period, it jumps by full periods using matrix exponentiation
 * and then handles the remainder by stepping through the sequence.
 *
 * @param n The number of steps to jump ahead.
 */
void QuadraticIrrational::jump_ahead_optimized(uint64_t n) {
    // This function is deprecated, use jump_ahead_optimized_v2 instead
    // which has multiple algorithm options and better overflow handling
    jump_ahead_optimized_v2(n);
    return;

    // Old implementation below (kept for reference but not used)
    if (n == 0) {
        return;
    }

    // First compute CFE period if not already done
    if (!cfe_computed_) {
        try {
            compute_cfe_period();
        } catch (const std::exception& e) {
            // Fall back to regular jump if CFE computation fails
            Rcpp::warning("QuadraticIrrational: CFE computation failed, using regular jump: %s",
                          e.what());
            jump_ahead(n);
            return;
        }
    }

    // If no period found or period is too short, use regular jump
    if (cfe_period_length_ == 0 || cfe_period_length_ < 10) {
        jump_ahead(n);
        return;
    }

    // Use matrix exponentiation for large jumps
    // The CFE recurrence can be represented as matrix multiplication
    // This is particularly efficient for jumps >> period length

    if (n > cfe_period_length_ * 2) {
        // For very large jumps, we can jump by periods efficiently
        uint64_t full_periods = n / cfe_period_length_;
        uint64_t remainder = n % cfe_period_length_;

        // Build transformation matrix for one period
        Matrix2x2 period_matrix(1, 0, 0, 1);  // Start with identity

        for (size_t i = 0; i < cfe_period_length_; ++i) {
            long a_i = cfe_coefficients_[i];
            Matrix2x2 step_matrix(a_i, 1, 1, 0);
            period_matrix = period_matrix * step_matrix;
        }

        // Apply the period transformation multiple times using binary exponentiation
        Matrix2x2 result = period_matrix.power(full_periods);

        // Apply the matrix transformation to current state using MPFR arithmetic
        // The transformation is: [x_{n+k}, y_{n+k}] = M^k * [x_n, y_n]
        // where M is the period matrix and k is the number of periods to jump

        // Create temporary MPFR variables for the transformation
        std::unique_ptr<MPFRWrapper> new_value =
            std::make_unique<MPFRWrapper>(value_->get_precision());
        std::unique_ptr<MPFRWrapper> temp_val =
            std::make_unique<MPFRWrapper>(value_->get_precision());

        // Apply transformation: new_value = (p * value + q * next) / (r * value + s * next)
        // where p, q, r, s are the matrix elements

        // Numerator: p * value + q * next
        // Use mpfr_t intermediates to avoid precision loss on platforms where long != int64_t
        MPFRWrapper p_mpfr(value_->get_precision()), q_mpfr(value_->get_precision()),
            r_mpfr(value_->get_precision()), s_mpfr(value_->get_precision());
        mpfr_set_si(*p_mpfr.get(), static_cast<long>(result.p), MPFR_RNDN);
        mpfr_set_si(*q_mpfr.get(), static_cast<long>(result.q), MPFR_RNDN);
        mpfr_set_si(*r_mpfr.get(), static_cast<long>(result.r), MPFR_RNDN);
        mpfr_set_si(*s_mpfr.get(), static_cast<long>(result.s), MPFR_RNDN);

        // If values exceed long range, use string conversion for exact representation
        if (result.p != static_cast<long>(result.p)) {
            mpfr_set_str(*p_mpfr.get(), std::to_string(result.p).c_str(), 10, MPFR_RNDN);
        }
        if (result.q != static_cast<long>(result.q)) {
            mpfr_set_str(*q_mpfr.get(), std::to_string(result.q).c_str(), 10, MPFR_RNDN);
        }
        if (result.r != static_cast<long>(result.r)) {
            mpfr_set_str(*r_mpfr.get(), std::to_string(result.r).c_str(), 10, MPFR_RNDN);
        }
        if (result.s != static_cast<long>(result.s)) {
            mpfr_set_str(*s_mpfr.get(), std::to_string(result.s).c_str(), 10, MPFR_RNDN);
        }

        mpfr_mul(*temp_val->get(), *value_->get(), *p_mpfr.get(), MPFR_RNDN);
        mpfr_mul(*new_value->get(), *next_->get(), *q_mpfr.get(), MPFR_RNDN);
        mpfr_add(*new_value->get(), *new_value->get(), *temp_val->get(), MPFR_RNDN);

        // Denominator: r * value + s * next
        mpfr_mul(*temp_val->get(), *value_->get(), *r_mpfr.get(), MPFR_RNDN);
        mpfr_mul(*temp_->get(), *next_->get(), *s_mpfr.get(), MPFR_RNDN);
        mpfr_add(*temp_->get(), *temp_->get(), *temp_val->get(), MPFR_RNDN);

        // Divide to get new value
        if (mpfr_zero_p(*temp_->get())) {
            // Fallback if denominator is zero
            for (uint64_t i = 0; i < full_periods * cfe_period_length_; ++i) {
                step_once();
            }
        } else {
            mpfr_div(*value_->get(), *new_value->get(), *temp_->get(), MPFR_RNDN);

            // Update next value using the recurrence relation
            // For quadratic irrational: x_{n+1} = (a*x_n^2 + b*x_n + c) / (x_n^2 + root)
            step_once();  // This updates next_ based on new value_
        }

        // Handle remainder with regular stepping
        for (uint64_t i = 0; i < remainder; ++i) {
            step_once();
        }
    } else {
        // For smaller jumps, regular method is fine
        jump_ahead(n);
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

// Reseed method implementation
void QuadraticIrrational::reseed() {
    // Reset to initial state with new random starting point
    step_ = 0;

    // If we have a seed, regenerate using a new seed
    if (has_seed_) {
        // Generate new seed from current state
        std::random_device rd;
        seed_ = rd();

        // Reinitialize random engine
        if (rd_engine_) {
            rd_engine_->seed(seed_);
        }

        // Skip to a new random position
        std::uniform_int_distribution<uint64_t> dist(1000, 10000);
        uint64_t skip_amount = dist(*rd_engine_);
        skip(skip_amount);
    } else {
        // Without seed, just reset to initial state
        mpfr_set_d(*state_->x.get(), 0.5, MPFR_RNDN);
        mpfr_set_d(*state_->x_prev.get(), 0.25, MPFR_RNDN);
    }

    // Clear CFE cache if computed
    if (cfe_computed_) {
        cfe_coefficients_.clear();
        cfe_period_length_ = 0;
        cfe_computed_ = false;
    }
}

}  // namespace qiprng
