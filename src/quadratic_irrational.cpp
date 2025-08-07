// File: quadratic_irrational.cpp
// --------------------------------------------------------------
#include "quadratic_irrational.hpp"
#include "deterministic_rng.hpp"
#include <limits> // For std::numeric_limits
#include <cstdlib> // For std::getenv

#ifndef M_PI // Ensure M_PI is defined
#define M_PI 3.14159265358979323846
#endif


namespace qiprng {

// Single iteration with improved error handling
void QuadraticIrrational::step_once() {
    if (!value_ || !next_ || !temp_ || !temp2_ || !root_ ||
        !value_->is_valid() || !next_->is_valid() || !temp_->is_valid() ||
        !temp2_->is_valid() || !root_->is_valid()) {
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

QuadraticIrrational::QuadraticIrrational(long a, long b, long c, mpfr_prec_t prec,
                                       uint64_t seed, bool has_seed)
    : a_(a), b_(b), c_(c) {
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
    
    // Check for non-positive discriminant
    if (disc_ll <= 0) {
         Rcpp::Rcerr << "a=" << a_ << ", b=" << b_ << ", c=" << c_ << ", disc=" << disc_ll << std::endl;
        throw std::runtime_error("QuadraticIrrational: non-positive discriminant");
    }
    
    // Check if discriminant is too large for safe square root calculation
    if (disc_ll > static_cast<long long>(std::pow(2.0, 53))) {
        Rcpp::warning("QuadraticIrrational: discriminant %lld is very large, potential precision issues", disc_ll);
    }

    try {
        value_ = std::make_unique<MPFRWrapper>(prec);
        root_ = std::make_unique<MPFRWrapper>(prec);
        next_ = std::make_unique<MPFRWrapper>(prec);
        temp_ = std::make_unique<MPFRWrapper>(prec);
        temp2_ = std::make_unique<MPFRWrapper>(prec);

        if (!value_->is_valid() || !root_->is_valid() || !next_->is_valid() ||
            !temp_->is_valid() || !temp2_->is_valid()) {
            throw std::runtime_error("QuadraticIrrational: Failed to initialize one or more MPFR wrappers");
        }

        int op_ret = 0;
        
        // MPFR may not have mpfr_set_sj for signed long long, use mpfr_set_si with casting
        op_ret = mpfr_set_si(*root_->get(), static_cast<long>(disc_ll), MPFR_RNDN);
        check_mpfr_result(op_ret, "set_discriminant");
        
        op_ret = mpfr_sqrt(*root_->get(), *root_->get(), MPFR_RNDN);
        // It's normal for sqrt to be inexact, so suppress this specific warning
        if (op_ret != 0 && !suppress_mpfr_warnings.load()) {
            static thread_local int sqrt_warning_count = 0;
            if (sqrt_warning_count < 1) { // Only show once per thread
                Rcpp::warning("Some inexact results in square root operations are normal and expected");
                sqrt_warning_count++;
            }
        }
        if (mpfr_nan_p(*root_->get())) throw std::runtime_error("QuadraticIrrational: sqrt(disc) resulted in NaN");

        op_ret = mpfr_set_si(*value_->get(), b_, MPFR_RNDN);
        check_mpfr_result(op_ret, "set_b_value");

        op_ret = mpfr_add(*value_->get(), *value_->get(), *root_->get(), MPFR_RNDN);
        check_mpfr_result(op_ret, "add_sqrt_disc");
        if (mpfr_nan_p(*value_->get())) throw std::runtime_error("QuadraticIrrational: (b+sqrt(disc)) resulted in NaN");

        if (a_ == 0) throw std::logic_error("QuadraticIrrational: 'a' is zero before division (should have been caught)"); // Should be caught earlier

        op_ret = mpfr_div_si(*value_->get(), *value_->get(), 2 * a_, MPFR_RNDN);
        check_mpfr_result(op_ret, "div_by_2a");
        if (mpfr_nan_p(*value_->get())) throw std::runtime_error("QuadraticIrrational: division by 2a resulted in NaN");

        op_ret = mpfr_frac(*value_->get(), *value_->get(), MPFR_RNDN);
        check_mpfr_result(op_ret, "initial_frac");
        if (mpfr_nan_p(*value_->get())) throw std::runtime_error("QuadraticIrrational: initial frac() resulted in NaN");

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
            auto det_rng = DeterministicRNGFactory::create(seed,
                std::to_string(a) + "_" + std::to_string(b) + "_" + std::to_string(c));
            std::uniform_int_distribution<uint64_t> skip_dist(MIN_WARMUP_ITERATIONS, MAX_WARMUP_ITERATIONS);
            skip_amt = skip_dist(det_rng);
        } else {
            // Original random behavior
            std::random_device rd;
            std::mt19937_64 rng(rd());
            std::uniform_int_distribution<uint64_t> skip_dist(MIN_WARMUP_ITERATIONS, MAX_WARMUP_ITERATIONS);
            skip_amt = skip_dist(rng);
        }
        
        // Perform the initial warm-up skip
        for (uint64_t i = 0; i < skip_amt; i++) {
            step_once();
        }

    } catch (const std::bad_alloc& e) {
        throw std::runtime_error(std::string("QuadraticIrrational: Memory allocation failed during construction: ") + e.what());
    } catch (const std::exception& e) {
        throw std::runtime_error(std::string("QuadraticIrrational: Initialization failed: ") + e.what());
    }
}


double QuadraticIrrational::next() {
    step_once();
    double val = mpfr_get_d(*value_->get(), MPFR_RNDN);
    if (std::isnan(val) || std::isinf(val)) {
         Rcpp::warning("QuadraticIrrational::next() produced NaN/Inf. Returning 0.5.");
         return 0.5; // A fallback value
    }
    return val;
}

void QuadraticIrrational::skip(uint64_t n) {
    jump_ahead(n);
}


void QuadraticIrrational::jump_ahead(uint64_t n) {
    if (!value_ || !next_ || !temp_ || !temp2_ || !root_ ||
        !value_->is_valid() || !next_->is_valid() || !temp_->is_valid() ||
        !temp2_->is_valid() || !root_->is_valid()) {
        throw std::runtime_error("QuadraticIrrational: Invalid MPFR state at the beginning of jump_ahead");
    }
    
    if (n == 0) {
        return; // No jump needed
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

size_t QuadraticIrrational::size() const {
    return 1;
}

void QuadraticIrrational::fill(double* buffer, size_t count) {
    for (size_t i = 0; i < count; i++) {
        buffer[i] = next();
    }
}

} // namespace qiprng