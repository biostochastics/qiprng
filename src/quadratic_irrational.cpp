// File: quadratic_irrational.cpp
// --------------------------------------------------------------
#include "quadratic_irrational.hpp"
#include <limits> // For std::numeric_limits

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

QuadraticIrrational::QuadraticIrrational(long a, long b, long c, mpfr_prec_t prec)
    : a_(a), b_(b), c_(c) {
    // Validate parameters more comprehensively
    if (a == 0) {
        throw std::invalid_argument("QuadraticIrrational: 'a' parameter cannot be zero");
    }
    
    if (prec < MPFR_PREC_MIN || prec > MPFR_PREC_MAX) {
        throw std::invalid_argument("QuadraticIrrational: Invalid precision value");
    }
    
    // Check for potential integer overflow in discriminant calculation
    // b² - 4ac
    const long long MAX_SAFE_LONG = std::numeric_limits<long>::max();
    
    // Check if b can safely be squared
    if (std::abs(static_cast<long long>(b)) > static_cast<long long>(std::sqrt(static_cast<double>(MAX_SAFE_LONG)))) {
        throw std::runtime_error("QuadraticIrrational: 'b' parameter is too large, would cause overflow in discriminant calculation");
    }
    
    // Check for the 4*a*c calculation
    if (std::abs(a) > MAX_SAFE_LONG / 4 || (a != 0 && std::abs(c) > MAX_SAFE_LONG / (4 * std::abs(a)))) {
        throw std::runtime_error("QuadraticIrrational: 'a' and 'c' parameters would cause overflow in 4*a*c calculation");
    }
    
    // Now safely calculate discriminant
    long long b_ll = static_cast<long long>(b_);
    long long a_ll = static_cast<long long>(a_);
    long long c_ll = static_cast<long long>(c_);
    long long b_squared = b_ll * b_ll;
    long long four_ac = 4LL * a_ll * c_ll;
    long long disc_ll = b_squared - four_ac;
    
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
        if (op_ret != 0 && !suppress_mpfr_warnings) {
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


        std::random_device rd;
        std::mt19937_64 rng(rd());
        std::uniform_int_distribution<uint64_t> skip_dist(1000, 10000); // Ensure a decent initial skip
        uint64_t skip_amt = skip_dist(rng);
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
    for (uint64_t i = 0; i < n; i++) {
        step_once();
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