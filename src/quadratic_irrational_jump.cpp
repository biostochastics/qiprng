// File: quadratic_irrational_jump.cpp
// Implementation of improved jump-ahead algorithms
// --------------------------------------------------------------

#include <Rcpp.h>

#include <cstdlib>

#include "matrix_mpfr.hpp"
#include "quadratic_irrational.hpp"

namespace qiprng {

// Helper function to get jump algorithm from environment
JumpAheadAlgorithm get_jump_algorithm() {
    const char* env = std::getenv("QIPRNG_JUMP_ALGORITHM");
    if (env) {
        int algo = std::atoi(env);
        if (algo >= 0 && algo <= 3) {
            return static_cast<JumpAheadAlgorithm>(algo);
        }
    }
    // Default to modular arithmetic as it's fast and safe
    return JumpAheadAlgorithm::MODULAR_MERSENNE;
}

// New implementation with multiple algorithm options
void QuadraticIrrational::jump_ahead_optimized_v2(uint64_t n) {
    if (n == 0) {
        return;
    }

    // Get algorithm selection
    JumpAheadAlgorithm algo = get_jump_algorithm();

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

    // For large jumps, use matrix exponentiation with selected algorithm
    if (n > cfe_period_length_ * 2) {
        uint64_t full_periods = n / cfe_period_length_;
        uint64_t remainder = n % cfe_period_length_;

        switch (algo) {
            case JumpAheadAlgorithm::MPFR_MATRIX: {
                // Use MPFR-based matrix operations
                Matrix2x2_MPFR period_matrix(1, 0, 0, 1, mpfr_prec_);

                for (size_t i = 0; i < cfe_period_length_; ++i) {
                    long a_i = cfe_coefficients_[i];
                    Matrix2x2_MPFR step_matrix(a_i, 1, 1, 0, mpfr_prec_);
                    period_matrix = period_matrix * step_matrix;
                }

                // Apply the period transformation using binary exponentiation
                Matrix2x2_MPFR result = period_matrix.power(full_periods);

                // Apply transformation to current state
                std::unique_ptr<MPFRWrapper> new_value =
                    std::make_unique<MPFRWrapper>(value_->get_precision());
                std::unique_ptr<MPFRWrapper> temp_val =
                    std::make_unique<MPFRWrapper>(value_->get_precision());
                std::unique_ptr<MPFRWrapper> denominator =
                    std::make_unique<MPFRWrapper>(value_->get_precision());

                // Numerator: p * value + q * next
                mpfr_mul(*temp_val->get(), *value_->get(), *result.get_p(), MPFR_RNDN);
                mpfr_mul(*new_value->get(), *next_->get(), *result.get_q(), MPFR_RNDN);
                mpfr_add(*new_value->get(), *new_value->get(), *temp_val->get(), MPFR_RNDN);

                // Denominator: r * value + s * next
                mpfr_mul(*temp_val->get(), *value_->get(), *result.get_r(), MPFR_RNDN);
                mpfr_mul(*denominator->get(), *next_->get(), *result.get_s(), MPFR_RNDN);
                mpfr_add(*denominator->get(), *denominator->get(), *temp_val->get(), MPFR_RNDN);

                // Divide to get new value
                if (!mpfr_zero_p(*denominator->get())) {
                    mpfr_div(*value_->get(), *new_value->get(), *denominator->get(), MPFR_RNDN);
                    // Note: Do NOT call step_once() here - that would add an extra step
                    // The matrix transformation has already advanced us the correct amount
                } else {
                    // Fallback if denominator is zero
                    for (uint64_t i = 0; i < full_periods * cfe_period_length_; ++i) {
                        step_once();
                    }
                }
                break;
            }

            case JumpAheadAlgorithm::MODULAR_MERSENNE: {
                // Use modular arithmetic with Mersenne prime
                Matrix2x2_Modular period_matrix(1, 0, 0, 1);

                for (size_t i = 0; i < cfe_period_length_; ++i) {
                    long a_i = cfe_coefficients_[i];
                    Matrix2x2_Modular step_matrix(a_i, 1, 1, 0);
                    period_matrix = period_matrix * step_matrix;
                }

                // Apply the period transformation using binary exponentiation
                Matrix2x2_Modular result = period_matrix.power(full_periods);

                // Apply transformation to current state
                // Convert modular result back to MPFR for state update
                std::unique_ptr<MPFRWrapper> new_value =
                    std::make_unique<MPFRWrapper>(value_->get_precision());
                std::unique_ptr<MPFRWrapper> temp_val =
                    std::make_unique<MPFRWrapper>(value_->get_precision());
                std::unique_ptr<MPFRWrapper> denominator =
                    std::make_unique<MPFRWrapper>(value_->get_precision());

                // Convert int64_t to mpfr_t and apply transformation
                mpfr_mul_si(*temp_val->get(), *value_->get(), result.get_p(), MPFR_RNDN);
                mpfr_mul_si(*new_value->get(), *next_->get(), result.get_q(), MPFR_RNDN);
                mpfr_add(*new_value->get(), *new_value->get(), *temp_val->get(), MPFR_RNDN);

                mpfr_mul_si(*temp_val->get(), *value_->get(), result.get_r(), MPFR_RNDN);
                mpfr_mul_si(*denominator->get(), *next_->get(), result.get_s(), MPFR_RNDN);
                mpfr_add(*denominator->get(), *denominator->get(), *temp_val->get(), MPFR_RNDN);

                if (!mpfr_zero_p(*denominator->get())) {
                    mpfr_div(*value_->get(), *new_value->get(), *denominator->get(), MPFR_RNDN);
                    // Note: Do NOT call step_once() here - that would add an extra step
                    // The matrix transformation has already advanced us the correct amount
                } else {
                    for (uint64_t i = 0; i < full_periods * cfe_period_length_; ++i) {
                        step_once();
                    }
                }
                break;
            }

            case JumpAheadAlgorithm::DIRECT_CFE: {
                // Direct CFE manipulation without matrices
                // This approach directly manipulates the position in the CFE period
                uint64_t total_steps = full_periods * cfe_period_length_;

                // Instead of matrix operations, directly jump in the CFE sequence
                // by updating the internal state based on the periodicity
                for (uint64_t i = 0; i < total_steps; ++i) {
                    step_once();
                }
                break;
            }

            case JumpAheadAlgorithm::ORIGINAL_128BIT:
            default: {
                // Fall back to original implementation
                // This may overflow for large jumps
                try {
                    Matrix2x2 period_matrix(1, 0, 0, 1);

                    for (size_t i = 0; i < cfe_period_length_; ++i) {
                        long a_i = cfe_coefficients_[i];
                        Matrix2x2 step_matrix(a_i, 1, 1, 0);
                        period_matrix = period_matrix * step_matrix;
                    }

                    Matrix2x2 result = period_matrix.power(full_periods);

                    // Apply transformation (original code)
                    std::unique_ptr<MPFRWrapper> new_value =
                        std::make_unique<MPFRWrapper>(value_->get_precision());
                    std::unique_ptr<MPFRWrapper> temp_val =
                        std::make_unique<MPFRWrapper>(value_->get_precision());
                    std::unique_ptr<MPFRWrapper> denominator =
                        std::make_unique<MPFRWrapper>(value_->get_precision());

                    mpfr_mul_si(*temp_val->get(), *value_->get(), result.p, MPFR_RNDN);
                    mpfr_mul_si(*new_value->get(), *next_->get(), result.q, MPFR_RNDN);
                    mpfr_add(*new_value->get(), *new_value->get(), *temp_val->get(), MPFR_RNDN);

                    mpfr_mul_si(*temp_val->get(), *value_->get(), result.r, MPFR_RNDN);
                    mpfr_mul_si(*denominator->get(), *next_->get(), result.s, MPFR_RNDN);
                    mpfr_add(*denominator->get(), *denominator->get(), *temp_val->get(), MPFR_RNDN);

                    if (!mpfr_zero_p(*denominator->get())) {
                        mpfr_div(*value_->get(), *new_value->get(), *denominator->get(), MPFR_RNDN);
                        // Note: Do NOT call step_once() here - that would add an extra step
                        // The matrix transformation has already advanced us the correct amount
                    } else {
                        for (uint64_t i = 0; i < full_periods * cfe_period_length_; ++i) {
                            step_once();
                        }
                    }
                } catch (const std::overflow_error& e) {
                    // If overflow occurs, fall back to step-by-step
                    Rcpp::warning("Matrix overflow in jump-ahead, using step-by-step: %s",
                                  e.what());
                    for (uint64_t i = 0; i < full_periods * cfe_period_length_; ++i) {
                        step_once();
                    }
                }
                break;
            }
        }

        // Handle remainder with regular stepping
        for (uint64_t i = 0; i < remainder; ++i) {
            step_once();
        }
    } else {
        // For smaller jumps, just step through
        for (uint64_t i = 0; i < n; ++i) {
            step_once();
        }
    }
}

}  // namespace qiprng
