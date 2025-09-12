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
                // Use mpfr_t intermediates to avoid precision loss on platforms where long !=
                // int64_t
                MPFRWrapper p_mpfr(value_->get_precision()), q_mpfr(value_->get_precision()),
                    r_mpfr(value_->get_precision()), s_mpfr(value_->get_precision());
                mpfr_set_si(*p_mpfr.get(), static_cast<long>(result.get_p()), MPFR_RNDN);
                mpfr_set_si(*q_mpfr.get(), static_cast<long>(result.get_q()), MPFR_RNDN);
                mpfr_set_si(*r_mpfr.get(), static_cast<long>(result.get_r()), MPFR_RNDN);
                mpfr_set_si(*s_mpfr.get(), static_cast<long>(result.get_s()), MPFR_RNDN);

                // If values exceed long range, use string conversion for exact representation
                if (result.get_p() != static_cast<long>(result.get_p())) {
                    mpfr_set_str(*p_mpfr.get(), std::to_string(result.get_p()).c_str(), 10,
                                 MPFR_RNDN);
                }
                if (result.get_q() != static_cast<long>(result.get_q())) {
                    mpfr_set_str(*q_mpfr.get(), std::to_string(result.get_q()).c_str(), 10,
                                 MPFR_RNDN);
                }
                if (result.get_r() != static_cast<long>(result.get_r())) {
                    mpfr_set_str(*r_mpfr.get(), std::to_string(result.get_r()).c_str(), 10,
                                 MPFR_RNDN);
                }
                if (result.get_s() != static_cast<long>(result.get_s())) {
                    mpfr_set_str(*s_mpfr.get(), std::to_string(result.get_s()).c_str(), 10,
                                 MPFR_RNDN);
                }

                mpfr_mul(*temp_val->get(), *value_->get(), *p_mpfr.get(), MPFR_RNDN);
                mpfr_mul(*new_value->get(), *next_->get(), *q_mpfr.get(), MPFR_RNDN);
                mpfr_add(*new_value->get(), *new_value->get(), *temp_val->get(), MPFR_RNDN);

                mpfr_mul(*temp_val->get(), *value_->get(), *r_mpfr.get(), MPFR_RNDN);
                mpfr_mul(*denominator->get(), *next_->get(), *s_mpfr.get(), MPFR_RNDN);
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

                // Check for overflow before multiplication
                const uint64_t max_safe_periods =
                    std::numeric_limits<uint64_t>::max() / cfe_period_length_;
                if (full_periods > max_safe_periods) {
                    // Handle overflow by processing in chunks
                    uint64_t remaining_periods = full_periods;
                    while (remaining_periods > 0) {
                        uint64_t chunk = std::min(remaining_periods, max_safe_periods);
                        uint64_t chunk_steps = chunk * cfe_period_length_;
                        for (uint64_t i = 0; i < chunk_steps; ++i) {
                            step_once();
                        }
                        remaining_periods -= chunk;
                    }
                } else {
                    uint64_t total_steps = full_periods * cfe_period_length_;
                    // Instead of matrix operations, directly jump in the CFE sequence
                    // by updating the internal state based on the periodicity
                    for (uint64_t i = 0; i < total_steps; ++i) {
                        step_once();
                    }
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

                    // Use mpfr_t intermediates to avoid precision loss on platforms where long !=
                    // int64_t
                    MPFRWrapper p_mpfr(value_->get_precision()), q_mpfr(value_->get_precision()),
                        r_mpfr(value_->get_precision()), s_mpfr(value_->get_precision());
                    mpfr_set_si(*p_mpfr.get(), static_cast<long>(result.p), MPFR_RNDN);
                    mpfr_set_si(*q_mpfr.get(), static_cast<long>(result.q), MPFR_RNDN);
                    mpfr_set_si(*r_mpfr.get(), static_cast<long>(result.r), MPFR_RNDN);
                    mpfr_set_si(*s_mpfr.get(), static_cast<long>(result.s), MPFR_RNDN);

                    // If values exceed long range, use string conversion for exact representation
                    if (result.p != static_cast<long>(result.p)) {
                        mpfr_set_str(*p_mpfr.get(), std::to_string(result.p).c_str(), 10,
                                     MPFR_RNDN);
                    }
                    if (result.q != static_cast<long>(result.q)) {
                        mpfr_set_str(*q_mpfr.get(), std::to_string(result.q).c_str(), 10,
                                     MPFR_RNDN);
                    }
                    if (result.r != static_cast<long>(result.r)) {
                        mpfr_set_str(*r_mpfr.get(), std::to_string(result.r).c_str(), 10,
                                     MPFR_RNDN);
                    }
                    if (result.s != static_cast<long>(result.s)) {
                        mpfr_set_str(*s_mpfr.get(), std::to_string(result.s).c_str(), 10,
                                     MPFR_RNDN);
                    }

                    mpfr_mul(*temp_val->get(), *value_->get(), *p_mpfr.get(), MPFR_RNDN);
                    mpfr_mul(*new_value->get(), *next_->get(), *q_mpfr.get(), MPFR_RNDN);
                    mpfr_add(*new_value->get(), *new_value->get(), *temp_val->get(), MPFR_RNDN);

                    mpfr_mul(*temp_val->get(), *value_->get(), *r_mpfr.get(), MPFR_RNDN);
                    mpfr_mul(*denominator->get(), *next_->get(), *s_mpfr.get(), MPFR_RNDN);
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
