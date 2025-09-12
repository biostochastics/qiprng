// File: quadratic_irrational.hpp
// --------------------------------------------------------------
// Purpose: Core mathematical engine for quadratic irrational PRNG
// Author: Sergey Kornilov
// Version: 0.6.2
//
// This file implements the quadratic irrational number class that forms
// the mathematical foundation of the PRNG. It provides:
//   - Continued fraction expansion (CFE) computation
//   - High-precision arithmetic via MPFR
//   - O(log n) jump-ahead capability with multiple algorithms
//   - Overflow protection using 128-bit arithmetic
//   - Period detection and cycle analysis
//
// Mathematical basis:
// A quadratic irrational is a number of the form (a + √d)/c where d is
// not a perfect square. These numbers have periodic continued fraction
// expansions which we exploit for random number generation.
// --------------------------------------------------------------
#ifndef QIPRNG_QUADRATIC_IRRATIONAL_HPP
#define QIPRNG_QUADRATIC_IRRATIONAL_HPP

#include <Rcpp.h>  // For Rcpp::warning, Rcpp::Rcout (used in .cpp)

#include <climits>        // For LONG_MAX, LONG_MIN
#include <cmath>          // For std::sqrt, __builtin_smull_overflow etc.
#include <memory>         // For std::unique_ptr
#include <random>         // For std::random_device, std::mt19937_64
#include <stdexcept>      // For std::runtime_error, std::invalid_argument
#include <unordered_map>  // For period detection hash table
#include <utility>        // For std::pair
#include <vector>         // For CFE coefficients storage

#include "prng_common.hpp"  // For MPFRWrapper

namespace qiprng {

/**
 * Jump-ahead algorithm selection for efficient sequence advancement
 *
 * Different algorithms offer various trade-offs between speed and overflow safety:
 * - ORIGINAL_128BIT: Fastest but may overflow for very large jumps
 * - MPFR_MATRIX: Safest, handles arbitrary precision but slower
 * - MODULAR_MERSENNE: Good balance using 2^61-1 prime modulus
 * - DIRECT_CFE: Avoids matrix operations entirely
 */
enum class JumpAheadAlgorithm {
    ORIGINAL_128BIT = 0,   // Original implementation with 128-bit arithmetic (may overflow)
    MPFR_MATRIX = 1,       // Use MPFR for matrix operations (no overflow, slower)
    MODULAR_MERSENNE = 2,  // Use modular arithmetic with Mersenne prime (fast, no overflow)
    DIRECT_CFE = 3         // Direct CFE manipulation without matrices
};

/**
 * 2x2 Matrix for efficient CFE jump-ahead operations
 *
 * Represents transformation matrices used in the jump-ahead algorithm.
 * Matrix multiplication is used to compute large jumps in O(log n) time
 * via binary exponentiation.
 *
 * The matrix represents: [p q]
 *                        [r s]
 */
struct Matrix2x2 {
    long p, q, r, s;

    Matrix2x2(long p_ = 1, long q_ = 0, long r_ = 0, long s_ = 1) : p(p_), q(q_), r(r_), s(s_) {}

    /**
     * Matrix multiplication with overflow protection
     * Uses 128-bit arithmetic when available, otherwise uses builtin overflow checks
     * @param other The matrix to multiply with
     * @return Product matrix
     * @throws std::overflow_error if result doesn't fit in long
     */
    Matrix2x2 operator*(const Matrix2x2& other) const {
#ifdef __SIZEOF_INT128__
        // Use 128-bit arithmetic for overflow protection
        using int128_t = __int128;
        int128_t pp = static_cast<int128_t>(p) * other.p + static_cast<int128_t>(q) * other.r;
        int128_t pq = static_cast<int128_t>(p) * other.q + static_cast<int128_t>(q) * other.s;
        int128_t rp = static_cast<int128_t>(r) * other.p + static_cast<int128_t>(s) * other.r;
        int128_t rs = static_cast<int128_t>(r) * other.q + static_cast<int128_t>(s) * other.s;

        // Check if results fit in long
        constexpr int128_t LONG_MAX_128 = static_cast<int128_t>(LONG_MAX);
        constexpr int128_t LONG_MIN_128 = static_cast<int128_t>(LONG_MIN);

        if (pp > LONG_MAX_128 || pp < LONG_MIN_128 || pq > LONG_MAX_128 || pq < LONG_MIN_128 ||
            rp > LONG_MAX_128 || rp < LONG_MIN_128 || rs > LONG_MAX_128 || rs < LONG_MIN_128) {
            throw std::overflow_error("Matrix multiplication overflow in jump-ahead operation");
        }

        return Matrix2x2(static_cast<long>(pp), static_cast<long>(pq), static_cast<long>(rp),
                         static_cast<long>(rs));
#else
        // Fallback: Check for overflow using builtin functions (GCC/Clang)
        long new_p, new_q, new_r, new_s;
        long temp1, temp2;

        // Calculate p * other.p + q * other.r
        if (__builtin_smull_overflow(p, other.p, &temp1) ||
            __builtin_smull_overflow(q, other.r, &temp2) ||
            __builtin_saddl_overflow(temp1, temp2, &new_p)) {
            throw std::overflow_error("Matrix multiplication overflow in jump-ahead operation");
        }

        // Calculate p * other.q + q * other.s
        if (__builtin_smull_overflow(p, other.q, &temp1) ||
            __builtin_smull_overflow(q, other.s, &temp2) ||
            __builtin_saddl_overflow(temp1, temp2, &new_q)) {
            throw std::overflow_error("Matrix multiplication overflow in jump-ahead operation");
        }

        // Calculate r * other.p + s * other.r
        if (__builtin_smull_overflow(r, other.p, &temp1) ||
            __builtin_smull_overflow(s, other.r, &temp2) ||
            __builtin_saddl_overflow(temp1, temp2, &new_r)) {
            throw std::overflow_error("Matrix multiplication overflow in jump-ahead operation");
        }

        // Calculate r * other.q + s * other.s
        if (__builtin_smull_overflow(r, other.q, &temp1) ||
            __builtin_smull_overflow(s, other.s, &temp2) ||
            __builtin_saddl_overflow(temp1, temp2, &new_s)) {
            throw std::overflow_error("Matrix multiplication overflow in jump-ahead operation");
        }

        return Matrix2x2(new_p, new_q, new_r, new_s);
#endif
    }

    /**
     * Binary exponentiation for O(log n) jump-ahead
     * @param n The exponent
     * @return This matrix raised to the nth power
     * Complexity: O(log n) matrix multiplications
     */
    Matrix2x2 power(uint64_t n) const;
};

/**
 * Quadratic Irrational Number Generator
 *
 * This class represents a quadratic irrational number of the form (a + √d)/c
 * and provides methods for continued fraction expansion and random number
 * generation. The CFE produces a sequence of coefficients that, when properly
 * normalized, yield uniformly distributed random numbers.
 *
 * Mathematical Properties:
 * - The discriminant b²-4ac must be positive and not a perfect square
 * - The continued fraction expansion is eventually periodic
 * - The period length affects the quality of randomness
 *
 * Thread Safety:
 * - NOT thread-safe; each thread should have its own instance
 * - Or use external synchronization when sharing instances
 *
 * Performance:
 * - Next value generation: O(1) amortized
 * - Jump-ahead by n: O(log n)
 * - Initialization: O(period_length)
 */
class QuadraticIrrational {
   private:
    long a_, b_, c_;
    long long discriminant_;  // Cached discriminant value
    mpfr_prec_t mpfr_prec_;   // MPFR precision
    std::unique_ptr<MPFRWrapper> value_, root_;
    std::unique_ptr<MPFRWrapper> next_;
    std::unique_ptr<MPFRWrapper> temp_;
    std::unique_ptr<MPFRWrapper> temp2_;

    // CFE period detection and caching
    std::vector<long> cfe_coefficients_;  // Stores the CFE period
    size_t cfe_period_length_;            // Length of the period
    bool cfe_computed_;                   // Whether CFE has been computed

    // Gauss-Legendre CFE state variables
    long P_n_, Q_n_;  // Current P and Q in the recurrence

    // State tracking for reseed
    uint64_t seed_;
    bool has_seed_;
    uint64_t step_;
    std::unique_ptr<std::mt19937_64> rd_engine_;

    // Jump-ahead algorithm selection
    JumpAheadAlgorithm jump_algorithm_;

    // MPFR state structure for holding current values
    struct MPFRState {
        MPFRWrapper x;
        MPFRWrapper x_prev;
        MPFRState(mpfr_prec_t prec) : x(prec), x_prev(prec) {}
    };
    std::unique_ptr<MPFRState> state_;

    void step_once();

    // Enhanced CFE computation with Gauss-Legendre algorithm
    /**
     * @brief Computes the continued fraction expansion (CFE) period of the quadratic irrational
     * number.
     */
    void compute_cfe_period();
    void step_once_gauss_legendre();

   public:
    QuadraticIrrational(long a, long b, long c, mpfr_prec_t prec, uint64_t seed = 0,
                        bool has_seed = false);
    ~QuadraticIrrational() = default;  // Default destructor is fine with unique_ptr

    double next();
    void skip(uint64_t n);
    void jump_ahead(uint64_t n);
    void jump_ahead_optimized(uint64_t n);     // Matrix-based O(log n) jump
    void jump_ahead_optimized_v2(uint64_t n);  // Enhanced jump with multiple algorithms
    size_t size() const;                       // Typically returns 1 for a single QI
    void fill(double* buffer, size_t size);

    // CFE-related methods for v0.5.0
    size_t get_cfe_period() const { return cfe_period_length_; }
    bool has_computed_cfe() const { return cfe_computed_; }
    const std::vector<long>& get_cfe_coefficients() const { return cfe_coefficients_; }

    // Validate discriminant is square-free (for quality PRNG)
    static bool is_square_free(long long n);

    // Accessor methods for cloning
    long getA() const { return a_; }
    long getB() const { return b_; }
    long getC() const { return c_; }
    mpfr_prec_t getMPFRPrecision() const { return mpfr_prec_; }

    // Reseed method for refreshing state
    void reseed();
};

}  // namespace qiprng

#endif  // QIPRNG_QUADRATIC_IRRATIONAL_HPP
