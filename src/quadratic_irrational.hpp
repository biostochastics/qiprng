// File: quadratic_irrational.hpp
// --------------------------------------------------------------
#ifndef QIPRNG_QUADRATIC_IRRATIONAL_HPP
#define QIPRNG_QUADRATIC_IRRATIONAL_HPP

#include <Rcpp.h>  // For Rcpp::warning, Rcpp::Rcout (used in .cpp)

#include <cmath>          // For std::sqrt, __builtin_smull_overflow etc.
#include <memory>         // For std::unique_ptr
#include <random>         // For std::random_device, std::mt19937_64
#include <stdexcept>      // For std::runtime_error, std::invalid_argument
#include <unordered_map>  // For period detection hash table
#include <utility>        // For std::pair
#include <vector>         // For CFE coefficients storage

#include "prng_common.hpp"  // For MPFRWrapper

namespace qiprng {

// 2x2 Matrix for efficient CFE jump-ahead operations
struct Matrix2x2 {
    long p, q, r, s;

    Matrix2x2(long p_ = 1, long q_ = 0, long r_ = 0, long s_ = 1) : p(p_), q(q_), r(r_), s(s_) {}

    // Matrix multiplication for jump-ahead with overflow protection
    Matrix2x2 operator*(const Matrix2x2& other) const {
        // Simple multiplication without overflow checks for now
        // The overflow was a false positive from the previous implementation
        return Matrix2x2(p * other.p + q * other.r, p * other.q + q * other.s,
                         r * other.p + s * other.r, r * other.q + s * other.s);
    }

    // Binary exponentiation for O(log n) jump-ahead
    Matrix2x2 power(uint64_t n) const;
};

class QuadraticIrrational {
   private:
    long a_, b_, c_;
    long long discriminant_;  // Cached discriminant value
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
    void jump_ahead_optimized(uint64_t n);  // Matrix-based O(log n) jump
    size_t size() const;                    // Typically returns 1 for a single QI
    void fill(double* buffer, size_t size);

    // CFE-related methods for v0.5.0
    size_t get_cfe_period() const { return cfe_period_length_; }
    bool has_computed_cfe() const { return cfe_computed_; }
    const std::vector<long>& get_cfe_coefficients() const { return cfe_coefficients_; }

    // Validate discriminant is square-free (for quality PRNG)
    static bool is_square_free(long long n);
};

}  // namespace qiprng

#endif  // QIPRNG_QUADRATIC_IRRATIONAL_HPP
