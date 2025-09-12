#ifndef QIPRNG_MATRIX_MPFR_HPP
#define QIPRNG_MATRIX_MPFR_HPP

#include <mpfr.h>

#include <cstdint>
#include <memory>

#include "prng_common.hpp"

namespace qiprng {

// MPFR-based 2x2 matrix for high-precision matrix operations
class Matrix2x2_MPFR {
   private:
    std::unique_ptr<MPFRWrapper> p_, q_, r_, s_;
    mpfr_prec_t precision_;

   public:
    // Constructor
    Matrix2x2_MPFR(mpfr_prec_t prec = 256) : precision_(prec) {
        p_ = std::make_unique<MPFRWrapper>(prec);
        q_ = std::make_unique<MPFRWrapper>(prec);
        r_ = std::make_unique<MPFRWrapper>(prec);
        s_ = std::make_unique<MPFRWrapper>(prec);

        // Initialize as identity matrix by default
        mpfr_set_si(*p_->get(), 1, MPFR_RNDN);
        mpfr_set_si(*q_->get(), 0, MPFR_RNDN);
        mpfr_set_si(*r_->get(), 0, MPFR_RNDN);
        mpfr_set_si(*s_->get(), 1, MPFR_RNDN);
    }

    // Constructor from long values
    Matrix2x2_MPFR(long p, long q, long r, long s, mpfr_prec_t prec = 256) : precision_(prec) {
        p_ = std::make_unique<MPFRWrapper>(prec);
        q_ = std::make_unique<MPFRWrapper>(prec);
        r_ = std::make_unique<MPFRWrapper>(prec);
        s_ = std::make_unique<MPFRWrapper>(prec);

        mpfr_set_si(*p_->get(), p, MPFR_RNDN);
        mpfr_set_si(*q_->get(), q, MPFR_RNDN);
        mpfr_set_si(*r_->get(), r, MPFR_RNDN);
        mpfr_set_si(*s_->get(), s, MPFR_RNDN);
    }

    // Copy constructor
    Matrix2x2_MPFR(const Matrix2x2_MPFR& other) : precision_(other.precision_) {
        p_ = std::make_unique<MPFRWrapper>(precision_);
        q_ = std::make_unique<MPFRWrapper>(precision_);
        r_ = std::make_unique<MPFRWrapper>(precision_);
        s_ = std::make_unique<MPFRWrapper>(precision_);

        mpfr_set(*p_->get(), *other.p_->get(), MPFR_RNDN);
        mpfr_set(*q_->get(), *other.q_->get(), MPFR_RNDN);
        mpfr_set(*r_->get(), *other.r_->get(), MPFR_RNDN);
        mpfr_set(*s_->get(), *other.s_->get(), MPFR_RNDN);
    }

    // Assignment operator
    Matrix2x2_MPFR& operator=(const Matrix2x2_MPFR& other) {
        if (this != &other) {
            // Sync precision and (re)allocate wrappers if necessary
            if (precision_ != other.precision_) {
                precision_ = other.precision_;
                p_ = std::make_unique<MPFRWrapper>(precision_);
                q_ = std::make_unique<MPFRWrapper>(precision_);
                r_ = std::make_unique<MPFRWrapper>(precision_);
                s_ = std::make_unique<MPFRWrapper>(precision_);
            }
            mpfr_set(*p_->get(), *other.p_->get(), MPFR_RNDN);
            mpfr_set(*q_->get(), *other.q_->get(), MPFR_RNDN);
            mpfr_set(*r_->get(), *other.r_->get(), MPFR_RNDN);
            mpfr_set(*s_->get(), *other.s_->get(), MPFR_RNDN);
        }
        return *this;
    }

    // Matrix multiplication using MPFR
    Matrix2x2_MPFR operator*(const Matrix2x2_MPFR& other) const {
        Matrix2x2_MPFR result(precision_);

        // Temporary variables for intermediate calculations
        MPFRWrapper temp1(precision_), temp2(precision_);

        // result.p = p * other.p + q * other.r
        mpfr_mul(*temp1.get(), *p_->get(), *other.p_->get(), MPFR_RNDN);
        mpfr_mul(*temp2.get(), *q_->get(), *other.r_->get(), MPFR_RNDN);
        mpfr_add(*result.p_->get(), *temp1.get(), *temp2.get(), MPFR_RNDN);

        // result.q = p * other.q + q * other.s
        mpfr_mul(*temp1.get(), *p_->get(), *other.q_->get(), MPFR_RNDN);
        mpfr_mul(*temp2.get(), *q_->get(), *other.s_->get(), MPFR_RNDN);
        mpfr_add(*result.q_->get(), *temp1.get(), *temp2.get(), MPFR_RNDN);

        // result.r = r * other.p + s * other.r
        mpfr_mul(*temp1.get(), *r_->get(), *other.p_->get(), MPFR_RNDN);
        mpfr_mul(*temp2.get(), *s_->get(), *other.r_->get(), MPFR_RNDN);
        mpfr_add(*result.r_->get(), *temp1.get(), *temp2.get(), MPFR_RNDN);

        // result.s = r * other.q + s * other.s
        mpfr_mul(*temp1.get(), *r_->get(), *other.q_->get(), MPFR_RNDN);
        mpfr_mul(*temp2.get(), *s_->get(), *other.s_->get(), MPFR_RNDN);
        mpfr_add(*result.s_->get(), *temp1.get(), *temp2.get(), MPFR_RNDN);

        return result;
    }

    // Binary exponentiation for O(log n) power
    Matrix2x2_MPFR power(uint64_t n) const {
        if (n == 0) {
            return Matrix2x2_MPFR(precision_);  // Identity matrix
        }

        Matrix2x2_MPFR result(precision_);  // Identity
        Matrix2x2_MPFR base(*this);

        while (n > 0) {
            if (n & 1) {
                result = result * base;
            }
            base = base * base;
            n >>= 1;
        }

        return result;
    }

    // Getters for matrix elements
    mpfr_t* get_p() { return p_->get(); }
    mpfr_t* get_q() { return q_->get(); }
    mpfr_t* get_r() { return r_->get(); }
    mpfr_t* get_s() { return s_->get(); }

    const mpfr_t* get_p() const { return p_->get(); }
    const mpfr_t* get_q() const { return q_->get(); }
    const mpfr_t* get_r() const { return r_->get(); }
    const mpfr_t* get_s() const { return s_->get(); }
};

// Modular arithmetic version with Mersenne prime
class Matrix2x2_Modular {
   private:
    // Use 2^61 - 1 as modulus (Mersenne prime)
    static constexpr int64_t MOD = 2305843009213693951LL;
    int64_t p, q, r, s;

    // Modular multiplication without overflow
    static int64_t mulmod(int64_t a, int64_t b) {
        // Handle negative values
        a = ((a % MOD) + MOD) % MOD;
        b = ((b % MOD) + MOD) % MOD;

#ifdef __SIZEOF_INT128__
        // Use 128-bit arithmetic for efficient computation
        __int128 prod = (__int128)a * b;
        // For Mersenne prime 2^61 - 1, we can use the property that
        // x mod (2^61 - 1) = (x & (2^61 - 1)) + (x >> 61)
        int64_t res = (int64_t)((prod & MOD) + (prod >> 61));
        // May need one more reduction
        if (res >= MOD)
            res -= MOD;
        return res;
#else
        // Fallback: decomposition method (still better than loops)
        // Split into 30-bit chunks to avoid overflow
        int64_t a_hi = a >> 30;
        int64_t a_lo = a & ((1LL << 30) - 1);
        int64_t b_hi = b >> 30;
        int64_t b_lo = b & ((1LL << 30) - 1);

        // Compute partial products
        int64_t res = (a_lo * b_lo) % MOD;

        // Middle terms: (a_hi * b_lo + a_lo * b_hi) * 2^30
        int64_t mid = ((a_hi * b_lo) % MOD + (a_lo * b_hi) % MOD) % MOD;
        // Multiply by 2^30 using repeated doubling (30 times is acceptable)
        for (int i = 0; i < 30; i++) {
            mid = (mid * 2) % MOD;
        }
        res = (res + mid) % MOD;

        // High term: a_hi * b_hi * 2^60
        int64_t high = (a_hi * b_hi) % MOD;
        // Multiply by 2^60 (close to modulus, be careful)
        for (int i = 0; i < 60; i++) {
            high = (high * 2) % MOD;
        }
        res = (res + high) % MOD;

        return res;
#endif
    }

    static int64_t addmod(int64_t a, int64_t b) {
        a = ((a % MOD) + MOD) % MOD;
        b = ((b % MOD) + MOD) % MOD;
        return (a + b) % MOD;
    }

   public:
    Matrix2x2_Modular(int64_t p_ = 1, int64_t q_ = 0, int64_t r_ = 0, int64_t s_ = 1)
        : p(((p_ % MOD) + MOD) % MOD), q(((q_ % MOD) + MOD) % MOD), r(((r_ % MOD) + MOD) % MOD),
          s(((s_ % MOD) + MOD) % MOD) {}

    Matrix2x2_Modular operator*(const Matrix2x2_Modular& other) const {
        return Matrix2x2_Modular(addmod(mulmod(p, other.p), mulmod(q, other.r)),
                                 addmod(mulmod(p, other.q), mulmod(q, other.s)),
                                 addmod(mulmod(r, other.p), mulmod(s, other.r)),
                                 addmod(mulmod(r, other.q), mulmod(s, other.s)));
    }

    Matrix2x2_Modular power(uint64_t n) const {
        if (n == 0) {
            return Matrix2x2_Modular();  // Identity
        }

        Matrix2x2_Modular result;  // Identity
        Matrix2x2_Modular base = *this;

        while (n > 0) {
            if (n & 1) {
                result = result * base;
            }
            base = base * base;
            n >>= 1;
        }

        return result;
    }

    // Getters
    int64_t get_p() const { return p; }
    int64_t get_q() const { return q; }
    int64_t get_r() const { return r; }
    int64_t get_s() const { return s; }
};

}  // namespace qiprng

#endif  // QIPRNG_MATRIX_MPFR_HPP
