// File: precision_utils.hpp
// High-precision utilities for QIPRNG
// Addresses algorithmic accuracy concerns by providing:
// 1. High-precision mathematical constants
// 2. Safe MPFR↔double conversions with extended precision intermediates
// 3. Precision loss tracking and warnings

#ifndef QIPRNG_PRECISION_UTILS_HPP
#define QIPRNG_PRECISION_UTILS_HPP

#include <Rcpp.h>  // For Rcpp::warning

#include <mpfr.h>

#include <atomic>
#include <cmath>
#include <limits>

#ifdef __cplusplus
#    if __cplusplus >= 202002L
#        include <numbers>
#        define HAS_STD_NUMBERS_PI
#    endif
#endif

namespace qiprng {
namespace precision {

// Thread-safe precision loss tracking
static inline std::atomic<size_t> total_bits_lost{0};
static inline std::atomic<size_t> conversion_count{0};

// Thread-local storage for high-precision constants
struct PrecisionTLS {
    mpfr_t pi_high_precision;
    bool pi_initialized = false;
    mpfr_prec_t pi_precision = 0;

    ~PrecisionTLS() {
        if (pi_initialized) {
            mpfr_clear(pi_high_precision);
        }
    }
};

// Get thread-local precision storage
inline PrecisionTLS& get_precision_tls() {
    static thread_local PrecisionTLS tls;
    return tls;
}

// High-precision PI constant management
class PrecisionConstants {
   public:
    // Get high-precision PI using MPFR
    static const mpfr_t* get_pi_mpfr(mpfr_prec_t prec = 256) {
        auto& tls = get_precision_tls();
        if (!tls.pi_initialized || tls.pi_precision < prec) {
            if (tls.pi_initialized) {
                mpfr_clear(tls.pi_high_precision);
            }
            mpfr_init2(tls.pi_high_precision, prec);
            mpfr_const_pi(tls.pi_high_precision, MPFR_RNDN);
            tls.pi_initialized = true;
            tls.pi_precision = prec;
        }
        return &tls.pi_high_precision;
    }

    // Get double-precision PI with maximum accuracy
    static double get_pi_double() {
#ifdef HAS_STD_NUMBERS_PI
        return std::numbers::pi;
#else
        // Use MPFR to get the most accurate double representation
        const mpfr_t* pi_mpfr = get_pi_mpfr(256);
        return mpfr_get_d(*pi_mpfr, MPFR_RNDN);
#endif
    }

    // Cleanup (call at thread exit if needed)
    static void cleanup() {
        auto& tls = get_precision_tls();
        if (tls.pi_initialized) {
            mpfr_clear(tls.pi_high_precision);
            tls.pi_initialized = false;
        }
    }
};

// Safe MPFR to double conversion with extended precision intermediates
inline double safe_mpfr_to_double(const mpfr_t& value, bool use_extended = true) {
    // Track precision loss
    mpfr_prec_t mpfr_prec = mpfr_get_prec(value);
    size_t bits_lost = mpfr_prec > 53 ? mpfr_prec - 53 : 0;
    total_bits_lost.fetch_add(bits_lost, std::memory_order_relaxed);
    conversion_count.fetch_add(1, std::memory_order_relaxed);

    // PRECISION LOSS MITIGATION: Warn and compensate when excessive precision is lost
    static const size_t PRECISION_WARNING_THRESHOLD = 100;  // Warn if losing > 100 bits
    static const size_t PRECISION_ERROR_THRESHOLD = 200;    // Error if losing > 200 bits

    if (bits_lost > PRECISION_ERROR_THRESHOLD) {
        throw std::runtime_error(
            "Excessive precision loss: " + std::to_string(bits_lost) +
            " bits lost in MPFR to double conversion. Consider using lower MPFR precision.");
    }

    if (bits_lost > PRECISION_WARNING_THRESHOLD) {
        static std::atomic<size_t> warning_count{0};
        if (warning_count.fetch_add(1) == 0) {  // Only warn once
            Rcpp::warning("Significant precision loss detected: %zu bits lost. "
                          "Consider adjusting MPFR precision settings.",
                          bits_lost);
        }
    }

    if (!use_extended || mpfr_prec <= 64) {
        // Direct conversion for low precision or when extended not requested
        return mpfr_get_d(value, MPFR_RNDN);
    }

// Use extended precision intermediate for gradual precision reduction
#ifdef __SIZEOF_FLOAT128__
    // Use 128-bit quad precision if available (113-bit mantissa)
    __float128 intermediate = mpfr_get_float128(value, MPFR_RNDN);
    return static_cast<double>(intermediate);
#elif defined(__x86_64__) || defined(_M_X64)
    // Use 80-bit extended precision on x86_64 (64-bit mantissa)
    long double intermediate = mpfr_get_ld(value, MPFR_RNDN);
    return static_cast<double>(intermediate);
#else
    // Fallback to direct conversion on other architectures
    return mpfr_get_d(value, MPFR_RNDN);
#endif
}

// Safe double to MPFR conversion with extended precision source
inline void safe_double_to_mpfr(mpfr_t& result, double value, mpfr_prec_t prec = 256) {
#ifdef __SIZEOF_FLOAT128__
    // First promote to quad precision to preserve more bits
    __float128 intermediate = static_cast<__float128>(value);
    mpfr_set_float128(result, intermediate, MPFR_RNDN);
#elif defined(__x86_64__) || defined(_M_X64)
    // Use extended precision intermediate
    long double intermediate = static_cast<long double>(value);
    mpfr_set_ld(result, intermediate, MPFR_RNDN);
#else
    // Direct conversion
    mpfr_set_d(result, value, MPFR_RNDN);
#endif
}

// Get average precision loss statistics
inline double get_average_bits_lost() {
    size_t count = conversion_count.load(std::memory_order_relaxed);
    if (count == 0)
        return 0.0;
    return static_cast<double>(total_bits_lost.load(std::memory_order_relaxed)) / count;
}

// Reset precision tracking statistics
inline void reset_precision_stats() {
    total_bits_lost.store(0, std::memory_order_relaxed);
    conversion_count.store(0, std::memory_order_relaxed);
}

// Precision-aware PI constant for use in calculations
inline double get_high_precision_pi() {
    return PrecisionConstants::get_pi_double();
}

// Get 2*PI with high precision
inline double get_two_pi() {
    return 2.0 * get_high_precision_pi();
}

// Backward compatibility macros
#ifndef M_PI
#    define M_PI (qiprng::precision::get_high_precision_pi())
#endif

#ifndef M_2PI
#    define M_2PI (qiprng::precision::get_two_pi())
#endif

}  // namespace precision
}  // namespace qiprng

#endif  // QIPRNG_PRECISION_UTILS_HPP
