#ifndef SIMD_OPERATIONS_HPP
#define SIMD_OPERATIONS_HPP

#include <cstddef>
#include <cstdint>
#include <cstring>
#include <algorithm>
#include "bit_operations.hpp"

// Platform detection
#if defined(__x86_64__) || defined(_M_X64) || defined(__i386__) || defined(_M_IX86)
    #define QIPRNG_X86
    #if defined(__AVX2__)
        #define QIPRNG_AVX2
        #include <immintrin.h>
    #elif defined(__SSE2__)
        #define QIPRNG_SSE2
        #include <emmintrin.h>
    #endif
#elif defined(__ARM_NEON) || defined(__aarch64__)
    #define QIPRNG_NEON
    #include <arm_neon.h>
#endif

namespace qiprng {
namespace simd {

// SIMD batch size constants
#ifdef QIPRNG_AVX2
    constexpr size_t SIMD_WIDTH = 4;  // 4 doubles per AVX2 register
    using simd_double = __m256d;
#elif defined(QIPRNG_SSE2)
    constexpr size_t SIMD_WIDTH = 2;  // 2 doubles per SSE2 register
    using simd_double = __m128d;
#elif defined(QIPRNG_NEON)
    constexpr size_t SIMD_WIDTH = 2;  // 2 doubles per NEON register
    using simd_double = float64x2_t;
#else
    constexpr size_t SIMD_WIDTH = 1;  // Fallback to scalar
    using simd_double = double;
#endif

// Check if SIMD is available
inline bool is_simd_available() {
#if defined(QIPRNG_AVX2) || defined(QIPRNG_SSE2) || defined(QIPRNG_NEON)
    return true;
#else
    return false;
#endif
}

// Vectorized XOR mixing for double arrays
inline void xor_mix_batch(double* dest, const double* src1, const double* src2, size_t count) {
#ifdef QIPRNG_AVX2
    size_t simd_count = count / 4;
    size_t remainder = count % 4;
    
    for (size_t i = 0; i < simd_count; ++i) {
        __m256i v1 = _mm256_loadu_si256(reinterpret_cast<const __m256i*>(src1 + i * 4));
        __m256i v2 = _mm256_loadu_si256(reinterpret_cast<const __m256i*>(src2 + i * 4));
        __m256i result = _mm256_xor_si256(v1, v2);
        _mm256_storeu_si256(reinterpret_cast<__m256i*>(dest + i * 4), result);
    }
    
    // Handle remainder
    for (size_t i = simd_count * 4; i < count; ++i) {
        dest[i] = bit_ops::xor_doubles_as_uint64(src1[i], src2[i]);
    }
    
#elif defined(QIPRNG_SSE2)
    size_t simd_count = count / 2;
    size_t remainder = count % 2;
    
    for (size_t i = 0; i < simd_count; ++i) {
        __m128i v1 = _mm_loadu_si128(reinterpret_cast<const __m128i*>(src1 + i * 2));
        __m128i v2 = _mm_loadu_si128(reinterpret_cast<const __m128i*>(src2 + i * 2));
        __m128i result = _mm_xor_si128(v1, v2);
        _mm_storeu_si128(reinterpret_cast<__m128i*>(dest + i * 2), result);
    }
    
    // Handle remainder
    for (size_t i = simd_count * 2; i < count; ++i) {
        dest[i] = bit_ops::xor_doubles_as_uint64(src1[i], src2[i]);
    }
    
#elif defined(QIPRNG_NEON)
    size_t simd_count = count / 2;
    size_t remainder = count % 2;
    
    for (size_t i = 0; i < simd_count; ++i) {
        uint64x2_t v1 = vld1q_u64(reinterpret_cast<const uint64_t*>(src1 + i * 2));
        uint64x2_t v2 = vld1q_u64(reinterpret_cast<const uint64_t*>(src2 + i * 2));
        uint64x2_t result = veorq_u64(v1, v2);
        vst1q_u64(reinterpret_cast<uint64_t*>(dest + i * 2), result);
    }
    
    // Handle remainder
    for (size_t i = simd_count * 2; i < count; ++i) {
        dest[i] = bit_ops::xor_doubles_as_uint64(src1[i], src2[i]);
    }
    
#else
    // Scalar fallback
    bit_ops::xor_doubles_batch(dest, src1, src2, count);
#endif
}

// Vectorized weighted averaging for double arrays
inline void weighted_average_batch(double* dest, const double* src1, const double* src2, 
                                  double weight1, double weight2, size_t count) {
#ifdef QIPRNG_AVX2
    __m256d w1 = _mm256_set1_pd(weight1);
    __m256d w2 = _mm256_set1_pd(weight2);
    
    size_t simd_count = count / 4;
    for (size_t i = 0; i < simd_count; ++i) {
        __m256d v1 = _mm256_loadu_pd(src1 + i * 4);
        __m256d v2 = _mm256_loadu_pd(src2 + i * 4);
        __m256d result = _mm256_add_pd(_mm256_mul_pd(v1, w1), _mm256_mul_pd(v2, w2));
        _mm256_storeu_pd(dest + i * 4, result);
    }
    
    // Handle remainder
    for (size_t i = simd_count * 4; i < count; ++i) {
        dest[i] = src1[i] * weight1 + src2[i] * weight2;
    }
    
#elif defined(QIPRNG_SSE2)
    __m128d w1 = _mm_set1_pd(weight1);
    __m128d w2 = _mm_set1_pd(weight2);
    
    size_t simd_count = count / 2;
    for (size_t i = 0; i < simd_count; ++i) {
        __m128d v1 = _mm_loadu_pd(src1 + i * 2);
        __m128d v2 = _mm_loadu_pd(src2 + i * 2);
        __m128d result = _mm_add_pd(_mm_mul_pd(v1, w1), _mm_mul_pd(v2, w2));
        _mm_storeu_pd(dest + i * 2, result);
    }
    
    // Handle remainder
    for (size_t i = simd_count * 2; i < count; ++i) {
        dest[i] = src1[i] * weight1 + src2[i] * weight2;
    }
    
#elif defined(QIPRNG_NEON)
    float64x2_t w1 = vdupq_n_f64(weight1);
    float64x2_t w2 = vdupq_n_f64(weight2);
    
    size_t simd_count = count / 2;
    for (size_t i = 0; i < simd_count; ++i) {
        float64x2_t v1 = vld1q_f64(src1 + i * 2);
        float64x2_t v2 = vld1q_f64(src2 + i * 2);
        float64x2_t result = vaddq_f64(vmulq_f64(v1, w1), vmulq_f64(v2, w2));
        vst1q_f64(dest + i * 2, result);
    }
    
    // Handle remainder
    for (size_t i = simd_count * 2; i < count; ++i) {
        dest[i] = src1[i] * weight1 + src2[i] * weight2;
    }
    
#else
    // Scalar fallback
    for (size_t i = 0; i < count; ++i) {
        dest[i] = src1[i] * weight1 + src2[i] * weight2;
    }
#endif
}

// Vectorized normalization to [0,1)
inline void normalize_batch(double* data, size_t count) {
#ifdef QIPRNG_AVX2
    __m256d one = _mm256_set1_pd(1.0);
    
    size_t simd_count = count / 4;
    for (size_t i = 0; i < simd_count; ++i) {
        __m256d v = _mm256_loadu_pd(data + i * 4);
        // Ensure values are in [0,1) by taking fractional part
        v = _mm256_sub_pd(v, _mm256_floor_pd(v));
        // Handle negative values
        __m256d mask = _mm256_cmp_pd(v, _mm256_setzero_pd(), _CMP_LT_OQ);
        v = _mm256_add_pd(v, _mm256_and_pd(mask, one));
        _mm256_storeu_pd(data + i * 4, v);
    }
    
    // Handle remainder
    for (size_t i = simd_count * 4; i < count; ++i) {
        double v = data[i];
        v = v - std::floor(v);  // Fractional part
        if (v < 0.0) v += 1.0;
        data[i] = v;
    }
    
#elif defined(QIPRNG_SSE2)
    __m128d one = _mm_set1_pd(1.0);
    __m128d zero = _mm_setzero_pd();
    
    size_t simd_count = count / 2;
    for (size_t i = 0; i < simd_count; ++i) {
        __m128d v = _mm_loadu_pd(data + i * 2);
        // Ensure values are in [0,1) - simplified for SSE2
        // Just clamp to [0,1) range
        v = _mm_max_pd(v, zero);
        v = _mm_min_pd(v, one);
        _mm_storeu_pd(data + i * 2, v);
    }
    
    // Handle remainder
    for (size_t i = simd_count * 2; i < count; ++i) {
        double v = data[i];
        v = v - std::floor(v);  // Fractional part
        if (v < 0.0) v += 1.0;
        data[i] = v;
    }
    
#elif defined(QIPRNG_NEON)
    float64x2_t one = vdupq_n_f64(1.0);
    float64x2_t zero = vdupq_n_f64(0.0);
    
    size_t simd_count = count / 2;
    for (size_t i = 0; i < simd_count; ++i) {
        float64x2_t v = vld1q_f64(data + i * 2);
        // Clamp to [0,1) range
        v = vmaxq_f64(v, zero);
        v = vminq_f64(v, one);
        vst1q_f64(data + i * 2, v);
    }
    
    // Handle remainder
    for (size_t i = simd_count * 2; i < count; ++i) {
        double v = data[i];
        v = v - std::floor(v);  // Fractional part
        if (v < 0.0) v += 1.0;
        data[i] = v;
    }
    
#else
    // Scalar fallback
    for (size_t i = 0; i < count; ++i) {
        double v = data[i];
        v = v - std::floor(v);  // Fractional part
        if (v < 0.0) v += 1.0;
        data[i] = v;
    }
#endif
}

// Get optimal batch size for SIMD operations
inline size_t get_optimal_batch_size() {
    return SIMD_WIDTH * 16;  // Process 16 SIMD vectors at a time
}

} // namespace simd
} // namespace qiprng

#endif // SIMD_OPERATIONS_HPP