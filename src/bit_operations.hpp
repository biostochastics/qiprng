#ifndef QIPRNG_BIT_OPERATIONS_HPP
#define QIPRNG_BIT_OPERATIONS_HPP

#include <cstring>
#include <cstdint>
#include <type_traits>

// Check for C++20 support
#if __cplusplus >= 202002L
#include <bit>
#define QIPRNG_HAS_BIT_CAST 1
#else
#define QIPRNG_HAS_BIT_CAST 0
#endif

namespace qiprng {
namespace bit_ops {

// Safe bit casting that avoids strict aliasing violations
template<typename To, typename From>
inline To safe_bit_cast(const From& from) noexcept {
    static_assert(sizeof(To) == sizeof(From), "Sizes must match");
    static_assert(std::is_trivially_copyable_v<To>, "To must be trivially copyable");
    static_assert(std::is_trivially_copyable_v<From>, "From must be trivially copyable");
    
#if QIPRNG_HAS_BIT_CAST
    return std::bit_cast<To>(from);
#else
    // Use union-based approach for C++17 and earlier
    union BitCaster {
        From from;
        To to;
    };
    BitCaster caster;
    caster.from = from;
    return caster.to;
#endif
}

// Specialized XOR for double values treating them as uint64_t
inline double xor_doubles_as_uint64(double a, double b) noexcept {
    uint64_t ua = safe_bit_cast<uint64_t>(a);
    uint64_t ub = safe_bit_cast<uint64_t>(b);
    uint64_t result = ua ^ ub;
    return safe_bit_cast<double>(result);
}

// Batch XOR with proper aliasing
inline void xor_doubles_batch(double* dest, const double* src1, const double* src2, size_t count) noexcept {
    for (size_t i = 0; i < count; ++i) {
        dest[i] = xor_doubles_as_uint64(src1[i], src2[i]);
    }
}

} // namespace bit_ops
} // namespace qiprng

#endif // QIPRNG_BIT_OPERATIONS_HPP