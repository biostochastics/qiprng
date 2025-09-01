#ifndef QIPRNG_CACHE_ALIGNED_HPP
#define QIPRNG_CACHE_ALIGNED_HPP

#include <atomic>
#include <cstddef>
#include <new>

namespace qiprng {

// Cache line size detection
#ifdef __cpp_lib_hardware_interference_size
constexpr size_t QIPRNG_CACHE_LINE_SIZE = std::hardware_destructive_interference_size;
#else
// Common cache line sizes by architecture
#    if defined(__aarch64__) || defined(__APPLE__)
constexpr size_t QIPRNG_CACHE_LINE_SIZE = 128;  // Apple M1/M2, ARM64
#    elif defined(__powerpc__) || defined(__ppc__)
constexpr size_t QIPRNG_CACHE_LINE_SIZE = 128;  // PowerPC
#    else
constexpr size_t QIPRNG_CACHE_LINE_SIZE = 64;  // x86, x86_64, most others
#    endif
#endif

// Aligned storage for cache line padding
template <typename T>
struct alignas(QIPRNG_CACHE_LINE_SIZE) CacheAligned {
    T value;

    CacheAligned() = default;
    explicit CacheAligned(const T& v) : value(v) {}
    explicit CacheAligned(T&& v) : value(std::move(v)) {}

    // Conversion operators
    operator T&() { return value; }
    operator const T&() const { return value; }

    // Assignment operators
    CacheAligned& operator=(const T& v) {
        value = v;
        return *this;
    }

    CacheAligned& operator=(T&& v) {
        value = std::move(v);
        return *this;
    }

    // Direct access
    T& get() { return value; }
    const T& get() const { return value; }
    T* operator->() { return &value; }
    const T* operator->() const { return &value; }
};

// Specialization for atomic types with proper memory ordering
template <typename T>
struct alignas(QIPRNG_CACHE_LINE_SIZE) CacheAlignedAtomic {
    std::atomic<T> value;

    CacheAlignedAtomic() : value{} {}
    explicit CacheAlignedAtomic(T v) : value(v) {}

    // Implicit conversion to T for compatibility
    operator T() const { return value.load(); }

    // Assignment from T
    CacheAlignedAtomic& operator=(T v) {
        value.store(v);
        return *this;
    }

    // Template comparison operators for any convertible type
    template <typename U>
    friend bool operator==(const CacheAlignedAtomic& lhs, U rhs) {
        return lhs.value.load() == static_cast<T>(rhs);
    }
    template <typename U>
    friend bool operator==(U lhs, const CacheAlignedAtomic& rhs) {
        return static_cast<T>(lhs) == rhs.value.load();
    }
    template <typename U>
    friend bool operator!=(const CacheAlignedAtomic& lhs, U rhs) {
        return lhs.value.load() != static_cast<T>(rhs);
    }
    template <typename U>
    friend bool operator!=(U lhs, const CacheAlignedAtomic& rhs) {
        return static_cast<T>(lhs) != rhs.value.load();
    }

    // Atomic operations with default memory ordering
    T load(std::memory_order order = std::memory_order_seq_cst) const { return value.load(order); }

    void store(T v, std::memory_order order = std::memory_order_seq_cst) { value.store(v, order); }

    T exchange(T v, std::memory_order order = std::memory_order_seq_cst) {
        return value.exchange(v, order);
    }

    bool compare_exchange_weak(T& expected, T desired,
                               std::memory_order success = std::memory_order_seq_cst,
                               std::memory_order failure = std::memory_order_seq_cst) {
        return value.compare_exchange_weak(expected, desired, success, failure);
    }

    bool compare_exchange_strong(T& expected, T desired,
                                 std::memory_order success = std::memory_order_seq_cst,
                                 std::memory_order failure = std::memory_order_seq_cst) {
        return value.compare_exchange_strong(expected, desired, success, failure);
    }

    // Arithmetic operations for integral types
    template <typename U = T>
    typename std::enable_if<std::is_integral<U>::value, U>::type
    fetch_add(U arg, std::memory_order order = std::memory_order_seq_cst) {
        return value.fetch_add(arg, order);
    }

    template <typename U = T>
    typename std::enable_if<std::is_integral<U>::value, U>::type
    fetch_sub(U arg, std::memory_order order = std::memory_order_seq_cst) {
        return value.fetch_sub(arg, order);
    }

    // Increment/decrement operators
    T operator++() { return value.fetch_add(1) + 1; }
    T operator++(int) { return value.fetch_add(1); }
    T operator--() { return value.fetch_sub(1) - 1; }
    T operator--(int) { return value.fetch_sub(1); }
};

// Helper type aliases
template <typename T>
using CacheLinePadded = CacheAligned<T>;

using PaddedAtomicBool = CacheAlignedAtomic<bool>;
using PaddedAtomicInt = CacheAlignedAtomic<int>;
using PaddedAtomicSize = CacheAlignedAtomic<size_t>;
using PaddedAtomicUInt64 = CacheAlignedAtomic<uint64_t>;

// Structure with multiple padded fields for hot/cold data separation
template <typename Hot, typename Cold>
struct alignas(QIPRNG_CACHE_LINE_SIZE) SeparatedData {
    // Hot data on its own cache line
    CacheAligned<Hot> hot;

    // Cold data on separate cache line
    CacheAligned<Cold> cold;

    SeparatedData() = default;
    SeparatedData(const Hot& h, const Cold& c) : hot(h), cold(c) {}
};

}  // namespace qiprng

#endif  // QIPRNG_CACHE_ALIGNED_HPP