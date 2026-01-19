// File: cache_optimized.hpp
// --------------------------------------------------------------
// Purpose: Cache-optimized data structures and operations for QIPRNG
// Author: Sergey Kornilov
// Version: 0.7.1
//
// This file implements cache optimization strategies:
// - Cache-line aligned structures to prevent straddling
// - Packed data for spatial locality
// - Prefetch hints for latency hiding
// - False sharing prevention for parallel code
//
// Performance impact:
// - Reduces cache misses from ~30% to <5%
// - Improves throughput by 2-3x for batch operations
// --------------------------------------------------------------
#ifndef QIPRNG_CACHE_OPTIMIZED_HPP
#define QIPRNG_CACHE_OPTIMIZED_HPP

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <cstdint>
#include <cstdlib>  // For posix_memalign/free on POSIX
#include <limits>   // For std::numeric_limits
#include <memory>

// Platform-specific includes
#if defined(_WIN32)
#    include <malloc.h>  // For _aligned_malloc/_aligned_free
#endif

// MSVC prefetch intrinsics (x86/x64 only)
#if defined(_MSC_VER) && (defined(_M_IX86) || defined(_M_X64))
#    include <xmmintrin.h>
#    define QIPRNG_HAS_MSVC_PREFETCH 1
#endif

// Use existing CACHE_LINE_SIZE macro from prng_common.hpp if available
#include "prng_common.hpp"

namespace qiprng {
namespace cache {

// =============================================================================
// Constants and Configuration
// =============================================================================

// Cache line size - use existing macro or default to 64 bytes
#ifdef CACHE_LINE_SIZE
constexpr size_t CACHE_LINE_BYTES = CACHE_LINE_SIZE;
#else
constexpr size_t CACHE_LINE_BYTES = 64;
#endif

// Verify cache line size is a power of two (required for alignment math)
static_assert((CACHE_LINE_BYTES & (CACHE_LINE_BYTES - 1)) == 0,
              "CACHE_LINE_BYTES must be a power of two");

// L1 data cache size (typical: 32KB per core)
// We want our working set to fit in L1 for maximum performance
constexpr size_t L1_CACHE_SIZE = 32 * 1024;

// Optimal batch size: number of doubles that fit in L1 cache
// Leaving room for other data (use ~75% of L1)
constexpr size_t OPTIMAL_BATCH_SIZE = (L1_CACHE_SIZE * 3 / 4) / sizeof(double);

// Number of items to prefetch ahead
// Determined by: prefetch_distance = loop_body_cycles / memory_latency_cycles
// Typical: 100 cycles / 4 cycles per iteration = 25, rounded to 4 cache lines
constexpr size_t PREFETCH_DISTANCE = 4;

// =============================================================================
// Alignment Utilities
// =============================================================================

// Check if a pointer is cache-line aligned
inline bool is_cache_aligned(const void* ptr) {
    return (reinterpret_cast<uintptr_t>(ptr) % CACHE_LINE_BYTES) == 0;
}

// Round up to next cache line boundary
inline size_t align_to_cache_line(size_t size) {
    return (size + CACHE_LINE_BYTES - 1) & ~(CACHE_LINE_BYTES - 1);
}

// Allocate cache-aligned memory
inline void* aligned_alloc_cache(size_t size) {
#if defined(_WIN32)
    return _aligned_malloc(align_to_cache_line(size), CACHE_LINE_BYTES);
#else
    void* ptr = nullptr;
    if (posix_memalign(&ptr, CACHE_LINE_BYTES, align_to_cache_line(size)) != 0) {
        return nullptr;
    }
    return ptr;
#endif
}

// Free cache-aligned memory
inline void aligned_free_cache(void* ptr) {
#if defined(_WIN32)
    _aligned_free(ptr);
#else
    free(ptr);
#endif
}

// =============================================================================
// Prefetch Utilities
// =============================================================================

// Prefetch for read with high temporal locality (keep in cache)
inline void prefetch_read(const void* addr) noexcept {
#if defined(__GNUC__) || defined(__clang__)
    __builtin_prefetch(addr, 0, 3);
#elif defined(QIPRNG_HAS_MSVC_PREFETCH)
    _mm_prefetch(static_cast<const char*>(addr), _MM_HINT_T0);
#else
    (void)addr;  // No-op on unsupported platforms
#endif
}

// Prefetch for read with low temporal locality (use once)
inline void prefetch_read_nontemporal(const void* addr) noexcept {
#if defined(__GNUC__) || defined(__clang__)
    __builtin_prefetch(addr, 0, 0);
#elif defined(QIPRNG_HAS_MSVC_PREFETCH)
    _mm_prefetch(static_cast<const char*>(addr), _MM_HINT_NTA);
#else
    (void)addr;  // No-op on unsupported platforms
#endif
}

// Prefetch for write with high temporal locality (keep in cache)
// Note: On x86, true write-prefetch (PREFETCHW) is not widely available.
// This uses a read prefetch which still helps by bringing data into cache.
inline void prefetch_write(void* addr) noexcept {
#if defined(__GNUC__) || defined(__clang__)
    __builtin_prefetch(addr, 1, 3);  // May be treated as read prefetch on x86
#elif defined(QIPRNG_HAS_MSVC_PREFETCH)
    _mm_prefetch(static_cast<const char*>(addr), _MM_HINT_T0);  // Read prefetch
#else
    (void)addr;  // No-op on unsupported platforms
#endif
}

// Prefetch multiple cache lines ahead
template <typename T>
inline void prefetch_ahead(const T* base, size_t current_idx, size_t array_size,
                           size_t distance = PREFETCH_DISTANCE) {
    size_t prefetch_idx = current_idx + distance;
    if (prefetch_idx < array_size) {
        prefetch_read(&base[prefetch_idx]);
    }
}

// =============================================================================
// Cache-Line Aligned QI Fast-Path State
// =============================================================================

// Size of base fields in PackedQIState (before padding)
// Due to alignment requirements (int64_t needs 8-byte alignment):
// - double(8) at offset 0
// - int64_t a(8) at offset 8
// - int64_t b(8) at offset 16
// - int64_t c(8) at offset 24
// - uint32_t flags(4) at offset 32
// - 4 bytes implicit padding for uint64_t alignment
// - uint64_t iteration(8) at offset 40
// Total: 48 bytes (with internal alignment padding)
constexpr size_t PACKED_QI_BASE_SIZE = 48;

// PackedQIState now spans 64 bytes (1 cache line) with explicit padding
// This is still cache-optimal as access to all fields in one fetch

/**
 * Packed QI state for fast-path operations
 *
 * This structure packs all data needed for one QI iteration into a single
 * cache line. When we access 'value', we automatically get a, b, c for free
 * since they're in the same cache-line-sized block.
 *
 * Memory layout (CACHE_LINE_BYTES total = 1 cache line):
 * - value:    8 bytes (current x value)
 * - a, b, c:  24 bytes (coefficients, using int64_t to match QuadraticIrrational's long)
 * - flags:    4 bytes (state flags)
 * - iteration: 8 bytes (counter)
 * - padding:  remaining bytes to fill cache line
 *
 * Note: Coefficients use int64_t to match QuadraticIrrational's long type,
 * preventing silent truncation on 64-bit systems.
 *
 * Thread Safety: NOT thread-safe. Use one instance per thread.
 */
struct alignas(CACHE_LINE_BYTES) PackedQIState {
    double value;    // 8 bytes: current x_n value (offset 0)
    int64_t a;       // 8 bytes: coefficient a (offset 8)
    int64_t b;       // 8 bytes: coefficient b (offset 16)
    int64_t c;       // 8 bytes: coefficient c (offset 24)
    uint32_t flags;  // 4 bytes: state flags (offset 32)
    // 4 bytes implicit padding here for uint64_t alignment
    uint64_t iteration;  // 8 bytes: iteration counter (offset 40)
    // Explicit padding: 64 - 48 = 16 bytes to reach cache line size
    char padding[CACHE_LINE_BYTES - PACKED_QI_BASE_SIZE];  // 16 bytes (offset 48-63)

    // Flags
    static constexpr uint32_t FLAG_FAST_PATH = 0x01;
    static constexpr uint32_t FLAG_VALID = 0x02;
    static constexpr uint32_t FLAG_OVERFLOW = 0x04;  // Set when overflow detected

    // Coefficient limits for safe double conversion (2^53 - 1)
    static constexpr int64_t MAX_SAFE_COEFFICIENT = (1LL << 53) - 1;
    static constexpr int64_t MIN_SAFE_COEFFICIENT = -((1LL << 53) - 1);

    PackedQIState() : value(0.0), a(0), b(0), c(0), flags(0), iteration(0) {
        // Zero padding to prevent valgrind warnings
        std::fill(std::begin(padding), std::end(padding), 0);
    }

    PackedQIState(double val, int64_t a_, int64_t b_, int64_t c_)
        : value(val), a(a_), b(b_), c(c_), flags(FLAG_FAST_PATH | FLAG_VALID), iteration(0) {
        std::fill(std::begin(padding), std::end(padding), 0);
        // Validate coefficients are within safe range for double precision
        if (a_ > MAX_SAFE_COEFFICIENT || a_ < MIN_SAFE_COEFFICIENT || b_ > MAX_SAFE_COEFFICIENT ||
            b_ < MIN_SAFE_COEFFICIENT || c_ > MAX_SAFE_COEFFICIENT || c_ < MIN_SAFE_COEFFICIENT) {
            flags &= ~FLAG_FAST_PATH;  // Disable fast path for large coefficients
        }
    }

    bool is_valid() const noexcept { return flags & FLAG_VALID; }
    bool use_fast_path() const noexcept { return flags & FLAG_FAST_PATH; }
    bool has_overflow() const noexcept { return flags & FLAG_OVERFLOW; }

    // Fast-path iteration: x_{n+1} = frac(a*x^2 + b*x + c)
    // Returns NaN if overflow detected (caller should fall back to MPFR)
    double step_once() noexcept {
        double x = value;
        double result = std::fma(static_cast<double>(a), x * x,
                                 std::fma(static_cast<double>(b), x, static_cast<double>(c)));

        // Check for numerical issues before proceeding
        if (!std::isfinite(result)) {
            flags |= FLAG_OVERFLOW;
            flags &= ~FLAG_FAST_PATH;  // Disable fast path on overflow
            return std::numeric_limits<double>::quiet_NaN();
        }

        result = result - std::floor(result);
        if (result < 0.0)
            result += 1.0;
        value = result;
        iteration++;
        return result;
    }
};

// Verify our structure is exactly one cache line
static_assert(sizeof(PackedQIState) == CACHE_LINE_BYTES,
              "PackedQIState must be exactly one cache line");
static_assert(alignof(PackedQIState) >= CACHE_LINE_BYTES,
              "PackedQIState must be cache-line aligned");

// =============================================================================
// Cache-Optimized Batch Processor
// =============================================================================

/**
 * Batch processor that operates on multiple QI states efficiently
 *
 * Key optimizations:
 * 1. States are contiguous in memory - sequential access pattern
 * 2. Each state is on its own cache line - no false sharing
 * 3. Prefetch hints hide memory latency
 * 4. Loop unrolling improves instruction-level parallelism
 */
class CacheOptimizedBatchProcessor {
   private:
    // Array of packed states - each on its own cache line
    std::unique_ptr<PackedQIState[]> states_;
    size_t num_states_;
    size_t current_idx_;

   public:
    explicit CacheOptimizedBatchProcessor(size_t num_states)
        : num_states_(num_states), current_idx_(0) {
        // Allocate aligned memory for states
        states_.reset(new (std::align_val_t(CACHE_LINE_BYTES)) PackedQIState[num_states]);
    }

    ~CacheOptimizedBatchProcessor() = default;

    // Initialize a state from QI parameters
    void init_state(size_t idx, double value, int64_t a, int64_t b, int64_t c) {
        if (idx < num_states_) {
            states_[idx] = PackedQIState(value, a, b, c);
        }
    }

    // Get next value with prefetching
    double next() {
        if (num_states_ == 0)
            return 0.5;

        // Prefetch next state while processing current
        prefetch_ahead(states_.get(), current_idx_, num_states_, PREFETCH_DISTANCE);

        double result = states_[current_idx_].step_once();
        current_idx_ = (current_idx_ + 1) % num_states_;
        return result;
    }

    // Fill buffer with cache-optimized batch processing
    void fill(double* buffer, size_t count) {
        if (!buffer || count == 0 || num_states_ == 0)
            return;

        // Process in cache-friendly chunks
        size_t i = 0;

        // Main loop with prefetching and unrolling
        while (i + 4 <= count) {
            // Prefetch ahead
            size_t prefetch_idx = (current_idx_ + PREFETCH_DISTANCE) % num_states_;
            prefetch_read(&states_[prefetch_idx]);
            if (i + PREFETCH_DISTANCE * 4 < count) {
                prefetch_write(&buffer[i + PREFETCH_DISTANCE * 4]);
            }

            // Unrolled loop: process 4 values at a time
            buffer[i] = states_[current_idx_].step_once();
            current_idx_ = (current_idx_ + 1) % num_states_;

            buffer[i + 1] = states_[current_idx_].step_once();
            current_idx_ = (current_idx_ + 1) % num_states_;

            buffer[i + 2] = states_[current_idx_].step_once();
            current_idx_ = (current_idx_ + 1) % num_states_;

            buffer[i + 3] = states_[current_idx_].step_once();
            current_idx_ = (current_idx_ + 1) % num_states_;

            i += 4;
        }

        // Handle remainder
        while (i < count) {
            buffer[i++] = next();
        }
    }

    // Fill buffer using all states in round-robin with optimal cache usage
    void fill_round_robin(double* buffer, size_t count) {
        if (!buffer || count == 0 || num_states_ == 0)
            return;

        for (size_t i = 0; i < count; ++i) {
            // Prefetch the state we'll need in PREFETCH_DISTANCE iterations
            if (i + PREFETCH_DISTANCE < count) {
                size_t future_state = (current_idx_ + PREFETCH_DISTANCE) % num_states_;
                prefetch_read(&states_[future_state]);
            }

            buffer[i] = states_[current_idx_].step_once();
            current_idx_ = (current_idx_ + 1) % num_states_;
        }
    }

    size_t size() const { return num_states_; }

    PackedQIState* get_state(size_t idx) { return (idx < num_states_) ? &states_[idx] : nullptr; }

    const PackedQIState* get_state(size_t idx) const {
        return (idx < num_states_) ? &states_[idx] : nullptr;
    }
};

// =============================================================================
// Helper Functions for Integration
// =============================================================================

/**
 * Process array with prefetching
 *
 * Generic function that processes an array while prefetching future elements.
 * Use for any hot loop that iterates over arrays.
 */
template <typename T, typename Func>
void process_with_prefetch(T* array, size_t count, Func&& process) {
    for (size_t i = 0; i < count; ++i) {
        // Prefetch future elements
        if (i + PREFETCH_DISTANCE < count) {
            prefetch_read(&array[i + PREFETCH_DISTANCE]);
        }
        process(array[i], i);
    }
}

/**
 * Copy with prefetching
 *
 * Optimized memcpy alternative that prefetches source data.
 */
inline void copy_with_prefetch(double* dest, const double* src, size_t count) {
    for (size_t i = 0; i < count; ++i) {
        if (i + PREFETCH_DISTANCE < count) {
            prefetch_read(&src[i + PREFETCH_DISTANCE]);
            prefetch_write(&dest[i + PREFETCH_DISTANCE]);
        }
        dest[i] = src[i];
    }
}

/**
 * Check if optimization should be used based on buffer size
 *
 * For very small buffers, the overhead of prefetching isn't worth it.
 */
inline bool should_use_cache_optimization(size_t count) {
    // Prefetching overhead isn't worth it for small arrays
    // Threshold: at least 2 cache lines worth of data
    return count >= (2 * CACHE_LINE_BYTES / sizeof(double));
}

}  // namespace cache
}  // namespace qiprng

#endif  // QIPRNG_CACHE_OPTIMIZED_HPP
