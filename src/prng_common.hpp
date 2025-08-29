#ifndef QIPRNG_COMMON_HPP
#define QIPRNG_COMMON_HPP

#include <mpfr.h>
#include <memory>
#include <stdexcept>
#include <vector>
#include <type_traits>
#include <thread>  // For thread_local
#include <atomic>  // For std::atomic
#include <mutex>   // For std::mutex
#include <Rcpp.h>  // For Rcpp::warning
#include <limits>  // For numeric_limits
#include <string>  // For error messages
#include <cmath>   // For sqrt
#include "precision_utils.hpp"  // For high-precision constants and safe conversions

namespace qiprng {

// v0.5.0: Memory pool for MPFR allocations to reduce overhead
template<typename T>
class MemoryPool {
private:
    struct Block {
        T data;
        bool in_use;
        Block() : in_use(false) {}
    };
    
    std::vector<std::unique_ptr<Block>> blocks_;
    std::mutex mutex_;
    size_t max_blocks_;
    
public:
    explicit MemoryPool(size_t max_blocks = 1024) : max_blocks_(max_blocks) {
        blocks_.reserve(max_blocks / 4);  // Pre-allocate some capacity
    }
    
    T* allocate() {
        std::lock_guard<std::mutex> lock(mutex_);
        
        // Find a free block
        for (auto& block : blocks_) {
            if (!block->in_use) {
                block->in_use = true;
                return &block->data;
            }
        }
        
        // Create new block if under limit
        if (blocks_.size() < max_blocks_) {
            blocks_.push_back(std::make_unique<Block>());
            blocks_.back()->in_use = true;
            return &blocks_.back()->data;
        }
        
        // Pool exhausted - fallback to regular allocation
        return nullptr;
    }
    
    void deallocate(T* ptr) {
        if (!ptr) return;
        
        std::lock_guard<std::mutex> lock(mutex_);
        
        for (auto& block : blocks_) {
            if (&block->data == ptr) {
                block->in_use = false;
                return;
            }
        }
    }
    
    size_t size() const {
        std::lock_guard<std::mutex> lock(const_cast<std::mutex&>(mutex_));
        return blocks_.size();
    }
    
    size_t available() const {
        std::lock_guard<std::mutex> lock(const_cast<std::mutex&>(mutex_));
        size_t count = 0;
        for (const auto& block : blocks_) {
            if (!block->in_use) count++;
        }
        return count;
    }
};

// Safe discriminant calculation with overflow protection
// Calculates discriminant = b^2 - 4ac with overflow checking
// Returns true on success, false on overflow
// On overflow, error_msg contains details about where overflow occurred
inline bool safe_calculate_discriminant(long a, long b, long c, 
                                       long long& result, 
                                       std::string& error_msg) {
    // Method 1: Use compiler builtin overflow checking if available (GCC/Clang)
#if defined(__GNUC__) || defined(__clang__)
    long long b_squared;
    long long four_ac;
    
    // Check b * b
    long long b_ll = static_cast<long long>(b);
    if (__builtin_mul_overflow(b_ll, b_ll, &b_squared)) {
        error_msg = "Overflow in b^2 calculation: b=" + std::to_string(b);
        return false;
    }
    
    // Check 4 * a
    long long four_a;
    long long a_ll = static_cast<long long>(a);
    if (__builtin_mul_overflow(4LL, a_ll, &four_a)) {
        error_msg = "Overflow in 4*a calculation: a=" + std::to_string(a);
        return false;
    }
    
    // Check (4*a) * c
    long long c_ll = static_cast<long long>(c);
    if (__builtin_mul_overflow(four_a, c_ll, &four_ac)) {
        error_msg = "Overflow in 4*a*c calculation: 4*a=" + std::to_string(four_a) + 
                   ", c=" + std::to_string(c);
        return false;
    }
    
    // Check b_squared - four_ac
    if (__builtin_sub_overflow(b_squared, four_ac, &result)) {
        error_msg = "Overflow in b^2 - 4ac calculation: b^2=" + std::to_string(b_squared) + 
                   ", 4ac=" + std::to_string(four_ac);
        return false;
    }
    
    return true;
    
// Method 2: Use __int128 if available (most 64-bit systems)
#elif defined(__SIZEOF_INT128__)
    __int128 b_128 = static_cast<__int128>(b);
    __int128 a_128 = static_cast<__int128>(a);
    __int128 c_128 = static_cast<__int128>(c);
    __int128 disc_128 = b_128 * b_128 - 4 * a_128 * c_128;
    
    // Check if result fits in long long
    if (disc_128 > std::numeric_limits<long long>::max() || 
        disc_128 < std::numeric_limits<long long>::min()) {
        error_msg = "Discriminant exceeds long long range for a=" + std::to_string(a) + 
                   ", b=" + std::to_string(b) + ", c=" + std::to_string(c);
        return false;
    }
    
    result = static_cast<long long>(disc_128);
    return true;
    
// Method 3: Manual overflow checking (portable fallback)
#else
    // Convert to long long for intermediate calculations
    long long a_ll = static_cast<long long>(a);
    long long b_ll = static_cast<long long>(b);
    long long c_ll = static_cast<long long>(c);
    
    const long long LLONG_MAX = std::numeric_limits<long long>::max();
    const long long LLONG_MIN = std::numeric_limits<long long>::min();
    
    // Check b * b
    if (b_ll != 0) {
        if (std::abs(b_ll) > std::sqrt(static_cast<double>(LLONG_MAX))) {
            error_msg = "Overflow risk in b^2: b=" + std::to_string(b);
            return false;
        }
    }
    long long b_squared = b_ll * b_ll;
    
    // Check 4 * a * c more carefully
    if (a_ll != 0 && c_ll != 0) {
        // First check if 4*a would overflow
        if (std::abs(a_ll) > LLONG_MAX / 4) {
            error_msg = "Overflow in 4*a: a=" + std::to_string(a);
            return false;
        }
        long long four_a = 4LL * a_ll;
        
        // Then check if (4*a)*c would overflow
        if (std::abs(four_a) > 0 && std::abs(c_ll) > LLONG_MAX / std::abs(four_a)) {
            error_msg = "Overflow in 4*a*c: 4*a=" + std::to_string(four_a) + 
                       ", c=" + std::to_string(c);
            return false;
        }
        long long four_ac = four_a * c_ll;
        
        // Check subtraction overflow
        if ((four_ac > 0 && b_squared < LLONG_MIN + four_ac) ||
            (four_ac < 0 && b_squared > LLONG_MAX + four_ac)) {
            error_msg = "Overflow in b^2 - 4ac: b^2=" + std::to_string(b_squared) + 
                       ", 4ac=" + std::to_string(four_ac);
            return false;
        }
        
        result = b_squared - four_ac;
    } else {
        result = b_squared;
    }
    
    return true;
#endif
}

// Global variable to control MPFR warning output (atomic for thread-safety)
extern std::atomic<bool> suppress_mpfr_warnings;

// Thread-local counter to limit warnings
thread_local extern int mpfr_warning_count;

// Helper function to check MPFR operation result and conditionally issue warning
inline bool check_mpfr_result(int ret, const char* operation_name, bool force_warning = false) {
    if (ret != 0 && (!suppress_mpfr_warnings.load() || force_warning)) {
        if (mpfr_warning_count < 5) {
            Rcpp::warning("MPFR %s operation resulted in inexact value", operation_name);
            mpfr_warning_count++;
        }
        return false;
    }
    return true;
}

// Single source of truth for PRNG defaults
struct PRNGDefaults {
    // Normal distribution generation method
    enum NormalMethod {
        BOX_MULLER,
        ZIGGURAT
    };
    
    static constexpr long  aa = 2;
    static constexpr long b = 5;
    static constexpr long c = -2;
    static constexpr size_t mpfr_precision = 53;  // IEEE double precision
    static constexpr size_t buffer_size = 1024;
    static constexpr double normal_mean = 0.0;
    static constexpr double normal_sd = 1.0;
    static constexpr double range_min = 0.0;
    static constexpr double range_max = 1.0;
    static constexpr double exponential_lambda = 1.0;
    static constexpr bool use_csv_discriminants = false;  // default to not using CSV discriminants
    static constexpr NormalMethod normal_method = ZIGGURAT; // default to faster algorithm
};

// v0.5.0: Global memory pool for MPFR allocations
// Singleton pattern for shared pool access
class MPFRMemoryPool {
private:
    static MPFRMemoryPool& instance() {
        static MPFRMemoryPool pool;
        return pool;
    }
    
    MemoryPool<mpfr_t> pool_;
    
    MPFRMemoryPool() : pool_(1024) {}  // Default 1024 MPFR objects
    
public:
    static mpfr_t* allocate() {
        return instance().pool_.allocate();
    }
    
    static void deallocate(mpfr_t* ptr) {
        instance().pool_.deallocate(ptr);
    }
    
    static size_t available() {
        return instance().pool_.available();
    }
};

// RAII wrapper for MPFR values with improved error handling and memory safety
// Optimized for v0.5.0 with enhanced performance features
class MPFRWrapper {
    mpfr_t value;
    bool initialized_;
    mpfr_prec_t cached_precision_;  // Cache precision to avoid repeated calls
    bool using_pool_;  // Track if allocated from pool

public:
    // Default precision for high-accuracy computations (256 bits as per v0.5.0 spec)
    static constexpr mpfr_prec_t DEFAULT_HIGH_PRECISION = 256;
    
    // v0.5.0: Factory methods for common use cases
    static std::unique_ptr<MPFRWrapper> create_high_precision() {
        return std::make_unique<MPFRWrapper>(DEFAULT_HIGH_PRECISION);
    }
    
    static std::unique_ptr<MPFRWrapper> create_standard() {
        return std::make_unique<MPFRWrapper>(53);  // IEEE double precision
    }
    
    static std::unique_ptr<MPFRWrapper> create_extended() {
        return std::make_unique<MPFRWrapper>(113);  // IEEE quad precision
    }
    
    // Optimized constructor with inline hint for better performance
    inline explicit MPFRWrapper(mpfr_prec_t prec = DEFAULT_HIGH_PRECISION) 
        : initialized_(false), cached_precision_(prec) {
        // Check for valid precision
        if (prec < MPFR_PREC_MIN || prec > MPFR_PREC_MAX) {
            throw std::invalid_argument("MPFRWrapper: Invalid precision value");
        }
        
        // Initialize with error handling
        try {
            mpfr_init2(value, prec);
            initialized_ = true;
            
            // Initialize to +0 and check for errors
            mpfr_set_zero(value, 1);  // Initialize to +0
            // No need to check return value as mpfr_set_zero returns void
        } catch (const std::bad_alloc& e) {
            // Handle memory allocation failures
            if (initialized_) {
                mpfr_clear(value);
                initialized_ = false;
            }
            throw std::runtime_error("MPFRWrapper: Memory allocation failed during initialization");
        } catch (const std::exception& e) {
            // Handle other initialization errors
            if (initialized_) {
                mpfr_clear(value);
                initialized_ = false;
            }
            throw;
        }
    }
    
    // Factory method for creating MPFRWrapper with specific initial value
    static MPFRWrapper from_long(long val, mpfr_prec_t prec = DEFAULT_HIGH_PRECISION) {
        MPFRWrapper result(prec);
        mpfr_set_si(*result.get(), val, MPFR_RNDN);
        return result;
    }
    
    // Factory method for creating MPFRWrapper from double
    static MPFRWrapper from_double(double val, mpfr_prec_t prec = DEFAULT_HIGH_PRECISION) {
        MPFRWrapper result(prec);
        mpfr_set_d(*result.get(), val, MPFR_RNDN);
        return result;
    }

    // Prevent double-free by ensuring we clear only once and track state
    ~MPFRWrapper() {
        if (initialized_) {
            mpfr_clear(value);
            initialized_ = false;
        }
    }

    // Get raw mpfr_t pointer for MPFR functions with validation
    // Inline for performance - frequently called in hot paths
    inline mpfr_t* get() { 
        if (!initialized_) [[unlikely]] {
            throw std::runtime_error("MPFRWrapper: Attempting to use uninitialized value");
        }
        return &value; 
    }
    
    inline const mpfr_t* get() const { 
        if (!initialized_) [[unlikely]] {
            throw std::runtime_error("MPFRWrapper: Attempting to use uninitialized value");
        }
        return &value; 
    }
    
    // Fast unchecked access for performance-critical sections where validity is guaranteed
    inline mpfr_t* get_unsafe() noexcept { return &value; }
    inline const mpfr_t* get_unsafe() const noexcept { return &value; }

    // Check if the wrapper is in a valid state
    inline bool is_valid() const noexcept {
        return initialized_;
    }

    // Non-copyable to prevent multiple ownership issues
    MPFRWrapper(const MPFRWrapper&) = delete;
    MPFRWrapper& operator=(const MPFRWrapper&) = delete;

    // Movable with proper ownership transfer - optimized for v0.5.0
    // The key issue we're fixing: ensuring proper ownership transfer and preventing double frees
    MPFRWrapper(MPFRWrapper&& other) noexcept 
        : initialized_(false), cached_precision_(other.cached_precision_) {
        if (other.initialized_) {
            try {
                // Initialize new instance with cached precision (faster than mpfr_get_prec)
                mpfr_init2(value, other.cached_precision_);
                // initialized_ is false, will be set true only on successful copy
                
                // Copy value from source
                int ret = mpfr_set(value, other.value, MPFR_RNDN);
                if (ret != 0 || mpfr_nan_p(value) || mpfr_inf_p(value)) {
                    mpfr_clear(value); // Clean up 'this' if copy failed or resulted in non-finite
                    initialized_ = false;
                } else {
                    initialized_ = true; // Successfully copied
                }
                
                // CRITICAL CHANGE: Clear source's MPFR data first
                mpfr_clear(other.value);
                // THEN mark source as not initialized (its MPFR data is gone)
                other.initialized_ = false;

            } catch (const std::exception& e) {
                // Clean up 'this' on error if it was partially initialized
                if (initialized_) { // This flag might be true if mpfr_init2 succeeded but mpfr_set failed then threw
                    mpfr_clear(value);
                    initialized_ = false;
                }
                // Attempt to clean up 'other' as well, as it's noexcept
                if (other.initialized_) { // Check flag before assuming other.value is clearable
                    try { mpfr_clear(other.value); } catch(...) { /* ignore */ }
                    other.initialized_ = false;
                }
            } catch (...) { // Catch-all for noexcept
                if (initialized_) { try { mpfr_clear(value); } catch(...) { /* ignore */ } initialized_ = false; }
                if (other.initialized_) { try { mpfr_clear(other.value); } catch(...) { /* ignore */ } other.initialized_ = false; }
            }
        }
    }

    // Move assignment with proper ownership transfer - optimized for v0.5.0
    MPFRWrapper& operator=(MPFRWrapper&& other) noexcept {
        if (this != &other) {
            // First clean up our own resources if initialized
            if (initialized_) {
                mpfr_clear(value);
                initialized_ = false;
            }
            
            // Copy cached precision
            cached_precision_ = other.cached_precision_;
            
            // Only proceed if source is initialized
            if (other.initialized_) {
                try {
                    // Initialize with cached precision (faster)
                    mpfr_init2(value, cached_precision_);
                    // initialized_ is false, will be set true only on successful copy
                    
                    // Copy value from source
                    int ret = mpfr_set(value, other.value, MPFR_RNDN);
                    if (ret != 0 || mpfr_nan_p(value) || mpfr_inf_p(value)) {
                        mpfr_clear(value); // Clean up 'this' if copy failed or resulted in non-finite
                        initialized_ = false;
                    } else {
                        initialized_ = true; // Successfully copied
                    }
                    
                    // CRITICAL CHANGE: Clear source's MPFR data first
                    mpfr_clear(other.value);
                    // THEN mark source as not initialized
                    other.initialized_ = false;

                } catch (const std::exception& e) {
                    // Clean up 'this' on error if it was partially initialized
                    if (initialized_) { // This flag might be true if mpfr_init2 succeeded but mpfr_set failed then threw
                        mpfr_clear(value);
                        initialized_ = false;
                    }
                    // Attempt to clean up 'other' as well
                    if (other.initialized_) { // Check flag
                        try { mpfr_clear(other.value); } catch(...) { /* ignore */ }
                        other.initialized_ = false;
                    }
                } catch (...) { // Catch-all for noexcept
                    if (initialized_) { try { mpfr_clear(value); } catch(...) { /* ignore */ } initialized_ = false; }
                    if (other.initialized_) { try { mpfr_clear(other.value); } catch(...) { /* ignore */ } other.initialized_ = false; }
                }
            }
        }
        return *this;
    }
    
    // Get precision of the MPFR value - optimized with cached value
    inline mpfr_prec_t get_precision() const noexcept {
        return cached_precision_;
    }
    
    // Set precision with validation - updates cache
    void set_precision(mpfr_prec_t prec) {
        if (!initialized_) [[unlikely]] {
            throw std::runtime_error("MPFRWrapper: Attempting to set precision of uninitialized value");
        }
        if (prec < MPFR_PREC_MIN || prec > MPFR_PREC_MAX) [[unlikely]] {
            throw std::invalid_argument("MPFRWrapper: Invalid precision value");
        }
        mpfr_set_prec(value, prec);
        cached_precision_ = prec;  // Update cache
    }
    
    // Fast precision change with reinitialization
    void resize_precision(mpfr_prec_t new_prec) {
        if (!initialized_) [[unlikely]] {
            throw std::runtime_error("MPFRWrapper: Attempting to resize uninitialized value");
        }
        if (new_prec == cached_precision_) {
            return;  // No change needed
        }
        if (new_prec < MPFR_PREC_MIN || new_prec > MPFR_PREC_MAX) [[unlikely]] {
            throw std::invalid_argument("MPFRWrapper: Invalid precision value");
        }
        
        // Preserve value while changing precision
        mpfr_prec_round(value, new_prec, MPFR_RNDN);
        cached_precision_ = new_prec;
    }
    
    // Check if value is NaN
    bool is_nan() const {
        if (!initialized_) {
            return true; // Uninitialized considered as NaN for safety
        }
        return mpfr_nan_p(value) != 0;
    }
    
    // Check if value is infinity
    bool is_inf() const {
        if (!initialized_) {
            return false;
        }
        return mpfr_inf_p(value) != 0;
    }
    
    // Safe conversion to double with validation
    inline double to_double() const {
        if (!initialized_) [[unlikely]] {
            throw std::runtime_error("MPFRWrapper: Attempting to convert uninitialized value to double");
        }
        if (is_nan()) [[unlikely]] {
            throw std::runtime_error("MPFRWrapper: Cannot convert NaN to double");
        }
        if (is_inf()) [[unlikely]] {
            throw std::runtime_error("MPFRWrapper: Cannot convert infinity to double");
        }
        // Use safe conversion with extended precision intermediates
        return precision::safe_mpfr_to_double(value, true);
    }
    
    // Precision-aware conversion with tracking
    inline double to_double_extended() const {
        if (!initialized_) [[unlikely]] {
            throw std::runtime_error("MPFRWrapper: Attempting to convert uninitialized value to double");
        }
        if (is_nan()) [[unlikely]] {
            throw std::runtime_error("MPFRWrapper: Cannot convert NaN to double");
        }
        if (is_inf()) [[unlikely]] {
            throw std::runtime_error("MPFRWrapper: Cannot convert infinity to double");
        }
        return precision::safe_mpfr_to_double(value, true);
    }
    
    // Optimized arithmetic operations for v0.5.0
    // These avoid creating temporaries and use in-place operations where possible
    
    // Add another MPFRWrapper to this one (in-place)
    inline void add_inplace(const MPFRWrapper& other) {
        mpfr_add(*get(), *get(), *other.get(), MPFR_RNDN);
    }
    
    // Subtract another MPFRWrapper from this one (in-place)
    inline void sub_inplace(const MPFRWrapper& other) {
        mpfr_sub(*get(), *get(), *other.get(), MPFR_RNDN);
    }
    
    // Multiply by another MPFRWrapper (in-place)
    inline void mul_inplace(const MPFRWrapper& other) {
        mpfr_mul(*get(), *get(), *other.get(), MPFR_RNDN);
    }
    
    // Divide by another MPFRWrapper (in-place)
    inline void div_inplace(const MPFRWrapper& other) {
        mpfr_div(*get(), *get(), *other.get(), MPFR_RNDN);
    }
    
    // Fast operations with long integers
    inline void add_si(long val) {
        mpfr_add_si(*get(), *get(), val, MPFR_RNDN);
    }
    
    inline void mul_si(long val) {
        mpfr_mul_si(*get(), *get(), val, MPFR_RNDN);
    }
    
    // Fast comparison operations
    inline int compare(const MPFRWrapper& other) const {
        return mpfr_cmp(*get(), *other.get());
    }
    
    inline int compare_si(long val) const {
        return mpfr_cmp_si(*get(), val);
    }
    
    // Efficient swap operation
    inline void swap(MPFRWrapper& other) noexcept {
        if (initialized_ && other.initialized_) {
            mpfr_swap(value, other.value);
            std::swap(cached_precision_, other.cached_precision_);
        }
    }

};

// Cache alignment constants
#ifndef CACHE_LINE_SIZE
#if defined(_MSC_VER)
// Windows
#define CACHE_LINE_SIZE 64
#elif defined(__APPLE__)
// macOS
#define CACHE_LINE_SIZE 64
#elif defined(__linux__)
// Linux
#define CACHE_LINE_SIZE 64
#else
// Default for unknown platforms
#define CACHE_LINE_SIZE 64
#endif
#endif

// Aligned allocation helper
template <typename T>
class AlignedAllocator {
public:
    using value_type = T;
    using pointer = T*;
    using const_pointer = const T*;
    using reference = T&;
    using const_reference = const T&;
    using size_type = std::size_t;
    using difference_type = std::ptrdiff_t;

    template <typename U>
    struct rebind {
        using other = AlignedAllocator<U>;
    };

    AlignedAllocator() noexcept {}
    AlignedAllocator(const AlignedAllocator&) noexcept = default;
    template <typename U>
    AlignedAllocator(const AlignedAllocator<U>&) noexcept {}

    pointer allocate(size_type n) {
        void* p = nullptr;
#if defined(_MSC_VER)
        p = _aligned_malloc(n * sizeof(T), CACHE_LINE_SIZE);
        if (!p) throw std::bad_alloc();
#else
        if (posix_memalign(&p, CACHE_LINE_SIZE, n * sizeof(T)) != 0) {
            throw std::bad_alloc();
        }
#endif
        return static_cast<pointer>(p);
    }

    void deallocate(pointer p, size_type) noexcept {
        if (p) {
#if defined(_MSC_VER)
            _aligned_free(p);
#else
            free(p);
#endif
        }
    }

    template <typename U, typename... Args>
    void construct(U* p, Args&&... args) {
        ::new (static_cast<void*>(p)) U(std::forward<Args>(args)...);
    }

    template <typename U>
    void destroy(U* p) noexcept {
        p->~U();
    }

    size_t max_size() const noexcept {
        return std::numeric_limits<size_t>::max() / sizeof(T);
    }
};

template <typename T, typename U>
bool operator==(const AlignedAllocator<T>&, const AlignedAllocator<U>&) noexcept {
    return true;
}

template <typename T, typename U>
bool operator!=(const AlignedAllocator<T>&, const AlignedAllocator<U>&) noexcept {
    return false;
}

// Type-safe buffer mixing helper with cache alignment
template<typename T>
class SecureBuffer {
    static_assert(std::is_trivially_copyable<T>::value, 
                  "Type must be trivially copyable");

    std::vector<T, AlignedAllocator<T>> data_;

public:
    explicit SecureBuffer(size_t size) : data_(size) {
        // Initialize with zeros for security
        std::fill(data_.begin(), data_.end(), T(0));
    }
    
    void clear() {
        if (!data_.empty()) {
            volatile T* ptr = data_.data();
            for (size_t i = 0; i < data_.size(); ++i) {
                ptr[i] = T(0);
            }
        }
        data_.clear();
    }

    ~SecureBuffer() {
        clear();
    }

    T* data() { return data_.data(); }
    const T* data() const { return data_.data(); }
    size_t size() const { return data_.size(); }
    
    void resize(size_t new_size) {
        clear();
        data_.resize(new_size);
        // Initialize with zeros for security
        std::fill(data_.begin(), data_.end(), T(0));
    }

    T& operator[](size_t i) { return data_[i]; }
    const T& operator[](size_t i) const { return data_[i]; }

    // Iterator support
    typename std::vector<T, AlignedAllocator<T>>::iterator begin() { return data_.begin(); }
    typename std::vector<T, AlignedAllocator<T>>::iterator end() { return data_.end(); }
    typename std::vector<T, AlignedAllocator<T>>::const_iterator begin() const { return data_.begin(); }
    typename std::vector<T, AlignedAllocator<T>>::const_iterator end() const { return data_.end(); }
};

} // namespace qiprng

#endif // QIPRNG_COMMON_HPP
