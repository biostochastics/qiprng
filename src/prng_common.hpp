#ifndef QIPRNG_COMMON_HPP
#define QIPRNG_COMMON_HPP

#include <mpfr.h>
#include <memory>
#include <stdexcept>
#include <vector>
#include <type_traits>
#include <thread>  // For thread_local
#include <atomic>  // For std::atomic
#include <Rcpp.h>  // For Rcpp::warning

namespace qiprng {

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

// RAII wrapper for MPFR values with improved error handling and memory safety
class MPFRWrapper {
    mpfr_t value;
    bool initialized_;

public:
    explicit MPFRWrapper(mpfr_prec_t prec) : initialized_(false) {
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

    // Prevent double-free by ensuring we clear only once and track state
    ~MPFRWrapper() {
        if (initialized_) {
            mpfr_clear(value);
            initialized_ = false;
        }
    }

    // Get raw mpfr_t pointer for MPFR functions with validation
    mpfr_t* get() { 
        if (!initialized_) {
            throw std::runtime_error("MPFRWrapper: Attempting to use uninitialized value");
        }
        return &value; 
    }
    
    const mpfr_t* get() const { 
        if (!initialized_) {
            throw std::runtime_error("MPFRWrapper: Attempting to use uninitialized value");
        }
        return &value; 
    }

    // Check if the wrapper is in a valid state
    bool is_valid() const {
        return initialized_;
    }

    // Non-copyable to prevent multiple ownership issues
    MPFRWrapper(const MPFRWrapper&) = delete;
    MPFRWrapper& operator=(const MPFRWrapper&) = delete;

    // Movable with proper ownership transfer
    // The key issue we're fixing: ensuring proper ownership transfer and preventing double frees
    MPFRWrapper(MPFRWrapper&& other) noexcept : initialized_(false) {
        if (other.initialized_) {
            try {
                // Initialize new instance
                mpfr_init2(value, mpfr_get_prec(other.value));
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

    // Move assignment with proper ownership transfer
    MPFRWrapper& operator=(MPFRWrapper&& other) noexcept {
        if (this != &other) {
            // First clean up our own resources if initialized
            if (initialized_) {
                mpfr_clear(value);
                initialized_ = false;
            }
            
            // Only proceed if source is initialized
            if (other.initialized_) {
                try {
                    // Initialize with source precision
                    mpfr_init2(value, mpfr_get_prec(other.value));
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
    
    // Get precision of the MPFR value
    mpfr_prec_t get_precision() const {
        if (!initialized_) {
            throw std::runtime_error("MPFRWrapper: Attempting to get precision of uninitialized value");
        }
        return mpfr_get_prec(value);
    }
    
    // Set precision with validation
    void set_precision(mpfr_prec_t prec) {
        if (!initialized_) {
            throw std::runtime_error("MPFRWrapper: Attempting to set precision of uninitialized value");
        }
        if (prec < MPFR_PREC_MIN || prec > MPFR_PREC_MAX) {
            throw std::invalid_argument("MPFRWrapper: Invalid precision value");
        }
        mpfr_set_prec(value, prec);
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
    double to_double() const {
        if (!initialized_) {
            throw std::runtime_error("MPFRWrapper: Attempting to convert uninitialized value to double");
        }
        if (is_nan()) {
            throw std::runtime_error("MPFRWrapper: Cannot convert NaN to double");
        }
        if (is_inf()) {
            throw std::runtime_error("MPFRWrapper: Cannot convert infinity to double");
        }
        return mpfr_get_d(value, MPFR_RNDN);
    }

};

// Cache alignment constants
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
