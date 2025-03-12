#ifndef QIPRNG_COMMON_HPP
#define QIPRNG_COMMON_HPP

#include <mpfr.h>
#include <memory>
#include <stdexcept>
#include <vector>
#include <type_traits>

namespace qiprng {

// Single source of truth for PRNG defaults
struct PRNGDefaults {
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
};

// RAII wrapper for MPFR values
class MPFRWrapper {
    mpfr_t value;

public:
    explicit MPFRWrapper(mpfr_prec_t prec) {
        mpfr_init2(value, prec);
        mpfr_set_zero(value, 1);  // Initialize to +0
    }

    ~MPFRWrapper() {
        mpfr_clear(value);
    }

    // Get raw mpfr_t pointer for MPFR functions
    mpfr_t* get() { return &value; }
    const mpfr_t* get() const { return &value; }

    // Delete copy operations to prevent double-free
    MPFRWrapper(const MPFRWrapper&) = delete;
    MPFRWrapper& operator=(const MPFRWrapper&) = delete;

    // Allow move operations
    MPFRWrapper(MPFRWrapper&& other) noexcept {
        mpfr_init2(value, mpfr_get_prec(other.value));
        mpfr_set(value, other.value, MPFR_RNDN);
        mpfr_set_zero(other.value, 1);  // Invalidate source with +0
    }

    MPFRWrapper& operator=(MPFRWrapper&& other) noexcept {
        if (this != &other) {
            mpfr_set_prec(value, mpfr_get_prec(other.value));
            mpfr_set(value, other.value, MPFR_RNDN);
            mpfr_set_zero(other.value, 1);  // Invalidate source with +0
        }
        return *this;
    }
};

// Type-safe buffer mixing helper
template<typename T>
class SecureBuffer {
    static_assert(std::is_trivially_copyable<T>::value, 
                  "Type must be trivially copyable");

    std::vector<T> data_;

public:
    explicit SecureBuffer(size_t size) : data_(size) {}
    
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
    }

    T& operator[](size_t i) { return data_[i]; }
    const T& operator[](size_t i) const { return data_[i]; }

    // Iterator support
    typename std::vector<T>::iterator begin() { return data_.begin(); }
    typename std::vector<T>::iterator end() { return data_.end(); }
    typename std::vector<T>::const_iterator begin() const { return data_.begin(); }
    typename std::vector<T>::const_iterator end() const { return data_.end(); }
};

} // namespace qiprng

#endif // QIPRNG_COMMON_HPP
