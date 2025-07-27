// File: multi_qi.hpp
// --------------------------------------------------------------
#ifndef QIPRNG_MULTI_QI_HPP
#define QIPRNG_MULTI_QI_HPP

#include "quadratic_irrational.hpp"
#include <vector>
#include <tuple>  // For std::tuple
#include <cstddef> // For size_t
#include <mutex>  // For std::mutex

namespace qiprng {

class MultiQI {
private:
    std::vector<std::unique_ptr<QuadraticIrrational>> qis_;
    size_t idx_;
    std::mutex mutex_; // Added for thread safety

public:
    MultiQI(const std::vector<std::tuple<long, long, long>>& abc_list, int mpfr_prec,
            uint64_t seed = 0, bool has_seed = false);
    ~MultiQI() = default; // Unique_ptr handles cleanup

    double next();
    void skip(uint64_t n);
    void jump_ahead(uint64_t n);
    size_t size() const;
    void fill(double* buffer, size_t fill_size);
    void fill_thread_safe(double* buffer, size_t fill_size); // Added thread-safe version
};

} // namespace qiprng

#endif // QIPRNG_MULTI_QI_HPP 