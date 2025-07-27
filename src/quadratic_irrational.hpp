// File: quadratic_irrational.hpp
// --------------------------------------------------------------
#ifndef QIPRNG_QUADRATIC_IRRATIONAL_HPP
#define QIPRNG_QUADRATIC_IRRATIONAL_HPP

#include "prng_common.hpp" // For MPFRWrapper
#include <Rcpp.h>           // For Rcpp::warning, Rcpp::Rcout (used in .cpp)
#include <memory>           // For std::unique_ptr
#include <stdexcept>        // For std::runtime_error, std::invalid_argument
#include <cmath>            // For std::sqrt, __builtin_smull_overflow etc.
#include <random>           // For std::random_device, std::mt19937_64

namespace qiprng {

class QuadraticIrrational {
private:
    long a_, b_, c_;
    std::unique_ptr<MPFRWrapper> value_, root_;
    std::unique_ptr<MPFRWrapper> next_;
    std::unique_ptr<MPFRWrapper> temp_;
    std::unique_ptr<MPFRWrapper> temp2_;

    void step_once();

public:
    QuadraticIrrational(long a, long b, long c, mpfr_prec_t prec);
    ~QuadraticIrrational() = default; // Default destructor is fine with unique_ptr

    double next();
    void skip(uint64_t n);
    void jump_ahead(uint64_t n);
    size_t size() const; // Typically returns 1 for a single QI
    void fill(double* buffer, size_t size);
};

} // namespace qiprng

#endif // QIPRNG_QUADRATIC_IRRATIONAL_HPP