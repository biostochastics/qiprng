# qiprng 0.5.0

## Major Features

### Extended Statistical Distributions
* Added Levy stable (alpha-stable) distribution using Chambers-Mallows-Stuck algorithm
* Added Pareto distribution for heavy-tailed modeling  
* Added Cauchy distribution with location and scale parameters
* Added multivariate normal distribution with full covariance support (requires Eigen3)
* Added Gaussian copula for modeling complex dependencies between marginals

### Performance Enhancements
* Integrated OpenMP for parallel random number generation
* Implemented work-stealing queue for dynamic load balancing
* Added SIMD vectorization (AVX2 on x86_64, NEON on ARM64)
* Optimized buffer management for reduced memory allocations

### Mathematical Improvements
* Enhanced CFE jump-ahead with O(log n) complexity using MPFR
* Improved mixing strategies (XOR, averaging, modular, cascade)
* Extended precision support for discriminant calculations

## Technical Changes
* Integrated Eigen3 library for matrix operations (optional)
* Added conditional compilation for platform-specific optimizations
* Enhanced thread safety with proper mutex and atomic operations
* Improved build system with automatic library detection

## Bug Fixes
* Fixed thread-local storage initialization issues
* Resolved race conditions in multi-threaded contexts
* Corrected namespace issues in extended distributions
* Fixed compilation errors with certain compiler versions

## Documentation
* Added comprehensive examples for all new distributions
* Updated performance benchmarks with parallel scaling results
* Enhanced API documentation with thread safety guarantees
* Added migration guide from v0.4.1

## Acknowledgments
This implementation continues to build on Vincent Granville's foundational work on quadratic irrational pseudo-random number generators.

---

# qiprng 0.4.1

## Bug Fixes
* Critical security and thread safety fixes
* Resolved double initialization issues
* Fixed memory management in cleanup routines

---

# qiprng 0.4.0

## Features
* Added new statistical distributions
* Improved discriminant selection algorithm
* Enhanced test framework with 70+ statistical tests

## Bug Fixes
* Fixed major stability issues
* Resolved compilation warnings
* Improved cross-platform compatibility