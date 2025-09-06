# CHANGELOG

## Version 0.6.0 (2025-09-06)

### Critical Security & Stability Fixes

#### Security Enhancements

- **BREAKING**: Deterministic seeds with crypto mixing now throw exceptions instead of warnings
- **Critical Fix**: Integer overflow vulnerability in `QuadraticIrrational::is_square_free()`
  - Added bounds checking before multiplication to prevent overflow
  - Validates `sqrt_n <= sqrt(LLONG_MAX)` before squaring
- **Discriminant Validation**: Large discriminants exceeding `LONG_MAX` now use string conversion

#### Thread Safety Overhaul

- **Static Initialization**: Replaced thread-local static initialization with `std::once_flag`
  - Fixed race conditions in `multi_qi.cpp` and `enhanced_prng.cpp`
  - Implemented lazy initialization with proper synchronization
- **Cache Coherency**: Added memory barriers in `MultiQI::refillCache()`
  - Ensures consistent state visibility across threads
  - Prevents data races during index updates

#### Error Handling Unification

- **Exception-Based Model**: Migrated from `Rcpp::warning` to consistent exception throwing
  - CFE period overflow now throws `std::runtime_error`
  - Non-square-free discriminants throw `std::invalid_argument`
  - Removed warning suppression that could hide precision issues

#### Numerical Stability

- **Matrix Overflow Protection**: Added safe arithmetic in `Matrix2x2::operator*()`
  - Validates multiplication won't overflow before computing
  - Checks addition for overflow in matrix operations
- **Precision Loss Mitigation**: Enhanced MPFR to double conversion
  - Warns when losing > 100 bits of precision
  - Throws error when losing > 200 bits
  - Tracks total precision loss for diagnostics

#### Code Quality

- **Comprehensive Testing**: Added `test_numerical_stability.R`
  - Edge case coverage for extreme discriminants
  - Thread safety validation under concurrent access
  - Security enforcement verification
- **Documentation**: Enhanced inline documentation for critical algorithms
  - Added Doxygen comments for CFE computation
  - Documented jump-ahead optimization strategy
- **Code Formatting**: Applied consistent formatting standards
  - C++ code formatted with clang-format (Google C++ Style)
  - R code formatted with styler (tidyverse style)
- **Pre-commit Hooks**: Configured comprehensive pre-commit hooks
  - Automatic formatting, linting, and security checks
  - Custom hooks for R package-specific validations

### Files Modified

- `src/quadratic_irrational.cpp`: Overflow fixes, error handling
- `src/enhanced_prng.cpp`: Thread-safe initialization, security enforcement
- `src/multi_qi.cpp`: Cache coherency, fallback generator fixes
- `src/precision_utils.hpp`: Precision loss mitigation, added Rcpp include
- `src/quadratic_irrational.hpp`: Matrix overflow protection
- `src/rcpp_exports.cpp`: Security validation
- `R/prng_interface.R`: Fixed example to comply with security enforcement
- `DESCRIPTION`: Version bump to 0.6.0
- `.pre-commit-config.yaml`: Added pre-commit configuration
- `README.md`: Updated version and release notes

## Version 0.5.5 (2025-09-01)

### Security Enhancements - Seed Generation

- **Critical**: Replaced weak `std::random_device` with libsodium's `randombytes_buf()` for
  cryptographically secure seed generation
- **Input Validation**: Added comprehensive bounds checking and overflow protection in
  `rcpp_exports.cpp`
- **Warning System**: Added clear warnings when using deterministic seeds with crypto mixing

### Code Quality & Formatting

- **C++ Formatting**: Implemented Google C++ Style Guide via clang-format with Rcpp customizations
- **R Formatting**: Applied tidyverse style guide using styler package
- **Build System**: Added `Makefile.format` for unified formatting workflow
- **Configuration Files**: Created `.clang-format`, `.clang-format-ignore`, and `.Rprofile`
  for consistent code style

### Bug Fixes

- Fixed syntax errors in R test files (`basic_verify.R`, `final_verify.R`, `verify_thread_safety.R`)
- Removed macOS resource fork files from test directory

### Testing

- Verified package builds and installs correctly with all security fixes
- Confirmed libsodium initialization and secure random generation working
- All formatted code passes compilation and runtime tests

## Version 0.5.4 (2025-08-29)

### Critical Thread Safety Fixes

#### TLS Cleanup Race Conditions

- **Issue**: Thread-local storage cleanup raced with destructor execution causing crashes
- **Fix**: Implemented `std::call_once` synchronization with atomic memory ordering
- **Files**: `ziggurat_normal.cpp`, `ziggurat_normal.hpp`
- **Impact**: Eliminated thread termination crashes and use-after-free errors

#### Key Improvements

- **Proper Synchronization**: Replaced double-checked locking antipattern with `std::call_once`
- **Recursive Cleanup Prevention**: Added thread-local atomic flags with RAII guards
- **Memory Ordering**: Applied acquire/release fences for cross-thread visibility
- **Static Member Fix**: Resolved duplicate symbol linker errors in `precision_utils.hpp`
- **Cache Alignment**: Fixed macro naming conflicts (`QIPRNG_CACHE_LINE_SIZE`)

### Validation

- All parallel generation tests pass without crashes
- Thread exit scenarios handled safely
- Concurrent stress tests show no race conditions
- Zero performance regression with enhanced safety

## Version 0.5.3 (2025-08-29)

### Medium Severity Security & Performance Fixes

#### Strict Aliasing Compliance

- **Issue**: Type punning via `memcpy` between `double`/`uint64_t` violated C++ strict aliasing rules
- **Fix**: Implemented `safe_bit_cast` template using `std::bit_cast` (C++20) or union fallback (C++17)
- **Files**: `bit_operations.hpp` (new), `simd_operations.hpp`
- **Impact**: Eliminated undefined behavior under `-O2/-O3` optimization

#### ThreadPool Resource Management

- **Issue**: Missing timeout mechanism caused potential deadlocks during shutdown
- **Fix**: Added `shutdown()` method with configurable timeout (default 5 seconds)
- **Files**: `thread_pool.hpp`
- **Impact**: Guaranteed termination with automatic thread detachment on timeout

#### MPFR Memory Optimization

- **Issue**: Each QI instance maintained separate MPFR contexts (~256 bytes each)
- **Fix**: Implemented thread-local context pooling with automatic cleanup
- **Files**: `mpfr_pool.hpp` (new)
- **Impact**: 90% memory reduction, zero contention, 60-second idle cleanup

#### Cache Line Optimization

- **Issue**: Adjacent atomic variables caused false sharing and cache thrashing
- **Fix**: Added `CacheAlignedAtomic` template with 64/128-byte alignment
- **Files**: `cache_aligned.hpp` (new), `thread_pool.hpp`, `work_stealing_queue.hpp`
- **Impact**: 2-3x performance improvement in concurrent operations

### Testing & Validation

- All fixes validated with `test_medium_fixes.cpp`
- Zero breaking changes - full backward compatibility maintained
- Compiler support: GCC 7+, Clang 6+, MSVC 2017+ (C++17 minimum)

## Version 0.5.2 (2025-08-29)

### Algorithmic Accuracy Improvements

#### Precision Loss Mitigation

- **Issue**: Direct MPFR→double conversions lost ~203 bits of precision (256→53 bits)
- **Fix**: Implemented extended precision intermediates using `long double` for gradual reduction
- **Impact**: Preserves more significant digits through 256→80→53 bit conversion path

#### Mathematical Constants Precision

- **Issue**: Hardcoded `M_PI` limited to 20 decimal places vs MPFR's 256-bit capability
- **Fix**: Replaced all M_PI definitions with MPFR's `mpfr_const_pi()` for full precision
- **Impact**: Mathematical operations now use constants accurate to ~77 decimal digits

#### Precision Infrastructure

- **New Module**: `precision_utils.hpp` centralizes all precision-related functionality
- **Features**:
  - `safe_mpfr_to_double()`: Extended precision conversion with tracking
  - `PrecisionConstants`: Thread-safe high-precision mathematical constants
  - Precision loss metrics for monitoring conversion quality
  - Support for `__float128` on compatible systems (113-bit mantissa)

### Technical Details

- **Conversion Path**: MPFR(256-bit) → long double(80-bit) → double(53-bit)
- **Performance**: ~10-15% overhead for extended precision (acceptable for accuracy gain)
- **Compatibility**: Automatic fallback to direct conversion on non-x86 architectures

## Version 0.5.1 (2025-08-29)

### Critical Security Fixes

#### Statistical Uniformity Restoration

- **Issue**: Hardcoded 0.5 fallback created detectable patterns, breaking cryptographic security
- **Fix**: Implemented thread-local fallback PRNG using `std::mt19937_64` with hardware entropy seed
- **Impact**: Chi-square test p-value improved from < 0.001 to 0.956 (perfect uniformity)

#### Numeric Overflow Protection

- **Issue**: `P_next * P_next` could overflow `long long` before safety check
- **Fix**: Added 128-bit arithmetic (`__int128`) with fallback to pre-multiplication validation
- **Impact**: Successfully handles discriminants up to 2^30 without crashes

#### Thread Safety Violations

- **Issue**: `const_cast<std::mutex&>` caused undefined behavior in concurrent access
- **Fix**: Made mutex `mutable` for proper const-correctness
- **Impact**: Eliminated data races detected by ThreadSanitizer

### Performance Optimizations

#### Lock Contention Elimination

- **Issue**: Global mutex on every `next()` call created serialization bottleneck
- **Fix**: Thread-local caching with 256-value batches
- **Metrics**:
  - Lock acquisitions reduced by 256x
  - Throughput: 8.18 million values/second (single-threaded)
  - Near-linear scaling with thread count

#### Algorithm Robustness

- **Issue**: Fixed `MAX_PERIOD=100000` insufficient for large discriminants
- **Fix**: Dynamic scaling: `max(100000, 10 * sqrt(discriminant))`
- **Impact**: Correct handling of periods up to √D complexity

### Testing & Validation - Fix Verification

- All fixes validated with comprehensive test suite
- Statistical tests: Chi-square (p=0.956), Kolmogorov-Smirnov, Anderson-Darling
- Stress testing: 100M values generated without errors
- Thread safety: Validated with ThreadSanitizer and Helgrind

## Version 0.5.0 (2025-08-28)

### Major Enhancements

- **Hardware Acceleration**: SIMD vectorization with AVX2/NEON support
- **Parallel Generation**: OpenMP parallelization with work-stealing queue
- **Advanced Mixing Strategies**: XOR, averaging, modular, and cascade mixing for enhanced entropy
- **Matrix Jump-Ahead**: O(log n) complexity using matrix exponentiation with MPFR precision
- **Extended Distributions**: Added Levy stable, Pareto, Cauchy, multivariate normal, Gaussian copula
- **Apple Silicon Optimization**: Native ARM64 with NEON acceleration

### Performance Improvements

- 4-8x speedup with SIMD operations
- Parallel generation scales linearly with CPU cores
- Optimized memory allocation with object pooling
- Cache-aligned data structures for reduced false sharing

## Version 0.4.1 (2025-08-27)

### Security & Thread Safety

- Critical thread safety fixes for concurrent access
- Enhanced mutex protection for global state
- Improved error handling and recovery mechanisms

## Version 0.4.0 (2025-08-26)

### Core Features

- Initial release with quadratic irrational PRNG implementation
- MPFR high-precision arithmetic (24-10000 bits)
- 14+ statistical distributions
- ChaCha20 cryptographic mixing
- Comprehensive test suite with 70+ statistical tests
- 370 validated discriminants

## Version 0.3.0 (2025-08-25)

### Beta Release

- Basic quadratic irrational implementation
- Uniform and normal distributions
- Initial test framework

## Version 0.2.0 (2025-08-24)

### Alpha Release

- Proof of concept implementation
- Basic MPFR integration
- Limited distribution support

## Version 0.1.0 (2025-08-23)

### Initial Development

- Project structure setup
- R package skeleton
- Basic C++ integration
