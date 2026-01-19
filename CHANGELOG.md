# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.7.2] - 2025-01-18

### Added

- `CONTRIBUTING.md` with comprehensive contributor guide (dev setup, pre-commit hooks, code style, testing)
- Contributing section in README.md with quick start guide

### Changed

- **Documentation terminology overhaul** for accurate security representation:
  - "Cryptographic mixing" → "ChaCha20 output mixing"
  - "Cryptographic-grade" → "enhanced statistical quality"
  - "Security guarantees" → "statistical properties"
  - Removed "military-grade" terminology from attribution notes
- Renamed MATH.md Section 7 to "ChaCha20 Output Mixing"
- Parameter `use_crypto_mixing` documentation updated to "ChaCha20 output mixing"

### Fixed

- `[[unlikely]]` C++20 attribute warnings when compiling with C++17
  - Added `QIPRNG_UNLIKELY` compatibility macro in `src/prng_common.hpp`
  - Eliminated 120+ compiler warnings across translation units

### Security

- Removed overstated "cryptographic security" claims (package lacks formal cryptographic analysis)
- Added Security Considerations section to README clarifying this is not a CSPRNG
- Revised NIST validation language: tests indicate good statistical properties, not cryptographic security
- Added peer-review status note for Granville reference

### Files Modified

- `README.Md`, `MATH.Md`, `NEWS.md`, `CHANGELOG.md`, `CONTRIBUTING.md`
- `R/qiprng-package.R`, `R/prng_interface.R`, `R/excellent_discriminants.R`
- `src/crypto_mixer.cpp`, `src/crypto_mixer.hpp`, `src/rcpp_exports.cpp`, `src/prng_common.hpp`

## [0.7.1] - 2025-01-04

### Fixed

- **PackedQIState coefficient truncation** (HIGH): Changed coefficient types from `int32_t` to `int64_t` in `cache_optimized.hpp` to prevent silent truncation when coefficients exceed ±2³¹
- **Missing overflow detection in PackedQIState::step_once()** (HIGH): Added `std::isfinite()` check after FMA computation; sets `FLAG_OVERFLOW` flag and returns `NaN` to trigger MPFR fallback
- **Stale MPFR state on fast path failure**: Syncs `fast_value_` back to MPFR before disabling fast path
- **Coefficient range validation**: Fast path auto-disabled when |a|, |b|, or |c| > 2⁵³-1 to prevent precision loss
- **Static destruction order segfault** (CRITICAL): Fixed crash during R session shutdown caused by destructor ordering between static globals and TLS
  - Added `g_shutdown_in_progress` atomic flag
  - Wrapped `perf::global_metrics` with safe accessor functions
  - Added `.prepare_for_unload_()` Rcpp export for ordered cleanup
  - Protected destructors in `MPFRContextPool`, `ThreadCleanupHelper`, `MultiQIOptimized::tl_data`

### Added

- `MAX_SAFE_COEFFICIENT` validation to disable fast path for large coefficient values
- `FAST_PATH_RESYNC_INTERVAL` constant (default: 1M iterations) for periodic MPFR re-sync
- `fast_path_iterations_` counter for tracking iterations since last MPFR sync
- `has_overflow()` method in PackedQIState

### Files Modified

- `src/cache_optimized.hpp`: int64_t coefficients, FLAG_OVERFLOW, coefficient validation, overflow check
- `src/quadratic_irrational.hpp`: fast_path_iterations_, FAST_PATH_RESYNC_INTERVAL, coefficient constants
- `src/quadratic_irrational.cpp`: coefficient range check, periodic re-sync, stale state handling
- `src/multi_qi_optimized.hpp`: shutdown flag declarations, cleanup methods
- `src/multi_qi_optimized.cpp`: shutdown-safe metrics access, TLS cleanup
- `src/mpfr_pool.hpp`: shutdown check in destructor
- `src/ziggurat_normal.cpp`: shutdown check in ThreadCleanupHelper destructor
- `src/rcpp_exports.cpp`: `.prepare_for_unload_()` function
- `R/zzz.R`: `.onUnload` calls comprehensive cleanup

## [0.7.0] - 2025-01-02

### Fixed

- **Normal distribution double-scaling** (CRITICAL): Ziggurat method applied mean/sd transformation twice, producing incorrect mean and inflated variance
- **Derived distributions using wrong normal** (CRITICAL): Lognormal, chi-squared (normal approx), binomial (normal approx), and Student-t now correctly use N(0,1) via new `generate_standard_normal()` method
- **Student-t non-integer degrees of freedom** (HIGH): Replaced integer loop with gamma-based chi-squared generation using Ahrens-Dieter (α<1) and Marsaglia-Tsang (α≥1) algorithms
- **Pareto edge case** (MEDIUM): Added clamping to prevent infinity when u=1.0
- **Thread pool shutdown race** (H1): Replaced `wait_for()` with blocking `join()` to guarantee worker termination
- **Static destruction order fiasco** (H2): Added explicit `shutdown_global_thread_pool()` called from `.onUnload`
- **OpenMP TLS cleanup issue** (H3): Removed OpenMP from `cleanup_thread_caches()` to prevent late TLS allocation
- **ThreadPool reset() flaw** (M1): Accept explicit thread count parameter instead of unreliable `capacity()`
- **Build system portability** (C1): Improved configure script with proper pkg-config discovery for Linux, MacPorts, and non-Homebrew macOS
- **Validation suite paths** (H4): Created `source_pkg_file()` helper using `system.file()` with development fallback
- **withTimeout export** (H5): Added `@export` roxygen2 tag for validation suite accessibility
- **CFE period bound** (M2): Added log term to period bound calculation for `O(sqrt(D) * log(D))` complexity
- **DeterministicSeedHelper entropy** (M4): Added high-resolution clock fallback when `std::random_device` returns zero entropy
- **MPFRWrapper swap()** (M8): Now swaps `initialized_` and `cached_precision_` along with MPFR handles
- **Member initialization order**: Fixed QuadraticIrrational constructor to match declaration order
- **MPFR_PREC_MAX comparison**: Use practical maximum (10000) instead of platform-dependent constant
- **kurtosis() API**: Updated `moments::kurtosis()` call for newer package versions
- **Test files**: Fixed mixing_strategy case sensitivity in benchmark_parallel.R
- **Orphaned docs**: Wrapped `test_qiprng.Rd` examples in `\dontrun{}`

### Added

- `generate_standard_normal()` internal method for N(0,1) samples isolated from config settings
- Cache optimization infrastructure in `cache_optimized.hpp`:
  - `PackedQIState` struct aligned to 64-byte cache lines
  - Cross-platform prefetch utilities (`prefetch_read()`, `prefetch_write()`, `prefetch_ahead()`)
  - Platform-specific aligned memory allocation
  - `CacheOptimizedBatchProcessor` class with loop unrolling
- Prefetch hints in `QuadraticIrrational::fill()` and `MultiQI::refillCache()` hot loops
- MSVC support with `<xmmintrin.h>` include and `QIPRNG_HAS_MSVC_PREFETCH` guard
- Meaningful error messages with platform-specific install instructions for missing dependencies
- Security warning comment for timing side-channel in `is_square_free()`
- WorkStealingQueue clarifying comment about mutex-based implementation

### Removed

- Tracked binary files (*.o,*.so) from repository
- Hardcoded Homebrew paths that broke Linux builds

### Files Modified

- `src/enhanced_prng.hpp`, `src/enhanced_prng.cpp`: generate_standard_normal(), distribution fixes
- `src/extended_distributions.hpp`: Pareto edge case, gamma-based Student-t
- `src/cache_optimized.hpp`: PackedQIState, prefetch utilities, batch processor
- `src/quadratic_irrational.cpp`, `src/quadratic_irrational.hpp`: prefetch hints, member init order
- `src/multi_qi.cpp`: cache optimization integration
- `src/thread_pool.hpp`: shutdown fix, reset() parameter
- `src/mpfr_pool.hpp`: swap() fix
- `configure`, `configure.ac`: portability improvements
- `R/validation_helpers.R`: source_pkg_file(), withTimeout export
- `inst/statisticaltests/`: moved statistical test files

## [0.6.6] - 2025-01-02

### Fixed

- **Incorrect square-free claim** in MATH.md: "D = k² + 1 is always square-free" is false (counterexample: k=7 yields 50 = 2 × 5²)
- **Overstated fixed-point avoidance claim**: "a > 0, c < 0 ensures no fixed points" is not guaranteed; documented practical mitigations
- Inconsistent section numbering in MATH.md (sections 8-16)

### Added

- Rigorous mathematical proofs in MATH.md:
  - **Gap Test (§16.1)**: Complete proof that gaps follow geometric distribution with P(G = k) = (1-p)^(k-1) · p
  - **Spectral Test (§16.2)**: Proof that I(ωₖ)/f(ωₖ) ~ Exp(1) for periodogram ordinates
  - **ChaCha20 Mixing Uniformity (§7.2.1)**: Measure-theoretic proof that (U + C) mod 1 ~ Unif[0,1)
  - **CFE Periodicity (§5)**: Lagrange's theorem with pigeonhole argument
  - **Matrix Jump-Ahead (§4)**: Polynomial composition semigroup foundation

## [0.6.5] - 2025-09-25

### Fixed

- **Deterministic mode reproducibility**: Reset `MultiQIOptimized::thread_counter_` between PRNG creations
- **Thread-local state management**: Added `resetDeterministicEngine()` and `reset_thread_counter()` for proper state reset
- All 23 deterministic tests now pass with perfect reproducibility

### Added

- Fast-path for standard precision (≤64 bits) with automatic activation for 53-64 bit operations
- Environment variable control: `QIPRNG_FORCE_MPFR` and `QIPRNG_FAST_PATH`
- Thread-local MPFR memory pool with `ScopedMPFR` wrapper
- `QIPRNG_ENABLE_MPFR_DIAGNOSTICS` compile-time macro for production builds
- Comprehensive benchmarking suite for performance validation
- Environment variable and deterministic mode documentation in README

### Changed

- Throughput maintains ~8-10M samples/sec (MPFR architectural limit)

## [0.6.4] - 2025-09-24

### Added

- Lock-free architecture with thread-local PRNG instances (zero-contention parallel generation)
- Thread-specific seeding using golden ratio prime (0x9E3779B97F4A7C15)
- `multi_qi_optimized.hpp/cpp` with new lock-free design
- Performance monitoring utilities for cache hit rates
- Architecture documentation explaining lock-free design

### Changed

- Cache size increased from 2048 to 4096 samples for better L1 utilization
- Cache-line aligned data structures (64 bytes) to prevent false sharing
- Added prefetching hints for sequential access patterns

### Performance

- Eliminated mutex contention (was ~5% overhead)
- MPFR operations remain primary bottleneck (~95% of runtime, 7-8 ops at ~122ns each)
- Architectural limit: ~8-10M samples/sec due to high-precision requirements

## [0.6.3] - 2025-09-21

### Added

- NIST SP 800-22 Statistical Test Suite validation:
  - **Pass rate**: 98.4% (185/188 tests)
  - **Coverage**: All 15 NIST test categories (Frequency, Runs, DFT, Random Excursions, Linear Complexity, etc.)
  - **Test data**: 1M bit sequences from 5 different discriminants
- Validation report in `validation/NIST_VALIDATION.md`
- Binary sequence generation scripts in `validation/nist_top5_sequences/`
- Test discriminants CSV for reproducibility

### Security

- Note: NIST tests indicate good statistical properties but do not establish cryptographic security

## [0.6.2] - 2025-09-11

### Added

- **Advanced jump-ahead algorithms** with `jump_ahead_optimized_v2()`:
  - `ORIGINAL_128BIT` (0): Original implementation (may overflow for large jumps)
  - `MPFR_MATRIX` (1): High-precision MPFR-based matrix operations (no overflow)
  - `MODULAR_MERSENNE` (2): Fast modular arithmetic with Mersenne prime 2⁶¹-1 (default)
  - `DIRECT_CFE` (3): Direct continued fraction manipulation
- Environment variable `QIPRNG_JUMP_ALGORITHM` for algorithm selection (0-3)
- `Matrix2x2_MPFR` and `Matrix2x2_Modular` classes for specialized matrix operations
- Pattern-based cache clearing with regex support in `clear_qiprng_cache()`
- Cache export/import: `export_cached_results()` and `import_cached_results()` with metadata
- `QIPRNG_TESTING` preprocessor guards for conditional compilation

### Fixed

- **Critical overflow bug**: Matrix multiplication overflow in jump-ahead for jumps > 10,000
- Optimized `mulmod()` using 128-bit arithmetic for O(1) performance

### Changed

- Test code moved to `tests/cpp/` directory (excluded from production builds)
- R functions standardized to snake_case naming

## [0.6.1] - 2025-09-11

### Added

- Thread-local MultiQI instance caching (reduces parallel overhead by ~20-30%)
- `#pragma omp simd` directives for SIMD+OpenMP compound speedup
- `clone()` and `reseed()` methods in MultiQI and QuadraticIrrational
- Accessor methods: `getA()`, `getB()`, `getC()`, `getMPFRPrecision()`
- `distribution_generator.hpp` with polymorphic distribution classes (Factory Pattern)
- `buffer_manager.hpp` with pluggable filling strategies (Strategy Pattern)
- Windows build support with `configure.win` and `QIPRNG_NO_CRYPTO` flag

### Fixed

- **Matrix arithmetic overflow**: Added 128-bit arithmetic path (`__int128`) with `__builtin_smull_overflow` fallback
- **ChaCha20 implementation**: Replaced `randombytes_buf` with `crypto_stream_chacha20` for deterministic mixing

### Changed

- Minimum chunk size increased from 256 to 4096 elements
- Batch processing in chunks of 8 doubles with cache prefetching

### Removed

- Unused fields: `using_pool_` from MPFRWrapper, `remainder` from SIMD ops, `buffer_pos` from parallel filling

### Files Modified

- `src/quadratic_irrational.hpp/cpp`: Overflow-safe matrix ops, accessor methods, reseed
- `src/multi_qi.hpp/cpp`: Clone and reseed implementations
- `src/distribution_generator.hpp`, `src/buffer_manager.hpp`: New modular components
- `src/crypto_mixer.cpp/hpp`: True ChaCha20 implementation
- `configure.win`: Windows build configuration

## [0.6.0] - 2025-09-06

### Changed

- **BREAKING**: Deterministic seeds with ChaCha20 mixing now throw exceptions (previously warnings)
- Exception-based error model: CFE period overflow throws `std::runtime_error`, non-square-free discriminants throw `std::invalid_argument`
- Replaced thread-local static initialization with `std::once_flag` for thread safety

### Fixed

- **Integer overflow vulnerability** in `QuadraticIrrational::is_square_free()`: Added bounds checking before multiplication
- **Race conditions** in `multi_qi.cpp` and `enhanced_prng.cpp` static initialization
- **Cache coherency**: Added memory barriers in `MultiQI::refillCache()`
- **Matrix overflow protection**: Safe arithmetic in `Matrix2x2::operator*()`

### Added

- Large discriminant validation using string conversion for values exceeding `LONG_MAX`
- Precision loss mitigation: warns at >100 bits lost, errors at >200 bits
- `test_numerical_stability.R` for edge cases, thread safety, and security enforcement
- Pre-commit hooks with automatic formatting, linting, and security checks

### Security

- Discriminant validation prevents integer overflow exploitation

### Files Modified

- `src/quadratic_irrational.cpp/hpp`: Overflow fixes, error handling, matrix protection
- `src/enhanced_prng.cpp`: Thread-safe initialization, security enforcement
- `src/multi_qi.cpp`: Cache coherency, fallback generator fixes
- `src/precision_utils.hpp`: Precision loss mitigation
- `.pre-commit-config.yaml`: Pre-commit configuration

## [0.5.5] - 2025-09-01

### Security

- **Critical**: Replaced weak `std::random_device` with libsodium's `randombytes_buf()` for secure seed generation
- Added comprehensive bounds checking and overflow protection in `rcpp_exports.cpp`
- Added warnings when using deterministic seeds with ChaCha20 mixing

### Added

- `.clang-format` with Google C++ Style Guide and Rcpp customizations
- `Makefile.format` for unified formatting workflow
- `.Rprofile` for tidyverse style configuration

### Fixed

- Syntax errors in R test files (`basic_verify.R`, `final_verify.R`, `verify_thread_safety.R`)
- Removed macOS resource fork files from test directory

## [0.5.4] - 2025-08-29

### Fixed

- **TLS cleanup race conditions**: Thread-local storage cleanup raced with destructor execution causing crashes
  - Implemented `std::call_once` synchronization with atomic memory ordering
  - Added thread-local atomic flags with RAII guards for recursive cleanup prevention
  - Applied acquire/release fences for cross-thread visibility
- Duplicate symbol linker errors in `precision_utils.hpp`
- Macro naming conflicts (`QIPRNG_CACHE_LINE_SIZE`)

## [0.5.3] - 2025-08-29

### Fixed

- **Strict aliasing violation**: Type punning via `memcpy` between `double`/`uint64_t` violated C++ rules
  - Implemented `safe_bit_cast` template using `std::bit_cast` (C++20) or union fallback (C++17)
- **ThreadPool deadlock**: Missing timeout mechanism during shutdown
  - Added `shutdown()` method with configurable timeout (default 5 seconds)
- **False sharing**: Adjacent atomic variables caused cache thrashing
  - Added `CacheAlignedAtomic` template with 64/128-byte alignment (2-3x performance improvement)

### Added

- `bit_operations.hpp` for safe bit casting
- `mpfr_pool.hpp` for thread-local MPFR context pooling (90% memory reduction)
- `cache_aligned.hpp` for cache-line aligned atomics

## [0.5.2] - 2025-08-29

### Fixed

- **Precision loss**: Direct MPFR→double conversions lost ~203 bits (256→53)
  - Implemented extended precision intermediates: MPFR(256-bit) → long double(80-bit) → double(53-bit)
- **Mathematical constants**: Hardcoded `M_PI` limited to 20 decimal places
  - Replaced with MPFR's `mpfr_const_pi()` for ~77 decimal digit accuracy

### Added

- `precision_utils.hpp` with `safe_mpfr_to_double()`, `PrecisionConstants`, precision loss metrics
- Support for `__float128` on compatible systems (113-bit mantissa)

## [0.5.1] - 2025-08-29

### Fixed

- **Statistical uniformity**: Hardcoded 0.5 fallback created detectable patterns
  - Implemented thread-local fallback PRNG with hardware entropy seed
  - Chi-square p-value improved from <0.001 to 0.956
- **Numeric overflow**: `P_next * P_next` could overflow before safety check
  - Added 128-bit arithmetic (`__int128`) with pre-multiplication validation fallback
- **Thread safety**: `const_cast<std::mutex&>` caused undefined behavior
  - Made mutex `mutable` for proper const-correctness
- **Lock contention**: Global mutex on every `next()` call
  - Thread-local caching with 256-value batches (256x fewer lock acquisitions)
- **Period bound**: Fixed `MAX_PERIOD=100000` insufficient for large discriminants
  - Dynamic scaling: `max(100000, 10 * sqrt(discriminant))`

### Performance

- Throughput: 8.18M values/second (single-threaded)
- Near-linear scaling with thread count

## [0.5.0] - 2025-08-28

### Added

- **Hardware acceleration**: SIMD vectorization with AVX2/NEON support (4-8x speedup)
- **Parallel generation**: OpenMP with work-stealing queue (linear scaling with cores)
- **Advanced mixing strategies**: XOR, averaging, modular, and cascade mixing
- **Matrix jump-ahead**: O(log n) complexity using matrix exponentiation with MPFR
- **Extended distributions**: Levy stable, Pareto, Cauchy, multivariate normal, Gaussian copula
- **Apple Silicon optimization**: Native ARM64 with NEON acceleration
- Object pooling for optimized memory allocation
- Cache-aligned data structures for reduced false sharing

## [0.4.1] - 2025-08-27

### Fixed

- Critical thread safety fixes for concurrent access
- Enhanced mutex protection for global state
- Improved error handling and recovery mechanisms

## [0.4.0] - 2025-08-26

### Added

- Initial release with quadratic irrational PRNG implementation
- MPFR high-precision arithmetic (24-10000 bits)
- 14+ statistical distributions
- ChaCha20 cryptographic mixing
- Comprehensive test suite with 70+ statistical tests
- 370 validated discriminants

## [0.3.0] - 2025-08-25

### Added

- Basic quadratic irrational implementation (beta)
- Uniform and normal distributions
- Initial test framework

## [0.2.0] - 2025-08-24

### Added

- Proof of concept implementation (alpha)
- Basic MPFR integration
- Limited distribution support

## [0.1.0] - 2025-08-23

### Added

- Project structure setup
- R package skeleton
- Basic C++ integration

---

[Unreleased]: https://github.com/biostochastics/qiprng/compare/v0.7.2...HEAD
[0.7.2]: https://github.com/biostochastics/qiprng/compare/v0.7.1...v0.7.2
[0.7.1]: https://github.com/biostochastics/qiprng/compare/v0.7.0...v0.7.1
[0.7.0]: https://github.com/biostochastics/qiprng/compare/v0.6.6...v0.7.0
[0.6.6]: https://github.com/biostochastics/qiprng/compare/v0.6.5...v0.6.6
[0.6.5]: https://github.com/biostochastics/qiprng/compare/v0.6.4...v0.6.5
[0.6.4]: https://github.com/biostochastics/qiprng/compare/v0.6.3...v0.6.4
[0.6.3]: https://github.com/biostochastics/qiprng/compare/v0.6.2...v0.6.3
[0.6.2]: https://github.com/biostochastics/qiprng/compare/v0.6.1...v0.6.2
[0.6.1]: https://github.com/biostochastics/qiprng/compare/v0.6.0...v0.6.1
[0.6.0]: https://github.com/biostochastics/qiprng/compare/v0.5.5...v0.6.0
[0.5.5]: https://github.com/biostochastics/qiprng/compare/v0.5.4...v0.5.5
[0.5.4]: https://github.com/biostochastics/qiprng/compare/v0.5.3...v0.5.4
[0.5.3]: https://github.com/biostochastics/qiprng/compare/v0.5.2...v0.5.3
[0.5.2]: https://github.com/biostochastics/qiprng/compare/v0.5.1...v0.5.2
[0.5.1]: https://github.com/biostochastics/qiprng/compare/v0.5.0...v0.5.1
[0.5.0]: https://github.com/biostochastics/qiprng/compare/v0.4.1...v0.5.0
[0.4.1]: https://github.com/biostochastics/qiprng/compare/v0.4.0...v0.4.1
[0.4.0]: https://github.com/biostochastics/qiprng/compare/v0.3.0...v0.4.0
[0.3.0]: https://github.com/biostochastics/qiprng/compare/v0.2.0...v0.3.0
[0.2.0]: https://github.com/biostochastics/qiprng/compare/v0.1.0...v0.2.0
[0.1.0]: https://github.com/biostochastics/qiprng/releases/tag/v0.1.0
