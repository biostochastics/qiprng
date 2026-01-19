# qiprng 0.7.3

## Code Review Security & Stability Fixes (2026-01-18)

### Security Enhancements

* **Secure memory zeroing with libsodium**: `SecureBuffer::clear()` now uses `sodium_memzero()` when libsodium is available, providing cryptographically secure memory wiping for ChaCha20 keys and nonces
* **Memory barrier for secure zeroing**: Added `std::atomic_thread_fence(std::memory_order_seq_cst)` as fallback when libsodium not available

### Critical Fixes

* **QIPRNG_UNLIKELY macro self-reference** (HIGH): Fixed self-referential macro `#define QIPRNG_UNLIKELY QIPRNG_UNLIKELY` that expanded to itself. Now correctly uses `[[unlikely]]` on C++20+ and empty on C++17
* **ThreadPool destructor deadlock** (HIGH): Added self-join detection to prevent deadlock when `ThreadPool` is destroyed from within one of its own worker threads. Detaches instead of joining self
* **MemoryPool nullptr on exhaustion** (HIGH): `MemoryPool::allocate()` now provides heap fallback instead of returning nullptr when pool is exhausted, preventing null pointer dereferences
* **Shutdown crash protection** (MAJOR): Added shutdown protection to `next_mixed()` and `skip()` methods to return safe fallback values during shutdown

### Additional Code Review Fixes (2026-01-18)

* **Performance metrics circular dependency** (MAJOR): Fixed `are_metrics_available()` checking `metrics_ptr != nullptr` which created a circular dependency - metrics would never initialize. Removed null check since `get_global_metrics()` safely initializes on first call.
* **Thread join async capture bug** (MEDIUM): Fixed lambda in `ziggurat_normal.cpp` capturing loop variable by reference in `std::async` call. Now captures thread reference directly to avoid undefined behavior.
* **Thread-local cache cleanup mismatch** (MEDIUM): Fixed `cleanup_thread_caches()` clearing a different cache than `fill_buffer_openmp()` used. Moved `MultiQICache` struct to namespace level so both functions share the same cache.
* **Unguarded source() call** (LOW): `load_excellent_discriminants()` now checks if `discriminant_reports.R` exists before sourcing, with fallback to package namespace.
* **Empty result crash** (LOW): Added `nrow()` checks in `load_excellent_discriminants()` and `print_excellent_summary()` before calling `min()`/`max()`/`mean()` to prevent errors on empty data frames.
* **modifyList semantics** (LOW): Fixed `modifyList()` in `validation_suite.R` to properly handle `keep.null` parameter - NULL values now remove elements when `keep.null=FALSE` (matching `base::modifyList` behavior).
* **withTimeout context** (LOW): Fixed `withTimeout()` to run expression in caller's environment using standard R metaprogramming pattern.

### Thread Safety & Cleanup

* **Thread-local cleanup for all threads** (MEDIUM): Added `ThreadManager::cleanupAllThreads()` call during `prepare_for_unload_()` to invoke cleanup callbacks registered by all threads
* **Cleanup failure handling** (MEDIUM): `.onUnload` now warns on cleanup failure and tracks success status instead of silently ignoring errors

### Build & Test Improvements

* **Configure script robustness** (LOW): Configure scripts now create `src/` directory if missing before writing `Makevars`
* **Test file sourcing in development** (LOW): Test files now fall back to `inst/` paths when `system.file()` returns empty, supporting both installed and development workflows
* **Skip tests gracefully**: Tests using external helper files now use `skip_if()` instead of `stop()` when files are unavailable

### Files Modified

* `src/prng_common.hpp`: SecureBuffer sodium_memzero, QIPRNG_UNLIKELY fix, MemoryPool heap fallback
* `src/thread_pool.hpp`: Self-join deadlock prevention
* `src/multi_qi_optimized.cpp`: Shutdown protection for next_mixed/skip
* `src/rcpp_exports.cpp`: ThreadManager cleanup call
* `configure`, `configure.win`: src directory creation
* `R/zzz.R`: Cleanup failure handling with warnings
* `tests/testthat/test-data-structure-fixes.R`: Development fallback for test file sourcing

## Security Hardening

* **Replace predictable fallback RNG seeds** (HIGH): Changed hardcoded seed `12345` to `DeterministicSeedHelper::get_fallback_seed()` across all fallback RNG instances in `ziggurat_normal.cpp` (~20 locations). Prevents predictable output when primary generator fails.
* **Improved secure memory zeroing**: Added memory barrier after volatile zeroing in `SecureBuffer::clear()` to ensure zeroing completes before subsequent operations.

## Critical Bug Fixes

* **PackedQIState::step_once() flag checking** (CRITICAL): Added flag validation at start of `step_once()` to return NaN when state is invalid, fast path is disabled, or overflow was detected. Previously computed results even with bad state.
* **Cleanup flag race condition** (CRITICAL): Keep `cleanup_in_progress` flag set to `true` after `prepare_for_unload_()` to prevent re-entry during library unload.

## Major Bug Fixes

* **Race condition with metrics_ptr**: Made `metrics_ptr` atomic to prevent data race when reading concurrently.
* **Null pointer during shutdown**: Changed `ensure_initialized()` to throw instead of silently returning during shutdown.
* **Cache size constant inconsistency**: Changed cache size from `OPTIMAL_BATCH_SIZE` (~3072) to consistent `4096` for reproducibility.
* **Loop optimization overhead**: Hoisted cache optimization check outside hot loop to avoid per-iteration overhead.
* **Off-by-one precision threshold**: Changed `>` to `>=` in precision error threshold comparison.
* **reseed() fast-path state corruption**: Added `ensure_mpfr_state()` call before setting new value to flush pending state.
* **Seed+crypto mixing validation**: Added `cfg.deterministic` check to only enforce restriction when deterministic mode is enabled.
* **Timeout shutdown ignored**: Implemented proper timeout-aware shutdown using `wait_for()`.

## Changed

* **Upgraded mixing constant** (MEDIUM): Replaced Java LCG constant `0x5DEECE66D` with SplitMix64's golden ratio constant `0x9e3779b97f4a7c15ULL` in `combine_mantissas()` for better statistical properties.
* **Fixed XOR mixing precision loss** (LOW): Changed normalization divisor from `UINT64_MAX` to mantissa mask (52 bits) in `mix_xor()` to preserve full precision.
* **Added timing side-channel documentation**: Documented minor timing difference in tie-breaking logic as acceptable trade-off for statistical PRNG use.

# qiprng 0.7.2

## Documentation

* Added `CONTRIBUTING.md` with comprehensive contributor guide (dev setup, pre-commit hooks, code style, testing)
* Added Contributing section in README.md with quick start guide

## Changed

* **Documentation terminology overhaul** for accurate security representation:
  * "Cryptographic mixing" → "ChaCha20 output mixing"
  * "Cryptographic-grade" → "enhanced statistical quality"
  * "Security guarantees" → "statistical properties"
  * Removed "military-grade" terminology from attribution notes
* Renamed MATH.md Section 7 to "ChaCha20 Output Mixing"
* Parameter `use_crypto_mixing` documentation updated to "ChaCha20 output mixing"

## Bug Fixes

* Fixed `[[unlikely]]` C++20 attribute warnings when compiling with C++17
  * Added `QIPRNG_UNLIKELY` compatibility macro in `src/prng_common.hpp`
  * Eliminated 120+ compiler warnings across translation units

## Security

* Removed overstated "cryptographic security" claims (package lacks formal cryptographic analysis)
* Added Security Considerations section to README clarifying this is not a CSPRNG
* Revised NIST validation language: tests indicate good statistical properties, not cryptographic security
* Added peer-review status note for Granville reference

# qiprng 0.7.1

## Bug Fixes

* **PackedQIState coefficient truncation** (HIGH): Changed coefficient types from `int32_t` to `int64_t` to prevent silent truncation when coefficients exceed ±2³¹
* **Missing overflow detection in PackedQIState::step_once()** (HIGH): Added `std::isfinite()` check after FMA computation; sets `FLAG_OVERFLOW` flag and returns `NaN` to trigger MPFR fallback
* **Stale MPFR state on fast path failure**: Syncs `fast_value_` back to MPFR before disabling fast path
* **Coefficient range validation**: Fast path auto-disabled when |a|, |b|, or |c| > 2⁵³-1 to prevent precision loss
* **Static destruction order segfault** (CRITICAL): Fixed crash during R session shutdown
  * Added `g_shutdown_in_progress` atomic flag
  * Wrapped `perf::global_metrics` with safe accessor functions
  * Added `.prepare_for_unload_()` Rcpp export for ordered cleanup

## New Features

* `MAX_SAFE_COEFFICIENT` validation to disable fast path for large coefficient values
* `FAST_PATH_RESYNC_INTERVAL` constant (default: 1M iterations) for periodic MPFR re-sync
* `fast_path_iterations_` counter for tracking iterations since last MPFR sync
* `has_overflow()` method in PackedQIState

# qiprng 0.7.0

## Critical Bug Fixes

* **Normal distribution double-scaling**: Ziggurat method applied mean/sd transformation twice
* **Derived distributions using wrong normal**: Lognormal, chi-squared, binomial, Student-t now correctly use N(0,1) via new `generate_standard_normal()` method
* **Student-t non-integer degrees of freedom**: Replaced integer loop with gamma-based chi-squared generation
* **Pareto edge case**: Added clamping to prevent infinity when u=1.0

## Thread Safety Fixes

* **Thread pool shutdown race**: Replaced `wait_for()` with blocking `join()`
* **Static destruction order fiasco**: Added `shutdown_global_thread_pool()` called from `.onUnload`
* **OpenMP TLS cleanup issue**: Removed OpenMP from `cleanup_thread_caches()`

## Build System

* **Build system portability**: Improved configure script with proper pkg-config discovery
* Removed hardcoded Homebrew paths that broke Linux builds
* Meaningful error messages with platform-specific install instructions

## New Features

* `generate_standard_normal()` internal method for N(0,1) samples
* Cache optimization infrastructure:
  * `PackedQIState` struct aligned to 64-byte cache lines
  * Cross-platform prefetch utilities
  * `CacheOptimizedBatchProcessor` class with loop unrolling
* MSVC support with `QIPRNG_HAS_MSVC_PREFETCH` guard

## Package Infrastructure

* Created `source_pkg_file()` helper using `system.file()` with development fallback
* Added `@export` roxygen2 tag for `withTimeout`
* Moved statistical test files to `inst/statisticaltests/`

## Other Fixes

* CFE period bound: Added log term for `O(sqrt(D) * log(D))` complexity
* DeterministicSeedHelper entropy: Added high-resolution clock fallback
* MPFRWrapper swap(): Now swaps `initialized_` and `cached_precision_`
* Fixed member initialization order in QuadraticIrrational constructor
* Fixed `moments::kurtosis()` API call for newer package versions

# qiprng 0.6.6

## Documentation Fixes

* **Fixed incorrect square-free claim** in MATH.md: "D = k² + 1 is always square-free" is false
* **Fixed overstated fixed-point avoidance claim**: "a > 0, c < 0 ensures no fixed points" is not guaranteed

## New Mathematical Proofs

* **Gap Test (§16.1)**: Complete proof that gaps follow geometric distribution
* **Spectral Test (§16.2)**: Proof that I(ωₖ)/f(ωₖ) ~ Exp(1) for periodogram ordinates
* **ChaCha20 Mixing Uniformity (§7.2.1)**: Measure-theoretic proof that (U + C) mod 1 ~ Unif[0,1)
* **CFE Periodicity (§5)**: Lagrange's theorem with pigeonhole argument
* **Matrix Jump-Ahead (§4)**: Polynomial composition semigroup foundation

# qiprng 0.6.5

## Bug Fixes

* **Deterministic mode reproducibility**: Reset `MultiQIOptimized::thread_counter_` between PRNG creations
* All 23 deterministic tests now pass with perfect reproducibility

## New Features

* Fast-path for standard precision (≤64 bits) with automatic activation
* Environment variable control: `QIPRNG_FORCE_MPFR` and `QIPRNG_FAST_PATH`
* Thread-local MPFR memory pool with `ScopedMPFR` wrapper
* `QIPRNG_ENABLE_MPFR_DIAGNOSTICS` compile-time macro for production builds

# qiprng 0.6.4

## New Features

* Lock-free architecture with thread-local PRNG instances
* Thread-specific seeding using golden ratio prime (0x9E3779B97F4A7C15)
* `multi_qi_optimized.hpp/cpp` with new lock-free design
* Performance monitoring utilities for cache hit rates

## Performance

* Cache size increased from 2048 to 4096 samples for better L1 utilization
* Eliminated mutex contention (was ~5% overhead)
* Architectural limit: ~8-10M samples/sec (MPFR-bound)

# qiprng 0.6.3

## Statistical Validation

* **NIST SP 800-22 Statistical Test Suite validation**:
  * Pass rate: 98.4% (185/188 tests)
  * Coverage: All 15 NIST test categories
  * Test data: 1M bit sequences from 5 different discriminants
* Added validation report in `validation/NIST_VALIDATION.md`
* Note: NIST tests indicate good statistical properties but do not establish cryptographic security

# qiprng 0.6.2

## New Features

* **Advanced jump-ahead algorithms** with `jump_ahead_optimized_v2()`:
  * `ORIGINAL_128BIT`, `MPFR_MATRIX`, `MODULAR_MERSENNE` (default), `DIRECT_CFE`
* Environment variable `QIPRNG_JUMP_ALGORITHM` for algorithm selection (0-3)
* Pattern-based cache clearing with regex support in `clear_qiprng_cache()`
* Cache export/import: `export_cached_results()` and `import_cached_results()`

## Bug Fixes

* **Critical overflow bug**: Matrix multiplication overflow in jump-ahead for jumps > 10,000

## Code Quality

* Test code moved to `tests/cpp/` directory
* R functions standardized to snake_case naming

# qiprng 0.6.1

## New Features

* Thread-local MultiQI instance caching (reduces parallel overhead by ~20-30%)
* `#pragma omp simd` directives for SIMD+OpenMP compound speedup
* `clone()` and `reseed()` methods in MultiQI and QuadraticIrrational
* Accessor methods: `getA()`, `getB()`, `getC()`, `getMPFRPrecision()`
* `distribution_generator.hpp` with polymorphic distribution classes
* `buffer_manager.hpp` with pluggable filling strategies
* Windows build support with `configure.win` and `QIPRNG_NO_CRYPTO` flag

## Bug Fixes

* **Matrix arithmetic overflow**: Added 128-bit arithmetic path (`__int128`)
* **ChaCha20 implementation**: Replaced `randombytes_buf` with `crypto_stream_chacha20`

# qiprng 0.6.0

## Breaking Changes

* Deterministic seeds with ChaCha20 mixing now throw exceptions (previously warnings)

## Bug Fixes

* **Integer overflow vulnerability** in `is_square_free()`: Added bounds checking
* **Race conditions** in static initialization
* **Cache coherency**: Added memory barriers in `MultiQI::refillCache()`

## New Features

* Large discriminant validation using string conversion
* Precision loss mitigation: warns at >100 bits lost, errors at >200 bits
* Pre-commit hooks with automatic formatting and security checks

## Security

* Discriminant validation prevents integer overflow exploitation

# qiprng 0.5.5

## Security

* Replaced weak `std::random_device` with libsodium's `randombytes_buf()`
* Added comprehensive bounds checking and overflow protection

## Code Quality

* `.clang-format` with Google C++ Style Guide
* `Makefile.format` for unified formatting workflow

# qiprng 0.5.4

## Bug Fixes

* **TLS cleanup race conditions**: Implemented `std::call_once` synchronization
* Fixed duplicate symbol linker errors in `precision_utils.hpp`

# qiprng 0.5.3

## Bug Fixes

* **Strict aliasing violation**: Implemented `safe_bit_cast` template
* **ThreadPool deadlock**: Added `shutdown()` method with timeout

## New Features

* `mpfr_pool.hpp` for thread-local MPFR context pooling (90% memory reduction)
* `cache_aligned.hpp` for cache-line aligned atomics (2-3x performance improvement)

# qiprng 0.5.2

## Bug Fixes

* **Precision loss**: MPFR(256-bit) → long double(80-bit) → double(53-bit) conversion path
* **Mathematical constants**: Replaced `M_PI` with MPFR's `mpfr_const_pi()`

## New Features

* `precision_utils.hpp` with `safe_mpfr_to_double()`, precision loss metrics
* Support for `__float128` on compatible systems

# qiprng 0.5.1

## Bug Fixes

* **Statistical uniformity**: Thread-local fallback PRNG (Chi-square p-value: 0.956)
* **Numeric overflow**: Added 128-bit arithmetic with pre-multiplication validation
* **Thread safety**: Made mutex `mutable` for proper const-correctness
* **Lock contention**: Thread-local caching with 256-value batches

## Performance

* Throughput: 8.18M values/second (single-threaded)
* Near-linear scaling with thread count

# qiprng 0.5.0

## New Features

* **Hardware acceleration**: SIMD vectorization with AVX2/NEON support (4-8x speedup)
* **Parallel generation**: OpenMP with work-stealing queue
* **Advanced mixing strategies**: XOR, averaging, modular, and cascade mixing
* **Matrix jump-ahead**: O(log n) complexity using matrix exponentiation
* **Extended distributions**: Levy stable, Pareto, Cauchy, multivariate normal, Gaussian copula
* **Apple Silicon optimization**: Native ARM64 with NEON acceleration

# qiprng 0.4.1

## Bug Fixes

* Critical thread safety fixes for concurrent access
* Enhanced mutex protection for global state

# qiprng 0.4.0

## New Features

* Initial release with quadratic irrational PRNG implementation
* MPFR high-precision arithmetic (24-10000 bits)
* 14+ statistical distributions
* ChaCha20 cryptographic mixing
* Comprehensive test suite with 70+ statistical tests
* 370 validated discriminants

# qiprng 0.3.0

* Basic quadratic irrational implementation (beta)
* Uniform and normal distributions
* Initial test framework

# qiprng 0.2.0

* Proof of concept implementation (alpha)
* Basic MPFR integration

# qiprng 0.1.0

* Project structure setup
* R package skeleton
* Basic C++ integration
