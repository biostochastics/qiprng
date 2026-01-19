# qiprng: Quadratic Irrational Pseudo-Random Number Generator for R

<img src="images/qiprng_logo.png" alt="QIPRNG Logo" width="300"/>

[![Version](https://img.shields.io/badge/Version-0.7.3-blue)](https://github.com/biostochastics/qiprng)
[![ThreadSafe](https://img.shields.io/badge/Thread%20Safe-Yes-brightgreen)](https://github.com/biostochastics/qiprng)
[![License](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)
[![R-CMD-check](https://img.shields.io/badge/R--CMD--check-passing-brightgreen)](https://github.com/biostochastics/qiprng)
[![Language](https://img.shields.io/badge/Language-R%2FC%2B%2B-blue.svg)](https://www.r-project.org/)
[![Tests](https://img.shields.io/badge/Statistical%20Tests-70%2B-brightgreen)](https://github.com/biostochastics/qiprng)
[![SIMD](https://img.shields.io/badge/SIMD-AVX2%2FNEON-orange)](https://github.com/biostochastics/qiprng)
[![Parallel](https://img.shields.io/badge/Parallel-OpenMP-green)](https://github.com/biostochastics/qiprng)

## Overview

The `qiprng` package implements a pseudo-random number generator based on quadratic
irrational numbers with hardware acceleration and parallel generation capabilities.
It offers high-precision arithmetic, optional ChaCha20 output mixing, and extensive
statistical distribution support.

```r
library(qiprng)
createPRNG()
x <- generatePRNG(10000)
hist(x, breaks = 50, main = "qiprng uniform distribution", col = "skyblue")
```

## Version History

For recent changes, see [NEWS.md](NEWS.md). For complete version history with
detailed technical notes, see [CHANGELOG.md](CHANGELOG.md).

## Key Features

The package offers high-precision computation using the MPFR library with configurable
precision from 24 to 10,000 bits, supporting over 14 statistical distributions including
standard continuous and discrete distributions as well as extended distributions like
Levy stable, Pareto, and multivariate normal. Hardware acceleration is provided through
SIMD vectorization with AVX2/NEON support and enhanced OpenMP parallelization with
thread-local caching, SIMD integration, and optimized buffer management for improved
performance on modern multi-core systems.

Multiple strategies support parallel stream generation: parameter splitting (independent
discriminants per stream), stream leapfrogging, and crypto counter-mode mixing. The
package provides MPFR-based high-precision iteration for sequential jump-ahead with
automatic overflow prevention. An optional output mixing
layer uses ChaCha20 stream cipher with XOR mixing for preserving uniformity or modular
addition for enhanced entropy. Thread safety is guaranteed through proper synchronization
primitives and thread-local resources, while deterministic mode with seed support enables
reproducible sequences for research and testing. The generator has been validated using
the NIST SP 800-22 statistical test framework.

## Quick Start

```r
# Install from GitHub
remotes::install_github("biostochastics/qiprng")

# Generate random numbers
library(qiprng)
createPRNG()
random_values <- generatePRNG(1000)
```

For detailed installation instructions by platform, see the [Installation](#installation)
section below.

## Acknowledgments

> This implementation is based on **Vincent Granville's** work on random number generators
> using quadratic irrationals. The mathematical foundation and the core algorithm design
> follow Granville's approach.
>
> **Reference:** Granville, V. (2022). [Random Number Generator Based on Quadratic
> Irrationals](https://mltechniques.com/2022/12/13/military-grade-fast-random-number-generator-based-on-quadratic-irrationals/).
> Note: The original work has not been peer-reviewed in cryptographic literature.

## Performance & Quality

The generator achieves excellent performance in single-threaded mode with
near-linear scaling in multi-threaded configurations. Memory usage remains constant at
O(1) with thread-local caching. All 750 possible discriminants have been validated
through statistical testing. The package includes a curated set of 370 discriminants
that passed comprehensive testing, which is used by default. For research purposes, all
discriminants can be enabled through configuration options.

The generator passes 98.4% of NIST SP 800-22 statistical tests, indicating good
distributional properties. Note that NIST tests detect obvious statistical defects but
do not establish cryptographic security—they are a necessary but not sufficient
condition for security-sensitive applications.

## Installation

This package contains C++ code and requires a compiler and several system libraries to be
installed on your system before the R package can be built.

### Prerequisites

#### macOS

You will need to install the Command Line Tools for Xcode and the required libraries using
[Homebrew](https://brew.sh/).

1. **Install Xcode Command Line Tools**:

    ```sh
    xcode-select --install
    ```

2. **Install Homebrew** (if you don't have it):

    ```sh
    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
    ```

3. **Install required libraries**:

    ```sh
    brew install pkg-config gmp mpfr libsodium openssl
    ```

#### Linux (Debian/Ubuntu)

Use `apt-get` to install the required development libraries:

```sh
sudo apt-get update
sudo apt-get install build-essential libmpfr-dev libgmp-dev libsodium-dev libssl-dev
```

#### Linux (Fedora/CentOS/RHEL)

Use `yum` or `dnf` to install the required development libraries:

```sh
sudo dnf groupinstall "Development Tools"
sudo dnf install gmp-devel mpfr-devel libsodium-devel openssl-devel
```

#### Windows

For Windows, the required libraries are downloaded automatically during installation, but
you need to have [Rtools](https://cran.r-project.org/bin/windows/Rtools/) installed and
configured correctly.

### Install the Package

Once the prerequisites are installed, you can install `qiprng` from GitHub using the
`remotes` package in R:

```r
if (!require("remotes")) {
  install.packages("remotes")
}
remotes::install_github("biostochastics/qiprng")
```

## Supported Distributions

**Continuous Distributions:**

- Uniform
- Normal
- Exponential
- Gamma
- Beta
- Log-Normal
- Weibull
- Chi-Squared
- Student's t
- Levy Stable (Alpha-stable random variates using Chambers-Mallows-Stuck algorithm)
- Pareto (Heavy-tailed distribution for extreme events modeling)
- Cauchy (Fat-tailed distribution with undefined mean)
- Multivariate Normal (Correlated normal vectors, requires Eigen3)

**Discrete Distributions:**

- Bernoulli
- Binomial
- Poisson
- Negative Binomial

**Special Distributions:**

- Gaussian Copula (Complex dependencies between different marginal distributions)

### Distribution Examples

```r
# Standard distributions
createPRNG(list(distribution = "normal", normal_mean = 0, normal_sd = 1))
normal_samples <- generatePRNG(1000)

createPRNG(list(distribution = "exponential", exponential_lambda = 0.5))
exp_samples <- generatePRNG(1000)

# Extended distributions
levy_samples <- generate_levy_stable(n = 1000, alpha = 1.5, beta = 0.5, mu = 0, sigma = 1)
pareto_samples <- generate_pareto(n = 1000, xm = 1, alpha = 2.5)
cauchy_samples <- generate_cauchy(n = 1000, location = 0, scale = 1)

# Multivariate normal
mean_vec <- c(0, 0, 0)
cov_mat <- matrix(c(1, 0.5, 0.2, 0.5, 1, 0.3, 0.2, 0.3, 1), 3, 3)
mvn_samples <- generate_multivariate_normal(n = 1000, mean_vec, cov_mat)

# Gaussian copula
correlation <- matrix(c(1, 0.7, 0.7, 1), 2, 2)
marginals <- list(
  list(type = "cauchy", location = 0, scale = 1),
  list(type = "pareto", xm = 1, alpha = 3)
)
copula_samples <- generate_with_copula(n = 1000, correlation, marginals)
```

## Architecture

### Lock-Free Thread-Local Design

The optimized architecture eliminates mutex contention through thread-local storage:

```cpp
// Each thread maintains its own PRNG state
static thread_local ThreadLocalData {
    std::vector<QuadraticIrrational> qis;  // Thread-local QI instances
    std::vector<double> cache;              // L1-optimized cache (4096 samples)
    size_t cache_pos;                       // Current position in cache
} tl_data;
```

Key optimizations include:

- **Zero Contention**: Each thread has independent PRNG instances
- **Cache-Line Alignment**: 64-byte alignment prevents false sharing
- **Golden Ratio Seeding**: `thread_seed = base_seed + thread_id * 0x9E3779B97F4A7C15`
- **Prefetching**: Hardware prefetch hints for sequential access patterns

## Usage

### Basic Usage

The simplest way to use qiprng is with default settings. The generator automatically
selects optimal parameters and uses uniform distribution over [0,1]:

```r
library(qiprng)
createPRNG()
x <- generatePRNG(10000)
mean(x)  # ~0.5
var(x)   # ~1/12
```

### Distributions

Configure the generator for different statistical distributions by specifying the
distribution type and its parameters:

```r
# Normal distribution
createPRNG(list(
  distribution = "normal",
  normal_mean = 0,
  normal_sd = 1
))
norm_samples <- generatePRNG(1000)

# Exponential distribution
createPRNG(list(
  distribution = "exponential",
  exponential_lambda = 0.5
))
exp_samples <- generatePRNG(1000)

# Switch distributions dynamically
updatePRNG(list(distribution = "poisson", poisson_lambda = 3.5))
pois_samples <- generatePRNG(1000)
```

### Advanced Configuration

For high-performance applications, you can customize precision, mixing strategies, and
parallelization:

```r
cfg <- list(
  a = 2L, b = 5L, c = -2L,         # Custom quadratic coefficients
  mpfr_precision = 256L,           # Higher precision (24-10000 bits)
  mixing_strategy = "cascade_mix",  # Enhanced entropy mixing
  use_parallel_filling = TRUE,     # Enable parallel generation
  buffer_size = 100000             # Large buffer for efficiency
)
createPRNG(cfg)
high_quality_samples <- generatePRNG(1000000)
```

### Thread-safe Example

```r
library(qiprng)
library(parallel)

# Create a thread-safe PRNG configuration
createPRNG(list(
  distribution = "normal",
  normal_method = "ziggurat",   # Both ziggurat and box_muller are supported and fully thread-safe
  use_threading = TRUE,         # Enable thread safety
  use_parallel_filling = FALSE, # For maximum stability
  buffer_size = 10000,          # Larger buffer for better performance
  debug = TRUE                  # Enable debug output
))

# Use parallel processing with the thread-safe PRNG
cl <- makeCluster(4)
clusterEvalQ(cl, library(qiprng))

# The same PRNG can now be safely used across parallel workers
results <- parSapply(cl, 1:4, function(i) {
  # Each worker gets values from the shared thread-safe PRNG
  values <- generatePRNG(5000)
  c(mean = mean(values), sd = sd(values))
})

stopCluster(cl)
print(results)
```

### Reproducible Random Numbers (Deterministic Mode)

For scientific computing and testing, you can enable fully reproducible sequences. Version 0.6.5
ensures perfect determinism - identical seeds always produce identical sequences:

```r
# Set a seed for reproducibility
cfg <- list(
  seed = 12345,
  a = 2L,
  b = 5L,
  c = -2L
)
createPRNG(cfg)

# This will always produce the same sequence
set1 <- generatePRNG(100)

# Create another PRNG with the same seed
cleanup_prng()
createPRNG(cfg)
set2 <- generatePRNG(100)

# Verify they're identical
all(set1 == set2)  # TRUE
```

Note: When seed is provided, the PRNG uses deterministic initialization while maintaining
mathematical properties. The generator performs a warm-up period for proper mixing. For
full determinism with normal distributions, use `normal_method = "box_muller"`.

### MultiQI Mixing Strategies

```r
# Multiple quadratic irrationals with mixing
cfg <- list(
  a = c(2, 3, 5),          # Multiple QI coefficients
  b = c(7, 11, 13),
  c = c(-3, -5, -7),
  mixing_strategy = "cascade_mix"  # Choose mixing strategy
)
createPRNG(cfg)

# Available strategies:
# round_robin: Cycles through QIs sequentially (fastest)
# xor_mix: XOR combines outputs for bit diffusion (preserves uniformity)
# averaging: Averages multiple QI outputs (note: produces triangular, not uniform distribution)
# modular_add: Modular addition of outputs (preserves uniformity)
# cascade_mix: Cascaded mixing for maximum entropy
```

### Jump-Ahead and Parallel Streams

```r
# For parallel applications, the recommended approach is parameter splitting:
# Different (a, b, c) triplets produce statistically independent sequences
cfg <- list(
  a = c(2, 3, 5),          # Multiple QI coefficients
  b = c(7, 11, 13),
  c = c(-3, -5, -7),
  mixing_strategy = "modular_add"
)
createPRNG(cfg)

# Sequential jump-ahead is also available for moderate distances:
createPRNG()
initial <- generatePRNG(5)
jumpAheadPRNG(10000)  # Direct iteration with MPFR precision
after_jump <- generatePRNG(5)

# Note: Unlike linear congruential generators, the nonlinear quadratic
# map does not admit O(log n) matrix-based jump-ahead. See MATH.md
# Appendix A.9 for the mathematical proof.
```

## Statistical Testing

The package includes a comprehensive testing framework with over 70 statistical tests
covering distribution uniformity, independence, correlation, and entropy. The test suite
implements the NIST SP 800-22 standards along with classical PRNG tests.

### NIST SP 800-22 Validation Results

Testing was performed using the NIST Statistical Test Suite (STS) on binary sequences
generated from five different discriminant configurations.

**Test Results Summary:**

- **Pass Rate:** 98.4% (185/188 tests passed)
- **Test Coverage:** All 15 NIST test categories
- **Sequence Length:** 1 million bits per test

The generator passed all fundamental randomness tests including Frequency, Runs, DFT,
Random Excursions, and Linear Complexity tests. The minor failures (3 out of 148
Non-overlapping Template tests) fall within acceptable statistical variation.

To reproduce the NIST validation:

- Binary sequence generation script: [`validation/nist_top5_sequences/generate_binary_for_nist.R`](validation/nist_top5_sequences/generate_binary_for_nist.R)
- Test discriminants used: [`validation/nist_top5_sequences/discriminants_used.csv`](validation/nist_top5_sequences/discriminants_used.csv)
- The script generates 100 sequences of 1M bits each per discriminant for NIST STS testing

```

## Mathematical Foundation

The generator implements the quadratic irrational recurrence relation
x_{n+1} = (a·x_n² + b·x_n + c) mod 1, where the coefficients are constrained to
a > 0, c < 0, and Δ = b² - 4ac > 0. These constraints define an empirical parameter
family that tends to produce richer folding behavior under mod-1 projection. With
properly validated parameters, this deterministic dynamical system on [0,1) produces
sequences with excellent statistical properties. Each parameter set is validated
through empirical testing.

**Key mathematical guarantees** (see [MATH.md](MATH.md) Appendix A for proofs):

- Modular-addition mixing with ChaCha20 yields *provably uniform* output (Haar invariance)
- XOR mixing with a uniform operand produces uniform mantissas
- Multi-stream modular addition reduces bias geometrically (Fourier damping)
- CFE periodicity for quadratic irrationals follows from Lagrange's theorem

**Important caveats**: The discriminant condition Δ > 0 is a screening heuristic, not
a mathematical guarantee of chaos or uniformity. The generator's statistical quality
depends on empirical validation of specific parameter sets. For rigorous analysis,
see [MATH.md](MATH.md).

## Performance Considerations

The qiprng generator prioritizes high-precision arithmetic and mathematical rigor over
raw speed. Using MPFR arbitrary-precision arithmetic and optional ChaCha20 output mixing,
it runs approximately 50x slower than simple linear congruential generators. This may be
justified when high-precision arithmetic or exotic seeding mechanisms are required. For
applications prioritizing throughput, consider xoshiro256++, PCG64, or similar modern
PRNGs which offer excellent statistical properties at higher speed. For general Monte
Carlo simulations where speed is paramount, standard generators like Mersenne Twister may
be more appropriate.

## Security Considerations

**This generator has not undergone formal cryptographic analysis.**

While qiprng passes standard statistical tests and offers optional ChaCha20 mixing, it
should not be considered a cryptographically secure PRNG (CSPRNG). Specifically:

- No proof exists that the generator satisfies the "next-bit test" (computational
  indistinguishability from true randomness)
- State recovery may be feasible if an attacker can determine the current position in
  the quadratic irrational expansion
- The ChaCha20 mixing layer, when enabled, provides the actual resistance to state
  recovery—the QI layer contributes computational cost without proven security benefit

**For cryptographic applications** (key generation, nonces, IVs, etc.), use:
- `sodium::randombytes()` (libsodium)
- `openssl::rand_bytes()`
- System CSPRNGs (`/dev/urandom`, `CryptGenRandom`)

**qiprng is appropriate for:**
- Scientific computing requiring reproducible, high-precision random sequences
- Research into number-theoretic PRNG constructions
- Applications where the seeding mechanism or precision requirements are not met by
  standard generators

## Caching Framework

The package includes a powerful caching system to optimize performance for repeated operations:

### Cache Management

```r
# Enable/disable caching
set_cache_enabled(TRUE)
is_cache_enabled()  # Check status

# Clear cache entries
clear_qiprng_cache()  # Clear all
clear_qiprng_cache("acf")  # Clear by pattern (regex supported)

# Export and import cache
export_cached_results("cache_backup.rds")
import_cached_results("cache_backup.rds", overwrite = FALSE)

# Cache statistics
qiprng_cache_stats()  # Overall cache info
test_cache_stats()    # Test-specific cache stats
```

### Cached Functions

The framework provides cached versions of expensive computations:

```r
# Cached statistical functions
cached_acf(x, lag.max = 50)     # Autocorrelation
cached_pacf(x, lag.max = 50)    # Partial autocorrelation
cached_spectrum(x)               # Spectral density
cached_compress(x, type = "gzip")  # Compression

# Test result caching
cached_test_result(test_func, test_name, test_category,
                  data, config, ...)
```

## API Reference

### Core Functions

The package provides a simple yet powerful API for random number generation:

- **`createPRNG(config)`** - Initializes a new generator with specified configuration
- **`generatePRNG(n)`** - Generates n random values using current settings
- **`updatePRNG(config)`** - Updates generator configuration dynamically
- **`jumpAheadPRNG(n)`** - Advances state by n steps for parallel streams
- **`reseedPRNG()`** - Reinitializes with fresh entropy
- **`cleanupPRNG()`** - Releases resources and cleans up memory

### Configuration Options

The generator accepts numerous configuration parameters to customize behavior. Key options
include quadratic coefficients (a, b, c), MPFR precision (24-10000 bits), distribution
type and parameters, mixing strategy for multiple QIs, ChaCha20 output mixing settings,
and parallelization options. See the package documentation for complete parameter
descriptions and valid ranges.

### Environment Variables

The following environment variables can be used to control MPFR optimization behavior:

- **`QIPRNG_FORCE_MPFR`** - Set to `1` to disable fast-path optimization and force MPFR operations
- **`QIPRNG_FAST_PATH`** - Set to `1` to force fast-path even for precision > 64 bits
- **`QIPRNG_ENABLE_MPFR_DIAGNOSTICS`** - Set to `1` to enable precision loss tracking (compile-time flag)

These variables are useful for debugging, benchmarking, and ensuring reproducible results
in research environments.

### Testing Functions

The testing framework provides comprehensive validation capabilities through `test_prng()`
for quick validation, `create_prng_test_suite()` for custom test configurations, and
`run_prng_test_suite()` for executing full test batteries with detailed reporting.

## Dependencies

- R (>= 4.0.0)
- Rcpp (>= 1.0.0)
- MPFR library (for high-precision arithmetic)
- libsodium (for optional ChaCha20 output mixing)

## Contributing

Contributions are welcome! See [CONTRIBUTING.md](CONTRIBUTING.md) for detailed
guidelines including:

- Development environment setup
- Pre-commit hooks configuration
- Code style guidelines (R and C++)
- Testing procedures

Quick start:

```bash
git clone https://github.com/biostochastics/qiprng.git
cd qiprng
pre-commit install  # Set up code quality hooks
R CMD INSTALL .
```

## Citation

If you use this package in your research, please cite:

```bibtex
@Manual{qiprng,
  title = {qiprng: Quadratic Irrational Pseudo-Random Number Generator for R},
  author = {Sergey Kornilov},
  year = {2025},
  note = {R package version 0.7.3},
  url = {https://github.com/biostochastics/qiprng}
}
```

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Contact

Sergey Kornilov - <sergey.kornilov@biostochastics.com>

Project Link: [https://github.com/biostochastics/qiprng](https://github.com/biostochastics/qiprng)

---

*Part of the [Biostochastics](https://github.com/biostochastics) collection of tools for
translational science and biomarker discovery*
