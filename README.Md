# qiprng: Quadratic Irrational Pseudo-Random Number Generator for R

<img src="images/qiprng_logo.png" alt="QIPRNG Logo" width="300"/>

[![Version](https://img.shields.io/badge/Version-0.5.0-blue)](https://github.com/biostochastics/qiprng)
[![ThreadSafe](https://img.shields.io/badge/Thread%20Safe-Yes-brightgreen)](https://github.com/biostochastics/qiprng)
[![License](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)
[![R-CMD-check](https://img.shields.io/badge/R--CMD--check-passing-brightgreen)](https://github.com/biostochastics/qiprng)
[![Language](https://img.shields.io/badge/Language-R%2FC%2B%2B-blue.svg)](https://www.r-project.org/)
[![Tests](https://img.shields.io/badge/Statistical%20Tests-70%2B-brightgreen)](https://github.com/biostochastics/qiprng)
[![SIMD](https://img.shields.io/badge/SIMD-AVX2%2FNEON-orange)](https://github.com/biostochastics/qiprng)
[![Parallel](https://img.shields.io/badge/Parallel-OpenMP-green)](https://github.com/biostochastics/qiprng)

## Overview

The `qiprng` package implements a pseudo-random number generator based on quadratic irrational numbers with hardware acceleration and parallel generation capabilities. It provides cryptographic security features and extensive statistical distribution support.

```r
library(qiprng)
createPRNG()
x <- generatePRNG(10000)
hist(x, breaks = 50, main = "qiprng uniform distribution", col = "skyblue")
```

## Key Features

### Version 0.5.0 Enhancements
- **Hardware Acceleration**: SIMD vectorization (AVX2/NEON), OpenMP parallelization
- **Advanced Mixing Strategies**: XOR, averaging, modular, cascade mixing for enhanced entropy
- **Parallel Generation**: Work-stealing queue for optimal load balancing
- **Matrix Jump-Ahead**: O(log n) complexity using matrix exponentiation with MPFR precision
- **Extended Distributions**: Levy stable, Pareto, Cauchy, multivariate normal, Gaussian copula
- **Apple Silicon Optimized**: Native ARM64 with NEON acceleration

### Core Capabilities
- **High-precision computation** using MPFR library (24-10000 bits precision)
- **14+ statistical distributions** with optimized algorithms
- **Cryptographic-grade security** using ChaCha20 cipher
- **Thread-safe operation** with mutex protection and thread-local resources
- **Deterministic/reproducible mode** with seed support
- **Comprehensive testing framework** with 70+ statistical tests
- **Validated discriminants** with 370 excellent-rated parameters

## Acknowledgments

> This implementation is based on  **Vincent Granville's** work on random number generators using quadratic irrationals. The mathematical foundation and the core algorithm design both follow Granville's approach.
>
> **Reference:** Granville, V. (2022). [Military Grade Fast Random Number Generator Based on Quadratic Irrationals](https://mltechniques.com/2022/12/13/military-grade-fast-random-number-generator-based-on-quadratic-irrationals/)

## Quality Assurance

### Discriminant Validation

All 750 possible discriminants have been tested with 1,000,000 samples against the comprehensive statistical test suite:

| Quality Rating | Count | Percentage |
|----------------|-------|------------|
| Excellent | 370 | 49.3% |
| Very Good | 324 | 43.2% |
| Good | 50 | 6.7% |
| Poor | 6 | 0.8% |

By default, qiprng uses only the 370 "Excellent" rated discriminants:

```r
# Production configuration (default)
config <- list(
  use_excellent_only = TRUE,    # Use validated excellent discriminants
  quality_threshold = 0.010     # Maximum autocorrelation threshold
)

# Research configuration (all discriminants)
config <- list(
  use_excellent_only = FALSE,
  use_csv_discriminants = TRUE
)
```

## Installation

This package contains C++ code and requires a compiler and several system libraries to be installed on your system before the R package can be built.

### Prerequisites

**macOS**

You will need to install the Command Line Tools for Xcode and the required libraries using [Homebrew](https://brew.sh/).

1.  **Install Xcode Command Line Tools**:
    ```sh
    xcode-select --install
    ```

2.  **Install Homebrew** (if you don't have it):
    ```sh
    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
    ```

3.  **Install required libraries**:
    ```sh
    brew install pkg-config gmp mpfr libsodium openssl
    ```

**Linux (Debian/Ubuntu)**

Use `apt-get` to install the required development libraries:

```sh
sudo apt-get update
sudo apt-get install build-essential libmpfr-dev libgmp-dev libsodium-dev libssl-dev
```

**Linux (Fedora/CentOS/RHEL)**

Use `yum` or `dnf` to install the required development libraries:

```sh
sudo dnf groupinstall "Development Tools"
sudo dnf install gmp-devel mpfr-devel libsodium-devel openssl-devel
```

**Windows**

For Windows, the required libraries are downloaded automatically during installation, but you need to have [Rtools](https://cran.r-project.org/bin/windows/Rtools/) installed and configured correctly.

### Install the Package

Once the prerequisites are installed, you can install `qiprng` from GitHub using the `remotes` package in R:

```r
if (!require("remotes")) {
  install.packages("remotes")
}
remotes::install_github("biostochastics/qiprng")
```


## Supported Distributions

### Standard Distributions
**Continuous**: Uniform, Normal, Exponential, Gamma, Beta, Log-Normal, Weibull, Chi-Squared, Student's t  
**Discrete**: Bernoulli, Binomial, Poisson, Negative Binomial

### Extended Distributions (v0.5.0)
- **Levy Stable**: Alpha-stable random variates using Chambers-Mallows-Stuck algorithm
- **Pareto**: Heavy-tailed distribution for extreme events modeling
- **Cauchy**: Fat-tailed distribution with undefined mean
- **Multivariate Normal**: Correlated normal vectors (requires Eigen3)
- **Gaussian Copula**: Complex dependencies between different marginal distributions

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

## Usage

### Basic Example

```r
library(qiprng)

# Create PRNG with default configuration
createPRNG()

# Generate 10,000 random numbers
x <- generatePRNG(10000)

# Basic statistical checks
mean(x)     # Should be close to 0.5 for uniform_01
var(x)      # Should be close to 1/12 for uniform_01
```

### Advanced Configuration

```r
# Mixing strategies for enhanced entropy
cfg <- list(
  a = 2L, b = 5L, c = -2L,
  mixing_strategy = "xor_mixing",  # Options: round_robin, xor_mixing, averaging, modular_add, cascade
  use_parallel_filling = TRUE,     # Enable parallel buffer filling
  buffer_size = 100000             # Large buffer for parallel efficiency
)
createPRNG(cfg)

# Jump-ahead for parallel streams
cfg$offset <- 1000000  # Jump ahead 1M values
createPRNG(cfg)
stream2 <- generatePRNG(10000)  # Independent stream
```

### Custom Configuration

```r
# Configure PRNG with specific parameters
cfg <- list(
  a = 2L,                    # Quadratic coefficient (must be positive)
  b = 5L,                    # Linear coefficient
  c = -2L,                   # Constant term (must be negative)
  mpfr_precision = 128L,     # Precision in bits (24-10000)
  use_crypto_mixing = TRUE,  # Apply cryptographic mixing
  distribution = "normal",
  normal_mean = 0,
  normal_sd = 1
)
createPRNG(cfg)
norm_samples <- generatePRNG(10000)

# Switch distribution dynamically
updatePRNG(list(distribution = "exponential", exponential_lambda = 0.5))
exp_samples <- generatePRNG(10000)
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

### Reproducible Random Numbers

For scientific computing and testing, you can enable fully reproducible sequences:

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

Note: When seed is provided, the PRNG uses deterministic initialization while maintaining mathematical properties. The generator performs a warm-up period for proper mixing. This mode is suitable for research and testing but not for cryptographic applications. For full determinism with normal distributions, use `normal_method = "box_muller"`.

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
# xor_mix: XOR combines outputs for bit diffusion
# averaging: Averages multiple QI outputs
# modular_add: Modular addition of outputs
# cascade_mix: Cascaded mixing for maximum entropy
```

### Jump-Ahead Functionality

```r
# Efficiently skip ahead in the sequence
createPRNG()
initial <- generatePRNG(5)

# Jump ahead 1 billion steps - O(log n) complexity
jumpAheadPRNG(1000000000)
after_jump <- generatePRNG(5)

# Uses matrix exponentiation with MPFR arithmetic for:
# - O(log n) time complexity
# - Astronomical jump distances
# - Full precision preservation
```

## Statistical Testing Framework

The package includes a comprehensive testing suite with 70+ tests across multiple categories:

### Test Categories

1. **Basic Distribution Tests** - Uniformity, chi-squared, mean/variance, effect sizes
2. **Runs and Independence Tests** - Runs, turning points, trend tests, gap analysis
3. **Correlation Tests** - Serial correlation, ACF/PACF, spectral analysis
4. **Binary and Bitwise Tests** - NIST SP 800-22 suite implementation
5. **Classical PRNG Tests** - Coupon collector, poker, birthday spacing
6. **Advanced Statistical Tests** - Entropy, gap distribution, pattern analysis
7. **Compression Tests** - GZIP/BZIP2 ratios, entropy calculations
8. **External Test Integration** - CryptRndTest, randtests packages

### Statistical Test Results

Comprehensive testing with 100 runs per test and sample sizes up to 5 million shows:

| Generator | Pass Rate | Moments Deviation |
|-----------|-----------|-------------------|
| dqrng (Threefry) | 100.0% | 0.009004 |
| QIPRNG (High Precision) | 96.8% | 0.001758 |
| QIPRNG (No Crypto) | 95.9% | 0.001427 |
| QIPRNG (Default) | 95.8% | 0.003427 |
| R Mersenne-Twister | 95.6% | 0.000804 |
| QIPRNG (With Crypto) | 95.2% | 0.002300 |

All QIPRNG variants demonstrate strong statistical properties with pass rates above 95% and low moment deviations, confirming the mathematical soundness of the quadratic irrational approach.

### Running Tests

```r
library(qiprng)

# Quick test with default configuration
results <- test_prng(sample_size = 1e5)

# Comprehensive test suite
createPRNG()
suite <- create_prng_test_suite(
  prng_func = function(n) generatePRNG(n),
  config = default_test_config,
  categories = c("basic", "runs", "correlation", "binary")
)
results <- run_prng_test_suite(suite, save_report = TRUE)

# View results
print(results$summary)
```

### Test Configuration

```r
# Custom test configuration
config <- default_test_config
config$basic_sample_size <- 1e6      # Larger samples for basic tests
config$significance_level <- 0.01    # Stricter significance level
config$parallel <- TRUE              # Enable parallel processing
config$cores <- 4                    # Use 4 cores

# Run with custom config
results <- test_prng(config = config)
```

## Mathematical Foundation

The PRNG is based on the recurrence relation:

```
x_{n+1} = (a * x_n^2 + b * x_n + c) mod 1
```

Where:
- a, b, c are integer coefficients (a > 0, c < 0, b² - 4ac > 0)
- x_n is the current state in [0,1]
- The discriminant Δ = b² - 4ac must be a non-perfect square

Based on ergodic theory of quadratic maps and the work of Vincent Granville (2022).

### Jump-Ahead Implementation

The `jumpAheadPRNG()` function efficiently advances the PRNG state by n steps without generating intermediate values. Using matrix exponentiation with MPFR arithmetic achieves O(log n) complexity.

**Applications**:
- Parallel simulations with non-overlapping sequences
- Reproducible subsampling at specific points
- Efficient jumps by astronomical distances (10^18+ steps)

**Technical Details**:
- Matrix exponentiation via binary exponentiation
- Full precision maintained with MPFR arithmetic
- O(log n) complexity: jump n steps in log₂(n) operations
- Period-aware using CFE detection

### Performance Considerations

The `qiprng` generator prioritizes cryptographic-quality randomness over raw speed. It is approximately 50x slower than standard generators like Mersenne Twister due to:

1. **High-precision arithmetic**: MPFR library for arbitrary precision vs simple integer operations
2. **Complex algorithm**: Quadratic irrational formula requires 5 MPFR operations per number
3. **Cryptographic mixing**: Optional ChaCha20 adds security at computational cost
4. **Thread safety**: Mutex protection and thread-local resources add overhead

Use qiprng when you need:
- Cryptographic-quality randomness
- Precise control over statistical properties
- Enhanced security features

For general statistical sampling where speed is critical, standard generators may be more appropriate.

### Core Functions

| Function | Description |
|----------|-------------|
| `createPRNG(config)` | Creates a new PRNG instance with specified configuration |
| `generatePRNG(n)` | Generates n random numbers using current configuration |
| `updatePRNG(config)` | Updates configuration of the current PRNG instance |
| `reseedPRNG()` | Forces a reseed of the PRNG with fresh entropy |
| `jumpAheadPRNG(n)` | Advances the PRNG state by skipping ahead n steps |
| `cleanupPRNG()` | Cleans up PRNG resources and releases memory |

### Testing Functions

| Function | Description |
|----------|-------------|
| `test_prng(sample_size, config, categories)` | Runs statistical tests on current PRNG |
| `create_prng_test_suite(prng_func, config, categories)` | Creates a comprehensive test suite |
| `run_prng_test_suite(suite, save_report)` | Executes all tests in the suite |
| `save_test_report(suite, filename)` | Saves HTML report with visualizations |

### Configuration Parameters

Key parameters for PRNG configuration:
- `a`, `b`, `c`: Quadratic coefficients (a > 0, c < 0, b² - 4ac > 0)
- `mpfr_precision`: Precision in bits (24-10000, default: 128)
- `use_crypto_mixing`: Apply ChaCha20 mixing (default: TRUE)
- `distribution`: Output distribution (default: "uniform_01")
- `use_excellent_only`: Use validated discriminants (default: TRUE)
- `seed`: Seed for reproducibility (default: NULL)
- `mixing_strategy`: MultiQI mixing strategy (default: "round_robin")

## Dependencies

- R (>= 4.0.0)
- Rcpp (>= 1.0.0)
- MPFR library (for high-precision arithmetic)
- libsodium (for optional cryptographic mixing)

## Contribution

Contributions are welcome! Please feel free to submit a Pull Request.

1. Fork the repository
2. Create your feature branch (`git checkout -b feature/AmazingFeature`)
3. Commit your changes (`git commit -m 'Add some AmazingFeature'`)
4. Push to the branch (`git push origin feature/AmazingFeature`)
5. Open a Pull Request

## Citation

If you use this package in your research, please cite:

```bibtex
@Manual{qiprng,
  title = {qiprng: Quadratic Irrational Pseudo-Random Number Generator for R},
  author = {Sergey Kornilov},
  year = {2025},
  note = {R package version 0.5.0},
  url = {https://github.com/biostochastics/qiprng}
}
```


## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Contact

Sergey Kornilov - sergey.kornilov@biostochastics.com

Project Link: [https://github.com/biostochastics/qiprng](https://github.com/biostochastics/qiprng)

---

*Part of the [Biostochastics](https://github.com/biostochastics) collection of tools for translational science and biomarker discovery*