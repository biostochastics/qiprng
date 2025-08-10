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

> High-performance, cryptographically-secure PRNG with hardware acceleration, parallel generation, and advanced mixing strategies

## 🚀 Version 0.5.0 Highlights

- **Hardware Acceleration**: SIMD vectorization (AVX2/NEON), OpenMP parallelization
- **Advanced Mixing Strategies**: XOR, averaging, modular, cascade mixing
- **Parallel Generation**: Work-stealing queue for optimal load balancing
- **Matrix Jump-Ahead**: O(log n) complexity with MPFR precision
- **Extended Distributions**: Levy stable, Pareto, Cauchy, multivariate normal
- **Apple Silicon Optimized**: Native ARM64 with NEON acceleration

## Overview

The `qiprng` package implements a cutting-edge pseudo-random number generator based on quadratic irrational numbers with advanced parallelization and hardware acceleration. It offers superior statistical properties, cryptographic security, and blazing-fast performance through modern CPU features.

```r
library(qiprng)
createPRNG()
x <- generatePRNG(10000)
hist(x, breaks = 50, main = "qiprng uniform distribution", col = "skyblue")
```

## Extended Statistical Distributions

Version 0.5.0 introduces advanced statistical distributions powered by the core QIPRNG engine:

### Levy Stable Distribution
Generate alpha-stable random variates using the Chambers-Mallows-Stuck algorithm:
```r
# Generate Levy stable random numbers
levy_samples <- generate_levy_stable(n = 1000, alpha = 1.5, beta = 0.5, mu = 0, sigma = 1)
# alpha: stability parameter (0,2], beta: skewness [-1,1]
```

### Pareto Distribution
Heavy-tailed distribution for modeling extreme events:
```r
# Generate Pareto distributed values
pareto_samples <- generate_pareto(n = 1000, xm = 1, alpha = 2.5)
# xm: scale (minimum value), alpha: shape parameter
```

### Cauchy Distribution
Fat-tailed distribution with undefined mean:
```r
# Generate Cauchy distributed values
cauchy_samples <- generate_cauchy(n = 1000, location = 0, scale = 1)
```

### Multivariate Normal Distribution
Generate correlated normal vectors (requires Eigen3):
```r
# Define mean and covariance
mean_vec <- c(0, 0, 0)
cov_mat <- matrix(c(1, 0.5, 0.2,
                    0.5, 1, 0.3,
                    0.2, 0.3, 1), 3, 3)
mvn_samples <- generate_multivariate_normal(n = 1000, mean_vec, cov_mat)
```

### Gaussian Copula
Model complex dependencies between different marginal distributions:
```r
# Define correlation and marginals
correlation <- matrix(c(1, 0.7, 0.7, 1), 2, 2)
marginals <- list(
  list(type = "cauchy", location = 0, scale = 1),
  list(type = "pareto", xm = 1, alpha = 3)
)
copula_samples <- generate_with_copula(n = 1000, correlation, marginals)
```

## Acknowledgments

> This implementation is based on  **Vincent Granville's** work on random number generators using quadratic irrationals. The mathematical foundation and the core algorithm design both follow Granville's approach.
>
> **Reference:** Granville, V. (2022). [Military Grade Fast Random Number Generator Based on Quadratic Irrationals](https://mltechniques.com/2022/12/13/military-grade-fast-random-number-generator-based-on-quadratic-irrationals/)

## Quality Assurance

### Comprehensive Discriminant Validation

The `qiprng` package has undergone rigorous quality analysis of all 750 possible discriminants. Each discriminant configuration was tested with 1,000,000 samples against our comprehensive statistical test suite.

**Validation Results:**

| Quality Rating | Count | Percentage | Description |
|----------------|-------|------------|-------------|
| Excellent | 370 | 49.3% | Passed all tests with p-values > 0.01 |
| Very Good | 324 | 43.2% | Minor deviations, suitable for most applications |
| Good | 50 | 6.7% | Acceptable for non-critical uses |
| Poor | 6 | 0.8% | Failed multiple tests, not recommended |

**Key Findings:**

- **370 Excellent Discriminants:** These parameter sets demonstrate exceptional statistical properties across all test categories
- **Robust Autocorrelation Testing:** Enhanced testing framework with stricter thresholds (max correlation < 0.010)
- **Production-Ready Selection:** Default configuration uses only the 370 excellent-rated discriminants
- **Statistical Guarantee:** 92.5% of discriminants rated "Good" or better, confirming the mathematical soundness

### Production-Ready Defaults

By default, qiprng uses only the 370 "Excellent" rated discriminants, ensuring maximum quality and reliability for production use:

```r
# Production configuration (default)
config <- list(
  use_excellent_only = TRUE,    # Use only validated excellent discriminants
  quality_threshold = 0.010     # Maximum autocorrelation threshold
)
```

For research or legacy compatibility:

```r
# Research configuration (all 750 discriminants)
config <- list(
  use_excellent_only = FALSE,   # Use all discriminants
  use_csv_discriminants = TRUE  # Load from discriminants.csv
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

## Features

- **High-precision computation** using the MPFR library (24-10000 bits precision)
- **14 statistical distributions**: uniform, normal, exponential, Poisson, gamma, beta, Bernoulli, binomial, log-normal, Weibull, chi-squared, Student's t, negative binomial
- **Cryptographic-grade security** using ChaCha20 cipher for enhanced randomness
- **Deterministic/reproducible mode** with seed support for scientific computing (v0.2.7)
- **Configurable parameters** for the quadratic recurrence relation
- **Robust thread-safe operation** with proper mutex protection and thread-local resources
- **Thread-safe normal distribution** with both Box-Muller and Ziggurat methods
- **Reliable distribution transitions** with proper multi-step process for switching distributions
- **Automatic reseeding** with configurable intervals
- **Enhanced error handling** with thread-local fallback generators for statistical correctness
- **Comprehensive statistical testing** framework with 70+ tests for PRNG quality evaluation
- **Optimized jump-ahead** with O(log n) complexity using matrix exponentiation and MPFR arithmetic (v0.5.0)
- **Validated discriminants** with 370 excellent-rated parameters for optimal quality
- **MultiQI ensemble management** with 5 mixing strategies for enhanced entropy (v0.5.0)
- **CFE period detection** using Gauss-Legendre algorithm for O(L) complexity (v0.5.0)
- **Dynamic QI generation** with square-free validation and D = k² + 1 pattern (v0.5.0)
- **Memory pooling** for MPFR objects reducing allocation overhead (v0.5.0)

## Supported Distributions

### Continuous Distributions

| Distribution | Parameters | Description | Example Usage |
|-------------|------------|-------------|---------------|
| **Uniform** | `range_min`, `range_max` | Uniform distribution on [min, max] | `distribution = "uniform_range"` |
| **Normal** | `normal_mean`, `normal_sd` | Normal (Gaussian) distribution | `distribution = "normal"` |
| **Exponential** | `exponential_lambda` | Exponential distribution with rate λ | `distribution = "exponential"` |
| **Gamma** | `gamma_shape`, `gamma_scale` | Gamma distribution | `distribution = "gamma"` |
| **Beta** | `beta_alpha`, `beta_beta` | Beta distribution on [0,1] | `distribution = "beta"` |
| **Log-Normal** | `lognormal_mu`, `lognormal_sigma` | Exponential of normal distribution | `distribution = "lognormal"` |
| **Weibull** | `weibull_shape`, `weibull_scale` | Weibull distribution (reliability analysis) | `distribution = "weibull"` |
| **Chi-Squared** | `chisquared_df` | Chi-squared distribution with df degrees of freedom | `distribution = "chisquared"` |
| **Student's t** | `student_t_df` | Student's t-distribution with df degrees of freedom | `distribution = "student_t"` |

### Discrete Distributions

| Distribution | Parameters | Description | Example Usage |
|-------------|------------|-------------|---------------|
| **Bernoulli** | `bernoulli_p` | Binary outcome with probability p | `distribution = "bernoulli"` |
| **Binomial** | `binomial_n`, `binomial_p` | Sum of n Bernoulli trials | `distribution = "binomial"` |
| **Poisson** | `poisson_lambda` | Poisson distribution with mean λ | `distribution = "poisson"` |
| **Negative Binomial** | `negative_binomial_r`, `negative_binomial_p` | Number of failures before r successes | `distribution = "negative_binomial"` |

### Distribution Features

- **Optimized algorithms**: Normal approximations for large parameters (e.g., binomial with n > 50 and np > 5)
- **Numerical stability**: Robust handling of edge cases and extreme parameter values
- **Thread-safe**: All distributions maintain thread safety with proper state management
- **Efficient reuse**: Complex distributions leverage simpler ones (e.g., chi-squared uses gamma)

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

### v0.5.0 New Features

```r
# Advanced Mixing Strategies for improved randomness
cfg <- list(
  a = 2L, b = 5L, c = -2L,
  mixing_strategy = "xor_mixing",  # Options: round_robin, xor_mixing, averaging, modular_add, cascade
  use_parallel_filling = TRUE,     # Enable parallel buffer filling
  buffer_size = 100000             # Large buffer for parallel efficiency
)
createPRNG(cfg)

# Benchmark parallel vs sequential generation
system.time(vals <- generatePRNG(1000000))  # Parallel: ~0.5s on Apple Silicon M1

# Jump-ahead for independent parallel streams
cfg$offset <- 1000000  # Jump ahead 1M values
createPRNG(cfg)
stream2 <- generatePRNG(10000)  # Independent stream
```

### Advanced Example

```r
# Create PRNG with custom configuration
cfg <- list(
  a = 2L,                    # Quadratic coefficient (must be positive)
  b = 5L,                    # Linear coefficient
  c = -2L,                   # Constant term (must be negative)
  mpfr_precision = 128L,     # Precision in bits (24-10000)
  use_crypto_mixing = TRUE,  # Apply cryptographic mixing
  distribution = "normal",   # Output distribution
  normal_mean = 0,           # Mean for normal distribution
  normal_sd = 1              # SD for normal distribution
)
createPRNG(cfg)

# Generate normal random numbers
norm_samples <- generatePRNG(10000)
qqnorm(norm_samples)
qqline(norm_samples)

# Update to exponential distribution
updatePRNG(list(
  distribution = "exponential",
  exponential_lambda = 0.5   # Rate parameter (mean = 1/lambda)
))

# Generate exponential random numbers
exp_samples <- generatePRNG(10000)
hist(exp_samples, breaks = 50, main = "Exponential Distribution", col = "lightgreen")
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

Note: When seed is provided, the PRNG uses deterministic initialization while maintaining all mathematical properties:
- **Discriminant selection**: When a seed is provided, discriminants are shuffled using a deterministic RNG seeded with the user's seed, providing reproducible but still randomized selection. Without a seed, discriminants are shuffled using the thread-local random RNG.
- **Warm-up period**: The generator performs 10,000-100,000 initial iterations to ensure proper mixing and unpredictable starting states.
- This mode is suitable for research and testing but should not be used when cryptographic randomness is required.

For full determinism with normal distributions, use `normal_method = "box_muller"` instead of the default ziggurat method, as the ziggurat implementation uses additional randomness for efficiency.

### MultiQI Mixing Strategies (v0.5.0)

The `qiprng` package now supports multiple quadratic irrationals working in ensemble with configurable mixing strategies for enhanced entropy:

```r
# Create PRNG with specific mixing strategy
cfg <- list(
  a = c(2, 3, 5),          # Multiple QI coefficients
  b = c(7, 11, 13),
  c = c(-3, -5, -7),
  mixing_strategy = "cascade_mix"  # Choose mixing strategy
)
createPRNG(cfg)

# Available mixing strategies:
# - "round_robin": Cycles through QIs sequentially (fastest)
# - "xor_mix": XOR combines outputs for bit diffusion
# - "averaging": Averages multiple QI outputs
# - "modular_add": Modular addition of outputs
# - "cascade_mix": Cascaded mixing for maximum entropy

# Update mixing strategy on the fly
updatePRNG(list(mixing_strategy = "xor_mix"))
```

Each mixing strategy offers different trade-offs between performance and entropy quality:
- **round_robin**: Best performance, good for general use
- **xor_mix**: Excellent bit diffusion, ideal for cryptographic applications
- **cascade_mix**: Maximum entropy mixing, best for high-security scenarios

### Jump-Ahead Example

```r
# The jump-ahead functionality allows efficient skipping in the sequence
createPRNG()

# Generate initial values
initial <- generatePRNG(5)

# Jump ahead 1 billion steps in the sequence - O(log n) complexity!
jumpAheadPRNG(1000000000)

# Generate values after the jump
after_jump <- generatePRNG(5)

# v0.5.0: Uses matrix exponentiation with MPFR arithmetic
# - O(log n) time complexity instead of O(n)
# - Can jump ahead by astronomical numbers efficiently
# - Maintains full precision and periodicity properties
```

## Statistical Testing Framework

The `qiprng` package includes a comprehensive statistical testing suite with over 70 tests to validate PRNG quality. The framework evaluates randomness across multiple dimensions:

### Test Categories

1. **Basic Distribution Tests** (7 tests)
   - Kolmogorov-Smirnov test for uniformity
   - Chi-squared goodness-of-fit test
   - Mean and variance tests
   - Min/max range tests with beta distribution
   - Effect sizes: Cohen's d, Cramér's V

2. **Runs and Independence Tests** (10 tests)
   - Runs above/below median
   - Turning points test for local extrema
   - Difference sign test
   - Bartels rank test
   - Cox-Stuart trend test
   - Gap tests for specific ranges ([0.0-0.1], [0.4-0.6], [0.9-1.0])

3. **Correlation Tests** (8 tests)
   - Serial correlation at multiple lags (1, 2, 3, 5, 10)
   - Autocorrelation function (ACF) analysis
   - Partial autocorrelation function (PACF)
   - Spectral analysis with Kolmogorov-Smirnov test
   - Ljung-Box test for overall autocorrelation

4. **Binary and Bitwise Tests** (14 tests)
   - NIST SP 800-22 test suite implementation
   - Monobit frequency test
   - Block frequency test
   - Runs test on binary sequences
   - Longest run of ones test
   - Serial test for bit patterns
   - Approximate entropy test
   - Random excursions tests
   - Linear complexity test (Berlekamp-Massey)

5. **Classical PRNG Tests** (3 tests)
   - Coupon collector test
   - Poker hand test with combinatorial probabilities
   - Birthday spacing test with Poisson distribution

6. **Advanced Statistical Tests** (4 tests)
   - Shannon entropy test for information content
   - Gap test with geometric distribution analysis
   - Serial correlation test at multiple lags
   - Poker test for digit pattern analysis

7. **Compression Tests** (4 tests)
   - GZIP and BZIP2 compression ratios
   - Shannon entropy calculation
   - Byte frequency chi-square test
   - Bootstrap-based compression analysis

8. **External Test Integration** (15+ tests)
   - CryptRndTest package: Adaptive chi-square, birthday spacings, GCD test
   - randtests package: Complete randomness test battery
   - Standard R tests with appropriate transformations

### Comprehensive Comparison Results

Based on extensive analysis with 100 runs per test and sample sizes up to 5 million:

| Generator | Overall Pass Rate | Statistical Moments Quality |
|-----------|------------------|----------------------------|
| dqrng (Threefry) | 100.0% | 0.009004 total deviation |
| dqrng (Xoshiro256++) | 98.8% | 0.004434 total deviation |
| dqrng (Xoroshiro128++) | 98.6% | 0.004670 total deviation |
| dqrng (PCG64) | 97.3% | 0.007339 total deviation |
| **QIPRNG (High Precision)** | **96.8%** | **0.001758 total deviation** |
| R L'Ecuyer-CMRG | 96.3% | 0.001169 total deviation |
| R Wichmann-Hill | 96.2% | 0.000747 total deviation |
| **QIPRNG (No Crypto)** | **95.9%** | **0.001427 total deviation** |
| **QIPRNG (Default)** | **95.8%** | **0.003427 total deviation** |
| R Marsaglia-Multicarry | 95.7% | 0.002615 total deviation |
| R Mersenne-Twister | 95.6% | 0.000804 total deviation |
| Crypto (OpenSSL) | 95.4% | 0.001587 total deviation |
| **QIPRNG (With Crypto)** | **95.2%** | **0.002300 total deviation** |
| dqrng (Xoroshiro128+) | 88.4% | 0.004006 total deviation |

**Key Findings:**
- Best statistical moments: R Knuth-TAOCP (0.000586), R Knuth-TAOCP-2002 (0.000612), R Wichmann-Hill (0.000747)
- QIPRNG variants show good moments adherence with QIPRNG (No Crypto) at 0.001427 total deviation
- All generators pass basic statistical tests with high rates (94-97%)
- Advanced tests (Gap, Spectral) now properly implemented with realistic pass rates
- Trade-off between raw speed and statistical quality: dqrng is fastest (~100M numbers/sec) while QIPRNG provides cryptographic security (~6M numbers/sec)

### Test Highlights

**Spectral Test**: Use Kolmogorov-Smirnov test to compare spectral densities against exponential distribution (expected for random sequences). Average pass rate: 99.1%

**Gap Test**: Analyze gaps between occurrences of values in range [0.3, 0.7], comparing to geometric distribution. Average pass rate: 96.4%

**Statistical Moments**: Analysis of deviations from theoretical values shows:
- Best overall: R Knuth-TAOCP with 0.000586 total deviation
- QIPRNG (No Crypto): 0.001427 total deviation - good performance
- dqrng variants: 0.004006-0.009004 total deviation
- All generators show acceptable adherence to theoretical moments

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

The `qiprng` package implements a PRNG based on the recurrence relation:

```
x_{n+1} = (a * x_n^2 + b * x_n + c) mod 1
```

Where:
- a, b, and c are integer coefficients (with constraints: a > 0, c < 0, b² - 4ac > 0)
- x_n is the current state in [0,1]
- x_{n+1} is the next state
- "mod 1" means taking the fractional part

The discriminant Δ = b² - 4ac must be a non-perfect square to ensure the generated sequence has desirable randomness properties. The mathematical foundation is based on the ergodic theory of quadratic maps and the work of Vincent Granville (2022).

### Jump-Ahead Functionality (v0.5.0 Enhanced)

The `jumpAheadPRNG()` function allows you to efficiently advance the PRNG state by n steps without generating all intermediate values. Version 0.5.0 introduces a revolutionary O(log n) implementation using matrix exponentiation with MPFR arithmetic.

**Key Applications**:
- **Parallel simulations**: Different workers can jump to non-overlapping segments of the sequence
- **Reproducible subsampling**: Jump to specific points in the sequence for reproducible results
- **Astronomical jumps**: Can efficiently jump ahead by 10^18 steps or more

**v0.5.0 Implementation Details**:
- **Matrix exponentiation**: Uses 2x2 transformation matrices raised to powers via binary exponentiation
- **MPFR arithmetic**: All operations maintain full precision during matrix computations
- **O(log n) complexity**: Jump ahead by n steps in just log₂(n) matrix multiplications
- **Period-aware**: Leverages CFE period detection for correct modular arithmetic

**Technical Innovation**:
The jump-ahead transforms the quadratic map into matrix form:
```
[x_{n+1}]   [p q] [x_n]
[   1   ] = [0 1] [ 1 ]
```
Where the matrix can be efficiently raised to the nth power using binary exponentiation, reducing complexity from O(n) to O(log n).

### Performance Characteristics

The `qiprng` generator is designed for cryptographic-quality randomness rather than raw speed. In benchmarks, it is approximately 50x slower than standard generators like Mersenne Twister.

**Why the performance difference?**

1. **High-precision arithmetic**: qiprng uses MPFR (Multiple Precision Floating-Point Reliable) library for arbitrary precision calculations, while standard generators use simple integer arithmetic
2. **Complex algorithm**: The quadratic irrational algorithm (x_{n+1} = frac(a*x_n² + b*x_n + c)) requires 5 MPFR operations per number vs 1-2 integer operations for linear generators
3. **Cryptographic mixing**: Optional ChaCha20 mixing adds additional security at a computational cost
4. **Thread safety**: Comprehensive mutex protection and thread-local resources add synchronization overhead

**This is a deliberate trade-off**: qiprng prioritizes cryptographic security and mathematical rigor over speed. Use it when you need:
- Cryptographic-quality randomness
- Quantum-inspired unpredictability
- Precise control over statistical properties
- Enhanced security features

For general statistical sampling where speed is critical and cryptographic security is not required, standard generators remain the better choice.

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

| Parameter | Type | Description | Default |
|-----------|------|-------------|---------|
| `a` | integer/vector | Quadratic coefficient(s) (must be > 0) | 2 |
| `b` | integer/vector | Linear coefficient(s) | 5 |
| `c` | integer/vector | Constant term(s) (must be < 0) | -2 |
| `mpfr_precision` | integer | Precision in bits (24-10000) | 128 |
| `use_crypto_mixing` | logical | Apply ChaCha20 mixing | TRUE |
| `distribution` | character | Output distribution | "uniform_01" |
| `use_excellent_only` | logical | Use only validated discriminants | TRUE |
| `seed` | integer | Seed for reproducibility | NULL |
| `mixing_strategy` | character | MultiQI mixing strategy (v0.5.0) | "round_robin" |

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