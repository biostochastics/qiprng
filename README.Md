# qiprng: Quadratic Irrational Pseudo-Random Number Generator for R

[![CRAN Version](https://img.shields.io/badge/CRAN-0.2.4-blue)](https://cran.r-project.org/package=qiprng)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![R-CMD-check](https://img.shields.io/badge/R--CMD--check-passing-brightgreen)](https://github.com/biostochastics/qiprng)
[![Coverage](https://img.shields.io/badge/Coverage-85%25-green)](https://github.com/biostochastics/qiprng)

## Overview

The `qiprng` R package provides a high-precision, configurable pseudo-random number generator based on quadratic irrational numbers with optional cryptographic mixing. This implementation is based on the theoretical framework described by Granville (2022), adapted here from Python to R/C++ for learning and practice purposes. It offers superior statistical properties compared to traditional linear congruential generators while maintaining high performance and flexibility. 

## Acknowledgments

The mathematical foundation and core algorithm design of this package is based on the work of Vincent Granville, specifically his cool recent publication on [random number generators using quadratic irrationals](https://mltechniques.com/2022/12/13/military-grade-fast-random-number-generator-based-on-quadratic-irrationals/). While Granville provided the original Python implementation, this package represents an independent R/C++ implementation developed for educational purposes and practical applications in statistical computing and data analysis (e.g., Monte Carlo simulations).
Cool if you find it useful. Even better if you find a bug! 

## References

- Granville, V. (2022). Synthetic Data and Generative AI 1st Edition. https://mltechniques.com/2022/12/13/military-grade-fast-random-number-generator-based-on-quadratic-irrationals/

## Features

- **High-precision computation** using the MPFR library (24-10000 bits precision)
- **Multiple output distributions**: uniform, normal, exponential
- **Optional cryptographic mixing** using ChaCha20 for enhanced security
- **Configurable parameters** for the quadratic recurrence relation
- **Thread-safe operation** with support for thread-local PRNG instances
- **Automatic reseeding** with configurable intervals
- **Secure memory management** using SecureBuffer for sensitive data
- **Skip-ahead functionality** for efficient parallel processing

## Installation

```r
# Install from CRAN
install.packages("qiprng")

# Or install the development version from GitHub
# install.packages("devtools")
devtools::install_github("biostochastics/qiprng")
```

## Mathematical Background

The `qiprng` package uses quadratic irrational numbers to generate random sequences. A quadratic irrational is a number that can be expressed as the solution to a quadratic equation ax² + bx + c = 0 with rational coefficients where the discriminant b² - 4ac is positive but not a perfect square.

The package implements the recurrence relation:

```
x_{n+1} = (a * x_n^2 + b * x_n + c) mod 1
```

Where:
- a, b, and c are integer coefficients (with constraints: a > 0, c < 0, b² - 4ac > 0)
- x_n is the current state
- x_{n+1} is the next state
- "mod 1" means taking the fractional part of the result

This approach produces sequences with excellent statistical properties and extremely long periods.

## Basic Usage

### Creating a PRNG instance

```r
library(qiprng)

# Create with default configuration
createPRNG()

# Or with custom parameters
cfg <- list(
  a = 2L,                    # Quadratic coefficient (must be positive)
  b = 5L,                    # Linear coefficient
  c = -2L,                   # Constant term (must be negative)
  mpfr_precision = 53L,      # Precision in bits (range: 24-10000)
  use_crypto_mixing = FALSE, # Apply cryptographic mixing
  buffer_size = 1024L,       # Internal buffer size
  reseed_interval = 1000L,   # Iterations between reseeds
  use_threading = FALSE      # Enable thread-local PRNG instances
)
createPRNG(cfg)
```

### Generating random numbers

```r
# Generate 10,000 random numbers
x <- generatePRNG(10000)

# Visualize the distribution
hist(x, breaks = 50, main = "qiprng distribution", col = "skyblue")

# Basic statistical checks
mean(x)     # Should be close to 0.5 for uniform_01
var(x)      # Should be close to 1/12 for uniform_01
```

### Updating PRNG configuration

```r
# Change to normal distribution
updatePRNG(list(
  distribution = "normal",
  normal_mean = 0,
  normal_sd = 1
))

# Generate normal random numbers
norm_samples <- generatePRNG(10000)
hist(norm_samples, breaks = 50, main = "Normal distribution", col = "lightgreen")
qqnorm(norm_samples)
qqline(norm_samples)
```

### Resetting and cleaning up

```r
# Force a reseed of the PRNG
reseedPRNG()

# Skip ahead in the sequence (more efficient than generating and discarding)
jumpAheadPRNG(10000)

# Clean up PRNG resources when done
cleanupPRNG()
```

## Advanced Usage

### Thread-Safe Operation

The package provides thread safety in two ways:

```r
# 1. Global mutex protection (safe but potentially slower for parallel workloads)
updatePRNG(list(use_threading = FALSE))

# 2. Thread-local PRNG instances (faster for parallel workloads)
updatePRNG(list(use_threading = TRUE))

# When using thread-local instances with parallel package:
library(parallel)
cl <- makeCluster(4)
clusterEvalQ(cl, {
  library(qiprng)
  createPRNG(list(use_threading = TRUE))
})
```

### Statistical Testing

The package includes built-in functionality to test the PRNG quality:

```r
# Run statistical tests on the current PRNG configuration
test_results <- test_qiprng(n = 100000)
print(test_results)

# Interpret the results
cat("Kolmogorov-Smirnov p-value:", test_results$ks_pvalue, "\n")
cat("Runs test Z-score:", test_results$runs_z_score, "\n")
cat("Spectral analysis SD:", test_results$spectral_sd, "\n")

# Test a specific configuration
custom_test <- test_qiprng(n = 50000, cfg = list(
  a = 7L,
  b = 3L,
  c = -4L,
  use_crypto_mixing = FALSE
))
print(custom_test)
```

### Working with Different Distributions

```r
# Uniform distribution in a custom range
updatePRNG(list(
  distribution = "uniform_range",
  range_min = -10,
  range_max = 10
))
uniform_range_samples <- generatePRNG(10000)

# Exponential distribution
updatePRNG(list(
  distribution = "exponential",
  exponential_lambda = 0.5
))
exp_samples <- generatePRNG(10000)

# Plot the distributions
par(mfrow = c(2, 1))
hist(uniform_range_samples, breaks = 50, main = "Uniform [-10, 10]", col = "lightblue")
hist(exp_samples, breaks = 50, main = "Exponential (lambda = 0.5)", col = "salmon")
```

### Monte Carlo Simulations

The high-quality random numbers from `qiprng` are ideal for Monte Carlo simulations:

```r
# Estimating π using Monte Carlo method
n_points <- 1000000
updatePRNG(list(distribution = "uniform_01"))

# Generate points in the unit square
x <- generatePRNG(n_points)
y <- generatePRNG(n_points)

# Count points inside the unit circle
inside_circle <- sum((x - 0.5)^2 + (y - 0.5)^2 <= 0.25)

# Estimate π
pi_estimate <- 4 * inside_circle / n_points
cat("π estimate:", pi_estimate, "\n")
cat("Actual π:", pi, "\n")
cat("Error:", abs(pi_estimate - pi), "\n")
```

### Cryptographic Applications

For applications requiring enhanced security, enable the cryptographic mixing:

```r
# Create PRNG with cryptographic mixing
createPRNG(list(
  a = 13L,
  b = 7L,
  c = -5L,
  mpfr_precision = 256L,
  use_crypto_mixing = TRUE
))

# Generate random bytes for cryptographic use
random_bytes <- as.raw(floor(generatePRNG(1000) * 256))
```

## Performance Considerations

- **MPFR Precision**: Higher precision values provide better statistical properties but consume more memory and may be slower
- **Buffer Size**: Larger buffers improve performance for batch operations but consume more memory
- **Cryptographic Mixing**: Adds security but reduces performance
- **Threading Mode**: Thread-local instances offer better parallelism but may use more memory

## API Reference

| Function | Description |
|----------|-------------|
| `createPRNG()` | Creates a new PRNG instance with specified configuration |
| `generatePRNG()` | Generates random numbers using current configuration |
| `updatePRNG()` | Updates configuration of the current PRNG instance |
| `reseedPRNG()` | Forces a reseed of the PRNG |
| `jumpAheadPRNG()` | Advances the PRNG state by skipping ahead |
| `cleanupPRNG()` | Cleans up PRNG resources |
| `test_qiprng()` | Runs statistical tests on the PRNG |

## Security Considerations

- The SecureBuffer class ensures sensitive data (like encryption keys) is securely erased when no longer needed
- The cryptographic mixing option enhances security but is not cryptographically secure by default
- For high-security applications, use hardware RNG sources when available

## Citation

If you use this package in your research, please cite:

@Manual{qiprng,
  title = {qiprng: Quadratic Irrational Pseudo-Random Number Generator},
  author = {Sergey Kornilov},
  year = {2025},
  note = {R package version 0.2.4},
  url = {https://github.com/biostochastics/qiprng},
}

## License

MIT
