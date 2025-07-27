# qiprng: Quadratic Irrational Pseudo-Random Number Generator for R

<img src="images/qiprng_logo.png" alt="QIPRNG Logo" width="300"/>

[![Version](https://img.shields.io/badge/Version-0.2.6-blue)](https://github.com/biostochastics/qiprng)
[![ThreadSafe](https://img.shields.io/badge/Thread%20Safe-Yes-brightgreen)](https://github.com/biostochastics/qiprng)
[![License](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)
[![R-CMD-check](https://img.shields.io/badge/R--CMD--check-passing-brightgreen)](https://github.com/biostochastics/qiprng)
[![Language](https://img.shields.io/badge/Language-R%2FC%2B%2B-blue.svg)](https://www.r-project.org/)

> High-precision, configurable pseudo-random number generator based on quadratic irrational numbers with optional cryptographic mixing

## Overview

The `qiprng` package implements a robust pseudo-random number generator based on quadratic irrational numbers. It offers superior statistical properties compared to traditional linear congruential generators while maintaining high performance and flexibility.

```r
library(qiprng)
createPRNG()
x <- generatePRNG(10000)
hist(x, breaks = 50, main = "qiprng uniform distribution", col = "skyblue")
```

## Acknowledgments

> This implementation is based on the groundbreaking work of **Vincent Granville** on random number generators using quadratic irrationals. The mathematical foundation and core algorithm design follows Granville's approach described in his 2022 publication.
>
> **Reference:** Granville, V. (2022). [Military Grade Fast Random Number Generator Based on Quadratic Irrationals](https://mltechniques.com/2022/12/13/military-grade-fast-random-number-generator-based-on-quadratic-irrationals/)

## Installation

```r
remotes::install_github("biostochastics/qiprng")
```

## Features

- **High-precision computation** using the MPFR library (24-10000 bits precision)
- **Multiple output distributions**: uniform, normal, exponential
- **Optional cryptographic mixing** using ChaCha20 for enhanced security
- **Configurable parameters** for the quadratic recurrence relation
- **Robust thread-safe operation** with proper mutex protection and thread-local resources
- **Thread-safe normal distribution** with both Box-Muller and Ziggurat methods
- **Reliable distribution transitions** with proper multi-step process for switching distributions
- **Automatic reseeding** with configurable intervals
- **Enhanced error handling** with thread-local fallback generators for statistical correctness
- **Comprehensive statistical testing** framework for PRNG quality evaluation

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

## Documentation

The `qiprng` package implements a PRNG based on the recurrence relation:

```
x_{n+1} = (a * x_n^2 + b * x_n + c) mod 1
```

Where:
- a, b, and c are integer coefficients (with constraints: a > 0, c < 0, b² - 4ac > 0)
- x_n is the current state
- x_{n+1} is the next state
- "mod 1" means taking the fractional part of the result

The mathematical foundation is based on the work of Vincent Granville (2022).

### Function Reference

| Function | Description |
|----------|-------------|
| `createPRNG()` | Creates a new PRNG instance with specified configuration |
| `generatePRNG()` | Generates random numbers using current configuration |
| `updatePRNG()` | Updates configuration of the current PRNG instance |
| `reseedPRNG()` | Forces a reseed of the PRNG |
| `jumpAheadPRNG()` | Advances the PRNG state by skipping ahead |
| `cleanupPRNG()` | Cleans up PRNG resources |
| `test_prng()` | Runs statistical tests on the PRNG |

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
  note = {R package version 0.2.6 - Thread-Safe Edition},
  url = {https://github.com/biostochastics/qiprng},
}
```

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Contact

Sergey Kornilov - sergey.kornilov@biostochastics.com

Project Link: [https://github.com/biostochastics/qiprng](https://github.com/biostochastics/qiprng)

---

*Part of the [Biostochastics](https://github.com/biostochastics) collection of tools for translational science and biomarker discovery*