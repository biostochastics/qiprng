# qiprng: Quadratic Irrational Pseudo-Random Number Generator for R

<img src="images/qiprng_logo.png" alt="QIPRNG Logo" width="300"/>

[![Version](https://img.shields.io/badge/Version-0.2.7-blue)](https://github.com/biostochastics/qiprng)
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

> This implementation is based on  **Vincent Granville's** work on random number generators using quadratic irrationals. The mathematical foundation and the core algorithm design both follow Granville's approach.
>
> **Reference:** Granville, V. (2022). [Military Grade Fast Random Number Generator Based on Quadratic Irrationals](https://mltechniques.com/2022/12/13/military-grade-fast-random-number-generator-based-on-quadratic-irrationals/)

## Quality Assurance

### Comprehensive Discriminant Analysis

The `qiprng` package has undergone a rigorous, comprehensive quality analysis. All 750 discriminants were tested with 1,000,000 samples each against a suite of stringent statistical tests. After correcting and hardening the testing framework, particularly the autocorrelation analysis, the final results provide a trustworthy assessment of each parameter set.

**Key Findings:**

- **370 Excellent Discriminants:** A total of 370 discriminants (49.3%) passed all tests with the highest quality rating, demonstrating exceptional statistical properties.
- **Robustness Across the Board:** 92.5% of the discriminants were rated as "Good" or better (including "Very-Good"), confirming the overall strength of the generation method.
- **Effective Discrimination:** The corrected, stricter testing framework successfully distinguishes between truly superior generators and those with minor statistical weaknesses, providing users with a reliable quality guarantee.

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
- **Multiple output distributions**: uniform, normal, exponential
- **Optional cryptographic mixing** using ChaCha20 for enhanced security
- **Deterministic/reproducible mode** with seed support for scientific computing (v0.2.7)
- **Configurable parameters** for the quadratic recurrence relation
- **Robust thread-safe operation** with proper mutex protection and thread-local resources
- **Thread-safe normal distribution** with both Box-Muller and Ziggurat methods
- **Reliable distribution transitions** with proper multi-step process for switching distributions
- **Automatic reseeding** with configurable intervals
- **Enhanced error handling** with thread-local fallback generators for statistical correctness
- **Comprehensive statistical testing** framework for PRNG quality evaluation
- **Jump-ahead functionality** for efficiently skipping ahead in the sequence

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

### Jump-Ahead Example

```r
# The jump-ahead functionality allows efficient skipping in the sequence
createPRNG()

# Generate initial values
initial <- generatePRNG(5)

# Jump ahead 1 million steps in the sequence
jumpAheadPRNG(1000000)

# Generate values after the jump
after_jump <- generatePRNG(5)

# This is much more efficient than generating and discarding 1 million values
# The jump-ahead uses block processing for cache efficiency
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

### Jump-Ahead Functionality

The `jumpAheadPRNG()` function allows you to efficiently advance the PRNG state by n steps without generating all intermediate values. This is particularly useful for:

- **Parallel simulations**: Different workers can jump to non-overlapping segments of the sequence
- **Reproducible subsampling**: Jump to specific points in the sequence for reproducible results
- **Performance optimization**: Skip large segments without the computational cost of generation

**Important Notes**:
- The jump-ahead implementation uses block processing for cache efficiency
- Due to the security-focused design with random initialization, two PRNGs with identical parameters may start at different positions in the sequence
- The function maintains all statistical properties of the PRNG
- Performance scales sub-linearly with jump size due to block processing optimizations

### Function Reference

| Function | Description |
|----------|-------------|
| `createPRNG()` | Creates a new PRNG instance with specified configuration |
| `generatePRNG()` | Generates random numbers using current configuration |
| `updatePRNG()` | Updates configuration of the current PRNG instance |
| `reseedPRNG()` | Forces a reseed of the PRNG |
| `jumpAheadPRNG()` | Advances the PRNG state by skipping ahead n steps |
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
  note = {R package version 0.2.8},
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