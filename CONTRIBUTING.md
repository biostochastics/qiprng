# Contributing to qiprng

Thank you for your interest in contributing to qiprng! This document provides
guidelines and setup instructions for development.

## Development Setup

### Prerequisites

- R (>= 4.0.0)
- C++ compiler with C++17 support
- System libraries: GMP, MPFR, libsodium, OpenSSL
- Optional: Eigen3 (for multivariate distributions)

### Installing Dependencies

**macOS (Homebrew):**

```bash
brew install gmp mpfr libsodium openssl eigen
```

**Ubuntu/Debian:**

```bash
sudo apt-get install libgmp-dev libmpfr-dev libsodium-dev libssl-dev libeigen3-dev
```

**Fedora/RHEL:**

```bash
sudo dnf install gmp-devel mpfr-devel libsodium-devel openssl-devel eigen3-devel
```

### Building from Source

```bash
git clone https://github.com/biostochastics/qiprng.git
cd qiprng
R CMD INSTALL .
```

Or from within R:

```r
devtools::install()
```

## Pre-commit Hooks

This project uses [pre-commit](https://pre-commit.com/) to ensure code quality
and consistency before commits.

### Installing Pre-commit

```bash
# Using pip
pip install pre-commit

# Using Homebrew (macOS)
brew install pre-commit
```

### Setting Up Hooks

After cloning the repository:

```bash
cd qiprng
pre-commit install
```

This installs hooks that run automatically on `git commit`.

### Running Hooks Manually

```bash
# Run on all files
pre-commit run --all-files

# Run specific hook
pre-commit run clang-format --all-files
pre-commit run style-files --all-files
```

### Configured Hooks

| Hook | Purpose | Files |
|------|---------|-------|
| **trailing-whitespace** | Remove trailing whitespace | All |
| **end-of-file-fixer** | Ensure files end with newline | All (except .Rd) |
| **check-yaml** | Validate YAML syntax | .yaml, .yml |
| **check-json** | Validate JSON syntax | .json |
| **check-added-large-files** | Prevent large files (>500KB) | All |
| **detect-private-key** | Prevent committing secrets | All |
| **mixed-line-ending** | Normalize to LF | All |
| **no-commit-to-branch** | Prevent direct commits to main | N/A |

#### R-specific Hooks

| Hook | Purpose |
|------|---------|
| **style-files** | Format R code with styler (tidyverse style) |
| **lintr** | Lint R code for common issues |
| **parsable-R** | Verify R code parses correctly |
| **no-browser-statement** | Prevent debug browser() calls |
| **no-debug-statement** | Prevent debug statements |
| **deps-in-desc** | Ensure dependencies listed in DESCRIPTION |
| **use-tidy-description** | Keep DESCRIPTION tidy |

#### C++ Hooks

| Hook | Purpose |
|------|---------|
| **clang-format** | Format C++ code (Google style with customizations) |

#### Security Hooks

| Hook | Purpose |
|------|---------|
| **detect-secrets** | Scan for accidentally committed secrets |
| **verify-seed-crypto-check** | Ensure seed/crypto incompatibility check exists |

#### Markdown Hooks

| Hook | Purpose |
|------|---------|
| **markdownlint** | Lint and fix markdown files |

### Skipping Hooks

If you need to bypass hooks temporarily (not recommended):

```bash
git commit --no-verify -m "message"
```

### Pre-push Hooks

The `r-cmd-check` hook runs on `git push` (not commit) to avoid slowing down
the commit workflow:

```bash
# Runs automatically on push
git push origin feature-branch
```

## Code Style

### R Code

- Follow [tidyverse style guide](https://style.tidyverse.org/)
- Use `styler::style_file()` or let pre-commit handle it
- Run `lintr::lint_package()` for additional checks

### C++ Code

- Follow Google C++ Style Guide with Rcpp customizations
- Use `.clang-format` configuration in repo root
- Format with `clang-format -i src/*.cpp src/*.hpp`

### Documentation

- R functions: Use roxygen2 with markdown
- C++ code: Use Doxygen-style comments for public APIs
- Markdown: Follow markdownlint rules

## Testing

### Running Tests

```r
# Run all tests
devtools::test()

# Run specific test file
testthat::test_file("tests/testthat/test-basic.R")
```

### R CMD Check

```bash
R CMD check --as-cran .
```

Or in R:

```r
devtools::check()
```

## Submitting Changes

1. Fork the repository
2. Create a feature branch: `git checkout -b feature/my-feature`
3. Make changes and commit (pre-commit hooks will run)
4. Push to your fork: `git push origin feature/my-feature`
5. Open a Pull Request

### Commit Messages

Use clear, descriptive commit messages:

```
feat: Add support for Weibull distribution
fix: Correct overflow in jump-ahead for large discriminants
docs: Update installation instructions for Windows
refactor: Simplify buffer management logic
test: Add edge case tests for normal distribution
```

## Reporting Issues

Please include:

- R version (`R.version.string`)
- Operating system
- Package version (`packageVersion("qiprng")`)
- Minimal reproducible example
- Full error message/traceback

## License

By contributing, you agree that your contributions will be licensed under the
MIT License.
