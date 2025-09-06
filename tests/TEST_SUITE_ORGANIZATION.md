# Test Suite Organization for qiprng

## Overview

The qiprng test suite provides comprehensive coverage of all package functionality using the standard R testthat framework. The test suite has been reorganized to eliminate redundancy and provide clear, maintainable test coverage.

## Current Test Structure

### Primary Test Framework

- **Entry Point**: `testthat.R` - Standard R package test runner
- **Test Directory**: `testthat/` - Contains all production test files
- **Test Pattern**: `test-*.R` - Consistent naming convention for all tests

### Test Categories

The test suite is organized into logical categories within the `testthat/` directory:

#### 1. Core Functionality Tests

- `test-basic.R` - Basic PRNG initialization and generation
- `test-prng.R` - Core PRNG operations
- `test-config.R` - Configuration management
- `test-config-manager.R` - Advanced configuration handling
- `test-validation.R` - Input validation and error handling

#### 2. Distribution Tests

- `test-distributions.R` - Standard distributions (uniform, normal)
- `test-new-distributions.R` - Extended distributions (Cauchy, Levy, Pareto)
- `test-ziggurat.R` - Ziggurat algorithm for normal distribution
- `test-normal.R` - Normal distribution generation methods

#### 3. Performance & Threading

- `test-thread-safety.R` - Thread safety validation
- `test-parallel-*.R` - Parallel processing tests
- `test-persistent-thread-pool.R` - Thread pool management
- `test-jump-ahead-performance.R` - Jump-ahead functionality

#### 4. Statistical Tests

- `test-statistical-tests.R` - Core statistical test suite
- `test-compression-bootstrap.R` - Compression and bootstrap tests
- `test-multidim-tests.R` - Multidimensional statistical tests
- `test-nist-*.R` - NIST test implementations
- `test-p-value-adjustment.R` - Multiple testing corrections

#### 5. Advanced Features

- `test-advanced.R` - Advanced PRNG features
- `test-caching-framework.R` - Result caching system
- `test-bootstrap-framework.R` - Bootstrap statistical framework
- `test-external-wrappers.R` - Integration with external packages

#### 6. Bug Fixes & Improvements

- `test-comprehensive-fixes.R` - Validation of bug fixes
- `test-comprehensive-improvements.R` - Phase 1-3 improvements
- `test-data-structure-fixes.R` - Data structure corrections
- `test-overflow-safety.R` - Numerical overflow protection

#### 7. Security

- `test-crypto-security.R` - Cryptographic security features
- `test-deterministic-mode.R` - Deterministic mode for testing

## Running Tests

### Run All Tests

```bash
# Standard R package testing
Rscript -e "devtools::test()"

# Or using testthat directly
Rscript tests/testthat.R

# Or within R
library(testthat)
test_package("qiprng")
```

### Run Specific Test Category

```r
# Run a specific test file
test_file("tests/testthat/test-basic.R")

# Run tests matching a pattern
test_dir("tests/testthat", filter = "thread")
```

### Continuous Integration

```bash
# For CI/CD pipelines
R CMD check .
```

## Test Coverage Areas

The test suite provides comprehensive coverage for:

1. **Core Functionality**
   - PRNG initialization and cleanup
   - Random number generation
   - Configuration management
   - State management

2. **Statistical Validity**
   - Distribution correctness
   - Statistical test suites (NIST, compression, etc.)
   - P-value adjustments and multiple testing corrections
   - Effect size calculations

3. **Performance & Scalability**
   - Thread safety
   - Parallel processing
   - Memory management
   - Performance benchmarks

4. **Robustness**
   - Edge cases and boundary conditions
   - Invalid input handling
   - Numerical stability
   - Overflow protection

5. **Integration**
   - External package compatibility
   - Cross-platform support
   - Different R environments

## Development Guidelines

### Adding New Tests

1. **Location**: Place new tests in `tests/testthat/`
2. **Naming**: Use pattern `test-<feature>.R`
3. **Structure**: Use testthat's `test_that()` blocks
4. **Documentation**: Include clear test descriptions
5. **Cleanup**: Always clean up resources (use `cleanup_prng()`)

### Test Best Practices

1. **Isolation**: Each test should be independent
2. **Determinism**: Use seeds for reproducible results
3. **Coverage**: Test both success and failure paths
4. **Performance**: Keep individual tests fast (<1 second)
5. **Clarity**: Use descriptive test names and expectations

### Example Test Structure

```r
test_that("descriptive test name", {
  # Setup
  cfg <- list(...)
  createPRNG(cfg)

  # Test
  result <- generatePRNG(1000)

  # Assertions
  expect_length(result, 1000)
  expect_true(all(result >= 0 & result <= 1))

  # Cleanup
  cleanup_prng()
})
```

## Maintenance Notes

### Recent Cleanup (2025)

- Removed 60+ redundant files from `standalone_tests/` directory
- Consolidated duplicate test coverage
- Standardized naming conventions
- Updated documentation to reflect actual structure

### Test Organization Principles

1. **Single Source of Truth**: All tests in `testthat/`
2. **Clear Categories**: Logical grouping by functionality
3. **No Redundancy**: Each feature tested once
4. **Standard Framework**: Use testthat exclusively

### Deprecated/Removed

- `standalone_tests/` directory (5,286 lines of redundant code)
- Development/debug test files
- Duplicate test implementations
- Non-standard test runners

## Test Statistics

- **Total Test Files**: ~40 in testthat/
- **Total Test Lines**: ~7,500 lines
- **Coverage Areas**: 7 major categories
- **Test Framework**: testthat 3.x

## Contributing

When contributing tests:

1. Follow the existing structure and naming conventions
2. Ensure tests pass locally before submitting
3. Include tests for new features in the same PR
4. Update this documentation if adding new test categories
