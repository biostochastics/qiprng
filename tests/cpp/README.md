# C++ Test Code

This directory contains C++ test code that has been isolated from the production build to reduce package size and ensure that test-only code is not included in the distributed package.

## Structure

- `test_*.cpp` - Individual test files
- `test_helpers.hpp` - Common test utilities and helpers (only available when QIPRNG_TESTING is defined)
- `Makefile` - Build configuration for standalone test compilation (not used during R package build)

## Building and Running Tests

These tests are excluded from the normal R package build process. They can be run in two ways:

### 1. Through R Testing Framework (Recommended)

```r
# Run all tests including R wrappers for C++ tests
devtools::test()

# Or use testthat directly
testthat::test_dir("tests/testthat")
```

### 2. Standalone Compilation (For Development)

```bash
cd tests/cpp
make test-all        # Build and run all tests
make test-thread-safety  # Run specific test
make clean          # Clean build artifacts
```

## QIPRNG_TESTING Flag

Test code is conditionally compiled using the `QIPRNG_TESTING` preprocessor flag. This ensures:

- Test helpers and utilities are only available during testing
- Production builds don't include test-specific code
- Package size is minimized

## Adding New Tests

1. Create a new `test_*.cpp` file in this directory
2. Include necessary headers with QIPRNG_TESTING guards:

```cpp
#ifdef QIPRNG_TESTING
#include "test_helpers.hpp"
// test implementation
#endif
```

1. Add corresponding R test wrapper in `tests/testthat/` if needed

## Notes

- These files are excluded from the R package build via `.Rbuildignore`
- Object files (*.o) should not be committed to version control
- Tests requiring R/Rcpp integration should have corresponding wrappers in `tests/testthat/`
