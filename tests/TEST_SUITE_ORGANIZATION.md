# Test Suite Organization for qiprng

## Overview

The qiprng test suite has been reorganized and consolidated to provide comprehensive coverage of all functionality, including recent improvements from Phases 1-3.

## Test Structure

### Master Test Runner
- **File**: `run_master_test_suite.R`
- **Purpose**: Consolidated test execution for all package components
- **Usage**: `Rscript tests/run_master_test_suite.R`

### Test Categories

#### 1. Core Functionality Tests
Located in `testthat/` with pattern `test-(basic|normal|thread|config|validation|caching)`
- Basic PRNG functionality
- Normal distribution generation
- Thread safety
- Configuration management
- Validation framework
- Caching system

#### 2. Statistical Improvements Tests
Located in `testthat/` with pattern `test-(p-value-adjustment|comprehensive-improvements)`
- **test-p-value-adjustment.R**: Phase 1 multiple testing correction
- **test-comprehensive-improvements.R**: All Phase 1-3 improvements

#### 3. Statistical Tests
Located in `testthat/` with pattern `test-(compression|bootstrap|multidim|nist|external)`
- Compression tests
- Bootstrap framework
- Multidimensional tests
- NIST test implementations
- External package integration

#### 4. Advanced Features
Located in `testthat/` with pattern `test-(advanced|ziggurat|parallel)`
- Advanced PRNG features
- Ziggurat algorithm
- Parallel processing

## Key Test Files

### Comprehensive Improvements Test
**File**: `testthat/test-comprehensive-improvements.R`

Tests all recent improvements:
- Phase 1: P-value adjustment
  - Multiple testing correction methods
  - Pass/fail decision updates
  - Helper functions
- Phase 2: Weighting system
  - Custom weight application
  - Multi-faceted rankings
  - Backward compatibility
- Phase 3: Effect sizes
  - Calculation accuracy
  - Interpretation conventions
  - Integration with results

### Legacy Test Files

Some test files appear to be redundant or development artifacts:
- `basic_test.R`, `basic_verify.R` - Likely superseded by `test-basic.R`
- `simple_ziggurat_test.R`, `safest_ziggurat_test.R` - Development versions
- `debug_thread_safety.R`, `verify_thread_safety.R` - Debugging artifacts

## Running Tests

### Run All Tests
```bash
# From package root
Rscript tests/run_master_test_suite.R
```

### Run Specific Category
```r
# In R
library(testthat)
test_file("tests/testthat/test-comprehensive-improvements.R")
```

### Run During Development
```r
# In R, from package root
devtools::test()
```

## Test Coverage

The test suite covers:
1. **Statistical Validity**: P-value adjustments, multiple testing correction
2. **Flexibility**: Configurable weighting systems
3. **Interpretability**: Effect size calculations and interpretations
4. **Performance**: Thread safety, parallel processing
5. **Integration**: All components working together
6. **Edge Cases**: Invalid inputs, empty data, numerical limits

## Maintenance Notes

1. **Adding New Tests**: Follow the naming convention `test-<feature>.R`
2. **Categories**: Update `test_categories` in master runner when adding new categories
3. **Dependencies**: Ensure all required files are sourced in test files
4. **Documentation**: Update this file when making structural changes

## Recommended Cleanup

To reduce confusion, consider removing:
- Duplicate basic test files
- Debug/verification scripts
- Development test versions

Keep only the canonical test files that follow the `test-*.R` pattern.