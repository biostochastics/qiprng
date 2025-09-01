#!/usr/bin/env Rscript

# Test script for parameter validation and error detection
library(qiprng)

cat("\n===== Parameter Validation Test =====\n\n")

# Test creation with valid parameters
cat("Testing valid parameters...\n")
result <- tryCatch(
  {
    createPRNG(list(
      a = 2,
      b = 5,
      c = -1,
      mpfr_precision = 53,
      debug = TRUE
    ))
    cat("SUCCESS: PRNG created with valid parameters\n")
    cleanup_prng()
    TRUE
  },
  error = function(e) {
    cat("ERROR:", e$message, "\n")
    FALSE
  }
)

# Test with invalid a parameter (a=0)
cat("\nTesting invalid 'a' parameter (a=0)...\n")
result_a_zero <- tryCatch(
  {
    createPRNG(list(
      a = 0, # Should fail - 'a' cannot be zero
      b = 5,
      c = -1,
      debug = TRUE
    ))
    cat("UNEXPECTED SUCCESS: PRNG created with a=0\n")
    cleanup_prng()
    FALSE # Should not reach here
  },
  error = function(e) {
    cat("EXPECTED ERROR:", e$message, "\n")
    TRUE # Error is expected
  }
)

# Test with very large b parameter that would cause overflow
cat("\nTesting very large 'b' parameter...\n")
result_large_b <- tryCatch(
  {
    createPRNG(list(
      a = 1,
      b = 2147483647, # INT_MAX, should trigger overflow checks
      c = -1,
      debug = TRUE
    ))
    cat("PRNG created with large 'b' parameter - check for appropriate warnings\n")
    cleanup_prng()
    TRUE
  },
  error = function(e) {
    cat("ERROR:", e$message, "\n")
    FALSE
  }
)

# Test with parameters that give non-positive discriminant
cat("\nTesting parameters with non-positive discriminant...\n")
result_non_positive_disc <- tryCatch(
  {
    createPRNG(list(
      a = 1,
      b = 2,
      c = 1, # This gives discriminant = 2^2 - 4*1*1 = 0
      debug = TRUE
    ))
    cat("UNEXPECTED SUCCESS: PRNG created with non-positive discriminant\n")
    cleanup_prng()
    FALSE # Should not reach here
  },
  error = function(e) {
    cat("EXPECTED ERROR:", e$message, "\n")
    TRUE # Error is expected
  }
)

# Test invalid precision
cat("\nTesting invalid precision value...\n")
result_invalid_precision <- tryCatch(
  {
    createPRNG(list(
      mpfr_precision = 1000000, # Too large
      debug = TRUE
    ))
    cat("UNEXPECTED SUCCESS: PRNG created with invalid precision\n")
    cleanup_prng()
    FALSE # Should not reach here
  },
  error = function(e) {
    cat("EXPECTED ERROR:", e$message, "\n")
    TRUE # Error is expected
  }
)

# Summary
cat("\n===== Parameter Validation Test Summary =====\n\n")
cat("Valid parameters test:", ifelse(result, "PASSED", "FAILED"), "\n")
cat("Invalid 'a' parameter test:", ifelse(result_a_zero, "PASSED", "FAILED"), "\n")
cat("Large 'b' parameter test:", ifelse(result_large_b, "PASSED", "FAILED"), "\n")
cat("Non-positive discriminant test:", ifelse(result_non_positive_disc, "PASSED", "FAILED"), "\n")
cat("Invalid precision test:", ifelse(result_invalid_precision, "PASSED", "FAILED"), "\n")

cat("\n===== Test completed! =====\n")
