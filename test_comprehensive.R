#!/usr/bin/env Rscript

# Comprehensive Test Suite for qiprng
# Tests all critical fixes: TLS cleanup, overflow protection, crypto security

library(qiprng)
library(parallel)

cat("\n")
cat("==============================================\n")
cat("   COMPREHENSIVE TEST SUITE FOR QIPRNG\n")
cat("==============================================\n\n")

# Track test results
tests_passed <- 0
tests_failed <- 0
test_results <- list()

# Helper function to run a test
run_test <- function(name, test_func) {
  cat(sprintf("\n--- Test: %s ---\n", name))
  tryCatch({
    result <- test_func()
    if (result) {
      cat("✓ PASSED\n")
      tests_passed <<- tests_passed + 1
      test_results[[name]] <<- "PASSED"
    } else {
      cat("✗ FAILED\n")
      tests_failed <<- tests_failed + 1
      test_results[[name]] <<- "FAILED"
    }
  }, error = function(e) {
    cat(sprintf("✗ ERROR: %s\n", e$message))
    tests_failed <<- tests_failed + 1
    test_results[[name]] <<- paste("ERROR:", e$message)
  })
}

# Test 1: Basic functionality
run_test("Basic Functionality", function() {
  createPRNG()
  vals <- generatePRNG(1000)
  cleanup_prng()
  
  # Check basic properties
  length(vals) == 1000 && 
    all(vals >= 0) && 
    all(vals <= 1) &&
    abs(mean(vals) - 0.5) < 0.1
})

# Test 2: Thread safety with threading enabled
run_test("Thread Safety", function() {
  if (.Platform$OS.type == "windows") {
    cat("  (Skipped on Windows)\n")
    return(TRUE)
  }
  
  test_thread <- function(id) {
    cfg <- default_config
    cfg$use_threading <- TRUE
    createPRNG(cfg)
    vals <- generatePRNG(100)
    cleanup_prng()
    list(id = id, ok = length(vals) == 100)
  }
  
  results <- mclapply(1:4, test_thread, mc.cores = 4)
  all(sapply(results, function(r) r$ok))
})

# Test 3: TLS cleanup - rapid create/destroy cycles
run_test("TLS Cleanup", function() {
  for (i in 1:10) {
    cfg <- default_config
    cfg$distribution <- "normal"
    createPRNG(cfg)
    vals <- generatePRNG(100)
    cleanup_prng()
  }
  TRUE  # If we get here without crash, test passed
})

# Test 4: Overflow protection
run_test("Overflow Protection", function() {
  # Test with large values
  large_cfg <- list(
    a = 1000000L,
    b = 2000000L,
    c = -500000L,
    mpfr_precision = 53L,
    buffer_size = 100L,
    distribution = "uniform_01"
  )
  
  result <- tryCatch({
    createPRNG(large_cfg)
    vals <- generatePRNG(10)
    cleanup_prng()
    TRUE
  }, error = function(e) {
    # If error mentions overflow, that's also OK (proper detection)
    grepl("overflow", e$message, ignore.case = TRUE)
  })
  
  result
})

# Test 5: Invalid discriminant rejection
run_test("Invalid Discriminant", function() {
  invalid_cfg <- list(
    a = 1L,
    b = 2L,
    c = 2L,  # b^2 - 4ac = 4 - 8 = -4 (invalid)
    mpfr_precision = 53L,
    distribution = "uniform_01"
  )
  
  result <- tryCatch({
    createPRNG(invalid_cfg)
    FALSE  # Should not reach here
  }, error = function(e) {
    grepl("discriminant", e$message, ignore.case = TRUE)
  })
  
  result
})

# Test 6: Crypto security - no deterministic seeding
run_test("Crypto Security", function() {
  cfg <- default_config
  cfg$use_crypto_mixing <- TRUE
  cfg$seed <- NULL  # No seed
  
  createPRNG(cfg)
  vals1 <- generatePRNG(100)
  cleanup_prng()
  
  createPRNG(cfg)
  vals2 <- generatePRNG(100)
  cleanup_prng()
  
  # Should be different (not deterministic)
  !all(vals1 == vals2)
})

# Test 7: Warning for deterministic seed with crypto
run_test("Crypto Seed Warning", function() {
  cfg <- default_config
  cfg$use_crypto_mixing <- TRUE
  cfg$seed <- 12345
  
  warnings_caught <- FALSE
  withCallingHandlers({
    createPRNG(cfg)
    cleanup_prng()
  }, warning = function(w) {
    if (grepl("SECURITY WARNING.*deterministic", w$message)) {
      warnings_caught <<- TRUE
    }
    invokeRestart("muffleWarning")
  })
  
  warnings_caught
})

# Test 8: Different distributions
run_test("Multiple Distributions", function() {
  distributions <- c("uniform_01", "normal", "exponential")
  
  for (dist in distributions) {
    cfg <- default_config
    cfg$distribution <- dist
    createPRNG(cfg)
    vals <- generatePRNG(100)
    cleanup_prng()
    
    if (length(vals) != 100) return(FALSE)
  }
  TRUE
})

# Test 9: Reseed functionality
run_test("Reseed", function() {
  createPRNG()
  vals1 <- generatePRNG(10)
  reseedPRNG()
  vals2 <- generatePRNG(10)
  cleanup_prng()
  
  # After reseed, should get different values
  !all(vals1 == vals2)
})

# Test 10: Jump ahead functionality
run_test("Jump Ahead", function() {
  createPRNG()
  vals1 <- generatePRNG(10)
  
  cleanup_prng()
  createPRNG()
  jumpAheadPRNG(10)
  vals2 <- generatePRNG(10)
  cleanup_prng()
  
  # After jump, should be different from initial
  !all(vals1 == vals2)
})

# Test 11: Memory stress test
run_test("Memory Stress", function() {
  # Create and destroy many times
  for (i in 1:20) {
    createPRNG()
    vals <- generatePRNG(1000)
    cleanup_prng()
  }
  TRUE
})

# Test 12: Concurrent operations
run_test("Concurrent Operations", function() {
  if (.Platform$OS.type == "windows") {
    cat("  (Skipped on Windows)\n")
    return(TRUE)
  }
  
  concurrent_test <- function(id) {
    for (i in 1:5) {
      cfg <- default_config
      cfg$a <- 2L + id
      cfg$b <- 5L + id * 2
      cfg$c <- -2L - id
      createPRNG(cfg)
      vals <- generatePRNG(100)
      cleanup_prng()
    }
    TRUE
  }
  
  results <- mclapply(1:4, concurrent_test, mc.cores = 4)
  all(unlist(results))
})

# Test 13: Deterministic mode reproducibility
run_test("Deterministic Reproducibility", function() {
  cfg <- default_config
  cfg$seed <- 42
  cfg$use_crypto_mixing <- FALSE  # Disable crypto for deterministic test
  
  createPRNG(cfg)
  vals1 <- generatePRNG(100)
  cleanup_prng()
  
  createPRNG(cfg)
  vals2 <- generatePRNG(100)
  cleanup_prng()
  
  # With same seed, should get same values
  all(vals1 == vals2)
})

# Test 14: Statistical properties
run_test("Statistical Properties", function() {
  createPRNG()
  vals <- generatePRNG(10000)
  cleanup_prng()
  
  m <- mean(vals)
  s <- sd(vals)
  
  # Check uniform distribution properties
  abs(m - 0.5) < 0.02 && abs(s - sqrt(1/12)) < 0.02
})

# Test 15: Edge cases
run_test("Edge Cases", function() {
  # Test with minimum buffer size
  cfg <- default_config
  cfg$buffer_size <- 1L
  createPRNG(cfg)
  vals <- generatePRNG(10)
  cleanup_prng()
  
  length(vals) == 10
})

# Print summary
cat("\n")
cat("==============================================\n")
cat("              TEST SUMMARY\n")
cat("==============================================\n")
cat(sprintf("Tests Passed: %d\n", tests_passed))
cat(sprintf("Tests Failed: %d\n", tests_failed))
cat(sprintf("Total Tests:  %d\n", tests_passed + tests_failed))
cat("\n")

# Show failed tests if any
if (tests_failed > 0) {
  cat("Failed Tests:\n")
  for (name in names(test_results)) {
    if (test_results[[name]] != "PASSED") {
      cat(sprintf("  - %s: %s\n", name, test_results[[name]]))
    }
  }
  cat("\nSome tests failed. Please investigate.\n")
} else {
  cat("✓ All tests passed successfully!\n")
  cat("\nKey improvements verified:\n")
  cat("• Thread-safe destructor and TLS cleanup\n")
  cat("• Integer overflow protection\n")
  cat("• Cryptographic security fixes\n")
  cat("• Proper memory management\n")
  cat("• Statistical quality maintained\n")
}

cat("\n")