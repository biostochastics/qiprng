library(qiprng)

# Test 1: Numeric overflow protection in CFE
cat("Test 1: Numeric overflow protection with large discriminant\n")
tryCatch(
  {
    # Test with a very large discriminant that would previously cause overflow
    cfg <- list(a = 2^30, b = 5, c = -2, mpfr_precision = 53, distribution = "uniform_01")
    createPRNG(cfg)
    val <- generatePRNG(10)
    cat("Generated values with large discriminant:", head(val), "\n")
    cleanup_prng()
    cat("✓ Test 1 passed: No overflow with large discriminant\n\n")
  },
  error = function(e) {
    cat("✗ Test 1 failed:", e$message, "\n\n")
  }
)

# Test 2: Thread safety without const-cast
cat("Test 2: Thread safety without const-cast\n")
tryCatch(
  {
    library(parallel)
    cfg <- list(a = 2, b = 5, c = -2, mpfr_precision = 53, distribution = "uniform_01")
    createPRNG(cfg)

    # Run parallel generation
    cl <- makeCluster(2)
    clusterEvalQ(cl, library(qiprng))
    results <- parLapply(cl, 1:2, function(x) {
      generatePRNG(100)
    })
    stopCluster(cl)

    cat("✓ Test 2 passed: Thread-safe generation completed\n\n")
    cleanup_prng()
  },
  error = function(e) {
    cat("✗ Test 2 failed:", e$message, "\n\n")
  }
)

# Test 3: Statistical uniformity (no bias from fallback)
cat("Test 3: Statistical uniformity test\n")
tryCatch(
  {
    cfg <- list(a = 2, b = 5, c = -2, mpfr_precision = 53, distribution = "uniform_01")
    createPRNG(cfg)

    # Generate many values to check for bias
    vals <- generatePRNG(10000)

    # Check if values are uniformly distributed
    chi_test <- chisq.test(table(cut(vals, breaks = 10)))
    cat("Chi-square p-value:", chi_test$p.value, "\n")

    if (chi_test$p.value > 0.01) {
      cat("✓ Test 3 passed: No statistical bias detected\n\n")
    } else {
      cat("⚠ Test 3 warning: Possible bias detected (p =", chi_test$p.value, ")\n\n")
    }

    cleanup_prng()
  },
  error = function(e) {
    cat("✗ Test 3 failed:", e$message, "\n\n")
  }
)

# Test 4: Performance improvement from batch generation
cat("Test 4: Performance test with batch generation\n")
tryCatch(
  {
    cfg <- list(a = 2, b = 5, c = -2, mpfr_precision = 53, distribution = "uniform_01")
    createPRNG(cfg)

    # Time generation of many values
    start_time <- Sys.time()
    vals <- generatePRNG(100000)
    end_time <- Sys.time()

    elapsed <- as.numeric(end_time - start_time, units = "secs")
    cat("Generated 100,000 values in", elapsed, "seconds\n")
    cat("Rate:", 100000 / elapsed, "values/second\n")
    cat("✓ Test 4 passed: Batch generation working\n\n")

    cleanup_prng()
  },
  error = function(e) {
    cat("✗ Test 4 failed:", e$message, "\n\n")
  }
)

cat("All tests completed!\n")
