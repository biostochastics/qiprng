library(qiprng)

# Test mixing strategies
cat("Testing MultiQI Mixing Strategies\n")
cat("==================================\n\n")

# Create MultiQI with multiple QIs
config <- list(
  a = c(1, 2, 3),
  b = c(5, 7, 11),
  c = c(-2, -3, -5),
  mpfr_precision = 256,
  distribution = "uniform_01"
)

tryCatch(
  {
    # Create PRNG with multiple QIs
    createPRNG(config)

    # Generate samples
    samples1 <- generatePRNG(1000)

    cat("Generated 1000 samples successfully\n")
    cat("Sample mean:", mean(samples1), "\n")
    cat("Sample variance:", var(samples1), "\n")
    cat("Min:", min(samples1), "Max:", max(samples1), "\n\n")

    # Test with jump-ahead
    cat("Testing jump-ahead functionality...\n")
    skipPRNG(10000)
    samples2 <- generatePRNG(100)
    cat("After jump-ahead - Mean:", mean(samples2), "\n\n")

    # Cleanup
    cleanup_prng()
    cat("Test completed successfully\n")
  },
  error = function(e) {
    cat("ERROR:", conditionMessage(e), "\n")
  }
)

# Test CFE period computation
cat("\nTesting CFE Period Computation\n")
cat("===============================\n\n")

config2 <- list(
  a = 1,
  b = 1,
  c = -1, # Should give discriminant = 5
  mpfr_precision = 256,
  distribution = "uniform_01"
)

tryCatch(
  {
    createPRNG(config2)

    # Generate some values to trigger CFE computation
    samples <- generatePRNG(100)
    cat("CFE test successful - generated 100 samples\n")
    cat("Discriminant = b^2 - 4ac = 1 - 4(1)(-1) = 5\n")

    cleanup_prng()
  },
  error = function(e) {
    cat("ERROR in CFE test:", conditionMessage(e), "\n")
  }
)

cat("\nAll tests completed\n")
