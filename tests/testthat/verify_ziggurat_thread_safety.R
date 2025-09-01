#!/usr/bin/env Rscript

# verify_ziggurat_thread_safety.R
# A simple script to verify ziggurat thread safety during cleanup

library(qiprng)

cat("Running ziggurat thread safety verification test...\n")

# Function to test ziggurat cleanup
test_ziggurat_cleanup <- function() {
  # Create a PRNG with ziggurat method for normal distribution
  prng <- createPRNG(
    a = 2,
    b = 5,
    c = -3,
    distribution = "normal",
    normal_method = "ziggurat",
    use_threading = TRUE,
    debug = TRUE
  )

  # Generate some numbers
  values <- generatePRNG(n = 1000)

  # Calculate stats to make sure we're actually using the generator
  cat(sprintf("Mean: %f\n", mean(values)))
  cat(sprintf("Variance: %f\n", var(values)))

  # Clean up the PRNG
  cat("Cleaning up PRNG...\n")
  cleanup_prng()

  # Force garbage collection
  gc()

  cat("First cleanup completed successfully\n")
}

# Test ziggurat cleanup multiple times
for (i in 1:10) {
  cat(sprintf("\n===== Test iteration %d =====\n", i))
  test_ziggurat_cleanup()

  # Another garbage collection after each iteration
  gc()
}

cat("\nZiggurat thread safety test completed successfully with no segfaults!\n")
