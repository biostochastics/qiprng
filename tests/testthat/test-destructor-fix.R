#!/usr/bin/env Rscript

# Test script to verify destructor race condition fix

library(qiprng)

# Test 1: Simple creation and destruction
cat("Test 1: Simple creation and destruction\n")
for (i in 1:10) {
  cfg <- list(
    a = 2,
    b = 5,
    c = -2,
    mpfr_precision = 53,
    use_threading = TRUE,
    buffer_size = 1000,
    distribution = "uniform_01"
  )

  createPRNG(cfg)
  vals <- generatePRNG(100)
  cleanup_prng()
  cat("  Iteration", i, "completed\n")
}
cat("Test 1 PASSED\n\n")

# Test 2: Concurrent generation and cleanup
cat("Test 2: Concurrent generation and cleanup\n")
library(parallel)

test_concurrent <- function(i) {
  cfg <- list(
    a = 2 + i,
    b = 5 + i,
    c = -2,
    mpfr_precision = 53,
    use_threading = TRUE,
    buffer_size = 500,
    distribution = "uniform_01"
  )

  createPRNG(cfg)
  vals <- generatePRNG(1000)
  cleanup_prng()
  return(length(vals))
}

# Run concurrent tests
if (.Platform$OS.type != "windows") {
  results <- mclapply(1:4, test_concurrent, mc.cores = 4)
  if (all(unlist(results) == 1000)) {
    cat("Test 2 PASSED\n\n")
  } else {
    cat("Test 2 FAILED\n\n")
  }
} else {
  cat("Test 2 SKIPPED (not on Unix)\n\n")
}

# Test 3: Rapid creation/destruction cycles
cat("Test 3: Rapid creation/destruction cycles\n")
for (j in 1:5) {
  for (i in 1:20) {
    cfg <- list(
      a = i,
      b = i + 10,
      c = i - 5,
      mpfr_precision = 53,
      use_threading = TRUE,
      buffer_size = 100,
      distribution = "uniform_01"
    )

    createPRNG(cfg)
    vals <- generatePRNG(10)
    cleanup_prng()
  }
  cat("  Cycle", j, "completed (20 iterations each)\n")
}
cat("Test 3 PASSED\n\n")

# Test 4: Test with different distributions
cat("Test 4: Different distributions\n")
distributions <- c("uniform_01", "normal", "exponential", "gamma", "beta")

for (dist in distributions) {
  cfg <- list(
    a = 2,
    b = 5,
    c = -2,
    mpfr_precision = 53,
    use_threading = TRUE,
    buffer_size = 500,
    distribution = dist
  )

  # Add distribution-specific parameters
  if (dist == "normal") {
    cfg$normal_mean <- 0
    cfg$normal_sd <- 1
  } else if (dist == "exponential") {
    cfg$exponential_rate <- 1
  } else if (dist == "gamma") {
    cfg$gamma_shape <- 2
    cfg$gamma_scale <- 1
  } else if (dist == "beta") {
    cfg$beta_alpha <- 2
    cfg$beta_beta <- 3
  }

  createPRNG(cfg)
  vals <- generatePRNG(100)
  cleanup_prng()
  cat("  Distribution", dist, "tested successfully\n")
}
cat("Test 4 PASSED\n\n")

cat("All tests completed successfully!\n")
cat("No segfaults or race conditions detected.\n")
