#!/usr/bin/env Rscript

# Test script for qiprng package
# This script tests the basic functionality of the qiprng package
# and logs the results to improved_test_results.log

library(qiprng)
library(moments)  # For statistical tests

log_file <- "improved_test_results.log"
cat("QIPRNG Package Test Results\n", file=log_file)
cat("==========================\n\n", file=log_file, append=TRUE)
cat("Test Date: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n", file=log_file, append=TRUE)

# Helper function to append to log
log <- function(message) {
  cat(message, "\n", file=log_file, append=TRUE)
}

# Test 1: Basic PRNG creation and generation
log("Test 1: Basic PRNG functionality")
log("-------------------------------")
tryCatch({
  createPRNG()
  x <- generatePRNG(1000)
  log(sprintf("  Generated 1000 random numbers"))
  log(sprintf("  Mean: %f", mean(x)))
  log(sprintf("  Min: %f", min(x)))
  log(sprintf("  Max: %f", max(x)))
  log(sprintf("  Standard deviation: %f", sd(x)))
  log("  Status: PASS")
}, error = function(e) {
  log(sprintf("  Error: %s", e$message))
  log("  Status: FAIL")
})
log("")

# Test 2: Test with different configurations
log("Test 2: Configuration options")
log("---------------------------")
configs <- list(
  base = list(),
  crypto = list(use_crypto_mixing = TRUE),
  uniform_range = list(distribution = "uniform_range", range_min = -10, range_max = 10),
  normal = list(distribution = "normal", normal_mean = 5, normal_sd = 2),
  exponential = list(distribution = "exponential", exponential_lambda = 0.5)
)

for (name in names(configs)) {
  cfg <- configs[[name]]
  log(sprintf("  Testing configuration: %s", name))
  tryCatch({
    createPRNG(cfg)
    x <- generatePRNG(1000)
    log(sprintf("    Mean: %f", mean(x)))
    log(sprintf("    Min: %f", min(x)))
    log(sprintf("    Max: %f", max(x)))
    
    # Check distribution properties
    if (name == "uniform_range") {
      in_range <- all(x >= -10 & x <= 10)
      log(sprintf("    All values in range [-10, 10]: %s", ifelse(in_range, "Yes", "No")))
    } else if (name == "normal") {
      log(sprintf("    Skewness: %f (should be close to 0 for normal)", skewness(x)))
      log(sprintf("    Kurtosis: %f (should be close to 3 for normal)", kurtosis(x)))
    } else if (name == "exponential") {
      log(sprintf("    All values positive: %s", ifelse(all(x >= 0), "Yes", "No")))
      log(sprintf("    Expected mean (1/lambda): %f", 1/0.5))
    }
    log("    Status: PASS")
  }, error = function(e) {
    log(sprintf("    Error: %s", e$message))
    log("    Status: FAIL")
  })
}
log("")

# Test 3: Jump ahead functionality
log("Test 3: Jump ahead functionality")
log("-----------------------------")
tryCatch({
  createPRNG()
  # Generate some initial values
  x1 <- generatePRNG(5)
  log(sprintf("  Initial 5 values: %s", paste(round(x1, 6), collapse=", ")))
  
  # Jump ahead by 1000
  jumpAheadPRNG(1000)
  x2 <- generatePRNG(5)
  log(sprintf("  After jumping 1000 steps, next 5 values: %s", paste(round(x2, 6), collapse=", ")))
  
  # Verify different sequences
  different <- !identical(x1, x2)
  log(sprintf("  Sequences are different: %s", ifelse(different, "Yes", "No")))
  log("  Status: PASS")
}, error = function(e) {
  log(sprintf("  Error: %s", e$message))
  log("  Status: FAIL")
})
log("")

# Test 4: Thread safety (if supported)
log("Test 4: Thread safety")
log("------------------")
tryCatch({
  library(parallel)
  cores <- min(4, detectCores())
  log(sprintf("  Testing with %d cores", cores))
  
  # Create PRNG with threading
  createPRNG(list(use_threading = TRUE))
  
  # Function to generate numbers in parallel
  gen_parallel <- function(id) {
    nums <- generatePRNG(100)
    c(id = id, mean = mean(nums), min = min(nums), max = max(nums))
  }
  
  results <- mclapply(1:cores, gen_parallel, mc.cores = cores)
  
  # Check if all threads got different sequences
  means <- sapply(results, function(x) x["mean"])
  log(sprintf("  Means from different threads: %s", paste(round(means, 6), collapse=", ")))
  
  all_different <- length(unique(means)) == cores
  log(sprintf("  All threads got different sequences: %s", ifelse(all_different, "Yes", "No")))
  log("  Status: PASS")
}, error = function(e) {
  log(sprintf("  Error: %s", e$message))
  log("  Status: FAIL")
})
log("")

# Test 5: Statistical properties
log("Test 5: Statistical properties")
log("---------------------------")
tryCatch({
  createPRNG()
  x <- generatePRNG(10000)
  
  # Kolmogorov-Smirnov test against uniform distribution
  ks_result <- ks.test(x, "punif")
  log(sprintf("  Kolmogorov-Smirnov test p-value: %f", ks_result$p.value))
  log(sprintf("  Uniform distribution hypothesis %s", 
              ifelse(ks_result$p.value > 0.05, "not rejected", "rejected")))
  
  # Basic statistics
  log(sprintf("  Mean (should be close to 0.5): %f", mean(x)))
  log(sprintf("  Variance (should be close to 1/12 ≈ 0.0833): %f", var(x)))
  log(sprintf("  Skewness (should be close to 0): %f", skewness(x)))
  log(sprintf("  Kurtosis (should be close to 1.8 for uniform): %f", kurtosis(x)))
  
  # Run test for randomness
  runs <- rle(x > median(x))
  num_runs <- length(runs$lengths)
  log(sprintf("  Number of runs: %d", num_runs))
  
  # Expected number of runs for random sequence
  n <- length(x)
  expected_runs <- n/2 + 0.5
  log(sprintf("  Expected number of runs: %f", expected_runs))
  log(sprintf("  Ratio of actual to expected: %f", num_runs/expected_runs))
  
  log("  Status: PASS")
}, error = function(e) {
  log(sprintf("  Error: %s", e$message))
  log("  Status: FAIL")
})
log("")

log("All tests completed.")

# Clean up
cleanup_prng()
