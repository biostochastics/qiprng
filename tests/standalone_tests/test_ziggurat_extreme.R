#!/usr/bin/env Rscript

# Extreme stress test for ziggurat normal generation with intensive threading

library(qiprng)
library(parallel)

# Suppress warnings
suppressMPFRWarnings(TRUE)

# Number of threads to use - use all available cores for maximum stress
num_cores <- parallel::detectCores()
cat("Using", num_cores, "cores for extreme stress test\n")

# Function to create/generate/cleanup in rapid succession
rapid_cycle <- function(i) {
  results <- numeric(100)
  
  for (j in 1:10) {
    cat(sprintf("Thread %d cycle %d: Creating PRNG\n", i, j))
    createPRNG(list(
      distribution = "normal",
      normal_method = "ziggurat",
      use_threading = TRUE,
      debug = FALSE
    ))
    
    cat(sprintf("Thread %d cycle %d: Generating values\n", i, j))
    values <- generatePRNG(1000)
    results[j] <- mean(values)
    
    cat(sprintf("Thread %d cycle %d: Cleaning up\n", i, j))
    cleanupPRNG()
  }
  
  cat(sprintf("Thread %d completed all cycles\n", i))
  return(results)
}

# Run parallel tests with aggressive cycling
cat("Running extreme stress test with rapid PRNG cycling...\n")
all_results <- parallel::mclapply(1:num_cores, rapid_cycle, mc.cores = num_cores)

cat("All tests completed successfully!\n")
cat("Overall mean:", mean(unlist(all_results)), "\n")

# Test large generation with cleanup immediately after
cat("\nTesting large generation with immediate cleanup...\n")
createPRNG(list(
  distribution = "normal",
  normal_method = "ziggurat",
  use_threading = TRUE,
  debug = FALSE,
  buffer_size = 1024 * 32  # Large buffer
))

cat("Generating 5 million values...\n")
values <- generatePRNG(5000000)
cat("Mean:", mean(values), "SD:", sd(values), "\n")

cat("Immediate cleanup after large generation...\n")
cleanupPRNG()

cat("All stress tests completed successfully!\n")