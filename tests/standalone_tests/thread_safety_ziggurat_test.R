#!/usr/bin/env Rscript

# Comprehensive thread safety test for the Ziggurat normal implementation
library(qiprng)
library(parallel)

cat("\n===== Testing thread safety of Ziggurat normal method =====\n\n")

# Configure PRNG for normal distribution with threading and Ziggurat method
cfg <- list(
  distribution = "normal",
  normal_method = "ziggurat", # Use the ziggurat method
  use_csv_discriminants = TRUE,
  use_threading = TRUE, # Enable thread-safe mode
  use_parallel_filling = FALSE, # Disable parallel filling
  debug = TRUE # Enable debug output
)

# Create the PRNG
cat("Creating PRNG with Ziggurat in thread-safe mode...\n")
createPRNG(cfg)

# Output current configuration
cat("Configuration applied.\n")

# Function to generate values in a thread
generate_thread_safe <- function(n, thread_id) {
  cat(sprintf("Thread %d starting to generate %d values...\n", thread_id, n))
  tryCatch(
    {
      values <- generatePRNG(n)
      stats <- c(
        mean = mean(values),
        sd = sd(values),
        min = min(values),
        max = max(values)
      )
      cat(sprintf(
        "Thread %d completed. Stats: mean=%.4f, sd=%.4f, min=%.4f, max=%.4f\n",
        thread_id, stats["mean"], stats["sd"], stats["min"], stats["max"]
      ))
      return(list(values = values, stats = stats, success = TRUE))
    },
    error = function(e) {
      cat(sprintf("Thread %d ERROR: %s\n", thread_id, e$message))
      return(list(values = NULL, stats = NULL, success = FALSE, error = e$message))
    }
  )
}

# Test with parallel threads
num_threads <- min(4, parallel::detectCores())
iterations_per_thread <- 5000
cat(sprintf(
  "\nStarting parallel test with %d threads, %d values per thread...\n",
  num_threads, iterations_per_thread
))

# Run parallel test
results <- parallel::mclapply(
  1:num_threads,
  function(i) generate_thread_safe(iterations_per_thread, i),
  mc.cores = num_threads
)

# Check results
success_count <- sum(sapply(results, function(r) r$success))
cat(sprintf("\nCompleted: %d/%d threads succeeded\n", success_count, num_threads))

if (success_count == num_threads) {
  cat("SUCCESS: All threads completed without errors\n")

  # Validate consistency of statistics
  means <- sapply(results, function(r) r$stats["mean"])
  sds <- sapply(results, function(r) r$stats["sd"])

  cat("\nStatistical validation:\n")
  cat(sprintf("Mean values: %s\n", paste(round(means, 4), collapse = ", ")))
  cat(sprintf("Mean of means: %.4f (should be close to 0)\n", mean(means)))
  cat(sprintf("SD values: %s\n", paste(round(sds, 4), collapse = ", ")))
  cat(sprintf("Mean of SDs: %.4f (should be close to 1)\n", mean(sds)))

  # Additional validation
  cat("\nBatch generation test...\n")
  large_batch <- generatePRNG(10000)
  cat(sprintf("Batch mean: %.4f, SD: %.4f\n", mean(large_batch), sd(large_batch)))

  # Test Box-Muller as comparison
  cat("\nSwitching to Box-Muller for comparison...\n")
  cfg$normal_method <- "box_muller"
  updatePRNG(cfg)

  # Run Box-Muller test
  box_muller_results <- parallel::mclapply(
    1:num_threads,
    function(i) generate_thread_safe(iterations_per_thread, i),
    mc.cores = num_threads
  )

  bm_success <- sum(sapply(box_muller_results, function(r) r$success))
  cat(sprintf("Box-Muller test: %d/%d threads succeeded\n", bm_success, num_threads))

  if (bm_success == num_threads) {
    cat("Box-Muller SUCCESS: All threads completed without errors\n")
    # Validate Box-Muller statistics
    bm_means <- sapply(box_muller_results, function(r) r$stats["mean"])
    bm_sds <- sapply(box_muller_results, function(r) r$stats["sd"])
    cat(sprintf("Box-Muller mean of means: %.4f\n", mean(bm_means)))
    cat(sprintf("Box-Muller mean of SDs: %.4f\n", mean(bm_sds)))
  }
} else {
  cat("FAILURE: Some threads encountered errors\n")

  # Print errors
  for (i in 1:length(results)) {
    if (!results[[i]]$success) {
      cat(sprintf("Thread %d error: %s\n", i, results[[i]]$error))
    }
  }
}

# Clean up
cat("\nCleaning up PRNG resources...\n")
cleanup_prng()

cat("\n===== Thread safety test completed! =====\n")
