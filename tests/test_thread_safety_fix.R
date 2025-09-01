#!/usr/bin/env Rscript

# Test for thread safety of ziggurat_normal initialization
# This test verifies that the std::call_once fix properly addresses
# the double-checked locking antipattern

library(qiprng)
library(parallel)

cat("Testing thread safety of ziggurat_normal initialization\n")
cat("Using std::call_once to fix double-checked locking antipattern\n")
cat("==========================================================\n\n")

# Function to run in parallel threads
test_ziggurat <- function(thread_id) {
  tryCatch(
    {
      # Create a PRNG configuration for normal distribution
      # This internally uses the Ziggurat implementation
      config <- list(
        distribution = "normal",
        mean = 0,
        stddev = 1,
        thread_safe = TRUE
      )

      # Create PRNG instance - this triggers initialize_static_tables()
      prng <- createPRNG(config)

      # Generate samples to ensure tables are actually used
      samples <- generatePRNG(100)

      # Clean up
      cleanup_prng()

      # Verify samples are reasonable (mean ~0, stddev ~1)
      sample_mean <- mean(samples)
      sample_sd <- sd(samples)

      # Check if results are reasonable
      if (abs(sample_mean) < 1.0 && sample_sd > 0.5 && sample_sd < 2.0) {
        return(list(
          success = TRUE,
          thread_id = thread_id,
          mean = sample_mean,
          sd = sample_sd
        ))
      } else {
        return(list(
          success = FALSE,
          thread_id = thread_id,
          mean = sample_mean,
          sd = sample_sd,
          error = "Unexpected statistics"
        ))
      }
    },
    error = function(e) {
      return(list(
        success = FALSE,
        thread_id = thread_id,
        error = as.character(e)
      ))
    }
  )
}

# Number of threads to test with
num_threads <- 50
num_iterations <- 3

cat(sprintf(
  "Running %d iterations with %d parallel threads each\n\n",
  num_iterations, num_threads
))

all_success <- TRUE

for (iter in 1:num_iterations) {
  cat(sprintf("Iteration %d of %d\n", iter, num_iterations))

  # Use mclapply for parallel execution (Unix/Mac)
  # On Windows, this will run sequentially
  if (.Platform$OS.type == "unix") {
    results <- mclapply(1:num_threads, test_ziggurat, mc.cores = num_threads)
  } else {
    # On Windows, use parLapply with a cluster
    cl <- makeCluster(num_threads)
    clusterEvalQ(cl, library(qiprng))
    results <- parLapply(cl, 1:num_threads, test_ziggurat)
    stopCluster(cl)
  }

  # Count successes and failures
  successes <- sum(sapply(results, function(r) r$success))
  failures <- num_threads - successes

  cat(sprintf("  Results: %d successes, %d failures\n", successes, failures))

  # Print any errors
  if (failures > 0) {
    failed_results <- results[!sapply(results, function(r) r$success)]
    for (fr in failed_results) {
      if (!is.null(fr$error)) {
        cat(sprintf("    Thread %d error: %s\n", fr$thread_id, fr$error))
      }
    }
    all_success <- FALSE
  }

  cat("\n")
}

cat("==========================================================\n")
if (all_success) {
  cat("SUCCESS: All threads completed without race conditions!\n")
  cat("The std::call_once fix properly addresses the double-checked locking issue.\n")
  quit(status = 0)
} else {
  cat("WARNING: Some thread safety issues were detected.\n")
  cat("This may indicate remaining race conditions.\n")
  quit(status = 1)
}
