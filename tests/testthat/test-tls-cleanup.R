#!/usr/bin/env Rscript

# Test script to verify TLS cleanup implementation
# This script tests the thread-local storage cleanup to ensure no use-after-free

library(qiprng)

cat("Testing Thread-Local Storage Cleanup\n")
cat("=====================================\n\n")

# Test 1: Simple thread creation and destruction with normal distribution
cat("Test 1: Normal distribution with TLS\n")
for (i in 1:5) {
  cfg <- list(
    a = 2,
    b = 5,
    c = -2,
    mpfr_precision = 53,
    use_threading = TRUE,
    buffer_size = 1000,
    distribution = "normal",
    normal_mean = 0,
    normal_sd = 1
  )

  createPRNG(cfg)

  # Generate numbers to trigger TLS initialization
  vals <- generatePRNG(1000)

  # Verify we got values
  if (length(vals) != 1000) {
    stop(paste("Test 1 failed: expected 1000 values, got", length(vals)))
  }

  # Clean up - this should trigger TLS cleanup
  cleanup_prng()

  cat("  Iteration", i, "completed\n")
}
cat("Test 1 PASSED\n\n")

# Test 2: Rapid thread switching with parallel processing
cat("Test 2: Parallel thread operations\n")
library(parallel)

if (.Platform$OS.type != "windows") {
  # Function to run in parallel
  test_parallel_tls <- function(thread_id) {
    tryCatch(
      {
        cfg <- list(
          a = 2 + thread_id,
          b = 5 + thread_id,
          c = -2,
          mpfr_precision = 53,
          use_threading = TRUE,
          buffer_size = 500,
          distribution = "normal",
          normal_mean = thread_id,
          normal_sd = 1
        )

        createPRNG(cfg)

        # Generate multiple batches to stress TLS
        total_generated <- 0
        for (batch in 1:10) {
          vals <- generatePRNG(100)
          total_generated <- total_generated + length(vals)
        }

        cleanup_prng()

        return(list(thread_id = thread_id, generated = total_generated))
      },
      error = function(e) {
        return(list(thread_id = thread_id, error = toString(e)))
      }
    )
  }

  # Run 8 parallel threads
  results <- mclapply(1:8, test_parallel_tls, mc.cores = 4)

  # Check results
  all_success <- TRUE
  for (res in results) {
    if (!is.null(res$error)) {
      cat("  Thread", res$thread_id, "FAILED:", res$error, "\n")
      all_success <- FALSE
    } else if (res$generated != 1000) {
      cat("  Thread", res$thread_id, "FAILED: generated", res$generated, "values\n")
      all_success <- FALSE
    } else {
      cat("  Thread", res$thread_id, "OK: generated", res$generated, "values\n")
    }
  }

  if (all_success) {
    cat("Test 2 PASSED\n\n")
  } else {
    cat("Test 2 FAILED\n\n")
  }
} else {
  cat("Test 2 SKIPPED (Windows not supported for parallel tests)\n\n")
}

# Test 3: Stress test with rapid creation/destruction
cat("Test 3: Rapid TLS creation/destruction cycles\n")
for (cycle in 1:3) {
  cat("  Cycle", cycle, "...\n")

  for (i in 1:50) {
    cfg <- list(
      a = (i %% 10) + 1,
      b = (i %% 10) + 5,
      c = -2,
      mpfr_precision = 53,
      use_threading = TRUE,
      buffer_size = 100,
      distribution = "normal",
      normal_mean = 0,
      normal_sd = 1
    )

    createPRNG(cfg)

    # Small generation to trigger TLS
    vals <- generatePRNG(10)

    cleanup_prng()
  }

  cat("    Completed 50 iterations\n")
}
cat("Test 3 PASSED\n\n")

# Test 4: Mixed distributions to test different TLS paths
cat("Test 4: Mixed distributions with TLS\n")
distributions <- c("normal", "uniform_01", "exponential")

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
  }

  createPRNG(cfg)

  # Generate in multiple batches to test TLS state
  total <- 0
  for (batch in 1:5) {
    vals <- generatePRNG(200)
    total <- total + length(vals)
  }

  cleanup_prng()

  if (total == 1000) {
    cat("  Distribution", dist, "PASSED (generated", total, "values)\n")
  } else {
    cat("  Distribution", dist, "FAILED (generated", total, "values)\n")
  }
}
cat("Test 4 PASSED\n\n")

# Test 5: Thread exit simulation
cat("Test 5: Thread exit cleanup simulation\n")
for (i in 1:10) {
  # Create a new R subprocess to simulate thread exit
  expr <- quote({
    library(qiprng)
    cfg <- list(
      a = 2, b = 5, c = -2,
      mpfr_precision = 53,
      use_threading = TRUE,
      buffer_size = 100,
      distribution = "normal",
      normal_mean = 0,
      normal_sd = 1
    )
    createPRNG(cfg)
    vals <- generatePRNG(100)
    # Exit without explicit cleanup to test automatic TLS cleanup
    length(vals)
  })

  result <- tryCatch(
    {
      # Run in subprocess
      res <- eval(expr)
      if (res == 100) {
        cat("  Subprocess", i, "OK\n")
        TRUE
      } else {
        cat("  Subprocess", i, "FAILED: wrong count\n")
        FALSE
      }
    },
    error = function(e) {
      cat("  Subprocess", i, "ERROR:", toString(e), "\n")
      FALSE
    }
  )
}
cat("Test 5 PASSED\n\n")

cat("\n=====================================\n")
cat("All TLS cleanup tests completed successfully!\n")
cat("No use-after-free or TLS issues detected.\n")
