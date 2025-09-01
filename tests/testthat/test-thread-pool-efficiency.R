context("Thread Pool Efficiency Tests")

library(parallel)
library(qiprng)

# Suppress MPFR warnings for cleaner test output
suppressMPFRWarnings(TRUE)

test_that("Thread pool optimization improves parallel performance", {
  skip_on_cran()
  skip_on_ci()

  # Skip if parallelization not available
  if (parallel::detectCores() <= 1) {
    skip("Multicore not available")
  }

  # Test parameters
  n_values <- 50000 # Number of random values to generate

  # Function to measure generation time
  measure_gen_time <- function(use_threading, use_parallel_filling) {
    # Create PRNG with specified settings
    cfg <- list(
      distribution = "normal",
      normal_method = "ziggurat",
      buffer_size = 10000, # Large buffer to see the effect
      use_threading = use_threading,
      use_parallel_filling = use_parallel_filling
    )

    # Create PRNG
    createPRNG(cfg)

    # Measure time for generation
    start_time <- Sys.time()
    values <- generatePRNG(n_values)
    end_time <- Sys.time()

    # Clean up
    cleanup_prng()

    # Return time taken and basic stats
    list(
      time = as.numeric(difftime(end_time, start_time, units = "secs")),
      mean = mean(values),
      sd = sd(values)
    )
  }

  # Run tests with different configurations
  result1 <- measure_gen_time(FALSE, FALSE) # No threading, no parallel filling
  result2 <- measure_gen_time(TRUE, FALSE) # Threading only
  result3 <- measure_gen_time(TRUE, TRUE) # Threading with parallel filling

  # Display results
  cat(sprintf(
    "No threading:          %.4f seconds (mean=%.4f, sd=%.4f)\n",
    result1$time, result1$mean, result1$sd
  ))
  cat(sprintf(
    "Threading only:        %.4f seconds (mean=%.4f, sd=%.4f)\n",
    result2$time, result2$mean, result2$sd
  ))
  cat(sprintf(
    "Threading + parallel:  %.4f seconds (mean=%.4f, sd=%.4f)\n",
    result3$time, result3$mean, result3$sd
  ))

  # Basic verification (don't fail test, just report)
  cat("\nPerformance comparison:\n")

  if (result2$time < result1$time) {
    speedup <- result1$time / result2$time
    cat(sprintf("- Thread optimization provides %.2fx speedup\n", speedup))
  } else {
    cat("- Thread optimization doesn't improve performance in this test\n")
  }

  if (result3$time < result2$time) {
    speedup <- result2$time / result3$time
    cat(sprintf("- Parallel filling provides additional %.2fx speedup\n", speedup))
  } else {
    cat("- Parallel filling doesn't improve performance in this test\n")
  }

  # Ensure all implementations produce proper normal distributions
  expect_true(abs(result1$mean) < 0.1, "Mean should be close to 0")
  expect_true(abs(result1$sd - 1.0) < 0.1, "SD should be close to 1")
  expect_true(abs(result2$mean) < 0.1, "Mean should be close to 0 with threading")
  expect_true(abs(result2$sd - 1.0) < 0.1, "SD should be close to 1 with threading")
  expect_true(abs(result3$mean) < 0.1, "Mean should be close to 0 with parallel filling")
  expect_true(abs(result3$sd - 1.0) < 0.1, "SD should be close to 1 with parallel filling")
})

test_that("Thread-safe Ziggurat comparison with Box-Muller", {
  skip_on_cran()
  skip_on_ci()

  # Skip if parallelization not available
  if (parallel::detectCores() <= 1) {
    skip("Multicore not available")
  }

  # Test parameters
  n_values <- 50000

  # Function to measure generation time with different methods
  measure_method_time <- function(method) {
    # Create PRNG with specified settings
    cfg <- list(
      distribution = "normal",
      normal_method = method, # "ziggurat" or "box_muller"
      buffer_size = 10000,
      use_threading = TRUE, # Always use threading for this test
      use_parallel_filling = FALSE
    )

    # Create PRNG
    createPRNG(cfg)

    # Measure time for generation
    start_time <- Sys.time()
    values <- generatePRNG(n_values)
    end_time <- Sys.time()

    # Clean up
    cleanup_prng()

    # Return time taken and basic stats
    list(
      time = as.numeric(difftime(end_time, start_time, units = "secs")),
      mean = mean(values),
      sd = sd(values)
    )
  }

  # Run tests with different methods
  ziggurat_result <- measure_method_time("ziggurat")
  boxmuller_result <- measure_method_time("box_muller")

  # Display results
  cat(sprintf(
    "Ziggurat method:   %.4f seconds (mean=%.4f, sd=%.4f)\n",
    ziggurat_result$time, ziggurat_result$mean, ziggurat_result$sd
  ))
  cat(sprintf(
    "Box-Muller method: %.4f seconds (mean=%.4f, sd=%.4f)\n",
    boxmuller_result$time, boxmuller_result$mean, boxmuller_result$sd
  ))

  # Comparison
  if (ziggurat_result$time < boxmuller_result$time) {
    speedup <- boxmuller_result$time / ziggurat_result$time
    cat(sprintf("\nZiggurat is %.2fx faster than Box-Muller in threaded mode\n", speedup))
  } else {
    speedup <- ziggurat_result$time / boxmuller_result$time
    cat(sprintf("\nBox-Muller is %.2fx faster than Ziggurat in threaded mode\n", speedup))
  }

  # Both methods should produce proper normal distributions
  expect_true(abs(ziggurat_result$mean) < 0.1, "Ziggurat mean should be close to 0")
  expect_true(abs(ziggurat_result$sd - 1.0) < 0.1, "Ziggurat SD should be close to 1")
  expect_true(abs(boxmuller_result$mean) < 0.1, "Box-Muller mean should be close to 0")
  expect_true(abs(boxmuller_result$sd - 1.0) < 0.1, "Box-Muller SD should be close to 1")
})
