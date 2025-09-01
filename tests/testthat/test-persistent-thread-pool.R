context("Persistent Thread Pool Tests")

library(parallel)
library(qiprng)

# Suppress MPFR warnings for cleaner test output
suppressMPFRWarnings(TRUE)

test_that("Persistent thread pool maintains state between operations", {
  skip_on_cran()
  skip_on_ci()

  # Skip if parallelization not available
  if (parallel::detectCores() <= 1) {
    skip("Multicore not available")
  }

  # Configuration with thread-safe Ziggurat and parallel filling
  cfg <- list(
    distribution = "normal",
    normal_method = "ziggurat",
    buffer_size = 10000,
    use_threading = TRUE,
    use_parallel_filling = TRUE
  )

  # Create PRNG
  createPRNG(cfg)

  # Generate multiple batches and check if speed improves
  # (thread pool initialization cost should occur only once)
  times <- numeric(5)
  means <- numeric(5)
  sds <- numeric(5)

  # Run five batches (first one should include thread pool creation)
  for (i in 1:5) {
    start_time <- Sys.time()
    values <- generatePRNG(10000)
    end_time <- Sys.time()

    times[i] <- as.numeric(difftime(end_time, start_time, units = "secs"))
    means[i] <- mean(values)
    sds[i] <- sd(values)

    cat(sprintf(
      "Batch %d: %.4f seconds (mean=%.4f, sd=%.4f)\n",
      i, times[i], means[i], sds[i]
    ))
  }

  # First batch should typically be slower due to thread pool initialization
  cat("\nTime comparison:\n")
  cat(sprintf("First batch: %.4f seconds\n", times[1]))
  cat(sprintf("Average of subsequent batches: %.4f seconds\n", mean(times[2:5])))

  if (times[1] > mean(times[2:5])) {
    speedup <- times[1] / mean(times[2:5])
    cat(sprintf("Thread pool persistence provides %.2fx speedup\n", speedup))
  } else {
    cat("No speedup observed - thread pool initialization may be fast on this system.\n")
  }

  # All batches should produce proper normal distributions
  for (i in 1:5) {
    expect_true(abs(means[i]) < 0.15, paste0("Batch ", i, " mean should be close to 0"))
    expect_true(abs(sds[i] - 1.0) < 0.15, paste0("Batch ", i, " SD should be close to 1"))
  }

  # Clean up
  cleanup_prng()
})

test_that("Persistent thread pool can handle reseed operations", {
  skip_on_cran()
  skip_on_ci()

  # Create PRNG with threading enabled
  cfg <- list(
    distribution = "normal",
    normal_method = "ziggurat",
    use_threading = TRUE,
    use_parallel_filling = TRUE
  )

  createPRNG(cfg)

  # Generate initial values
  values1 <- generatePRNG(1000)

  # Explicitly reseed the PRNG
  reseedPRNG()

  # Generate new values after reseed
  values2 <- generatePRNG(1000)

  # Verify that the distributions are different after reseeding
  # (correlation should be low)
  correlation <- cor(values1, values2)
  cat(sprintf("Correlation between pre-reseed and post-reseed: %.4f\n", correlation))

  # Verify thread pool still works after reseeding
  expect_true(abs(mean(values1)) < 0.2, "Pre-reseed mean should be close to 0")
  expect_true(abs(sd(values1) - 1.0) < 0.2, "Pre-reseed SD should be close to 1")
  expect_true(abs(mean(values2)) < 0.2, "Post-reseed mean should be close to 0")
  expect_true(abs(sd(values2) - 1.0) < 0.2, "Post-reseed SD should be close to 1")

  # Correlation should be low if reseeding worked properly with thread pool
  expect_true(abs(correlation) < 0.3, "Correlation should be low after reseeding")

  # Clean up
  cleanup_prng()
})
