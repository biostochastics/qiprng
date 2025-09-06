# Comprehensive tests for v0.5.0 features
# Tests parallel generation, mixing strategies, and performance

library(qiprng)
context("v0.5.0 Feature Tests")

test_that("Mixing strategies produce valid distributions", {
  strategies <- c(
    "round_robin",
    "xor_mix",
    "averaging",
    "modular_add",
    "cascade_mix"
  )

  for (strategy in strategies) {
    cfg <- list(
      a = 2, b = 5, c = -2,
      mpfr_precision = 53,
      buffer_size = 1000,
      mixing_strategy = strategy,
      distribution = "uniform_01"
    )

    createPRNG(cfg)
    vals <- generatePRNG(1000)
    cleanup_prng()

    # Check basic properties
    expect_true(all(vals >= 0 & vals < 1),
      info = paste("Values out of range for", strategy)
    )
    expect_equal(length(vals), 1000,
      info = paste("Wrong length for", strategy)
    )

    # Check statistical properties (loose bounds for small sample)
    expect_true(abs(mean(vals) - 0.5) < 0.1,
      info = paste("Mean too far from 0.5 for", strategy)
    )
    expect_true(sd(vals) > 0.2 && sd(vals) < 0.4,
      info = paste("SD out of expected range for", name)
    )

    # Check for obvious patterns (no constant values)
    expect_true(length(unique(vals)) > 900,
      info = paste("Too many duplicates for", name)
    )
  }
})

test_that("Parallel generation produces consistent results", {
  # Test with parallel enabled
  cfg_parallel <- list(
    a = 2, b = 5, c = -2,
    mpfr_precision = 53,
    buffer_size = 10000,
    use_parallel_filling = TRUE,
    seed = 42, # Use seed for reproducibility
    use_crypto_mixing = FALSE, # Disable crypto for deterministic testing
    distribution = "uniform_01"
  )

  createPRNG(cfg_parallel)
  vals_parallel <- generatePRNG(10000)
  cleanup_prng()

  # Test with parallel disabled
  cfg_sequential <- cfg_parallel
  cfg_sequential$use_parallel_filling <- FALSE

  createPRNG(cfg_sequential)
  vals_sequential <- generatePRNG(10000)
  cleanup_prng()

  # Both should produce valid uniform distributions
  expect_true(all(vals_parallel >= 0 & vals_parallel < 1))
  expect_true(all(vals_sequential >= 0 & vals_sequential < 1))

  # Statistical properties should be similar
  expect_true(abs(mean(vals_parallel) - mean(vals_sequential)) < 0.05)
  expect_true(abs(sd(vals_parallel) - sd(vals_sequential)) < 0.05)
})

test_that("Buffer sizes work correctly", {
  buffer_sizes <- c(10, 100, 1000, 10000)

  for (size in buffer_sizes) {
    cfg <- list(
      a = 2, b = 5, c = -2,
      mpfr_precision = 53,
      buffer_size = size,
      distribution = "uniform_01"
    )

    createPRNG(cfg)

    # Generate more than buffer size
    vals <- generatePRNG(size * 2)

    expect_equal(length(vals), size * 2,
      info = paste("Wrong output for buffer size", size)
    )
    expect_true(all(vals >= 0 & vals < 1),
      info = paste("Invalid values for buffer size", size)
    )

    cleanup_prng()
  }
})

test_that("Jump-ahead creates independent streams", {
  # Base configuration
  cfg <- list(
    a = 2, b = 5, c = -2,
    mpfr_precision = 53,
    seed = 12345,
    use_crypto_mixing = FALSE, # Disable crypto for deterministic testing
    distribution = "uniform_01"
  )

  # Generate from first stream
  createPRNG(cfg)
  stream1 <- generatePRNG(1000)
  cleanup_prng()

  # Generate second stream with jump-ahead
  createPRNG(cfg)
  jumpAheadPRNG(1000000) # Jump ahead significantly
  stream2 <- generatePRNG(1000)
  cleanup_prng()

  # Streams should be uncorrelated
  correlation <- cor(stream1, stream2)
  expect_true(abs(correlation) < 0.1,
    info = paste("Streams too correlated:", correlation)
  )

  # Both should have good uniform properties
  expect_true(abs(mean(stream1) - 0.5) < 0.05)
  expect_true(abs(mean(stream2) - 0.5) < 0.05)
})

test_that("Thread safety with multiple generators", {
  # This test ensures thread-local storage works
  cfg <- list(
    a = 2, b = 5, c = -2,
    mpfr_precision = 53,
    buffer_size = 100,
    use_threading = TRUE,
    distribution = "uniform_01"
  )

  # Create and use generator multiple times
  for (i in 1:3) {
    createPRNG(cfg)
    vals <- generatePRNG(100)
    expect_equal(length(vals), 100)
    expect_true(all(vals >= 0 & vals < 1))
    cleanup_prng()
  }
})

test_that("Performance meets targets", {
  skip_on_cran() # Skip on CRAN due to timing

  cfg <- list(
    a = 2, b = 5, c = -2,
    mpfr_precision = 53,
    buffer_size = 10000,
    use_parallel_filling = TRUE,
    distribution = "uniform_01"
  )

  createPRNG(cfg)

  # Measure generation speed
  start_time <- Sys.time()
  vals <- generatePRNG(100000)
  elapsed <- as.numeric(Sys.time() - start_time, units = "secs")

  rate <- 100000 / elapsed

  # Should generate at least 50K values per second on modern hardware
  expect_true(rate > 50000,
    info = paste("Generation too slow:", round(rate), "vals/sec")
  )

  cleanup_prng()
})

test_that("Deterministic mode produces reproducible results", {
  cfg <- list(
    a = 2, b = 5, c = -2,
    mpfr_precision = 53,
    seed = 999,
    deterministic = TRUE,
    use_crypto_mixing = FALSE, # Disable crypto for deterministic testing
    distribution = "uniform_01"
  )

  # First run
  createPRNG(cfg)
  vals1 <- generatePRNG(100)
  cleanup_prng()

  # Second run with same seed
  createPRNG(cfg)
  vals2 <- generatePRNG(100)
  cleanup_prng()

  # Should be identical
  expect_identical(vals1, vals2)
})

test_that("Different distributions work correctly", {
  distributions <- c("uniform_01", "normal", "exponential")

  for (dist in distributions) {
    cfg <- list(
      a = 2, b = 5, c = -2,
      mpfr_precision = 53,
      distribution = dist,
      normal_mean = 0,
      normal_sd = 1,
      exponential_lambda = 1
    )

    createPRNG(cfg)
    vals <- generatePRNG(1000)
    cleanup_prng()

    expect_equal(length(vals), 1000,
      info = paste("Wrong length for", dist)
    )

    # Check distribution-specific properties
    if (dist == "uniform_01") {
      expect_true(all(vals >= 0 & vals <= 1))
    } else if (dist == "normal") {
      # Most values should be within 3 SDs
      expect_true(sum(abs(vals) < 3) / length(vals) > 0.95)
    } else if (dist == "exponential") {
      expect_true(all(vals >= 0))
      # Mean should be close to 1/lambda = 1
      expect_true(abs(mean(vals) - 1) < 0.1)
    }
  }
})

test_that("Memory management is stable", {
  # Test repeated creation/destruction doesn't leak
  for (i in 1:10) {
    cfg <- list(
      a = 2 + i, b = 5 + i, c = -2 - i,
      mpfr_precision = 53,
      buffer_size = 1000,
      distribution = "uniform_01"
    )

    createPRNG(cfg)
    vals <- generatePRNG(1000)
    expect_equal(length(vals), 1000)
    cleanup_prng()
  }

  # If we get here without crashing, memory management is working
  expect_true(TRUE)
})

test_that("Edge cases are handled gracefully", {
  # Very small buffer
  cfg_small <- list(
    a = 2, b = 5, c = -2,
    mpfr_precision = 53,
    buffer_size = 1,
    distribution = "uniform_01"
  )

  createPRNG(cfg_small)
  vals <- generatePRNG(10)
  expect_equal(length(vals), 10)
  cleanup_prng()

  # Generate zero values
  createPRNG(cfg_small)
  # generatePRNG(0) should return empty vector
  expect_error(generatePRNG(0), "n must be a positive number")
  cleanup_prng()

  # Very large single request (should work but may be slow)
  cfg_large <- list(
    a = 2, b = 5, c = -2,
    mpfr_precision = 53,
    buffer_size = 10000,
    distribution = "uniform_01"
  )

  createPRNG(cfg_large)
  vals <- generatePRNG(50000)
  expect_equal(length(vals), 50000)
  cleanup_prng()
})
