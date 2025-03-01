context("Basic PRNG functionality")

# Make sure we have the qiprng package loaded
library(qiprng)

test_that("PRNG creation works", {
  # Create with default config
  createPRNG()
  
  # Should not throw error
  expect_no_error(createPRNG())
  
  # Create with custom config - ensure b^2 - 4ac > 0
  cfg_default <- list(a = 2L, b = 5L, c = -2L)  # b^2 - 4ac = 25 - (-16) = 41 > 0
  expect_no_error(createPRNG(cfg_default))
  
  # Create with custom config
  cfg <- list(a = 3L, b = 4L, c = -2L, distribution = "uniform_01")  # b^2 - 4ac = 16 - (-24) = 40 > 0
  expect_no_error(createPRNG(cfg))
})

test_that("PRNG generation works", {
  # Create PRNG with valid parameters
  cfg <- list(a = 2L, b = 5L, c = -2L)  # b^2 - 4ac = 25 - (-16) = 41 > 0
  createPRNG(cfg)
  
  # Generate some numbers
  n <- 100
  x <- generatePRNG(n)
  
  # Check output
  expect_equal(length(x), n)
  expect_true(is.numeric(x))
  expect_true(all(x >= 0 & x <= 1))  # Default is uniform_01
})

test_that("PRNG update works", {
  # Create PRNG with valid parameters
  cfg <- list(a = 2L, b = 5L, c = -2L)  # b^2 - 4ac = 25 - (-16) = 41 > 0
  createPRNG(cfg)
  
  # Update config
  cfg_update <- list(distribution = "normal", normal_mean = 5, normal_sd = 2)
  expect_no_error(updatePRNG(cfg_update))
  
  # Generate some numbers with new config
  n <- 1000
  x <- generatePRNG(n)
  
  # Check that output follows normal distribution
  expect_equal(length(x), n)
  expect_true(is.numeric(x))
  
  # Mean should be approximately 5
  expect_true(abs(mean(x) - 5) < 0.5)
})

test_that("Statistical tests work", {
  # Create PRNG with valid parameters
  cfg <- list(a = 2L, b = 5L, c = -2L)  # b^2 - 4ac = 25 - (-16) = 41 > 0
  createPRNG(cfg)
  
  # Run tests
  results <- test_qiprng(n = 1000)
  
  # Check results structure
  expect_true(is.list(results))
  expect_true("ks_pvalue" %in% names(results))
  expect_true("runs_z_score" %in% names(results))
  expect_true("spectral_sd" %in% names(results))
})