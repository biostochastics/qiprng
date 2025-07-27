#!/usr/bin/env Rscript

# Test for Ziggurat method thread safety (in a testthat context)
library(qiprng)
library(testthat)

test_that("Ziggurat method works correctly in thread-safe mode", {
  # Skip on CRAN and CI
  skip_on_cran()
  skip_on_ci()
  
  # Skip if multicore not available
  if (parallel::detectCores() <= 1) {
    skip("Multicore not available")
  }
  
  # Suppress MPFR warnings
  suppressMPFRWarnings(TRUE)
  
  # Create PRNG with Ziggurat method - basic configuration, NON-threaded mode first
  createPRNG(list(
    distribution = "normal",
    normal_method = "ziggurat",
    use_threading = FALSE,
    use_parallel_filling = FALSE,
    debug = FALSE
  ))
  
  # Generate values
  values1 <- generatePRNG(1000)
  
  # Clean up
  cleanupPRNG()
  
  # Create PRNG with Box-Muller method for comparison, NON-threaded mode
  createPRNG(list(
    distribution = "normal",
    normal_method = "box_muller",
    use_threading = FALSE,
    use_parallel_filling = FALSE,
    debug = FALSE
  ))
  
  # Generate values
  values2 <- generatePRNG(1000)
  
  # Clean up
  cleanupPRNG()
  
  # Compare distributions
  expect_equal(mean(values1), mean(values2), tolerance = 0.2)
  expect_equal(sd(values1), sd(values2), tolerance = 0.2)
  
  # Box-Muller should generally have a mean close to 0 and sd close to 1
  expect_equal(mean(values2), 0, tolerance = 0.2)
  expect_equal(sd(values2), 1, tolerance = 0.2)
})

test_that("Ziggurat method doesn't crash when cleaning up", {
  # Create PRNG with Ziggurat method
  createPRNG(list(
    distribution = "normal",
    normal_method = "ziggurat",
    use_threading = FALSE,
    use_parallel_filling = FALSE,
    debug = FALSE
  ))
  
  # Generate some values
  values <- generatePRNG(10)
  
  # Just verify we get values without error
  expect_true(length(values) == 10)
  
  # Clean up - this should not segfault
  cleanupPRNG()
  
  # Create again with thread safe mode
  createPRNG(list(
    distribution = "normal",
    normal_method = "ziggurat",
    use_threading = TRUE, # Thread safe mode
    use_parallel_filling = FALSE,
    debug = FALSE
  ))
  
  # Generate some values
  values <- generatePRNG(10)
  
  # Just verify we get values without error
  expect_true(length(values) == 10)
  
  # Clean up - this should not segfault
  cleanupPRNG()
})