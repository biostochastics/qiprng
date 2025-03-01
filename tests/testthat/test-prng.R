context("PRNG functionality")

test_that("PRNG initialization works", {
  expect_no_error(setup_prng())
})

test_that("PRNG generates valid numbers", {
  setup_prng()
  nums <- generatePRNG(1000)
  expect_true(all(nums >= 0 & nums <= 1))
})

test_that("PRNG generates expected distributions", {
  # Test uniform distribution
  setup_prng()
  nums <- generatePRNG(1000)
  expect_true(all(nums >= 0 & nums <= 1))
  
  # Test normal distribution
  cfg <- list(distribution = "normal")
  expect_no_error(updatePRNG(cfg))
  nums <- generatePRNG(1000)
  expect_gt(shapiro.test(nums)$p.value, 0.05)
  
  # Test exponential distribution
  cfg <- list(distribution = "exponential", exponential_lambda = 1)
  expect_no_error(updatePRNG(cfg))
  nums <- generatePRNG(1000)
  expect_true(all(nums >= 0))
})

test_that("Configuration updates work", {
  setup_prng()
  
  # Update to normal distribution
  cfg <- list(
    distribution = "normal",
    normal_mean = 0,
    normal_sd = 1
  )
  expect_no_error(updatePRNG(cfg))
  
  nums <- generatePRNG(10000)
  
  # Test mean and standard deviation
  expect_true(abs(mean(nums)) < 0.1)
  expect_true(abs(sd(nums) - 1) < 0.1)
  
  # Test quantiles
  expect_true(mean(abs(nums) <= 1.96) > 0.9)  # ~95% within 2 SD
  expect_true(mean(abs(nums) <= 2.58) > 0.98)  # ~99% within 3 SD
  
  # Test shape
  library(moments)
  skewness <- skewness(nums)
  kurtosis <- kurtosis(nums) - 3  # Excess kurtosis
  
  expect_true(abs(skewness) < 0.2)
  expect_true(abs(kurtosis) < 0.5)
})