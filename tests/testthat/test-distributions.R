context("Distribution transformations")

test_that("Normal distribution properties", {
  setup_prng()
  updatePRNG(list(distribution = "normal"))
  nums <- generatePRNG(10000)
  
  # Test mean and standard deviation
  expect_true(abs(mean(nums)) < 0.2)
  expect_true(abs(sd(nums) - 1) < 0.2)
  
  # Test quantiles
  expect_true(mean(abs(nums) <= 1.96) > 0.85)
  expect_true(mean(abs(nums) <= 2.58) > 0.95)
  
  # Test shape
  library(moments)
  skewness <- skewness(nums)
  kurtosis <- kurtosis(nums) - 3  # Excess kurtosis
  
  expect_true(abs(skewness) < 0.3)
  expect_true(abs(kurtosis) < 0.7)
})

test_that("Uniform range properties", {
  setup_prng()
  updatePRNG(list(
    distribution = "uniform_range",
    range_min = -10,
    range_max = 10
  ))
  nums <- generatePRNG(10000)
  
  # Test range
  expect_true(all(nums >= -10 & nums <= 10))
  
  # Test quartiles
  q <- quantile(nums, probs = c(0.25, 0.5, 0.75))
  expect_true(abs(q[2]) < 0.5)  # Median should be close to 0
  expect_true(abs(q[3] + q[1]) < 0.5)  # Quartiles should be symmetric
})

test_that("Exponential distribution properties", {
  setup_prng()
  lambda <- 1
  updatePRNG(list(
    distribution = "exponential",
    exponential_lambda = lambda
  ))
  nums <- generatePRNG(10000)
  
  # Test range and mean
  expect_true(all(nums >= 0))
  expect_true(abs(mean(nums) - 1) < 0.2)
  expect_true(abs(sd(nums) - 1) < 0.3)
  
  # Test distribution shape
  x_grid <- seq(0, 5, by = 0.1)
  emp_cdf <- sapply(x_grid, function(x) mean(nums <= x))
  theo_cdf <- 1 - exp(-x_grid)
  max_diff <- max(abs(emp_cdf - theo_cdf))
  expect_true(max_diff < 0.15)
})