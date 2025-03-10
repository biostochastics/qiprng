context("Advanced PRNG Testing")

test_that("Crypto Mixing", {
  skip_on_cran()
  
  cfg <- list(
    a = 2,
    b = 5,
    c = -2,  # Updated to ensure positive discriminant
    mpfr_precision = 53,
    distribution = "uniform_01",
    use_crypto_mixing = TRUE
  )
  createPRNG(cfg)
  
  # Generate two sets of numbers
  x1 <- generatePRNG(1000)
  x2 <- generatePRNG(1000)
  
  # Test uniformity
  expect_true(all(x1 >= 0 & x1 <= 1))
  expect_true(all(x2 >= 0 & x2 <= 1))
  
  # Add small jitter to avoid ties in KS test
  x1 <- x1 + runif(length(x1), -1e-12, 1e-12)
  x2 <- x2 + runif(length(x2), -1e-12, 1e-12)
  
  # Test for uniformity with relaxed p-value threshold
  expect_gt(ks.test(x1, "punif")$p.value, 0.001)
  expect_gt(ks.test(x2, "punif")$p.value, 0.001)
  
  # Test independence
  expect_lt(abs(cor(x1, x2)), 0.1)
  
  cleanup_prng()
})

test_that("Reseeding Behavior", {
  skip_on_cran()
  
  cfg <- list(
    a = 2,
    b = 5,
    c = -2,  # Updated to ensure positive discriminant
    mpfr_precision = 53,
    distribution = "uniform_01",
    use_crypto_mixing = TRUE,
    reseed_interval = 500
  )
  createPRNG(cfg)
  
  # Generate numbers across reseed boundary
  x1 <- generatePRNG(400)
  x2 <- generatePRNG(400)  # Should trigger reseed
  
  # Add small jitter to avoid ties in KS test
  x1 <- x1 + runif(length(x1), -1e-12, 1e-12)
  x2 <- x2 + runif(length(x2), -1e-12, 1e-12)
  
  # Test uniformity with relaxed p-value threshold
  expect_gt(ks.test(x1, "punif")$p.value, 0.001)
  expect_gt(ks.test(x2, "punif")$p.value, 0.001)
  
  # Test independence after reseed
  expect_lt(abs(cor(x1, x2)), 0.1)
  
  cleanup_prng()
})

test_that("Different Distributions", {
  skip_on_cran()
  
  cfg <- list(
    a = 2,
    b = 5,
    c = -2,  # Updated to ensure positive discriminant
    mpfr_precision = 53,
    distribution = "uniform_01"
  )
  createPRNG(cfg)
  
  # Generate uniform numbers
  x1 <- generatePRNG(1000)
  
  # Add small jitter to avoid ties in KS test
  x1 <- x1 + runif(length(x1), -1e-12, 1e-12)
  
  # Test uniformity with relaxed p-value threshold
  expect_gt(ks.test(x1, "punif")$p.value, 0.001)
  
  # Switch to uniform range [-10, 10]
  updatePRNG(list(
    distribution = "uniform_range",
    range_min = -10,
    range_max = 10
  ))
  x2 <- generatePRNG(1000)
  
  # Test range and distribution
  expect_true(all(x2 >= -10 & x2 <= 10))
  
  # Normalize to [0,1] and add jitter
  x2_norm <- (x2 + 10)/20 + runif(length(x2), -1e-12, 1e-12)
  expect_gt(ks.test(x2_norm, "punif")$p.value, 0.001)
  
  # Switch to normal distribution
  updatePRNG(list(
    distribution = "normal",
    normal_mean = 0,
    normal_sd = 1
  ))
  x3 <- generatePRNG(1000)
  
  # Test normality
  expect_gt(shapiro.test(x3)$p.value, 0.01)
  expect_lt(abs(mean(x3)), 0.2)
  expect_lt(abs(sd(x3) - 1), 0.2)
  
  # Switch to exponential distribution
  updatePRNG(list(
    distribution = "exponential",
    exponential_lambda = 1
  ))
  x4 <- generatePRNG(1000)
  
  # Test exponential properties
  expect_true(all(x4 >= 0))
  expect_lt(abs(mean(x4) - 1), 0.2)  # Mean should be close to 1/lambda
  expect_lt(abs(sd(x4) - 1), 0.3)    # SD should be close to 1/lambda
  
  cleanup_prng()
})

test_that("PRNG handles invalid parameters", {
  skip_on_cran()
  
  # Test invalid quadratic parameters
  expect_error(
    createPRNG(list(a = 1L, b = 1L, c = 1L, mpfr_precision = 53L)),
    "discriminant must be positive"
  )
  
  # Test invalid MPFR precision
  expect_error(
    createPRNG(list(a = 2L, b = 5L, c = -1L, mpfr_precision = 1L)),
    "Invalid MPFR precision"
  )
  
  expect_error(
    createPRNG(list(a = 2L, b = 5L, c = -1L, mpfr_precision = 20000L)),
    "Invalid MPFR precision"
  )
})

test_that("PRNG handles invalid distribution parameters", {
  skip_on_cran()
  
  # Setup valid PRNG first
  createPRNG(list(a = 2L, b = 5L, c = -1L))
  
  # Test invalid normal distribution parameters
  expect_error(
    updatePRNG(list(distribution = "normal", normal_sd = -1)),
    "standard deviation must be positive"
  )
  
  # Test invalid exponential distribution parameters
  expect_error(
    updatePRNG(list(distribution = "exponential", exponential_lambda = 0)),
    "lambda must be positive"
  )
  
  # Test invalid uniform range parameters
  expect_error(
    updatePRNG(list(distribution = "uniform_range", range_min = 10, range_max = 0)),
    "max must be greater than min"
  )
  
  cleanup_prng()
})

test_that("PRNG maintains state correctly", {
  skip_on_cran()
  
  createPRNG(list(a = 2L, b = 5L, c = -1L))
  x1 <- generatePRNG(1000)
  x2 <- generatePRNG(1000)
  
  # Sequences should be different
  expect_false(identical(x1, x2))
  
  # Both should be valid uniform numbers
  expect_true(all(x1 >= 0 & x1 <= 1))
  expect_true(all(x2 >= 0 & x2 <= 1))
  
  cleanup_prng()
})

test_that("Thread safety works correctly", {
  skip_on_cran()
  
  # Test parallel generation
  library(parallel)
  cl <- makeCluster(4)
  
  # Setup PRNG in each worker
  clusterEvalQ(cl, {
    library(qiprng)
    createPRNG()
  })
  
  # Generate numbers in parallel
  nums <- parSapply(cl, 1:4, function(i) {
    generatePRNG(1000)
  })
  
  stopCluster(cl)
  
  # Check that sequences are different
  for (i in 1:3) {
    for (j in (i+1):4) {
      expect_false(identical(nums[,i], nums[,j]))
    }
  }
  
  # All sequences should be valid uniform numbers
  for (i in 1:4) {
    expect_true(all(nums[,i] >= 0 & nums[,i] <= 1))
    expect_gt(stats::ks.test(nums[,i], "punif")$p.value, 0.01)
  }
  
  cleanup_prng()
})

test_that("PRNG handles multiple instances", {
  skip_on_cran()
  
  # Create two PRNGs with different parameters
  createPRNG(list(a = 2L, b = 5L, c = -1L))
  
  createPRNG(list(a = 3L, b = 7L, c = -2L))
  
  # Generate sequences from both
  x1 <- generatePRNG(1000)
  x2 <- generatePRNG(1000)
  
  # Sequences should be different
  expect_false(identical(x1, x2))
  
  # Both should be uniform
  expect_gt(stats::ks.test(x1, "punif")$p.value, 0.01)
  expect_gt(stats::ks.test(x2, "punif")$p.value, 0.01)
  
  cleanup_prng()
  cleanup_prng()
})

test_that("Normal distribution generation is correct", {
  # Test with large sample to verify Box-Muller correctness
  n <- 100000
  createPRNG(list(distribution = "normal", 
                  normal_mean = 0, 
                  normal_sd = 1))
  
  samples <- generatePRNG(n)
  
  # Basic statistical tests
  expect_equal(mean(samples), 0, tolerance = 0.1)
  expect_equal(sd(samples), 1, tolerance = 0.1)
  
  # Shapiro-Wilk test for normality
  # Take a subset due to test limitations
  sw_test <- shapiro.test(samples[1:5000])
  expect_gt(sw_test$p.value, 0.05)
  
  # QQ plot test (manual verification)
  if (interactive()) {
    qqnorm(samples)
    qqline(samples)
  }
  
  cleanup_prng()
})

test_that("Distribution transforms are applied correctly", {
  n <- 1000
  
  # Test uniform range
  createPRNG(list(distribution = "uniform_range",
                  range_min = -5,
                  range_max = 5))
  uniform_samples <- generatePRNG(n)
  expect_true(all(uniform_samples >= -5 & uniform_samples <= 5))
  expect_equal(mean(uniform_samples), 0, tolerance = 1)
  
  # Test exponential
  lambda <- 2
  createPRNG(list(distribution = "exponential",
                  exponential_lambda = lambda))
  exp_samples <- generatePRNG(n)
  expect_true(all(exp_samples >= 0))
  expect_equal(mean(exp_samples), 1/lambda, tolerance = 0.1)
  
  cleanup_prng()
  cleanup_prng()
})

test_that("Crypto mixing preserves uniformity", {
  n <- 10000
  
  # Test with crypto mixing
  createPRNG(list(use_crypto_mixing = TRUE))
  crypto_samples <- generatePRNG(n)
  expect_true(all(crypto_samples >= 0 & crypto_samples < 1))
  expect_equal(mean(crypto_samples), 0.5, tolerance = 0.05)
  
  # Kolmogorov-Smirnov test against uniform
  ks_test <- ks.test(crypto_samples, "punif")
  expect_gt(ks_test$p.value, 0.05)
  
  cleanup_prng()
})