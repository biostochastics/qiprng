context("New Distribution Tests")

library(qiprng)

test_that("Bernoulli distribution properties", {
  # skip_on_cran()

  # Test with p = 0.3
  cfg <- list(
    a = 2,
    b = 5,
    c = -2,
    mpfr_precision = 53,
    distribution = "bernoulli",
    bernoulli_p = 0.3
  )
  createPRNG(cfg)

  n <- 10000
  nums <- generatePRNG(n)

  # Check values are binary
  expect_true(all(nums %in% c(0, 1)))

  # Check mean (should be close to p)
  sample_mean <- mean(nums)
  expect_equal(sample_mean, 0.3, tolerance = 0.02)

  # Check variance (should be close to p*(1-p) = 0.21)
  sample_var <- var(nums)
  expect_equal(sample_var, 0.21, tolerance = 0.02)

  cleanup_prng()
})

test_that("Binomial distribution properties", {
  # skip_on_cran()

  # Test with n=10, p=0.5
  cfg <- list(
    a = 2,
    b = 5,
    c = -2,
    mpfr_precision = 53,
    distribution = "binomial",
    binomial_n = 10,
    binomial_p = 0.5
  )
  createPRNG(cfg)

  n <- 10000
  nums <- generatePRNG(n)

  # Check range
  expect_true(all(nums >= 0 & nums <= 10))
  expect_true(all(nums == floor(nums))) # Should be integers

  # Check mean (should be n*p = 5)
  expect_equal(mean(nums), 5, tolerance = 0.1)

  # Check variance (should be n*p*(1-p) = 2.5)
  expect_equal(var(nums), 2.5, tolerance = 0.2)

  # Compare with R's rbinom using KS test
  r_binom <- rbinom(n, 10, 0.5)
  ks_test <- ks.test(nums, r_binom)
  expect_gt(ks_test$p.value, 0.01)

  cleanup_prng()
})

test_that("Log-Normal distribution properties", {
  # skip_on_cran()

  # Test with mu=0, sigma=1
  cfg <- list(
    a = 2,
    b = 5,
    c = -2,
    mpfr_precision = 53,
    distribution = "lognormal",
    lognormal_mu = 0,
    lognormal_sigma = 1
  )
  createPRNG(cfg)

  n <- 5000
  nums <- generatePRNG(n)

  # Check all values are positive
  expect_true(all(nums > 0))

  # Check mean (should be exp(mu + sigma^2/2) = exp(0.5) ≈ 1.649)
  expected_mean <- exp(0.5)
  expect_equal(mean(nums), expected_mean, tolerance = 0.2)

  # Check log transform gives normal distribution
  log_nums <- log(nums)
  sw_test <- shapiro.test(log_nums[1:1000])
  expect_gt(sw_test$p.value, 0.001) # Relaxed threshold for random variations

  cleanup_prng()
})

test_that("Weibull distribution properties", {
  # skip_on_cran()

  # Test with shape=2, scale=1 (Rayleigh distribution)
  cfg <- list(
    a = 2,
    b = 5,
    c = -2,
    mpfr_precision = 53,
    distribution = "weibull",
    weibull_shape = 2,
    weibull_scale = 1
  )
  createPRNG(cfg)

  n <- 5000
  nums <- generatePRNG(n)

  # Check all values are positive
  expect_true(all(nums >= 0))

  # Check mean (should be scale * gamma(1 + 1/shape) = 1 * gamma(1.5) ≈ 0.886)
  expected_mean <- 1 * gamma(1.5)
  expect_equal(mean(nums), expected_mean, tolerance = 0.1)

  # Compare with R's rweibull
  r_weibull <- rweibull(n, shape = 2, scale = 1)
  ks_test <- ks.test(nums, r_weibull)
  expect_gt(ks_test$p.value, 0.01)

  cleanup_prng()
})

test_that("Chi-Squared distribution properties", {
  # skip_on_cran()

  # Test with df=4
  cfg <- list(
    a = 2,
    b = 5,
    c = -2,
    mpfr_precision = 53,
    distribution = "chisquared",
    chisquared_df = 4
  )
  createPRNG(cfg)

  n <- 5000
  nums <- generatePRNG(n)

  # Check all values are positive
  expect_true(all(nums >= 0))

  # Check mean (should be df = 4)
  expect_equal(mean(nums), 4, tolerance = 0.2)

  # Check variance (should be 2*df = 8)
  expect_equal(var(nums), 8, tolerance = 1)

  # Compare with R's rchisq
  r_chisq <- rchisq(n, df = 4)
  ks_test <- ks.test(nums, r_chisq)
  expect_gt(ks_test$p.value, 0.01)

  cleanup_prng()
})

test_that("Student's t distribution properties", {
  # skip_on_cran()

  # Test with df=5
  cfg <- list(
    a = 2,
    b = 5,
    c = -2,
    mpfr_precision = 53,
    distribution = "student_t",
    student_t_df = 5
  )
  createPRNG(cfg)

  n <- 5000
  nums <- generatePRNG(n)

  # Check mean (should be 0 for df > 1)
  expect_equal(mean(nums), 0, tolerance = 0.1)

  # Check variance (should be df/(df-2) = 5/3 ≈ 1.667 for df > 2)
  expected_var <- 5 / 3
  expect_equal(var(nums), expected_var, tolerance = 0.3)

  # Compare with R's rt
  r_t <- rt(n, df = 5)
  ks_test <- ks.test(nums, r_t)
  expect_gt(ks_test$p.value, 0.01)

  cleanup_prng()
})

test_that("Negative Binomial distribution properties", {
  # skip_on_cran()

  # Test with r=3, p=0.4
  cfg <- list(
    a = 2,
    b = 5,
    c = -2,
    mpfr_precision = 53,
    distribution = "negative_binomial",
    negative_binomial_r = 3,
    negative_binomial_p = 0.4
  )
  createPRNG(cfg)

  n <- 5000
  nums <- generatePRNG(n)

  # Check all values are non-negative integers
  expect_true(all(nums >= 0))
  expect_true(all(nums == floor(nums)))

  # Check mean (should be r*(1-p)/p = 3*0.6/0.4 = 4.5)
  expected_mean <- 3 * 0.6 / 0.4
  expect_equal(mean(nums), expected_mean, tolerance = 0.3)

  # Check variance (should be r*(1-p)/(p^2) = 3*0.6/(0.16) = 11.25)
  expected_var <- 3 * 0.6 / (0.4 * 0.4)
  expect_equal(var(nums), expected_var, tolerance = 2)

  cleanup_prng()
})

test_that("Distribution edge cases", {
  # skip_on_cran()

  # Test Bernoulli with p=0
  cfg <- list(
    a = 2, b = 5, c = -2,
    mpfr_precision = 53,
    distribution = "bernoulli",
    bernoulli_p = 0
  )
  createPRNG(cfg)
  nums <- generatePRNG(100)
  expect_true(all(nums == 0))
  cleanup_prng()

  # Test Bernoulli with p=1
  cfg$bernoulli_p <- 1
  createPRNG(cfg)
  nums <- generatePRNG(100)
  expect_true(all(nums == 1))
  cleanup_prng()

  # Test Binomial with n=0
  cfg <- list(
    a = 2, b = 5, c = -2,
    mpfr_precision = 53,
    distribution = "binomial",
    binomial_n = 0,
    binomial_p = 0.5
  )
  createPRNG(cfg)
  nums <- generatePRNG(100)
  expect_true(all(nums == 0))
  cleanup_prng()
})

test_that("Normal approximations work correctly", {
  # skip_on_cran()

  # Test binomial with large n (should use normal approximation)
  cfg <- list(
    a = 2, b = 5, c = -2,
    mpfr_precision = 53,
    distribution = "binomial",
    binomial_n = 100,
    binomial_p = 0.5
  )
  createPRNG(cfg)

  n <- 5000
  nums <- generatePRNG(n)

  # Should be approximately normal with mean=50, sd=5
  expect_equal(mean(nums), 50, tolerance = 0.5)
  expect_equal(sd(nums), 5, tolerance = 0.3)

  # Values should still be bounded by [0, 100]
  expect_true(all(nums >= 0 & nums <= 100))

  cleanup_prng()

  # Test chi-squared with large df (should use normal approximation)
  cfg <- list(
    a = 2, b = 5, c = -2,
    mpfr_precision = 53,
    distribution = "chisquared",
    chisquared_df = 150
  )
  createPRNG(cfg)

  nums <- generatePRNG(n)

  # Should be approximately normal with mean=150, sd=sqrt(300)
  expect_equal(mean(nums), 150, tolerance = 2)
  expect_equal(sd(nums), sqrt(300), tolerance = 2)

  cleanup_prng()
})
