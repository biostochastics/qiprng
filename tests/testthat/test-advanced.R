context("Advanced PRNG Testing")

test_that("Crypto Mixing", {
  skip_on_cran()
  
  # Test with crypto mixing
  prng <- createPRNG(list(
    a = 2L,
    b = 5L,
    c = -1L,
    mpfr_precision = 53L,
    use_crypto_mixing = TRUE,
    buffer_size = 1024L
  ))
  
  x1 <- generatePRNG(prng, 5000)
  
  # Test without crypto mixing
  updatePRNG(prng, list(use_crypto_mixing = FALSE))
  x2 <- generatePRNG(prng, 5000)
  
  # Results should be different
  expect_false(identical(x1, x2))
  
  # Both should pass basic uniformity tests
  expect_gt(stats::ks.test(x1, "punif")$p.value, 0.01)
  expect_gt(stats::ks.test(x2, "punif")$p.value, 0.01)
  
  cleanup_prng(prng)
})

test_that("Reseeding Behavior", {
  skip_on_cran()
  
  # Test with frequent reseeding
  prng <- createPRNG(list(
    a = 2L,
    b = 5L,
    c = -1L,
    reseed_interval = 1000L,
    use_crypto_mixing = TRUE
  ))
  
  x1 <- generatePRNG(prng, 5000)
  
  # Test without reseeding
  updatePRNG(prng, list(reseed_interval = 0L))
  x2 <- generatePRNG(prng, 5000)
  
  # Both sequences should be valid
  expect_gt(stats::ks.test(x1, "punif")$p.value, 0.01)
  expect_gt(stats::ks.test(x2, "punif")$p.value, 0.01)
  
  cleanup_prng(prng)
})

test_that("Different Distributions", {
  skip_on_cran()
  
  # Test uniform_01 (default)
  prng <- createPRNG(list(
    a = 2L,
    b = 5L,
    c = -1L
  ))
  
  x1 <- generatePRNG(prng, 5000)
  expect_true(all(x1 >= 0 & x1 <= 1))
  expect_gt(stats::ks.test(x1, "punif")$p.value, 0.01)
  
  # Test uniform_range
  updatePRNG(prng, list(
    distribution = "uniform_range",
    range_min = -10,
    range_max = 10
  ))
  x2 <- generatePRNG(prng, 5000)
  expect_true(all(x2 >= -10 & x2 <= 10))
  expect_gt(stats::ks.test((x2 + 10)/20, "punif")$p.value, 0.01)
  
  # Test normal
  updatePRNG(prng, list(
    distribution = "normal",
    normal_mean = 0,
    normal_sd = 1
  ))
  x3 <- generatePRNG(prng, 5000)
  expect_gt(stats::ks.test(x3, "pnorm")$p.value, 0.01)
  
  # Test exponential
  updatePRNG(prng, list(
    distribution = "exponential",
    exponential_lambda = 1
  ))
  x4 <- generatePRNG(prng, 5000)
  expect_true(all(x4 >= 0))
  expect_gt(stats::ks.test(x4, "pexp")$p.value, 0.01)
  
  cleanup_prng(prng)
})

test_that("PRNG handles invalid parameters", {
  # Test invalid quadratic parameters (non-positive discriminant)
  expect_error(
    createPRNG(list(
      a = 1L,
      b = 1L,
      c = 1L,
      mpfr_precision = 53L
    )),
    "discriminant must be positive"
  )
  
  # Test invalid precision
  expect_error(
    createPRNG(list(
      a = 2L,
      b = 5L,
      c = -1L,
      mpfr_precision = 1L  # Too small
    )),
    "Invalid MPFR precision"
  )
  
  expect_error(
    createPRNG(list(
      a = 2L,
      b = 5L,
      c = -1L,
      mpfr_precision = 20000L  # Too large
    )),
    "Invalid MPFR precision"
  )
})

test_that("PRNG handles invalid distribution parameters", {
  # First create PRNG with valid parameters
  prng <- createPRNG(list(
    a = 2L,
    b = 5L,
    c = -1L
  ))
  
  # Test invalid normal distribution parameters
  expect_error(
    updatePRNG(prng, list(
      distribution = "normal",
      normal_sd = -1  # Must be positive
    )), 
    "standard deviation must be positive"
  )
  
  # Test invalid exponential distribution parameters
  expect_error(
    updatePRNG(prng, list(
      distribution = "exponential",
      exponential_lambda = 0  # Must be positive
    )),
    "lambda must be positive"
  )
  
  # Test invalid uniform range parameters
  expect_error(
    updatePRNG(prng, list(
      distribution = "uniform_range",
      range_min = 10,
      range_max = 0  # Must be greater than min
    )),
    "max must be greater than min"
  )
  
  cleanup_prng(prng)
})

test_that("PRNG maintains state correctly", {
  prng <- createPRNG(list(
    a = 2L,
    b = 5L,
    c = -1L
  ))
  
  # Generate some numbers
  x1 <- generatePRNG(prng, 1000)
  
  # Reseed and generate more
  reseedPRNG(prng)
  x2 <- generatePRNG(prng, 1000)
  
  # Sequences should be different
  expect_false(identical(x1, x2))
  
  # Both should be uniform
  expect_gt(stats::ks.test(x1, "punif")$p.value, 0.01)
  expect_gt(stats::ks.test(x2, "punif")$p.value, 0.01)
  
  cleanup_prng(prng)
})

test_that("PRNG handles multiple instances", {
  # Create two PRNGs with different parameters
  prng1 <- createPRNG(list(
    a = 2L,
    b = 5L,
    c = -1L
  ))
  
  prng2 <- createPRNG(list(
    a = 3L,
    b = 7L,
    c = -2L
  ))
  
  # Generate sequences from both
  x1 <- generatePRNG(prng1, 1000)
  x2 <- generatePRNG(prng2, 1000)
  
  # Sequences should be different
  expect_false(identical(x1, x2))
  
  # Both should be uniform
  expect_gt(stats::ks.test(x1, "punif")$p.value, 0.01)
  expect_gt(stats::ks.test(x2, "punif")$p.value, 0.01)
  
  cleanup_prng(prng1)
  cleanup_prng(prng2)
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
})

test_that("Thread safety works correctly", {
  skip_on_cran()
  library(parallel)
  
  # Create cluster with 4 cores
  cl <- makeCluster(4)
  on.exit(stopCluster(cl))
  
  # Load package on all cores
  clusterEvalQ(cl, library(qiprng))
  
  # Initialize PRNG
  createPRNG()
  
  # Generate numbers from multiple threads
  results <- parSapply(cl, 1:4, function(i) {
    generatePRNG(1000)
  })
  
  # Verify results
  expect_equal(dim(results), c(1000, 4))
  
  # Check that sequences are different (not duplicated across threads)
  cor_matrix <- cor(results)
  diag(cor_matrix) <- 0
  expect_true(all(abs(cor_matrix) < 0.1))
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
})