# test-caching-framework.R
# Tests for the caching framework

test_that("caching framework loads and initializes correctly", {
  # Source the caching framework
  source("../../R/caching_framework.R")
  
  # Check that functions exist
  expect_true(exists("cached_acf"))
  expect_true(exists("cached_pacf"))
  expect_true(exists("cached_spectrum"))
  expect_true(exists("cached_compress"))
  
  # Initialize cache
  cache_dir <- tempfile("test_cache")
  init_cache(cache_dir = cache_dir, enabled = TRUE)
  
  # Check cache is enabled
  expect_true(is_cache_enabled())
  
  # Clean up
  unlink(cache_dir, recursive = TRUE)
})

test_that("cached_acf returns same results as acf", {
  skip_if_not_installed("R.cache")
  skip_if_not_installed("digest")
  
  source("../../R/caching_framework.R")
  
  # Initialize cache
  cache_dir <- tempfile("test_cache")
  init_cache(cache_dir = cache_dir, enabled = TRUE)
  
  # Generate test data
  set.seed(123)
  x <- rnorm(1000)
  
  # Compare results
  result1 <- cached_acf(x, lag.max = 20)
  result2 <- acf(x, lag.max = 20, plot = FALSE)
  
  expect_equal(result1$acf, result2$acf)
  expect_equal(result1$lag, result2$lag)
  
  # Test caching - second call should be faster
  t1 <- system.time(cached_acf(x, lag.max = 20))
  t2 <- system.time(cached_acf(x, lag.max = 20))
  
  # Second call should be faster (cached) or at least not slower
  # Note: timing can be variable, so we allow equality
  expect_true(t2[3] <= t1[3] * 1.1,
              info = paste("First call:", t1[3], "seconds, Second call:", t2[3], "seconds"))
  
  # Clean up
  clear_qiprng_cache()
  unlink(cache_dir, recursive = TRUE)
})

test_that("cached_pacf returns same results as pacf", {
  skip_if_not_installed("R.cache")
  skip_if_not_installed("digest")
  
  source("../../R/caching_framework.R")
  
  # Initialize cache
  cache_dir <- tempfile("test_cache")
  init_cache(cache_dir = cache_dir, enabled = TRUE)
  
  # Generate test data
  set.seed(123)
  x <- rnorm(1000)
  
  # Compare results
  result1 <- cached_pacf(x, lag.max = 20)
  result2 <- pacf(x, lag.max = 20, plot = FALSE)
  
  expect_equal(result1$acf, result2$acf)
  expect_equal(result1$lag, result2$lag)
  
  # Clean up
  clear_qiprng_cache()
  unlink(cache_dir, recursive = TRUE)
})

test_that("cached_spectrum returns same results as spectrum", {
  skip_if_not_installed("R.cache")
  skip_if_not_installed("digest")
  
  source("../../R/caching_framework.R")
  
  # Initialize cache
  cache_dir <- tempfile("test_cache")
  init_cache(cache_dir = cache_dir, enabled = TRUE)
  
  # Generate test data
  set.seed(123)
  x <- rnorm(1000)
  
  # Compare results
  result1 <- cached_spectrum(x)
  result2 <- spectrum(x, plot = FALSE)
  
  expect_equal(result1$spec, result2$spec)
  expect_equal(result1$freq, result2$freq)
  
  # Clean up
  clear_qiprng_cache()
  unlink(cache_dir, recursive = TRUE)
})

test_that("cached_compress returns same results as memCompress", {
  skip_if_not_installed("R.cache")
  skip_if_not_installed("digest")
  
  source("../../R/caching_framework.R")
  
  # Initialize cache
  cache_dir <- tempfile("test_cache")
  init_cache(cache_dir = cache_dir, enabled = TRUE)
  
  # Generate test data
  x_raw <- as.raw(sample(0:255, 1000, replace = TRUE))
  
  # Compare results
  result1 <- cached_compress(x_raw, type = "gzip")
  result2 <- memCompress(x_raw, type = "gzip")
  
  expect_equal(result1, result2)
  
  # Clean up
  clear_qiprng_cache()
  unlink(cache_dir, recursive = TRUE)
})

test_that("cache can be disabled", {
  skip_if_not_installed("R.cache")
  skip_if_not_installed("digest")
  
  source("../../R/caching_framework.R")
  
  # Disable cache
  set_cache_enabled(FALSE)
  expect_false(is_cache_enabled())
  
  # Generate test data
  set.seed(123)
  x <- rnorm(1000)
  
  # With cache disabled, should call regular function
  result1 <- cached_acf(x, lag.max = 20)
  result2 <- acf(x, lag.max = 20, plot = FALSE)
  
  expect_equal(result1$acf, result2$acf)
  
  # Re-enable cache
  set_cache_enabled(TRUE)
  expect_true(is_cache_enabled())
})

test_that("cache statistics work correctly", {
  skip_if_not_installed("R.cache")
  skip_if_not_installed("digest")
  
  source("../../R/caching_framework.R")
  
  # Initialize cache
  cache_dir <- tempfile("test_cache")
  init_cache(cache_dir = cache_dir, enabled = TRUE)
  
  # Initially empty
  stats <- qiprng_cache_stats()
  expect_equal(stats$num_entries, 0)
  expect_equal(stats$size_mb, 0)
  
  # Generate some cached data
  x <- rnorm(1000)
  cached_acf(x, lag.max = 20)
  cached_pacf(x, lag.max = 20)
  
  # Check stats updated
  stats <- qiprng_cache_stats()
  expect_true(stats$num_entries > 0)
  
  # Clean up
  clear_qiprng_cache()
  unlink(cache_dir, recursive = TRUE)
})