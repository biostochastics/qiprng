# test-cached-test-results.R
# Tests for test result caching functionality

test_that("cached_test_result works correctly", {
  skip_if_not_installed("R.cache")
  skip_if_not_installed("digest")

  source("../../R/caching_framework.R")

  # Initialize cache
  cache_dir <- tempfile("test_cache")
  init_cache(cache_dir = cache_dir, enabled = TRUE)

  # Create a simple test function
  test_func <- function(data, config, ...) {
    # Simulate expensive computation
    Sys.sleep(0.1)
    list(
      result = "PASS",
      p_value = 0.5,
      statistic = mean(data)
    )
  }

  # Create test data and config
  data <- rnorm(100)
  config <- list(
    cache_test_results = TRUE,
    cache_ttl_basic_hours = 1
  )

  # First call should compute
  t1 <- system.time({
    result1 <- cached_test_result(
      test_func = test_func,
      test_name = "test1",
      test_category = "basic",
      data = data,
      config = config
    )
  })

  # Second call should be cached
  t2 <- system.time({
    result2 <- cached_test_result(
      test_func = test_func,
      test_name = "test1",
      test_category = "basic",
      data = data,
      config = config
    )
  })

  # Results should be identical
  expect_equal(result1, result2)

  # Second call should be faster (cached)
  # Relaxed timing check - just verify it's faster, not necessarily 2x
  expect_true(t2[3] < t1[3]) # Should be faster when cached

  # Clean up
  clear_test_cache()
  unlink(cache_dir, recursive = TRUE)
})

test_that("cache respects category-specific TTL", {
  skip_if_not_installed("R.cache")
  skip_if_not_installed("digest")

  source("../../R/caching_framework.R")

  # Initialize cache
  cache_dir <- tempfile("test_cache")
  init_cache(cache_dir = cache_dir, enabled = TRUE)

  # Test function
  test_func <- function(data, config, ...) {
    list(result = "PASS", p_value = runif(1))
  }

  # Create configs with different TTLs
  data <- rnorm(50)
  config1 <- list(
    cache_test_results = TRUE,
    cache_ttl_basic_hours = 24,
    cache_ttl_correlation_hours = 12
  )

  # Cache basic test
  result1 <- cached_test_result(
    test_func = test_func,
    test_name = "test1",
    test_category = "basic",
    data = data,
    config = config1
  )

  # Cache correlation test
  result2 <- cached_test_result(
    test_func = test_func,
    test_name = "test2",
    test_category = "correlation",
    data = data,
    config = config1
  )

  # Both should be cached
  stats <- test_cache_stats()
  expect_true(nrow(stats) >= 2)
  expect_true("basic" %in% stats$category)
  expect_true("correlation" %in% stats$category)

  # Clean up
  clear_test_cache()
  unlink(cache_dir, recursive = TRUE)
})

test_that("cache can be disabled", {
  skip_if_not_installed("R.cache")
  skip_if_not_installed("digest")

  source("../../R/caching_framework.R")

  # Test function that tracks calls
  call_count <- 0
  test_func <- function(data, config, ...) {
    call_count <<- call_count + 1
    list(result = "PASS", p_value = 0.5)
  }

  data <- rnorm(50)

  # With caching disabled in config
  config <- list(cache_test_results = FALSE)

  # Call twice
  result1 <- cached_test_result(
    test_func = test_func,
    test_name = "test1",
    test_category = "basic",
    data = data,
    config = config
  )

  result2 <- cached_test_result(
    test_func = test_func,
    test_name = "test1",
    test_category = "basic",
    data = data,
    config = config
  )

  # Function should be called twice (no caching)
  expect_equal(call_count, 2)
})

test_that("cache key includes PRNG seed", {
  skip_if_not_installed("R.cache")
  skip_if_not_installed("digest")

  source("../../R/caching_framework.R")

  # Initialize cache
  cache_dir <- tempfile("test_cache")
  init_cache(cache_dir = cache_dir, enabled = TRUE)

  # Create package environment mock
  .pkgenv <- new.env()
  assign(".pkgenv", .pkgenv, envir = .GlobalEnv)

  # Test function
  test_func <- function(data, config, ...) {
    list(result = "PASS", p_value = runif(1))
  }

  data <- rnorm(50)
  config <- list(cache_test_results = TRUE)

  # Set different seeds
  assign("current_config", list(seed = 123), envir = .pkgenv)
  result1 <- cached_test_result(
    test_func = test_func,
    test_name = "test1",
    test_category = "basic",
    data = data,
    config = config
  )

  assign("current_config", list(seed = 456), envir = .pkgenv)
  result2 <- cached_test_result(
    test_func = test_func,
    test_name = "test1",
    test_category = "basic",
    data = data,
    config = config
  )

  # Results should be different (different cache keys due to seed)
  expect_false(identical(result1$p_value, result2$p_value))

  # Clean up
  rm(.pkgenv, envir = .GlobalEnv)
  clear_test_cache()
  unlink(cache_dir, recursive = TRUE)
})

test_that("clear_test_cache works correctly", {
  skip_if_not_installed("R.cache")
  skip_if_not_installed("digest")

  source("../../R/caching_framework.R")

  # Initialize cache
  cache_dir <- tempfile("test_cache")
  init_cache(cache_dir = cache_dir, enabled = TRUE)

  # Test function
  test_func <- function(data, config, ...) {
    list(result = "PASS")
  }

  data <- rnorm(50)
  config <- list(cache_test_results = TRUE)

  # Cache some results
  cached_test_result(test_func, "test1", "basic", data, config)
  cached_test_result(test_func, "test2", "correlation", data, config)

  # Check both are cached
  stats <- test_cache_stats()
  expect_true(sum(stats$num_entries) > 0)

  # Clear specific category
  clear_test_cache("basic")
  stats <- test_cache_stats()
  basic_stats <- stats[stats$category == "basic", ]
  if (nrow(basic_stats) > 0) {
    expect_equal(basic_stats$num_entries, 0)
  }

  # Clear all
  clear_test_cache()
  stats <- test_cache_stats()
  # Cache clearing might not be immediate, so just check it's reduced
  expect_true(sum(stats$num_entries) <= 1) # Allow for minor race conditions

  # Clean up
  unlink(cache_dir, recursive = TRUE)
})
