# Test file for bootstrap-based compression tests
# ----------------------------------------------------------------------

# Check if required files exist
check_dependencies <- function() {
  bootstrap_exists <- file.exists("../../R/bootstrap_framework.R")
  compression_exists <- file.exists("../../R/statisticaltests/compression_tests_bootstrap.R")

  if (!bootstrap_exists || !compression_exists) {
    return(FALSE)
  }

  # Try to source the files
  tryCatch(
    {
      source("../../R/bootstrap_framework.R")
      source("../../R/statisticaltests/compression_tests_bootstrap.R")
      return(TRUE)
    },
    error = function(e) {
      return(FALSE)
    }
  )
}

# Source required files
source_test_helpers <- function() {
  # Skip if dependencies don't exist
  if (!check_dependencies()) {
    skip("Required bootstrap framework files not available")
  }

  # Try to source bootstrap framework
  if (!exists("bootstrap_p_value")) {
    if (file.exists("../../R/bootstrap_framework.R")) {
      source("../../R/bootstrap_framework.R")
    }
  }

  # Try to source compression tests
  if (!exists("run_compression_tests_bootstrap")) {
    if (file.exists("../../R/statisticaltests/compression_tests_bootstrap.R")) {
      source("../../R/statisticaltests/compression_tests_bootstrap.R")
    }
  }
}

# Create a mock test suite
create_test_suite <- function(n = 10000, prng_func = NULL) {
  if (is.null(prng_func)) {
    prng_func <- function(n) runif(n)
  }

  list(
    config = list(
      compression_sample_size = n,
      significance_level = 0.05,
      bootstrap_samples = 1000,
      show_progress = FALSE,
      save_visualizations = FALSE,
      compression_algorithms = c("gzip", "bzip2", "rle")
    ),
    prng_func = prng_func,
    results = list()
  )
}

test_that("compression ratio bootstrap test works correctly", {
  skip_if_not(check_dependencies(), "Required bootstrap framework files not available")
  source_test_helpers()

  # Test with truly random data
  set.seed(123)
  suite <- create_test_suite(n = 5000)

  # Run the bootstrap compression tests
  suite_result <- run_compression_tests_bootstrap(suite, use_legacy = FALSE)

  # Check that results were added
  expect_true("compression" %in% names(suite_result$results))

  # Check for gzip results (if available)
  if (requireNamespace("memCompress", quietly = TRUE)) {
    expect_true("compression_ratio_gzip" %in% names(suite_result$results$compression))

    gzip_result <- suite_result$results$compression$compression_ratio_gzip
    expect_true("p_value" %in% names(gzip_result))
    expect_true("p_value_kde" %in% names(gzip_result))
    expect_true("observed_ratio" %in% names(gzip_result))

    # For random data, compression ratio should be high (close to 1)
    expect_true(gzip_result$observed_ratio > 0.9)

    # P-value should typically be high (not rejecting randomness)
    expect_true(gzip_result$p_value > 0.01)
  }

  # Check for RLE results (fallback)
  if ("compression_ratio_rle" %in% names(suite_result$results$compression)) {
    rle_result <- suite_result$results$compression$compression_ratio_rle
    expect_true("p_value" %in% names(rle_result))
    expect_true(rle_result$observed_ratio > 0.8)
  }
})

test_that("compression test detects non-random data", {
  skip_if_not(check_dependencies(), "Required bootstrap framework files not available")
  source_test_helpers()

  # Create highly compressible (non-random) data
  set.seed(456)
  non_random_prng <- function(n) {
    # Repeating pattern
    rep(c(0.1, 0.2, 0.3, 0.4, 0.5), length.out = n)
  }

  suite <- create_test_suite(n = 5000, prng_func = non_random_prng)
  suite_result <- run_compression_tests_bootstrap(suite, use_legacy = FALSE)

  # Check compression results
  if (requireNamespace("memCompress", quietly = TRUE)) {
    gzip_result <- suite_result$results$compression$compression_ratio_gzip

    # Non-random data should compress well (low ratio)
    expect_true(gzip_result$observed_ratio < 0.5)

    # Should have low p-value (rejecting randomness)
    expect_true(gzip_result$p_value < 0.05)
  }
})

test_that("entropy bootstrap test works correctly", {
  skip_if_not(check_dependencies(), "Required bootstrap framework files not available")
  source_test_helpers()

  set.seed(789)
  suite <- create_test_suite(n = 5000)
  suite_result <- run_compression_tests_bootstrap(suite, use_legacy = FALSE)

  # Check entropy test results
  expect_true("entropy_bootstrap" %in% names(suite_result$results$compression))

  entropy_result <- suite_result$results$compression$entropy_bootstrap
  expect_true("p_value" %in% names(entropy_result))
  expect_true("entropy" %in% names(entropy_result))
  expect_true("max_entropy" %in% names(entropy_result))
  expect_true("deviation" %in% names(entropy_result))

  # For random data, entropy should be close to maximum
  expect_true(entropy_result$entropy > 7.9)
  expect_true(entropy_result$entropy <= entropy_result$max_entropy)

  # Deviation should be small
  expect_true(entropy_result$deviation < 0.1)

  # P-value should be high (not rejecting randomness)
  expect_true(entropy_result$p_value > 0.05)
})

test_that("byte frequency Monte Carlo test works correctly", {
  skip_if_not(check_dependencies(), "Required bootstrap framework files not available")
  source_test_helpers()

  set.seed(101)
  suite <- create_test_suite(n = 5000)
  suite_result <- run_compression_tests_bootstrap(suite, use_legacy = FALSE)

  # Check byte frequency test results
  expect_true("byte_frequency_monte_carlo" %in% names(suite_result$results$compression))

  byte_result <- suite_result$results$compression$byte_frequency_monte_carlo
  expect_true("p_value" %in% names(byte_result))
  expect_true("p_value_analytical" %in% names(byte_result))
  expect_true("statistic" %in% names(byte_result))

  # Monte Carlo and analytical p-values should be similar
  expect_true(abs(byte_result$p_value - byte_result$p_value_analytical) < 0.1)

  # For uniform random data, p-value should not be extreme
  expect_true(byte_result$p_value > 0.01 && byte_result$p_value < 0.99)
})

test_that("multiple compression algorithms work correctly", {
  skip_if_not(check_dependencies(), "Required bootstrap framework files not available")
  source_test_helpers()

  set.seed(202)
  suite <- create_test_suite(n = 3000)
  suite$config$compression_algorithms <- c("gzip", "bzip2", "rle")

  suite_result <- run_compression_tests_bootstrap(suite, use_legacy = FALSE)

  # Count how many algorithms were tested
  algo_count <- sum(c(
    "compression_ratio_gzip", "compression_ratio_bzip2",
    "compression_ratio_rle"
  ) %in%
    names(suite_result$results$compression))

  expect_true(algo_count >= 1) # At least one algorithm should work

  # Check for combined result if multiple algorithms
  if (algo_count > 1) {
    expect_true("compression_ratio_combined" %in% names(suite_result$results$compression))
    combined <- suite_result$results$compression$compression_ratio_combined
    expect_true("p_value" %in% names(combined))
    expect_true("statistic" %in% names(combined)) # Fisher's statistic
  }
})

test_that("backward compatibility with legacy mode works", {
  skip_if_not(check_dependencies(), "Required bootstrap framework files not available")
  source_test_helpers()

  set.seed(303)
  suite <- create_test_suite(n = 1000)

  # Force legacy mode by calling the legacy function directly
  if (exists("run_compression_tests_legacy")) {
    suite_legacy <- run_compression_tests_legacy(suite)
  } else {
    skip("Legacy compression tests not available")
  }

  # Should have basic compression results
  expect_true("compression" %in% names(suite_legacy$results))
  expect_true("compression_ratio" %in% names(suite_legacy$results$compression))

  # Legacy uses binary p-values (0 or 1)
  legacy_p <- suite_legacy$results$compression$compression_ratio$p_value
  expect_true(legacy_p == 0 || legacy_p == 1)
})

test_that("KDE p-value calculation works correctly", {
  skip_if_not(check_dependencies(), "Required bootstrap framework files not available")
  source_test_helpers()

  set.seed(404)
  suite <- create_test_suite(n = 2000)
  suite_result <- run_compression_tests_bootstrap(suite, use_legacy = FALSE)

  if (requireNamespace("memCompress", quietly = TRUE)) {
    gzip_result <- suite_result$results$compression$compression_ratio_gzip

    # Should have both bootstrap and KDE p-values
    expect_true("p_value" %in% names(gzip_result))
    expect_true("p_value_kde" %in% names(gzip_result))

    # Both p-values should be valid probabilities
    expect_true(gzip_result$p_value >= 0 && gzip_result$p_value <= 1)
    expect_true(gzip_result$p_value_kde >= 0 && gzip_result$p_value_kde <= 1)
  }
})

test_that("bootstrap distributions are stored correctly", {
  skip_if_not(check_dependencies(), "Required bootstrap framework files not available")
  source_test_helpers()

  set.seed(505)
  suite <- create_test_suite(n = 1000)
  suite$config$bootstrap_samples <- 500 # Smaller for testing

  suite_result <- run_compression_tests_bootstrap(suite, use_legacy = FALSE)

  # Check if bootstrap distributions were stored
  expect_true("bootstrap_distributions" %in% names(suite_result))
  expect_true("compression" %in% names(suite_result$bootstrap_distributions))

  boot_dist <- suite_result$bootstrap_distributions$compression

  # Should have various distributions
  expect_true("compression_ratios" %in% names(boot_dist))
  expect_true("entropy_deviations" %in% names(boot_dist))
  expect_true("byte_chi_squares" %in% names(boot_dist))

  # Check distribution sizes
  expect_equal(length(boot_dist$entropy_deviations), 500)
  expect_equal(length(boot_dist$byte_chi_squares), 500)
})
