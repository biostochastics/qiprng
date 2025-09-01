# File: cached_basic_tests.R
# ----------------------------------------------------------------------
#' Cached wrapper for basic distribution tests
#'
#' This module provides cached versions of the basic distribution tests
#' to improve performance for repeated test runs on the same data.

#' Run cached basic distribution tests
#'
#' @param suite The test suite object
#' @return Updated test suite with results
#' @export
run_cached_basic_tests <- function(suite) {
  # Check if caching functions are available
  if (!exists("cached_test_result", mode = "function")) {
    # Fall back to regular tests if caching not available
    return(run_basic_tests(suite))
  }

  # Generate data for testing
  n <- suite$config$basic_sample_size

  # Ensure n is valid
  if (length(n) != 1 || !is.numeric(n) || is.na(n)) {
    n <- 100
  }
  if (n < 10) {
    n <- 10
  }

  x <- suite$prng_func(n)

  # Initialize results
  suite$results$basic <- list()

  # Define test functions for each test type
  ks_test_func <- function(data, config, ...) {
    ks_result <- ks.test(data, "punif", 0, 1)
    list(
      description = "Kolmogorov-Smirnov Test for Uniformity",
      result = if (is.na(ks_result$p.value)) {
        "INCONCLUSIVE"
      } else if (ks_result$p.value >= config$significance_level) {
        "PASS"
      } else {
        "FAIL"
      },
      p_value = ks_result$p.value,
      statistic = ks_result$statistic,
      details = paste(
        "Tests if the distribution is uniform on [0,1].",
        "p-value should be above significance level for uniformity."
      )
    )
  }

  chi_squared_func <- function(data, config, ...) {
    n <- length(data)
    bins_count <- config$chi_squared_bins

    if (length(bins_count) != 1 || !is.numeric(bins_count) || is.na(bins_count)) {
      bins_count <- min(20, max(5, n %/% 5))
    }

    if (n < bins_count * 5) {
      bins_count <- max(5, n %/% 5)
    }

    bins <- cut(data,
      breaks = seq(0, 1, length.out = bins_count + 1),
      include.lowest = TRUE
    )
    observed <- table(bins)
    expected <- rep(n / bins_count, bins_count)

    if (length(observed) < bins_count) {
      expected <- rep(n / length(observed), length(observed))
    }

    chi_result <- tryCatch(
      {
        chisq.test(observed, p = expected / sum(expected))
      },
      error = function(e) {
        list(p.value = NA, statistic = NA, parameter = NA)
      }
    )

    list(
      description = "Chi-squared Goodness-of-Fit Test",
      result = ifelse(!is.na(chi_result$p.value) &&
        chi_result$p.value >= config$significance_level,
      "PASS", ifelse(is.na(chi_result$p.value), "INCONCLUSIVE", "FAIL")
      ),
      p_value = chi_result$p.value,
      statistic = chi_result$statistic,
      details = paste(
        "Tests uniformity using chi-squared test with", bins_count, "bins.",
        "p-value should be above significance level for uniformity."
      )
    )
  }

  mean_test_func <- function(data, config, ...) {
    n <- length(data)
    sample_mean <- mean(data)
    expected_mean <- 0.5
    expected_sd <- 1 / sqrt(12 * n) # SD of mean for uniform(0,1)

    z_statistic <- (sample_mean - expected_mean) / expected_sd
    p_value <- 2 * pnorm(-abs(z_statistic))

    list(
      description = "Mean Test",
      result = if (is.na(p_value)) {
        "INCONCLUSIVE"
      } else if (p_value >= config$significance_level) {
        "PASS"
      } else {
        "FAIL"
      },
      p_value = p_value,
      statistic = sample_mean,
      details = paste(
        "Tests if mean is close to 0.5.",
        "Sample mean:", round(sample_mean, 6),
        "Expected:", expected_mean
      )
    )
  }

  variance_test_func <- function(data, config, ...) {
    n <- length(data)
    sample_var <- var(data)
    expected_var <- 1 / 12 # Variance of uniform(0,1)

    # Chi-squared test for variance
    chi_statistic <- (n - 1) * sample_var / expected_var
    p_value <- 2 * min(
      pchisq(chi_statistic, n - 1),
      1 - pchisq(chi_statistic, n - 1)
    )

    list(
      description = "Variance Test",
      result = if (is.na(p_value)) {
        "INCONCLUSIVE"
      } else if (p_value >= config$significance_level) {
        "PASS"
      } else {
        "FAIL"
      },
      p_value = p_value,
      statistic = sample_var,
      details = paste(
        "Tests if variance is close to 1/12.",
        "Sample variance:", round(sample_var, 6),
        "Expected:", round(expected_var, 6)
      )
    )
  }

  # Run cached tests
  suite$results$basic$ks_test <- cached_test_result(
    test_func = ks_test_func,
    test_name = "ks_test",
    test_category = "basic",
    data = x,
    config = suite$config
  )

  suite$results$basic$chi_squared <- cached_test_result(
    test_func = chi_squared_func,
    test_name = "chi_squared",
    test_category = "basic",
    data = x,
    config = suite$config
  )

  suite$results$basic$mean_test <- cached_test_result(
    test_func = mean_test_func,
    test_name = "mean_test",
    test_category = "basic",
    data = x,
    config = suite$config
  )

  suite$results$basic$variance_test <- cached_test_result(
    test_func = variance_test_func,
    test_name = "variance_test",
    test_category = "basic",
    data = x,
    config = suite$config
  )

  return(suite)
}
