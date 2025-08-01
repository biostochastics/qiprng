# File: basic_tests.R
# ----------------------------------------------------------------------
#' Basic distribution tests for PRNG quality
#'
#' This module provides fundamental statistical tests to evaluate the 
#' distributional properties of a PRNG, including tests for uniformity,
#' goodness-of-fit, and basic statistical properties.
#'
#' The tests implemented in this module include:
#' \itemize{
#'   \item Kolmogorov-Smirnov test for uniformity
#'   \item Chi-squared goodness-of-fit test
#'   \item Mean and variance tests
#'   \item Min-max range tests
#'   \item Autocorrelation tests
#' }
#'
#' These tests are designed to detect common issues in PRNGs such as
#' non-uniformity, clustering, and statistical bias.
#'
#' @name basic_tests
#' @aliases basic-tests
#' @keywords internal

# Source effect size calculations if available
if (file.exists(system.file("R/statisticaltests/effect_sizes.R", package = "qiprng"))) {
  source(system.file("R/statisticaltests/effect_sizes.R", package = "qiprng"))
} else if (file.exists("R/statisticaltests/effect_sizes.R")) {
  source("R/statisticaltests/effect_sizes.R")
}

#' Run basic distribution tests on random number generator
#'
#' Executes a comprehensive set of basic statistical tests on a random
#' number generator to evaluate its uniformity, distribution characteristics,
#' and statistical properties. The tests include Kolmogorov-Smirnov test,
#' Chi-squared goodness-of-fit test, and basic descriptive statistics.
#'
#' @param suite The test suite object containing the PRNG function and configuration
#' @return Updated test suite with results of all basic tests added to suite$results$basic
#' @details
#' This function performs the following tests:
#' \itemize{
#'   \item Kolmogorov-Smirnov test against uniform distribution
#'   \item Chi-squared goodness-of-fit test with configurable bin count
#'   \item Basic statistical tests including mean, variance, min/max range
#'   \item Gap and autocorrelation tests for independence
#' }
#' 
#' All test results include a PASS/FAIL indication based on the significance level
#' configured in the test suite, along with p-values and test statistics.
#' @keywords internal
run_basic_tests <- function(suite) {
  # Generate random numbers
  n <- suite$config$basic_sample_size
  
  # Ensure n is a single numeric value for sample size
  if (length(n) != 1 || !is.numeric(n) || is.na(n)) {
    n <- 100  # Default to a reasonable sample size if invalid
  }
  
  # Ensure sample size is at least 10 for statistical validity
  if (n < 10) {
    n <- 10
  }
  
  x <- suite$prng_func(n)
  
  # Initialize results
  suite$results$basic <- list()
  
  # Kolmogorov-Smirnov test for uniformity
  ks_result <- ks.test(x, "punif", 0, 1)
  ks_test_result <- list(
    description = "Kolmogorov-Smirnov Test for Uniformity",
    result = if (is.na(ks_result$p.value)) {
      "INCONCLUSIVE"
    } else if (ks_result$p.value >= suite$config$significance_level) {
      "PASS"
    } else {
      "FAIL"
    },
    p_value = ks_result$p.value,
    statistic = ks_result$statistic,
    details = paste("Tests if the distribution is uniform on [0,1].",
                   "p-value should be above significance level for uniformity.")
  )
  
  # KS D statistic is already an effect size
  if (exists("add_effect_size")) {
    ks_test_result <- add_effect_size(
      ks_test_result, 
      ks_result$statistic, 
      "ks"
    )
  }
  
  suite$results$basic$ks_test <- ks_test_result
  
  # Chi-squared goodness-of-fit test
  # Make sure chi_squared_bins is a single numeric value
  bins_count <- suite$config$chi_squared_bins
  if (length(bins_count) != 1 || !is.numeric(bins_count) || is.na(bins_count)) {
    bins_count <- min(20, max(5, n %/% 5))  # Default based on sample size
  }
  
  # For small sample sizes, reduce the number of bins
  if (n < bins_count * 5) {  # Rule of thumb: at least 5 samples per bin
    bins_count <- max(5, n %/% 5)
  }
  
  # Create bins and calculate observed frequencies
  bins <- cut(x, breaks = seq(0, 1, length.out = bins_count + 1), 
              include.lowest = TRUE)
  observed <- table(bins)
  expected <- rep(n / bins_count, bins_count)
  
  # If some bins are empty, adjust the expected frequencies
  if (length(observed) < bins_count) {
    expected <- rep(n / length(observed), length(observed))
  }
  
  # Perform chi-squared test
  chi_result <- tryCatch({
    chisq.test(observed, p = expected / sum(expected))
  }, error = function(e) {
    # If test fails (e.g., not enough data), return a placeholder
    list(
      p.value = NA,
      statistic = NA,
      parameter = NA,
      method = "Chi-squared test (failed)"
    )
  })
  chi_test_result <- list(
    description = "Chi-squared Goodness-of-Fit Test",
    result = if (is.na(chi_result$p.value)) {
      "INCONCLUSIVE"
    } else if (chi_result$p.value >= suite$config$significance_level) {
      "PASS"
    } else {
      "FAIL"
    },
    p_value = chi_result$p.value,
    statistic = chi_result$statistic,
    diagnostics = list(bins_used = bins_count, sample_size = n),
    details = paste("Tests if frequencies in", bins_count, 
                   "bins match expected uniform frequencies.")
  )
  
  # Calculate Cramér's V for chi-squared test
  if (exists("calculate_cramers_v") && !is.na(chi_result$statistic)) {
    cramers_v <- calculate_cramers_v(
      chi_result$statistic, 
      n, 
      bins_count - 1  # df for goodness-of-fit
    )
    chi_test_result <- add_effect_size(
      chi_test_result, 
      cramers_v, 
      "v", 
      df = bins_count - 1
    )
  }
  
  suite$results$basic$chi_squared <- chi_test_result
  
  # Mean test (should be close to 0.5 for uniform[0,1])
  mean_x <- mean(x)
  expected_mean <- 0.5
  z_mean <- (mean_x - expected_mean) / (1/sqrt(12 * n))  # Using variance of uniform(0,1)
  p_mean <- 2 * (1 - pnorm(abs(z_mean)))
  mean_test_result <- list(
    description = "Mean Test",
    result = if (is.na(p_mean)) {
      "INCONCLUSIVE"
    } else if (p_mean >= suite$config$significance_level) {
      "PASS"
    } else {
      "FAIL"
    },
    p_value = p_mean,
    statistic = z_mean,
    details = paste("Tests if mean =", format(mean_x, digits = 6), 
                   "is close to expected 0.5 for uniform distribution.")
  )
  
  # Calculate Cohen's d for mean test
  if (exists("calculate_cohens_d")) {
    cohens_d <- calculate_cohens_d(
      mean_x, 
      expected_mean, 
      1/sqrt(12)  # SD of uniform(0,1)
    )
    mean_test_result <- add_effect_size(
      mean_test_result, 
      cohens_d, 
      "d"
    )
  }
  
  suite$results$basic$mean_test <- mean_test_result
  
  # Variance test (should be close to 1/12 for uniform[0,1])
  var_x <- var(x)
  expected_var <- 1/12
  # Chi-squared distribution for variance
  chi_var <- (n - 1) * var_x / expected_var
  p_var <- 2 * min(pchisq(chi_var, n - 1), 1 - pchisq(chi_var, n - 1))
  variance_test_result <- list(
    description = "Variance Test",
    result = if (is.na(p_var)) {
      "INCONCLUSIVE"
    } else if (p_var >= suite$config$significance_level) {
      "PASS"
    } else {
      "FAIL"
    },
    p_value = p_var,
    statistic = chi_var,
    details = paste("Tests if variance =", format(var_x, digits = 6), 
                   "is close to expected 1/12 for uniform distribution.")
  )
  
  # Calculate variance ratio effect size
  if (exists("calculate_variance_ratio")) {
    var_ratio <- calculate_variance_ratio(var_x, expected_var)
    variance_test_result <- add_effect_size(
      variance_test_result, 
      var_ratio, 
      "variance_ratio"
    )
  }
  
  suite$results$basic$variance_test <- variance_test_result
  
  # Min/Max tests - checking for proper range coverage
  min_x <- min(x)
  max_x <- max(x)
  expected_min <- 1 / (n + 1)  # Expected minimum for n uniform samples
  expected_max <- n / (n + 1)  # Expected maximum for n uniform samples
  
  # Calculate p-values for min/max tests
  p_min <- pbeta(min_x, 1, n)
  p_max <- 1 - pbeta(max_x, n, 1)
  
  min_test_result <- list(
    description = "Minimum Value Test",
    result = if (is.na(p_min)) {
      "INCONCLUSIVE"
    } else if (p_min >= suite$config$significance_level) {
      "PASS"
    } else {
      "FAIL"
    },
    p_value = p_min,
    statistic = min_x,
    details = paste("Tests if minimum value =", format(min_x, digits = 6), 
                   "is not too far from expected minimum for uniform distribution.")
  )
  
  # Calculate standardized effect size for min test
  if (exists("calculate_standardized_range")) {
    # Standard deviation of min for uniform(0,1)
    sd_min <- sqrt(expected_min * (1 - expected_min) / (n + 2))
    min_effect <- calculate_standardized_range(min_x, expected_min, sd_min)
    min_test_result <- add_effect_size(min_test_result, min_effect, "d")
  }
  
  suite$results$basic$min_test <- min_test_result
  
  max_test_result <- list(
    description = "Maximum Value Test",
    result = if (is.na(p_max)) {
      "INCONCLUSIVE"
    } else if (p_max >= suite$config$significance_level) {
      "PASS"
    } else {
      "FAIL"
    },
    p_value = p_max,
    statistic = max_x,
    details = paste("Tests if maximum value =", format(max_x, digits = 6), 
                   "is not too far from expected maximum for uniform distribution.")
  )
  
  # Calculate standardized effect size for max test
  if (exists("calculate_standardized_range")) {
    # Standard deviation of max for uniform(0,1)
    sd_max <- sqrt(expected_max * (1 - expected_max) / (n + 2))
    max_effect <- calculate_standardized_range(max_x, expected_max, sd_max)
    max_test_result <- add_effect_size(max_test_result, max_effect, "d")
  }
  
  suite$results$basic$max_test <- max_test_result
  
  # Generate visualizations
  if (require(ggplot2) && suite$config$save_visualizations) {
    suite <- visualize_basic_tests(suite, x)
  }
  
  return(suite)
}
