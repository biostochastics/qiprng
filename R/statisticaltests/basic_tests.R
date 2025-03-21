# File: basic_tests.R
# ----------------------------------------------------------------------
#' Basic distribution tests for PRNG quality
#'
#' This module provides basic statistical tests to evaluate the 
#' distribution properties of a PRNG, including tests for uniformity,
#' goodness-of-fit, and basic statistical properties.

#' Run basic distribution tests
#'
#' @param suite The test suite object
#' @return Updated test suite with results
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
  suite$results$basic$ks_test <- list(
    description = "Kolmogorov-Smirnov Test for Uniformity",
    result = ifelse(ks_result$p.value >= suite$config$significance_level, "PASS", "FAIL"),
    p_value = ks_result$p.value,
    statistic = ks_result$statistic,
    details = paste("Tests if the distribution is uniform on [0,1].",
                   "p-value should be above significance level for uniformity.")
  )
  
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
  suite$results$basic$chi_squared <- list(
    description = "Chi-squared Goodness-of-Fit Test",
    result = ifelse(!is.na(chi_result$p.value) && 
                   chi_result$p.value >= suite$config$significance_level, 
                   "PASS", ifelse(is.na(chi_result$p.value), "INCONCLUSIVE", "FAIL")),
    p_value = chi_result$p.value,
    statistic = chi_result$statistic,
    diagnostics = list(bins_used = bins_count, sample_size = n),
    details = paste("Tests if frequencies in", bins_count, 
                   "bins match expected uniform frequencies.")
  )
  
  # Mean test (should be close to 0.5 for uniform[0,1])
  mean_x <- mean(x)
  expected_mean <- 0.5
  z_mean <- (mean_x - expected_mean) / (1/sqrt(12 * n))  # Using variance of uniform(0,1)
  p_mean <- 2 * (1 - pnorm(abs(z_mean)))
  suite$results$basic$mean_test <- list(
    description = "Mean Test",
    result = ifelse(p_mean >= suite$config$significance_level, "PASS", "FAIL"),
    p_value = p_mean,
    statistic = z_mean,
    details = paste("Tests if mean =", format(mean_x, digits = 6), 
                   "is close to expected 0.5 for uniform distribution.")
  )
  
  # Variance test (should be close to 1/12 for uniform[0,1])
  var_x <- var(x)
  expected_var <- 1/12
  # Chi-squared distribution for variance
  chi_var <- (n - 1) * var_x / expected_var
  p_var <- 2 * min(pchisq(chi_var, n - 1), 1 - pchisq(chi_var, n - 1))
  suite$results$basic$variance_test <- list(
    description = "Variance Test",
    result = ifelse(p_var >= suite$config$significance_level, "PASS", "FAIL"),
    p_value = p_var,
    statistic = chi_var,
    details = paste("Tests if variance =", format(var_x, digits = 6), 
                   "is close to expected 1/12 for uniform distribution.")
  )
  
  # Min/Max tests - checking for proper range coverage
  min_x <- min(x)
  max_x <- max(x)
  expected_min <- 1 / (n + 1)  # Expected minimum for n uniform samples
  expected_max <- n / (n + 1)  # Expected maximum for n uniform samples
  
  # Calculate p-values for min/max tests
  p_min <- pbeta(min_x, 1, n)
  p_max <- 1 - pbeta(max_x, n, 1)
  
  suite$results$basic$min_test <- list(
    description = "Minimum Value Test",
    result = ifelse(p_min >= suite$config$significance_level, "PASS", "FAIL"),
    p_value = p_min,
    statistic = min_x,
    details = paste("Tests if minimum value =", format(min_x, digits = 6), 
                   "is not too far from expected minimum for uniform distribution.")
  )
  
  suite$results$basic$max_test <- list(
    description = "Maximum Value Test",
    result = ifelse(p_max >= suite$config$significance_level, "PASS", "FAIL"),
    p_value = p_max,
    statistic = max_x,
    details = paste("Tests if maximum value =", format(max_x, digits = 6), 
                   "is not too far from expected maximum for uniform distribution.")
  )
  
  # Generate visualizations
  if (require(ggplot2) && suite$config$save_visualizations) {
    suite <- visualize_basic_tests(suite, x)
  }
  
  return(suite)
}
