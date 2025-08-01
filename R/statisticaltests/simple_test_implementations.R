# File: simple_test_implementations.R
# ----------------------------------------------------------------------
#' Simple implementations of statistical tests for direct use with raw data
#'
#' This module provides simplified versions of statistical tests that work
#' directly with numeric vectors instead of suite objects.

#' Simple basic statistical tests
#' @export
simple_basic_tests <- function(x) {
  results <- list()
  
  # Kolmogorov-Smirnov test for uniformity
  ks_result <- ks.test(x, "punif", 0, 1)
  results$ks_test <- list(
    p_value = ks_result$p.value,
    statistic = as.numeric(ks_result$statistic),
    result = if (is.na(ks_result$p.value)) {
      "INCONCLUSIVE"
    } else if (ks_result$p.value >= 0.05) {
      "PASS"
    } else {
      "FAIL"
    },
    description = "Kolmogorov-Smirnov Test for Uniformity"
  )
  
  # Chi-squared goodness-of-fit test
  n <- length(x)
  n_bins <- min(20, max(5, floor(n/5)))
  bins <- cut(x, breaks = seq(0, 1, length.out = n_bins + 1), include.lowest = TRUE)
  observed <- table(bins)
  expected <- rep(n / n_bins, n_bins)
  
  if (length(observed) == n_bins && min(expected) >= 5) {
    chi_result <- chisq.test(observed, p = expected / sum(expected))
    results$chi_squared <- list(
      p_value = chi_result$p.value,
      statistic = as.numeric(chi_result$statistic),
      result = if (is.na(chi_result$p.value)) {
      "INCONCLUSIVE"
    } else if (chi_result$p.value >= 0.05) {
      "PASS"
    } else {
      "FAIL"
    },
      description = paste("Chi-squared test with", n_bins, "bins")
    )
  } else {
    results$chi_squared <- list(
      p_value = NA,
      statistic = NA,
      result = "INCONCLUSIVE",
      description = "Insufficient data for chi-squared test"
    )
  }
  
  # Mean test (should be close to 0.5)
  mean_x <- mean(x)
  z_mean <- (mean_x - 0.5) / (1/sqrt(12 * n))
  p_mean <- 2 * (1 - pnorm(abs(z_mean)))
  results$mean_test <- list(
    p_value = p_mean,
    statistic = z_mean,
    result = if (is.na(p_mean)) {
      "INCONCLUSIVE"
    } else if (p_mean >= 0.05) {
      "PASS"
    } else {
      "FAIL"
    },
    description = paste("Mean test, observed mean =", round(mean_x, 4))
  )
  
  # Variance test (should be close to 1/12)
  var_x <- var(x)
  chi_var <- (n - 1) * var_x / (1/12)
  p_var <- 2 * min(pchisq(chi_var, n - 1), 1 - pchisq(chi_var, n - 1))
  results$variance_test <- list(
    p_value = p_var,
    statistic = chi_var,
    result = if (is.na(p_var)) {
      "INCONCLUSIVE"
    } else if (p_var >= 0.05) {
      "PASS"
    } else {
      "FAIL"
    },
    description = paste("Variance test, observed variance =", round(var_x, 6))
  )
  
  return(results)
}

#' Simple runs test
#' @export
simple_runs_test <- function(x) {
  results <- list()
  
  # Runs test for randomness
  if (requireNamespace("randtests", quietly = TRUE)) {
    # Convert to binary based on median
    median_x <- median(x)
    binary_x <- as.integer(x > median_x)
    
    runs_result <- randtests::runs.test(binary_x)
    results$runs_test <- list(
      p_value = runs_result$p.value,
      statistic = runs_result$statistic,
      result = if (is.na(runs_result$p.value)) {
      "INCONCLUSIVE"
    } else if (runs_result$p.value >= 0.05) {
      "PASS"
    } else {
      "FAIL"
    },
      description = "Runs test for randomness"
    )
  } else {
    # Simple runs implementation
    binary_x <- as.integer(x > 0.5)
    runs <- rle(binary_x)$lengths
    n_runs <- length(runs)
    n0 <- sum(binary_x == 0)
    n1 <- sum(binary_x == 1)
    
    if (n0 > 0 && n1 > 0) {
      expected_runs <- (2 * n0 * n1) / (n0 + n1) + 1
      var_runs <- (2 * n0 * n1 * (2 * n0 * n1 - n0 - n1)) / 
                  ((n0 + n1)^2 * (n0 + n1 - 1))
      
      if (var_runs > 0) {
        z_stat <- (n_runs - expected_runs) / sqrt(var_runs)
        p_value <- 2 * (1 - pnorm(abs(z_stat)))
        
        results$runs_test <- list(
          p_value = p_value,
          statistic = z_stat,
          result = if (is.na(p_value)) {
      "INCONCLUSIVE"
    } else if (p_value >= 0.05) {
      "PASS"
    } else {
      "FAIL"
    },
          description = paste("Runs test, observed runs =", n_runs)
        )
      } else {
        results$runs_test <- list(
          p_value = NA,
          statistic = NA,
          result = "INCONCLUSIVE",
          description = "Insufficient variance for runs test"
        )
      }
    } else {
      results$runs_test <- list(
        p_value = NA,
        statistic = NA,
        result = "FAIL",
        description = "All values on one side of median"
      )
    }
  }
  
  return(results)
}

#' Simple correlation tests
#' @export
simple_correlation_tests <- function(x, max_lag = 10) {
  results <- list()
  
  # Serial correlation test (lag-1)
  if (length(x) > 2) {
    lag1_cor <- cor(x[-length(x)], x[-1])
    # Fisher z-transform for testing
    z <- 0.5 * log((1 + lag1_cor) / (1 - lag1_cor))
    se_z <- 1 / sqrt(length(x) - 3)
    p_value <- 2 * (1 - pnorm(abs(z / se_z)))
    
    results$serial_correlation <- list(
      p_value = p_value,
      statistic = lag1_cor,
      result = if (is.na(p_value)) {
      "INCONCLUSIVE"
    } else if (p_value >= 0.05) {
      "PASS"
    } else {
      "FAIL"
    },
      description = paste("Lag-1 serial correlation =", round(lag1_cor, 4))
    )
  }
  
  # Ljung-Box test
  if (length(x) > max_lag + 1) {
    acf_vals <- acf(x, lag.max = max_lag, plot = FALSE)$acf[-1]
    n <- length(x)
    lb_stat <- n * (n + 2) * sum((acf_vals^2) / (n - 1:max_lag))
    p_value <- 1 - pchisq(lb_stat, df = max_lag)
    
    results$ljung_box <- list(
      p_value = p_value,
      statistic = lb_stat,
      result = if (is.na(p_value)) {
      "INCONCLUSIVE"
    } else if (p_value >= 0.05) {
      "PASS"
    } else {
      "FAIL"
    },
      description = paste("Ljung-Box test with lag =", max_lag)
    )
  }
  
  return(results)
}

#' Simple frequency test for binary sequences
#' @export
simple_binary_tests <- function(x) {
  results <- list()
  
  # Convert to binary
  if (all(x >= 0 & x <= 1)) {
    binary_x <- as.integer(x > 0.5)
  } else {
    binary_x <- as.integer(x %% 2)
  }
  
  # Frequency test
  n_ones <- sum(binary_x)
  n <- length(binary_x)
  expected_ones <- n / 2
  
  # Normal approximation
  z_stat <- (n_ones - expected_ones) / sqrt(n / 4)
  p_value <- 2 * (1 - pnorm(abs(z_stat)))
  
  results$frequency_test <- list(
    p_value = p_value,
    statistic = z_stat,
    result = if (is.na(p_value)) {
      "INCONCLUSIVE"
    } else if (p_value >= 0.05) {
      "PASS"
    } else {
      "FAIL"
    },
    description = paste("Binary frequency test, proportion of 1s =", round(n_ones/n, 4))
  )
  
  # Runs test for binary
  if (n_ones > 0 && n_ones < n) {
    runs <- rle(binary_x)$lengths
    n_runs <- length(runs)
    n0 <- n - n_ones
    n1 <- n_ones
    
    expected_runs <- (2 * n0 * n1) / n + 1
    var_runs <- (2 * n0 * n1 * (2 * n0 * n1 - n)) / (n^2 * (n - 1))
    
    if (var_runs > 0) {
      z_runs <- (n_runs - expected_runs) / sqrt(var_runs)
      p_runs <- 2 * (1 - pnorm(abs(z_runs)))
      
      results$binary_runs <- list(
        p_value = p_runs,
        statistic = z_runs,
        result = if (is.na(p_runs)) {
      "INCONCLUSIVE"
    } else if (p_runs >= 0.05) {
      "PASS"
    } else {
      "FAIL"
    },
        description = paste("Binary runs test, observed runs =", n_runs)
      )
    }
  }
  
  return(results)
}
