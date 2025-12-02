# File: correlation_tests.R
# ----------------------------------------------------------------------
#' Correlation tests for PRNG quality
#'
#' This module provides tests to evaluate the correlation properties
#' of the PRNG output, including autocorrelation, spectral analysis,
#' and serial correlation.

#' Run correlation tests
#'
#' @param suite The test suite object
#' @return Updated test suite with results
#' @keywords internal
run_correlation_tests <- function(suite) {
  # Generate random numbers
  n <- suite$config$correlation_sample_size
  x <- suite$prng_func(n)

  # Initialize results
  suite$results$correlation <- list()

  # Serial correlation test
  # Compare (x_1, x_2, ..., x_{n-1}) with (x_2, x_3, ..., x_n)
  serial_correlation <- function(x, lag = 1) {
    n <- length(x)
    if (n <= lag) {
      return(list(p.value = NA, statistic = NA))
    }

    x1 <- x[1:(n - lag)]
    x2 <- x[(lag + 1):n]

    # Calculate correlation
    cor_val <- cor(x1, x2)

    # For large n, Fisher's Z transformation gives a normal distribution
    z <- 0.5 * log((1 + cor_val) / (1 - cor_val))
    z_statistic <- z * sqrt(n - lag - 3)

    # Two-sided p-value
    p_value <- 2 * pnorm(-abs(z_statistic))

    list(p.value = p_value, statistic = cor_val)
  }

  # Run serial correlation tests with different lags
  lags <- c(1, 2, 3, 5, 10)
  for (lag in lags) {
    corr_result <- serial_correlation(x, lag)

    suite$results$correlation[[paste0("serial_lag_", lag)]] <- list(
      description = paste("Serial Correlation (lag =", lag, ")"),
      result = ifelse(!is.na(corr_result$p.value) &&
        corr_result$p.value >= suite$config$significance_level,
      "PASS", ifelse(is.na(corr_result$p.value), "INCONCLUSIVE", "FAIL")
      ),
      p_value = corr_result$p.value,
      statistic = corr_result$statistic,
      details = paste("Tests correlation between values with lag", lag)
    )
  }

  # ACF and PACF test
  # Calculate autocorrelation for multiple lags and test if any are significant
  acf_test <- function(x, max_lag = 20) {
    # Use cached version if available, otherwise fall back to regular acf
    if (exists("cached_acf", mode = "function")) {
      acf_vals <- cached_acf(x, lag.max = max_lag)$acf
    } else {
      acf_vals <- acf(x, lag.max = max_lag, plot = FALSE)$acf
    }
    acf_vals <- acf_vals[-1] # Remove lag 0 (always 1)

    # Critical value for 95% confidence
    n <- length(x)
    critical_value <- 1.96 / sqrt(n)

    # Check if any autocorrelations exceed critical value
    sig_count <- sum(abs(acf_vals) > critical_value)
    expected_sig <- 0.05 * max_lag # Expected false positives at 5% level

    # Binomial test for significant autocorrelations
    p_value <- 1 - pbinom(sig_count - 1, max_lag, 0.05)

    # Maximum autocorrelation
    max_abs_acf <- max(abs(acf_vals))

    list(
      p.value = p_value,
      statistic = max_abs_acf,
      sig_count = sig_count,
      expected_sig = expected_sig,
      acf_values = acf_vals
    )
  }

  # PACF test
  pacf_test <- function(x, max_lag = 20) {
    # Use cached version if available, otherwise fall back to regular pacf
    if (exists("cached_pacf", mode = "function")) {
      pacf_vals <- cached_pacf(x, lag.max = max_lag)$acf
    } else {
      pacf_vals <- pacf(x, lag.max = max_lag, plot = FALSE)$acf
    }

    # Critical value for 95% confidence
    n <- length(x)
    critical_value <- 1.96 / sqrt(n)

    # Check if any partial autocorrelations exceed critical value
    sig_count <- sum(abs(pacf_vals) > critical_value)
    expected_sig <- 0.05 * max_lag # Expected false positives at 5% level

    # Binomial test for significant partial autocorrelations
    p_value <- 1 - pbinom(sig_count - 1, max_lag, 0.05)

    # Maximum partial autocorrelation
    max_abs_pacf <- max(abs(pacf_vals))

    list(
      p.value = p_value,
      statistic = max_abs_pacf,
      sig_count = sig_count,
      expected_sig = expected_sig,
      pacf_values = pacf_vals
    )
  }

  # Run ACF test
  max_lag <- 20
  acf_result <- acf_test(x, max_lag)
  suite$results$correlation$acf_test <- list(
    description = "Autocorrelation Function Test",
    result = if (is.na(acf_result$p.value)) {
      "INCONCLUSIVE"
    } else if (acf_result$p.value >= suite$config$significance_level) {
      "PASS"
    } else {
      "FAIL"
    },
    p_value = acf_result$p.value,
    statistic = acf_result$statistic,
    details = paste(
      "Tests if autocorrelations exceed critical value.",
      "Significant lags:", acf_result$sig_count,
      "/ Expected:", round(acf_result$expected_sig, 2)
    )
  )

  # Run PACF test
  pacf_result <- pacf_test(x, max_lag)
  suite$results$correlation$pacf_test <- list(
    description = "Partial Autocorrelation Function Test",
    result = if (is.na(pacf_result$p.value)) {
      "INCONCLUSIVE"
    } else if (pacf_result$p.value >= suite$config$significance_level) {
      "PASS"
    } else {
      "FAIL"
    },
    p_value = pacf_result$p.value,
    statistic = pacf_result$statistic,
    details = paste(
      "Tests if partial autocorrelations exceed critical value.",
      "Significant lags:", pacf_result$sig_count,
      "/ Expected:", round(pacf_result$expected_sig, 2)
    )
  )

  # Spectral analysis
  spectral_test <- function(x) {
    # Calculate spectrum
    # Use cached version if available, otherwise fall back to regular spectrum
    if (exists("cached_spectrum", mode = "function")) {
      spec_result <- cached_spectrum(x, plot = FALSE)
    } else {
      spec_result <- spectrum(x, plot = FALSE)
    }

    # In a random sequence, spectral densities should be approximately exponential
    # We'll use Kolmogorov-Smirnov test to compare with exponential distribution
    spec_values <- spec_result$spec
    scaled_spec <- spec_values / mean(spec_values)

    # KS test against exponential(1)
    ks_result <- suppressWarnings(ks.test(scaled_spec, "pexp", 1))

    # Calculate peak-to-mean ratio
    peak_to_mean <- max(spec_values) / mean(spec_values)

    list(
      p.value = ks_result$p.value,
      statistic = ks_result$statistic,
      peak_to_mean = peak_to_mean,
      frequencies = spec_result$freq,
      spec_values = spec_values
    )
  }

  # Run spectral test
  spec_result <- spectral_test(x)
  suite$results$correlation$spectral_test <- list(
    description = "Spectral Analysis Test",
    result = if (is.na(spec_result$p.value)) {
      "INCONCLUSIVE"
    } else if (spec_result$p.value >= suite$config$significance_level) {
      "PASS"
    } else {
      "FAIL"
    },
    p_value = spec_result$p.value,
    statistic = spec_result$statistic,
    details = paste(
      "Tests if spectrum follows expected distribution.",
      "Peak/Mean ratio:", round(spec_result$peak_to_mean, 2)
    )
  )

  # Ljung-Box test for autocorrelation
  lb_test <- function(x, max_lag = 20) {
    # Apply Box-Ljung test
    result <- Box.test(x, lag = max_lag, type = "Ljung-Box")
    list(p.value = result$p.value, statistic = result$statistic)
  }

  # Run Ljung-Box test
  lb_result <- lb_test(x, max_lag)
  suite$results$correlation$ljung_box <- list(
    description = "Ljung-Box Test",
    result = if (is.na(lb_result$p.value)) {
      "INCONCLUSIVE"
    } else if (lb_result$p.value >= suite$config$significance_level) {
      "PASS"
    } else {
      "FAIL"
    },
    p_value = lb_result$p.value,
    statistic = lb_result$statistic,
    details = paste("Tests for overall autocorrelation up to lag", max_lag)
  )

  # Generate visualizations
  if (require(ggplot2) && suite$config$save_visualizations) {
    suite <- visualize_correlation_tests(
      suite, x,
      acf_result$acf_values,
      pacf_result$pacf_values,
      spec_result$frequencies,
      spec_result$spec_values
    )
  }

  return(suite)
}
