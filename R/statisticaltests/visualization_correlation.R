# File: visualization_correlation.R
# ----------------------------------------------------------------------
#' Visualization functions for correlation tests
#'
#' This file contains functions for visualizing the results of
#' correlation tests for PRNG quality evaluation.

#' Visualize correlation tests
#'
#' @param suite The test suite object
#' @param x The generated random numbers
#' @param acf_values Autocorrelation function values
#' @param pacf_values Partial autocorrelation function values
#' @param frequencies Spectral frequencies
#' @param spec_values Spectral values
#' @return Updated test suite with visualization paths
#' @keywords internal
visualize_correlation_tests <- function(suite, x, acf_values, pacf_values, 
                                      frequencies, spec_values) {
  # Set up output directory if it doesn't exist
  output_dir <- file.path(suite$config$output_dir, "visualizations", "correlation")
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # 1. Serial Correlation plot
  # Calculate lag-k correlations and plot them
  max_lag <- 20
  correlations <- numeric(max_lag)
  for (lag in 1:max_lag) {
    n <- length(x)
    if (lag < n) {
      correlations[lag] <- cor(x[1:(n-lag)], x[(lag+1):n])
    } else {
      correlations[lag] <- NA
    }
  }
  
  corr_data <- data.frame(
    Lag = 1:max_lag,
    Correlation = correlations
  )
  
  p1 <- ggplot2::ggplot(corr_data, ggplot2::aes(x = Lag, y = Correlation)) +
    ggplot2::geom_bar(stat = "identity", fill = "steelblue", width = 0.7) +
    ggplot2::geom_hline(yintercept = 0, color = "black", linetype = "solid") +
    # Add critical values
    ggplot2::geom_hline(yintercept = c(-1.96/sqrt(length(x)), 1.96/sqrt(length(x))), 
                     color = "red", linetype = "dashed") +
    ggplot2::scale_x_continuous(breaks = 1:max_lag) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "Serial Correlation by Lag",
      subtitle = "Correlation between values and their lagged versions (with critical values)",
      x = "Lag",
      y = "Correlation"
    )
  
  # Save plot
  serial_corr_path <- file.path(output_dir, "serial_correlation_plot.png")
  ggplot2::ggsave(serial_corr_path, p1, width = 10, height = 6)
  
  # 2. ACF Plot
  acf_data <- data.frame(
    Lag = 1:length(acf_values),
    ACF = acf_values
  )
  
  p2 <- ggplot2::ggplot(acf_data, ggplot2::aes(x = Lag, y = ACF)) +
    ggplot2::geom_bar(stat = "identity", fill = "steelblue", width = 0.7) +
    ggplot2::geom_hline(yintercept = 0, color = "black", linetype = "solid") +
    ggplot2::geom_hline(yintercept = c(-1.96/sqrt(length(x)), 1.96/sqrt(length(x))), 
                     color = "red", linetype = "dashed") +
    ggplot2::scale_x_continuous(breaks = seq(0, length(acf_values), by = 5)) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "Autocorrelation Function (ACF)",
      subtitle = "With critical values at 95% confidence (red dashed lines)",
      x = "Lag",
      y = "ACF"
    )
  
  # Save plot
  acf_path <- file.path(output_dir, "acf_plot.png")
  ggplot2::ggsave(acf_path, p2, width = 10, height = 6)
  
  # 3. PACF Plot
  pacf_data <- data.frame(
    Lag = 1:length(pacf_values),
    PACF = pacf_values
  )
  
  p3 <- ggplot2::ggplot(pacf_data, ggplot2::aes(x = Lag, y = PACF)) +
    ggplot2::geom_bar(stat = "identity", fill = "steelblue", width = 0.7) +
    ggplot2::geom_hline(yintercept = 0, color = "black", linetype = "solid") +
    ggplot2::geom_hline(yintercept = c(-1.96/sqrt(length(x)), 1.96/sqrt(length(x))), 
                     color = "red", linetype = "dashed") +
    ggplot2::scale_x_continuous(breaks = seq(0, length(pacf_values), by = 5)) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "Partial Autocorrelation Function (PACF)",
      subtitle = "With critical values at 95% confidence (red dashed lines)",
      x = "Lag",
      y = "PACF"
    )
  
  # Save plot
  pacf_path <- file.path(output_dir, "pacf_plot.png")
  ggplot2::ggsave(pacf_path, p3, width = 10, height = 6)
  
  # 4. Spectral Analysis Plot
  spec_data <- data.frame(
    Frequency = frequencies,
    Power = spec_values
  )
  
  p4 <- ggplot2::ggplot(spec_data, ggplot2::aes(x = Frequency, y = Power)) +
    ggplot2::geom_line(color = "steelblue") +
    ggplot2::geom_hline(yintercept = mean(spec_values), color = "red", linetype = "dashed") +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "Spectral Density",
      subtitle = "Power spectrum (with mean line in red)",
      x = "Frequency",
      y = "Spectral Density"
    )
  
  # Save plot
  spectral_path <- file.path(output_dir, "spectral_density_plot.png")
  ggplot2::ggsave(spectral_path, p4, width = 10, height = 6)
  
  # 5. Multiple Lag Scatter Plot
  # Create a data frame for different lags
  lags_to_plot <- c(1, 5, 10)
  lag_scatter_data <- data.frame()
  
  for (lag in lags_to_plot) {
    if (lag < length(x)) {
      temp_data <- data.frame(
        Value = x[1:(length(x)-lag)],
        Lagged = x[(lag+1):length(x)],
        Lag = paste("Lag", lag)
      )
      lag_scatter_data <- rbind(lag_scatter_data, temp_data)
    }
  }
  
  p5 <- ggplot2::ggplot(lag_scatter_data, ggplot2::aes(x = Value, y = Lagged)) +
    ggplot2::geom_point(alpha = 0.3, color = "steelblue") +
    ggplot2::facet_wrap(~Lag, scales = "free") +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "Lag Scatter Plots",
      subtitle = "Value vs. Lagged Value for Different Lags",
      x = "Value",
      y = "Lagged Value"
    )
  
  # Save plot
  multi_lag_path <- file.path(output_dir, "multiple_lag_scatter_plot.png")
  ggplot2::ggsave(multi_lag_path, p5, width = 12, height = 6)
  
  # Store visualization paths in suite
  suite$visualizations$correlation <- list(
    serial_correlation_plot = serial_corr_path,
    acf_plot = acf_path,
    pacf_plot = pacf_path,
    spectral_density_plot = spectral_path,
    multiple_lag_scatter_plot = multi_lag_path
  )
  
  return(suite)
}
