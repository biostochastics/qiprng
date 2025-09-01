# File: visualization_basic.R
# ----------------------------------------------------------------------
#' Visualization functions for basic distribution tests
#'
#' This module provides specialized visualization functions for the results of
#' basic distribution tests in PRNG quality evaluation. The visualizations help
#' in identifying patterns and anomalies that might not be apparent from
#' numerical test results alone.
#'
#' The visualizations implemented include:
#' \itemize{
#'   \item Histograms with expected distribution overlays
#'   \item Q-Q plots comparing empirical vs. theoretical distributions
#'   \item Cumulative distribution function (CDF) comparisons
#'   \item Lag plots for visual identification of correlations
#'   \item Series plots to visualize the sequence of random numbers
#' }
#'
#' All visualizations are generated using ggplot2 and saved in a structured
#' directory hierarchy for easy reference in reports.
#'
#' @name visualization_basic
#' @aliases visualization-basic
#' @keywords internal

#' Create visualizations for basic distribution test results
#'
#' Generates a comprehensive set of visualizations to help interpret the
#' results of basic distribution tests on random number generators. These
#' visualizations are designed to make statistical properties visually apparent
#' and to complement the numerical test results.
#'
#' @param suite The test suite object containing configuration and results
#' @param x The generated random numbers to visualize
#' @return Updated test suite with visualization paths added to suite$visualizations$basic
#' @details
#' This function creates the following visualizations:
#' \itemize{
#'   \item Histogram with expected uniform distribution overlay
#'   \item Q-Q plot comparing sample quantiles to theoretical uniform quantiles
#'   \item Cumulative distribution function comparison
#'   \item Series plot showing the sequence of generated numbers
#'   \item Lag-1 scatter plot to visualize sequential dependencies
#'   \item Box plot showing distribution characteristics
#' }
#'
#' All visualizations are saved as PNG files in the configured output directory
#' under visualizations/basic/ and the paths are stored in the test suite object.
#' @keywords internal
visualize_basic_tests <- function(suite, x) {
  # Set up output directory if it doesn't exist
  output_dir <- file.path(suite$config$output_dir, "visualizations", "basic")
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # 1. Histogram of random values
  p1 <- ggplot2::ggplot(data.frame(Value = x), ggplot2::aes(x = Value)) +
    ggplot2::geom_histogram(bins = 30, fill = "steelblue", color = "white", alpha = 0.7) +
    ggplot2::stat_function(
      fun = function(x) length(x) * (1 / 30),
      color = "red", linetype = "dashed", size = 1
    ) + # Expected uniform density
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "Distribution of Random Values",
      subtitle = "Histogram with expected uniform distribution (dashed line)",
      x = "Value",
      y = "Frequency"
    )

  # Save plot
  hist_path <- file.path(output_dir, "histogram.png")
  ggplot2::ggsave(hist_path, p1, width = 8, height = 6)

  # 2. QQ-plot against uniform distribution
  uniform_quantiles <- qunif(ppoints(length(x)))
  p2 <- ggplot2::ggplot(
    data.frame(Theoretical = uniform_quantiles, Empirical = sort(x)),
    ggplot2::aes(x = Theoretical, y = Empirical)
  ) +
    ggplot2::geom_point(alpha = 0.5, color = "steelblue") +
    ggplot2::geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "Q-Q Plot Against Uniform Distribution",
      subtitle = "Points should follow the line for a uniform distribution",
      x = "Theoretical Quantiles",
      y = "Empirical Quantiles"
    )

  # Save plot
  qq_path <- file.path(output_dir, "qq_plot.png")
  ggplot2::ggsave(qq_path, p2, width = 8, height = 6)

  # 3. Cumulative distribution function
  cdf_data <- data.frame(
    Value = sort(x),
    Empirical = (1:length(x)) / length(x),
    Theoretical = punif(sort(x))
  )

  p3 <- ggplot2::ggplot(cdf_data) +
    ggplot2::geom_step(ggplot2::aes(x = Value, y = Empirical), color = "steelblue") +
    ggplot2::geom_line(ggplot2::aes(x = Value, y = Theoretical),
      color = "red", linetype = "dashed"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "Cumulative Distribution Function",
      subtitle = "Empirical CDF (blue) vs. Theoretical Uniform CDF (dashed red)",
      x = "Value",
      y = "Cumulative Probability"
    )

  # Save plot
  cdf_path <- file.path(output_dir, "cdf_plot.png")
  ggplot2::ggsave(cdf_path, p3, width = 8, height = 6)

  # 4. Density plot
  p4 <- ggplot2::ggplot(data.frame(Value = x), ggplot2::aes(x = Value)) +
    ggplot2::geom_density(fill = "steelblue", alpha = 0.3) +
    ggplot2::geom_hline(yintercept = 1, color = "red", linetype = "dashed") + # Expected uniform density
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "Density Plot of Random Values",
      subtitle = "Empirical density with expected uniform density (dashed line)",
      x = "Value",
      y = "Density"
    )

  # Save plot
  density_path <- file.path(output_dir, "density_plot.png")
  ggplot2::ggsave(density_path, p4, width = 8, height = 6)

  # 5. Deviation from expected values (residuals)
  bins <- 30
  hist_data <- hist(x, breaks = bins, plot = FALSE)
  expected <- length(x) / bins

  residuals_data <- data.frame(
    Bin = hist_data$mids,
    Residual = (hist_data$counts - expected) / sqrt(expected) # Standardized residuals
  )

  p5 <- ggplot2::ggplot(residuals_data, ggplot2::aes(x = Bin, y = Residual)) +
    ggplot2::geom_bar(stat = "identity", fill = "steelblue", alpha = 0.7) +
    ggplot2::geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
    ggplot2::geom_hline(yintercept = c(-2, 2), color = "orange", linetype = "dotted") +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "Standardized Residuals from Uniform Distribution",
      subtitle = "Deviations from expected frequencies (dotted lines at ±2 std dev)",
      x = "Bin Center",
      y = "Standardized Residual"
    )

  # Save plot
  residuals_path <- file.path(output_dir, "residuals_plot.png")
  ggplot2::ggsave(residuals_path, p5, width = 10, height = 6)

  # Store visualization paths in suite
  suite$visualizations$basic <- list(
    histogram = hist_path,
    qq_plot = qq_path,
    cdf_plot = cdf_path,
    density_plot = density_path,
    residuals_plot = residuals_path
  )

  return(suite)
}
