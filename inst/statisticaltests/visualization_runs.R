# File: visualization_runs.R
# ----------------------------------------------------------------------
#' Visualization functions for runs and independence tests
#'
#' This file contains functions for visualizing the results of
#' runs and independence tests for PRNG quality evaluation.

#' Visualize runs tests
#'
#' @param suite The test suite object
#' @param x The generated random numbers
#' @param runs_above_below The results of runs above/below test
#' @param turning_points The results of turning points test
#' @return Updated test suite with visualization paths
#' @keywords internal
visualize_runs_tests <- function(suite, x, runs_above_below = NULL, turning_points = NULL) {
  # Set up output directory if it doesn't exist
  output_dir <- file.path(suite$config$output_dir, "visualizations", "runs")
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # 1. Sequence plot (first 1000 values)
  n_display <- min(1000, length(x))
  sequence_data <- data.frame(
    Index = 1:n_display,
    Value = x[1:n_display]
  )

  p1 <- ggplot2::ggplot(sequence_data, ggplot2::aes(x = Index, y = Value)) +
    ggplot2::geom_line(color = "steelblue", alpha = 0.7) +
    ggplot2::geom_hline(yintercept = 0.5, color = "red", linetype = "dashed") +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "Sequence of Random Values",
      subtitle = paste("First", n_display, "values with mean line (should appear random)"),
      x = "Sequence Index",
      y = "Value"
    )

  # Save plot
  sequence_path <- file.path(output_dir, "sequence_plot.png")
  ggplot2::ggsave(sequence_path, p1, width = 10, height = 6)

  # 2. Lagged scatter plot (x_i vs x_{i+1})
  lag_data <- data.frame(
    Value = x[1:(length(x) - 1)],
    Next_Value = x[2:length(x)]
  )

  p2 <- ggplot2::ggplot(lag_data, ggplot2::aes(x = Value, y = Next_Value)) +
    ggplot2::geom_point(alpha = 0.3, color = "steelblue") +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "Lag-1 Scatter Plot",
      subtitle = "Each value plotted against its successor (should show no pattern)",
      x = "Value (i)",
      y = "Value (i+1)"
    )

  # Save plot
  lag_path <- file.path(output_dir, "lag_scatter_plot.png")
  ggplot2::ggsave(lag_path, p2, width = 8, height = 8)

  # 3. Runs above/below median visualization
  if (!is.null(runs_above_below)) {
    # Create a sequence showing runs above/below median
    median_val <- 0.5
    above_below <- ifelse(x > median_val, 1, 0)

    # For visualization, we'll look at the first 200 values
    n_vis <- min(200, length(above_below))

    runs_data <- data.frame(
      Index = 1:n_vis,
      Value = x[1:n_vis],
      Above = above_below[1:n_vis]
    )

    p3 <- ggplot2::ggplot(runs_data, ggplot2::aes(x = Index, y = Value, color = factor(Above))) +
      ggplot2::geom_line() +
      ggplot2::geom_point() +
      ggplot2::geom_hline(yintercept = median_val, linetype = "dashed") +
      ggplot2::scale_color_manual(
        values = c("red", "blue"),
        labels = c("Below Median", "Above Median")
      ) +
      ggplot2::theme_minimal() +
      ggplot2::labs(
        title = "Runs Above and Below Median",
        subtitle = paste("First", n_vis, "values with runs highlighted"),
        x = "Sequence Index",
        y = "Value",
        color = "Position"
      )

    # Save plot
    runs_path <- file.path(output_dir, "runs_above_below_plot.png")
    ggplot2::ggsave(runs_path, p3, width = 10, height = 6)
  } else {
    runs_path <- NULL
  }

  # 4. Turning points visualization
  if (!is.null(turning_points)) {
    # Identify turning points
    n <- length(x)
    is_turning_point <- rep(FALSE, n)

    # A turning point occurs at index i if (x_{i-1} < x_i > x_{i+1}) or (x_{i-1} > x_i < x_{i+1})
    for (i in 2:(n - 1)) {
      is_turning_point[i] <- (x[i - 1] < x[i] && x[i] > x[i + 1]) ||
        (x[i - 1] > x[i] && x[i] < x[i + 1])
    }

    # For visualization, we'll look at the first 200 values
    n_vis <- min(200, n)

    turning_data <- data.frame(
      Index = 1:n_vis,
      Value = x[1:n_vis],
      Turning = is_turning_point[1:n_vis]
    )

    p4 <- ggplot2::ggplot(turning_data, ggplot2::aes(x = Index, y = Value)) +
      ggplot2::geom_line(color = "gray") +
      ggplot2::geom_point(
        data = turning_data[turning_data$Turning, ],
        ggplot2::aes(x = Index, y = Value),
        color = "red", size = 3
      ) +
      ggplot2::theme_minimal() +
      ggplot2::labs(
        title = "Turning Points in Sequence",
        subtitle = paste("First", n_vis, "values with turning points highlighted in red"),
        x = "Sequence Index",
        y = "Value"
      )

    # Save plot
    turning_path <- file.path(output_dir, "turning_points_plot.png")
    ggplot2::ggsave(turning_path, p4, width = 10, height = 6)
  } else {
    turning_path <- NULL
  }

  # 5. Gap test visualization (if we have gap test results)
  if (!is.null(suite$results$runs$gap_test_01)) {
    # Calculate gaps
    gap_bounds <- c(0, 0.1)
    in_range <- x >= gap_bounds[1] & x < gap_bounds[2]

    # Find indices of values in range
    indices_in_range <- which(in_range)

    # Calculate gaps
    gaps <- diff(indices_in_range) - 1

    # Create histogram of gaps
    max_gap <- max(gaps, 30) # For visualization, cap at 30
    gap_counts <- table(factor(pmin(gaps, max_gap), levels = 0:max_gap))

    # Expected geometric distribution
    p_in_range <- gap_bounds[2] - gap_bounds[1]
    expected_probs <- dgeom(0:max_gap, p_in_range)
    expected_counts <- length(gaps) * expected_probs

    gap_data <- data.frame(
      Gap = as.integer(names(gap_counts)),
      Observed = as.numeric(gap_counts),
      Expected = expected_counts
    )

    # Convert to long format for grouped bar chart
    gap_data_long <- reshape2::melt(gap_data,
      id.vars = "Gap",
      variable.name = "Type",
      value.name = "Count"
    )

    p5 <- ggplot2::ggplot(gap_data_long, ggplot2::aes(x = factor(Gap), y = Count, fill = Type)) +
      ggplot2::geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
      ggplot2::scale_fill_manual(values = c("steelblue", "orange")) +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)) +
      ggplot2::labs(
        title = "Gap Test Results (0.0-0.1)",
        subtitle = "Distribution of gaps between values in the range [0, 0.1)",
        x = "Gap Size",
        y = "Frequency",
        fill = ""
      )

    # Save plot
    gap_path <- file.path(output_dir, "gap_test_plot.png")
    ggplot2::ggsave(gap_path, p5, width = 10, height = 6)
  } else {
    gap_path <- NULL
  }

  # Store visualization paths in suite
  suite$visualizations$runs <- list(
    sequence_plot = sequence_path,
    lag_scatter_plot = lag_path,
    runs_above_below_plot = runs_path,
    turning_points_plot = turning_path,
    gap_test_plot = gap_path
  )

  return(suite)
}
