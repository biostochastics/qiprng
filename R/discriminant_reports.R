# Discriminant Analysis Report Generation
# Generates comprehensive reports from discriminant testing results

# Suppress NOTE for global variables in ggplot aes()
utils::globalVariables(c(
  "a", "b", "c", "discriminant", "overall_score",
  "quality_rating", "empirical_rating", "rating", "count", "test",
  "pass_rate", "p_value", "pvalue", "n_sig_lags"
))

#' Generate summary statistics from test results
#'
#' @param results List of test results from run_discriminant_analysis
#' @return Data frame with summary statistics
generate_summary_stats <- function(results) {
  # Safe extraction function
  safe_extract <- function(x, path, default = NA) {
    tryCatch(
      {
        result <- x
        for (step in path) {
          if (is.null(result) || !step %in% names(result)) {
            return(default)
          }
          result <- result[[step]]
        }
        if (is.null(result)) default else result
      },
      error = function(e) default
    )
  }

  # Extract key metrics with safe extraction
  # Pre-compute autocorrelation diagnostics
  sig_lags_list <- lapply(results, function(x) safe_extract(x, c("autocorrelation", "significant_lags"), list()))
  n_sig_lags <- sapply(sig_lags_list, function(l) if (is.null(l)) NA_integer_ else length(l))
  max_abs_acf <- sapply(results, function(x) {
    ac <- safe_extract(x, c("autocorrelation", "autocorrelations"), NA)
    if (is.numeric(ac)) max(abs(ac), na.rm = TRUE) else NA
  })

  summary_data <- data.frame(
    index = sapply(results, function(x) safe_extract(x, "index", NA)),
    a = sapply(results, function(x) safe_extract(x, c("parameters", "a"), NA)),
    b = sapply(results, function(x) safe_extract(x, c("parameters", "b"), NA)),
    c = sapply(results, function(x) safe_extract(x, c("parameters", "c"), NA)),
    discriminant = sapply(results, function(x) safe_extract(x, c("parameters", "discriminant"), NA)),
    overall_score = sapply(results, function(x) safe_extract(x, "overall_score", 0)),
    quality_rating = sapply(results, function(x) safe_extract(x, "quality_rating", "Error")),
    uniformity_passed = sapply(results, function(x) safe_extract(x, c("uniformity", "passed"), FALSE)),
    independence_passed = sapply(results, function(x) safe_extract(x, c("independence", "passed"), FALSE)),
    ks_p_value = sapply(results, function(x) safe_extract(x, c("uniformity", "ks_p_value"), NA)),
    chi_sq_p_value = sapply(results, function(x) safe_extract(x, c("uniformity", "chi_sq_p_value"), NA)),
    independence_p_value = sapply(results, function(x) safe_extract(x, c("independence", "p_value"), NA)),
    autocorrelation_passed = sapply(results, function(x) safe_extract(x, c("autocorrelation", "passed"), FALSE)),
    periodicity_p_value = sapply(results, function(x) safe_extract(x, c("periodicity", "p_value"), NA)),
    periodicity_passed = sapply(results, function(x) safe_extract(x, c("periodicity", "passed"), FALSE)),
    n_sig_lags = n_sig_lags,
    max_abs_acf = max_abs_acf,
    mean_error = sapply(results, function(x) safe_extract(x, c("moments", "mean_error"), NA)),
    var_error = sapply(results, function(x) safe_extract(x, c("moments", "var_error"), NA)),
    stringsAsFactors = FALSE
  )

  # Empirical quality bands based on overall_score quantiles
  qu <- quantile(summary_data$overall_score, probs = c(0.40, 0.60, 0.75, 0.90), na.rm = TRUE)

  # Handle case where quantiles are not unique (many identical scores)
  breaks <- c(-Inf, qu[1], qu[2], qu[3], qu[4], Inf)
  unique_breaks <- unique(breaks)

  if (length(unique_breaks) < length(breaks)) {
    # Fallback to simple fixed thresholds if quantiles aren't unique
    cat("Warning: Quantiles not unique, using fixed empirical thresholds\n")
    summary_data$empirical_rating <- cut(
      summary_data$overall_score,
      breaks = c(-Inf, 0.40, 0.60, 0.75, 0.90, Inf),
      labels = c("Poor", "Fair", "Good", "Very-Good", "Excellent"),
      right = TRUE
    )
  } else {
    summary_data$empirical_rating <- cut(
      summary_data$overall_score,
      breaks = breaks,
      labels = c("Poor", "Fair", "Good", "Very-Good", "Excellent"),
      right = TRUE
    )
  }

  return(summary_data)
}

#' Create visualizations for discriminant analysis
#'
#' @param summary_data Summary statistics data frame
#' @return List of ggplot objects
create_visualizations <- function(summary_data) {
  plots <- list()

  # 1. Overall Score Distribution
  plots$score_dist <- ggplot(summary_data, aes(x = overall_score)) +
    geom_histogram(bins = 20, fill = "skyblue", alpha = 0.7, color = "black") +
    labs(
      title = "Distribution of Overall Quality Scores",
      x = "Overall Score (0-1)", y = "Count"
    ) +
    theme_minimal()

  # 2. Quality Rating Pie Chart
  quality_counts <- table(summary_data$empirical_rating)
  quality_df <- data.frame(
    rating = names(quality_counts),
    count = as.numeric(quality_counts)
  )

  plots$quality_pie <- ggplot(quality_df, aes(x = "", y = count, fill = rating)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y", start = 0) +
    labs(title = "Distribution of Quality Ratings") +
    theme_void() +
    scale_fill_brewer(palette = "Set3")

  # 3. Discriminant vs Overall Score
  plots$discriminant_score <- ggplot(summary_data, aes(x = discriminant, y = overall_score)) +
    geom_point(alpha = 0.6, color = "darkblue") +
    geom_smooth(method = "loess", se = TRUE, color = "red") +
    labs(
      title = "Discriminant Value vs Overall Quality Score",
      x = "Discriminant Value", y = "Overall Score"
    ) +
    theme_minimal()

  # 4. Test Pass Rates
  test_pass_data <- data.frame(
    test = c("Uniformity", "Independence", "Autocorrelation", "Periodicity"),
    pass_rate = c(
      mean(summary_data$uniformity_passed, na.rm = TRUE),
      mean(summary_data$independence_passed, na.rm = TRUE),
      mean(summary_data$autocorrelation_passed, na.rm = TRUE),
      mean(summary_data$periodicity_passed, na.rm = TRUE)
    )
  )

  plots$test_pass_rates <- ggplot(test_pass_data, aes(x = test, y = pass_rate)) +
    geom_bar(stat = "identity", fill = "lightgreen", color = "black") +
    labs(
      title = "Test Pass Rates Across All Discriminants",
      x = "Test Type", y = "Pass Rate"
    ) +
    ylim(0, 1) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  # 5. Parameter Relationships
  plots$param_relationships <- ggplot(summary_data, aes(x = a, y = abs(c), color = overall_score)) +
    geom_point(alpha = 0.7) +
    scale_color_gradient(low = "red", high = "green", name = "Quality Score") +
    labs(
      title = "Parameter Relationships (a vs |c|)",
      x = "Parameter a", y = "Absolute value of parameter c"
    ) +
    theme_minimal()

  # 6. Autocorrelation diagnostics
  plots$autocorr_hist <- ggplot(summary_data, aes(x = n_sig_lags)) +
    geom_histogram(bins = 20, fill = "salmon", color = "black") +
    labs(title = "Distribution of Significant Autocorrelation Lags", x = "# Significant Lags", y = "Count") +
    theme_minimal()

  plots$autocorr_vs_score <- ggplot(summary_data, aes(x = n_sig_lags, y = overall_score)) +
    geom_point(alpha = 0.6, color = "purple") +
    labs(title = "Quality Score vs # Significant Autocorrelation Lags", x = "# Significant Lags", y = "Overall Score") +
    theme_minimal()

  # 7. P-value distributions
  p_values <- data.frame(
    discriminant = rep(summary_data$discriminant, 4),
    pvalue = c(
      summary_data$ks_p_value, summary_data$chi_sq_p_value,
      summary_data$independence_p_value, summary_data$periodicity_p_value
    ),
    test = rep(c("Uniformity (KS)", "Uniformity (Chi-Sq)", "Independence (Runs)", "Periodicity (Fisher)"),
      each = nrow(summary_data)
    )
  )
  p_values <- p_values[!is.na(p_values$pvalue), ]

  plots$pvalue_dist <- ggplot(p_values, aes(x = pvalue, fill = test)) +
    geom_histogram(bins = 20, alpha = 0.7, position = "identity") +
    facet_wrap(~test, scales = "free_y") +
    labs(
      title = "Distribution of P-values by Test Type",
      x = "P-value", y = "Count"
    ) +
    theme_minimal()

  return(plots)
}

#' Generate detailed report for top and bottom performers
#'
#' @param results List of test results
#' @param summary_data Summary statistics data frame
#' @param n_top Number of top performers to report
#' @param n_bottom Number of bottom performers to report
#' @return Character string with detailed report
generate_detailed_report <- function(results, summary_data, n_top = 5, n_bottom = 5) {
  # Sort by overall score
  sorted_indices <- order(summary_data$overall_score, decreasing = TRUE)

  report <- paste0(
    "# Discriminant Analysis Detailed Report\n\n",
    "## Executive Summary\n\n",
    "Total discriminants tested: ", nrow(summary_data), "\n",
    "Sample size per discriminant: ", ifelse(length(results) > 0, results[[1]]$sample_size, "N/A"), "\n\n",
    "### Overall Performance:\n",
    "- Excellent (score ≥ 0.75): ", sum(summary_data$quality_rating == "Excellent"), " (",
    round(100 * mean(summary_data$quality_rating == "Excellent"), 1), "%)\n",
    "- Good (score ≥ 0.50): ", sum(summary_data$quality_rating == "Good"), " (",
    round(100 * mean(summary_data$quality_rating == "Good"), 1), "%)\n",
    "- Fair (score ≥ 0.25): ", sum(summary_data$quality_rating == "Fair"), " (",
    round(100 * mean(summary_data$quality_rating == "Fair"), 1), "%)\n",
    "- Poor (score < 0.25): ", sum(summary_data$quality_rating == "Poor"), " (",
    round(100 * mean(summary_data$quality_rating == "Poor"), 1), "%)\n",
    "- Errors: ", sum(summary_data$quality_rating == "Error"), " (",
    round(100 * mean(summary_data$quality_rating == "Error"), 1), "%)\n\n"
  )

  # Test-specific pass rates
  report <- paste0(
    report,
    "### Test Pass Rates:\n",
    "- Uniformity (Kolmogorov-Smirnov): ", round(100 * mean(summary_data$uniformity_passed, na.rm = TRUE), 1), "%\n",
    "- Independence (Runs Test): ", round(100 * mean(summary_data$independence_passed, na.rm = TRUE), 1), "%\n",
    "- Autocorrelation: ", round(100 * mean(summary_data$autocorrelation_passed, na.rm = TRUE), 1), "%\n",
    "- Periodicity (Spectral): ", round(100 * mean(summary_data$periodicity_passed, na.rm = TRUE), 1), "%\n\n"
  )

  # Top performers
  report <- paste0(report, "## Top ", n_top, " Performing Discriminants\n\n")

  for (i in seq_len(min(n_top, nrow(summary_data)))) {
    idx <- sorted_indices[i]
    row <- summary_data[idx, ]
    result <- results[[row$index]]

    report <- paste0(
      report,
      "### Rank ", i, ": Discriminant #", row$index, "\n",
      "**Parameters:** a=", row$a, ", b=", row$b, ", c=", row$c, ", Δ=", row$discriminant, "\n",
      "**Overall Score:** ", round(row$overall_score, 3), " (", row$quality_rating, ")\n\n",
      "**Test Results:**\n"
    )

    if (!is.null(result$uniformity)) {
      report <- paste0(
        report,
        "- Uniformity: ", result$uniformity$interpretation,
        " (KS p=", round(result$uniformity$ks_p_value, 4), ", ChiSq p=",
        round(result$uniformity$chi_sq_p_value, 4), ")\n"
      )
    }

    if (!is.null(result$independence)) {
      independence_status <- "✗ FAIL"
      if (!is.null(result$independence$passed) && length(result$independence$passed) > 0 && !is.na(result$independence$passed) && result$independence$passed) {
        independence_status <- "✓ PASS"
      }
      report <- paste0(
        report,
        "- Independence: ", independence_status,
        " (p=", round(result$independence$p_value, 4), ")\n"
      )
    }

    if (!is.null(result$autocorrelation)) {
      autocorr_status <- "✗ FAIL"
      if (!is.null(result$autocorrelation$passed) && length(result$autocorrelation$passed) > 0 && !is.na(result$autocorrelation$passed) && result$autocorrelation$passed) {
        autocorr_status <- "✓ PASS"
      }
      report <- paste0(
        report,
        "- Autocorrelation: ", autocorr_status,
        " (", result$autocorrelation$interpretation, ")\n"
      )
    }

    if (!is.null(result$periodicity)) {
      periodicity_status <- "✗ FAIL"
      if (!is.null(result$periodicity$passed) && isTRUE(result$periodicity$passed)) {
        periodicity_status <- "✓ PASS"
      }
      report <- paste0(
        report,
        "- Periodicity: ", periodicity_status,
        " (", result$periodicity$interpretation, ")\n"
      )
    }

    if (!is.null(result$moments)) {
      report <- paste0(
        report,
        "- Mean error: ", round(result$moments$mean_error, 6), "\n",
        "- Variance error: ", round(result$moments$var_error, 6), "\n"
      )
    }

    report <- paste0(report, "\n")
  }

  # Bottom performers
  report <- paste0(report, "## Bottom ", n_bottom, " Performing Discriminants\n\n")

  bottom_start <- max(1, nrow(summary_data) - n_bottom + 1)
  for (i in bottom_start:nrow(summary_data)) {
    idx <- sorted_indices[i]
    row <- summary_data[idx, ]
    result <- results[[row$index]]
    rank <- nrow(summary_data) - i + 1

    report <- paste0(
      report,
      "### Rank ", i, " (Bottom ", rank, "): Discriminant #", row$index, "\n",
      "**Parameters:** a=", row$a, ", b=", row$b, ", c=", row$c, ", Δ=", row$discriminant, "\n",
      "**Overall Score:** ", round(row$overall_score, 3), " (", row$quality_rating, ")\n\n"
    )

    if (!is.null(result$error)) {
      report <- paste0(report, "**Error:** ", result$error, "\n\n")
    } else {
      report <- paste0(report, "**Failed Tests:**\n")

      if (!is.null(result$uniformity) && !result$uniformity$passed) {
        report <- paste0(
          report, "- Uniformity: ", result$uniformity$interpretation,
          " (KS p=", round(result$uniformity$ks_p_value, 4), ", ChiSq p=",
          round(result$uniformity$chi_sq_p_value, 4), ")\n"
        )
      }
      if (!is.null(result$independence) && !result$independence$passed) {
        report <- paste0(report, "- Independence (p=", round(result$independence$p_value, 4), ")\n")
      }
      if (!is.null(result$autocorrelation) && !result$autocorrelation$passed) {
        report <- paste0(report, "- Autocorrelation\n")
      }
      if (!is.null(result$periodicity) && !isTRUE(result$periodicity$passed)) {
        report <- paste0(report, "- Periodicity\n")
      }

      report <- paste0(report, "\n")
    }
  }

  return(report)
}

#' Save all reports and visualizations
#'
#' @param results List of test results
#' @param output_dir Directory to save outputs
save_analysis_results <- function(results, output_dir = "discriminant_analysis_results") {
  # Create output directory
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Generate summary statistics
  summary_data <- generate_summary_stats(results)

  # Save summary data
  write.csv(summary_data, file.path(output_dir, "summary_statistics.csv"), row.names = FALSE)

  # Create and save visualizations
  plots <- create_visualizations(summary_data)

  # Save individual plots
  ggsave(file.path(output_dir, "score_distribution.png"), plots$score_dist, width = 10, height = 6)
  ggsave(file.path(output_dir, "quality_ratings.png"), plots$quality_pie, width = 8, height = 6)
  ggsave(file.path(output_dir, "discriminant_vs_score.png"), plots$discriminant_score, width = 10, height = 6)
  ggsave(file.path(output_dir, "test_pass_rates.png"), plots$test_pass_rates, width = 10, height = 6)
  ggsave(file.path(output_dir, "parameter_relationships.png"), plots$param_relationships, width = 10, height = 6)
  ggsave(file.path(output_dir, "pvalue_distributions.png"), plots$pvalue_dist, width = 12, height = 6)

  # Create combined plot
  combined_plot <- grid.arrange(
    plots$score_dist, plots$quality_pie,
    plots$test_pass_rates, plots$discriminant_score,
    ncol = 2, nrow = 2
  )
  ggsave(file.path(output_dir, "combined_analysis.png"), combined_plot, width = 16, height = 12)

  # Generate and save detailed report
  detailed_report <- generate_detailed_report(results, summary_data)
  writeLines(detailed_report, file.path(output_dir, "detailed_report.md"))

  # Save raw results
  saveRDS(results, file.path(output_dir, "raw_results.rds"))

  cat("Analysis results saved to:", output_dir, "\n")
  cat("Files created:\n")
  cat("- summary_statistics.csv\n")
  cat("- detailed_report.md\n")
  cat("- Various visualization PNG files\n")
  cat("- raw_results.rds\n")

  return(list(
    summary_data = summary_data,
    plots = plots,
    detailed_report = detailed_report
  ))
}
