# File: comprehensive_visualizations.R
# ----------------------------------------------------------------------
#' Comprehensive visualization functions for generator comparison
#'
#' This module provides advanced visualizations for the comprehensive
#' generator comparison framework.

#' Generate all visualizations
#' @export
generate_all_visualizations <- function(
    suite, internal, external, multidim, performance,
    output_dir, verbose = TRUE) {
  # Check for required packages
  required_packages <- c("ggplot2", "gridExtra", "viridis")
  missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]

  if (length(missing_packages) > 0) {
    warning(sprintf(
      "Missing packages for full visualization: %s",
      paste(missing_packages, collapse = ", ")
    ))
    return(NULL)
  }

  plots <- list()

  # 1. Test Result Heatmap
  if (verbose) cat("  Creating test result heatmap...\n")
  plots$heatmap <- create_test_result_heatmap(suite, output_dir)

  # 2. P-value Distribution Plots
  if (verbose) cat("  Creating p-value distribution plots...\n")
  plots$pvalue_dist <- create_pvalue_distribution_plots(suite, output_dir)

  # 3. Test Statistic Distributions
  if (verbose) cat("  Creating test statistic distributions...\n")
  plots$stat_dist <- create_statistic_distribution_plots(suite, internal, output_dir)

  # 4. Performance Comparison
  if (verbose) cat("  Creating performance comparison plots...\n")
  plots$performance <- create_performance_plots(performance, output_dir)

  # 5. Category-wise Comparison
  if (verbose) cat("  Creating category-wise comparison plots...\n")
  plots$category <- create_category_comparison_plots(suite, output_dir)

  # 6. Time Series Analysis (for correlation tests)
  if (verbose) cat("  Creating time series analysis plots...\n")
  plots$timeseries <- create_timeseries_plots(internal, output_dir)

  # 7. Sample Size Sensitivity Analysis
  if (verbose) cat("  Creating sample size sensitivity plots...\n")
  plots$sensitivity <- create_sensitivity_plots(suite, output_dir)

  # 8. Generator Ranking Visualization
  if (verbose) cat("  Creating generator ranking plots...\n")
  plots$ranking <- create_ranking_plots(suite, output_dir)

  return(plots)
}

#' Create test result heatmap
#' @export
create_test_result_heatmap <- function(suite, output_dir) {
  library(ggplot2)
  library(viridis)

  # Extract test results into matrix format
  test_matrix <- extract_test_matrix(suite)

  if (is.null(test_matrix) || nrow(test_matrix) == 0) {
    return(NULL)
  }

  # Convert to long format for ggplot
  plot_data <- expand.grid(
    Test = rownames(test_matrix),
    Generator = colnames(test_matrix),
    stringsAsFactors = FALSE
  )

  plot_data$Result <- as.vector(test_matrix)
  plot_data$ResultNum <- ifelse(plot_data$Result == "PASS", 1,
    ifelse(plot_data$Result == "FAIL", 0, NA)
  )

  # Create heatmap
  p <- ggplot(plot_data, aes(x = Generator, y = Test, fill = Result)) +
    geom_tile(color = "white", size = 0.5) +
    scale_fill_manual(
      values = c("PASS" = "#27ae60", "FAIL" = "#e74c3c", "ERROR" = "#f39c12"),
      na.value = "grey90"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
      axis.text.y = element_text(size = 8),
      plot.title = element_text(size = 16, face = "bold"),
      legend.position = "bottom"
    ) +
    labs(
      title = "Comprehensive Test Results Heatmap",
      x = "Generator",
      y = "Statistical Test",
      fill = "Result"
    )

  # Save plot
  ggsave(
    file.path(output_dir, "test_result_heatmap.png"),
    p,
    width = 12, height = 16, dpi = 300
  )

  # Create summary heatmap by category
  category_data <- aggregate_by_category(plot_data)

  p_category <- ggplot(category_data, aes(x = Generator, y = Category, fill = PassRate)) +
    geom_tile(color = "white", size = 1) +
    geom_text(aes(label = sprintf("%.0f%%", PassRate * 100)),
      color = "white", size = 4
    ) +
    scale_fill_viridis(limits = c(0, 1), labels = scales::percent) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
      axis.text.y = element_text(size = 12),
      plot.title = element_text(size = 16, face = "bold")
    ) +
    labs(
      title = "Test Pass Rates by Category",
      x = "Generator",
      y = "Test Category",
      fill = "Pass Rate"
    )

  ggsave(
    file.path(output_dir, "category_heatmap.png"),
    p_category,
    width = 10, height = 8, dpi = 300
  )

  return(list(full = p, category = p_category))
}

#' Create p-value distribution plots
#' @export
create_pvalue_distribution_plots <- function(suite, output_dir) {
  library(ggplot2)
  library(gridExtra)

  # Extract p-values
  pvalue_data <- extract_pvalues(suite)

  if (nrow(pvalue_data) == 0) {
    return(NULL)
  }

  # Overall p-value distribution
  p_overall <- ggplot(pvalue_data, aes(x = p_value)) +
    geom_histogram(bins = 20, fill = "steelblue", color = "black", alpha = 0.7) +
    geom_vline(xintercept = 0.05, color = "red", linetype = "dashed", size = 1) +
    facet_wrap(~Generator, scales = "free_y") +
    theme_minimal() +
    labs(
      title = "P-value Distribution by Generator",
      x = "P-value",
      y = "Count",
      caption = "Red line indicates α = 0.05"
    )

  # Q-Q plot for p-value uniformity
  p_qq <- ggplot(pvalue_data, aes(sample = p_value)) +
    stat_qq(distribution = qunif) +
    stat_qq_line(distribution = qunif, color = "red") +
    facet_wrap(~Generator) +
    theme_minimal() +
    labs(
      title = "Q-Q Plot: P-value Uniformity Check",
      x = "Theoretical Quantiles",
      y = "Sample Quantiles"
    )

  # Combine plots
  combined <- grid.arrange(p_overall, p_qq, nrow = 2)

  ggsave(
    file.path(output_dir, "pvalue_distributions.png"),
    combined,
    width = 12, height = 10, dpi = 300
  )

  # Create detailed p-value analysis by test category
  p_category <- ggplot(pvalue_data, aes(x = p_value, fill = Category)) +
    geom_histogram(bins = 20, alpha = 0.7, position = "identity") +
    geom_vline(xintercept = 0.05, color = "red", linetype = "dashed") +
    facet_grid(Category ~ Generator) +
    theme_minimal() +
    theme(legend.position = "none") +
    labs(
      title = "P-value Distribution by Test Category and Generator",
      x = "P-value",
      y = "Count"
    )

  ggsave(
    file.path(output_dir, "pvalue_by_category.png"),
    p_category,
    width = 14, height = 10, dpi = 300
  )

  return(list(overall = p_overall, qq = p_qq, category = p_category))
}

#' Create test statistic distribution plots
#' @export
create_statistic_distribution_plots <- function(suite, internal, output_dir) {
  library(ggplot2)
  library(gridExtra)

  # Extract test statistics from results
  stat_data <- data.frame()

  # Process each generator
  for (gen_name in names(suite$results)) {
    gen_results <- suite$results[[gen_name]]

    # Extract statistics from each test category
    for (category in names(gen_results)) {
      if (category == "summary") next

      cat_results <- gen_results[[category]]
      for (test_name in names(cat_results)) {
        test_result <- cat_results[[test_name]]

        # Extract statistic value if available
        if (!is.null(test_result$statistic) && !is.na(test_result$statistic)) {
          stat_data <- rbind(stat_data, data.frame(
            Generator = gen_name,
            Category = category,
            Test = test_name,
            Statistic = as.numeric(test_result$statistic),
            stringsAsFactors = FALSE
          ))
        }
      }
    }
  }

  if (nrow(stat_data) == 0) {
    warning("No test statistics found for distribution plots")
    return(NULL)
  }

  # Create plots for major test types
  plot_list <- list()

  # Chi-squared statistics
  chi_data <- stat_data[grepl("chi", stat_data$Test, ignore.case = TRUE), ]
  if (nrow(chi_data) > 0) {
    p_chi <- ggplot(chi_data, aes(x = Statistic, fill = Generator)) +
      geom_histogram(alpha = 0.6, position = "identity", bins = 30) +
      facet_wrap(~Test, scales = "free") +
      theme_minimal() +
      labs(
        title = "Chi-squared Test Statistics Distribution",
        x = "Test Statistic",
        y = "Count"
      ) +
      theme(legend.position = "bottom")
    plot_list$chi_squared <- p_chi
  }

  # KS statistics
  ks_data <- stat_data[grepl("ks|kolmogorov", stat_data$Test, ignore.case = TRUE), ]
  if (nrow(ks_data) > 0) {
    p_ks <- ggplot(ks_data, aes(x = Statistic, fill = Generator)) +
      geom_histogram(alpha = 0.6, position = "identity", bins = 30) +
      facet_wrap(~Test, scales = "free") +
      theme_minimal() +
      labs(
        title = "Kolmogorov-Smirnov Test Statistics Distribution",
        x = "Test Statistic",
        y = "Count"
      ) +
      theme(legend.position = "bottom")
    plot_list$ks <- p_ks
  }

  # Z-statistics (for mean tests, etc.)
  z_data <- stat_data[grepl("mean|variance", stat_data$Test, ignore.case = TRUE), ]
  if (nrow(z_data) > 0) {
    p_z <- ggplot(z_data, aes(x = Statistic, fill = Generator)) +
      geom_histogram(alpha = 0.6, position = "identity", bins = 30) +
      geom_vline(xintercept = c(-1.96, 1.96), linetype = "dashed", alpha = 0.5) +
      facet_wrap(~Test, scales = "free") +
      theme_minimal() +
      labs(
        title = "Z-statistic Distribution (Mean/Variance Tests)",
        x = "Test Statistic",
        y = "Count"
      ) +
      theme(legend.position = "bottom")
    plot_list$z_stats <- p_z
  }

  # Overall statistics comparison
  p_overall <- ggplot(stat_data, aes(x = Generator, y = Statistic)) +
    geom_boxplot(aes(fill = Generator)) +
    facet_wrap(~Category, scales = "free_y") +
    theme_minimal() +
    labs(
      title = "Test Statistics by Category and Generator",
      x = "Generator",
      y = "Test Statistic"
    ) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "none"
    )

  plot_list$overall <- p_overall

  # Save individual plots
  for (name in names(plot_list)) {
    ggsave(
      file.path(output_dir, sprintf("statistic_dist_%s.png", name)),
      plot_list[[name]],
      width = 12, height = 8, dpi = 300
    )
  }

  # Create combined plot
  if (length(plot_list) > 1) {
    combined <- do.call(grid.arrange, c(plot_list, nrow = length(plot_list)))
    ggsave(
      file.path(output_dir, "statistic_distributions_combined.png"),
      combined,
      width = 14, height = 6 * length(plot_list), dpi = 300
    )
  }

  return(plot_list)
}

#' Create performance comparison plots
#' @export
create_performance_plots <- function(performance, output_dir) {
  library(ggplot2)
  library(scales)

  # Prepare data
  perf_data <- prepare_performance_data(performance)

  # Speed comparison
  p_speed <- ggplot(perf_data, aes(
    x = sample_size, y = samples_per_second,
    color = Generator, group = Generator
  )) +
    geom_line(size = 1.5) +
    geom_point(size = 3) +
    scale_x_log10(labels = comma) +
    scale_y_log10(labels = comma) +
    theme_minimal() +
    labs(
      title = "Generator Performance: Samples per Second",
      x = "Sample Size",
      y = "Samples per Second",
      color = "Generator"
    ) +
    theme(legend.position = "bottom")

  # Time scaling
  p_scaling <- ggplot(perf_data, aes(
    x = sample_size, y = time_seconds,
    color = Generator, group = Generator
  )) +
    geom_line(size = 1.5) +
    geom_point(size = 3) +
    scale_x_log10(labels = comma) +
    scale_y_log10() +
    theme_minimal() +
    labs(
      title = "Generator Performance: Time Scaling",
      x = "Sample Size",
      y = "Time (seconds)",
      color = "Generator"
    ) +
    theme(legend.position = "bottom")

  # Relative performance
  base_perf <- perf_data[perf_data$Generator == names(performance)[1], ]
  perf_data <- merge(perf_data, base_perf[, c("sample_size", "samples_per_second")],
    by = "sample_size", suffixes = c("", "_base")
  )
  perf_data$relative_speed <- perf_data$samples_per_second / perf_data$samples_per_second_base

  p_relative <- ggplot(perf_data, aes(
    x = sample_size, y = relative_speed,
    color = Generator, group = Generator
  )) +
    geom_line(size = 1.5) +
    geom_point(size = 3) +
    geom_hline(yintercept = 1, linetype = "dashed", alpha = 0.5) +
    scale_x_log10(labels = comma) +
    theme_minimal() +
    labs(
      title = sprintf("Relative Performance (vs %s)", names(performance)[1]),
      x = "Sample Size",
      y = "Relative Speed",
      color = "Generator"
    ) +
    theme(legend.position = "bottom")

  # Combine plots
  combined <- grid.arrange(p_speed, p_scaling, p_relative, nrow = 3)

  ggsave(
    file.path(output_dir, "performance_comparison.png"),
    combined,
    width = 10, height = 12, dpi = 300
  )

  return(list(speed = p_speed, scaling = p_scaling, relative = p_relative))
}

#' Create category-wise comparison plots
#' @export
create_category_comparison_plots <- function(suite, output_dir) {
  library(ggplot2)

  # Extract category statistics
  category_stats <- extract_category_statistics(suite)

  # Stacked bar chart
  p_stacked <- ggplot(category_stats, aes(x = Generator, y = Count, fill = Result)) +
    geom_bar(stat = "identity", position = "stack") +
    facet_wrap(~Category, scales = "free_y") +
    scale_fill_manual(values = c("PASS" = "#27ae60", "FAIL" = "#e74c3c", "ERROR" = "#f39c12")) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(
      title = "Test Results by Category",
      x = "Generator",
      y = "Number of Tests",
      fill = "Result"
    )

  ggsave(
    file.path(output_dir, "category_comparison.png"),
    p_stacked,
    width = 12, height = 8, dpi = 300
  )

  # Radar chart for multi-dimensional comparison
  if (requireNamespace("ggradar", quietly = TRUE)) {
    radar_data <- prepare_radar_data(category_stats)

    p_radar <- ggradar::ggradar(
      radar_data,
      grid.min = 0,
      grid.max = 1,
      values.radar = c("0%", "50%", "100%"),
      group.line.width = 1.5,
      group.point.size = 3,
      legend.position = "bottom"
    ) +
      labs(title = "Generator Performance Radar Chart")

    ggsave(
      file.path(output_dir, "generator_radar.png"),
      p_radar,
      width = 10, height = 10, dpi = 300
    )
  }

  return(list(stacked = p_stacked))
}

#' Create time series plots
#' @export
create_timeseries_plots <- function(internal, output_dir) {
  # This would create time series visualizations
  # For now, create a placeholder
  return(NULL)
}

#' Create sample size sensitivity plots
#' @export
create_sensitivity_plots <- function(suite, output_dir) {
  # This would require running tests with different sample sizes
  # For now, create a placeholder or skip
  return(NULL)
}

#' Create generator ranking plots
#' @export
create_ranking_plots <- function(suite, output_dir) {
  library(ggplot2)

  # Calculate rankings
  rankings <- calculate_generator_rankings(suite)

  # Overall ranking
  p_overall <- ggplot(rankings$overall, aes(
    x = reorder(Generator, Rank),
    y = Score, fill = Generator
  )) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = sprintf("#%d", Rank)), vjust = -0.5) +
    coord_flip() +
    theme_minimal() +
    theme(legend.position = "none") +
    labs(
      title = "Overall Generator Ranking",
      x = "Generator",
      y = "Overall Score"
    )

  # Category-specific rankings
  p_category <- ggplot(
    rankings$by_category,
    aes(
      x = Category, y = reorder(Generator, -Rank),
      fill = factor(Rank)
    )
  ) +
    geom_tile(color = "white") +
    geom_text(aes(label = Rank), color = "white", size = 5) +
    scale_fill_viridis_d(direction = -1) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "none"
    ) +
    labs(
      title = "Generator Rankings by Test Category",
      x = "Test Category",
      y = "Generator"
    )

  combined <- grid.arrange(p_overall, p_category, nrow = 2, heights = c(1, 2))

  ggsave(
    file.path(output_dir, "generator_rankings.png"),
    combined,
    width = 10, height = 12, dpi = 300
  )

  return(list(overall = p_overall, category = p_category))
}

# Helper functions

#' Extract test matrix from suite results
extract_test_matrix <- function(suite) {
  # Extract test results from aggregated data
  if (is.null(suite$aggregated)) {
    return(NULL)
  }

  # Get all test names
  all_tests <- unique(unlist(lapply(suite$aggregated, function(gen) {
    names(gen$raw_summary$test)
  })))

  # Create matrix
  test_matrix <- matrix(NA, nrow = length(all_tests), ncol = length(suite$aggregated))
  rownames(test_matrix) <- all_tests
  colnames(test_matrix) <- names(suite$aggregated)

  # Fill matrix
  for (gen_name in names(suite$aggregated)) {
    gen_data <- suite$aggregated[[gen_name]]
    if (!is.null(gen_data$raw_summary)) {
      for (i in 1:nrow(gen_data$raw_summary)) {
        test_name <- gen_data$raw_summary$test[i]
        if (test_name %in% all_tests) {
          test_matrix[test_name, gen_name] <- ifelse(gen_data$raw_summary$passed[i], "PASS", "FAIL")
        }
      }
    }
  }

  return(test_matrix)
}

#' Extract p-values from suite
extract_pvalues <- function(suite) {
  pvalue_list <- list()

  for (gen_name in names(suite$aggregated)) {
    gen_data <- suite$aggregated[[gen_name]]
    if (!is.null(gen_data$raw_summary)) {
      for (i in 1:nrow(gen_data$raw_summary)) {
        pvalue_list[[length(pvalue_list) + 1]] <- data.frame(
          Generator = gen_name,
          Test = gen_data$raw_summary$test[i],
          Category = gen_data$raw_summary$category[i],
          p_value = gen_data$raw_summary$p_value[i],
          stringsAsFactors = FALSE
        )
      }
    }
  }

  if (length(pvalue_list) > 0) {
    return(do.call(rbind, pvalue_list))
  } else {
    return(data.frame(
      Generator = character(), Test = character(),
      Category = character(), p_value = numeric()
    ))
  }
}

#' Prepare performance data for plotting
prepare_performance_data <- function(performance) {
  perf_list <- list()

  for (gen_name in names(performance)) {
    gen_perf <- performance[[gen_name]]
    if (!is.null(gen_perf$timings)) {
      for (i in 1:nrow(gen_perf$timings)) {
        perf_list[[length(perf_list) + 1]] <- data.frame(
          Generator = gen_name,
          sample_size = gen_perf$timings$n[i],
          time_seconds = gen_perf$timings$time[i],
          samples_per_second = gen_perf$timings$n[i] / gen_perf$timings$time[i],
          stringsAsFactors = FALSE
        )
      }
    }
  }

  if (length(perf_list) > 0) {
    return(do.call(rbind, perf_list))
  } else {
    return(data.frame(
      Generator = character(), sample_size = numeric(),
      time_seconds = numeric(), samples_per_second = numeric()
    ))
  }
}

#' Extract category statistics
extract_category_statistics <- function(suite) {
  cat_list <- list()

  for (gen_name in names(suite$aggregated)) {
    gen_data <- suite$aggregated[[gen_name]]
    if (!is.null(gen_data$by_category)) {
      for (cat_name in names(gen_data$by_category)) {
        cat_data <- gen_data$by_category[[cat_name]]
        cat_list[[length(cat_list) + 1]] <- data.frame(
          Generator = gen_name,
          Category = cat_name,
          Result = "PASS",
          Count = cat_data$passed,
          stringsAsFactors = FALSE
        )
        cat_list[[length(cat_list) + 1]] <- data.frame(
          Generator = gen_name,
          Category = cat_name,
          Result = "FAIL",
          Count = cat_data$failed,
          stringsAsFactors = FALSE
        )
      }
    }
  }

  if (length(cat_list) > 0) {
    return(do.call(rbind, cat_list))
  } else {
    return(data.frame(
      Generator = character(), Category = character(),
      Result = character(), Count = numeric()
    ))
  }
}

#' Calculate generator rankings
calculate_generator_rankings <- function(suite) {
  # Overall rankings
  overall_scores <- sapply(names(suite$aggregated), function(gen) {
    gen_data <- suite$aggregated[[gen]]
    gen_data$overall_metrics$pass_rate
  })

  overall_df <- data.frame(
    Generator = names(overall_scores),
    Score = overall_scores,
    Rank = rank(-overall_scores),
    stringsAsFactors = FALSE
  )

  # Category rankings
  cat_rankings <- list()
  all_categories <- unique(unlist(lapply(suite$aggregated, function(gen) {
    names(gen$by_category)
  })))

  for (cat in all_categories) {
    cat_scores <- sapply(names(suite$aggregated), function(gen) {
      if (cat %in% names(suite$aggregated[[gen]]$by_category)) {
        suite$aggregated[[gen]]$by_category[[cat]]$pass_rate
      } else {
        0
      }
    })

    for (gen in names(cat_scores)) {
      cat_rankings[[length(cat_rankings) + 1]] <- data.frame(
        Generator = gen,
        Category = cat,
        Rank = rank(-cat_scores)[gen],
        stringsAsFactors = FALSE
      )
    }
  }

  by_category_df <- if (length(cat_rankings) > 0) {
    do.call(rbind, cat_rankings)
  } else {
    data.frame(Generator = character(), Category = character(), Rank = numeric())
  }

  return(list(overall = overall_df, by_category = by_category_df))
}

#' Aggregate by category for heatmap
aggregate_by_category <- function(plot_data) {
  # Group by generator and category
  agg_data <- aggregate(ResultNum ~ Generator + Test,
    data = plot_data,
    FUN = function(x) sum(x, na.rm = TRUE) / length(x)
  )
  names(agg_data)[3] <- "PassRate"

  # Extract category from test name
  agg_data$Category <- sapply(
    strsplit(as.character(agg_data$Test), "_"),
    function(x) x[1]
  )

  # Final aggregation by category
  cat_data <- aggregate(PassRate ~ Generator + Category,
    data = agg_data,
    FUN = mean, na.rm = TRUE
  )

  return(cat_data)
}
