# File: comprehensive_reporting.R
# ----------------------------------------------------------------------
#' Comprehensive reporting functions for generator comparison
#'
#' This module provides detailed analysis and interpretation of
#' generator comparison results.

#' Generate detailed analysis report
#' @export
generate_detailed_analysis <- function(
  results,
  sample_size,
  confidence_level = 0.95
) {
  analysis <- list()

  # Overall statistics
  analysis$overall <- analyze_overall_results(results)

  # Per-generator analysis
  analysis$by_generator <- lapply(names(results$aggregated), function(gen) {
    analyze_generator(results$aggregated[[gen]], gen)
  })
  names(analysis$by_generator) <- names(results$aggregated)

  # Test category analysis
  analysis$by_category <- analyze_by_category(results)

  # Statistical power analysis
  analysis$power <- analyze_statistical_power(results, sample_size)

  # Failure pattern analysis
  analysis$failure_patterns <- analyze_failure_patterns(results)

  # Performance analysis
  analysis$performance <- analyze_performance(results)

  return(analysis)
}

#' Generate executive summary
#' @export
generate_executive_summary <- function(results, analysis) {
  # Get top performers
  rankings <- rank_generators(analysis$overall)

  summary_text <- paste0(
    "EXECUTIVE SUMMARY\n",
    "=================\n\n",
    "Key Findings:\n",
    "- Tested ", length(results$aggregated), " random number generators\n",
    "- Performed ", analysis$overall$total_test_types, " different statistical tests\n",
    "- Best overall performer: ", rankings$best_overall$name,
    " (", sprintf("%.1f%%", rankings$best_overall$pass_rate * 100), " pass rate)\n",
    "- Most consistent generator: ", rankings$most_consistent$name,
    " (std dev: ", sprintf("%.3f", rankings$most_consistent$std_dev), ")\n",
    "- Fastest generator: ", rankings$fastest$name,
    " (", sprintf("%.2fx", rankings$fastest$relative_speed), " baseline speed)\n\n",
    "Critical Issues Found:\n"
  )

  # Add critical failures
  critical <- analysis$failure_patterns$critical
  if (length(critical) > 0) {
    for (issue in critical) {
      summary_text <- paste0(
        summary_text,
        "- ", issue$generator, ": ", issue$description, "\n"
      )
    }
  } else {
    summary_text <- paste0(summary_text, "- No critical failures detected\n")
  }

  return(list(
    text = summary_text,
    rankings = rankings,
    critical_issues = critical
  ))
}

#' Generate recommendations based on analysis
#' @export
generate_recommendations <- function(analysis, use_cases = NULL) {
  if (is.null(use_cases)) {
    use_cases <- c("general", "cryptographic", "simulation", "gaming")
  }

  recommendations <- list()

  for (use_case in use_cases) {
    recommendations[[use_case]] <- recommend_for_use_case(
      analysis,
      use_case
    )
  }

  return(recommendations)
}

#' Generate technical appendix
#' @export
generate_technical_appendix <- function(
  results,
  test_categories,
  sample_config
) {
  appendix <- list()

  # Test descriptions
  appendix$test_descriptions <- compile_test_descriptions(test_categories)

  # Statistical methodology
  # Get p-value adjustment method from results if available
  p_adjustment_method <- if (!is.null(results$unified_suite$config$p_adjustment_method)) {
    results$unified_suite$config$p_adjustment_method
  } else {
    "None"
  }

  appendix$methodology <- list(
    sample_size = sample_config$size,
    n_runs = sample_config$n_runs,
    bootstrap_iterations = sample_config$n_bootstrap,
    significance_level = 0.05,
    multiple_testing_correction = p_adjustment_method,
    confidence_intervals = "Bootstrap percentile method"
  )

  # Detailed results tables
  appendix$detailed_results <- compile_detailed_results(results)

  # P-value distributions
  appendix$p_value_analysis <- analyze_p_value_distributions(results)

  return(appendix)
}

#' Generate HTML report
#' @export
generate_html_report <- function(
  results,
  analysis,
  summary,
  recommendations,
  appendix,
  viz_results,
  output_file
) {
  # Start HTML document
  html <- paste0(
    "<!DOCTYPE html>
<html>
<head>
<title>Comprehensive PRNG Comparison Report</title>
<style>
body { font-family: Arial, sans-serif; margin: 40px; line-height: 1.6; }
h1, h2, h3 { color: #333; }
.summary-box { background: #f4f4f4; padding: 20px; border-radius: 5px; margin: 20px 0; }
.generator-section { margin: 30px 0; padding: 20px; border: 1px solid #ddd; }
.pass { color: green; font-weight: bold; }
.fail { color: red; font-weight: bold; }
.warning { color: orange; font-weight: bold; }
table { border-collapse: collapse; width: 100%; margin: 20px 0; }
th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }
th { background-color: #f2f2f2; }
.chart-container { text-align: center; margin: 20px 0; }
.recommendation { background: #e8f5e9; padding: 15px; margin: 10px 0; border-radius: 5px; }
.critical { background: #ffebee; padding: 15px; margin: 10px 0; border-radius: 5px; }
</style>
</head>
<body>

<h1>Comprehensive Random Number Generator Comparison</h1>
<p>Report generated: ", Sys.Date(), "</p>

"
  )

  # Add executive summary
  html <- paste0(html, format_executive_summary(summary))

  # Add methodology
  html <- paste0(html, format_methodology(appendix$methodology))

  # Add visualizations
  html <- paste0(html, "<h2>Visual Analysis</h2>")
  for (viz_name in names(viz_results$plots)) {
    if (viz_name %in% c("heatmap", "p_value_dist", "performance")) {
      html <- paste0(
        html,
        '<div class="chart-container">',
        "<h3>", viz_results$descriptions[[viz_name]], "</h3>",
        '<img src="', basename(viz_results$files[[viz_name]]),
        '" width="800" alt="', viz_name, '">',
        "</div>"
      )
    }
  }

  # Add detailed analysis
  html <- paste0(html, format_detailed_analysis(analysis))

  # Add recommendations
  html <- paste0(html, format_recommendations(recommendations))

  # Add technical appendix
  html <- paste0(html, format_technical_appendix(appendix))

  # Close HTML
  html <- paste0(html, "</body></html>")

  # Write file
  writeLines(html, output_file)

  return(list(
    file = output_file,
    size = file.size(output_file)
  ))
}

#' Generate Markdown report
#' @export
generate_markdown_report <- function(
  results,
  analysis,
  summary,
  recommendations,
  appendix,
  output_file
) {
  md <- paste0(
    "# Comprehensive Random Number Generator Comparison\n\n",
    "*Report generated: ", Sys.Date(), "*\n\n"
  )

  # Executive summary
  md <- paste0(md, "## Executive Summary\n\n", summary$text, "\n\n")

  # Top performers table
  md <- paste0(md, "## Generator Rankings\n\n")
  md <- paste0(md, format_rankings_table_md(summary$rankings))

  # Detailed results by generator
  md <- paste0(md, "\n## Detailed Results by Generator\n\n")

  for (gen_name in names(analysis$by_generator)) {
    gen_analysis <- analysis$by_generator[[gen_name]]

    md <- paste0(md, "### ", gen_name, "\n\n")
    md <- paste0(
      md,
      "- Overall Pass Rate: ", sprintf("%.1f%%", gen_analysis$pass_rate * 100), "\n",
      "- Total Tests: ", gen_analysis$total_tests, "\n",
      "- Failed Tests: ", gen_analysis$failed_tests, "\n",
      "- Mean P-value: ", sprintf("%.3f", gen_analysis$mean_p_value), "\n",
      "- Performance: ", sprintf("%.2fx", gen_analysis$performance), " baseline\n\n"
    )

    if (length(gen_analysis$failures) > 0) {
      md <- paste0(md, "**Failed Tests:**\n")
      for (failure in gen_analysis$failures) {
        md <- paste0(md, "- ", failure, "\n")
      }
      md <- paste0(md, "\n")
    }
  }

  # Test category analysis
  md <- paste0(md, "\n## Analysis by Test Category\n\n")

  for (cat_name in names(analysis$by_category)) {
    cat_analysis <- analysis$by_category[[cat_name]]
    md <- paste0(
      md,
      "### ", cat_analysis$name, "\n",
      "- Tests in category: ", cat_analysis$n_tests, "\n",
      "- Average pass rate: ", sprintf("%.1f%%", cat_analysis$avg_pass_rate * 100), "\n",
      "- Most difficult test: ", cat_analysis$hardest_test, "\n\n"
    )
  }

  # Recommendations
  md <- paste0(md, "\n## Recommendations\n\n")

  for (use_case in names(recommendations)) {
    rec <- recommendations[[use_case]]
    md <- paste0(
      md,
      "### ", stringr::str_to_title(use_case), " Use Case\n\n",
      "**Recommended Generator:** ", rec$recommended, "\n\n",
      "**Reason:** ", rec$reason, "\n\n",
      "**Alternatives:**\n"
    )

    for (alt in rec$alternatives) {
      md <- paste0(md, "- ", alt, "\n")
    }
    md <- paste0(md, "\n")
  }

  # Statistical notes
  md <- paste0(
    md,
    "\n## Statistical Notes\n\n",
    "- Significance level: α = 0.05\n",
    "- Multiple testing correction: Bonferroni\n",
    "- Bootstrap iterations: ", appendix$methodology$bootstrap_iterations, "\n",
    "- Sample size: ", format(appendix$methodology$sample_size, big.mark = ","), "\n\n"
  )

  # Write file
  writeLines(md, output_file)

  return(list(
    file = output_file,
    size = file.size(output_file)
  ))
}

# Helper functions for analysis
analyze_overall_results <- function(results) {
  all_tests <- unlist(lapply(results$aggregated, function(gen) {
    gen$overall_metrics$total_tests
  }))

  all_passes <- unlist(lapply(results$aggregated, function(gen) {
    gen$overall_metrics$total_tests - gen$overall_metrics$failed_tests
  }))

  list(
    total_generators = length(results$aggregated),
    total_test_types = length(unique(unlist(lapply(results$aggregated, function(gen) {
      names(gen$by_category)
    })))),
    total_tests_run = sum(all_tests),
    total_passes = sum(all_passes),
    overall_pass_rate = sum(all_passes) / sum(all_tests)
  )
}

analyze_generator <- function(gen_results, gen_name) {
  list(
    name = gen_name,
    pass_rate = gen_results$overall_metrics$pass_rate,
    weighted_pass_rate = if (!is.null(gen_results$overall_metrics$weighted_pass_rate)) {
      gen_results$overall_metrics$weighted_pass_rate
    } else {
      gen_results$overall_metrics$pass_rate
    },
    total_tests = gen_results$overall_metrics$total_tests,
    failed_tests = gen_results$overall_metrics$failed_tests,
    mean_p_value = gen_results$overall_metrics$mean_p_value,
    std_p_value = gen_results$overall_metrics$std_p_value,
    performance = gen_results$performance$relative_speed,
    failures = gen_results$overall_metrics$failed_test_names,
    category_pass_rates = gen_results$overall_metrics$category_pass_rates,
    weights_used = gen_results$overall_metrics$weights_used
  )
}

analyze_by_category <- function(results) {
  # Get all categories
  all_categories <- unique(unlist(lapply(results$aggregated, function(gen) {
    names(gen$by_category)
  })))

  category_analysis <- list()

  for (cat in all_categories) {
    # Collect pass rates for this category across generators
    pass_rates <- c()
    test_names <- c()

    for (gen_name in names(results$aggregated)) {
      if (cat %in% names(results$aggregated[[gen_name]]$by_category)) {
        cat_data <- results$aggregated[[gen_name]]$by_category[[cat]]
        pass_rates <- c(pass_rates, cat_data$pass_rate)
        test_names <- unique(c(test_names, cat_data$failed_tests))
      }
    }

    category_analysis[[cat]] <- list(
      name = cat,
      n_tests = length(test_names),
      avg_pass_rate = mean(pass_rates, na.rm = TRUE),
      std_pass_rate = sd(pass_rates, na.rm = TRUE),
      hardest_test = if (length(test_names) > 0) test_names[1] else "None"
    )
  }

  return(category_analysis)
}

analyze_statistical_power <- function(results, sample_size) {
  # Simplified power analysis
  list(
    sample_size = sample_size,
    min_detectable_bias = sqrt(1 / sample_size) * 3, # 3 sigma
    power_estimate = 0.80, # Standard assumption
    notes = "Power analysis based on standard statistical assumptions"
  )
}

analyze_failure_patterns <- function(results) {
  failures <- list()
  critical <- list()

  for (gen_name in names(results$aggregated)) {
    gen_data <- results$aggregated[[gen_name]]

    if (gen_data$overall_metrics$pass_rate < 0.5) {
      critical[[length(critical) + 1]] <- list(
        generator = gen_name,
        description = paste0(
          "Failed more than 50% of tests (",
          sprintf("%.1f%%", (1 - gen_data$overall_metrics$pass_rate) * 100),
          " failure rate)"
        )
      )
    }

    # Check for category-specific failures
    for (cat in names(gen_data$by_category)) {
      if (gen_data$by_category[[cat]]$pass_rate < 0.3) {
        failures[[length(failures) + 1]] <- list(
          generator = gen_name,
          category = cat,
          pass_rate = gen_data$by_category[[cat]]$pass_rate
        )
      }
    }
  }

  list(
    critical = critical,
    by_category = failures
  )
}

analyze_performance <- function(results) {
  perf_data <- lapply(names(results$aggregated), function(gen) {
    list(
      generator = gen,
      speed = results$aggregated[[gen]]$performance$relative_speed,
      efficiency = results$aggregated[[gen]]$performance$efficiency
    )
  })

  # Sort by speed
  perf_data <- perf_data[order(sapply(perf_data, function(x) x$speed),
    decreasing = TRUE
  )]

  return(perf_data)
}

rank_generators <- function(analysis) {
  # Extract generator metrics
  gen_names <- names(analysis$by_generator)
  if (length(gen_names) == 0) {
    return(list(
      rankings = list(
        by_weighted_pass_rate = data.frame(generator = "N/A", weighted_pass_rate = 0),
        by_unweighted_pass_rate = data.frame(generator = "N/A", pass_rate = 0),
        by_consistency = data.frame(generator = "N/A", p_value_std_dev = 1),
        by_speed = data.frame(generator = "N/A", relative_speed = 0)
      ),
      summary = list(
        best_overall = list(name = "N/A", weighted_pass_rate = 0),
        most_consistent = list(name = "N/A", std_dev = 1),
        fastest = list(name = "N/A", relative_speed = 0)
      )
    ))
  }

  gen_metrics <- analysis$by_generator

  # Create data frames for each ranking metric
  ranking_data <- data.frame(
    generator = gen_names,
    stringsAsFactors = FALSE
  )

  # Extract metrics
  ranking_data$weighted_pass_rate <- sapply(gen_metrics, function(x) {
    if (!is.null(x$weighted_pass_rate)) x$weighted_pass_rate else x$pass_rate
  })

  ranking_data$unweighted_pass_rate <- sapply(gen_metrics, function(x) {
    if (!is.null(x$pass_rate)) x$pass_rate else 0
  })

  ranking_data$p_value_std_dev <- sapply(gen_metrics, function(x) {
    if (!is.null(x$std_p_value)) x$std_p_value else NA
  })

  ranking_data$relative_speed <- sapply(gen_metrics, function(x) {
    if (!is.null(x$performance)) x$performance else 0
  })

  ranking_data$total_tests <- sapply(gen_metrics, function(x) {
    if (!is.null(x$total_tests)) x$total_tests else 0
  })

  ranking_data$failed_tests <- sapply(gen_metrics, function(x) {
    if (!is.null(x$failed_tests)) x$failed_tests else 0
  })

  # Create sorted rankings for each metric
  rankings <- list(
    by_weighted_pass_rate = ranking_data[
      order(ranking_data$weighted_pass_rate, decreasing = TRUE),
      c("generator", "weighted_pass_rate", "total_tests", "failed_tests")
    ],
    by_unweighted_pass_rate = ranking_data[
      order(ranking_data$unweighted_pass_rate, decreasing = TRUE),
      c("generator", "unweighted_pass_rate", "total_tests", "failed_tests")
    ],
    by_consistency = ranking_data[
      !is.na(ranking_data$p_value_std_dev) &
        order(ranking_data$p_value_std_dev, decreasing = FALSE),
      c("generator", "p_value_std_dev")
    ],
    by_speed = ranking_data[
      order(ranking_data$relative_speed, decreasing = TRUE),
      c("generator", "relative_speed")
    ]
  )

  # Reset row names
  for (name in names(rankings)) {
    rownames(rankings[[name]]) <- NULL
  }

  # Create summary for backward compatibility
  best_idx <- which.max(ranking_data$weighted_pass_rate)
  consistent_idx <- which.min(ranking_data$p_value_std_dev[!is.na(ranking_data$p_value_std_dev)])
  fast_idx <- which.max(ranking_data$relative_speed)

  summary <- list(
    best_overall = if (length(best_idx) > 0) {
      list(
        name = gen_names[best_idx],
        weighted_pass_rate = ranking_data$weighted_pass_rate[best_idx],
        pass_rate = ranking_data$unweighted_pass_rate[best_idx]
      )
    } else {
      list(name = "N/A", weighted_pass_rate = 0, pass_rate = 0)
    },
    most_consistent = if (length(consistent_idx) > 0) {
      list(
        name = gen_names[consistent_idx],
        std_dev = ranking_data$p_value_std_dev[consistent_idx]
      )
    } else {
      list(name = "N/A", std_dev = 1)
    },
    fastest = if (length(fast_idx) > 0) {
      list(
        name = gen_names[fast_idx],
        relative_speed = ranking_data$relative_speed[fast_idx]
      )
    } else {
      list(name = "N/A", relative_speed = 0)
    }
  )

  list(
    rankings = rankings,
    summary = summary,
    full_data = ranking_data
  )
}

recommend_for_use_case <- function(analysis, use_case) {
  # Get all generators sorted by pass rate
  generators <- analysis$by_generator
  sorted_gens <- generators[order(sapply(generators, function(x) x$pass_rate),
    decreasing = TRUE
  )]

  recommendations <- switch(use_case,
    "cryptographic" = {
      # For crypto, need perfect or near-perfect scores
      suitable <- sorted_gens[sapply(sorted_gens, function(x) x$pass_rate > 0.99)]
      list(
        recommended = if (length(suitable) > 0) suitable[[1]]$name else "None suitable",
        reason = "Requires passing all statistical tests with high confidence",
        alternatives = if (length(suitable) > 1) {
          sapply(
            suitable[2:min(3, length(suitable))],
            function(x) x$name
          )
        } else {
          c()
        }
      )
    },
    "simulation" = {
      # For simulation, balance performance and quality
      suitable <- sorted_gens[sapply(sorted_gens, function(x) {
        x$pass_rate > 0.90 && x$performance > 0.5
      })]
      list(
        recommended = if (length(suitable) > 0) suitable[[1]]$name else sorted_gens[[1]]$name,
        reason = "Good statistical properties with reasonable performance",
        alternatives = if (length(suitable) > 1) {
          sapply(
            suitable[2:min(3, length(suitable))],
            function(x) x$name
          )
        } else {
          c()
        }
      )
    },
    "gaming" = {
      # For gaming, prioritize speed
      by_speed <- generators[order(sapply(generators, function(x) x$performance),
        decreasing = TRUE
      )]
      suitable <- by_speed[sapply(by_speed, function(x) x$pass_rate > 0.80)]
      list(
        recommended = if (length(suitable) > 0) suitable[[1]]$name else by_speed[[1]]$name,
        reason = "Fast performance with acceptable randomness quality",
        alternatives = if (length(suitable) > 1) {
          sapply(
            suitable[2:min(3, length(suitable))],
            function(x) x$name
          )
        } else {
          c()
        }
      )
    },

    # Default/general use case
    {
      list(
        recommended = sorted_gens[[1]]$name,
        reason = "Best overall statistical performance",
        alternatives = if (length(sorted_gens) > 1) {
          sapply(
            sorted_gens[2:min(3, length(sorted_gens))],
            function(x) x$name
          )
        } else {
          c()
        }
      )
    }
  )

  return(recommendations)
}

# Format helpers
format_executive_summary <- function(summary) {
  paste0(
    '<div class="summary-box">',
    "<h2>Executive Summary</h2>",
    "<pre>", summary$text, "</pre>",
    "</div>"
  )
}

format_methodology <- function(methodology) {
  paste0(
    "<h2>Methodology</h2>",
    '<div class="summary-box">',
    "<ul>",
    "<li><strong>Sample Size:</strong> ", format(methodology$sample_size, big.mark = ","), "</li>",
    "<li><strong>Test Runs:</strong> ", methodology$n_runs, "</li>",
    "<li><strong>Bootstrap Iterations:</strong> ", methodology$bootstrap_iterations, "</li>",
    "<li><strong>Significance Level:</strong> α = ", methodology$significance_level, "</li>",
    "<li><strong>Multiple Testing Correction:</strong> ", methodology$multiple_testing_correction, "</li>",
    "</ul>",
    "</div>"
  )
}

format_detailed_analysis <- function(analysis) {
  html <- "<h2>Detailed Analysis</h2>"

  # Add generator-specific sections
  for (gen_name in names(analysis$by_generator)) {
    gen <- analysis$by_generator[[gen_name]]

    pass_class <- if (gen$pass_rate > 0.95) {
      "pass"
    } else if (gen$pass_rate > 0.80) {
      "warning"
    } else {
      "fail"
    }

    html <- paste0(
      html,
      '<div class="generator-section">',
      "<h3>", gen_name, "</h3>",
      '<p class="', pass_class, '">Pass Rate: ',
      sprintf("%.1f%%", gen$pass_rate * 100), "</p>",
      "<p>Performance: ", sprintf("%.2fx", gen$performance), " baseline</p>"
    )

    if (length(gen$failures) > 0) {
      html <- paste0(
        html,
        "<p><strong>Failed Tests:</strong></p>",
        "<ul>"
      )
      for (failure in gen$failures) {
        html <- paste0(html, "<li>", failure, "</li>")
      }
      html <- paste0(html, "</ul>")
    }

    html <- paste0(html, "</div>")
  }

  return(html)
}

format_recommendations <- function(recommendations) {
  html <- "<h2>Recommendations by Use Case</h2>"

  for (use_case in names(recommendations)) {
    rec <- recommendations[[use_case]]

    html <- paste0(
      html,
      '<div class="recommendation">',
      "<h3>", stringr::str_to_title(use_case), " Applications</h3>",
      "<p><strong>Recommended:</strong> ", rec$recommended, "</p>",
      "<p><strong>Reason:</strong> ", rec$reason, "</p>"
    )

    if (length(rec$alternatives) > 0) {
      html <- paste0(
        html, "<p><strong>Alternatives:</strong> ",
        paste(rec$alternatives, collapse = ", "), "</p>"
      )
    }

    html <- paste0(html, "</div>")
  }

  return(html)
}

format_technical_appendix <- function(appendix) {
  paste0(
    "<h2>Technical Appendix</h2>",
    "<p>Full technical details and raw data are available in the accompanying files.</p>",
    "<p>Statistical methodology: ", appendix$methodology$confidence_intervals, "</p>"
  )
}

format_rankings_table_md <- function(rankings) {
  paste0(
    "| Category | Generator | Metric |\n",
    "|----------|-----------|--------|\n",
    "| Best Overall | ", rankings$best_overall$name, " | ",
    sprintf("%.1f%% pass rate", rankings$best_overall$pass_rate * 100), " |\n",
    "| Most Consistent | ", rankings$most_consistent$name, " | ",
    sprintf("%.3f std dev", rankings$most_consistent$std_p_value), " |\n",
    "| Fastest | ", rankings$fastest$name, " | ",
    sprintf("%.2fx speed", rankings$fastest$performance), " |\n"
  )
}

compile_test_descriptions <- function(test_categories) {
  # Compile descriptions of all tests
  descs <- list()
  for (cat_name in names(test_categories)) {
    descs[[cat_name]] <- test_categories[[cat_name]]$name
  }
  return(descs)
}

compile_detailed_results <- function(results) {
  # Create detailed results tables
  # This would be expanded based on specific needs
  return(results$aggregated)
}

analyze_p_value_distributions <- function(results) {
  # Analyze distribution of p-values
  all_p_values <- c()

  for (gen_name in names(results$raw)) {
    for (run in results$raw[[gen_name]]) {
      for (cat in run) {
        if (!is.null(cat$summary)) {
          all_p_values <- c(all_p_values, cat$summary$p_value)
        }
      }
    }
  }

  all_p_values <- all_p_values[!is.na(all_p_values)]

  list(
    n = length(all_p_values),
    mean = mean(all_p_values),
    median = median(all_p_values),
    sd = sd(all_p_values),
    uniform_test = if (length(all_p_values) > 30) ks.test(all_p_values, "punif") else NULL
  )
}
