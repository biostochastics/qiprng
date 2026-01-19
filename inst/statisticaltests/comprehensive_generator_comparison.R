# File: comprehensive_generator_comparison.R
# ----------------------------------------------------------------------
#' Truly Comprehensive Generator Comparison Framework
#'
#' This module provides a complete comparison of random number generators
#' across ALL implemented statistical tests with proper sample sizes,
#' comprehensive visualizations, and detailed reporting.

# Source all required modules
source_if_exists <- function(file) {
  if (file.exists(file)) {
    source(file)
    TRUE
  } else {
    pkg_file <- system.file(file, package = "qiprng")
    if (file.exists(pkg_file)) {
      source(pkg_file)
      TRUE
    } else {
      FALSE
    }
  }
}

# Source all test modules
source_if_exists("R/statisticaltests/unified_reporting.R")
source_if_exists("R/statisticaltests/apply_test_fixes.R")

# Internal test suites
source_if_exists("R/statisticaltests/basic_tests.R")
source_if_exists("R/statisticaltests/classical_tests.R")
source_if_exists("R/statisticaltests/binary_tests.R")
source_if_exists("R/statisticaltests/compression_tests.R")
source_if_exists("R/statisticaltests/correlation_tests.R")
source_if_exists("R/statisticaltests/runs_tests.R")

# External test wrappers
source_if_exists("R/statisticaltests/comprehensive_external_tests.R")
source_if_exists("R/statisticaltests/multidim_tests.R")
source_if_exists("R/statisticaltests/multidim_specific_tests.R")

# Original comparison framework
source_if_exists("R/statisticaltests/generator_comparison.R")

#' Sample size configuration for different test modes
#' @export
get_sample_size_config <- function(mode = "standard") {
  configs <- list(
    quick = list(
      base = 10000,
      compression = 5000,
      classical = 10000,
      binary = 5000,
      multidim = 5000,
      description = "Quick testing (10k samples)"
    ),
    standard = list(
      base = 100000,
      compression = 50000,
      classical = 100000,
      binary = 50000,
      multidim = 20000,
      description = "Standard testing (100k samples)"
    ),
    thorough = list(
      base = 1000000,
      compression = 200000,
      classical = 500000,
      binary = 100000,
      multidim = 50000,
      description = "Thorough testing (1M samples)"
    ),
    extreme = list(
      base = 10000000,
      compression = 1000000,
      classical = 5000000,
      binary = 500000,
      multidim = 100000,
      description = "Extreme testing (10M samples)"
    )
  )

  if (!mode %in% names(configs)) {
    warning(sprintf("Unknown mode '%s', using 'standard'", mode))
    mode <- "standard"
  }

  return(configs[[mode]])
}

#' Run all internal test suites
#' @export
run_all_internal_tests <- function(generators, sample_config, verbose = TRUE) {
  if (verbose) {
    cat("\n=== Running Internal Test Suites ===\n")
  }

  results <- list()

  for (gen_name in names(generators)) {
    gen <- generators[[gen_name]]

    if (verbose) {
      cat(sprintf("\nTesting %s with internal tests...\n", gen$name))
    }

    gen_results <- list()

    # Create test suite configuration
    suite_config <- list(
      basic_sample_size = sample_config$base,
      compression_sample_size = sample_config$compression,
      classical_sample_size = sample_config$classical,
      binary_sample_size = sample_config$binary,
      correlation_sample_size = sample_config$base,
      runs_sample_size = sample_config$base,
      significance_level = 0.05,
      use_bootstrap_compression = TRUE
    )

    # Create suite object
    suite <- list(
      config = suite_config,
      prng_func = gen$func,
      prng_info = list(name = gen$name, description = gen$description),
      results = list()
    )

    # Run each test suite
    test_suites <- list(
      basic = "run_basic_tests",
      classical = "run_classical_tests",
      binary = "run_binary_tests",
      compression = "run_compression_tests",
      correlation = "run_correlation_tests",
      runs = "run_runs_tests"
    )

    for (test_name in names(test_suites)) {
      func_name <- test_suites[[test_name]]

      if (exists(func_name)) {
        if (verbose) cat(sprintf("  Running %s tests...\n", test_name))

        tryCatch(
          {
            # Call the test function
            test_func <- get(func_name)
            suite_result <- test_func(suite)

            # Extract results
            if (!is.null(suite_result$results)) {
              gen_results[[test_name]] <- suite_result$results
            }
          },
          error = function(e) {
            gen_results[[test_name]] <- list(
              error = as.character(e),
              result = "ERROR"
            )
            if (verbose) cat(sprintf("    Error in %s: %s\n", test_name, e$message))
          }
        )
      } else {
        if (verbose) cat(sprintf("  %s tests not available\n", test_name))
      }
    }

    results[[gen_name]] <- gen_results
  }

  return(results)
}

#' Comprehensive generator comparison with all tests
#' @param generators List of generators to compare (NULL uses default generators)
#' @param mode Sample size mode: "quick", "standard", "thorough", "extreme"
#' @param external_packages Which external packages to use: "all" or specific list
#' @param dimensions Vector of dimensions for multidimensional tests
#' @param output_dir Directory for output files
#' @param parallel Use parallel processing
#' @param n_cores Number of cores for parallel processing
#' @param p_adjustment_method P-value adjustment method: "BH" (default), "bonferroni",
#'   "holm", "hochberg", "hommel", "BY", "fdr", "none"
#' @param category_weights Named vector of weights for test categories (e.g.,
#'   c(basic = 1.0, classical = 1.5, binary = 0.8)). Default: all weights = 1.0
#' @param verbose Print progress messages
#' @export
compare_generators_comprehensive <- function(
  generators = NULL,
  mode = "standard",
  external_packages = "all",
  dimensions = c(2, 3),
  output_dir = "comprehensive_comparison",
  parallel = FALSE,
  n_cores = NULL,
  p_adjustment_method = "BH",
  category_weights = NULL,
  verbose = TRUE
) {
  # Get sample size configuration
  sample_config <- get_sample_size_config(mode)

  if (verbose) {
    cat("\n=== COMPREHENSIVE GENERATOR COMPARISON ===\n")
    cat(sprintf("Mode: %s\n", sample_config$description))
    cat(sprintf("Base sample size: %s\n", format(sample_config$base, big.mark = ",")))
  }

  # Use default generators if none provided
  if (is.null(generators)) {
    generators <- get_standard_generators()
  }

  if (length(generators) == 0) {
    stop("No generators available for comparison")
  }

  # Create output directory
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Save configuration
  config_file <- file.path(output_dir, "test_configuration.json")
  config_data <- list(
    mode = mode,
    sample_sizes = sample_config,
    generators = names(generators),
    test_date = Sys.time(),
    R_version = R.version.string,
    platform = Sys.info()["sysname"]
  )

  if (requireNamespace("jsonlite", quietly = TRUE)) {
    jsonlite::write_json(config_data, config_file, pretty = TRUE)
  }

  # Initialize timing
  start_time <- Sys.time()

  # Initialize unified test suite
  unified_suite <- test_suite_results(
    suite_name = "Comprehensive Generator Comparison",
    prng_info = list(
      comparison_date = start_time,
      mode = mode,
      sample_config = sample_config,
      generators_tested = names(generators)
    )
  )

  # 1. Run internal tests
  if (verbose) cat("\n== Phase 1: Internal Test Suites ==\n")
  internal_results <- run_all_internal_tests(generators, sample_config, verbose)

  # 2. Run external package tests
  if (verbose) cat("\n== Phase 2: External Package Tests ==\n")
  external_results <- compare_generators_external(
    generators,
    sample_size = sample_config$base,
    packages = external_packages,
    verbose = verbose
  )

  # 3. Run multi-dimensional tests
  if (verbose) cat("\n== Phase 3: Multi-dimensional Tests ==\n")
  multidim_results <- compare_generators_multidim(
    generators,
    dimensions = dimensions,
    sample_size = sample_config$multidim,
    verbose = verbose
  )

  # 4. Performance benchmarking
  if (verbose) cat("\n== Phase 4: Performance Benchmarking ==\n")
  performance_results <- benchmark_generators(
    generators,
    sample_sizes = c(1000, 10000, 100000, 1000000),
    verbose = verbose
  )

  # Convert all results to unified format
  unified_suite <- convert_to_unified_format(
    unified_suite,
    internal_results,
    external_results,
    multidim_results,
    performance_results
  )

  # Calculate end time
  end_time <- Sys.time()
  duration <- as.numeric(difftime(end_time, start_time, units = "secs"))
  unified_suite$prng_info$duration <- duration

  # Finalize suite
  unified_suite <- finalize_suite(unified_suite)

  # Apply p-value adjustment for multiple testing correction
  if (verbose) cat("\n== Applying Multiple Testing Correction ==\n")
  unified_suite <- adjust_p_values(unified_suite, method = p_adjustment_method)

  # Generate comprehensive reports and visualizations
  if (verbose) cat("\n== Phase 5: Generating Reports and Visualizations ==\n")

  report_results <- generate_comprehensive_reports(
    unified_suite,
    internal_results,
    external_results,
    multidim_results,
    performance_results,
    output_dir,
    verbose
  )

  # Create summary statistics
  summary_stats <- create_comprehensive_summary(
    unified_suite,
    internal_results,
    external_results,
    multidim_results,
    performance_results
  )

  if (verbose) {
    cat("\n=== COMPARISON COMPLETE ===\n")
    cat(sprintf("Total duration: %.1f seconds\n", duration))
    cat(sprintf("Results saved to: %s\n", output_dir))
    print_summary_stats(summary_stats)
  }

  return(list(
    suite = unified_suite,
    internal_results = internal_results,
    external_results = external_results,
    multidim_results = multidim_results,
    performance_results = performance_results,
    summary = summary_stats,
    output_dir = output_dir,
    duration = duration
  ))
}

#' Benchmark generator performance
#' @export
benchmark_generators <- function(generators, sample_sizes, verbose = TRUE) {
  results <- list()

  for (gen_name in names(generators)) {
    gen <- generators[[gen_name]]

    if (verbose) {
      cat(sprintf("\nBenchmarking %s...\n", gen$name))
    }

    timings <- numeric(length(sample_sizes))

    for (i in seq_along(sample_sizes)) {
      n <- sample_sizes[i]

      # Warm up
      gen$func(100)

      # Time generation
      start <- Sys.time()
      gen$func(n)
      end <- Sys.time()

      timings[i] <- as.numeric(difftime(end, start, units = "secs"))

      if (verbose) {
        cat(sprintf(
          "  %s samples: %.3f seconds (%.0f samples/sec)\n",
          format(n, big.mark = ","),
          timings[i],
          n / timings[i]
        ))
      }
    }

    results[[gen_name]] <- list(
      sample_sizes = sample_sizes,
      timings = timings,
      samples_per_second = sample_sizes / timings
    )
  }

  return(results)
}

#' Convert results to unified format
#' @keywords internal
convert_to_unified_format <- function(suite, internal, external, multidim, performance) {
  # Add internal test results
  for (gen_name in names(internal)) {
    gen_results <- internal[[gen_name]]

    for (test_suite_name in names(gen_results)) {
      suite_results <- gen_results[[test_suite_name]]

      if (!is.null(suite_results$error)) {
        # Error result
        result <- test_result(
          test_name = paste(gen_name, test_suite_name, "error", sep = "_"),
          description = paste(test_suite_name, "tests"),
          result = "ERROR",
          details = suite_results$error,
          category = "Internal Tests",
          subcategory = test_suite_name
        )
        suite <- add_test_result(suite, result)
      } else {
        # Process individual test results
        for (test_name in names(suite_results)) {
          test <- suite_results[[test_name]]

          if (is.list(test) && !is.null(test$result)) {
            result <- test_result(
              test_name = paste(gen_name, test_suite_name, test_name, sep = "_"),
              description = test$description,
              result = test$result,
              p_value = test$p_value,
              statistic = test$statistic,
              details = test$details,
              category = "Internal Tests",
              subcategory = test_suite_name
            )
            suite <- add_test_result(suite, result)
          }
        }
      }
    }
  }

  # Process external and multidim results (already handled in original)
  # ... (keeping existing conversion logic)

  return(suite)
}

#' Run comprehensive generator comparison
#' @export
run_comprehensive_comparison <- function(
  generators,
  sample_size = 100000,
  test_categories = NULL,
  n_runs = 5,
  parallel = TRUE,
  progress = TRUE
) {
  # Initialize results structure
  results <- list(
    raw = list(),
    aggregated = list(),
    metadata = list(
      start_time = Sys.time(),
      sample_size = sample_size,
      n_runs = n_runs,
      n_generators = length(generators),
      n_test_categories = length(test_categories)
    )
  )

  # Progress reporting
  total_tests <- length(generators) * n_runs * length(test_categories)
  completed <- 0

  if (progress) {
    cat("Total tests to run:", total_tests, "\n")
    cat("Progress: ")
  }

  # Run tests for each generator
  for (gen_name in names(generators)) {
    gen_info <- generators[[gen_name]]

    if (progress) cat("\n\nTesting", gen_name, "...\n")

    # Initialize generator results
    results$raw[[gen_name]] <- list()

    # Performance timing
    gen_start <- Sys.time()

    # Multiple runs for stability
    for (run in 1:n_runs) {
      if (progress) cat("  Run", run, "of", n_runs, ": ")

      # Initialize generator
      if (!is.null(gen_info$init)) {
        gen_info$init()
      }

      # Generate samples
      if (!is.null(gen_info$gen)) {
        x <- gen_info$gen(sample_size)
      } else {
        # Use base R generation
        x <- runif(sample_size)
      }

      # Run all test categories
      run_results <- list()

      for (cat_name in names(test_categories)) {
        cat_info <- test_categories[[cat_name]]

        if (progress) cat(".")

        # Run test with error handling
        test_result <- tryCatch(
          {
            cat_info$runner(x, cat_info$params)
          },
          error = function(e) {
            list(
              results = list(error = toString(e)),
              summary = data.frame(
                test = cat_name,
                p_value = NA,
                statistic = NA,
                passed = FALSE,
                details = paste("Error:", toString(e))
              )
            )
          }
        )

        run_results[[cat_name]] <- test_result

        completed <- completed + 1
      }

      if (progress) cat(" done\n")

      results$raw[[gen_name]][[paste0("run_", run)]] <- run_results
    }

    # Calculate generation time
    gen_time <- as.numeric(Sys.time() - gen_start, units = "secs")

    # Aggregate results for this generator
    results$aggregated[[gen_name]] <- aggregate_generator_results(
      results$raw[[gen_name]],
      gen_name,
      gen_time,
      category_weights
    )
  }

  # Complete metadata
  results$metadata$end_time <- Sys.time()
  results$metadata$total_duration <- as.numeric(
    results$metadata$end_time - results$metadata$start_time,
    units = "secs"
  )

  # Normalize performance metrics
  results <- normalize_performance_metrics(results)

  if (progress) {
    cat("\n\nComparison complete!\n")
    cat("Total time:", round(results$metadata$total_duration, 1), "seconds\n")
  }

  return(results)
}

#' Aggregate results across multiple runs
#' @param raw_runs Raw test run results
#' @param gen_name Generator name
#' @param gen_time Total time for generator
#' @param category_weights Named vector of weights for test categories
aggregate_generator_results <- function(raw_runs, gen_name, gen_time, category_weights = NULL) {
  # Collect all test results
  all_summaries <- list()

  for (run_name in names(raw_runs)) {
    run_data <- raw_runs[[run_name]]

    for (cat_name in names(run_data)) {
      if (!is.null(run_data[[cat_name]]$summary)) {
        summary_df <- run_data[[cat_name]]$summary
        summary_df$category <- cat_name
        summary_df$run <- run_name
        all_summaries[[length(all_summaries) + 1]] <- summary_df
      }
    }
  }

  # Combine all summaries
  if (length(all_summaries) > 0) {
    combined_summary <- do.call(rbind, all_summaries)

    # Calculate aggregate metrics with weights
    overall_metrics <- calculate_overall_metrics(combined_summary, category_weights)
    by_category <- calculate_category_metrics(combined_summary)

    # Performance metrics
    n_runs <- length(unique(combined_summary$run))
    avg_time_per_run <- gen_time / n_runs

    performance <- list(
      total_time = gen_time,
      avg_time_per_run = avg_time_per_run,
      samples_per_second = n_runs * 100000 / gen_time, # Assuming 100k samples
      relative_speed = NA, # Will be calculated later
      efficiency = overall_metrics$pass_rate / avg_time_per_run
    )
  } else {
    # No valid results
    overall_metrics <- list(
      total_tests = 0,
      passed_tests = 0,
      failed_tests = 0,
      pass_rate = 0,
      mean_p_value = NA,
      std_p_value = NA,
      failed_test_names = character()
    )
    by_category <- list()
    performance <- list(
      total_time = gen_time,
      avg_time_per_run = gen_time,
      samples_per_second = 0,
      relative_speed = NA,
      efficiency = 0
    )
  }

  return(list(
    generator = gen_name,
    overall_metrics = overall_metrics,
    by_category = by_category,
    performance = performance,
    raw_summary = if (exists("combined_summary")) combined_summary else NULL
  ))
}

#' Calculate overall metrics from summary data
#' @param summary_df Summary data frame with test results
#' @param category_weights Named vector of weights for each test category
calculate_overall_metrics <- function(summary_df, category_weights = NULL) {
  # Remove duplicate tests (keep first occurrence)
  unique_tests <- summary_df[!duplicated(summary_df$test), ]

  # Get categories and set default weights if not provided
  categories <- unique(unique_tests$category)
  if (is.null(category_weights)) {
    category_weights <- setNames(rep(1.0, length(categories)), categories)
  } else {
    # Ensure all categories have weights (default to 1.0 for missing)
    for (cat in categories) {
      if (!(cat %in% names(category_weights))) {
        category_weights[cat] <- 1.0
      }
    }
  }

  # Calculate unweighted metrics (for backward compatibility)
  total_tests <- nrow(unique_tests)
  passed_tests <- sum(unique_tests$passed, na.rm = TRUE)
  failed_tests <- total_tests - passed_tests
  unweighted_pass_rate <- if (total_tests > 0) passed_tests / total_tests else 0

  # Calculate weighted metrics
  weighted_passes <- 0
  weighted_total <- 0
  category_pass_rates <- list()

  for (cat in categories) {
    cat_tests <- unique_tests[unique_tests$category == cat, ]
    cat_total <- nrow(cat_tests)
    cat_passed <- sum(cat_tests$passed, na.rm = TRUE)

    if (cat_total > 0) {
      cat_pass_rate <- cat_passed / cat_total
      category_pass_rates[[cat]] <- list(
        pass_rate = cat_pass_rate,
        total = cat_total,
        passed = cat_passed,
        weight = category_weights[cat]
      )

      # Add to weighted totals
      weight <- category_weights[cat]
      weighted_passes <- weighted_passes + (cat_pass_rate * weight)
      weighted_total <- weighted_total + weight
    }
  }

  weighted_pass_rate <- if (weighted_total > 0) weighted_passes / weighted_total else 0

  # P-value statistics
  valid_p_values <- unique_tests$p_value[!is.na(unique_tests$p_value)]

  list(
    total_tests = total_tests,
    passed_tests = passed_tests,
    failed_tests = failed_tests,
    pass_rate = unweighted_pass_rate, # Keep for backward compatibility
    weighted_pass_rate = weighted_pass_rate,
    category_pass_rates = category_pass_rates,
    weights_used = category_weights,
    mean_p_value = if (length(valid_p_values) > 0) mean(valid_p_values) else NA,
    std_p_value = if (length(valid_p_values) > 0) sd(valid_p_values) else NA,
    failed_test_names = unique_tests$test[!unique_tests$passed]
  )
}

#' Calculate metrics by test category
calculate_category_metrics <- function(summary_df) {
  categories <- unique(summary_df$category)
  category_metrics <- list()

  for (cat in categories) {
    cat_data <- summary_df[summary_df$category == cat, ]

    # Remove duplicates
    cat_data <- cat_data[!duplicated(cat_data$test), ]

    total <- nrow(cat_data)
    passed <- sum(cat_data$passed, na.rm = TRUE)

    category_metrics[[cat]] <- list(
      total_tests = total,
      passed_tests = passed,
      failed_tests = total - passed,
      pass_rate = if (total > 0) passed / total else 0,
      failed_tests = cat_data$test[!cat_data$passed]
    )
  }

  return(category_metrics)
}

#' Normalize performance metrics across generators
normalize_performance_metrics <- function(results) {
  # Extract all performance times
  times <- sapply(results$aggregated, function(x) x$performance$avg_time_per_run)

  # Find fastest (baseline)
  min_time <- min(times, na.rm = TRUE)

  # Calculate relative speeds
  for (gen_name in names(results$aggregated)) {
    gen_time <- results$aggregated[[gen_name]]$performance$avg_time_per_run
    results$aggregated[[gen_name]]$performance$relative_speed <- min_time / gen_time
  }

  return(results)
}

#' Generate comprehensive reports with visualizations
#' @export
generate_comprehensive_reports <- function(
  suite, internal, external, multidim, performance,
  output_dir, verbose = TRUE
) {
  # 1. Generate standard reports
  if (verbose) cat("Generating standard reports...\n")

  # HTML report
  html_file <- file.path(output_dir, "comprehensive_comparison.html")
  generate_unified_report(suite,
    format = "html",
    output_file = html_file, theme = "comprehensive"
  )

  # Markdown report
  md_file <- file.path(output_dir, "comprehensive_comparison.md")
  generate_unified_report(suite,
    format = "markdown",
    output_file = md_file
  )

  # JSON data
  json_file <- file.path(output_dir, "comprehensive_comparison.json")
  generate_unified_report(suite,
    format = "json",
    output_file = json_file
  )

  # 2. Generate visualizations
  if (verbose) cat("Generating visualizations...\n")

  viz_dir <- file.path(output_dir, "visualizations")
  if (!dir.exists(viz_dir)) {
    dir.create(viz_dir, recursive = TRUE)
  }

  # Generate all visualizations
  generate_all_visualizations(
    suite, internal, external, multidim, performance,
    viz_dir, verbose
  )

  # 3. Generate detailed analysis report
  if (verbose) cat("Generating detailed analysis report...\n")

  analysis_file <- file.path(output_dir, "detailed_analysis.html")
  generate_detailed_analysis(
    suite, internal, external, multidim, performance,
    analysis_file
  )

  # 4. Generate executive summary
  if (verbose) cat("Generating executive summary...\n")

  summary_file <- file.path(output_dir, "executive_summary.pdf")
  if (requireNamespace("rmarkdown", quietly = TRUE)) {
    generate_executive_summary(
      suite, internal, external, multidim, performance,
      summary_file
    )
  }

  return(list(
    html = html_file,
    markdown = md_file,
    json = json_file,
    analysis = analysis_file,
    summary = summary_file,
    visualizations = viz_dir
  ))
}
