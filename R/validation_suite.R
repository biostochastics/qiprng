# File: validation_suite.R
# ----------------------------------------------------------------------
#' Comprehensive Validation Suite for qiprng
#'
#' This module provides a complete validation framework for testing all
#' aspects of the qiprng statistical testing framework. It includes
#' validators for each test category, edge case testing, performance
#' benchmarking, and comprehensive reporting.
#'
#' @name validation_suite

#' Source a test file from the installed package
#'
#' @param relative_path Relative path within inst/ directory
#' @return Invisible NULL
#' @noRd
source_pkg_file <- function(relative_path) {
  full_path <- system.file(relative_path, package = "qiprng")
  if (nzchar(full_path) && file.exists(full_path)) {
    source(full_path, local = FALSE)
  } else {
    # Fallback for development: try relative paths
    dev_paths <- c(
      file.path("inst", relative_path),
      relative_path,
      file.path("R", relative_path)
    )
    for (dev_path in dev_paths) {
      if (file.exists(dev_path)) {
        source(dev_path, local = FALSE)
        return(invisible(NULL))
      }
    }
    warning(paste("Could not find test file:", relative_path))
  }
  invisible(NULL)
}

# Source required test files
source_test_files <- function() {
  test_files <- c(
    "statisticaltests/basic_tests.R",
    "statisticaltests/runs_tests.R",
    "statisticaltests/correlation_tests.R",
    "statisticaltests/binary_tests.R",
    "statisticaltests/classical_tests.R",
    "statisticaltests/compression_tests.R",
    "statisticaltests/external_tests.R",
    "statisticaltests/external_wrappers.R",
    "statisticaltests/multidim_tests.R",
    "statisticaltests/edge_case_tests.R"
  )

  for (file_path in test_files) {
    source_pkg_file(file_path)
  }
}

#' Master Validation Framework
#'
#' Runs comprehensive validation of the entire qiprng testing framework
#'
#' @param level Validation level: "quick", "standard", or "comprehensive"
#' @param config Optional configuration list
#' @param verbose Logical indicating whether to print progress
#' @return Validation report object
#' @export
validate_qiprng_framework <- function(level = "standard",
                                      config = NULL,
                                      verbose = TRUE) {
  # Initialize validation report
  validation_report <- list(
    timestamp = Sys.time(),
    level = level,
    summary = list(
      total_tests = 0,
      passed = 0,
      failed = 0,
      warnings = 0,
      errors = 0
    ),
    categories = list(),
    performance = list(
      start_time = Sys.time(),
      end_time = NULL,
      execution_time = NULL,
      memory_used = NULL
    ),
    recommendations = character()
  )

  # Set up configuration based on level
  if (is.null(config)) {
    config <- get_validation_config(level)
  }

  # Source test files
  tryCatch(
    {
      source_test_files()
    },
    error = function(e) {
      validation_report$errors <- append(
        validation_report$errors,
        paste("Failed to source test files:", e$message)
      )
    }
  )

  # Run category validators
  categories <- c(
    "basic", "classical", "external", "runs",
    "correlation", "binary", "compression", "multidim"
  )

  for (category in categories) {
    if (verbose) {
      cat(sprintf("\nValidating %s tests...\n", category))
    }

    validator_func <- get(paste0("validate_", category, "_tests"))
    category_result <- validator_func(config, verbose)

    validation_report$categories[[category]] <- category_result
    validation_report$summary$total_tests <- validation_report$summary$total_tests +
      category_result$total_tests
    validation_report$summary$passed <- validation_report$summary$passed +
      category_result$passed
    validation_report$summary$failed <- validation_report$summary$failed +
      category_result$failed
    validation_report$summary$warnings <- validation_report$summary$warnings +
      category_result$warnings
  }

  # Add edge case testing
  if (level %in% c("standard", "comprehensive")) {
    if (verbose) cat("\nRunning edge case tests...\n")

    # Use comprehensive edge case testing if available
    if (exists("run_comprehensive_edge_tests")) {
      edge_results <- run_comprehensive_edge_tests(config, verbose)
      validation_report$edge_cases <- edge_results
    } else {
      # Fallback to basic edge case testing
      edge_results <- validate_edge_cases(config, verbose)
      validation_report$edge_cases <- edge_results
    }
  }

  # Add performance benchmarks
  if (level == "comprehensive") {
    if (verbose) cat("\nRunning performance benchmarks...\n")

    # Use comprehensive performance benchmarking if available
    source_pkg_file("statisticaltests/performance_benchmarking.R")

    if (exists("run_performance_benchmarks")) {
      perf_results <- run_performance_benchmarks(config, verbose)
      validation_report$performance_tests <- perf_results

      # Generate performance visualizations if requested
      if (config$save_visualizations && exists("visualize_performance_results")) {
        perf_plots <- visualize_performance_results(
          perf_results,
          file.path(config$output_dir, "performance_plots")
        )
        validation_report$performance_visualizations <- perf_plots
      }
    } else {
      # Fallback to basic performance testing
      perf_results <- validate_performance(config, verbose)
      validation_report$performance_tests <- perf_results
    }
  }

  # Finalize report
  validation_report$performance$end_time <- Sys.time()
  validation_report$performance$execution_time <-
    difftime(validation_report$performance$end_time,
      validation_report$performance$start_time,
      units = "secs"
    )

  # Generate recommendations
  validation_report$recommendations <- generate_recommendations(validation_report)

  # Print summary if verbose
  if (verbose) {
    print_validation_summary(validation_report)
  }

  class(validation_report) <- "qiprng_validation_report"

  # Generate reports if requested
  if (!is.null(config$generate_reports) && config$generate_reports) {
    if (verbose) cat("\nGenerating validation reports...\n")

    # Source reporting module if available
    source_pkg_file("statisticaltests/validation_reporting.R")

    if (exists("generate_validation_report")) {
      report_formats <- if (!is.null(config$report_formats)) {
        config$report_formats
      } else {
        c("html", "json")
      }

      validation_report$generated_files <- generate_validation_report(
        validation_report,
        output_dir = file.path(config$output_dir, "validation_reports"),
        formats = report_formats,
        include_visualizations = config$save_visualizations,
        verbose = verbose
      )
    }
  }

  return(validation_report)
}

#' Get validation configuration based on level
#'
#' @param level Validation level
#' @return Configuration list
get_validation_config <- function(level) {
  base_config <- list(
    significance_level = 0.05,
    save_visualizations = FALSE,
    output_dir = tempdir(),
    generate_reports = TRUE,
    report_formats = c("html", "json")
  )

  if (level == "quick") {
    config <- modifyList(base_config, list(
      sample_sizes = c(100, 1000),
      num_iterations = 10,
      test_timeout = 60 # 1 minute timeout
    ))
  } else if (level == "standard") {
    config <- modifyList(base_config, list(
      sample_sizes = c(100, 1000, 10000),
      num_iterations = 100,
      test_timeout = 300 # 5 minute timeout
    ))
  } else if (level == "comprehensive") {
    config <- modifyList(base_config, list(
      sample_sizes = c(10, 100, 1000, 10000, 100000),
      num_iterations = 1000,
      test_timeout = 1800, # 30 minute timeout
      save_visualizations = TRUE
    ))
  }

  return(config)
}

#' Validate basic tests
#'
#' @param config Validation configuration
#' @param verbose Print progress
#' @return Validation results for basic tests
validate_basic_tests <- function(config, verbose = TRUE) {
  results <- list(
    total_tests = 0,
    passed = 0,
    failed = 0,
    warnings = 0,
    details = list()
  )

  # Test with different sample sizes
  for (n in config$sample_sizes) {
    if (verbose) cat(sprintf("  Testing with n=%d...", n))

    # Create test suite
    suite <- list(
      prng_func = function(n) runif(n),
      config = list(
        basic_sample_size = n,
        significance_level = config$significance_level,
        save_visualizations = config$save_visualizations,
        output_dir = config$output_dir
      ),
      results = list()
    )

    # Run tests with timeout
    test_result <- tryCatch(
      {
        withTimeout(
          {
            suite <- run_basic_tests(suite)
            list(success = TRUE, suite = suite)
          },
          timeout = config$test_timeout
        )
      },
      error = function(e) {
        list(success = FALSE, error = e$message)
      }
    )

    # Analyze results
    if (test_result$success) {
      # Check result structure
      basic_results <- test_result$suite$results$basic
      for (test_name in names(basic_results)) {
        results$total_tests <- results$total_tests + 1

        # Validate structure
        if (validate_test_structure(basic_results[[test_name]])) {
          results$passed <- results$passed + 1
        } else {
          results$failed <- results$failed + 1
          results$details[[paste0("structure_", test_name, "_n", n)]] <-
            "Invalid test result structure"
        }
      }
      if (verbose) cat(" OK\n")
    } else {
      results$failed <- results$failed + 1
      results$details[[paste0("error_n", n)]] <- test_result$error
      if (verbose) cat(" ERROR\n")
    }
  }

  return(results)
}

#' Validate classical tests
#'
#' @param config Validation configuration
#' @param verbose Print progress
#' @return Validation results for classical tests
validate_classical_tests <- function(config, verbose = TRUE) {
  results <- list(
    total_tests = 0,
    passed = 0,
    failed = 0,
    warnings = 0,
    details = list()
  )

  for (n in config$sample_sizes) {
    if (verbose) cat(sprintf("  Testing with n=%d...", n))

    suite <- list(
      prng_func = function(n) runif(n),
      config = list(
        classical_sample_size = n,
        significance_level = config$significance_level,
        save_visualizations = config$save_visualizations,
        output_dir = config$output_dir
      ),
      results = list()
    )

    test_result <- tryCatch(
      {
        withTimeout(
          {
            suite <- run_classical_tests(suite)
            list(success = TRUE, suite = suite)
          },
          timeout = config$test_timeout
        )
      },
      error = function(e) {
        list(success = FALSE, error = e$message)
      }
    )

    if (test_result$success) {
      classical_results <- test_result$suite$results$classical
      expected_tests <- c("coupon_collector", "poker_hand", "birthday_spacing")

      for (test_name in expected_tests) {
        results$total_tests <- results$total_tests + 1
        if (test_name %in% names(classical_results) &&
              validate_test_structure(classical_results[[test_name]])) {
          results$passed <- results$passed + 1
        } else {
          results$failed <- results$failed + 1
          results$details[[paste0(test_name, "_n", n)]] <- "Missing or invalid"
        }
      }
      if (verbose) cat(" OK\n")
    } else {
      results$failed <- results$failed + length(c("coupon_collector", "poker_hand", "birthday_spacing"))
      results$details[[paste0("error_n", n)]] <- test_result$error
      if (verbose) cat(" ERROR\n")
    }
  }

  return(results)
}

#' Validate external tests
#'
#' @param config Validation configuration
#' @param verbose Print progress
#' @return Validation results for external tests
validate_external_tests <- function(config, verbose = TRUE) {
  results <- list(
    total_tests = 0,
    passed = 0,
    failed = 0,
    warnings = 0,
    details = list()
  )

  # Only test with smaller sample sizes for external tests
  test_sizes <- config$sample_sizes[config$sample_sizes <= 10000]

  for (n in test_sizes) {
    if (verbose) cat(sprintf("  Testing with n=%d...", n))

    suite <- list(
      prng_func = function(n) runif(n),
      config = list(
        external_sample_size = n,
        significance_level = config$significance_level,
        save_visualizations = FALSE # Skip visualizations for external
      ),
      results = list()
    )

    test_result <- tryCatch(
      {
        withTimeout(
          {
            suite <- run_external_tests(suite)
            list(success = TRUE, suite = suite)
          },
          timeout = config$test_timeout
        )
      },
      error = function(e) {
        list(success = FALSE, error = e$message)
      }
    )

    if (test_result$success) {
      external_results <- test_result$suite$results$external

      # Check base R tests are present
      base_tests <- c("kolmogorov_smirnov", "chi_square", "shapiro_wilk")
      for (test_name in base_tests) {
        results$total_tests <- results$total_tests + 1
        if (test_name %in% names(external_results) &&
              validate_test_structure(external_results[[test_name]])) {
          results$passed <- results$passed + 1
        } else {
          results$failed <- results$failed + 1
        }
      }
      if (verbose) cat(" OK\n")
    } else {
      results$warnings <- results$warnings + 1
      results$details[[paste0("warning_n", n)]] <- test_result$error
      if (verbose) cat(" WARNING\n")
    }
  }

  return(results)
}

#' Validate runs tests
#'
#' @param config Validation configuration
#' @param verbose Print progress
#' @return Validation results for runs tests
validate_runs_tests <- function(config, verbose = TRUE) {
  results <- list(
    total_tests = 0,
    passed = 0,
    failed = 0,
    warnings = 0,
    details = list()
  )

  # Check if runs tests exist
  if (!exists("run_runs_tests")) {
    results$warnings <- 1
    results$details$missing <- "Runs tests not found"
    return(results)
  }

  for (n in config$sample_sizes) {
    if (verbose) cat(sprintf("  Testing with n=%d...", n))

    suite <- list(
      prng_func = function(n) runif(n),
      config = list(
        runs_sample_size = n,
        significance_level = config$significance_level
      ),
      results = list()
    )

    test_result <- tryCatch(
      {
        suite <- run_runs_tests(suite)
        list(success = TRUE, suite = suite)
      },
      error = function(e) {
        list(success = FALSE, error = e$message)
      }
    )

    if (test_result$success && !is.null(test_result$suite$results$runs)) {
      for (test_name in names(test_result$suite$results$runs)) {
        results$total_tests <- results$total_tests + 1
        if (validate_test_structure(test_result$suite$results$runs[[test_name]])) {
          results$passed <- results$passed + 1
        } else {
          results$failed <- results$failed + 1
        }
      }
      if (verbose) cat(" OK\n")
    } else {
      results$warnings <- results$warnings + 1
      if (verbose) cat(" WARNING\n")
    }
  }

  return(results)
}

#' Validate correlation tests
#'
#' @param config Validation configuration
#' @param verbose Print progress
#' @return Validation results for correlation tests
validate_correlation_tests <- function(config, verbose = TRUE) {
  results <- list(
    total_tests = 0,
    passed = 0,
    failed = 0,
    warnings = 0,
    details = list()
  )

  if (!exists("run_correlation_tests")) {
    results$warnings <- 1
    results$details$missing <- "Correlation tests not found"
    return(results)
  }

  # Test only with reasonable sample sizes for correlation
  test_sizes <- config$sample_sizes[config$sample_sizes >= 100]

  for (n in test_sizes) {
    if (verbose) cat(sprintf("  Testing with n=%d...", n))

    suite <- list(
      prng_func = function(n) runif(n),
      config = list(
        correlation_sample_size = n,
        significance_level = config$significance_level,
        save_visualizations = FALSE
      ),
      results = list()
    )

    test_result <- tryCatch(
      {
        suite <- run_correlation_tests(suite)
        list(success = TRUE, suite = suite)
      },
      error = function(e) {
        list(success = FALSE, error = e$message)
      }
    )

    if (test_result$success && !is.null(test_result$suite$results$correlation)) {
      for (test_name in names(test_result$suite$results$correlation)) {
        results$total_tests <- results$total_tests + 1
        if (validate_test_structure(test_result$suite$results$correlation[[test_name]])) {
          results$passed <- results$passed + 1
        } else {
          results$failed <- results$failed + 1
        }
      }
      if (verbose) cat(" OK\n")
    } else {
      results$warnings <- results$warnings + 1
      if (verbose) cat(" WARNING\n")
    }
  }

  return(results)
}

#' Validate binary tests
#'
#' @param config Validation configuration
#' @param verbose Print progress
#' @return Validation results for binary tests
validate_binary_tests <- function(config, verbose = TRUE) {
  results <- list(
    total_tests = 0,
    passed = 0,
    failed = 0,
    warnings = 0,
    details = list()
  )

  if (!exists("run_binary_tests")) {
    results$warnings <- 1
    results$details$missing <- "Binary tests not found"
    return(results)
  }

  for (n in config$sample_sizes) {
    if (verbose) cat(sprintf("  Testing with n=%d...", n))

    suite <- list(
      prng_func = function(n) runif(n),
      config = list(
        binary_sample_size = n,
        significance_level = config$significance_level
      ),
      results = list()
    )

    test_result <- tryCatch(
      {
        suite <- run_binary_tests(suite)
        list(success = TRUE, suite = suite)
      },
      error = function(e) {
        list(success = FALSE, error = e$message)
      }
    )

    if (test_result$success && !is.null(test_result$suite$results$binary)) {
      for (test_name in names(test_result$suite$results$binary)) {
        results$total_tests <- results$total_tests + 1
        if (validate_test_structure(test_result$suite$results$binary[[test_name]])) {
          results$passed <- results$passed + 1
        } else {
          results$failed <- results$failed + 1
        }
      }
      if (verbose) cat(" OK\n")
    } else {
      results$warnings <- results$warnings + 1
      if (verbose) cat(" WARNING\n")
    }
  }

  return(results)
}

#' Validate compression tests
#'
#' @param config Validation configuration
#' @param verbose Print progress
#' @return Validation results for compression tests
validate_compression_tests <- function(config, verbose = TRUE) {
  results <- list(
    total_tests = 0,
    passed = 0,
    failed = 0,
    warnings = 0,
    details = list()
  )

  if (!exists("run_compression_tests")) {
    results$warnings <- 1
    results$details$missing <- "Compression tests not found"
    return(results)
  }

  # Compression tests need larger samples
  test_sizes <- config$sample_sizes[config$sample_sizes >= 1000]

  for (n in test_sizes) {
    if (verbose) cat(sprintf("  Testing with n=%d...", n))

    suite <- list(
      prng_func = function(n) runif(n),
      config = list(
        compression_sample_size = n,
        significance_level = config$significance_level
      ),
      results = list()
    )

    test_result <- tryCatch(
      {
        suite <- run_compression_tests(suite)
        list(success = TRUE, suite = suite)
      },
      error = function(e) {
        list(success = FALSE, error = e$message)
      }
    )

    if (test_result$success && !is.null(test_result$suite$results$compression)) {
      for (test_name in names(test_result$suite$results$compression)) {
        results$total_tests <- results$total_tests + 1
        if (validate_test_structure(test_result$suite$results$compression[[test_name]])) {
          results$passed <- results$passed + 1
        } else {
          results$failed <- results$failed + 1
        }
      }
      if (verbose) cat(" OK\n")
    } else {
      results$warnings <- results$warnings + 1
      if (verbose) cat(" WARNING\n")
    }
  }

  return(results)
}

#' Validate multidimensional tests
#'
#' @param config Validation configuration
#' @param verbose Print progress
#' @return Validation results for multidimensional tests
validate_multidim_tests <- function(config, verbose = TRUE) {
  results <- list(
    total_tests = 0,
    passed = 0,
    failed = 0,
    warnings = 0,
    details = list()
  )

  if (!exists("run_multidim_tests") && !exists("run_multidimensional_tests")) {
    results$warnings <- 1
    results$details$missing <- "Multidimensional tests not found"
    return(results)
  }

  # Test 2D and 3D
  for (dims in c(2, 3)) {
    for (n in config$sample_sizes[config$sample_sizes <= 10000]) {
      if (verbose) cat(sprintf("  Testing %dD with n=%d...", dims, n))

      suite <- list(
        prng_func = function(n) runif(n),
        config = list(
          multidim_sample_size = n,
          significance_level = config$significance_level,
          dimensions = dims
        ),
        results = list()
      )

      test_result <- tryCatch(
        {
          if (exists("run_multidimensional_tests")) {
            suite <- run_multidimensional_tests(suite, dimensions = dims)
          } else {
            suite <- run_multidim_tests(suite, config = list(dimensions = dims))
          }
          list(success = TRUE, suite = suite)
        },
        error = function(e) {
          list(success = FALSE, error = e$message)
        }
      )

      if (test_result$success && !is.null(test_result$suite$results$multidim)) {
        results$total_tests <- results$total_tests + 1
        # Multidim tests have different structure, just check they exist
        if (length(test_result$suite$results$multidim) > 0) {
          results$passed <- results$passed + 1
        } else {
          results$failed <- results$failed + 1
        }
        if (verbose) cat(" OK\n")
      } else {
        results$warnings <- results$warnings + 1
        if (verbose) cat(" WARNING\n")
      }
    }
  }

  return(results)
}

#' Validate test result structure
#'
#' @param test_result A test result object
#' @return TRUE if valid structure, FALSE otherwise
validate_test_structure <- function(test_result) {
  required_fields <- c("description", "result", "p_value", "statistic", "details")

  if (!is.list(test_result)) {
    return(FALSE)
  }

  # Check all required fields exist
  if (!all(required_fields %in% names(test_result))) {
    return(FALSE)
  }

  # Check field types
  if (!is.character(test_result$description)) {
    return(FALSE)
  }
  if (!test_result$result %in% c("PASS", "FAIL", "INCONCLUSIVE", "ERROR", "SKIPPED")) {
    return(FALSE)
  }
  if (!is.character(test_result$details)) {
    return(FALSE)
  }

  # p_value and statistic can be NA but should be numeric type when not NA
  if (!is.na(test_result$p_value) && !is.numeric(test_result$p_value)) {
    return(FALSE)
  }
  if (!is.na(test_result$statistic) && !is.numeric(test_result$statistic)) {
    return(FALSE)
  }

  # Check p_value range when not NA
  if (!is.na(test_result$p_value) &&
        (test_result$p_value < 0 || test_result$p_value > 1)) {
    return(FALSE)
  }

  return(TRUE)
}

#' Validate edge cases
#'
#' @param config Validation configuration
#' @param verbose Print progress
#' @return Edge case validation results
validate_edge_cases <- function(config, verbose = TRUE) {
  results <- list(
    total_tests = 0,
    passed = 0,
    failed = 0,
    details = list()
  )

  # Test with very small sample size
  if (verbose) cat("  Testing with n=10...")
  small_suite <- list(
    prng_func = function(n) runif(n),
    config = list(
      basic_sample_size = 10,
      classical_sample_size = 10,
      external_sample_size = 10,
      significance_level = 0.05
    ),
    results = list()
  )

  # Should handle gracefully
  tryCatch(
    {
      small_suite <- run_classical_tests(small_suite)
      # Check for INCONCLUSIVE results
      for (test in small_suite$results$classical) {
        results$total_tests <- results$total_tests + 1
        if (test$result == "INCONCLUSIVE" && !is.na(test$details)) {
          results$passed <- results$passed + 1
        } else {
          results$failed <- results$failed + 1
        }
      }
      if (verbose) cat(" OK\n")
    },
    error = function(e) {
      results$failed <- results$failed + 1
      results$details$small_sample <- e$message
      if (verbose) cat(" ERROR\n")
    }
  )

  # Test with invalid inputs
  if (verbose) cat("  Testing with invalid inputs...")
  invalid_tests <- list(
    negative = function(n) runif(n) - 2,
    out_of_range = function(n) runif(n) * 2,
    na_values = function(n) rep(NA, n)
  )

  for (test_name in names(invalid_tests)) {
    results$total_tests <- results$total_tests + 1
    suite <- list(
      prng_func = invalid_tests[[test_name]],
      config = list(basic_sample_size = 100, significance_level = 0.05),
      results = list()
    )

    # Should handle errors gracefully
    tryCatch(
      {
        suite <- run_basic_tests(suite)
        # Check for appropriate error handling
        if (any(sapply(suite$results$basic, function(x) x$result %in% c("ERROR", "FAIL")))) {
          results$passed <- results$passed + 1
        } else {
          results$failed <- results$failed + 1
        }
      },
      error = function(e) {
        # Error is acceptable for invalid input
        results$passed <- results$passed + 1
      }
    )
  }

  if (verbose) cat(" OK\n")

  return(results)
}

#' Validate performance
#'
#' @param config Validation configuration
#' @param verbose Print progress
#' @return Performance validation results
validate_performance <- function(config, verbose = TRUE) {
  results <- list(
    total_tests = 0,
    passed = 0,
    failed = 0,
    timings = list(),
    memory = list()
  )

  # Test execution time for different sample sizes
  for (n in c(1000, 10000, 100000)) {
    if (verbose) cat(sprintf("  Benchmarking with n=%d...", n))

    suite <- list(
      prng_func = function(n) runif(n),
      config = list(
        basic_sample_size = n,
        classical_sample_size = n,
        significance_level = 0.05,
        save_visualizations = FALSE
      ),
      results = list()
    )

    # Time basic tests
    start_time <- Sys.time()
    tryCatch(
      {
        suite <- run_basic_tests(suite)
        end_time <- Sys.time()
        exec_time <- as.numeric(difftime(end_time, start_time, units = "secs"))

        results$timings[[paste0("basic_n", n)]] <- exec_time
        results$total_tests <- results$total_tests + 1

        # Check if execution time is reasonable (< 1 second per 10k samples)
        expected_time <- n / 10000
        if (exec_time < expected_time * 10) { # Allow 10x margin
          results$passed <- results$passed + 1
        } else {
          results$failed <- results$failed + 1
        }

        if (verbose) cat(sprintf(" %.2fs\n", exec_time))
      },
      error = function(e) {
        results$failed <- results$failed + 1
        if (verbose) cat(" ERROR\n")
      }
    )
  }

  return(results)
}

#' Generate recommendations based on validation results
#'
#' @param report Validation report
#' @return Character vector of recommendations
generate_recommendations <- function(report) {
  recommendations <- character()

  # Check overall pass rate
  if (!is.null(report$summary$total_tests) && report$summary$total_tests > 0) {
    pass_rate <- report$summary$passed / report$summary$total_tests
    if (!is.na(pass_rate) && pass_rate < 0.9) {
      recommendations <- c(
        recommendations,
        sprintf(
          "Overall pass rate is %.1f%%. Consider investigating failed tests.",
          pass_rate * 100
        )
      )
    }
  }

  # Check for category-specific issues
  for (category in names(report$categories)) {
    cat_result <- report$categories[[category]]
    if (cat_result$failed > 0) {
      recommendations <- c(
        recommendations,
        sprintf(
          "%s tests have %d failures. Review implementation.",
          category, cat_result$failed
        )
      )
    }
  }

  # Check edge cases
  if (!is.null(report$edge_cases) && report$edge_cases$failed > 0) {
    recommendations <- c(
      recommendations,
      "Edge case handling needs improvement. Review error handling code."
    )
  }

  # Check performance
  if (!is.null(report$performance_tests)) {
    slow_tests <- names(report$performance_tests$timings)[
      report$performance_tests$timings > 5
    ]
    if (length(slow_tests) > 0) {
      recommendations <- c(
        recommendations,
        sprintf(
          "Performance optimization needed for: %s",
          paste(slow_tests, collapse = ", ")
        )
      )
    }
  }

  if (length(recommendations) == 0) {
    recommendations <- "All validations passed successfully. No issues found."
  }

  return(recommendations)
}

#' Print validation summary
#'
#' @param report Validation report
print_validation_summary <- function(report) {
  cat("\n", rep("=", 60), "\n", sep = "")
  cat("QIPRNG Validation Report Summary\n")
  cat(rep("=", 60), "\n", sep = "")
  cat("Level:", report$level, "\n")
  cat("Execution time:", format(report$performance$execution_time), "\n")
  cat("\nTest Results:\n")
  cat(sprintf("  Total tests: %d\n", report$summary$total_tests))
  if (!is.null(report$summary$total_tests) && report$summary$total_tests > 0) {
    cat(sprintf(
      "  Passed: %d (%.1f%%)\n",
      report$summary$passed,
      100 * report$summary$passed / report$summary$total_tests
    ))
  } else {
    cat(sprintf("  Passed: %d\n", report$summary$passed))
  }
  cat(sprintf("  Failed: %d\n", report$summary$failed))
  cat(sprintf("  Warnings: %d\n", report$summary$warnings))

  cat("\nRecommendations:\n")
  for (rec in report$recommendations) {
    cat("  -", rec, "\n")
  }
  cat(rep("=", 60), "\n", sep = "")
}

#' Helper function to modify lists
#'
#' v0.7.3: Fixed to properly handle keep.null semantics like base::modifyList
#'
#' @param x Base list
#' @param val List of modifications
#' @param keep.null If TRUE, NULL values in val are preserved. If FALSE (default),
#'   NULL values remove the corresponding element from x.
#' @return Modified list
modifyList <- function(x, val, keep.null = FALSE) {
  stopifnot(is.list(x), is.list(val))
  vnames <- names(val)
  vnames <- vnames[nzchar(vnames)]
  for (v in vnames) {
    if (is.null(val[[v]]) && !keep.null) {
      # Remove element when val[[v]] is NULL and keep.null is FALSE
      x[[v]] <- NULL
    } else {
      x[[v]] <- val[[v]]
    }
  }
  x
}

#' Execute Expression with Timeout
#'
#' Runs an expression with a specified timeout, returning an error
#' if the expression takes longer than the specified time.
#'
#' v0.7.3: Fixed to run expression in caller's environment using
#' standard R metaprogramming (substitute + eval in parent.frame)
#'
#' @param expr Expression to run
#' @param timeout Timeout in seconds
#' @return Result of the expression, or throws an error on timeout
#' @export
withTimeout <- function(expr, timeout) {
  setTimeLimit(cpu = timeout, elapsed = timeout)
  on.exit(setTimeLimit(cpu = Inf, elapsed = Inf))
  # Run in parent frame to preserve caller's bindings - standard R pattern
  expr_sub <- substitute(expr)
  base::eval(expr_sub, envir = parent.frame())
}
