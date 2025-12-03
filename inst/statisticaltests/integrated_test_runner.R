# File: integrated_test_runner.R
# ----------------------------------------------------------------------
#' Integrated Test Runner with Configuration, Progress Monitoring, and Result Aggregation
#'
#' This module provides a unified test runner that integrates:
#' - Configuration management (tool paths and test parameters)
#' - Progress monitoring with real-time updates
#' - Result aggregation and summary statistics
#'
#' @name integrated-test-runner
NULL

# Load required modules
source_if_exists <- function(path) {
  if (file.exists(path)) {
    source(path)
  } else {
    warning(paste("Module not found:", path))
  }
}

# Load dependencies
source_if_exists("R/statisticaltests/config_manager.R")
source_if_exists("R/statisticaltests/progress_monitor.R")
source_if_exists("R/statisticaltests/result_aggregator.R")

#' Run integrated test suite
#'
#' @param suite Test suite object
#' @param config_file Optional configuration file path
#' @param progress_backend Progress monitoring backend: "console", "file", "callback", "none"
#' @param export_results Whether to export results to file
#' @param export_format Export format: "json", "csv", "rds"
#' @return Test suite with integrated results
#' @export
run_integrated_test_suite <- function(suite,
                                      config_file = NULL,
                                      progress_backend = "console",
                                      export_results = TRUE,
                                      export_format = "json") {
  # 1. Load configuration
  config <- load_config(config_file)

  # Validate configuration
  if (!validate_internal_config(config)) {
    warning("Configuration validation failed. Using defaults for invalid parameters.")
  }

  # Apply configuration to suite
  suite <- apply_config_to_suite(suite, config)

  # 2. Initialize progress monitor
  progress_config <- list(
    use_cli = get_test_param("visualization.enabled", config, TRUE),
    show_eta = TRUE,
    show_category = TRUE,
    log_file = if (progress_backend == "file") {
      paste0("test_progress_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log")
    } else {
      NULL
    }
  )

  progress_monitor <- ProgressMonitor$new(progress_backend, progress_config)

  # 3. Initialize result aggregator
  aggregator_config <- list(
    track_execution_time = TRUE,
    compute_incremental_stats = TRUE,
    p_value_precision = 4
  )

  result_aggregator <- ResultAggregator$new(aggregator_config)

  # 4. Count total tests
  total_tests <- count_total_tests(suite, config)
  categories <- get_test_categories(suite, config)

  # 5. Start progress monitoring
  progress_monitor$start(total_tests, categories)

  # Add progress callback to suite
  suite$progress_callback <- function(event_type, event_data) {
    if (event_type == "test_start") {
      progress_monitor$update_test(event_data$test_name, "running")
    } else if (event_type == "test_complete") {
      progress_monitor$update_test(event_data$test_name, "completed", event_data$details)
      # Add result to aggregator
      result_aggregator$add_result(event_data$category, event_data$test_name, event_data$result)
    } else if (event_type == "test_error") {
      progress_monitor$update_test(event_data$test_name, "failed", event_data$error)
      result_aggregator$add_error(event_data$category, event_data$test_name, event_data$error)
    } else if (event_type == "category_start") {
      progress_monitor$update_category(event_data$category, event_data$total_tests, "running")
    } else if (event_type == "category_complete") {
      progress_monitor$update_category(event_data$category, status = "completed")
    }
  }

  # 6. Run tests with integrated monitoring
  tryCatch(
    {
      # Run each category of tests
      for (category in categories) {
        if (should_run_category(category, config)) {
          suite <- run_category_with_monitoring(suite, category, config, progress_monitor, result_aggregator)
        }
      }

      # Complete progress monitoring
      progress_monitor$complete(list(
        total_tests = result_aggregator$statistics$overall$total_tests,
        passed_tests = result_aggregator$statistics$overall$passed_tests,
        failed_tests = result_aggregator$statistics$overall$failed_tests
      ))
    },
    interrupt = function(e) {
      progress_monitor$cancel()
      warning("Test suite interrupted by user")
    },
    error = function(e) {
      progress_monitor$complete(list(error = e$message))
      stop(e)
    }
  )

  # 7. Compute final statistics
  result_aggregator$compute_statistics()

  # 8. Add integrated results to suite
  suite$integrated_results <- list(
    aggregator = result_aggregator,
    summary = result_aggregator$get_summary(),
    failed_tests = result_aggregator$get_failed_tests(),
    statistics = result_aggregator$statistics
  )

  # 9. Export results if requested
  if (export_results) {
    export_file <- paste0(
      "test_results_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".",
      ifelse(export_format == "json", "json",
        ifelse(export_format == "csv", "csv", "rds")
      )
    )
    result_aggregator$export_results(export_file, export_format)
    message(paste("Results exported to:", export_file))
  }

  # 10. Print summary
  cat("\n", result_aggregator$get_summary("text"), "\n")

  return(suite)
}

#' Apply configuration to test suite
#' @keywords internal
apply_config_to_suite <- function(suite, config) {
  # Apply global parameters
  suite$config$sample_size <- get_test_param(
    "global.default_sample_size", config,
    suite$config$sample_size
  )
  suite$config$significance_level <- get_test_param(
    "global.significance_level", config,
    suite$config$significance_level
  )

  # Apply performance settings
  suite$config$parallel <- get_test_param("performance.parallel_enabled", config, FALSE)
  suite$config$max_cores <- get_test_param(
    "performance.max_cores", config,
    parallel::detectCores() - 1
  )
  suite$config$chunk_size <- get_test_param("performance.chunk_size", config, 1000)

  # Apply visualization settings
  suite$config$save_visualizations <- get_test_param("visualization.enabled", config, TRUE)
  suite$config$plot_format <- get_test_param("visualization.output_format", config, "png")
  suite$config$plot_dpi <- get_test_param("visualization.dpi", config, 300)

  return(suite)
}

#' Count total tests
#' @keywords internal
count_total_tests <- function(suite, config) {
  count <- 0

  # Count basic tests
  if (isTRUE(suite$config$run_basic_tests)) count <- count + 5

  # Count classical tests
  if (isTRUE(suite$config$run_classical_tests)) count <- count + 6

  # Count binary tests
  if (isTRUE(suite$config$run_binary_tests)) count <- count + 8

  # Count correlation tests
  if (isTRUE(suite$config$run_correlation_tests)) count <- count + 4

  # Count compression tests
  if (isTRUE(suite$config$run_compression_tests)) {
    algorithms <- get_test_param(
      "compression_tests.algorithms", config,
      c("gzip", "bzip2", "xz")
    )
    count <- count + length(algorithms)
  }

  # Count external tests
  if (isTRUE(suite$config$run_external_tests)) {
    # Count enabled external tools
    if (!is.null(config$external_tools)) {
      enabled_tools <- sum(sapply(config$external_tools, function(tool) {
        isTRUE(tool$enabled)
      }))
      count <- count + enabled_tools
    }
  }

  return(count)
}

#' Get test categories
#' @keywords internal
get_test_categories <- function(suite, config) {
  categories <- character()

  if (isTRUE(suite$config$run_basic_tests)) categories <- c(categories, "basic")
  if (isTRUE(suite$config$run_classical_tests)) categories <- c(categories, "classical")
  if (isTRUE(suite$config$run_binary_tests)) categories <- c(categories, "binary")
  if (isTRUE(suite$config$run_correlation_tests)) categories <- c(categories, "correlation")
  if (isTRUE(suite$config$run_compression_tests)) categories <- c(categories, "compression")
  if (isTRUE(suite$config$run_external_tests)) categories <- c(categories, "external")

  return(categories)
}

#' Check if category should run
#' @keywords internal
should_run_category <- function(category, config) {
  # Could add category-specific enable/disable logic here
  return(TRUE)
}

#' Run category with monitoring
#' @keywords internal
run_category_with_monitoring <- function(suite, category, config, monitor, aggregator) {
  # Get category-specific configuration
  cat_config <- get_test_config(paste0(category, "_tests"), config)

  # Notify category start
  if (!is.null(suite$progress_callback)) {
    suite$progress_callback("category_start", list(
      category = category,
      total_tests = NA # Would need to count per category
    ))
  }

  # Run tests based on category
  suite <- switch(category,
    basic = run_basic_tests(suite),
    classical = run_classical_tests(suite),
    binary = run_binary_tests(suite),
    correlation = run_correlation_tests(suite),
    compression = run_compression_tests(suite),
    external = run_external_tests_enhanced(suite, config),
    suite
  )

  # Notify category complete
  if (!is.null(suite$progress_callback)) {
    suite$progress_callback("category_complete", list(category = category))
  }

  return(suite)
}

#' Create parallel-aware test runner
#'
#' @param config Configuration object
#' @export
create_parallel_test_runner <- function(config = NULL) {
  if (is.null(config)) {
    config <- load_config()
  }

  # Check if parallel is enabled
  parallel_enabled <- get_test_param("performance.parallel_enabled", config, TRUE)
  max_cores <- get_test_param("performance.max_cores", config, parallel::detectCores() - 1)

  if (!parallel_enabled) {
    return(NULL)
  }

  # Set up parallel backend
  if (requireNamespace("parallel", quietly = TRUE)) {
    cl <- parallel::makeCluster(max_cores)

    # Export configuration to workers
    parallel::clusterExport(cl, c("config", "load_config", "get_test_param"),
      envir = environment()
    )

    return(cl)
  }

  return(NULL)
}

#' Example usage function
#' @export
demo_integrated_runner <- function() {
  # Create example test suite
  suite <- list(
    config = list(
      run_basic_tests = TRUE,
      run_classical_tests = TRUE,
      run_binary_tests = FALSE,
      run_correlation_tests = TRUE,
      run_compression_tests = TRUE,
      run_external_tests = FALSE,
      sample_size = 10000,
      significance_level = 0.01
    ),
    prng_func = function(n) runif(n)
  )

  # Run with integrated features
  suite <- run_integrated_test_suite(
    suite,
    config_file = NULL, # Use defaults
    progress_backend = "console",
    export_results = TRUE,
    export_format = "json"
  )

  return(suite)
}
