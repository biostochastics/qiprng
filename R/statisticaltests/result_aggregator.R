# File: result_aggregator.R
# ----------------------------------------------------------------------
#' Result Aggregation Framework for PRNG Test Suite
#'
#' This module provides comprehensive result aggregation capabilities
#' for the PRNG test suite, supporting incremental aggregation,
#' error handling, and summary statistics.
#'
#' Features:
#' \itemize{
#'   \item Incremental result aggregation
#'   \item Partial result handling
#'   \item Statistical summaries
#'   \item Error recovery
#'   \item Multi-level aggregation (test, category, suite)
#' }
#'
#' @name result-aggregator
#' @import stats
NULL

#' Result aggregator class
#'
#' @export
ResultAggregator <- R6::R6Class("ResultAggregator",
  public = list(
    #' @field results Aggregated results
    results = NULL,

    #' @field config Aggregator configuration
    config = NULL,

    #' @field statistics Summary statistics
    statistics = NULL,

    #' @field errors Collection of errors
    errors = NULL,

    #' Initialize result aggregator
    #'
    #' @param config Configuration options
    initialize = function(config = list()) {
      self$config <- merge_aggregator_config(config)
      self$results <- list(
        by_category = list(),
        by_test = list(),
        overall = list(),
        raw = list()
      )
      self$statistics <- list()
      self$errors <- list()
    },

    #' Add test result
    #'
    #' @param category Test category
    #' @param test_name Test name
    #' @param result Test result object
    #' @param metadata Additional metadata
    add_result = function(category, test_name, result, metadata = NULL) {
      # Store raw result
      if (!category %in% names(self$results$raw)) {
        self$results$raw[[category]] <- list()
      }
      self$results$raw[[category]][[test_name]] <- result

      # Process and aggregate
      processed <- private$process_result(result, test_name, category)

      # Add to category aggregation
      if (!category %in% names(self$results$by_category)) {
        self$results$by_category[[category]] <- private$init_category_stats()
      }
      private$update_category_stats(category, processed)

      # Add to test aggregation
      if (!test_name %in% names(self$results$by_test)) {
        self$results$by_test[[test_name]] <- list()
      }
      self$results$by_test[[test_name]][[category]] <- processed

      # Update overall statistics
      private$update_overall_stats(processed)

      # Store metadata if provided
      if (!is.null(metadata)) {
        if (is.null(self$results$metadata)) {
          self$results$metadata <- list()
        }
        self$results$metadata[[paste(category, test_name, sep = "_")]] <- metadata
      }
    },

    #' Add error
    #'
    #' @param category Test category
    #' @param test_name Test name
    #' @param error Error message or condition
    #' @param severity Error severity: "warning", "error", "critical"
    add_error = function(category, test_name, error, severity = "error") {
      error_entry <- list(
        timestamp = Sys.time(),
        category = category,
        test = test_name,
        error = as.character(error),
        severity = severity
      )

      self$errors[[length(self$errors) + 1]] <- error_entry

      # Update statistics
      if (!category %in% names(self$results$by_category)) {
        self$results$by_category[[category]] <- private$init_category_stats()
      }
      self$results$by_category[[category]]$error_count <-
        self$results$by_category[[category]]$error_count + 1
    },

    #' Aggregate results from multiple sources
    #'
    #' @param result_list List of results to aggregate
    #' @param parallel Whether results come from parallel execution
    aggregate_batch = function(result_list, parallel = FALSE) {
      if (parallel) {
        # Handle results from parallel workers
        for (worker_results in result_list) {
          if (!is.null(worker_results$results)) {
            for (cat in names(worker_results$results)) {
              for (test in names(worker_results$results[[cat]])) {
                self$add_result(cat, test, worker_results$results[[cat]][[test]])
              }
            }
          }

          # Handle errors from workers
          if (!is.null(worker_results$errors)) {
            for (error in worker_results$errors) {
              self$add_error(error$category, error$test, error$error, error$severity)
            }
          }
        }
      } else {
        # Sequential aggregation
        for (result in result_list) {
          if (!is.null(result$category) && !is.null(result$test) && !is.null(result$result)) {
            self$add_result(result$category, result$test, result$result)
          }
        }
      }

      # Recompute statistics
      self$compute_statistics()
    },

    #' Compute summary statistics
    compute_statistics = function() {
      # Overall pass rate
      total_tests <- 0
      passed_tests <- 0
      failed_tests <- 0

      for (cat in names(self$results$by_category)) {
        cat_stats <- self$results$by_category[[cat]]
        total_tests <- total_tests + cat_stats$total_tests
        passed_tests <- passed_tests + cat_stats$passed_tests
        failed_tests <- failed_tests + cat_stats$failed_tests
      }

      self$statistics$overall <- list(
        total_tests = total_tests,
        passed_tests = passed_tests,
        failed_tests = failed_tests,
        error_count = length(self$errors),
        pass_rate = if (total_tests > 0) passed_tests / total_tests else NA,
        categories_tested = length(self$results$by_category),
        unique_tests = length(self$results$by_test)
      )

      # Category-level statistics
      self$statistics$by_category <- lapply(self$results$by_category, function(cat_stats) {
        list(
          pass_rate = if (cat_stats$total_tests > 0) {
            cat_stats$passed_tests / cat_stats$total_tests
          } else {
            NA
          },
          avg_p_value = if (length(cat_stats$p_values) > 0) {
            mean(cat_stats$p_values, na.rm = TRUE)
          } else {
            NA
          },
          min_p_value = if (length(cat_stats$p_values) > 0) {
            min(cat_stats$p_values, na.rm = TRUE)
          } else {
            NA
          },
          error_rate = if (cat_stats$total_tests > 0) {
            cat_stats$error_count / cat_stats$total_tests
          } else {
            NA
          }
        )
      })

      # Test-level statistics
      self$statistics$by_test <- lapply(self$results$by_test, function(test_results) {
        p_values <- sapply(test_results, function(r) r$p_value)
        p_values <- p_values[!is.na(p_values)]

        list(
          categories_run = length(test_results),
          avg_p_value = if (length(p_values) > 0) mean(p_values) else NA,
          min_p_value = if (length(p_values) > 0) min(p_values) else NA,
          max_p_value = if (length(p_values) > 0) max(p_values) else NA,
          consistency = if (length(p_values) > 1) 1 - sd(p_values) else NA
        )
      })

      # Distribution of p-values
      all_p_values <- unlist(lapply(self$results$by_category, function(cat) cat$p_values))
      all_p_values <- all_p_values[!is.na(all_p_values)]

      if (length(all_p_values) > 0) {
        self$statistics$p_value_distribution <- list(
          mean = mean(all_p_values),
          median = median(all_p_values),
          sd = sd(all_p_values),
          quantiles = quantile(all_p_values, probs = c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99)),
          uniformity_test = ks.test(all_p_values, "punif")$p.value
        )
      }
    },

    #' Get summary report
    #'
    #' @param format Output format: "text", "data.frame", "list"
    #' @return Summary report in requested format
    get_summary = function(format = "list") {
      if (is.null(self$statistics$overall)) {
        self$compute_statistics()
      }

      if (format == "text") {
        return(private$format_text_summary())
      } else if (format == "data.frame") {
        return(private$format_df_summary())
      } else {
        return(self$statistics)
      }
    },

    #' Get results by category
    #'
    #' @param category Category name (NULL for all)
    #' @return Results for specified category
    get_category_results = function(category = NULL) {
      if (is.null(category)) {
        return(self$results$by_category)
      } else {
        return(self$results$by_category[[category]])
      }
    },

    #' Get results by test
    #'
    #' @param test_name Test name (NULL for all)
    #' @return Results for specified test
    get_test_results = function(test_name = NULL) {
      if (is.null(test_name)) {
        return(self$results$by_test)
      } else {
        return(self$results$by_test[[test_name]])
      }
    },

    #' Get failed tests
    #'
    #' @return List of failed tests with details
    get_failed_tests = function() {
      failed <- list()

      for (cat in names(self$results$raw)) {
        for (test in names(self$results$raw[[cat]])) {
          result <- self$results$raw[[cat]][[test]]
          if (!is.null(result$result) && result$result == "FAIL") {
            failed[[paste(cat, test, sep = "_")]] <- list(
              category = cat,
              test = test,
              p_value = result$p_value,
              details = result$details
            )
          }
        }
      }

      return(failed)
    },

    #' Export results
    #'
    #' @param file Path to export file
    #' @param format Export format: "json", "csv", "rds"
    export_results = function(file, format = "json") {
      if (format == "json") {
        jsonlite::write_json(list(
          results = self$results,
          statistics = self$statistics,
          errors = self$errors,
          timestamp = Sys.time()
        ), file, pretty = TRUE)
      } else if (format == "csv") {
        summary_df <- private$format_df_summary()
        write.csv(summary_df, file, row.names = FALSE)
      } else if (format == "rds") {
        saveRDS(list(
          results = self$results,
          statistics = self$statistics,
          errors = self$errors
        ), file)
      } else {
        stop("Unsupported export format")
      }
    }
  ),
  private = list(
    # Initialize category statistics
    init_category_stats = function() {
      list(
        total_tests = 0,
        passed_tests = 0,
        failed_tests = 0,
        error_count = 0,
        p_values = numeric(),
        statistics = numeric(),
        execution_times = numeric()
      )
    },

    # Process individual result
    process_result = function(result, test_name, category) {
      processed <- list(
        test_name = test_name,
        category = category,
        result = result$result,
        p_value = result$p_value,
        statistic = result$statistic,
        timestamp = Sys.time()
      )

      # Extract additional fields if present
      if (!is.null(result$details)) {
        processed$details <- result$details
      }

      if (!is.null(result$execution_time)) {
        processed$execution_time <- result$execution_time
      }

      return(processed)
    },

    # Update category statistics
    update_category_stats = function(category, processed_result) {
      cat_stats <- self$results$by_category[[category]]

      cat_stats$total_tests <- cat_stats$total_tests + 1

      if (!is.null(processed_result$result)) {
        if (processed_result$result == "PASS") {
          cat_stats$passed_tests <- cat_stats$passed_tests + 1
        } else if (processed_result$result == "FAIL") {
          cat_stats$failed_tests <- cat_stats$failed_tests + 1
        }
      }

      if (!is.null(processed_result$p_value) && !is.na(processed_result$p_value)) {
        cat_stats$p_values <- c(cat_stats$p_values, processed_result$p_value)
      }

      if (!is.null(processed_result$statistic) && !is.na(processed_result$statistic)) {
        cat_stats$statistics <- c(cat_stats$statistics, processed_result$statistic)
      }

      if (!is.null(processed_result$execution_time)) {
        cat_stats$execution_times <- c(cat_stats$execution_times, processed_result$execution_time)
      }

      self$results$by_category[[category]] <- cat_stats
    },

    # Update overall statistics
    update_overall_stats = function(processed_result) {
      if (is.null(self$results$overall$total_tests)) {
        self$results$overall <- list(
          total_tests = 0,
          passed_tests = 0,
          failed_tests = 0,
          all_p_values = numeric()
        )
      }

      self$results$overall$total_tests <- self$results$overall$total_tests + 1

      if (!is.null(processed_result$result)) {
        if (processed_result$result == "PASS") {
          self$results$overall$passed_tests <- self$results$overall$passed_tests + 1
        } else if (processed_result$result == "FAIL") {
          self$results$overall$failed_tests <- self$results$overall$failed_tests + 1
        }
      }

      if (!is.null(processed_result$p_value) && !is.na(processed_result$p_value)) {
        self$results$overall$all_p_values <- c(self$results$overall$all_p_values, processed_result$p_value)
      }
    },

    # Format text summary
    format_text_summary = function() {
      lines <- character()

      lines <- c(lines, "PRNG Test Suite Summary")
      lines <- c(lines, paste(rep("=", 50), collapse = ""))

      # Overall statistics
      lines <- c(lines, sprintf("Total Tests: %d", self$statistics$overall$total_tests))
      lines <- c(lines, sprintf(
        "Passed: %d (%.1f%%)",
        self$statistics$overall$passed_tests,
        self$statistics$overall$pass_rate * 100
      ))
      lines <- c(lines, sprintf("Failed: %d", self$statistics$overall$failed_tests))
      lines <- c(lines, sprintf("Errors: %d", self$statistics$overall$error_count))
      lines <- c(lines, "")

      # Category breakdown
      lines <- c(lines, "Results by Category:")
      for (cat in names(self$statistics$by_category)) {
        cat_stats <- self$statistics$by_category[[cat]]
        lines <- c(lines, sprintf(
          "  %s: %.1f%% pass rate (avg p-value: %.3f)",
          cat,
          cat_stats$pass_rate * 100,
          cat_stats$avg_p_value
        ))
      }

      # P-value distribution
      if (!is.null(self$statistics$p_value_distribution)) {
        lines <- c(lines, "")
        lines <- c(lines, "P-value Distribution:")
        lines <- c(lines, sprintf("  Mean: %.3f", self$statistics$p_value_distribution$mean))
        lines <- c(lines, sprintf("  Median: %.3f", self$statistics$p_value_distribution$median))
        lines <- c(lines, sprintf(
          "  Uniformity test p-value: %.3f",
          self$statistics$p_value_distribution$uniformity_test
        ))
      }

      return(paste(lines, collapse = "\n"))
    },

    # Format data frame summary
    format_df_summary = function() {
      # Create category summary
      cat_data <- data.frame(
        category = names(self$results$by_category),
        stringsAsFactors = FALSE
      )

      cat_data$total_tests <- sapply(self$results$by_category, function(x) x$total_tests)
      cat_data$passed_tests <- sapply(self$results$by_category, function(x) x$passed_tests)
      cat_data$failed_tests <- sapply(self$results$by_category, function(x) x$failed_tests)
      cat_data$pass_rate <- sapply(self$statistics$by_category, function(x) x$pass_rate)
      cat_data$avg_p_value <- sapply(self$statistics$by_category, function(x) x$avg_p_value)

      return(cat_data)
    }
  )
)

#' Default aggregator configuration
#' @keywords internal
default_aggregator_config <- list(
  track_execution_time = TRUE,
  compute_incremental_stats = TRUE,
  store_raw_results = TRUE,
  p_value_precision = 4
)

#' Merge aggregator configuration
#' @keywords internal
merge_aggregator_config <- function(config) {
  merged <- default_aggregator_config

  for (name in names(config)) {
    if (name %in% names(merged)) {
      merged[[name]] <- config[[name]]
    }
  }

  return(merged)
}

#' Create result aggregator for test suite
#'
#' @param suite Test suite object
#' @param config Configuration options
#' @export
create_test_result_aggregator <- function(suite, config = NULL) {
  # Merge configurations
  aggregator_config <- config
  if (is.null(aggregator_config)) {
    aggregator_config <- list()
  }

  # Add suite-specific configuration
  if (!is.null(suite$config$track_execution_time)) {
    aggregator_config$track_execution_time <- suite$config$track_execution_time
  }

  # Create aggregator
  aggregator <- ResultAggregator$new(aggregator_config)

  return(aggregator)
}

#' Aggregate results from parallel execution
#'
#' @param results List of results from parallel workers
#' @param suite Test suite object
#' @export
aggregate_parallel_results <- function(results, suite = NULL) {
  aggregator <- ResultAggregator$new()

  # Process each worker's results
  for (worker_result in results) {
    if (is.list(worker_result) && !is.null(worker_result$category)) {
      # Single category result
      category <- worker_result$category
      if (!is.null(worker_result$results)) {
        for (test_name in names(worker_result$results)) {
          aggregator$add_result(category, test_name, worker_result$results[[test_name]])
        }
      }
    } else if (is.list(worker_result)) {
      # Multiple categories from one worker
      for (category in names(worker_result)) {
        if (is.list(worker_result[[category]])) {
          for (test_name in names(worker_result[[category]])) {
            aggregator$add_result(category, test_name, worker_result[[category]][[test_name]])
          }
        }
      }
    }
  }

  # Compute final statistics
  aggregator$compute_statistics()

  return(aggregator)
}
