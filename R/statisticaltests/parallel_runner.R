# File: parallel_runner.R
# ----------------------------------------------------------------------
#' Parallel test runner for PRNG test suite
#'
#' This module provides parallel execution capabilities for the PRNG test suite,
#' with cross-platform support and intelligent test scheduling.

#' Run PRNG test suite in parallel
#'
#' Executes test categories in parallel using platform-appropriate methods.
#' Uses mclapply on Unix-like systems and parLapply on Windows.
#'
#' @param suite The test suite object created by create_prng_test_suite
#' @param save_report Whether to save the test report
#' @return The test suite object with results
#' @export
run_prng_test_suite_parallel <- function(suite, save_report = TRUE) {
  if (suite$config$verbose) {
    cat("Running PRNG test suite in parallel...\n")
  }

  # Check if parallel execution is enabled
  if (!suite$config$parallel || length(suite$categories) < 2) {
    if (suite$config$verbose) {
      cat("  Falling back to sequential execution\n")
    }
    return(run_prng_test_suite(suite, save_report))
  }

  # Detect platform and available cores
  platform <- .Platform$OS.type
  num_cores <- min(suite$config$cores, parallel::detectCores() - 1, length(suite$categories))

  if (suite$config$verbose) {
    cat("  Platform:", platform, "\n")
    cat("  Using", num_cores, "cores\n")
  }

  # Prepare test execution information
  test_info <- prepare_test_execution(suite)

  # Execute tests in parallel
  if (platform == "unix") {
    # Unix/Linux/macOS - use mclapply
    results <- run_tests_unix(suite, test_info, num_cores)
  } else {
    # Windows - use parLapply
    results <- run_tests_windows(suite, test_info, num_cores)
  }

  # Merge results back into suite
  suite <- merge_parallel_results(suite, results)

  # Generate summary
  suite <- summarize_test_results(suite)

  # Save report if requested
  if (save_report) {
    save_test_report(suite)
  }

  return(suite)
}

#' Prepare test execution information
#'
#' Creates a list of test execution information including dependencies
#' and computational costs for scheduling.
#'
#' @param suite The test suite object
#' @return A list with test execution information
#' @keywords internal
prepare_test_execution <- function(suite) {
  # Define test dependencies (most are independent)
  dependencies <- list(
    basic = character(0),
    runs = character(0),
    correlation = character(0),
    binary = character(0),
    classical = character(0),
    compression = character(0),
    external = c("basic") # External tests might benefit from basic test results
  )

  # Estimate computational cost (relative units)
  # Based on sample sizes and algorithm complexity
  costs <- list(
    basic = suite$config$basic_sample_size / 1e5,
    runs = suite$config$runs_sample_size / 1e5,
    correlation = suite$config$correlation_sample_size / 1e5 * 2, # More expensive
    binary = suite$config$binary_sample_size / 1e5 * 3, # Bit operations are expensive
    classical = suite$config$classical_sample_size / 1e5,
    compression = suite$config$compression_sample_size / 1e5 * 2,
    external = suite$config$external_sample_size / 1e5 * 5 # Very expensive
  )

  # Filter to selected categories
  test_info <- list()
  for (cat in suite$categories) {
    test_info[[cat]] <- list(
      category = cat,
      dependencies = dependencies[[cat]],
      cost = costs[[cat]],
      ready = length(dependencies[[cat]]) == 0
    )
  }

  return(test_info)
}

#' Run tests on Unix-like systems using mclapply
#'
#' @param suite The test suite object
#' @param test_info Test execution information
#' @param num_cores Number of cores to use
#' @return List of test results
#' @keywords internal
run_tests_unix <- function(suite, test_info, num_cores) {
  # Schedule tests based on dependencies and cost
  scheduled <- schedule_tests(test_info)

  # Function to run a single test category
  run_category <- function(category) {
    if (suite$config$verbose) {
      cat("  Running", QPRNG_TEST_CATEGORIES[[category]], "on process", Sys.getpid(), "\n")
    }

    # Create a copy of the suite for this category
    cat_suite <- suite
    cat_suite$categories <- category

    # Run the test
    result <- switch(category,
      "basic" = run_basic_tests(cat_suite),
      "runs" = run_runs_tests(cat_suite),
      "correlation" = run_correlation_tests(cat_suite),
      "binary" = run_binary_tests(cat_suite),
      "classical" = run_classical_tests(cat_suite),
      "compression" = run_compression_tests(cat_suite),
      "external" = run_external_tests(cat_suite),
      cat_suite
    )

    return(list(category = category, results = result$results[[category]]))
  }

  # Run tests in parallel batches
  results <- list()

  for (batch in scheduled) {
    if (length(batch) > 0) {
      batch_results <- parallel::mclapply(
        batch,
        run_category,
        mc.cores = min(num_cores, length(batch)),
        mc.preschedule = FALSE # Dynamic scheduling
      )

      # Check for errors
      for (res in batch_results) {
        if (inherits(res, "try-error")) {
          warning("Test category failed: ", attr(res, "condition")$message)
        } else {
          results[[res$category]] <- res$results
        }
      }
    }
  }

  return(results)
}

#' Run tests on Windows using parLapply
#'
#' @param suite The test suite object
#' @param test_info Test execution information
#' @param num_cores Number of cores to use
#' @return List of test results
#' @keywords internal
run_tests_windows <- function(suite, test_info, num_cores) {
  # Create cluster
  cl <- parallel::makeCluster(num_cores)
  on.exit(parallel::stopCluster(cl))

  # Export necessary objects and functions to cluster
  parallel::clusterExport(cl, c(
    "QPRNG_TEST_CATEGORIES",
    "run_basic_tests", "run_runs_tests", "run_correlation_tests",
    "run_binary_tests", "run_classical_tests", "run_compression_tests",
    "run_external_tests"
  ), envir = environment())

  # Load required packages on workers
  parallel::clusterEvalQ(cl, {
    library(qiprng)
    # Load any other required packages
  })

  # Schedule tests
  scheduled <- schedule_tests(test_info)

  # Function to run a single test category
  run_category <- function(category, suite_data) {
    # Reconstruct suite on worker
    cat_suite <- suite_data
    cat_suite$categories <- category

    # Run the test
    result <- switch(category,
      "basic" = run_basic_tests(cat_suite),
      "runs" = run_runs_tests(cat_suite),
      "correlation" = run_correlation_tests(cat_suite),
      "binary" = run_binary_tests(cat_suite),
      "classical" = run_classical_tests(cat_suite),
      "compression" = run_compression_tests(cat_suite),
      "external" = run_external_tests(cat_suite),
      cat_suite
    )

    return(list(category = category, results = result$results[[category]]))
  }

  # Run tests in parallel batches
  results <- list()

  for (batch in scheduled) {
    if (length(batch) > 0) {
      batch_results <- parallel::parLapply(
        cl,
        batch,
        run_category,
        suite_data = suite
      )

      # Collect results
      for (res in batch_results) {
        if (!inherits(res, "try-error")) {
          results[[res$category]] <- res$results
        }
      }
    }
  }

  return(results)
}

#' Schedule tests based on dependencies and computational cost
#'
#' @param test_info Test execution information
#' @return List of test batches that can run in parallel
#' @keywords internal
schedule_tests <- function(test_info) {
  scheduled <- list()
  remaining <- names(test_info)
  completed <- character(0)

  while (length(remaining) > 0) {
    # Find tests that are ready to run
    ready <- character(0)
    for (test in remaining) {
      deps <- test_info[[test]]$dependencies
      if (all(deps %in% completed)) {
        ready <- c(ready, test)
      }
    }

    if (length(ready) == 0) {
      warning("Circular dependencies detected in test scheduling")
      break
    }

    # Sort by cost (descending) for better load balancing
    costs <- sapply(ready, function(t) test_info[[t]]$cost)
    ready <- ready[order(costs, decreasing = TRUE)]

    # Add to schedule
    scheduled <- append(scheduled, list(ready))

    # Mark as completed
    completed <- c(completed, ready)
    remaining <- setdiff(remaining, ready)
  }

  return(scheduled)
}

#' Merge parallel test results back into suite
#'
#' @param suite The original test suite
#' @param results Results from parallel execution
#' @return Updated test suite
#' @keywords internal
merge_parallel_results <- function(suite, results) {
  # Clear existing results
  suite$results <- list()

  # Merge results
  for (category in names(results)) {
    suite$results[[category]] <- results[[category]]
  }

  return(suite)
}

#' Detect optimal number of cores for parallel execution
#'
#' @param requested_cores Number of cores requested by user
#' @param num_tests Number of tests to run
#' @return Optimal number of cores to use
#' @export
detect_optimal_cores <- function(requested_cores = NULL, num_tests = 7) {
  # Get system information
  total_cores <- parallel::detectCores()

  # Default to leaving one core free
  if (is.null(requested_cores)) {
    requested_cores <- total_cores - 1
  }

  # Ensure we don't use more cores than tests
  optimal_cores <- min(requested_cores, num_tests)

  # Ensure at least 1 core
  optimal_cores <- max(1, optimal_cores)

  # On small systems, be conservative
  if (total_cores <= 2) {
    optimal_cores <- 1
  }

  return(optimal_cores)
}
