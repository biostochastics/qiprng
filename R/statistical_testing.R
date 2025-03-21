# File: statistical_testing.R
# ----------------------------------------------------------------------
#' Statistical Testing Framework for QPRNG
#'
#' A comprehensive framework for statistically testing the quality of 
#' random number generators. This module provides various test suites,
#' visualizations, and reporting capabilities to assess PRNG performance.
#'
#' @import ggplot2
#' @import gridExtra
#' @import randtests
#' @import nortest
#' @import parallel
#' @importFrom stats ks.test chisq.test acf pacf spectrum shapiro.test sd cor
#' @importFrom utils write.table installed.packages
#' @importFrom grDevices png dev.off
NULL

#' List of available test categories
QPRNG_TEST_CATEGORIES <- list(
  basic = "Basic Distribution Tests",
  runs = "Runs and Independence Tests",
  correlation = "Correlation Tests",
  binary = "Binary and Bitwise Tests",
  classical = "Classical PRNG Tests",
  compression = "Compression Tests",
  external = "External Test Batteries"
)

#' Default test configuration
#'
#' @export
default_test_config <- list(
  # Test sample sizes
  basic_sample_size = 1e5,
  runs_sample_size = 1e5, 
  correlation_sample_size = 1e5,
  binary_sample_size = 1e6,
  classical_sample_size = 1e5,
  compression_sample_size = 1e6,
  external_sample_size = 1e7,
  
  # Test parameters
  chi_squared_bins = 100,
  significance_level = 0.05,
  
  # Visual parameters
  plot_width = 10,
  plot_height = 8, 
  plot_dpi = 120,
  
  # Reporting options
  save_results = TRUE,
  save_visualizations = TRUE,
  verbose = TRUE,
  output_dir = "prng_test_results",
  
  # External test options
  use_dieharder = TRUE,
  use_dieharder_quick = TRUE,  # If FALSE, runs all tests (-a)
  use_ent = TRUE,
  
  # Processing options
  parallel = TRUE,
  cores = parallel::detectCores() - 1
)

#' Create a PRNG test suite
#'
#' Creates a test suite for evaluating the quality of a PRNG.
#'
#' @param prng_func A function that generates n random numbers
#' @param config Test configuration parameters
#' @param categories Test categories to include (default: all)
#' @return A test suite object
#' @export
create_prng_test_suite <- function(prng_func, 
                                    config = default_test_config,
                                    categories = names(QPRNG_TEST_CATEGORIES)) {
  # Merge with defaults
  if (!identical(config, default_test_config)) {
    config <- modifyList(default_test_config, config)
  }
  
  # Validate PRNG function
  if (!is.function(prng_func)) {
    stop("prng_func must be a function that takes n as an argument")
  }
  
  # Test a small sample to make sure it works
  test_sample <- try(prng_func(10), silent = TRUE)
  if (inherits(test_sample, "try-error") || !is.numeric(test_sample) || length(test_sample) != 10) {
    stop("prng_func must return a numeric vector of length n")
  }
  
  # Validate categories
  valid_categories <- names(QPRNG_TEST_CATEGORIES)
  invalid_categories <- setdiff(categories, valid_categories)
  if (length(invalid_categories) > 0) {
    stop("Invalid categories: ", paste(invalid_categories, collapse = ", "), 
         ". Valid categories: ", paste(valid_categories, collapse = ", "))
  }
  
  # Create output directory if needed
  if (config$save_results || config$save_visualizations) {
    dir.create(config$output_dir, showWarnings = FALSE, recursive = TRUE)
  }
  
  # Create the test suite
  suite <- list(
    prng_func = prng_func,
    config = config,
    categories = categories,
    tests = list(),
    results = list(),
    visualization = list(),
    summary = NULL
  )
  
  return(suite)
}

#' Run the PRNG test suite
#'
#' @param suite A test suite created by create_prng_test_suite
#' @param save_report Whether to save a comprehensive report
#' @return The updated test suite with results
#' @export
run_prng_test_suite <- function(suite, save_report = TRUE) {
  if (suite$config$verbose) {
    cat("Running PRNG test suite...\n")
  }
  
  # Run tests for each selected category
  for (category in suite$categories) {
    if (suite$config$verbose) {
      cat("  Running", QPRNG_TEST_CATEGORIES[[category]], "...\n")
    }
    
    # Call the appropriate function for each category
    suite <- switch(category,
      "basic" = run_basic_tests(suite),
      "runs" = run_runs_tests(suite),
      "correlation" = run_correlation_tests(suite),
      "binary" = run_binary_tests(suite),
      "classical" = run_classical_tests(suite),
      "compression" = run_compression_tests(suite),
      "external" = run_external_tests(suite),
      suite  # Default: return unchanged
    )
  }
  
  # Generate summary
  suite <- summarize_test_results(suite)
  
  # Save report if requested
  if (save_report) {
    save_test_report(suite)
  }
  
  return(suite)
}

#' Generate a summary of test results
#'
#' @param suite The test suite with results
#' @return The test suite with summary added
#' @keywords internal
summarize_test_results <- function(suite) {
  # Initialize summary statistics
  total_tests <- 0
  passed_tests <- 0
  failed_tests <- 0
  inconclusive_tests <- 0
  
  # Summary by category
  category_summary <- list()
  
  # Process each category
  for (category in suite$categories) {
    if (!is.null(suite$results[[category]])) {
      cat_total <- 0
      cat_passed <- 0
      cat_failed <- 0
      cat_inconclusive <- 0
      
      # Process each test in category
      for (test_name in names(suite$results[[category]])) {
        result <- suite$results[[category]][[test_name]]
        
        # Skip if not a test result or no result field
        if (!is.list(result) || is.null(result$result)) {
          next
        }
        
        total_tests <- total_tests + 1
        cat_total <- cat_total + 1
        
        if (result$result == "PASS") {
          passed_tests <- passed_tests + 1
          cat_passed <- cat_passed + 1
        } else if (result$result == "FAIL") {
          failed_tests <- failed_tests + 1
          cat_failed <- cat_failed + 1
        } else {
          inconclusive_tests <- inconclusive_tests + 1
          cat_inconclusive <- cat_inconclusive + 1
        }
      }
      
      # Store category summary
      category_summary[[category]] <- list(
        total = cat_total,
        passed = cat_passed,
        failed = cat_failed,
        inconclusive = cat_inconclusive,
        pass_rate = if (cat_total > 0) cat_passed / cat_total else 0
      )
    }
  }
  
  # Calculate overall pass rate
  pass_rate <- if (total_tests > 0) passed_tests / total_tests else 0
  
  # Create summary object
  suite$summary <- list(
    total_tests = total_tests,
    passed_tests = passed_tests,
    failed_tests = failed_tests,
    inconclusive_tests = inconclusive_tests,
    pass_rate = pass_rate,
    by_category = category_summary
  )
  
  return(suite)
}

#' Save a comprehensive test report
#'
#' @param suite The test suite with results
#' @param filename Optional filename for the report
#' @return The path to the saved report (invisibly)
#' @export
save_test_report <- function(suite, filename = NULL) {
  if (is.null(filename)) {
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    filename <- file.path(suite$config$output_dir, paste0("prng_test_report_", timestamp, ".html"))
  }
  
  # Create HTML report
  report <- c(
    "<!DOCTYPE html>",
    "<html>",
    "<head>",
    "  <title>PRNG Statistical Test Report</title>",
    "  <style>",
    "    body { font-family: Arial, sans-serif; margin: 20px; }",
    "    h1 { color: #2c3e50; }",
    "    h2 { color: #3498db; margin-top: 30px; }",
    "    h3 { color: #16a085; margin-top: 20px; }",
    "    .summary { background-color: #f8f9fa; padding: 15px; border-radius: 5px; }",
    "    .pass { color: #27ae60; }",
    "    .fail { color: #e74c3c; }",
    "    .inconclusive { color: #f39c12; }",
    "    table { border-collapse: collapse; width: 100%; margin-top: 10px; }",
    "    th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }",
    "    th { background-color: #f2f2f2; }",
    "    tr:nth-child(even) { background-color: #f9f9f9; }",
    "    .visualization { margin: 20px 0; }",
    "  </style>",
    "</head>",
    "<body>",
    "  <h1>PRNG Statistical Test Report</h1>",
    paste0("  <p>Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "</p>")
  )
  
  # Add overall summary
  if (!is.null(suite$summary)) {
    report <- c(report,
      "  <h2>Overall Summary</h2>",
      "  <div class='summary'>",
      paste0("    <p>Total tests run: ", suite$summary$total_tests, "</p>"),
      paste0("    <p>Tests passed: <span class='pass'>", suite$summary$passed_tests, " (", 
             format(suite$summary$pass_rate * 100, digits=2), "%)</span></p>"),
      paste0("    <p>Tests failed: <span class='fail'>", suite$summary$failed_tests, "</span></p>"),
      paste0("    <p>Tests inconclusive: <span class='inconclusive'>", suite$summary$inconclusive_tests, "</span></p>"),
      "  </div>"
    )
    
    # Add category summaries
    report <- c(report, "  <h2>Category Summaries</h2>")
    report <- c(report, "  <table>",
                "    <tr><th>Category</th><th>Total</th><th>Passed</th><th>Failed</th><th>Inconclusive</th><th>Pass Rate</th></tr>")
    
    for (category in names(suite$summary$by_category)) {
      cat_summary <- suite$summary$by_category[[category]]
      report <- c(report, paste0(
        "    <tr>",
        "<td>", QPRNG_TEST_CATEGORIES[[category]], "</td>",
        "<td>", cat_summary$total, "</td>",
        "<td>", cat_summary$passed, "</td>",
        "<td>", cat_summary$failed, "</td>",
        "<td>", cat_summary$inconclusive, "</td>",
        "<td>", format(cat_summary$pass_rate * 100, digits=2), "%</td>",
        "</tr>"
      ))
    }
    report <- c(report, "  </table>")
  }
  
  # Add detailed results for each category
  for (category in suite$categories) {
    if (!is.null(suite$results[[category]])) {
      report <- c(report, paste0("  <h2>", QPRNG_TEST_CATEGORIES[[category]], "</h2>"))
      
      # Add table for test results
      report <- c(report, "  <table>",
                  "    <tr><th>Test</th><th>Result</th><th>Description</th><th>Details</th></tr>")
      
      for (test_name in names(suite$results[[category]])) {
        result <- suite$results[[category]][[test_name]]
        
        # Skip if not a valid test result
        if (!is.list(result) || is.null(result$result)) {
          next
        }
        
        # Determine result class
        result_class <- switch(result$result,
          "PASS" = "pass",
          "FAIL" = "fail",
          "inconclusive"
        )
        
        # Format details as needed
        details <- ""
        if (!is.null(result$p_value)) {
          details <- paste0("p-value: ", format(result$p_value, digits=4))
        }
        if (!is.null(result$statistic)) {
          if (details != "") details <- paste0(details, ", ")
          details <- paste0(details, "statistic: ", format(result$statistic, digits=4))
        }
        if (!is.null(result$details)) {
          if (details != "") details <- paste0(details, ", ")
          details <- paste0(details, result$details)
        }
        
        report <- c(report, paste0(
          "    <tr>",
          "<td>", test_name, "</td>",
          "<td class='", result_class, "'>", result$result, "</td>",
          "<td>", result$description, "</td>",
          "<td>", details, "</td>",
          "</tr>"
        ))
      }
      report <- c(report, "  </table>")
      
      # Add visualizations if available
      if (!is.null(suite$visualization[[category]])) {
        report <- c(report, "  <h3>Visualizations</h3>")
        for (viz_name in names(suite$visualization[[category]])) {
          if (suite$config$save_visualizations) {
            viz_path <- suite$visualization[[category]][[viz_name]]
            # Use relative path in the HTML
            rel_path <- basename(viz_path)
            report <- c(report, paste0(
              "  <div class='visualization'>",
              "    <h4>", viz_name, "</h4>",
              "    <img src='", rel_path, "' alt='", viz_name, "' style='max-width: 100%;'>",
              "  </div>"
            ))
          }
        }
      }
    }
  }
  
  # Close HTML
  report <- c(report, "</body>", "</html>")
  
  # Write to file
  writeLines(report, filename)
  
  if (suite$config$verbose) {
    cat("Test report saved to:", filename, "\n")
  }
  
  return(invisible(filename))
}

# Include test modules
source("R/statisticaltests/basic_tests.R")
source("R/statisticaltests/runs_tests.R")
source("R/statisticaltests/correlation_tests.R")
source("R/statisticaltests/binary_tests.R")
source("R/statisticaltests/classical_tests.R")
source("R/statisticaltests/compression_tests.R")
source("R/statisticaltests/external_tests.R")

# Include visualization modules
source("R/statisticaltests/visualization_basic.R")
source("R/statisticaltests/visualization_runs.R")
source("R/statisticaltests/visualization_correlation.R")
source("R/statisticaltests/visualization_binary.R")
source("R/statisticaltests/visualization_classical.R")
source("R/statisticaltests/visualization.R")

#' Run statistical tests on the current PRNG configuration
#'
#' @param sample_size Number of random numbers to generate for testing
#' @param config Test configuration (see default_test_config)
#' @param categories Test categories to include
#' @param report_file Optional filename for the test report
#' @return A test suite object with results
#' @export
test_prng <- function(sample_size = 1e6, 
                       config = default_test_config,
                       categories = names(QPRNG_TEST_CATEGORIES),
                       report_file = NULL) {
  
  # Create config with updated sample sizes based on the provided sample_size
  test_config <- config
  test_config$basic_sample_size <- min(sample_size, 1e5)
  test_config$runs_sample_size <- min(sample_size, 1e5)
  test_config$correlation_sample_size <- min(sample_size, 1e5)
  test_config$binary_sample_size <- min(sample_size, 1e6)
  test_config$classical_sample_size <- min(sample_size, 1e5)
  test_config$compression_sample_size <- min(sample_size, 1e6)
  test_config$external_sample_size <- min(sample_size, 1e7)
  
  # Create wrapper function for generatePRNG
  prng_func <- function(n) {
    generatePRNG(n)
  }
  
  # Create and run test suite
  suite <- create_prng_test_suite(prng_func, test_config, categories)
  suite <- run_prng_test_suite(suite, save_report = !is.null(report_file))
  
  # Save report with custom filename if provided
  if (!is.null(report_file)) {
    save_test_report(suite, report_file)
  }
  
  return(suite)
}
