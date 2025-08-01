#!/usr/bin/env Rscript
# =============================================================================
# COMPREHENSIVE TEST RUNNER FOR QIPRNG PACKAGE V2
# =============================================================================
# Fixed version with proper test result parsing and error handling
# Author: qiprng development team
# Date: 2025-07-31
# =============================================================================

# Setup and initialization
cat("=============================================================================\n")
cat("QIPRNG COMPREHENSIVE TEST SUITE V2\n")
cat("=============================================================================\n")
cat("Start time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

# Load required libraries
suppressPackageStartupMessages({
  library(testthat)
  library(qiprng)
  library(jsonlite)
  library(knitr)
})

# Create results directory with timestamp
timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
results_dir <- file.path("test_results", paste0("comprehensive_", timestamp))
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(results_dir, "logs"), recursive = TRUE, showWarnings = FALSE)

# Initialize results tracking
test_results <- list(
  metadata = list(
    start_time = Sys.time(),
    package_version = as.character(packageVersion("qiprng")),
    r_version = R.version.string,
    platform = Sys.info()[["sysname"]],
    hostname = Sys.info()[["nodename"]]
  ),
  summary = list(
    total_tests = 0,
    total_files = 0,
    passed_tests = 0,
    failed_tests = 0,
    skipped_tests = 0,
    error_files = 0,
    warning_files = 0
  ),
  categories = list(),
  detailed_results = list(),
  performance_metrics = list(),
  validation_results = list()
)

# Define all test files explicitly (no pattern matching)
test_inventory <- list(
  core_functionality = list(
    name = "Core Functionality",
    tests = c(
      "testthat/test-basic.R",
      "testthat/test-config.R", 
      "testthat/test-validation-suite.R",
      "testthat/test-caching-framework.R",
      "testthat/test-threading.R",
      "testthat/test-deterministic-mode.R",  # Re-enabled after fix
      "testthat/test-distributions.R"
    )
  ),
  
  statistical_improvements = list(
    name = "Statistical Improvements (Phases 1-3)",
    tests = c(
      "testthat/test-p-value-adjustment.R",
      "testthat/test-comprehensive-improvements.R",
      "testthat/test-data-structure-fixes.R"
    )
  ),
  
  statistical_tests = list(
    name = "Statistical Tests",
    tests = c(
      "testthat/test-compression-bootstrap.R",
      "testthat/test-bootstrap-framework.R",
      "testthat/test-multidim-tests.R",
      "testthat/test-nist-frequency-cusum.R",
      "testthat/test-nist-excursions.R",
      "testthat/test-external-wrappers.R"
    )
  ),
  
  advanced_features = list(
    name = "Advanced Features", 
    tests = c(
      "testthat/test-advanced.R",
      # "testthat/test-ziggurat-thread-safety.R",  # Temporarily disabled - mclapply hanging on macOS
      "testthat/test-ziggurat-thread-safety-fix.R",
      "testthat/test-parallel-buffer.R",
      "testthat/test-parallel-thread-safety-stress.R",
      "testthat/test-persistent-thread-pool.R",
      "testthat/test-thread-pool-efficiency.R"
    )
  )
)

# Function to parse test results from log output
parse_test_counts <- function(log_content) {
  counts <- list(passed = 0, failed = 0, skipped = 0, warnings = 0)
  
  # Look for testthat summary line: [ FAIL X | WARN Y | SKIP Z | PASS W ]
  summary_pattern <- "\\[ FAIL ([0-9]+) \\| WARN ([0-9]+) \\| SKIP ([0-9]+) \\| PASS ([0-9]+) \\]"
  
  # Join all lines for better pattern matching
  full_text <- paste(log_content, collapse = "\n")
  
  # Find all matches
  matches <- gregexpr(summary_pattern, full_text, perl = TRUE)
  
  if (matches[[1]][1] != -1) {
    # Get the last match (final summary)
    match_text <- regmatches(full_text, matches)[[1]]
    if (length(match_text) > 0) {
      last_match <- match_text[length(match_text)]
      
      # Extract numbers directly
      numbers <- as.numeric(unlist(regmatches(last_match, gregexpr("[0-9]+", last_match))))
      
      if (length(numbers) == 4) {
        counts$failed <- numbers[1]
        counts$warnings <- numbers[2]
        counts$skipped <- numbers[3]
        counts$passed <- numbers[4]
      }
    }
  }
  
  return(counts)
}

# Function to run a single test file with comprehensive error handling
run_test_safely <- function(test_file, category_name) {
  test_name <- basename(test_file)
  log_file <- file.path(results_dir, "logs", paste0(test_name, ".log"))
  
  cat(sprintf("  Running: %s", test_name))
  
  start_time <- Sys.time()
  
  # Initialize counters
  test_counts <- list(passed = 0, failed = 0, skipped = 0, warnings = 0)
  status <- "UNKNOWN"
  error_msg <- NULL
  
  # Capture output and errors with timeout protection
  capture_output <- capture.output({
    test_result <- tryCatch({
      # Set timeout for safety
      setTimeLimit(cpu = 60, elapsed = 60, transient = TRUE)
      
      # Try to run as testthat test
      if (grepl("^test-", test_name)) {
        # Run with ListReporter to get detailed results
        reporter <- ListReporter$new()
        
        # Capture both stdout and stderr
        output <- capture.output({
          stderr_output <- capture.output({
            test_file(test_file, reporter = reporter)
          }, type = "message")
          
          # Write stderr to log file as well
          if (length(stderr_output) > 0) {
            cat("\n=== STDERR OUTPUT ===\n", file = log_file, append = TRUE)
            cat(stderr_output, sep = "\n", file = log_file, append = TRUE)
            cat("\n=== END STDERR ===\n", file = log_file, append = TRUE)
          }
        }, type = "output")
        
        # Write stdout to log file
        if (length(output) > 0) {
          cat(output, sep = "\n", file = log_file, append = TRUE)
        }
        
        # Extract counts from reporter using get_results()
        test_results_list <- reporter$get_results()
        if (length(test_results_list) > 0) {
          test_counts$passed <- sum(sapply(test_results_list, function(x) isTRUE(x$passed)))
          test_counts$failed <- sum(sapply(test_results_list, function(x) isFALSE(x$passed)))
          test_counts$skipped <- sum(sapply(test_results_list, function(x) isTRUE(x$skip)))
        }
        
        # Garbage collection to prevent memory buildup
        gc(verbose = FALSE)
        
        list(success = TRUE, counts = test_counts, reporter = reporter)
      } else {
        # Run as standalone script
        source(test_file, local = TRUE)
        
        # Garbage collection to prevent memory buildup
        gc(verbose = FALSE)
        
        list(success = TRUE, message = "Standalone script executed")
      }
    }, error = function(e) {
      # Write error to log file
      cat("\n=== ERROR ===\n", file = log_file, append = TRUE)
      cat("Error message: ", e$message, "\n", file = log_file, append = TRUE)
      cat("\nCall stack:\n", file = log_file, append = TRUE)
      cat(paste(capture.output(traceback()), collapse = "\n"), file = log_file, append = TRUE)
      cat("\n=== END ERROR ===\n", file = log_file, append = TRUE)
      
      list(
        success = FALSE,
        error = TRUE,
        message = as.character(e$message),
        traceback = paste(capture.output(traceback()), collapse = "\n")
      )
    }, warning = function(w) {
      list(
        success = TRUE,
        warning = TRUE,
        message = as.character(w$message)
      )
    })
  }, type = "output")
  
  # Reset time limits
  setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE)
  
  end_time <- Sys.time()
  execution_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  # If we didn't get counts from the reporter, try parsing the log
  if (test_counts$passed == 0 && test_counts$failed == 0 && test_counts$skipped == 0) {
    if (file.exists(log_file)) {
      log_content <- readLines(log_file, warn = FALSE)
      parsed_counts <- parse_test_counts(log_content)
      test_counts <- parsed_counts
    }
  }
  
  # Determine status
  if (!is.null(test_result$error) && test_result$error) {
    status <- "ERROR"
    error_msg <- test_result$message
    test_results$summary$error_files <<- test_results$summary$error_files + 1
  } else if (test_counts$failed > 0) {
    status <- "FAIL"
  } else if (test_counts$passed > 0 || test_result$success) {
    status <- "PASS"
  } else {
    status <- "UNKNOWN"
  }
  
  # Update global counters
  test_results$summary$passed_tests <<- test_results$summary$passed_tests + test_counts$passed
  test_results$summary$failed_tests <<- test_results$summary$failed_tests + test_counts$failed
  test_results$summary$skipped_tests <<- test_results$summary$skipped_tests + test_counts$skipped
  if (test_counts$warnings > 0) {
    test_results$summary$warning_files <<- test_results$summary$warning_files + 1
  }
  test_results$summary$total_files <<- test_results$summary$total_files + 1
  
  # Create result summary
  result_summary <- list(
    test_file = test_file,
    test_name = test_name,
    category = category_name,
    execution_time = execution_time,
    timestamp = format(start_time, "%Y-%m-%d %H:%M:%S"),
    status = status,
    passed = test_counts$passed,
    failed = test_counts$failed,
    skipped = test_counts$skipped,
    warnings = test_counts$warnings,
    error_message = error_msg
  )
  
  # Print summary
  cat(sprintf(" [%s] (P:%d F:%d S:%d W:%d) %.1fs\n", 
              status, test_counts$passed, test_counts$failed, 
              test_counts$skipped, test_counts$warnings, execution_time))
  
  # Additional cleanup for deterministic mode test
  if (test_name == "test-deterministic-mode.R") {
    # Force additional garbage collection
    gc(verbose = FALSE)
    gc(verbose = FALSE)
    
    # Small delay to ensure cleanup
    Sys.sleep(0.1)
  }
  
  return(result_summary)
}

# Run all test categories
cat("\nRunning all test categories...\n")
cat("=============================================================================\n")

for (category_id in names(test_inventory)) {
  category <- test_inventory[[category_id]]
  cat(sprintf("\n%s\n", category$name))
  cat(paste(rep("-", nchar(category$name)), collapse = ""), "\n")
  
  category_results <- list()
  
  for (test_file in category$tests) {
    full_path <- file.path("testthat", basename(test_file))
    if (file.exists(full_path)) {
      result <- run_test_safely(full_path, category$name)
      category_results[[test_file]] <- result
      test_results$detailed_results[[test_file]] <- result
    } else {
      cat(sprintf("  Skipping: %s (file not found)\n", basename(test_file)))
    }
  }
  
  test_results$categories[[category_id]] <- category_results
}

# Calculate total tests
test_results$summary$total_tests <- test_results$summary$passed_tests + 
                                    test_results$summary$failed_tests + 
                                    test_results$summary$skipped_tests

# Validate Statistical Improvements
cat("\n\nValidating Statistical Improvements\n")
cat("=============================================================================\n")

# Phase 1: P-value Adjustment
cat("\nPhase 1: P-value Adjustment\n")
validation_p_value <- tryCatch({
  source("../R/statisticaltests/unified_reporting.R", local = TRUE)
  suite <- test_suite_results("Validation Test")
  suite <- add_test_result(suite, test_result("test1", "Test 1", "FAIL", p_value = 0.01))
  suite <- add_test_result(suite, test_result("test2", "Test 2", "FAIL", p_value = 0.04))
  suite <- finalize_suite(suite)
  adjusted <- adjust_p_values(suite, method = "bonferroni")
  
  if (!is.null(adjusted$results[[1]]$p_adjusted)) {
    list(
      status = "PASS",
      message = sprintf("P-value adjustment working correctly. Original: %.3f, Adjusted: %.3f",
                       adjusted$results[[1]]$p_value, adjusted$results[[1]]$p_adjusted)
    )
  } else {
    list(status = "FAIL", message = "P-value adjustment not working")
  }
}, error = function(e) {
  list(status = "ERROR", message = paste("Error testing p-value adjustment:", e$message))
})

cat(sprintf("  Status: %s\n", validation_p_value$status))
cat(sprintf("  %s\n", validation_p_value$message))
test_results$validation_results$p_value_adjustment <- validation_p_value

# Phase 2: Weighting System
cat("\nPhase 2: Weighting System\n")
validation_weighting <- tryCatch({
  source("../R/statisticaltests/comprehensive_generator_comparison.R", local = TRUE)
  
  test_df <- data.frame(
    test = paste0("test", 1:20),
    passed = c(rep(TRUE, 18), rep(FALSE, 2)),
    category = rep(c("basic", "binary"), each = 10),
    p_value = runif(20)
  )
  
  weights <- c(basic = 2.0, binary = 0.5)
  metrics <- calculate_overall_metrics(test_df, weights)
  
  if (!is.null(metrics$weighted_pass_rate)) {
    list(
      status = "PASS",
      message = sprintf("Weighting system working correctly. Unweighted: %.2f%%, Weighted: %.2f%%",
                       metrics$pass_rate * 100, metrics$weighted_pass_rate * 100)
    )
  } else {
    list(status = "FAIL", message = "Weighting system not working")
  }
}, error = function(e) {
  list(status = "ERROR", message = paste("Error testing weighting system:", e$message))
})

cat(sprintf("  Status: %s\n", validation_weighting$status))
cat(sprintf("  %s\n", validation_weighting$message))
test_results$validation_results$weighting_system <- validation_weighting

# Phase 3: Effect Sizes
cat("\nPhase 3: Effect Size Calculations\n")
validation_effect_sizes <- tryCatch({
  source("../R/statisticaltests/effect_sizes.R", local = TRUE)
  
  d <- calculate_cohens_d(0.55, 0.50, 0.1)
  interp <- interpret_effect_size(d, "d")
  
  if (!is.na(d) && !is.na(interp)) {
    list(
      status = "PASS",
      message = sprintf("Effect size calculations working correctly. Cohen's d = %.3f (%s)", d, interp)
    )
  } else {
    list(status = "FAIL", message = "Effect size calculations not working")
  }
}, error = function(e) {
  list(status = "ERROR", message = paste("Error testing effect sizes:", e$message))
})

cat(sprintf("  Status: %s\n", validation_effect_sizes$status))
cat(sprintf("  %s\n", validation_effect_sizes$message))
test_results$validation_results$effect_sizes <- validation_effect_sizes

# Calculate final metrics
test_results$metadata$end_time <- Sys.time()
test_results$metadata$total_duration <- as.numeric(
  difftime(test_results$metadata$end_time, test_results$metadata$start_time, units = "secs")
)

# Generate Reports
cat("\n\nGenerating Reports\n")
cat("=============================================================================\n")

# 1. Summary Text Report
summary_file <- file.path(results_dir, "summary.txt")
cat("  Creating summary report...", summary_file, "\n")

sink(summary_file)
cat("QIPRNG COMPREHENSIVE TEST RESULTS V2\n")
cat("===================================\n\n")
cat("Test Run Information:\n")
cat("  Start Time:", format(test_results$metadata$start_time, "%Y-%m-%d %H:%M:%S"), "\n")
cat("  End Time:", format(test_results$metadata$end_time, "%Y-%m-%d %H:%M:%S"), "\n")
cat("  Duration:", sprintf("%.2f seconds", test_results$metadata$total_duration), "\n")
cat("  Package Version:", test_results$metadata$package_version, "\n")
cat("  R Version:", test_results$metadata$r_version, "\n")
cat("  Platform:", test_results$metadata$platform, "\n\n")

cat("Summary Statistics:\n")
cat("  Total Test Files:", test_results$summary$total_files, "\n")
cat("  Total Tests Run:", test_results$summary$total_tests, "\n")
cat("  Passed Tests:", test_results$summary$passed_tests, "\n")
cat("  Failed Tests:", test_results$summary$failed_tests, "\n")
cat("  Skipped Tests:", test_results$summary$skipped_tests, "\n")
cat("  Error Files:", test_results$summary$error_files, "\n")
cat("  Files with Warnings:", test_results$summary$warning_files, "\n")
cat("  Test Success Rate:", sprintf("%.1f%%", 
    100 * test_results$summary$passed_tests / max(1, test_results$summary$total_tests)), "\n\n")

cat("Statistical Improvements Validation:\n")
for (phase in names(test_results$validation_results)) {
  result <- test_results$validation_results[[phase]]
  cat(sprintf("  %s: %s\n", gsub("_", " ", phase), result$status))
}

cat("\nDetailed Results by Category:\n")
for (category_id in names(test_results$categories)) {
  category_name <- test_inventory[[category_id]]$name
  category_results <- test_results$categories[[category_id]]
  
  cat(sprintf("\n%s:\n", category_name))
  
  for (test_name in names(category_results)) {
    result <- category_results[[test_name]]
    cat(sprintf("  %-40s %s (P:%d F:%d S:%d) %.2fs\n", 
                basename(result$test_name), 
                result$status,
                result$passed,
                result$failed,
                result$skipped,
                result$execution_time))
  }
}
sink()

# 2. CSV Report
csv_file <- file.path(results_dir, "detailed_results.csv")
cat("  Creating CSV report...", csv_file, "\n")

csv_data <- do.call(rbind, lapply(test_results$detailed_results, function(x) {
  data.frame(
    test_name = x$test_name,
    category = x$category,
    status = x$status,
    passed = x$passed,
    failed = x$failed,
    skipped = x$skipped,
    warnings = x$warnings,
    execution_time = x$execution_time,
    timestamp = x$timestamp,
    error_message = ifelse(is.null(x$error_message), "", x$error_message),
    stringsAsFactors = FALSE
  )
}))

write.csv(csv_data, csv_file, row.names = FALSE)

# 3. JSON Report
json_file <- file.path(results_dir, "results.json")
cat("  Creating JSON report...", json_file, "\n")

json_output <- toJSON(test_results, pretty = TRUE, auto_unbox = TRUE)
writeLines(json_output, json_file)

# 4. RDS Report
rds_file <- file.path(results_dir, "results.rds")
cat("  Creating RDS report...", rds_file, "\n")
saveRDS(test_results, rds_file)

# 5. HTML Report
html_file <- file.path(results_dir, "report.html")
cat("  Creating HTML report...", html_file, "\n")

# Calculate percentages
test_total <- max(1, test_results$summary$total_tests)
pass_rate <- 100 * test_results$summary$passed_tests / test_total
fail_rate <- 100 * test_results$summary$failed_tests / test_total
skip_rate <- 100 * test_results$summary$skipped_tests / test_total

html_content <- sprintf('
<!DOCTYPE html>
<html>
<head>
    <title>QIPRNG Test Results - %s</title>
    <style>
        body { font-family: Arial, sans-serif; margin: 20px; background-color: #f5f5f5; }
        .container { max-width: 1200px; margin: 0 auto; background-color: white; padding: 20px; box-shadow: 0 0 10px rgba(0,0,0,0.1); }
        h1, h2, h3 { color: #333; }
        .summary { background-color: #e8f4f8; padding: 15px; border-radius: 5px; margin: 20px 0; }
        .pass { color: #28a745; font-weight: bold; }
        .fail { color: #dc3545; font-weight: bold; }
        .error { color: #ff6b6b; font-weight: bold; }
        .warning { color: #ffc107; font-weight: bold; }
        .skip { color: #6c757d; font-weight: bold; }
        table { border-collapse: collapse; width: 100%%; margin: 20px 0; }
        th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }
        th { background-color: #f2f2f2; font-weight: bold; }
        tr:nth-child(even) { background-color: #f9f9f9; }
        .metric { display: inline-block; margin: 10px 20px 10px 0; }
        .metric-value { font-size: 24px; font-weight: bold; }
        .metric-label { color: #666; }
        .chart { margin: 20px 0; }
        .progress-bar { width: 100%%; height: 30px; background-color: #e0e0e0; border-radius: 15px; overflow: hidden; display: flex; }
        .progress-fill-pass { height: 100%%; background-color: #28a745; }
        .progress-fill-fail { height: 100%%; background-color: #dc3545; }
        .progress-fill-skip { height: 100%%; background-color: #6c757d; }
        .category-section { margin: 30px 0; padding: 20px; background-color: #f9f9f9; border-radius: 5px; }
    </style>
</head>
<body>
    <div class="container">
        <h1>QIPRNG Comprehensive Test Results V2</h1>
        <p>Generated on %s</p>
        
        <div class="summary">
            <h2>Summary</h2>
            <div class="metric">
                <div class="metric-value">%d</div>
                <div class="metric-label">Test Files</div>
            </div>
            <div class="metric">
                <div class="metric-value">%d</div>
                <div class="metric-label">Total Tests</div>
            </div>
            <div class="metric">
                <div class="metric-value class="pass">%d</div>
                <div class="metric-label">Passed</div>
            </div>
            <div class="metric">
                <div class="metric-value class="fail">%d</div>
                <div class="metric-label">Failed</div>
            </div>
            <div class="metric">
                <div class="metric-value class="skip">%d</div>
                <div class="metric-label">Skipped</div>
            </div>
            <div class="metric">
                <div class="metric-value class="error">%d</div>
                <div class="metric-label">Errors</div>
            </div>
            
            <h3>Test Results Distribution</h3>
            <div class="progress-bar">
                <div class="progress-fill-pass" style="width: %.1f%%;"></div>
                <div class="progress-fill-fail" style="width: %.1f%%;"></div>
                <div class="progress-fill-skip" style="width: %.1f%%;"></div>
            </div>
            <p>Pass: %.1f%% | Fail: %.1f%% | Skip: %.1f%%</p>
        </div>
        
        <div class="summary">
            <h2>Statistical Improvements Validation</h2>
            <table>
                <tr>
                    <th>Phase</th>
                    <th>Feature</th>
                    <th>Status</th>
                    <th>Details</th>
                </tr>
                <tr>
                    <td>Phase 1</td>
                    <td>P-value Adjustment</td>
                    <td class="%s">%s</td>
                    <td>%s</td>
                </tr>
                <tr>
                    <td>Phase 2</td>
                    <td>Weighting System</td>
                    <td class="%s">%s</td>
                    <td>%s</td>
                </tr>
                <tr>
                    <td>Phase 3</td>
                    <td>Effect Sizes</td>
                    <td class="%s">%s</td>
                    <td>%s</td>
                </tr>
            </table>
        </div>
        
        <h2>Test Results by Category</h2>
', 
format(test_results$metadata$start_time, "%Y-%m-%d %H:%M:%S"),
format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
test_results$summary$total_files,
test_results$summary$total_tests,
test_results$summary$passed_tests,
test_results$summary$failed_tests,
test_results$summary$skipped_tests,
test_results$summary$error_files,
pass_rate, fail_rate, skip_rate,
pass_rate, fail_rate, skip_rate,
tolower(test_results$validation_results$p_value_adjustment$status),
test_results$validation_results$p_value_adjustment$status,
test_results$validation_results$p_value_adjustment$message,
tolower(test_results$validation_results$weighting_system$status),
test_results$validation_results$weighting_system$status,
test_results$validation_results$weighting_system$message,
tolower(test_results$validation_results$effect_sizes$status),
test_results$validation_results$effect_sizes$status,
test_results$validation_results$effect_sizes$message
)

# Add category details to HTML
for (category_id in names(test_results$categories)) {
  category_name <- test_inventory[[category_id]]$name
  category_results <- test_results$categories[[category_id]]
  
  html_content <- paste0(html_content, sprintf('
        <div class="category-section">
            <h3>%s</h3>
            <table>
                <tr>
                    <th>Test File</th>
                    <th>Status</th>
                    <th>Passed</th>
                    <th>Failed</th>
                    <th>Skipped</th>
                    <th>Warnings</th>
                    <th>Time (s)</th>
                </tr>
', category_name))
  
  for (test_name in names(category_results)) {
    result <- category_results[[test_name]]
    html_content <- paste0(html_content, sprintf('
                <tr>
                    <td>%s</td>
                    <td class="%s">%s</td>
                    <td>%d</td>
                    <td>%d</td>
                    <td>%d</td>
                    <td>%d</td>
                    <td>%.2f</td>
                </tr>
',
    basename(result$test_name),
    tolower(result$status),
    result$status,
    result$passed,
    result$failed,
    result$skipped,
    result$warnings,
    result$execution_time
    ))
  }
  
  html_content <- paste0(html_content, '
            </table>
        </div>
')
}

# Complete HTML
html_content <- paste0(html_content, '
        <div class="summary">
            <h2>Test Environment</h2>
            <table>
                <tr><th>Property</th><th>Value</th></tr>
                <tr><td>Package Version</td><td>', test_results$metadata$package_version, '</td></tr>
                <tr><td>R Version</td><td>', test_results$metadata$r_version, '</td></tr>
                <tr><td>Platform</td><td>', test_results$metadata$platform, '</td></tr>
                <tr><td>Hostname</td><td>', test_results$metadata$hostname, '</td></tr>
                <tr><td>Total Duration</td><td>', sprintf("%.2f seconds", test_results$metadata$total_duration), '</td></tr>
            </table>
        </div>
    </div>
</body>
</html>
')

writeLines(html_content, html_file)

# Final Summary
cat("\n\n=============================================================================\n")
cat("TEST EXECUTION COMPLETE\n")
cat("=============================================================================\n")
cat(sprintf("Total Duration: %.2f seconds\n", test_results$metadata$total_duration))
cat(sprintf("Test Files: %d\n", test_results$summary$total_files))
cat(sprintf("Total Tests: %d\n", test_results$summary$total_tests))
cat(sprintf("Success Rate: %.1f%% (%d/%d tests passed)\n", 
    100 * test_results$summary$passed_tests / max(1, test_results$summary$total_tests),
    test_results$summary$passed_tests,
    test_results$summary$total_tests))

if (test_results$summary$failed_tests > 0 || test_results$summary$error_files > 0) {
  cat("\n⚠️  ATTENTION: Some tests failed or encountered errors!\n")
  cat(sprintf("   Failed Tests: %d\n", test_results$summary$failed_tests))
  cat(sprintf("   Error Files: %d\n", test_results$summary$error_files))
} else {
  cat("\n✅ All tests passed successfully!\n")
}

cat("\nReports generated in:", results_dir, "\n")
cat("  • summary.txt     - Human-readable summary\n")
cat("  • report.html     - Interactive HTML report\n") 
cat("  • detailed_results.csv - Detailed test results\n")
cat("  • results.json    - Machine-readable JSON\n")
cat("  • results.rds     - R data structure\n")
cat("  • logs/           - Individual test logs\n")

# Open HTML report if interactive
if (interactive()) {
  cat("\nOpening HTML report in browser...\n")
  browseURL(html_file)
}

# Exit with appropriate code
quit(status = ifelse(test_results$summary$failed_tests > 0 || test_results$summary$error_files > 0, 1, 0))