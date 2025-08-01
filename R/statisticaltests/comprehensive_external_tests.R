# File: comprehensive_external_tests.R
# ----------------------------------------------------------------------
#' Comprehensive External Test Runner
#'
#' This module integrates all external test wrappers into a unified framework
#' for comprehensive randomness testing using multiple R packages.

# Source wrapper files
source_wrappers <- function() {
  # Try to source wrapper files
  wrapper_files <- c("external_wrappers.R", "additional_wrappers.R")
  
  for (file in wrapper_files) {
    # Try package installation path first
    path <- system.file(file.path("R/statisticaltests", file), package = "qiprng")
    if (path == "") {
      # Try relative paths for development
      if (file.exists(file)) {
        source(file)
      } else if (file.exists(file.path("R/statisticaltests", file))) {
        source(file.path("R/statisticaltests", file))
      }
    } else if (file.exists(path)) {
      source(path)
    }
  }
}

# Source wrappers on load
source_wrappers()

#' Run Comprehensive External Test Suite
#'
#' Runs all available external randomness tests from multiple packages
#'
#' @param x Numeric vector of random numbers in [0,1)
#' @param packages Character vector of packages to include, or "all"
#' @param alpha Significance level (default: 0.05)
#' @param verbose Logical: print progress messages (default: TRUE)
#' @return List containing test results and summary statistics
#' @export
run_comprehensive_external_tests <- function(x, 
                                           packages = "all",
                                           alpha = 0.05,
                                           verbose = TRUE) {
  
  # Available packages mapping
  available_packages <- c(
    "CryptRndTest" = "cryptrndtest",
    "randtests" = "randtests", 
    "tseries" = "tseries",
    "randtoolbox" = "randtoolbox",
    "lawstat" = "lawstat",
    "entropy" = "entropy"
  )
  
  if ("all" %in% packages) {
    packages <- names(available_packages)
  }
  
  # Initialize results
  results <- list(
    tests = list(),
    summary = list(),
    metadata = list(
      sample_size = length(x),
      alpha = alpha,
      timestamp = Sys.time()
    )
  )
  
  # Run tests from each package
  for (pkg in packages) {
    pkg_id <- available_packages[pkg]
    
    if (is.na(pkg_id)) {
      if (verbose) cat("Warning: Unknown package", pkg, "\n")
      next
    }
    
    if (verbose) cat("Running tests from", pkg, "...\n")
    
    if (pkg == "CryptRndTest" && exists("run_cryptrndtest_suite")) {
      tryCatch({
        results$tests[[pkg]] <- run_cryptrndtest_suite(x, alpha = alpha)
      }, error = function(e) {
        results$tests[[pkg]] <- list(error = as.character(e))
      })
      
    } else if (pkg == "randtests" && exists("run_randtests_suite")) {
      tryCatch({
        results$tests[[pkg]] <- run_randtests_suite(x, alpha = alpha)
      }, error = function(e) {
        results$tests[[pkg]] <- list(error = as.character(e))
      })
      
    } else if (pkg == "tseries" && exists("run_tseries_suite")) {
      tryCatch({
        results$tests[[pkg]] <- run_tseries_suite(x, alpha = alpha)
      }, error = function(e) {
        results$tests[[pkg]] <- list(error = as.character(e))
      })
      
    } else if (pkg == "randtoolbox" && exists("run_randtoolbox_suite")) {
      tryCatch({
        results$tests[[pkg]] <- run_randtoolbox_suite(x, alpha = alpha)
      }, error = function(e) {
        results$tests[[pkg]] <- list(error = as.character(e))
      })
      
    } else if (pkg == "lawstat" && exists("run_lawstat_suite")) {
      tryCatch({
        results$tests[[pkg]] <- run_lawstat_suite(x, alpha = alpha)
      }, error = function(e) {
        results$tests[[pkg]] <- list(error = as.character(e))
      })
      
    } else if (pkg == "entropy" && exists("run_entropy_suite")) {
      tryCatch({
        results$tests[[pkg]] <- run_entropy_suite(x, alpha = alpha)
      }, error = function(e) {
        results$tests[[pkg]] <- list(error = as.character(e))
      })
      
    } else {
      results$tests[[pkg]] <- list(
        error = paste("Package", pkg, "tests not available")
      )
    }
  }
  
  # Calculate summary statistics
  total_tests <- 0
  passed <- 0
  failed <- 0
  errors <- 0
  
  for (pkg in names(results$tests)) {
    pkg_results <- results$tests[[pkg]]
    
    if (!is.null(pkg_results$error)) {
      errors <- errors + 1
      next
    }
    
    for (test_name in names(pkg_results)) {
      if (is.list(pkg_results[[test_name]]) && 
          !is.null(pkg_results[[test_name]]$result)) {
        total_tests <- total_tests + 1
        
        if (pkg_results[[test_name]]$result == "PASS") {
          passed <- passed + 1
        } else if (pkg_results[[test_name]]$result == "FAIL") {
          failed <- failed + 1
        } else {
          errors <- errors + 1
        }
      }
    }
  }
  
  results$summary <- list(
    total_tests = total_tests,
    passed = passed,
    failed = failed,
    errors = errors,
    pass_rate = if (total_tests > 0) passed / total_tests else 0,
    packages_used = names(results$tests)
  )
  
  if (verbose) {
    cat("\n=== Test Summary ===\n")
    cat("Total tests:", total_tests, "\n")
    cat("Passed:", passed, sprintf("(%.1f%%)\n", 100 * results$summary$pass_rate))
    cat("Failed:", failed, "\n")
    cat("Errors:", errors, "\n")
  }
  
  return(results)
}

#' Print Comprehensive Test Results
#'
#' Pretty prints the results from comprehensive external tests
#'
#' @param results Results object from run_comprehensive_external_tests
#' @param detail_level "summary", "failed", or "all"
#' @export
print_comprehensive_results <- function(results, detail_level = "summary") {
  cat("\n====== Comprehensive External Test Results ======\n")
  cat("Sample size:", results$metadata$sample_size, "\n")
  cat("Significance level:", results$metadata$alpha, "\n")
  cat("Timestamp:", format(results$metadata$timestamp, "%Y-%m-%d %H:%M:%S"), "\n")
  
  cat("\n--- Overall Summary ---\n")
  cat("Total tests:", results$summary$total_tests, "\n")
  cat("Passed:", results$summary$passed, 
      sprintf("(%.1f%%)\n", 100 * results$summary$pass_rate))
  cat("Failed:", results$summary$failed, "\n")
  cat("Errors:", results$summary$errors, "\n")
  
  if (detail_level != "summary") {
    cat("\n--- Detailed Results ---\n")
    
    for (pkg in names(results$tests)) {
      cat("\n[", pkg, "]\n", sep = "")
      pkg_results <- results$tests[[pkg]]
      
      if (!is.null(pkg_results$error)) {
        cat("  ERROR:", pkg_results$error, "\n")
        next
      }
      
      for (test_name in names(pkg_results)) {
        test <- pkg_results[[test_name]]
        
        if (!is.list(test) || is.null(test$result)) next
        
        if (detail_level == "all" || 
            (detail_level == "failed" && test$result != "PASS")) {
          
          cat(sprintf("  %-30s: %s", test$description, test$result))
          
          if (!is.null(test$p_value) && !is.na(test$p_value)) {
            cat(sprintf(" (p=%.4f)", test$p_value))
          }
          
          if (!is.null(test$details)) {
            cat("\n    Details:", test$details)
          }
          
          if (!is.null(test$error)) {
            cat("\n    Error:", test$error)
          }
          
          cat("\n")
        }
      }
    }
  }
  
  cat("\n")
}

#' Generate Test Report
#'
#' Creates a detailed report of comprehensive test results
#'
#' @param results Results from run_comprehensive_external_tests
#' @param file Output file path (HTML format)
#' @param include_plots Whether to include visualization plots
#' @export
generate_test_report <- function(results, file = "external_tests_report.html", 
                               include_plots = TRUE) {
  
  # Start HTML
  html <- c(
    "<!DOCTYPE html>",
    "<html>",
    "<head>",
    "  <title>Comprehensive External Randomness Test Report</title>",
    "  <style>",
    "    body { font-family: Arial, sans-serif; margin: 20px; }",
    "    h1, h2, h3 { color: #333; }",
    "    .pass { color: green; font-weight: bold; }",
    "    .fail { color: red; font-weight: bold; }",
    "    .error { color: orange; font-weight: bold; }",
    "    table { border-collapse: collapse; width: 100%; margin: 10px 0; }",
    "    th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }",
    "    th { background-color: #f2f2f2; }",
    "    .summary-box { background-color: #f9f9f9; padding: 15px; ",
    "                   border-radius: 5px; margin: 10px 0; }",
    "  </style>",
    "</head>",
    "<body>",
    "  <h1>Comprehensive External Randomness Test Report</h1>"
  )
  
  # Add metadata
  html <- c(html,
    "  <div class='summary-box'>",
    paste0("    <p><strong>Generated:</strong> ", 
           format(results$metadata$timestamp, "%Y-%m-%d %H:%M:%S"), "</p>"),
    paste0("    <p><strong>Sample Size:</strong> ", 
           format(results$metadata$sample_size, big.mark = ","), "</p>"),
    paste0("    <p><strong>Significance Level:</strong> ", 
           results$metadata$alpha, "</p>"),
    paste0("    <p><strong>Packages Used:</strong> ", 
           paste(results$summary$packages_used, collapse = ", "), "</p>"),
    "  </div>"
  )
  
  # Add overall summary
  html <- c(html,
    "  <h2>Overall Summary</h2>",
    "  <div class='summary-box'>",
    paste0("    <p><strong>Total Tests:</strong> ", results$summary$total_tests, "</p>"),
    paste0("    <p><strong>Passed:</strong> <span class='pass'>", 
           results$summary$passed, " (", 
           sprintf("%.1f%%", 100 * results$summary$pass_rate), ")</span></p>"),
    paste0("    <p><strong>Failed:</strong> <span class='fail'>", 
           results$summary$failed, "</span></p>"),
    paste0("    <p><strong>Errors:</strong> <span class='error'>", 
           results$summary$errors, "</span></p>"),
    "  </div>"
  )
  
  # Add detailed results table
  html <- c(html,
    "  <h2>Detailed Test Results</h2>",
    "  <table>",
    "    <tr>",
    "      <th>Package</th>",
    "      <th>Test</th>",
    "      <th>Result</th>",
    "      <th>P-Value</th>",
    "      <th>Details</th>",
    "    </tr>"
  )
  
  # Add each test result
  for (pkg in names(results$tests)) {
    pkg_results <- results$tests[[pkg]]
    
    if (!is.null(pkg_results$error)) {
      html <- c(html,
        "    <tr>",
        paste0("      <td>", pkg, "</td>"),
        "      <td colspan='4' class='error'>Error: ", pkg_results$error, "</td>",
        "    </tr>"
      )
      next
    }
    
    for (test_name in names(pkg_results)) {
      test <- pkg_results[[test_name]]
      
      if (!is.list(test) || is.null(test$result)) next
      
      result_class <- tolower(test$result)
      p_value_str <- if (!is.null(test$p_value) && !is.na(test$p_value)) {
        sprintf("%.4f", test$p_value)
      } else {
        "N/A"
      }
      
      details_str <- if (!is.null(test$details)) test$details else ""
      if (!is.null(test$error)) {
        details_str <- paste("Error:", test$error)
      }
      
      html <- c(html,
        "    <tr>",
        paste0("      <td>", pkg, "</td>"),
        paste0("      <td>", test$description, "</td>"),
        paste0("      <td class='", result_class, "'>", test$result, "</td>"),
        paste0("      <td>", p_value_str, "</td>"),
        paste0("      <td>", details_str, "</td>"),
        "    </tr>"
      )
    }
  }
  
  html <- c(html, "  </table>")
  
  # Add p-value distribution analysis
  p_values <- c()
  for (pkg in names(results$tests)) {
    pkg_results <- results$tests[[pkg]]
    if (!is.null(pkg_results$error)) next
    
    for (test_name in names(pkg_results)) {
      test <- pkg_results[[test_name]]
      if (!is.null(test$p_value) && !is.na(test$p_value) && 
          test$result != "ERROR") {
        p_values <- c(p_values, test$p_value)
      }
    }
  }
  
  if (length(p_values) > 0) {
    html <- c(html,
      "  <h2>P-Value Distribution Analysis</h2>",
      "  <div class='summary-box'>",
      paste0("    <p><strong>Number of p-values:</strong> ", length(p_values), "</p>"),
      paste0("    <p><strong>Mean p-value:</strong> ", 
             sprintf("%.3f", mean(p_values)), "</p>"),
      paste0("    <p><strong>Median p-value:</strong> ", 
             sprintf("%.3f", median(p_values)), "</p>"),
      paste0("    <p><strong>Min p-value:</strong> ", 
             sprintf("%.4f", min(p_values)), "</p>"),
      paste0("    <p><strong>Max p-value:</strong> ", 
             sprintf("%.4f", max(p_values)), "</p>")
    )
    
    # Kolmogorov-Smirnov test for uniformity
    if (length(p_values) >= 8) {
      ks_test <- ks.test(p_values, punif)
      html <- c(html,
        paste0("    <p><strong>KS test for uniformity:</strong> p = ", 
               sprintf("%.4f", ks_test$p.value), 
               " (", ifelse(ks_test$p.value > 0.05, "uniform", "non-uniform"), ")</p>")
      )
    }
    
    html <- c(html, "  </div>")
  }
  
  # Close HTML
  html <- c(html, "</body>", "</html>")
  
  # Write to file
  writeLines(html, file)
  
  cat("Report saved to:", file, "\n")
  invisible(file)
}

# Export functions
if (!exists("comprehensive_export")) {
  comprehensive_export <- list(
    run_comprehensive_external_tests = run_comprehensive_external_tests,
    print_comprehensive_results = print_comprehensive_results,
    generate_test_report = generate_test_report
  )
}