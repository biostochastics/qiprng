# File: unified_reporting.R
# ----------------------------------------------------------------------
#' Unified Reporting Framework for qiprng Test Results
#'
#' This module provides a standardized reporting system for all test results,
#' including internal tests, external package tests, and multi-dimensional tests.
#' Reports can be generated in multiple formats (HTML, PDF, JSON, markdown).

#' S3 class for test results
#' @export
test_result <- function(test_name, description, result, p_value = NULL, 
                       statistic = NULL, details = NULL, data = NULL,
                       category = "general", subcategory = NULL,
                       p_adjusted = NULL, adjustment_method = NULL,
                       effect_size = NULL, effect_size_interpretation = NULL) {
  structure(
    list(
      test_name = test_name,
      description = description,
      result = result,  # "PASS", "FAIL", "ERROR", "SKIP"
      p_value = p_value,
      p_adjusted = p_adjusted,  # Adjusted p-value after multiple testing correction
      adjustment_method = adjustment_method,  # Method used for p-value adjustment
      statistic = statistic,
      effect_size = effect_size,  # Effect size measure (e.g., Cohen's d, Cramér's V)
      effect_size_interpretation = effect_size_interpretation,  # e.g., "small", "medium", "large"
      details = details,
      data = data,  # Additional data (e.g., plots, tables)
      category = category,
      subcategory = subcategory,
      timestamp = Sys.time()
    ),
    class = "test_result"
  )
}

#' Print method for test_result
#' @export
print.test_result <- function(x, ...) {
  cat(sprintf("Test: %s\n", x$test_name))
  cat(sprintf("Description: %s\n", x$description))
  cat(sprintf("Result: %s\n", x$result))
  if (!is.null(x$p_value)) {
    cat(sprintf("P-value: %.4f\n", x$p_value))
  }
  if (!is.null(x$p_adjusted)) {
    cat(sprintf("Adjusted P-value: %.4f (%s)\n", x$p_adjusted, 
                x$adjustment_method %||% "unknown"))
  }
  if (!is.null(x$statistic)) {
    cat(sprintf("Statistic: %.4f\n", x$statistic))
  }
  if (!is.null(x$effect_size)) {
    cat(sprintf("Effect Size: %.4f", x$effect_size))
    if (!is.null(x$effect_size_interpretation)) {
      cat(sprintf(" (%s)", x$effect_size_interpretation))
    }
    cat("\n")
  }
  if (!is.null(x$details)) {
    cat(sprintf("Details: %s\n", x$details))
  }
  invisible(x)
}

#' S3 class for test suite results
#' @export
test_suite_results <- function(suite_name, prng_info = NULL) {
  structure(
    list(
      suite_name = suite_name,
      prng_info = prng_info,
      results = list(),
      metadata = list(
        start_time = Sys.time(),
        end_time = NULL,
        total_tests = 0,
        passed = 0,
        failed = 0,
        errors = 0,
        skipped = 0,
        categories = list()
      ),
      config = list(
        significance_level = 0.05,
        sample_size = NULL,
        test_parameters = list()
      )
    ),
    class = "test_suite_results"
  )
}

#' Add test result to suite
#' @export
add_test_result <- function(suite, result) {
  UseMethod("add_test_result")
}

#' @export
add_test_result.test_suite_results <- function(suite, result) {
  if (!inherits(result, "test_result")) {
    stop("Result must be a test_result object")
  }
  
  # Add to results
  suite$results[[length(suite$results) + 1]] <- result
  
  # Update metadata
  suite$metadata$total_tests <- suite$metadata$total_tests + 1
  
  if (result$result == "PASS") {
    suite$metadata$passed <- suite$metadata$passed + 1
  } else if (result$result == "FAIL") {
    suite$metadata$failed <- suite$metadata$failed + 1
  } else if (result$result == "ERROR") {
    suite$metadata$errors <- suite$metadata$errors + 1
  } else if (result$result == "SKIP") {
    suite$metadata$skipped <- suite$metadata$skipped + 1
  }
  
  # Track categories
  if (!result$category %in% names(suite$metadata$categories)) {
    suite$metadata$categories[[result$category]] <- list(
      total = 0, passed = 0, failed = 0, errors = 0, skipped = 0
    )
  }
  
  cat_meta <- suite$metadata$categories[[result$category]]
  cat_meta$total <- cat_meta$total + 1
  
  if (result$result == "PASS") {
    cat_meta$passed <- cat_meta$passed + 1
  } else if (result$result == "FAIL") {
    cat_meta$failed <- cat_meta$failed + 1
  } else if (result$result == "ERROR") {
    cat_meta$errors <- cat_meta$errors + 1
  } else if (result$result == "SKIP") {
    cat_meta$skipped <- cat_meta$skipped + 1
  }
  
  suite$metadata$categories[[result$category]] <- cat_meta
  
  return(suite)
}

#' Finalize test suite
#' @export
finalize_suite <- function(suite) {
  suite$metadata$end_time <- Sys.time()
  suite$metadata$duration <- difftime(
    suite$metadata$end_time, 
    suite$metadata$start_time, 
    units = "secs"
  )
  suite$metadata$pass_rate <- if (suite$metadata$total_tests > 0) {
    suite$metadata$passed / suite$metadata$total_tests
  } else {
    0
  }
  return(suite)
}

#' Adjust p-values for multiple testing correction
#' 
#' This function applies p-value adjustment to control for multiple testing
#' when running many statistical tests simultaneously. It extracts all p-values
#' from test results, applies the specified adjustment method, and adds the
#' adjusted p-values back to the results.
#' 
#' @param suite Test suite object containing results
#' @param method P-value adjustment method. Options: "BH" (Benjamini-Hochberg, default),
#'   "bonferroni", "holm", "hochberg", "hommel", "BY", "fdr", "none"
#' @param alpha Significance level (default: 0.05)
#' @return Updated test suite with adjusted p-values
#' @export
adjust_p_values <- function(suite, method = "BH", alpha = 0.05) {
  # Validate method
  valid_methods <- c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none")
  if (!method %in% valid_methods) {
    stop(sprintf("Invalid p-value adjustment method '%s'. Choose from: %s", 
                 method, paste(valid_methods, collapse = ", ")))
  }
  
  # If method is "none", return suite unchanged
  if (method == "none") {
    message("No p-value adjustment applied (method = 'none')")
    return(suite)
  }
  
  # Extract all p-values with their indices
  p_values <- numeric()
  p_indices <- list()
  
  for (i in seq_along(suite$results)) {
    result <- suite$results[[i]]
    if (!is.null(result$p_value) && !is.na(result$p_value)) {
      p_values <- c(p_values, result$p_value)
      p_indices[[length(p_values)]] <- i
    }
  }
  
  # Check if we have p-values to adjust
  if (length(p_values) == 0) {
    warning("No p-values found in test results. No adjustment performed.")
    return(suite)
  }
  
  # Apply p-value adjustment
  adjusted_p_values <- p.adjust(p_values, method = method)
  
  # Add adjusted p-values back to results
  for (j in seq_along(p_values)) {
    idx <- p_indices[[j]]
    suite$results[[idx]]$p_adjusted <- adjusted_p_values[j]
    suite$results[[idx]]$adjustment_method <- method
    
    # Update pass/fail status based on adjusted p-value
    if (!is.na(adjusted_p_values[j])) {
      if (adjusted_p_values[j] >= alpha) {
        # Test passes with adjusted p-value
        if (suite$results[[idx]]$result == "FAIL" && 
            suite$results[[idx]]$p_value >= alpha) {
          # Was already passing with raw p-value, keep as PASS
          suite$results[[idx]]$result <- "PASS"
        } else if (suite$results[[idx]]$result == "FAIL") {
          # Was failing with raw p-value but passes with adjusted
          suite$results[[idx]]$result <- "PASS"
          suite$results[[idx]]$adjusted_decision <- TRUE
        }
      } else {
        # Test fails with adjusted p-value
        if (suite$results[[idx]]$result == "PASS") {
          # Was passing with raw p-value but fails with adjusted
          suite$results[[idx]]$result <- "FAIL"
          suite$results[[idx]]$adjusted_decision <- TRUE
        }
      }
    }
  }
  
  # Add adjustment metadata to suite
  suite$config$p_adjustment_method <- method
  suite$config$p_adjustment_alpha <- alpha
  suite$metadata$p_values_adjusted <- TRUE
  suite$metadata$total_tests_adjusted <- length(p_values)
  
  # Recalculate summary statistics
  suite$metadata$passed <- sum(sapply(suite$results, function(r) r$result == "PASS"))
  suite$metadata$failed <- sum(sapply(suite$results, function(r) r$result == "FAIL"))
  suite$metadata$pass_rate <- if (suite$metadata$total_tests > 0) {
    suite$metadata$passed / suite$metadata$total_tests
  } else {
    0
  }
  
  message(sprintf("Applied %s p-value adjustment to %d tests", method, length(p_values)))
  
  return(suite)
}

#' Get effective p-value (adjusted if available, raw otherwise)
#' @param result Test result object
#' @return The p-value to use for decisions (adjusted if available)
#' @export
get_effective_p_value <- function(result) {
  if (!is.null(result$p_adjusted) && !is.na(result$p_adjusted)) {
    return(result$p_adjusted)
  } else if (!is.null(result$p_value) && !is.na(result$p_value)) {
    return(result$p_value)
  } else {
    return(NA)
  }
}

#' Check if test passes based on effective p-value
#' @param result Test result object
#' @param alpha Significance level (default: 0.05)
#' @return TRUE if test passes, FALSE otherwise
#' @export
test_passes <- function(result, alpha = 0.05) {
  p_val <- get_effective_p_value(result)
  if (is.na(p_val)) {
    # If no p-value, use the result field
    return(result$result == "PASS")
  }
  return(p_val >= alpha)
}

#' Generate unified report
#' @export
generate_unified_report <- function(suite, format = "html", 
                                   output_file = NULL,
                                   include_plots = TRUE,
                                   theme = "default") {
  
  if (is.null(output_file)) {
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    output_file <- sprintf("qiprng_test_report_%s.%s", timestamp, 
                          if (format == "pdf") "pdf" else format)
  }
  
  # Finalize suite if not already done
  if (is.null(suite$metadata$end_time)) {
    suite <- finalize_suite(suite)
  }
  
  # Generate report based on format
  if (format == "html") {
    generate_html_report(suite, output_file, include_plots, theme)
  } else if (format == "markdown") {
    generate_markdown_report(suite, output_file, include_plots)
  } else if (format == "json") {
    generate_json_report(suite, output_file)
  } else if (format == "pdf") {
    # Generate HTML first, then convert to PDF
    temp_html <- tempfile(fileext = ".html")
    generate_html_report(suite, temp_html, include_plots, theme)
    # Would need additional package like pagedown or webshot for PDF conversion
    warning("PDF generation requires additional packages. Generated HTML instead.")
    file.rename(temp_html, gsub("\\.pdf$", ".html", output_file))
  } else {
    stop("Unsupported format. Choose from: html, markdown, json, pdf")
  }
  
  invisible(output_file)
}

#' Generate HTML report
generate_html_report <- function(suite, output_file, include_plots = TRUE, 
                                theme = "default") {
  
  # CSS themes
  themes <- list(
    default = list(
      primary = "#2c3e50",
      success = "#27ae60",
      danger = "#e74c3c",
      warning = "#f39c12",
      info = "#3498db",
      background = "#ecf0f1",
      text = "#333333"
    ),
    dark = list(
      primary = "#34495e",
      success = "#00bc8c",
      danger = "#e74c3c",
      warning = "#f39c12",
      info = "#3498db",
      background = "#222222",
      text = "#ffffff"
    )
  )
  
  theme_colors <- themes[[theme]] %||% themes$default
  
  # Start HTML
  html <- c(
    '<!DOCTYPE html>',
    '<html lang="en">',
    '<head>',
    '  <meta charset="UTF-8">',
    '  <meta name="viewport" content="width=device-width, initial-scale=1.0">',
    '  <title>qiprng Test Report</title>',
    '  <style>',
    sprintf('    :root {
      --primary: %s;
      --success: %s;
      --danger: %s;
      --warning: %s;
      --info: %s;
      --background: %s;
      --text: %s;
    }', theme_colors$primary, theme_colors$success, theme_colors$danger,
        theme_colors$warning, theme_colors$info, theme_colors$background,
        theme_colors$text),
    '    body { 
      font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif;
      line-height: 1.6;
      color: var(--text);
      background-color: var(--background);
      margin: 0;
      padding: 0;
    }',
    '    .container {
      max-width: 1200px;
      margin: 0 auto;
      padding: 20px;
      background-color: white;
      box-shadow: 0 0 10px rgba(0,0,0,0.1);
    }',
    '    h1, h2, h3 { color: var(--primary); }',
    '    .header {
      background-color: var(--primary);
      color: white;
      padding: 30px;
      text-align: center;
      margin: -20px -20px 20px -20px;
    }',
    '    .summary-grid {
      display: grid;
      grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
      gap: 20px;
      margin: 20px 0;
    }',
    '    .summary-card {
      background-color: #f8f9fa;
      padding: 20px;
      border-radius: 8px;
      text-align: center;
      border: 1px solid #dee2e6;
    }',
    '    .summary-card h3 { margin: 0 0 10px 0; font-size: 1.1rem; }',
    '    .summary-card .value { font-size: 2rem; font-weight: bold; }',
    '    .pass { color: var(--success); }',
    '    .fail { color: var(--danger); }',
    '    .error { color: var(--warning); }',
    '    .skip { color: var(--info); }',
    '    table {
      width: 100%;
      border-collapse: collapse;
      margin: 20px 0;
    }',
    '    th, td {
      padding: 12px;
      text-align: left;
      border-bottom: 1px solid #dee2e6;
    }',
    '    th {
      background-color: #f8f9fa;
      font-weight: 600;
      color: var(--primary);
    }',
    '    tr:hover { background-color: #f8f9fa; }',
    '    .category-section {
      margin: 30px 0;
      padding: 20px;
      border: 1px solid #dee2e6;
      border-radius: 8px;
    }',
    '    .progress-bar {
      width: 100%;
      height: 30px;
      background-color: #e9ecef;
      border-radius: 15px;
      overflow: hidden;
      margin: 10px 0;
    }',
    '    .progress-fill {
      height: 100%;
      background-color: var(--success);
      text-align: center;
      line-height: 30px;
      color: white;
      font-weight: bold;
      transition: width 0.3s ease;
    }',
    '    .test-details {
      font-size: 0.9rem;
      color: #6c757d;
    }',
    '    .footer {
      text-align: center;
      margin-top: 40px;
      padding-top: 20px;
      border-top: 1px solid #dee2e6;
      color: #6c757d;
    }',
    '  </style>',
    '</head>',
    '<body>',
    '  <div class="container">'
  )
  
  # Header
  html <- c(html,
    '    <div class="header">',
    sprintf('      <h1>%s</h1>', suite$suite_name),
    '      <p>qiprng Statistical Test Report</p>',
    '    </div>'
  )
  
  # Summary section
  html <- c(html,
    '    <h2>Summary</h2>',
    '    <div class="summary-grid">',
    '      <div class="summary-card">',
    '        <h3>Total Tests</h3>',
    sprintf('        <div class="value">%d</div>', suite$metadata$total_tests),
    '      </div>',
    '      <div class="summary-card">',
    '        <h3>Passed</h3>',
    sprintf('        <div class="value pass">%d</div>', suite$metadata$passed),
    '      </div>',
    '      <div class="summary-card">',
    '        <h3>Failed</h3>',
    sprintf('        <div class="value fail">%d</div>', suite$metadata$failed),
    '      </div>',
    '      <div class="summary-card">',
    '        <h3>Errors</h3>',
    sprintf('        <div class="value error">%d</div>', suite$metadata$errors),
    '      </div>',
    '      <div class="summary-card">',
    '        <h3>Pass Rate</h3>',
    sprintf('        <div class="value">%.1f%%</div>', 
            suite$metadata$pass_rate * 100),
    '      </div>',
    '      <div class="summary-card">',
    '        <h3>Duration</h3>',
    sprintf('        <div class="value">%.1fs</div>', 
            as.numeric(suite$metadata$duration)),
    '      </div>',
    '    </div>'
  )
  
  # Overall progress bar
  html <- c(html,
    '    <div class="progress-bar">',
    sprintf('      <div class="progress-fill" style="width: %.1f%%">%.1f%% Passed</div>',
            suite$metadata$pass_rate * 100, suite$metadata$pass_rate * 100),
    '    </div>'
  )
  
  # Test details
  html <- c(html,
    '    <h2>Test Details</h2>'
  )
  
  # Group by category
  for (category in names(suite$metadata$categories)) {
    cat_results <- Filter(function(r) r$category == category, suite$results)
    if (length(cat_results) == 0) next
    
    cat_meta <- suite$metadata$categories[[category]]
    cat_pass_rate <- if (cat_meta$total > 0) {
      cat_meta$passed / cat_meta$total * 100
    } else {
      0
    }
    
    html <- c(html,
      sprintf('    <div class="category-section">'),
      sprintf('      <h3>%s</h3>', category),
      sprintf('      <p>%d tests, %.1f%% passed</p>', 
              cat_meta$total, cat_pass_rate),
      '      <table>',
      '        <tr>',
      '          <th>Test</th>',
      '          <th>Description</th>',
      '          <th>Result</th>',
      '          <th>P-value</th>',
      '          <th>Details</th>',
      '        </tr>'
    )
    
    # Add each test result
    for (result in cat_results) {
      p_value_str <- if (!is.null(result$p_value) && !is.na(result$p_value)) {
        sprintf("%.4f", result$p_value)
      } else {
        "N/A"
      }
      
      details_str <- result$details %||% ""
      
      html <- c(html,
        '        <tr>',
        sprintf('          <td>%s</td>', result$test_name),
        sprintf('          <td>%s</td>', result$description),
        sprintf('          <td class="%s">%s</td>', 
                tolower(result$result), result$result),
        sprintf('          <td>%s</td>', p_value_str),
        sprintf('          <td class="test-details">%s</td>', details_str),
        '        </tr>'
      )
    }
    
    html <- c(html,
      '      </table>',
      '    </div>'
    )
  }
  
  # PRNG info if available
  if (!is.null(suite$prng_info)) {
    html <- c(html,
      '    <h2>PRNG Information</h2>',
      '    <div class="category-section">',
      '      <table>'
    )
    
    for (name in names(suite$prng_info)) {
      html <- c(html,
        '        <tr>',
        sprintf('          <td><strong>%s</strong></td>', name),
        sprintf('          <td>%s</td>', suite$prng_info[[name]]),
        '        </tr>'
      )
    }
    
    html <- c(html,
      '      </table>',
      '    </div>'
    )
  }
  
  # Footer
  html <- c(html,
    '    <div class="footer">',
    sprintf('      <p>Generated on %s by qiprng v%s</p>',
            format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
            as.character(packageVersion("qiprng"))),
    '    </div>',
    '  </div>',
    '</body>',
    '</html>'
  )
  
  writeLines(html, output_file)
  cat("HTML report saved to:", output_file, "\n")
  invisible(output_file)
}

#' Generate Markdown report
generate_markdown_report <- function(suite, output_file, include_plots = TRUE) {
  md <- c(
    sprintf("# %s", suite$suite_name),
    "",
    "## Summary",
    "",
    sprintf("- **Total Tests**: %d", suite$metadata$total_tests),
    sprintf("- **Passed**: %d (%.1f%%)", suite$metadata$passed, 
            suite$metadata$pass_rate * 100),
    sprintf("- **Failed**: %d", suite$metadata$failed),
    sprintf("- **Errors**: %d", suite$metadata$errors),
    sprintf("- **Skipped**: %d", suite$metadata$skipped),
    sprintf("- **Duration**: %.1f seconds", as.numeric(suite$metadata$duration)),
    "",
    "## Test Results by Category",
    ""
  )
  
  # Group by category
  for (category in names(suite$metadata$categories)) {
    cat_results <- Filter(function(r) r$category == category, suite$results)
    if (length(cat_results) == 0) next
    
    cat_meta <- suite$metadata$categories[[category]]
    
    md <- c(md,
      sprintf("### %s", category),
      "",
      sprintf("%d tests, %d passed, %d failed", 
              cat_meta$total, cat_meta$passed, cat_meta$failed),
      "",
      "| Test | Result | P-value | Details |",
      "|------|--------|---------|---------|"
    )
    
    for (result in cat_results) {
      p_value_str <- if (!is.null(result$p_value) && !is.na(result$p_value)) {
        sprintf("%.4f", result$p_value)
      } else {
        "N/A"
      }
      
      md <- c(md,
        sprintf("| %s | %s | %s | %s |",
                result$test_name,
                result$result,
                p_value_str,
                result$details %||% "")
      )
    }
    
    md <- c(md, "")
  }
  
  writeLines(md, output_file)
  cat("Markdown report saved to:", output_file, "\n")
  invisible(output_file)
}

#' Generate JSON report
generate_json_report <- function(suite, output_file) {
  # Convert to JSON-friendly format
  json_data <- list(
    suite_name = suite$suite_name,
    metadata = list(
      start_time = format(suite$metadata$start_time, "%Y-%m-%d %H:%M:%S"),
      end_time = format(suite$metadata$end_time, "%Y-%m-%d %H:%M:%S"),
      duration = as.numeric(suite$metadata$duration),
      total_tests = suite$metadata$total_tests,
      passed = suite$metadata$passed,
      failed = suite$metadata$failed,
      errors = suite$metadata$errors,
      skipped = suite$metadata$skipped,
      pass_rate = suite$metadata$pass_rate,
      categories = suite$metadata$categories
    ),
    config = suite$config,
    prng_info = suite$prng_info,
    results = lapply(suite$results, function(r) {
      list(
        test_name = r$test_name,
        description = r$description,
        result = r$result,
        p_value = r$p_value,
        statistic = r$statistic,
        details = r$details,
        category = r$category,
        subcategory = r$subcategory,
        timestamp = format(r$timestamp, "%Y-%m-%d %H:%M:%S")
      )
    })
  )
  
  json_text <- jsonlite::toJSON(json_data, pretty = TRUE, auto_unbox = TRUE)
  writeLines(json_text, output_file)
  cat("JSON report saved to:", output_file, "\n")
  invisible(output_file)
}

#' Convert legacy test results to unified format
#' @export
convert_legacy_results <- function(legacy_suite) {
  unified_suite <- test_suite_results("Legacy Test Suite")
  
  # Extract PRNG info if available
  if (!is.null(legacy_suite$prng_func)) {
    unified_suite$prng_info <- list(
      type = "qiprng",
      config = legacy_suite$config
    )
  }
  
  # Convert each category of results
  for (category in names(legacy_suite$results)) {
    cat_results <- legacy_suite$results[[category]]
    
    for (test_name in names(cat_results)) {
      test <- cat_results[[test_name]]
      
      # Skip if not a proper test result
      if (!is.list(test) || is.null(test$result)) next
      
      # Create unified test result
      unified_result <- test_result(
        test_name = test_name,
        description = test$description %||% test_name,
        result = test$result,
        p_value = test$p_value,
        statistic = test$statistic,
        details = test$details,
        category = category
      )
      
      unified_suite <- add_test_result(unified_suite, unified_result)
    }
  }
  
  return(finalize_suite(unified_suite))
}

# Null operator
`%||%` <- function(a, b) if (is.null(a)) b else a