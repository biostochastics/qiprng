# File: validation_reporting.R
# ----------------------------------------------------------------------
#' Comprehensive Validation Reporting Module
#'
#' This module provides advanced reporting functionality for the validation
#' suite, including HTML reports, PDF generation, and data export capabilities.
#'
#' @name validation_reporting

#' Generate comprehensive validation report
#'
#' Creates a detailed report from validation results in multiple formats
#'
#' @param validation_report The validation report object from validate_qiprng_framework
#' @param output_dir Directory to save report files
#' @param formats Character vector of output formats: "html", "pdf", "json", "csv"
#' @param include_visualizations Whether to include visualization plots
#' @param verbose Print progress messages
#' @return List of generated file paths
#' @export
generate_validation_report <- function(validation_report,
                                     output_dir = "validation_reports",
                                     formats = c("html", "json"),
                                     include_visualizations = TRUE,
                                     verbose = TRUE) {
  
  # Create output directory
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Generate timestamp for filenames
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  
  # List to store generated file paths
  generated_files <- list()
  
  # Generate HTML report
  if ("html" %in% formats) {
    if (verbose) cat("Generating HTML report...\n")
    html_file <- file.path(output_dir, 
                          paste0("validation_report_", timestamp, ".html"))
    generate_html_report(validation_report, html_file, include_visualizations)
    generated_files$html <- html_file
  }
  
  # Generate PDF report (requires pandoc or rmarkdown)
  if ("pdf" %in% formats) {
    if (verbose) cat("Generating PDF report...\n")
    pdf_file <- file.path(output_dir, 
                         paste0("validation_report_", timestamp, ".pdf"))
    if (requireNamespace("rmarkdown", quietly = TRUE)) {
      generate_pdf_report(validation_report, pdf_file, include_visualizations)
      generated_files$pdf <- pdf_file
    } else {
      warning("PDF generation requires rmarkdown package")
    }
  }
  
  # Generate JSON report
  if ("json" %in% formats) {
    if (verbose) cat("Generating JSON report...\n")
    json_file <- file.path(output_dir, 
                          paste0("validation_report_", timestamp, ".json"))
    generate_json_report(validation_report, json_file)
    generated_files$json <- json_file
  }
  
  # Generate CSV summary
  if ("csv" %in% formats) {
    if (verbose) cat("Generating CSV summary...\n")
    csv_file <- file.path(output_dir, 
                         paste0("validation_summary_", timestamp, ".csv"))
    generate_csv_summary(validation_report, csv_file)
    generated_files$csv <- csv_file
  }
  
  if (verbose) {
    cat("\nReports generated:\n")
    for (format in names(generated_files)) {
      cat(sprintf("  %s: %s\n", format, generated_files[[format]]))
    }
  }
  
  return(generated_files)
}

#' Generate HTML validation report
#'
#' @param report Validation report object
#' @param output_file Output HTML file path
#' @param include_visualizations Whether to include plots
generate_html_report <- function(report, output_file, include_visualizations = TRUE) {
  
  # Start HTML document
  html <- c(
    '<!DOCTYPE html>',
    '<html lang="en">',
    '<head>',
    '  <meta charset="UTF-8">',
    '  <meta name="viewport" content="width=device-width, initial-scale=1.0">',
    '  <title>QIPRNG Validation Report</title>',
    generate_html_styles(),
    '  <script src="https://cdn.jsdelivr.net/npm/chart.js"></script>',
    '</head>',
    '<body>',
    '  <div class="container">'
  )
  
  # Add header
  html <- c(html, generate_html_header(report))
  
  # Add executive summary
  html <- c(html, generate_html_summary(report))
  
  # Add detailed results by category
  html <- c(html, generate_html_category_results(report))
  
  # Add edge case results if available
  if (!is.null(report$edge_cases)) {
    html <- c(html, generate_html_edge_cases(report$edge_cases))
  }
  
  # Add performance results if available
  if (!is.null(report$performance_tests)) {
    html <- c(html, generate_html_performance(report$performance_tests))
  }
  
  # Add visualizations if requested
  if (include_visualizations && !is.null(report$visualizations)) {
    html <- c(html, generate_html_visualizations(report$visualizations))
  }
  
  # Add recommendations
  html <- c(html, generate_html_recommendations(report$recommendations))
  
  # Add footer
  html <- c(html, generate_html_footer())
  
  # Close HTML
  html <- c(html,
    '  </div>',
    generate_html_scripts(),
    '</body>',
    '</html>'
  )
  
  # Write to file
  writeLines(html, output_file)
}

#' Generate HTML styles
generate_html_styles <- function() {
  c(
    '  <style>',
    '    * { box-sizing: border-box; margin: 0; padding: 0; }',
    '    body {',
    '      font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif;',
    '      line-height: 1.6;',
    '      color: #333;',
    '      background-color: #f5f5f5;',
    '    }',
    '    .container {',
    '      max-width: 1200px;',
    '      margin: 0 auto;',
    '      padding: 20px;',
    '      background-color: white;',
    '      box-shadow: 0 0 10px rgba(0,0,0,0.1);',
    '    }',
    '    h1, h2, h3 { margin-bottom: 1em; }',
    '    h1 { color: #2c3e50; border-bottom: 3px solid #3498db; padding-bottom: 10px; }',
    '    h2 { color: #34495e; margin-top: 2em; }',
    '    h3 { color: #7f8c8d; margin-top: 1.5em; }',
    '    .summary-box {',
    '      background-color: #ecf0f1;',
    '      padding: 20px;',
    '      border-radius: 8px;',
    '      margin: 20px 0;',
    '    }',
    '    .metric {',
    '      display: inline-block;',
    '      margin: 10px 20px 10px 0;',
    '      padding: 10px 15px;',
    '      background-color: white;',
    '      border-radius: 5px;',
    '      box-shadow: 0 2px 5px rgba(0,0,0,0.1);',
    '    }',
    '    .metric-value {',
    '      font-size: 2em;',
    '      font-weight: bold;',
    '      display: block;',
    '    }',
    '    .metric-label {',
    '      font-size: 0.9em;',
    '      color: #7f8c8d;',
    '    }',
    '    .pass { color: #27ae60; }',
    '    .fail { color: #e74c3c; }',
    '    .warning { color: #f39c12; }',
    '    .inconclusive { color: #95a5a6; }',
    '    table {',
    '      width: 100%;',
    '      border-collapse: collapse;',
    '      margin: 20px 0;',
    '      background-color: white;',
    '    }',
    '    th, td {',
    '      padding: 12px;',
    '      text-align: left;',
    '      border-bottom: 1px solid #ddd;',
    '    }',
    '    th {',
    '      background-color: #34495e;',
    '      color: white;',
    '      font-weight: bold;',
    '    }',
    '    tr:hover { background-color: #f5f5f5; }',
    '    .progress-bar {',
    '      width: 100%;',
    '      height: 30px;',
    '      background-color: #ecf0f1;',
    '      border-radius: 15px;',
    '      overflow: hidden;',
    '      margin: 10px 0;',
    '    }',
    '    .progress-fill {',
    '      height: 100%;',
    '      background-color: #3498db;',
    '      text-align: center;',
    '      line-height: 30px;',
    '      color: white;',
    '      font-weight: bold;',
    '      transition: width 0.3s ease;',
    '    }',
    '    .chart-container {',
    '      position: relative;',
    '      height: 400px;',
    '      margin: 20px 0;',
    '    }',
    '    .recommendation {',
    '      background-color: #fff3cd;',
    '      border-left: 4px solid #ffc107;',
    '      padding: 10px 15px;',
    '      margin: 10px 0;',
    '    }',
    '    .timestamp {',
    '      color: #7f8c8d;',
    '      font-size: 0.9em;',
    '      margin-top: 20px;',
    '    }',
    '    .badge {',
    '      display: inline-block;',
    '      padding: 3px 8px;',
    '      border-radius: 3px;',
    '      font-size: 0.85em;',
    '      font-weight: bold;',
    '      margin-left: 10px;',
    '    }',
    '    .badge-pass { background-color: #d4edda; color: #155724; }',
    '    .badge-fail { background-color: #f8d7da; color: #721c24; }',
    '    .badge-warning { background-color: #fff3cd; color: #856404; }',
    '  </style>'
  )
}

#' Generate HTML header section
generate_html_header <- function(report) {
  c(
    '    <header>',
    '      <h1>QIPRNG Validation Report</h1>',
    sprintf('      <p class="timestamp">Generated: %s</p>', report$timestamp),
    sprintf('      <p>Validation Level: <strong>%s</strong></p>', 
            tools::toTitleCase(report$level)),
    sprintf('      <p>Execution Time: <strong>%s</strong></p>', 
            format(report$performance$execution_time)),
    '    </header>'
  )
}

#' Generate HTML summary section
generate_html_summary <- function(report) {
  pass_rate <- 100 * report$summary$passed / report$summary$total_tests
  
  c(
    '    <section class="summary-box">',
    '      <h2>Executive Summary</h2>',
    '      <div class="metrics">',
    sprintf('        <div class="metric">
                       <span class="metric-value">%d</span>
                       <span class="metric-label">Total Tests</span>
                     </div>', report$summary$total_tests),
    sprintf('        <div class="metric">
                       <span class="metric-value pass">%d</span>
                       <span class="metric-label">Passed</span>
                     </div>', report$summary$passed),
    sprintf('        <div class="metric">
                       <span class="metric-value fail">%d</span>
                       <span class="metric-label">Failed</span>
                     </div>', report$summary$failed),
    sprintf('        <div class="metric">
                       <span class="metric-value warning">%d</span>
                       <span class="metric-label">Warnings</span>
                     </div>', report$summary$warnings),
    '      </div>',
    '      <div class="progress-bar">',
    sprintf('        <div class="progress-fill" style="width: %.1f%%">%.1f%% Pass Rate</div>',
            pass_rate, pass_rate),
    '      </div>',
    '    </section>'
  )
}

#' Generate HTML category results
generate_html_category_results <- function(report) {
  html <- c('    <section>', '      <h2>Test Results by Category</h2>')
  
  # Create summary table
  html <- c(html,
    '      <table>',
    '        <thead>',
    '          <tr>',
    '            <th>Category</th>',
    '            <th>Total</th>',
    '            <th>Passed</th>',
    '            <th>Failed</th>',
    '            <th>Warnings</th>',
    '            <th>Pass Rate</th>',
    '            <th>Status</th>',
    '          </tr>',
    '        </thead>',
    '        <tbody>'
  )
  
  for (category in names(report$categories)) {
    cat_data <- report$categories[[category]]
    pass_rate <- 100 * cat_data$passed / max(cat_data$total_tests, 1)
    
    # Determine status badge
    if (cat_data$failed > 0) {
      status <- '<span class="badge badge-fail">FAIL</span>'
    } else if (cat_data$warnings > 0) {
      status <- '<span class="badge badge-warning">WARNING</span>'
    } else {
      status <- '<span class="badge badge-pass">PASS</span>'
    }
    
    html <- c(html,
      '          <tr>',
      sprintf('            <td><strong>%s</strong></td>', 
              tools::toTitleCase(gsub("_", " ", category))),
      sprintf('            <td>%d</td>', cat_data$total_tests),
      sprintf('            <td class="pass">%d</td>', cat_data$passed),
      sprintf('            <td class="fail">%d</td>', cat_data$failed),
      sprintf('            <td class="warning">%d</td>', cat_data$warnings),
      sprintf('            <td>%.1f%%</td>', pass_rate),
      sprintf('            <td>%s</td>', status),
      '          </tr>'
    )
  }
  
  html <- c(html,
    '        </tbody>',
    '      </table>',
    '    </section>'
  )
  
  # Add detailed results for each category
  for (category in names(report$categories)) {
    if (!is.null(report$categories[[category]]$details) && 
        length(report$categories[[category]]$details) > 0) {
      html <- c(html,
        sprintf('    <section>'),
        sprintf('      <h3>%s - Detailed Results</h3>', 
                tools::toTitleCase(gsub("_", " ", category))),
        '      <ul>'
      )
      
      for (detail_name in names(report$categories[[category]]$details)) {
        html <- c(html,
          sprintf('        <li><strong>%s:</strong> %s</li>',
                  detail_name, report$categories[[category]]$details[[detail_name]])
        )
      }
      
      html <- c(html, '      </ul>', '    </section>')
    }
  }
  
  return(html)
}

#' Generate HTML edge case results
generate_html_edge_cases <- function(edge_results) {
  html <- c(
    '    <section>',
    '      <h2>Edge Case Testing</h2>'
  )
  
  # Summary metrics
  if (!is.null(edge_results$total_tests)) {
    pass_rate <- 100 * edge_results$passed / max(edge_results$total_tests, 1)
    
    html <- c(html,
      '      <div class="summary-box">',
      sprintf('        <p>Total edge case tests: <strong>%d</strong></p>', 
              edge_results$total_tests),
      sprintf('        <p>Passed: <strong class="pass">%d</strong> (%.1f%%)</p>', 
              edge_results$passed, pass_rate),
      sprintf('        <p>Failed: <strong class="fail">%d</strong></p>', 
              edge_results$failed),
      '      </div>'
    )
  }
  
  # Category breakdown if available
  if (!is.null(edge_results$categories)) {
    html <- c(html,
      '      <h3>Edge Case Categories</h3>',
      '      <table>',
      '        <thead>',
      '          <tr>',
      '            <th>Category</th>',
      '            <th>Tests</th>',
      '            <th>Passed</th>',
      '            <th>Failed</th>',
      '          </tr>',
      '        </thead>',
      '        <tbody>'
    )
    
    for (cat_name in names(edge_results$categories)) {
      cat_data <- edge_results$categories[[cat_name]]
      html <- c(html,
        '          <tr>',
        sprintf('            <td>%s</td>', 
                tools::toTitleCase(gsub("_", " ", cat_name))),
        sprintf('            <td>%d</td>', cat_data$total_tests),
        sprintf('            <td class="pass">%d</td>', cat_data$passed),
        sprintf('            <td class="fail">%d</td>', cat_data$failed),
        '          </tr>'
      )
    }
    
    html <- c(html, '        </tbody>', '      </table>')
  }
  
  html <- c(html, '    </section>')
  return(html)
}

#' Generate HTML performance results
generate_html_performance <- function(perf_results) {
  html <- c(
    '    <section>',
    '      <h2>Performance Benchmarks</h2>'
  )
  
  if (!is.null(perf_results$timings)) {
    html <- c(html,
      '      <h3>Execution Times</h3>',
      '      <div class="chart-container">',
      '        <canvas id="performanceChart"></canvas>',
      '      </div>',
      '      <table>',
      '        <thead>',
      '          <tr>',
      '            <th>Test</th>',
      '            <th>Sample Size</th>',
      '            <th>Execution Time (s)</th>',
      '            <th>Throughput (samples/s)</th>',
      '          </tr>',
      '        </thead>',
      '        <tbody>'
    )
    
    for (test_name in names(perf_results$timings)) {
      exec_time <- perf_results$timings[[test_name]]
      # Extract sample size from test name
      n <- as.numeric(gsub(".*_n", "", test_name))
      throughput <- n / exec_time
      
      html <- c(html,
        '          <tr>',
        sprintf('            <td>%s</td>', test_name),
        sprintf('            <td>%s</td>', format(n, big.mark = ",")),
        sprintf('            <td>%.3f</td>', exec_time),
        sprintf('            <td>%s</td>', format(round(throughput), big.mark = ",")),
        '          </tr>'
      )
    }
    
    html <- c(html, '        </tbody>', '      </table>')
  }
  
  html <- c(html, '    </section>')
  return(html)
}

#' Generate HTML recommendations
generate_html_recommendations <- function(recommendations) {
  html <- c(
    '    <section>',
    '      <h2>Recommendations</h2>'
  )
  
  if (length(recommendations) > 0) {
    for (rec in recommendations) {
      html <- c(html,
        sprintf('      <div class="recommendation">%s</div>', rec)
      )
    }
  } else {
    html <- c(html,
      '      <div class="recommendation">',
      '        All tests passed successfully. No issues found.',
      '      </div>'
    )
  }
  
  html <- c(html, '    </section>')
  return(html)
}

#' Generate HTML footer
generate_html_footer <- function() {
  c(
    '    <footer>',
    '      <hr style="margin-top: 40px;">',
    '      <p class="timestamp">',
    '        Generated by QIPRNG Validation Suite',
    sprintf('        | R %s.%s', R.version$major, R.version$minor),
    '      </p>',
    '    </footer>'
  )
}

#' Generate HTML scripts
generate_html_scripts <- function() {
  c(
    '  <script>',
    '    // Performance chart',
    '    const perfCanvas = document.getElementById("performanceChart");',
    '    if (perfCanvas) {',
    '      const ctx = perfCanvas.getContext("2d");',
    '      // Chart configuration would go here',
    '    }',
    '  </script>'
  )
}

#' Generate JSON report
#'
#' @param report Validation report object  
#' @param output_file Output JSON file path
generate_json_report <- function(report, output_file) {
  # Convert report to JSON
  json_data <- jsonlite::toJSON(report, pretty = TRUE, auto_unbox = TRUE)
  
  # Write to file
  writeLines(json_data, output_file)
}

#' Generate CSV summary
#'
#' @param report Validation report object
#' @param output_file Output CSV file path  
generate_csv_summary <- function(report, output_file) {
  # Create summary data frame
  summary_df <- data.frame(
    timestamp = as.character(report$timestamp),
    level = report$level,
    total_tests = report$summary$total_tests,
    passed = report$summary$passed,
    failed = report$summary$failed,
    warnings = report$summary$warnings,
    pass_rate = report$summary$passed / report$summary$total_tests,
    execution_time = as.numeric(report$performance$execution_time),
    stringsAsFactors = FALSE
  )
  
  # Add category summaries
  for (category in names(report$categories)) {
    cat_data <- report$categories[[category]]
    summary_df[[paste0(category, "_total")]] <- cat_data$total_tests
    summary_df[[paste0(category, "_passed")]] <- cat_data$passed
    summary_df[[paste0(category, "_failed")]] <- cat_data$failed
  }
  
  # Write to CSV
  write.csv(summary_df, output_file, row.names = FALSE)
}

#' Generate PDF report using R Markdown
#'
#' @param report Validation report object
#' @param output_file Output PDF file path
#' @param include_visualizations Whether to include plots
generate_pdf_report <- function(report, output_file, include_visualizations = TRUE) {
  # Create temporary Rmd file
  rmd_file <- tempfile(fileext = ".Rmd")
  
  # Generate R Markdown content
  rmd_content <- generate_rmarkdown_content(report, include_visualizations)
  
  # Write Rmd file
  writeLines(rmd_content, rmd_file)
  
  # Render to PDF
  tryCatch({
    rmarkdown::render(rmd_file, 
                     output_format = "pdf_document",
                     output_file = output_file,
                     quiet = TRUE)
  }, error = function(e) {
    warning("PDF generation failed: ", e$message)
  })
  
  # Clean up
  unlink(rmd_file)
}

#' Generate R Markdown content for PDF report
generate_rmarkdown_content <- function(report, include_visualizations) {
  c(
    '---',
    'title: "QIPRNG Validation Report"',
    sprintf('date: "%s"', format(report$timestamp, "%B %d, %Y")),
    'output:',
    '  pdf_document:',
    '    toc: true',
    '    toc_depth: 3',
    '---',
    '',
    '```{r setup, include=FALSE}',
    'knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)',
    'library(ggplot2)',
    'library(knitr)',
    '```',
    '',
    '# Executive Summary',
    '',
    sprintf('This validation was performed at level **%s** and took **%s** to complete.',
            report$level, format(report$performance$execution_time)),
    '',
    '## Overall Results',
    '',
    sprintf('- Total tests run: %d', report$summary$total_tests),
    sprintf('- Tests passed: %d (%.1f%%)', 
            report$summary$passed, 
            100 * report$summary$passed / report$summary$total_tests),
    sprintf('- Tests failed: %d', report$summary$failed),
    sprintf('- Warnings: %d', report$summary$warnings),
    '',
    '# Detailed Results',
    '',
    generate_rmd_category_section(report),
    '',
    '# Recommendations',
    '',
    generate_rmd_recommendations(report$recommendations)
  )
}

#' Generate R Markdown category section
generate_rmd_category_section <- function(report) {
  sections <- character()
  
  for (category in names(report$categories)) {
    cat_data <- report$categories[[category]]
    
    sections <- c(sections,
      sprintf('## %s', tools::toTitleCase(gsub("_", " ", category))),
      '',
      sprintf('- Total tests: %d', cat_data$total_tests),
      sprintf('- Passed: %d', cat_data$passed),
      sprintf('- Failed: %d', cat_data$failed),
      sprintf('- Pass rate: %.1f%%', 
              100 * cat_data$passed / max(cat_data$total_tests, 1)),
      ''
    )
    
    if (length(cat_data$details) > 0) {
      sections <- c(sections, '### Details', '')
      for (detail_name in names(cat_data$details)) {
        sections <- c(sections,
          sprintf('- **%s**: %s', detail_name, cat_data$details[[detail_name]])
        )
      }
      sections <- c(sections, '')
    }
  }
  
  return(sections)
}

#' Generate R Markdown recommendations
generate_rmd_recommendations <- function(recommendations) {
  if (length(recommendations) > 0) {
    return(paste("- ", recommendations, collapse = "\n"))
  } else {
    return("All validations passed successfully. No issues found.")
  }
}