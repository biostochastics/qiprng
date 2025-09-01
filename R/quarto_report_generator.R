# Quarto Report Generation Module
# Enhanced reporting with validation and iteration capabilities

library(quarto)

#' Generate and render comprehensive Quarto report
#'
#' @param results List of test results from run_discriminant_analysis
#' @param output_file Output filename (without extension)
#' @param validate_results Whether to perform validation checks
#' @return List with report status and validation results
generate_quarto_report <- function(results, output_file = "discriminant_analysis_report", validate_results = TRUE) {
  cat("=== QUARTO REPORT GENERATION ===\n")

  # Step 1: Validate input results
  if (validate_results) {
    cat("Step 1: Validating analysis results...\n")
    validation <- validate_analysis_results(results)

    if (!validation$valid) {
      stop("Validation failed: ", paste(validation$errors, collapse = "; "))
    }

    cat("✓ Validation passed:", validation$summary, "\n")
  }

  # Step 2: Prepare data for report
  cat("Step 2: Preparing data for Quarto report...\n")
  summary_data <- generate_summary_stats(results)

  # Save results for the Quarto document to load
  saveRDS(results, "full_analysis_results.rds")
  cat("✓ Results saved to full_analysis_results.rds\n")

  # Step 3: Render the Quarto document
  cat("Step 3: Rendering Quarto document...\n")

  tryCatch(
    {
      # Check if Quarto is available
      if (!quarto::quarto_version() == "") {
        cat("✓ Quarto version:", quarto::quarto_version(), "\n")
      }

      # Render HTML version
      html_file <- paste0(output_file, ".html")
      quarto::quarto_render("discriminant_analysis_report.qmd",
        output_file = html_file,
        output_format = "html"
      )
      cat("✓ HTML report generated:", html_file, "\n")

      # Render PDF version if possible
      tryCatch(
        {
          pdf_file <- paste0(output_file, ".pdf")
          quarto::quarto_render("discriminant_analysis_report.qmd",
            output_file = pdf_file,
            output_format = "pdf"
          )
          cat("✓ PDF report generated:", pdf_file, "\n")
        },
        error = function(e) {
          cat("⚠ PDF generation failed (LaTeX may not be available):", e$message, "\n")
        }
      )
    },
    error = function(e) {
      cat("✗ Quarto rendering failed:", e$message, "\n")
      cat("Falling back to R Markdown rendering...\n")

      # Fallback: Convert to Rmd and render
      convert_qmd_to_rmd("discriminant_analysis_report.qmd")
      rmarkdown::render("discriminant_analysis_report.Rmd",
        output_file = paste0(output_file, ".html")
      )
    }
  )

  # Step 4: Generate additional outputs
  cat("Step 4: Generating supplementary outputs...\n")

  # Export summary CSV
  write.csv(summary_data, "discriminant_analysis_summary.csv", row.names = FALSE)
  cat("✓ Summary CSV exported\n")

  # Create visualizations directory
  if (!dir.exists("visualizations")) {
    dir.create("visualizations")
  }

  # Generate and save key visualizations
  plots <- create_visualizations(summary_data)
  ggsave("visualizations/quality_distribution.png", plots$score_dist, width = 10, height = 6, dpi = 300)
  ggsave("visualizations/test_pass_rates.png", plots$test_pass_rates, width = 10, height = 6, dpi = 300)
  ggsave("visualizations/parameter_relationships.png", plots$param_relationships, width = 10, height = 6, dpi = 300)
  cat("✓ Key visualizations saved to visualizations/\n")

  # Step 5: Final validation
  cat("Step 5: Final validation and summary...\n")
  final_validation <- validate_outputs(summary_data, results)

  return(list(
    success = TRUE,
    validation = if (validate_results) validation else NULL,
    final_validation = final_validation,
    summary_data = summary_data,
    n_discriminants = nrow(summary_data),
    files_generated = c(
      paste0(output_file, ".html"),
      "discriminant_analysis_summary.csv",
      "full_analysis_results.rds"
    )
  ))
}

#' Validate analysis results for completeness and correctness
#'
#' @param results List of analysis results
#' @return List with validation status and details
validate_analysis_results <- function(results) {
  errors <- c()
  warnings <- c()

  # Check basic structure
  if (!is.list(results) || length(results) == 0) {
    errors <- c(errors, "Results is not a valid list or is empty")
  }

  # Check each result structure
  valid_results <- 0
  error_results <- 0

  for (i in seq_along(results)) {
    result <- results[[i]]

    if (is.null(result$parameters)) {
      errors <- c(errors, paste("Result", i, "missing parameters"))
    }

    if (!is.null(result$error)) {
      error_results <- error_results + 1
    } else {
      valid_results <- valid_results + 1

      # Check test results structure
      required_tests <- c("uniformity", "independence", "autocorrelation", "periodicity", "moments")
      missing_tests <- required_tests[!sapply(required_tests, function(test) !is.null(result[[test]]))]

      if (length(missing_tests) > 0) {
        warnings <- c(warnings, paste("Result", i, "missing tests:", paste(missing_tests, collapse = ", ")))
      }
    }
  }

  # Summary statistics
  total_results <- length(results)
  success_rate <- valid_results / total_results

  if (success_rate < 0.5) {
    errors <- c(errors, paste("Low success rate:", round(success_rate * 100, 1), "%"))
  } else if (success_rate < 0.8) {
    warnings <- c(warnings, paste("Moderate success rate:", round(success_rate * 100, 1), "%"))
  }

  return(list(
    valid = length(errors) == 0,
    errors = errors,
    warnings = warnings,
    summary = paste("Validated", total_results, "results:", valid_results, "successful,", error_results, "errors"),
    success_rate = success_rate,
    total_results = total_results,
    valid_results = valid_results,
    error_results = error_results
  ))
}

#' Validate final outputs for consistency
#'
#' @param summary_data Summary statistics data frame
#' @param results Original results list
#' @return List with validation details
validate_outputs <- function(summary_data, results) {
  checks <- list()

  # Check data consistency
  checks$data_consistency <- nrow(summary_data) == length(results)

  # Check score ranges
  checks$score_range <- all(summary_data$overall_score >= 0 & summary_data$overall_score <= 1, na.rm = TRUE)

  # Check discriminant calculations
  calculated_discriminants <- summary_data$b^2 - 4 * summary_data$a * summary_data$c
  checks$discriminant_accuracy <- all(abs(calculated_discriminants - summary_data$discriminant) < 1e-10, na.rm = TRUE)

  # Check parameter constraints
  checks$parameter_constraints <- all(summary_data$a > 0 & summary_data$c < 0 & summary_data$discriminant > 0, na.rm = TRUE)

  # Summary
  all_passed <- all(unlist(checks))

  return(list(
    all_passed = all_passed,
    checks = checks,
    summary = if (all_passed) "All validation checks passed" else "Some validation checks failed"
  ))
}

#' Convert Quarto document to R Markdown (fallback)
#'
#' @param qmd_file Path to .qmd file
convert_qmd_to_rmd <- function(qmd_file) {
  # Read the Quarto file
  lines <- readLines(qmd_file)

  # Convert Quarto-specific syntax to R Markdown
  # This is a basic conversion - more sophisticated conversion may be needed
  lines <- gsub("```\\{r ([^}]*)\\}", "```{r \\1}", lines)
  lines <- gsub("#\\| ", "", lines) # Remove Quarto chunk options

  # Write R Markdown file
  rmd_file <- gsub("\\.qmd$", ".Rmd", qmd_file)
  writeLines(lines, rmd_file)

  cat("✓ Converted", qmd_file, "to", rmd_file, "\n")
  return(rmd_file)
}

#' Run complete analysis with Quarto reporting
#'
#' @param discriminants_file Path to discriminants CSV
#' @param sample_size Sample size per discriminant
#' @param max_discriminants Maximum discriminants to test (NULL for all)
#' @param output_name Output file name prefix
#' @return Complete analysis results with report
run_complete_analysis_with_quarto <- function(discriminants_file = "discriminants.csv",
                                              sample_size = 10000,
                                              max_discriminants = NULL,
                                              output_name = "discriminant_analysis_report") {
  cat("=== COMPLETE DISCRIMINANT ANALYSIS WITH QUARTO REPORTING ===\n")
  cat("Sample size:", sample_size, "\n")
  cat("Max discriminants:", ifelse(is.null(max_discriminants), "All", max_discriminants), "\n")
  cat("Output name:", output_name, "\n\n")

  # Step 1: Run the statistical analysis
  cat("PHASE 1: Statistical Analysis\n")
  start_time <- Sys.time()

  results <- run_discriminant_analysis(
    discriminants_file = discriminants_file,
    sample_size = sample_size,
    max_discriminants = max_discriminants
  )

  analysis_time <- difftime(Sys.time(), start_time, units = "mins")
  cat("✓ Analysis completed in", round(as.numeric(analysis_time), 2), "minutes\n\n")

  # Step 2: Generate Quarto report
  cat("PHASE 2: Report Generation\n")
  report_result <- generate_quarto_report(results, output_name, validate_results = TRUE)

  # Step 3: Final summary
  cat("\n=== ANALYSIS COMPLETE ===\n")
  cat("Total discriminants analyzed:", report_result$n_discriminants, "\n")
  cat("Files generated:\n")
  for (file in report_result$files_generated) {
    cat("  -", file, "\n")
  }

  if (report_result$final_validation$all_passed) {
    cat("✓ All validation checks passed\n")
  } else {
    cat("⚠ Some validation issues detected - check logs\n")
  }

  cat("\nAnalysis duration:", round(as.numeric(analysis_time), 2), "minutes\n")

  return(list(
    results = results,
    report = report_result,
    analysis_time = analysis_time
  ))
}
