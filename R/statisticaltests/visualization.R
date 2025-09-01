# File: visualization.R
# ----------------------------------------------------------------------
#' Combined visualization functions for PRNG testing
#'
#' This module provides functions for creating integrated visualizations
#' and comprehensive summary reports across different PRNG test categories.
#' Unlike the category-specific visualization modules, this module focuses
#' on creating combined views that help evaluate the overall quality of
#' random number generators.
#'
#' Features include:
#' \itemize{
#'   \item Summary visualizations across all test categories
#'   \item Interactive HTML reports with drill-down capabilities
#'   \item Configurable dashboard generation
#'   \item Test result tables with statistics
#'   \item Comparative visualizations between different PRNGs
#' }
#'
#' The visualizations integrate the results from all test categories into
#' a cohesive report that can be used for quality assessment and documentation.
#'
#' @name visualization_combined
#' @aliases visualization-combined
#' @keywords internal

#' Generate all visualizations for a test suite
#'
#' Creates comprehensive visualizations for all test results in a PRNG test suite.
#' This function generates summary charts, detailed test visualizations, and HTML tables
#' of test results that can be used for quality assessment and reporting.
#'
#' @param suite The test suite object with results from run_prng_test_suite
#' @return Updated test suite with visualization paths added under suite$visualizations
#' @examples
#' \dontrun{
#' # Create and run a test suite for qiprng
#' createPRNG() # Initialize with defaults
#' suite <- create_prng_test_suite(function(n) generatePRNG(n))
#' suite <- run_prng_test_suite(suite)
#'
#' # Generate visualizations
#' suite <- visualize_all_tests(suite)
#'
#' # Visualization paths are now available in the suite object
#' print(names(suite$visualizations))
#' }
#' @export
visualize_all_tests <- function(suite) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    warning("ggplot2 package is required for visualizations")
    return(suite)
  }

  # Ensure visualization directory exists
  output_dir <- file.path(suite$config$output_dir, "visualizations")
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # To avoid R CMD check NOTEs
  Category <- Test <- PassFail <- Value <- NULL

  # 1. Create a summary visualization of all test results
  if (length(suite$results) > 0) {
    # Extract test results from each category
    test_summary <- data.frame(
      Category = character(),
      Test = character(),
      PassFail = character(),
      stringsAsFactors = FALSE
    )

    for (category in names(suite$results)) {
      for (test_name in names(suite$results[[category]])) {
        test <- suite$results[[category]][[test_name]]
        if (!is.null(test$result)) {
          test_summary <- rbind(test_summary, data.frame(
            Category = category,
            Test = test$description,
            PassFail = test$result,
            stringsAsFactors = FALSE
          ))
        }
      }
    }

    if (nrow(test_summary) > 0) {
      # Create summary plot
      p1 <- ggplot2::ggplot(test_summary, ggplot2::aes(x = Category, fill = PassFail)) +
        ggplot2::geom_bar(position = "stack") +
        ggplot2::scale_fill_manual(values = c(
          "PASS" = "green", "FAIL" = "red",
          "SKIPPED" = "gray", "ERROR" = "orange"
        )) +
        ggplot2::theme_minimal() +
        ggplot2::labs(
          title = "Statistical Test Results Summary",
          x = "Test Category",
          y = "Count",
          fill = "Result"
        )

      # Save plot
      summary_path <- file.path(output_dir, "test_results_summary.png")
      ggplot2::ggsave(summary_path, p1, width = 10, height = 6)

      # Store visualization path
      if (is.null(suite$visualizations)) {
        suite$visualizations <- list()
      }
      suite$visualizations$summary <- list(
        results_summary = summary_path
      )
    }
  }

  # 2. Generate a test-by-test overview table
  test_details <- data.frame(
    Category = character(),
    Test = character(),
    Result = character(),
    PValue = numeric(),
    stringsAsFactors = FALSE
  )

  for (category in names(suite$results)) {
    for (test_name in names(suite$results[[category]])) {
      test <- suite$results[[category]][[test_name]]
      if (!is.null(test$result)) {
        p_value <- ifelse(!is.null(test$p_value), test$p_value, NA)
        test_details <- rbind(test_details, data.frame(
          Category = category,
          Test = test$description,
          Result = test$result,
          PValue = p_value,
          stringsAsFactors = FALSE
        ))
      }
    }
  }

  if (nrow(test_details) > 0) {
    # Sort by category and test
    test_details <- test_details[order(test_details$Category, test_details$Test), ]

    # Create an HTML table
    if (requireNamespace("knitr", quietly = TRUE) &&
      requireNamespace("kableExtra", quietly = TRUE)) {
      html_table <- knitr::kable(test_details,
        format = "html",
        caption = "Statistical Test Results"
      ) %>%
        kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

      # Save table to file
      table_path <- file.path(output_dir, "test_results_table.html")
      writeLines(html_table, table_path)

      # Store path
      suite$visualizations$summary$results_table <- table_path
    }
  }

  # 3. Generate dashboard if requested
  if (suite$config$generate_dashboard &&
    requireNamespace("flexdashboard", quietly = TRUE)) {
    dashboard_path <- generate_dashboard(suite)
    if (!is.null(dashboard_path)) {
      suite$visualizations$dashboard <- dashboard_path
    }
  }

  return(suite)
}

#' Generate a comprehensive dashboard report
#'
#' @param suite The test suite object
#' @return Path to the generated dashboard
#' @keywords internal
generate_dashboard <- function(suite) {
  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    warning("rmarkdown package is required for dashboard generation")
    return(NULL)
  }

  # Create dashboard directory
  dashboard_dir <- file.path(suite$config$output_directory, "dashboard")
  if (!dir.exists(dashboard_dir)) {
    dir.create(dashboard_dir, recursive = TRUE)
  }

  # Create Rmd content for dashboard
  dashboard_rmd <- file.path(dashboard_dir, "prng_test_dashboard.Rmd")

  # Dashboard header
  header <- '---
title: "PRNG Statistical Test Results"
output:
  flexdashboard::flex_dashboard:
    orientation: rows
    theme: cosmo
---

```{r setup, include=FALSE}
library(flexdashboard)
library(ggplot2)
library(DT)
```

'

  # Dashboard content
  content <- "
Overview
=====================================

Row
-------------------------------------

### Test Summary

```{r}
test_summary <- data.frame(
  Category = character(),
  Test = character(),
  Result = character(),
  PValue = numeric(),
  stringsAsFactors = FALSE
)

"

  # Add code to load test results
  result_loading <- ""
  for (category in names(suite$results)) {
    for (test_name in names(suite$results[[category]])) {
      test <- suite$results[[category]][[test_name]]
      if (!is.null(test$result)) {
        p_value <- ifelse(!is.null(test$p_value), test$p_value, "NA")
        result_loading <- paste0(result_loading, "test_summary <- rbind(test_summary, data.frame(
          Category = '", category, "',
          Test = '", test$description, "',
          Result = '", test$result, "',
          PValue = ", p_value, ",
          stringsAsFactors = FALSE
        ))\n")
      }
    }
  }

  content <- paste0(content, result_loading, "
# Show interactive table of results
DT::datatable(test_summary,
              options = list(pageLength = 25),
              rownames = FALSE)
```

Row
-------------------------------------

### Results by Category

```{r}
# Create summary plot by category
ggplot(test_summary, aes(x = Category, fill = Result)) +
  geom_bar(position = 'stack') +
  scale_fill_manual(values = c('PASS' = 'green', 'FAIL' = 'red',
                               'SKIPPED' = 'gray', 'ERROR' = 'orange')) +
  theme_minimal() +
  labs(
    title = 'Statistical Test Results Summary',
    x = 'Test Category',
    y = 'Count',
    fill = 'Result'
  )
```

### P-Value Distribution

```{r}
# Create p-value histogram
p_value_df <- test_summary[!is.na(test_summary$PValue), ]
if(nrow(p_value_df) > 0) {
  ggplot(p_value_df, aes(x = PValue)) +
    geom_histogram(bins = 20, fill = 'steelblue', alpha = 0.7) +
    geom_vline(xintercept = 0.05, linetype = 'dashed', color = 'red') +
    theme_minimal() +
    labs(
      title = 'P-Value Distribution',
      subtitle = 'Red line indicates typical 0.05 significance level',
      x = 'P-Value',
      y = 'Count'
    )
} else {
  plot(1, type = 'n', axes = FALSE, xlab = '', ylab = '')
  text(1, 1, 'No p-values available')
}
```

Visualizations
=====================================

")

  # Add tabs for each category of visualizations
  if (!is.null(suite$visualizations)) {
    for (category in names(suite$visualizations)) {
      if (category != "summary" && category != "dashboard") {
        content <- paste0(content, "
Row {.tabset}
-------------------------------------

### ", toupper(substr(category, 1, 1)), substr(category, 2, nchar(category)), " Tests\n")

        for (viz_name in names(suite$visualizations[[category]])) {
          viz_path <- suite$visualizations[[category]][[viz_name]]
          # Replace absolute path with relative path for dashboard
          rel_path <- gsub(dashboard_dir, ".", viz_path)

          content <- paste0(content, "
#### ", gsub("_", " ", viz_name), "

![", gsub("_", " ", viz_name), "](", rel_path, ")\n")
        }
      }
    }
  }

  # Write to file
  writeLines(paste0(header, content), dashboard_rmd)

  # Render the dashboard
  output_file <- file.path(dashboard_dir, "prng_test_dashboard.html")
  tryCatch(
    {
      rmarkdown::render(dashboard_rmd, output_file = output_file)
      return(output_file)
    },
    error = function(e) {
      warning(paste("Failed to render dashboard:", e$message))
      return(NULL)
    }
  )
}

#' Create a combined test report in PDF or HTML format
#'
#' Generates a comprehensive statistical report from test results in PDF or HTML format.
#' The report includes summary tables, visualizations, and detailed test results with
#' statistics and p-values. This is useful for documenting PRNG quality and
#' sharing results with others.
#'
#' @param suite The test suite object with results from run_prng_test_suite
#' @param format Output format, either "pdf" or "html"
#' @return Path to the generated report file
#' @examples
#' \dontrun{
#' # Create and run a test suite
#' createPRNG()
#' suite <- create_prng_test_suite(function(n) generatePRNG(n))
#' suite <- run_prng_test_suite(suite)
#'
#' # Generate visualizations first
#' suite <- visualize_all_tests(suite)
#'
#' # Generate an HTML report
#' report_path <- generate_report(suite, format = "html")
#'
#' # Open the report
#' browseURL(report_path)
#' }
#' @export
generate_report <- function(suite, format = "html") {
  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    warning("rmarkdown package is required for report generation")
    return(NULL)
  }

  # Create report directory
  report_dir <- file.path(suite$config$output_directory, "report")
  if (!dir.exists(report_dir)) {
    dir.create(report_dir, recursive = TRUE)
  }

  # Create Rmd content for report
  report_rmd <- file.path(report_dir, "prng_test_report.Rmd")

  # Report header
  header <- paste0(
    '---
title: "PRNG Statistical Test Report"
author: "Generated by qiprng package"
date: "', format(Sys.time(), "%Y-%m-%d %H:%M:%S"), '"
output: ',
    ifelse(format == "pdf", "pdf_document", "html_document"),
    "
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)
library(ggplot2)
library(knitr)
```

# PRNG Test Report

This report presents the results of statistical tests conducted on a pseudo-random number generator.

## Test Configuration

- Sample size: ", suite$config$sample_size, "
- Significance level: ", suite$config$significance_level, "
- PRNG type: ", ifelse(!is.null(suite$config$prng_type), suite$config$prng_type, "Custom"), "

"
  )

  # Report content - Overview of test results
  content <- "
## Test Results Summary

```{r}
test_summary <- data.frame(
  Category = character(),
  Test = character(),
  Result = character(),
  PValue = numeric(),
  stringsAsFactors = FALSE
)

"

  # Add code to load test results
  result_loading <- ""
  for (category in names(suite$results)) {
    for (test_name in names(suite$results[[category]])) {
      test <- suite$results[[category]][[test_name]]
      if (!is.null(test$result)) {
        p_value <- ifelse(!is.null(test$p_value), paste0(test$p_value), "NA")
        result_loading <- paste0(result_loading, "test_summary <- rbind(test_summary, data.frame(
          Category = '", category, "',
          Test = '", test$description, "',
          Result = '", test$result, "',
          PValue = ", p_value, ",
          stringsAsFactors = FALSE
        ))\n")
      }
    }
  }

  content <- paste0(content, result_loading, "
# Create summary table
kable(test_summary,
      caption = 'Statistical Test Results',
      align = c('l', 'l', 'c', 'r'))
```

### Results Visualization

```{r fig.width=10, fig.height=6}
# Create summary plot by category
ggplot(test_summary, aes(x = Category, fill = Result)) +
  geom_bar(position = 'stack') +
  scale_fill_manual(values = c('PASS' = 'green', 'FAIL' = 'red',
                               'SKIPPED' = 'gray', 'ERROR' = 'orange')) +
  theme_minimal() +
  labs(
    title = 'Statistical Test Results Summary',
    x = 'Test Category',
    y = 'Count',
    fill = 'Result'
  )
```

### P-Value Distribution

```{r fig.width=8, fig.height=5}
# Create p-value histogram
p_value_df <- test_summary[!is.na(test_summary$PValue), ]
if(nrow(p_value_df) > 0) {
  ggplot(p_value_df, aes(x = PValue)) +
    geom_histogram(bins = 20, fill = 'steelblue', alpha = 0.7) +
    geom_vline(xintercept = 0.05, linetype = 'dashed', color = 'red') +
    theme_minimal() +
    labs(
      title = 'P-Value Distribution',
      subtitle = 'Red line indicates typical 0.05 significance level',
      x = 'P-Value',
      y = 'Count'
    )
}
```

## Detailed Test Results

")

  # Add detailed sections for each category
  for (category in names(suite$results)) {
    content <- paste0(content, "
### ", toupper(substr(category, 1, 1)), substr(category, 2, nchar(category)), " Tests\n\n")

    for (test_name in names(suite$results[[category]])) {
      test <- suite$results[[category]][[test_name]]
      if (!is.null(test$result)) {
        content <- paste0(content, "#### ", test$description, "\n\n")
        content <- paste0(content, "- **Result:** ", test$result, "\n")

        if (!is.null(test$p_value)) {
          content <- paste0(content, "- **P-value:** ", round(test$p_value, 4), "\n")
        }

        if (!is.null(test$statistic)) {
          content <- paste0(content, "- **Test statistic:** ", round(test$statistic, 4), "\n")
        }

        if (!is.null(test$details)) {
          content <- paste0(content, "- **Details:** ", test$details, "\n")
        }

        content <- paste0(content, "\n")
      }
    }

    # Add category visualizations if available
    if (!is.null(suite$visualizations) && !is.null(suite$visualizations[[category]])) {
      content <- paste0(content, "#### Visualizations\n\n")

      for (viz_name in names(suite$visualizations[[category]])) {
        viz_path <- suite$visualizations[[category]][[viz_name]]
        # Replace absolute path with relative path for report
        rel_path <- gsub(report_dir, ".", viz_path)

        content <- paste0(content, "![", gsub("_", " ", viz_name), "](", rel_path, ")\n\n")
      }
    }
  }

  # Write to file
  writeLines(paste0(header, content), report_rmd)

  # Render the report
  output_file <- file.path(report_dir, paste0("prng_test_report.", ifelse(format == "pdf", "pdf", "html")))
  tryCatch(
    {
      rmarkdown::render(report_rmd, output_file = output_file)
      return(output_file)
    },
    error = function(e) {
      warning(paste("Failed to render report:", e$message))
      return(NULL)
    }
  )
}
