# Get package directory and set working directory
pkg_dir <- getwd()

# Load required packages
library(testthat)
library(devtools)
library(qiprng)

# Set up log file
log_file <- file.path(pkg_dir, "tests/testthat/test_log.txt")

# Create directory if it doesn't exist
dir.create(dirname(log_file), recursive = TRUE, showWarnings = FALSE)

# Redirect output to both console and file
sink(log_file, split=TRUE)

# Run tests with increased failure limit
options(testthat.progress.max_fails = 1000)
test_results <- devtools::test(pkg_dir, reporter = "progress")

# Close the output redirection
sink()

# Print summary
cat("\nTest Summary:\n")
if (!is.null(test_results)) {
  cat("Total:", sum(unlist(test_results)), "\n")
  cat("Passed:", test_results$success, "\n")
  cat("Failed:", test_results$failure, "\n")
  cat("Warnings:", test_results$warning, "\n")
  cat("Skipped:", test_results$skip, "\n")
} else {
  cat("No test results available\n")
}
