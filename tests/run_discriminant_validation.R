#!/usr/bin/env Rscript

# Script to systematically validate all 750 discriminants
# This can be run as a standalone validation or as part of the test suite

library(qiprng)

# Source the discriminant testing functions
source_paths <- c(
  system.file("R/discriminant_tests.R", package = "qiprng", mustWork = FALSE),
  "R/discriminant_tests.R",
  "../R/discriminant_tests.R"
)

for (path in source_paths) {
  if (file.exists(path)) {
    source(path)
    break
  }
}

# Find discriminants file
discriminants_paths <- c(
  system.file("analysis/data/discriminants.csv", package = "qiprng", mustWork = FALSE),
  "analysis/data/discriminants.csv",
  "../analysis/data/discriminants.csv"
)

discriminants_file <- NULL
for (path in discriminants_paths) {
  if (file.exists(path)) {
    discriminants_file <- path
    break
  }
}

if (is.null(discriminants_file)) {
  stop("Could not find discriminants.csv file")
}

# Parse command line arguments
args <- commandArgs(trailingOnly = TRUE)
sample_size <- ifelse(length(args) > 0, as.numeric(args[1]), 10000)
max_discriminants <- ifelse(length(args) > 1, as.numeric(args[2]), NULL)
n_cores <- ifelse(length(args) > 2, as.numeric(args[3]), parallel::detectCores() - 1)
output_file <- ifelse(length(args) > 3, args[4], "discriminant_validation_results.rds")

cat("========================================\n")
cat("DISCRIMINANT VALIDATION SUITE\n")
cat("========================================\n")
cat("Configuration:\n")
cat("  Discriminants file:", discriminants_file, "\n")
cat("  Sample size per test:", sample_size, "\n")
cat("  Max discriminants:", ifelse(is.null(max_discriminants), "ALL (750)", max_discriminants), "\n")
cat("  CPU cores:", n_cores, "\n")
cat("  Output file:", output_file, "\n")
cat("========================================\n\n")

# Run validation
start_time <- Sys.time()

results <- run_discriminant_analysis(
  discriminants_file = discriminants_file,
  sample_size = sample_size,
  max_discriminants = max_discriminants,
  n_cores = n_cores,
  chunk_size = 50
)

end_time <- Sys.time()
total_time <- difftime(end_time, start_time, units = "mins")

# Analyze results
cat("\n========================================\n")
cat("VALIDATION RESULTS SUMMARY\n")
cat("========================================\n")

n_tested <- length(results)
quality_counts <- table(sapply(results, function(r) r$quality_rating))
scores <- sapply(results, function(r) r$overall_score)

cat("Total discriminants tested:", n_tested, "\n")
cat("Total runtime:", round(as.numeric(total_time), 2), "minutes\n")
cat("Average time per discriminant:", round(as.numeric(total_time) * 60 / n_tested, 2), "seconds\n")
cat("\n")

cat("Quality Distribution:\n")
for (rating in names(sort(quality_counts, decreasing = TRUE))) {
  count <- quality_counts[rating]
  pct <- round(count / n_tested * 100, 1)
  cat(sprintf("  %-12s: %4d (%5.1f%%)\n", rating, count, pct))
}
cat("\n")

cat("Score Statistics:\n")
cat("  Mean score:", round(mean(scores, na.rm = TRUE), 3), "\n")
cat("  Median score:", round(median(scores, na.rm = TRUE), 3), "\n")
cat("  Min score:", round(min(scores, na.rm = TRUE), 3), "\n")
cat("  Max score:", round(max(scores, na.rm = TRUE), 3), "\n")
cat("  Std deviation:", round(sd(scores, na.rm = TRUE), 3), "\n")
cat("\n")

# Identify best and worst discriminants
best_indices <- order(scores, decreasing = TRUE)[1:min(5, n_tested)]
worst_indices <- order(scores)[1:min(5, n_tested)]

cat("Top 5 Best Discriminants:\n")
for (idx in best_indices) {
  r <- results[[idx]]
  cat(sprintf(
    "  #%3d: a=%2d, b=%3d, c=%3d, Δ=%4d, Score=%.3f, Rating=%s\n",
    r$index, r$parameters$a, r$parameters$b, r$parameters$c,
    r$parameters$discriminant, r$overall_score, r$quality_rating
  ))
}
cat("\n")

cat("Bottom 5 Discriminants:\n")
for (idx in worst_indices) {
  r <- results[[idx]]
  cat(sprintf(
    "  #%3d: a=%2d, b=%3d, c=%3d, Δ=%4d, Score=%.3f, Rating=%s\n",
    r$index, r$parameters$a, r$parameters$b, r$parameters$c,
    r$parameters$discriminant, r$overall_score, r$quality_rating
  ))
}

# Save results
saveRDS(results, output_file)
cat("\n")
cat("Results saved to:", output_file, "\n")

# Generate detailed report if requested
if (interactive() || "--report" %in% commandArgs()) {
  cat("\n========================================\n")
  cat("DETAILED TEST BREAKDOWN\n")
  cat("========================================\n")

  # Count test passes
  uniformity_passes <- sum(sapply(results, function(r) r$uniformity$passed), na.rm = TRUE)
  independence_passes <- sum(sapply(results, function(r) r$independence$passed), na.rm = TRUE)
  autocorr_passes <- sum(sapply(results, function(r) r$autocorrelation$passed), na.rm = TRUE)
  periodicity_passes <- sum(sapply(results, function(r) r$periodicity$passed), na.rm = TRUE)

  cat("Individual Test Pass Rates:\n")
  cat(sprintf(
    "  Uniformity:      %3d/%3d (%.1f%%)\n",
    uniformity_passes, n_tested, uniformity_passes / n_tested * 100
  ))
  cat(sprintf(
    "  Independence:    %3d/%3d (%.1f%%)\n",
    independence_passes, n_tested, independence_passes / n_tested * 100
  ))
  cat(sprintf(
    "  Autocorrelation: %3d/%3d (%.1f%%)\n",
    autocorr_passes, n_tested, autocorr_passes / n_tested * 100
  ))
  cat(sprintf(
    "  Periodicity:     %3d/%3d (%.1f%%)\n",
    periodicity_passes, n_tested, periodicity_passes / n_tested * 100
  ))
}

cat("\n========================================\n")
cat("VALIDATION COMPLETE\n")
cat("========================================\n")

# Exit with appropriate code
excellent_count <- quality_counts["Excellent"]
if (is.na(excellent_count)) excellent_count <- 0

if (excellent_count >= 370) {
  cat("SUCCESS: At least 370 discriminants rated as Excellent\n")
  quit(status = 0)
} else {
  cat("WARNING: Only", excellent_count, "discriminants rated as Excellent (target: 370)\n")
  quit(status = 1)
}
