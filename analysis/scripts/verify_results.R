# Script to verify the contents of the analysis results

# Path to the summary file
summary_path <- "analysis/results/discriminant_analysis_results/summary_statistics.csv"

# Check if the file exists
if (!file.exists(summary_path)) {
  stop("ERROR: Summary statistics file not found at: ", summary_path)
}

# Read the data
cat("Reading summary statistics from:", summary_path, "\n\n")
summary_data <- read.csv(summary_path)

# Print dimensions
cat("=== FILE DIMENSIONS ===\n")
print(dim(summary_data))
cat("\n")

# Print the head
cat("=== FILE HEAD ===\n")
print(head(summary_data))
cat("\n")

# Print the tail
cat("=== FILE TAIL ===\n")
print(tail(summary_data))
cat("\n")

# Print summary of key columns
cat("=== SUMMARY OF KEY COLUMNS ===\n")
cat("--- Quality Rating ---\
")
print(table(summary_data$quality_rating))
cat("\n--- Overall Score ---\
")
print(summary(summary_data$overall_score))
cat("\n")

cat("Verification script finished.\n")
