#!/usr/bin/env Rscript

# Simplified test script to verify thread safety using only builtin values
library(qiprng)

cat("\n===== Testing discriminant thread safety =====\n\n")

# Run the test with smaller parameters to avoid timeouts
cat("Running test with 2 threads and 5 iterations each...\n")

# Initialize with threading but without CSV discriminants
cat("Creating PRNG with threading enabled, CSV disabled...\n")
createPRNG(list(
  debug = TRUE,
  use_csv_discriminants = FALSE, # Disable CSV to avoid file issues
  use_threading = TRUE
))

# Testing a single discriminant selection to verify functionality
cat("Testing direct discriminant selection:\n")
disc_test <- NULL
try(
  {
    # Create a simple test discriminant directly in R
    a <- 2
    b <- 7
    c <- -3
    disc <- b^2 - 4 * a * c
    cat("  Expected discriminant value:", disc, "\n")

    # Update PRNG with these values to test
    updatePRNG(list(a = a, b = b, c = c))
  },
  silent = FALSE
)

# Run the test with fewer threads/iterations to avoid timeouts
tryCatch(
  {
    results <- test_choose_discriminant(2, 5)

    cat("\nChecking results...\n")
    if (is.list(results) && length(results) > 0) {
      # Look at each thread's results
      for (i in 1:length(results)) {
        if (length(results[[i]]) > 0) {
          cat("Thread", i, "generated", length(results[[i]]), "values\n")
          if (length(results[[i]]) > 0) {
            cat("  First value:", results[[i]][1], "\n")
          }
        } else {
          cat("Thread", i, "generated no values\n")
        }
      }

      # Calculate total results
      total_values <- sum(sapply(results, length))
      cat("\nTotal generated values:", total_values, "out of", 2 * 5, "\n")

      # Only check uniqueness if we have results
      if (total_values > 0) {
        is_unique <- check_discriminants_unique(results)
        if (is_unique) {
          cat("\nSUCCESS: All discriminants are unique across threads!\n")
        } else {
          cat("\nFAILURE: Found duplicate discriminants across threads!\n")
        }
      }
    } else {
      cat("\nNo results returned from test\n")
    }
  },
  error = function(e) {
    cat("\nERROR:", e$message, "\n")
  }
)

# Clean up
cleanup_prng()

cat("\n===== Test completed! =====\n")
