#!/usr/bin/env Rscript

# Debug script for thread safety issues
library(qiprng)

create_test_discriminants <- function() {
  # Define paths to check
  test_paths <- c(
    "discriminants.csv",
    "../discriminants.csv",
    "../../discriminants.csv"
  )

  # Check if any file exists
  file_exists <- FALSE
  for (path in test_paths) {
    if (file.exists(path)) {
      cat("Found discriminants file at:", path, "\n")
      file_exists <- TRUE
      break
    }
  }

  # If no file exists, create one
  if (!file_exists) {
    cat("Creating temporary discriminants.csv file for testing\n")
    # Generate 100 discriminants with valid a,b,c values
    csv_data <- "a,b,c,discriminant\n"
    for (i in 1:100) {
      a <- 1 + (i %% 10)
      b <- 3 + 2 * (i %% 8)
      c <- -1 - (i %% 5)
      discriminant <- b^2 - 4 * a * c
      csv_data <- paste0(csv_data, a, ",", b, ",", c, ",", discriminant, "\n")
    }

    # Write to file
    writeLines(csv_data, "discriminants.csv")
    cat("Created discriminants.csv with 100 entries\n")
  }
}

# 1. Create test discriminants file if needed
create_test_discriminants()

# Simple discriminant test - single thread
cat("\n===== Testing discriminant selection (single thread) =====\n")
config <- list(
  debug = TRUE,
  use_csv_discriminants = TRUE,
  use_threading = FALSE
)
createPRNG(config)

# Print message about current directory
cat("Current directory:", getwd(), "\n")

# Try to select one discriminant
tryCatch(
  {
    discriminant <- .Call("_qiprng_chooseUniqueDiscriminant_wrapped", 5, 1000000)
    cat("Selected discriminant:", discriminant, "\n")
  },
  error = function(e) {
    cat("Error selecting discriminant:", e$message, "\n")
  }
)

# Clean up
cleanup_prng()

# Test thread version
cat("\n===== Testing discriminant thread safety (explicit) =====\n")
config <- list(
  debug = TRUE,
  use_csv_discriminants = TRUE,
  use_threading = TRUE
)
createPRNG(config)

# Run the test but with explicit verbose flag
tryCatch(
  {
    # Add a wrapper function to the global env
    .chooseUniqueDiscriminant <- function(min_val = 5, max_val = 1000000) {
      return(.Call("_qiprng_chooseUniqueDiscriminant_wrapped", min_val, max_val))
    }

    # Try to select some discriminants
    discriminants <- list()
    for (i in 1:5) {
      tryCatch(
        {
          disc <- .chooseUniqueDiscriminant(5, 1000000)
          discriminants[[i]] <- disc
          cat("Thread 1 - Selected discriminant", i, ":", disc, "\n")
        },
        error = function(e) {
          cat("Thread 1 - Error selecting discriminant", i, ":", e$message, "\n")
        }
      )
    }

    cat("Selected", length(discriminants), "discriminants\n")
  },
  error = function(e) {
    cat("Error in test:", e$message, "\n")
  }
)

# Clean up
cleanup_prng()

# Let's try the actual test again
cat("\n===== Running official thread safety test =====\n")
config <- list(
  debug = TRUE,
  use_csv_discriminants = TRUE,
  use_threading = TRUE
)
createPRNG(config)

# Run with fewer threads and iterations for debugging
test_results <- test_choose_discriminant(2, 3)
cat("test_results class:", class(test_results), "\n")
cat("test_results length:", length(test_results), "\n")

if (length(test_results) > 0) {
  for (i in 1:length(test_results)) {
    cat("Thread", i, "results:", length(test_results[[i]]), "discriminants\n")
    if (length(test_results[[i]]) > 0) {
      cat("  First discriminant:", test_results[[i]][1], "\n")
    }
  }
}

# Clean up
cleanup_prng()

cat("\n===== Debug completed! =====\n")
