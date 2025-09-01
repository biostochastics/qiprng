#!/usr/bin/env Rscript

# Basic test for QIPRNG functionality
library(qiprng)

cat("\n===== Basic QIPRNG Functionality Test =====\n\n")

# Initialize PRNG with basic configuration
cat("Creating PRNG with basic configuration...\n")
tryCatch(
  {
    cfg <- list(
      distribution = "uniform_01",
      use_threading = FALSE,
      use_parallel_filling = FALSE,
      debug = TRUE
    )
    createPRNG(cfg)
    cat("PRNG created successfully.\n")

    # Generate some values
    cat("\nGenerating random numbers...\n")
    values <- generatePRNG(1000)
    cat("Generated", length(values), "values.\n")
    cat("Mean:", mean(values), "\n")
    cat("Variance:", var(values), "\n")

    # Test normal distribution
    cat("\nSwitching to normal distribution with Box-Muller...\n")
    updatePRNG(list(
      distribution = "normal",
      normal_method = "box_muller",
      use_threading = FALSE,
      use_parallel_filling = FALSE
    ))

    # Generate normal values
    normal_values <- generatePRNG(1000)
    cat("Generated", length(normal_values), "normal values.\n")
    cat("Mean:", mean(normal_values), "\n")
    cat("Variance:", var(normal_values), "\n")

    # Clean up
    cat("\nCleaning up...\n")
    cleanup_prng()

    cat("\nTest completed successfully!\n")
  },
  error = function(e) {
    cat("ERROR:", e$message, "\n")
  }
)

cat("\n===== Test finished =====\n")
