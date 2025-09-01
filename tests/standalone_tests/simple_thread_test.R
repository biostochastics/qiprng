#!/usr/bin/env Rscript

# Simple test for thread safety in QIPRNG
library(qiprng)

cat("\n===== Simple Thread Safety Test =====\n\n")

# Initialize PRNG with thread-safe configuration
cat("Creating PRNG with thread-safe configuration...\n")
tryCatch(
  {
    cfg <- list(
      distribution = "uniform_01",
      use_threading = TRUE,
      use_parallel_filling = TRUE,
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
      normal_method = "box_muller"
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
