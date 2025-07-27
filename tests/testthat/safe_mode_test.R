#!/usr/bin/env Rscript

# Safe mode test for QIPRNG thread safety
library(qiprng)

cat("\n===== QIPRNG Safe Mode Test =====\n\n")

# Initialize PRNG with thread-safe configuration but no parallel filling
cat("Creating PRNG with thread-safe safe mode...\n")
tryCatch({
  cfg <- list(
    distribution = "uniform_01",
    use_threading = TRUE,         # Enable threading
    use_parallel_filling = FALSE, # Disable parallel filling for safety
    debug = TRUE
  )
  createPRNG(cfg)
  
  # Display configuration
  cat("PRNG created successfully with threading (safe mode).\n")
  
  # Generate some uniform values
  cat("\nGenerating uniform random numbers...\n")
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
  cat("Generating normal random numbers...\n")
  normal_values <- generatePRNG(1000)
  cat("Generated", length(normal_values), "normal values.\n")
  cat("Mean:", mean(normal_values), "\n")
  cat("Variance:", var(normal_values), "\n")
  
  # Clean up
  cat("\nCleaning up...\n")
  cleanup_prng()
  
  cat("\nTest completed successfully!\n")
}, error = function(e) {
  cat("ERROR:", e$message, "\n")
})

cat("\n===== Safe Mode Test Finished =====\n")