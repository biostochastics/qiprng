#!/usr/bin/env Rscript

# Final comprehensive test for QIPRNG with all fixes
library(qiprng)

cat("\n===== QIPRNG Final Test =====\n\n")

# Helper function to test different configurations
test_configuration <- function(description, config) {
  cat(paste0("Testing: ", description, "\n"))

  # Create PRNG with the specified configuration
  tryCatch(
    {
      cat("  Creating PRNG... ")
      createPRNG(config)
      cat("SUCCESS\n")

      cat("  Generating values... ")
      values <- generatePRNG(1000)
      cat("SUCCESS\n")

      # Calculate statistics
      mean_val <- mean(values)
      var_val <- var(values)
      min_val <- min(values)
      max_val <- max(values)
      has_nan <- any(is.nan(values))
      has_inf <- any(is.infinite(values))

      cat("  Mean:", mean_val, "\n")
      cat("  Variance:", var_val, "\n")
      cat("  Range:", min_val, "to", max_val, "\n")
      cat("  NaN/Inf values:", has_nan || has_inf, "\n")

      cat("  Cleaning up... ")
      cleanup_prng()
      cat("SUCCESS\n")

      TRUE
    },
    error = function(e) {
      cat("ERROR:", e$message, "\n")
      cleanup_prng()
      FALSE
    }
  )

  cat("\n")
}

# Test recommended configurations
cat("===== TESTING RECOMMENDED CONFIGURATIONS =====\n\n")

# Basic non-threaded Ziggurat
test_configuration("Basic Ziggurat (non-threaded)", list(
  distribution = "normal",
  normal_method = "ziggurat",
  use_threading = FALSE,
  use_parallel_filling = FALSE,
  debug = TRUE
))

# Thread-safe Ziggurat
test_configuration("Thread-safe Ziggurat", list(
  distribution = "normal",
  normal_method = "ziggurat",
  use_threading = TRUE,
  use_parallel_filling = FALSE,
  debug = TRUE
))

# Thread-safe Box-Muller with parallel filling
test_configuration("Thread-safe Box-Muller with parallel filling", list(
  distribution = "normal",
  normal_method = "box_muller",
  use_threading = TRUE,
  use_parallel_filling = TRUE,
  debug = TRUE
))

# Default configuration (should use Box-Muller when parallel filling is enabled)
test_configuration("Default with parallel filling", list(
  distribution = "normal",
  use_threading = TRUE,
  use_parallel_filling = TRUE,
  debug = TRUE
))

cat("===== FINAL TEST COMPLETED =====\n")
