#!/usr/bin/env Rscript

# Simpler test script for Ziggurat method validation
library(qiprng)

cat("\n===== Simple Ziggurat Test =====\n\n")

# Test basic Ziggurat (non-threaded)
cat("Testing basic Ziggurat (non-threaded)...\n")
tryCatch(
  {
    # Create and use PRNG
    createPRNG(list(
      distribution = "normal",
      normal_method = "ziggurat",
      use_threading = FALSE,
      use_parallel_filling = FALSE,
      debug = TRUE
    ))

    # Generate values
    values <- generatePRNG(1000)
    cat("  Mean:", mean(values), "\n")
    cat("  Variance:", var(values), "\n")

    # Clean up
    cleanup_prng()
    cat("SUCCESS\n\n")
  },
  error = function(e) {
    cat("ERROR:", e$message, "\n\n")
    cleanup_prng()
  }
)

# Test thread-safe Ziggurat
cat("Testing thread-safe Ziggurat...\n")
tryCatch(
  {
    # Create and use PRNG
    createPRNG(list(
      distribution = "normal",
      normal_method = "ziggurat",
      use_threading = TRUE,
      use_parallel_filling = FALSE,
      debug = TRUE
    ))

    # Generate values
    values <- generatePRNG(1000)
    cat("  Mean:", mean(values), "\n")
    cat("  Variance:", var(values), "\n")

    # Clean up
    cleanup_prng()
    cat("SUCCESS\n\n")
  },
  error = function(e) {
    cat("ERROR:", e$message, "\n\n")
    cleanup_prng()
  }
)

# Test Ziggurat with parallel filling (should use Box-Muller)
cat("Testing Ziggurat with parallel filling (auto-uses Box-Muller)...\n")
tryCatch(
  {
    # Create and use PRNG
    createPRNG(list(
      distribution = "normal",
      normal_method = "ziggurat",
      use_threading = TRUE,
      use_parallel_filling = TRUE,
      debug = TRUE
    ))

    # Generate values
    values <- generatePRNG(1000)
    cat("  Mean:", mean(values), "\n")
    cat("  Variance:", var(values), "\n")

    # Clean up
    cleanup_prng()
    cat("SUCCESS\n\n")
  },
  error = function(e) {
    cat("ERROR:", e$message, "\n\n")
    cleanup_prng()
  }
)

cat("===== Simple Ziggurat Test Completed =====\n")
