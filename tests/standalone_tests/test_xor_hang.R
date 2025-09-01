#!/usr/bin/env Rscript

library(qiprng)

# Test XOR mixing with single QI - using string value
cat("Testing XOR mixing with single QI (using string)...\n")
cfg <- list(
  a = 2, b = 5, c = -2,
  mpfr_precision = 53,
  buffer_size = 100,
  mixing_strategy = "xor_mix", # Use string instead of integer
  distribution = "uniform_01"
)

createPRNG(cfg)

# Try to generate just 10 values with a timeout
tryCatch(
  {
    # Use setTimeLimit to prevent hanging
    setTimeLimit(cpu = 2, elapsed = 2)

    vals <- generatePRNG(10)
    cat("Generated", length(vals), "values\n")
    cat("Mean:", mean(vals), "\n")
    cleanup_prng()
    cat("Success!\n")

    # Reset time limit
    setTimeLimit(cpu = Inf, elapsed = Inf)
  },
  error = function(e) {
    cat("Error or timeout:", e$message, "\n")
    # Reset time limit
    setTimeLimit(cpu = Inf, elapsed = Inf)
    cleanup_prng()
  }
)

cat("\nTest complete.\n")
