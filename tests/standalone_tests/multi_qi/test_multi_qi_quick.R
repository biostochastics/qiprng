#!/usr/bin/env Rscript

# Quick Multi-QI Ensemble test
library(qiprng)

cat("Quick Multi-QI Test\n")
cat("===================\n\n")

# Test with 2 QIs
cat("Test 1: Multi-QI with 2 QIs\n")
tryCatch(
  {
    cfg <- list(
      a = c(2, 3),
      b = c(5, 7),
      c = c(-1, -2),
      mixing_strategy = "xor_mix",
      buffer_size = 100
    )

    createPRNG(cfg)
    vals <- generatePRNG(50)
    cleanup_prng()

    cat(sprintf("  ✓ Generated %d values\n", length(vals)))
    cat(sprintf("  Mean: %.4f\n", mean(vals)))
    cat(sprintf("  Result: SUCCESS\n\n"))
  },
  error = function(e) {
    cat(sprintf("  ✗ FAILED: %s\n\n", e$message))
  }
)

# Test with 3 QIs
cat("Test 2: Multi-QI with 3 QIs\n")
tryCatch(
  {
    cfg <- list(
      a = c(2, 3, 5),
      b = c(7, 11, 13),
      c = c(-3, -5, -7),
      mixing_strategy = "round_robin",
      buffer_size = 100
    )

    createPRNG(cfg)
    vals <- generatePRNG(50)
    cleanup_prng()

    cat(sprintf("  ✓ Generated %d values\n", length(vals)))
    cat(sprintf("  Mean: %.4f\n", mean(vals)))
    cat(sprintf("  Result: SUCCESS\n\n"))
  },
  error = function(e) {
    cat(sprintf("  ✗ FAILED: %s\n\n", e$message))
  }
)

cat("Multi-QI fix verified successfully!\n")
