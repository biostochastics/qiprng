#!/usr/bin/env Rscript

# Simple test for v0.5.0 features
library(qiprng)

cat("Testing qiprng v0.5.0 Enhancements (Simple)\n")
cat("============================================\n\n")

# Test 1: Basic generation works
cat("Test 1: Basic generation\n")
cfg <- list(
  a = 2, b = 5, c = -2,
  mpfr_precision = 53,
  buffer_size = 100,
  distribution = "uniform_01"
)

createPRNG(cfg)
vals <- generatePRNG(100)
cleanup_prng()

cat(sprintf("  Generated %d values\n", length(vals)))
cat(sprintf("  Mean: %.4f, SD: %.4f\n", mean(vals), sd(vals)))
cat(sprintf("  Range: [%.4f, %.4f]\n", min(vals), max(vals)))

# Test 2: Different mixing strategies
cat("\nTest 2: Mixing strategies\n")
strategies <- c("round_robin", "xor_mixing", "averaging")

for (strategy in strategies) {
  cfg$mixing_strategy <- strategy
  createPRNG(cfg)
  vals <- generatePRNG(100)
  cleanup_prng()
  cat(sprintf("  %s: mean=%.4f\n", strategy, mean(vals)))
}

cat("\n============================================\n")
cat("Simple v0.5.0 Tests Complete\n")
