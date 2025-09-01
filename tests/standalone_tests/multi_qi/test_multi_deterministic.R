#!/usr/bin/env Rscript

# Test deterministic Multi-QI
library(qiprng)

cat("Testing Deterministic Multi-QI\n\n")

# Test with manual Multi-QI configuration and seed
cfg <- list(
  a = c(2, 3),
  b = c(5, 7),
  c = c(-1, -2),
  buffer_size = 10,
  mpfr_precision = 53,
  seed = 12345,
  use_crypto_mixing = FALSE # Disable crypto to ensure determinism
)

cat("First run with seed 12345:\n")
createPRNG(cfg)
vals1 <- generatePRNG(10)
cleanup_prng()
cat("Values:", vals1, "\n\n")

cat("Second run with same seed:\n")
createPRNG(cfg)
vals2 <- generatePRNG(10)
cleanup_prng()
cat("Values:", vals2, "\n\n")

if (identical(vals1, vals2)) {
  cat("✓ PASSED: Deterministic Multi-QI is working correctly\n")
} else {
  cat("✗ FAILED: Values differ between runs with same seed\n")
  cat("Differences:\n")
  print(vals1 - vals2)
}
