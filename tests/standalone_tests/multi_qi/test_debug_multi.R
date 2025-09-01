#!/usr/bin/env Rscript

# Debug Multi-QI test
library(qiprng)

cat("Debug Multi-QI Test\n")

# Enable debug mode if available
Sys.setenv(DEBUG_MIXING = "1")

# Test creating Multi-QI config
cfg <- list(
  a = c(2, 3),
  b = c(5, 7),
  c = c(-1, -2),
  buffer_size = 10,
  mpfr_precision = 53
)

cat("Config:\n")
print(cfg)

cat("\nCreating PRNG...\n")
createPRNG(cfg)
cat("PRNG created successfully!\n")

cat("\nGenerating 1 value...\n")
val1 <- generatePRNG(1)
cat("Value 1:", val1, "\n")

cat("\nGenerating 1 more value...\n")
val2 <- generatePRNG(1)
cat("Value 2:", val2, "\n")

cleanup_prng()
cat("Test complete!\n")
