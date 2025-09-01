#!/usr/bin/env Rscript

# Minimal Multi-QI test
library(qiprng)

cat("Minimal Multi-QI Test\n")

# Test creating Multi-QI config only
cfg <- list(
  a = c(2, 3),
  b = c(5, 7),
  c = c(-1, -2),
  buffer_size = 10
)

cat("Creating PRNG...\n")
createPRNG(cfg)
cat("PRNG created successfully!\n")

cat("Generating 5 values...\n")
vals <- generatePRNG(5)
cat("Values:", vals, "\n")

cleanup_prng()
cat("Test complete!\n")
