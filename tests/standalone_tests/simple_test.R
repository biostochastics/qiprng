#!/usr/bin/env Rscript

# Simple test script to verify our changes to the package
library(qiprng)

cat("\n===== Testing basic functionality =====\n\n")

# Test with default configuration
cat("Creating PRNG with default configuration...\n")
createPRNG()
cat("PRNG created successfully.\n")

cat("Generating uniform random numbers...\n")
samples_uniform <- generatePRNG(1000)
cat("Mean:", mean(samples_uniform), "(should be ~0.5)\n")
cat("Variance:", var(samples_uniform), "(should be ~0.083)\n")

cat("\nUpdating to normal distribution with Box-Muller...\n")
updatePRNG(list(
  distribution = "normal",
  normal_method = "box_muller",
  use_threading = TRUE,
  use_parallel_filling = TRUE
))
cat("Configuration updated successfully.\n")

cat("Generating normal random numbers...\n")
samples_normal <- generatePRNG(1000)
cat("Mean:", mean(samples_normal), "(should be ~0)\n")
cat("Variance:", var(samples_normal), "(should be ~1)\n")

cat("\nCleaning up resources...\n")
cleanupPRNG()
cat("Resources cleaned up successfully.\n")

cat("\n===== Test completed! =====\n")
