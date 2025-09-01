#!/usr/bin/env Rscript

# Basic test for Ziggurat method
library(qiprng)

cat("\n===== Basic Ziggurat Test =====\n\n")

# Create PRNG with Ziggurat method - basic configuration
cat("Creating PRNG with Ziggurat method (basic configuration)...\n")
createPRNG(list(
  distribution = "normal",
  normal_method = "ziggurat",
  use_threading = FALSE,
  use_parallel_filling = FALSE,
  debug = TRUE
))

# Generate some values and verify
cat("Generating 1000 values...\n")
values <- generatePRNG(1000)

# Calculate basic statistics
cat("Mean:", mean(values), "(expect ~0)\n")
cat("Variance:", var(values), "(expect ~1)\n")
cat("Min:", min(values), "\n")
cat("Max:", max(values), "\n")
cat("NaN values:", any(is.nan(values)), "\n")
cat("Infinity values:", any(is.infinite(values)), "\n")

# Clean up
cat("Cleaning up...\n")
cleanup_prng()

# Create PRNG with Box-Muller method for comparison
cat("\nCreating PRNG with Box-Muller method (for comparison)...\n")
createPRNG(list(
  distribution = "normal",
  normal_method = "box_muller",
  use_threading = FALSE,
  use_parallel_filling = FALSE,
  debug = TRUE
))

# Generate some values and verify
cat("Generating 1000 values...\n")
values2 <- generatePRNG(1000)

# Calculate basic statistics
cat("Mean:", mean(values2), "(expect ~0)\n")
cat("Variance:", var(values2), "(expect ~1)\n")
cat("Min:", min(values2), "\n")
cat("Max:", max(values2), "\n")
cat("NaN values:", any(is.nan(values2)), "\n")
cat("Infinity values:", any(is.infinite(values2)), "\n")

# Clean up
cat("Cleaning up...\n")
cleanup_prng()

# Test thread-safe mode with Ziggurat
cat("\nCreating PRNG with Ziggurat method (thread-safe mode)...\n")
createPRNG(list(
  distribution = "normal",
  normal_method = "ziggurat",
  use_threading = TRUE,
  use_parallel_filling = FALSE,
  debug = TRUE
))

# Generate some values and verify
cat("Generating 1000 values...\n")
values3 <- generatePRNG(1000)

# Calculate basic statistics
cat("Mean:", mean(values3), "(expect ~0)\n")
cat("Variance:", var(values3), "(expect ~1)\n")
cat("Min:", min(values3), "\n")
cat("Max:", max(values3), "\n")
cat("NaN values:", any(is.nan(values3)), "\n")
cat("Infinity values:", any(is.infinite(values3)), "\n")

# Clean up
cat("Cleaning up...\n")
cleanup_prng()

cat("\n===== Basic Ziggurat Test Completed! =====\n")
