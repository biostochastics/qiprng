#!/usr/bin/env Rscript

# Simple test script for thread safety in discriminant selection
library(qiprng)

cat("\n===== Testing thread safety of discriminant selection =====\n\n")

# Configure PRNG for normal distribution with threading enabled
cfg <- list(
  distribution = "normal",
  normal_method = "ziggurat", # Test the ziggurat method
  use_csv_discriminants = TRUE, # Test discriminant selection
  use_threading = TRUE, # Enable thread-local PRNG
  use_parallel_filling = TRUE, # Test parallel filling
  debug = TRUE # Enable debug output
)

cat("Creating PRNG with threading and parallel filling enabled...\n")
createPRNG(cfg)

# Generate a large number of values to test threading
cat("Generating 10,000 values...\n")
samples <- generatePRNG(10000)

# Basic statistics
cat(paste0("Mean: ", mean(samples), " (should be close to 0)\n"))
cat(paste0("Variance: ", var(samples), " (should be close to 1)\n"))
cat(paste0("Min: ", min(samples), "\n"))
cat(paste0("Max: ", max(samples), "\n"))

# Clean up
cat("Cleaning up PRNG resources...\n")
cleanup_prng()

cat("\nNow switching to Box-Muller method...\n")

# Test with Box-Muller method
cfg$normal_method <- "box_muller"
cat("Creating PRNG with Box-Muller method...\n")
createPRNG(cfg)

# Generate values again
cat("Generating 10,000 values...\n")
samples <- generatePRNG(10000)

# Basic statistics
cat(paste0("Mean: ", mean(samples), " (should be close to 0)\n"))
cat(paste0("Variance: ", var(samples), " (should be close to 1)\n"))
cat(paste0("Min: ", min(samples), "\n"))
cat(paste0("Max: ", max(samples), "\n"))

# Clean up
cat("Cleaning up PRNG resources...\n")
cleanup_prng()

cat("\n===== Test completed! =====\n")
