#!/usr/bin/env Rscript

# Simple test script for normal distribution
library(qiprng)

cat("\n===== Testing Normal Distribution =====\n\n")

# Create PRNG for normal distribution
cat("Creating PRNG with normal distribution configuration...\n")
createPRNG()
updatePRNG(list(
  distribution = "normal",
  normal_method = "box_muller",
  normal_mean = 0,
  normal_sd = 1
))

# Generate normal values
cat("Generating normal values...\n")
normal_values <- generatePRNG(1000)
cat("Mean:", mean(normal_values), "Variance:", var(normal_values), "\n")

# Clean up
cat("Cleaning up...\n")
cleanupPRNG()

cat("\n===== Test completed successfully! =====\n")
