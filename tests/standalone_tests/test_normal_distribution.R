#!/usr/bin/env Rscript

# Standalone test script for normal distribution
# This script doesn't require rebuilding the package, just runs with existing installation

# Load the qiprng package
library(qiprng)

cat("\n===== Testing Normal Distribution Fixes =====\n\n")

# Set up for testing the normal distribution
cat("Creating PRNG with normal distribution...\n")

# Create the PRNG with normal distribution
createPRNG(list(
  distribution = "normal",
  normal_mean = 0,
  normal_sd = 1,
  debug = TRUE
))

# Display current configuration (removed since getPRNGConfig isn't available)
cat("Created PRNG with normal distribution\n")

# Generate normal values
cat("\nGenerating 10,000 normal values...\n")
normal_values <- generatePRNG(10000)

# Basic statistics
mean_val <- mean(normal_values)
var_val <- var(normal_values)
min_val <- min(normal_values)
max_val <- max(normal_values)
all_identical <- all(normal_values == normal_values[1])

# Print results
cat("All values identical:", all_identical, "\n")
cat("Mean:", mean_val, "(expect ~0)\n")
cat("Variance:", var_val, "(expect ~1)\n")
cat("Range: [", min_val, ",", max_val, "]\n")

# Generate histogram to visualize distribution
if (!all_identical) {
  cat("\nGenerating histogram of values...\n")
  hist(normal_values, breaks=50, main="Normal Distribution Test", 
       xlab="Value", col="lightblue", border="white")
}

# Test with threading enabled
cat("\n===== Testing with threading enabled =====\n")

# Update configuration to enable threading
updatePRNG(list(
  use_threading = TRUE
))

# Generate normal values with threading
cat("Generating 10,000 normal values with threading...\n")
threaded_values <- generatePRNG(10000)

# Basic statistics for threaded values
threaded_mean <- mean(threaded_values)
threaded_var <- var(threaded_values)
threaded_min <- min(threaded_values)
threaded_max <- max(threaded_values)
threaded_all_identical <- all(threaded_values == threaded_values[1])

# Print threaded results
cat("All values identical:", threaded_all_identical, "\n")
cat("Mean:", threaded_mean, "(expect ~0)\n")
cat("Variance:", threaded_var, "(expect ~1)\n")
cat("Range: [", threaded_min, ",", threaded_max, "]\n")

# Clean up
cat("\nCleaning up...\n")
cleanup_prng()

cat("\n===== Test completed! =====\n")