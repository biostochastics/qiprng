#!/usr/bin/env Rscript

# Test script for qiprng package
cat("Testing qiprng package...\n")

# Load the library
library(qiprng)

# Create PRNG with default configuration
cat("Creating PRNG with default configuration...\n")
createPRNG()

# Generate random numbers
cat("Generating 10,000 random numbers...\n")
x <- generatePRNG(10000)

# Basic statistical checks
cat("Basic statistical tests:\n")
cat("Mean:", mean(x), "- Should be close to 0.5 for uniform_01\n")
cat("Variance:", var(x), "- Should be close to 1/12 (approx. 0.0833) for uniform_01\n")

# Test with different output distribution
cat("\nUpdating to normal distribution...\n")
updatePRNG(list(
  distribution = "normal",
  normal_mean = 0,
  normal_sd = 1
))

# Generate normal random numbers
cat("Generating 10,000 normal random numbers...\n")
norm_samples <- generatePRNG(10000)
cat("Normal mean:", mean(norm_samples), "- Should be close to 0\n")
cat("Normal variance:", var(norm_samples), "- Should be close to 1\n")

# Test with exponential distribution
cat("\nUpdating to exponential distribution...\n")
updatePRNG(list(
  distribution = "exponential",
  exponential_lambda = 2.0
))

# Generate exponential random numbers
cat("Generating 10,000 exponential random numbers...\n")
exp_samples <- generatePRNG(10000)
cat("Exponential mean:", mean(exp_samples), "- Should be close to 1/lambda = 0.5\n")
cat("Exponential variance:", var(exp_samples), "- Should be close to 1/lambda^2 = 0.25\n")

cat("\nTesting completed successfully.\n")
