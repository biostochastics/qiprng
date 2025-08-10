#!/usr/bin/env Rscript

# Test script to verify normal distribution fixes
library(qiprng)

cat("\n===== Testing Normal Distribution Fixes =====\n\n")

# Create PRNG with normal distribution
cat("Creating PRNG with normal distribution...\n")
createPRNG(list(
  distribution = "normal",
  normal_method = "box_muller",
  normal_mean = 0,
  normal_sd = 1,
  debug = TRUE
))

# Test for all identical values issue
cat("Generating 1000 normal values...\n")
normal_values <- generatePRNG(1000)

# Check if all values are identical
all_same <- all(normal_values == normal_values[1])
cat("All values identical:", all_same, "\n")

# Calculate basic statistics
cat("Mean:", mean(normal_values), "\n")
cat("Variance:", var(normal_values), "\n")
cat("Min:", min(normal_values), "\n")
cat("Max:", max(normal_values), "\n")

# Run minimal test
test_that("Normal distribution values are not all identical", {
  expect_false(all_same)
  expect_true(abs(mean(normal_values)) < 0.3)
  expect_true(abs(var(normal_values) - 1) < 0.3)
})

# Create PRNG with threading enabled to test thread safety
cat("\nTesting with threading enabled...\n")
createPRNG(list(
  distribution = "normal",
  normal_method = "box_muller",
  normal_mean = 0,
  normal_sd = 1,
  use_threading = TRUE,
  debug = TRUE
))

# Generate values with threading
threaded_values <- generatePRNG(1000)

# Check for identical values issue
threaded_all_same <- all(threaded_values == threaded_values[1])
cat("All threaded values identical:", threaded_all_same, "\n")
cat("Mean:", mean(threaded_values), "\n")
cat("Variance:", var(threaded_values), "\n")

# Test threading results
test_that("Threaded normal distribution works", {
  expect_false(threaded_all_same)
  expect_true(abs(mean(threaded_values)) < 0.3)
  expect_true(abs(var(threaded_values) - 1) < 0.3)
})

# Clean up
cleanup_prng()

cat("\n===== Test completed! =====\n")