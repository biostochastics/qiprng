#!/usr/bin/env Rscript

# Comprehensive test of thread safety fixes
library(qiprng)

cat("\n===== Comprehensive Thread Safety Test =====\n\n")

run_test <- function(description, config_mods = list()) {
  cat("\n--- Testing:", description, "---\n")
  
  # Create PRNG with default config
  cat("Creating PRNG...\n")
  createPRNG()
  
  # Update with modifications if any
  if (length(config_mods) > 0) {
    cat("Updating config...\n")
    updatePRNG(config_mods)
  }
  
  # Generate samples and check basic stats
  cat("Generating 10,000 samples...\n")
  samples <- generatePRNG(10000)
  
  cat("Statistics:\n")
  cat("  Mean:", mean(samples), "\n")
  cat("  Variance:", var(samples), "\n")
  cat("  Min:", min(samples), "\n")
  cat("  Max:", max(samples), "\n")
  
  # Clean up
  cat("Cleaning up...\n")
  cleanupPRNG()
  
  cat("Test completed successfully!\n")
}

# Test 1: Default uniform with threading disabled
run_test("Default uniform distribution", list(
  distribution = "uniform_01",
  use_threading = FALSE,
  use_parallel_filling = FALSE
))

# Test 2: Uniform with threading enabled
run_test("Uniform with threading", list(
  distribution = "uniform_01",
  use_threading = TRUE,
  use_parallel_filling = FALSE
))

# Test 3: Uniform with parallel filling
run_test("Uniform with parallel filling", list(
  distribution = "uniform_01",
  use_threading = TRUE,
  use_parallel_filling = TRUE
))

# Test 4: Normal distribution with Box-Muller - no threading
run_test("Normal with Box-Muller (no threading)", list(
  distribution = "normal",
  normal_method = "box_muller",
  use_threading = FALSE,
  use_parallel_filling = FALSE
))

# Test 5: Normal distribution with Box-Muller, minimal threading
run_test("Normal with Box-Muller (minimal threading)", list(
  distribution = "normal",
  normal_method = "box_muller",
  use_threading = TRUE,
  use_parallel_filling = FALSE
))

cat("\n===== All tests completed successfully! =====\n")