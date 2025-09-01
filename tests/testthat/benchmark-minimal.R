#!/usr/bin/env Rscript

# Minimal QIPRNG Benchmarking Example
# This script demonstrates a minimal benchmark for testing

# Load the qiprng package
library(qiprng)

# Suppress MPFR warnings for cleaner output
suppressMPFRWarnings(TRUE)

# Create and test PRNG
test_prng <- function() {
  # Default configuration
  cat("Testing default configuration...\n")
  createPRNG()
  vals <- generatePRNG(1000)
  cat(sprintf("Mean: %.4f, SD: %.4f\n", mean(vals), sd(vals)))
  cleanup_prng()

  # Configuration with crypto mixing
  cat("\nTesting with crypto mixing...\n")
  createPRNG(list(use_crypto_mixing = TRUE))
  vals <- generatePRNG(1000)
  cat(sprintf("Mean: %.4f, SD: %.4f\n", mean(vals), sd(vals)))
  cleanup_prng()

  # Test normal distribution with Ziggurat method
  cat("\nTesting normal distribution with Ziggurat method...\n")
  createPRNG(list(
    distribution = "normal",
    normal_method = "ziggurat"
  ))
  vals <- generatePRNG(1000)
  cat(sprintf("Mean: %.4f, SD: %.4f\n", mean(vals), sd(vals)))
  cleanup_prng()

  # Test normal distribution with Box-Muller method
  cat("\nTesting normal distribution with Box-Muller method...\n")
  createPRNG(list(
    distribution = "normal",
    normal_method = "box_muller"
  ))
  vals <- generatePRNG(1000)
  cat(sprintf("Mean: %.4f, SD: %.4f\n", mean(vals), sd(vals)))
  cleanup_prng()
}

# Simple timing benchmark
benchmark_prng <- function() {
  cat("\nBenchmarking PRNG performance...\n")
  configs <- list(
    "Standard" = list(),
    "Crypto" = list(use_crypto_mixing = TRUE),
    "Normal (Ziggurat)" = list(distribution = "normal", normal_method = "ziggurat"),
    "Normal (Box-Muller)" = list(distribution = "normal", normal_method = "box_muller")
  )

  n <- 100000
  results <- data.frame(
    Configuration = character(),
    Time_ms = numeric(),
    stringsAsFactors = FALSE
  )

  for (name in names(configs)) {
    cat(sprintf("Testing %s: ", name))
    cfg <- configs[[name]]

    # Run the test
    time_ms <- system.time({
      createPRNG(cfg)
      vals <- generatePRNG(n)
      cleanup_prng()
    })[3] * 1000 # Convert to milliseconds

    cat(sprintf("%.2f ms\n", time_ms))

    # Add to results
    results <- rbind(results, data.frame(
      Configuration = name,
      Time_ms = time_ms,
      stringsAsFactors = FALSE
    ))
  }

  # Return results
  return(results)
}

# Run the tests
cat("=== QIPRNG Minimal Benchmarks ===\n\n")
test_prng()
results <- benchmark_prng()

# Display summary
cat("\n=== Summary ===\n")
print(results)
cat("\n=== Test Complete ===\n")
