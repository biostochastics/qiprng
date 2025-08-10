#!/usr/bin/env Rscript

# Test script for Ziggurat table caching
library(qiprng)
library(microbenchmark)

cat("\n===== Testing Ziggurat Table Caching Optimization =====\n\n")

# First run should initialize cached tables
cat("Creating first PRNG with normal distribution using Ziggurat method...\n")
start_time <- proc.time()
createPRNG(list(
  distribution = "normal",
  normal_method = "ziggurat",
  normal_mean = 0,
  normal_sd = 1
))
end_time <- proc.time()
init_time <- end_time - start_time
cat("First initialization took:", init_time[3], "seconds\n")

# Generate some values
x1 <- generatePRNG(1000)
cat("Mean:", mean(x1), "\n")
cat("Variance:", var(x1), "\n")
cleanup_prng()

# Second run should use cached tables
cat("\nCreating second PRNG with normal distribution using Ziggurat method...\n")
start_time <- proc.time()
createPRNG(list(
  distribution = "normal",
  normal_method = "ziggurat",
  normal_mean = 0,
  normal_sd = 1
))
end_time <- proc.time()
second_init_time <- end_time - start_time
cat("Second initialization took:", second_init_time[3], "seconds\n")

# Generate some values
x2 <- generatePRNG(1000)
cat("Mean:", mean(x2), "\n")
cat("Variance:", var(x2), "\n")
cleanup_prng()

# Calculate speedup
if (init_time[3] > 0) {
  speedup <- init_time[3] / second_init_time[3]
  cat("\nSpeedup factor:", speedup, "times\n")
}

# Benchmark creation of multiple PRNGs
cat("\nBenchmarking creation of multiple PRNGs with cached tables...\n")
benchmark_results <- microbenchmark(
  create_prng = {
    createPRNG(list(
      distribution = "normal",
      normal_method = "ziggurat",
      normal_mean = 0,
      normal_sd = 1
    ))
    cleanup_prng()
  },
  times = 10
)

print(benchmark_results)

cat("\n===== Test completed! =====\n")