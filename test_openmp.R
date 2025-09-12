#!/usr/bin/env Rscript

library(qiprng)

# Test OpenMP performance with large buffer
cat("Testing OpenMP parallelization...\n\n")

# Create PRNGs with different threading settings
config_single <- list(
  buffer_size = 1000000,
  use_parallel_filling = FALSE,
  use_threading = FALSE
)

config_parallel <- list(
  buffer_size = 1000000,
  use_parallel_filling = TRUE,
  use_threading = TRUE
)

# Test single-threaded
cat("Benchmarking single-threaded generation (1M samples)...\n")
createPRNG(config_single)
time_single <- system.time({
  samples_single <- generatePRNG(1000000)
})
cat(sprintf("Single-threaded time: %.3f seconds\n", time_single[["elapsed"]]))
cat(sprintf("Samples/second: %.0f\n", 1000000 / time_single[["elapsed"]]))

# Test multi-threaded (OpenMP)
cat("\nBenchmarking OpenMP parallel generation (1M samples)...\n")
updatePRNG(config_parallel)
time_parallel <- system.time({
  samples_parallel <- generatePRNG(1000000)
})
cat(sprintf("OpenMP parallel time: %.3f seconds\n", time_parallel[["elapsed"]]))
cat(sprintf("Samples/second: %.0f\n", 1000000 / time_parallel[["elapsed"]]))

# Calculate speedup
speedup <- time_single[["elapsed"]] / time_parallel[["elapsed"]]
cat(sprintf("\nSpeedup: %.2fx\n", speedup))

# Verify results are valid
cat("\nVerifying sample quality...\n")
cat(sprintf("Single-threaded mean: %.6f (expected: ~0.5)\n", mean(samples_single)))
cat(sprintf("Parallel mean: %.6f (expected: ~0.5)\n", mean(samples_parallel)))
cat(sprintf("Single-threaded sd: %.6f (expected: ~0.289)\n", sd(samples_single)))
cat(sprintf("Parallel sd: %.6f (expected: ~0.289)\n", sd(samples_parallel)))

# Test with different buffer sizes
cat("\n=== Testing different buffer sizes ===\n")
buffer_sizes <- c(1024, 10000, 100000, 500000)
for (size in buffer_sizes) {
  # Single-threaded
  updatePRNG(list(
    buffer_size = size,
    use_parallel_filling = FALSE,
    use_threading = FALSE
  ))
  time_s <- system.time(generatePRNG(100000))

  # Parallel
  updatePRNG(list(
    buffer_size = size,
    use_parallel_filling = TRUE,
    use_threading = TRUE
  ))
  time_p <- system.time(generatePRNG(100000))

  cat(sprintf(
    "Buffer size %7d: Single=%.3fs, Parallel=%.3fs, Speedup=%.2fx\n",
    size, time_s[["elapsed"]], time_p[["elapsed"]],
    time_s[["elapsed"]] / time_p[["elapsed"]]
  ))
}

cat("\nOpenMP test complete!\n")
