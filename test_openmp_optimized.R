#!/usr/bin/env Rscript

library(qiprng)

cat("Testing Optimized OpenMP Performance\n")
cat("=====================================\n\n")

# Test with increasingly large buffer sizes
buffer_sizes <- c(10000, 50000, 100000, 500000, 1000000, 5000000)
sample_sizes <- c(100000, 500000, 1000000, 5000000, 10000000, 10000000)

results <- data.frame(
  buffer_size = integer(),
  sample_size = integer(),
  single_time = numeric(),
  parallel_time = numeric(),
  speedup = numeric(),
  samples_per_sec_single = numeric(),
  samples_per_sec_parallel = numeric()
)

for (i in seq_along(buffer_sizes)) {
  buffer_size <- buffer_sizes[i]
  sample_size <- sample_sizes[i]

  cat(sprintf("\nTesting buffer size: %d, sample size: %d\n", buffer_size, sample_size))

  # Single-threaded configuration
  config_single <- list(
    buffer_size = buffer_size,
    use_parallel_filling = FALSE,
    use_threading = FALSE
  )

  # Parallel configuration (optimized OpenMP)
  config_parallel <- list(
    buffer_size = buffer_size,
    use_parallel_filling = TRUE,
    use_threading = TRUE
  )

  # Test single-threaded
  createPRNG(config_single)
  time_single <- system.time({
    samples_single <- generatePRNG(sample_size)
  })

  # Test parallel (optimized)
  updatePRNG(config_parallel)
  time_parallel <- system.time({
    samples_parallel <- generatePRNG(sample_size)
  })

  # Calculate metrics
  speedup <- time_single[["elapsed"]] / time_parallel[["elapsed"]]
  samples_per_sec_single <- sample_size / time_single[["elapsed"]]
  samples_per_sec_parallel <- sample_size / time_parallel[["elapsed"]]

  # Store results
  results <- rbind(results, data.frame(
    buffer_size = buffer_size,
    sample_size = sample_size,
    single_time = time_single[["elapsed"]],
    parallel_time = time_parallel[["elapsed"]],
    speedup = speedup,
    samples_per_sec_single = samples_per_sec_single,
    samples_per_sec_parallel = samples_per_sec_parallel
  ))

  cat(sprintf(
    "  Single-threaded: %.3fs (%.0f samples/sec)\n",
    time_single[["elapsed"]], samples_per_sec_single
  ))
  cat(sprintf(
    "  Parallel:        %.3fs (%.0f samples/sec)\n",
    time_parallel[["elapsed"]], samples_per_sec_parallel
  ))
  cat(sprintf("  Speedup:         %.2fx\n", speedup))
}

cat("\n\nPerformance Summary\n")
cat("===================\n")
print(results)

# Calculate average speedup
avg_speedup <- mean(results$speedup)
cat(sprintf("\nAverage speedup: %.2fx\n", avg_speedup))

# Memory efficiency test
cat("\n\nMemory Efficiency Test\n")
cat("======================\n")

# Get initial memory usage
gc()
mem_before <- as.numeric(gc()[2, 2])

# Generate large batch with optimized parallel
config_large <- list(
  buffer_size = 5000000,
  use_parallel_filling = TRUE,
  use_threading = TRUE
)
updatePRNG(config_large)
large_samples <- generatePRNG(50000000)

# Get memory after
gc()
mem_after <- as.numeric(gc()[2, 2])
mem_used <- mem_after - mem_before

cat(sprintf("Memory used for 50M samples: %.1f MB\n", mem_used))
cat(sprintf("Memory per million samples: %.2f MB\n", mem_used / 50))

# Verify quality is maintained
cat("\n\nQuality Verification\n")
cat("===================\n")
cat(sprintf("Mean: %.6f (expected: ~0.5)\n", mean(large_samples[1:1000000])))
cat(sprintf("SD:   %.6f (expected: ~0.289)\n", sd(large_samples[1:1000000])))

# Check for any NaN or Inf values
if (any(is.nan(large_samples) | is.infinite(large_samples))) {
  cat("WARNING: Invalid values detected!\n")
} else {
  cat("All values are valid.\n")
}

cat("\nOptimized OpenMP test complete!\n")
