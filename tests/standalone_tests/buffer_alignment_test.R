#!/usr/bin/env Rscript

# Test script for buffer alignment and performance
library(qiprng)
library(microbenchmark)

cat("\n===== Buffer Alignment Test =====\n\n")

# Function to generate random numbers and measure performance
test_buffer_performance <- function(buffer_size, iterations = 10) {
  cat(paste0("Testing buffer size: ", buffer_size, "\n"))

  # Create PRNG with specified buffer size
  createPRNG(list(
    buffer_size = buffer_size,
    debug = TRUE
  ))

  # Measure performance for generating a large number of values
  perf_results <- microbenchmark(
    generate = generatePRNG(buffer_size * 10),
    times = iterations
  )

  cleanup_prng()

  return(perf_results)
}

# Test with different buffer sizes
buffer_sizes <- c(128, 256, 512, 1024, 2048, 4096, 8192)
results <- list()

for (size in buffer_sizes) {
  results[[as.character(size)]] <- test_buffer_performance(size)
}

# Print summarized results
cat("\n===== Buffer Performance Results =====\n\n")
for (size in names(results)) {
  perf <- results[[size]]
  cat(paste0("Buffer size: ", size, "\n"))
  cat(paste0("  Mean time: ", round(mean(perf$time) / 1e6, 3), " ms\n"))
  cat(paste0("  Min time:  ", round(min(perf$time) / 1e6, 3), " ms\n"))
  cat(paste0("  Max time:  ", round(max(perf$time) / 1e6, 3), " ms\n\n"))
}

# Test thread safety with aligned buffers
cat("Testing thread safety with aligned buffers...\n")
result_thread_safety <- tryCatch(
  {
    createPRNG(list(
      buffer_size = 4096, # Larger buffer to test alignment
      use_threading = TRUE,
      use_parallel_filling = FALSE, # Safe mode
      debug = TRUE
    ))

    # Generate values to test thread safety
    values <- generatePRNG(10000)

    cleanup_prng()

    cat("Successfully generated", length(values), "values with thread-safe aligned buffers\n")
    TRUE
  },
  error = function(e) {
    cat("ERROR:", e$message, "\n")
    FALSE
  }
)

cat("\n===== Buffer Alignment Test completed! =====\n")
cat("Thread safety test with aligned buffers:", ifelse(result_thread_safety, "PASSED", "FAILED"), "\n")
