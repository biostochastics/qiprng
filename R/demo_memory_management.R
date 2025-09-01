# Demo: Memory Management System for PRNG Testing
# -----------------------------------------------

# Load required modules
source("R/statisticaltests/memory_manager.R")
source("R/statisticaltests/config_manager.R")

# Demo 1: Basic Memory Monitoring
cat("=== Demo 1: Basic Memory Monitoring ===\n")

# Create memory manager
mem_manager <- MemoryManager$new(list(
  memory_limit_mb = 2048,
  gc_threshold_mb = 512,
  verbose = TRUE
))

# Start monitoring
mem_manager$start_monitoring()

# Create some large objects
cat("\nCreating large objects...\n")
large_vector <- runif(1e7) # ~76 MB
mem_manager$track_object_size("large_vector", large_vector, "demo")

large_matrix <- matrix(runif(5e6), nrow = 5000) # ~38 MB
mem_manager$track_object_size("large_matrix", large_matrix, "demo")

# Check memory
mem_check <- mem_manager$check_memory()
cat(sprintf("\nCurrent memory usage: %.1f MB\n", mem_check$memory_info$used_mb))

# Demo 2: Automatic Garbage Collection
cat("\n=== Demo 2: Automatic Garbage Collection ===\n")

# Create more objects to trigger GC
for (i in 1:5) {
  temp_data <- runif(5e6)
  mem_manager$track_object_size(paste0("temp_data_", i), temp_data, "temp")

  # Check memory (will trigger GC if threshold exceeded)
  mem_manager$check_memory()
}

# Clean up temp objects
rm(temp_data)
mem_manager$collect_garbage()

# Demo 3: Chunked Processing
cat("\n=== Demo 3: Memory-Efficient Chunked Processing ===\n")

# Define a processing function
process_chunk <- function(chunk) {
  # Simulate some computation
  result <- mean(chunk) + sd(chunk)
  return(result)
}

# Process large dataset in chunks
large_data <- runif(2e7) # ~152 MB
cat("Processing large dataset in chunks...\n")

results <- mem_manager$process_in_chunks(
  data = large_data,
  process_func = process_chunk,
  chunk_size = 1e6,
  gc_between_chunks = TRUE
)

cat(sprintf(
  "Processed %d chunks, results length: %d\n",
  ceiling(length(large_data) / 1e6), length(results)
))

# Clean up
rm(large_data)
mem_manager$collect_garbage()

# Demo 4: Memory-Aware Function Wrapping
cat("\n=== Demo 4: Memory-Aware Function Wrapping ===\n")

# Original function that uses memory
memory_intensive_function <- function(n) {
  data <- matrix(runif(n), ncol = 100)
  result <- colMeans(data)
  return(result)
}

# Wrap with memory monitoring
wrapped_function <- mem_manager$wrap_function(
  memory_intensive_function,
  check_before = TRUE,
  check_after = TRUE,
  gc_after = TRUE
)

# Call wrapped function
cat("Calling memory-intensive function with monitoring...\n")
result <- wrapped_function(1e6)
cat(sprintf("Function returned %d values\n", length(result)))

# Demo 5: Integration with Test Suite
cat("\n=== Demo 5: Integration with Test Suite ===\n")

# Simulate a test suite with memory management
run_test_with_memory_management <- function(test_name, sample_size) {
  cat(sprintf("\nRunning test: %s\n", test_name))

  # Track test data
  test_data <- runif(sample_size)
  mem_manager$track_object_size(paste0(test_name, "_data"), test_data, "tests")

  # Simulate test computation
  test_result <- list(
    mean = mean(test_data),
    sd = sd(test_data),
    p_value = runif(1)
  )

  # Clean up test data
  rm(test_data)

  # Check memory after test
  mem_info <- mem_manager$check_memory()

  return(test_result)
}

# Run multiple tests
test_sizes <- c(1e6, 2e6, 5e6)
test_names <- c("uniformity_test", "correlation_test", "entropy_test")

for (i in seq_along(test_names)) {
  result <- run_test_with_memory_management(test_names[i], test_sizes[i])
  cat(sprintf("Test result: p-value = %.4f\n", result$p_value))
}

# Demo 6: Memory Usage Summary
cat("\n=== Demo 6: Memory Usage Summary ===\n")

# Get summary
summary_text <- mem_manager$get_summary("text")
cat("\n", summary_text, "\n")

# Get summary as data frame
summary_df <- mem_manager$get_summary("data.frame")
cat("\nSummary metrics:\n")
print(summary_df)

# Demo 7: Chunked Random Number Generation
cat("\n=== Demo 7: Chunked Random Number Generation ===\n")

# Generate large amount of random numbers efficiently
cat("Generating 50 million random numbers in chunks...\n")
start_time <- Sys.time()

random_numbers <- generate_random_chunked(
  n = 5e7,
  generator = runif,
  chunk_size = 5e6,
  memory_manager = mem_manager
)

end_time <- Sys.time()
cat(sprintf(
  "Generated %d numbers in %.2f seconds\n",
  length(random_numbers),
  as.numeric(difftime(end_time, start_time, units = "secs"))
))

# Clean up
rm(random_numbers)
mem_manager$collect_garbage()

# Demo 8: Object Size Tracking Report
cat("\n=== Demo 8: Object Size Tracking Report ===\n")

# Get largest objects
largest <- mem_manager$stats$object_sizes
if (length(largest) > 0) {
  cat("\nTracked objects:\n")
  for (name in names(largest)) {
    obj <- largest[[name]]
    cat(sprintf(
      "- %s: %.1f MB (%s, created at %s)\n",
      obj$name, obj$size_mb, obj$class,
      format(obj$timestamp, "%H:%M:%S")
    ))
  }
}

# Clean old tracked objects
mem_manager$clean_tracked_objects(pattern = "temp", older_than = 300)
cat("\nCleaned temporary objects older than 5 minutes\n")

# Final cleanup
cat("\n=== Final Cleanup ===\n")
mem_manager$stop_monitoring()

# Remove large objects
rm(large_vector, large_matrix)
final_gc <- gc(verbose = FALSE)

cat("\nMemory management demo complete!\n")
cat("\nKey features demonstrated:\n")
cat("- Real-time memory monitoring\n")
cat("- Automatic garbage collection based on thresholds\n")
cat("- Memory-efficient chunked processing\n")
cat("- Function wrapping with memory checks\n")
cat("- Object size tracking and reporting\n")
cat("- Integration with test suites\n")
cat("- Chunked random number generation\n")
