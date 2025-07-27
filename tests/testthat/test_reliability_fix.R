#!/usr/bin/env Rscript

# Test script to fix test reliability issues
library(qiprng)

cat("\n===== Test Reliability Improvement =====\n\n")

# Helper function to ensure clean PRNG state for each test
reset_prng_state <- function() {
  # Attempt to clean up any existing PRNG
  tryCatch({
    cleanup_prng()
  }, error = function(e) {
    # Ignore errors if no PRNG exists
  })
  
  # Force garbage collection to clean up resources
  gc()
}

# Run a series of tests with isolated PRNG state
run_isolated_test <- function(test_name, config) {
  cat(paste0("Running test: ", test_name, "...\n"))
  
  # Reset PRNG state to ensure clean state
  reset_prng_state()
  
  # Run the test with error handling
  result <- tryCatch({
    # Create PRNG with specified configuration
    createPRNG(config)
    
    # Generate values
    values <- generatePRNG(1000)
    
    # Calculate basic statistics
    stats <- list(
      mean = mean(values),
      variance = var(values),
      min = min(values),
      max = max(values)
    )
    
    # Clean up
    cleanup_prng()
    
    # Return success with statistics
    list(
      success = TRUE,
      stats = stats
    )
  }, error = function(e) {
    # Return failure with error message
    list(
      success = FALSE,
      error = e$message
    )
  })
  
  # Print results
  if (result$success) {
    cat(paste0("  Test completed successfully\n"))
    cat(paste0("  Mean: ", result$stats$mean, "\n"))
    cat(paste0("  Variance: ", result$stats$variance, "\n"))
    cat(paste0("  Range: [", result$stats$min, ", ", result$stats$max, "]\n"))
  } else {
    cat(paste0("  TEST FAILED: ", result$error, "\n"))
  }
  
  # Ensure clean state before returning
  reset_prng_state()
  
  cat("\n")
  return(result$success)
}

# Define tests with different configurations
tests <- list(
  list(
    name = "Uniform distribution",
    config = list(
      distribution = "uniform_01",
      debug = TRUE
    )
  ),
  list(
    name = "Normal distribution",
    config = list(
      distribution = "normal",
      normal_method = "box_muller",
      debug = TRUE
    )
  ),
  list(
    name = "Threaded uniform",
    config = list(
      distribution = "uniform_01",
      use_threading = TRUE,
      use_parallel_filling = FALSE,
      debug = TRUE
    )
  ),
  list(
    name = "Threaded normal",
    config = list(
      distribution = "normal",
      normal_method = "box_muller",
      use_threading = TRUE,
      use_parallel_filling = FALSE,
      debug = TRUE
    )
  ),
  list(
    name = "Custom parameters",
    config = list(
      a = 3,
      b = 7,
      c = -2,
      mpfr_precision = 64,
      debug = TRUE
    )
  )
)

# Run all tests
results <- list()
for (test in tests) {
  results[[test$name]] <- run_isolated_test(test$name, test$config)
}

# Print summary
cat("===== Test Reliability Summary =====\n\n")
all_passed <- TRUE
for (name in names(results)) {
  passed <- results[[name]]
  cat(paste0(name, ": ", ifelse(passed, "PASSED", "FAILED"), "\n"))
  if (!passed) all_passed <- FALSE
}

cat("\nOverall test suite: ", ifelse(all_passed, "PASSED", "FAILED"), "\n")
cat("\n===== Test Reliability Improvement Completed! =====\n")