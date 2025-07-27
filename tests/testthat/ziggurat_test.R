#!/usr/bin/env Rscript

# Testing script for Ziggurat normal distribution in QIPRNG
library(qiprng)

cat("\n===== Ziggurat Normal Distribution Test for QIPRNG =====\n\n")

# Test with Ziggurat method
ziggurat_test <- function() {
  cat("Testing normal distribution with Ziggurat method...\n")
  
  # Create a configuration with Ziggurat method
  cfg <- list(
    a = 2L,                    # Quadratic coefficient
    b = 5L,                    # Linear coefficient
    c = -2L,                   # Constant term
    mpfr_precision = 53L,      # Double precision
    buffer_size = 1024L,       # Small buffer for testing
    distribution = "normal",
    normal_method = "ziggurat",
    normal_mean = 0,
    normal_sd = 1,
    use_threading = FALSE,     # Disable threading
    use_parallel_filling = FALSE,
    use_csv_discriminants = FALSE, # Don't use CSV
    debug = TRUE
  )
  
  tryCatch({
    cat("  Creating PRNG...\n")
    createPRNG(cfg)
    
    cat("  Generating values...\n")
    values <- generatePRNG(1000)
    
    # Calculate statistics
    cat("  Mean:", mean(values), "\n")
    cat("  Variance:", var(values), "\n")
    cat("  Range:", min(values), "to", max(values), "\n")
    cat("  NaN values:", sum(is.nan(values)), "\n")
    cat("  Infinity values:", sum(is.infinite(values)), "\n")
    
    cat("  Cleaning up...\n")
    cleanup_prng()
    
    cat("SUCCESS\n\n")
    return(TRUE)
  }, error = function(e) {
    cat("ERROR:", e$message, "\n\n")
    cleanup_prng()
    return(FALSE)
  })
}

# Test with Ziggurat method in threading mode
ziggurat_threaded_test <- function() {
  cat("Testing normal distribution with Ziggurat method (threading enabled)...\n")
  
  # Create a configuration with Ziggurat method
  cfg <- list(
    a = 2L,                    # Quadratic coefficient
    b = 5L,                    # Linear coefficient
    c = -2L,                   # Constant term
    mpfr_precision = 53L,      # Double precision
    buffer_size = 1024L,       # Small buffer for testing
    distribution = "normal",
    normal_method = "ziggurat",
    normal_mean = 0,
    normal_sd = 1,
    use_threading = TRUE,      # Enable threading
    use_parallel_filling = FALSE,
    use_csv_discriminants = FALSE, # Don't use CSV
    debug = TRUE
  )
  
  tryCatch({
    cat("  Creating PRNG...\n")
    createPRNG(cfg)
    
    cat("  Generating values...\n")
    values <- generatePRNG(1000)
    
    # Calculate statistics
    cat("  Mean:", mean(values), "\n")
    cat("  Variance:", var(values), "\n")
    cat("  Range:", min(values), "to", max(values), "\n")
    cat("  NaN values:", sum(is.nan(values)), "\n")
    cat("  Infinity values:", sum(is.infinite(values)), "\n")
    
    cat("  Cleaning up...\n")
    cleanup_prng()
    
    cat("SUCCESS\n\n")
    return(TRUE)
  }, error = function(e) {
    cat("ERROR:", e$message, "\n\n")
    cleanup_prng()
    return(FALSE)
  })
}

# Test with Ziggurat method in large batch
ziggurat_large_batch_test <- function() {
  cat("Testing normal distribution with Ziggurat method (large batch)...\n")
  
  # Create a configuration with Ziggurat method
  cfg <- list(
    a = 2L,                    # Quadratic coefficient
    b = 5L,                    # Linear coefficient
    c = -2L,                   # Constant term
    mpfr_precision = 53L,      # Double precision
    buffer_size = 4096L,       # Larger buffer for batch testing
    distribution = "normal",
    normal_method = "ziggurat",
    normal_mean = 0,
    normal_sd = 1,
    use_threading = TRUE,      # Enable threading
    use_parallel_filling = FALSE,
    use_csv_discriminants = FALSE, # Don't use CSV
    debug = TRUE
  )
  
  tryCatch({
    cat("  Creating PRNG...\n")
    createPRNG(cfg)
    
    cat("  Generating large batch of values (10,000)...\n")
    values <- generatePRNG(10000)
    
    # Calculate statistics
    cat("  Mean:", mean(values), "\n")
    cat("  Variance:", var(values), "\n")
    cat("  Range:", min(values), "to", max(values), "\n")
    cat("  NaN values:", sum(is.nan(values)), "\n")
    cat("  Infinity values:", sum(is.infinite(values)), "\n")
    
    cat("  Cleaning up...\n")
    cleanup_prng()
    
    cat("SUCCESS\n\n")
    return(TRUE)
  }, error = function(e) {
    cat("ERROR:", e$message, "\n\n")
    cleanup_prng()
    return(FALSE)
  })
}

# Run tests
cat("Running basic Ziggurat test...\n")
ziggurat_result <- ziggurat_test()

cat("Running threaded Ziggurat test...\n")
ziggurat_threaded_result <- ziggurat_threaded_test()

cat("Running large batch Ziggurat test...\n")
ziggurat_large_batch_result <- ziggurat_large_batch_test()

# Print summary
cat("\n===== Test Results =====\n")
cat("Basic Ziggurat test:", if(ziggurat_result) "PASSED" else "FAILED", "\n")
cat("Threaded Ziggurat test:", if(ziggurat_threaded_result) "PASSED" else "FAILED", "\n")
cat("Large batch Ziggurat test:", if(ziggurat_large_batch_result) "PASSED" else "FAILED", "\n")

# Final status
if (ziggurat_result && ziggurat_threaded_result && ziggurat_large_batch_result) {
  cat("\nAll Ziggurat tests PASSED!\n")
} else {
  cat("\nSome Ziggurat tests FAILED!\n")
}

cat("===== Test completed =====\n")