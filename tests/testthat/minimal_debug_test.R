#!/usr/bin/env Rscript

# Minimal testing script with explicit configuration for QIPRNG
library(qiprng)

cat("\n===== Minimal Debug Test for QIPRNG =====\n\n")

# Test with non-threaded mode
basic_test <- function() {
  cat("Testing with explicit uniform_01 config (non-threaded)...\n")
  
  # Create a very explicit configuration to avoid any defaults
  cfg <- list(
    a = 2L,                    # Quadratic coefficient
    b = 5L,                    # Linear coefficient
    c = -2L,                   # Constant term
    mpfr_precision = 53L,      # Double precision
    buffer_size = 1024L,       # Small buffer for testing
    distribution = "uniform_01",
    normal_method = "box_muller",
    use_threading = FALSE,     # Disable threading
    use_parallel_filling = FALSE,
    use_csv_discriminants = FALSE, # Don't use CSV
    debug = TRUE
  )
  
  tryCatch({
    cat("  Creating PRNG...\n")
    createPRNG(cfg)
    
    cat("  Generating values...\n")
    values <- generatePRNG(5)
    cat("  Values:", paste(values, collapse=", "), "\n")
    
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

# Test with threaded mode
threaded_test <- function() {
  cat("Testing with explicit uniform_01 config (thread-safe)...\n")
  
  # Create a very explicit configuration with threading enabled
  cfg <- list(
    a = 2L,                    # Quadratic coefficient
    b = 5L,                    # Linear coefficient
    c = -2L,                   # Constant term
    mpfr_precision = 53L,      # Double precision
    buffer_size = 1024L,       # Small buffer for testing
    distribution = "uniform_01",
    normal_method = "box_muller",
    use_threading = TRUE,      # Enable threading
    use_parallel_filling = FALSE,
    use_csv_discriminants = FALSE, # Don't use CSV
    debug = TRUE
  )
  
  tryCatch({
    cat("  Creating PRNG...\n")
    createPRNG(cfg)
    
    cat("  Generating values...\n")
    values <- generatePRNG(5)
    cat("  Values:", paste(values, collapse=", "), "\n")
    
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

# Test with non-threaded Ziggurat
ziggurat_test <- function() {
  cat("Testing with normal distribution using Ziggurat method...\n")
  
  # Create a configuration with Ziggurat method
  cfg <- list(
    a = 2L,                    # Quadratic coefficient
    b = 5L,                    # Linear coefficient
    c = -2L,                   # Constant term
    mpfr_precision = 53L,      # Double precision
    buffer_size = 1024L,       # Small buffer for testing
    distribution = "normal",
    normal_method = "ziggurat",
    use_threading = FALSE,     # Disable threading
    use_parallel_filling = FALSE,
    use_csv_discriminants = FALSE, # Don't use CSV
    debug = TRUE
  )
  
  tryCatch({
    cat("  Creating PRNG...\n")
    createPRNG(cfg)
    
    cat("  Generating values...\n")
    values <- generatePRNG(5)
    cat("  Values:", paste(values, collapse=", "), "\n")
    
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

# Test with threaded Ziggurat
threaded_ziggurat_test <- function() {
  cat("Testing with normal distribution using Ziggurat method (thread-safe)...\n")
  
  # Create a configuration with Ziggurat method and threading
  cfg <- list(
    a = 2L,                    # Quadratic coefficient
    b = 5L,                    # Linear coefficient
    c = -2L,                   # Constant term
    mpfr_precision = 53L,      # Double precision
    buffer_size = 1024L,       # Small buffer for testing
    distribution = "normal",
    normal_method = "ziggurat",
    use_threading = TRUE,      # Enable threading
    use_parallel_filling = FALSE,
    use_csv_discriminants = FALSE, # Don't use CSV
    debug = TRUE
  )
  
  tryCatch({
    cat("  Creating PRNG...\n")
    createPRNG(cfg)
    
    cat("  Generating values...\n")
    values <- generatePRNG(5)
    cat("  Values:", paste(values, collapse=", "), "\n")
    
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

# Run all tests
non_threaded_result <- basic_test()
threaded_result <- threaded_test()
ziggurat_result <- ziggurat_test()
threaded_ziggurat_result <- threaded_ziggurat_test()

if (non_threaded_result && threaded_result && ziggurat_result && threaded_ziggurat_result) {
  cat("All tests PASSED!\n")
} else {
  cat("Some tests FAILED!\n")
  if (!non_threaded_result) cat("- Non-threaded test failed\n")
  if (!threaded_result) cat("- Thread-safe test failed\n")
  if (!ziggurat_result) cat("- Ziggurat test failed\n")
  if (!threaded_ziggurat_result) cat("- Thread-safe Ziggurat test failed\n")
}

cat("===== Test completed =====\n")