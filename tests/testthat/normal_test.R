#!/usr/bin/env Rscript

# Testing script for normal distribution in QIPRNG
library(qiprng)

cat("\n===== Normal Distribution Test for QIPRNG =====\n\n")

# Test with Box-Muller method
box_muller_test <- function() {
  cat("Testing normal distribution with Box-Muller method...\n")
  
  # Create a configuration with Box-Muller method
  cfg <- list(
    a = 2L,                    # Quadratic coefficient
    b = 5L,                    # Linear coefficient
    c = -2L,                   # Constant term
    mpfr_precision = 53L,      # Double precision
    buffer_size = 1024L,       # Small buffer for testing
    distribution = "normal",
    normal_method = "box_muller",
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
    values <- generatePRNG(10)
    
    # Calculate statistics
    cat("  Mean:", mean(values), "\n")
    cat("  Variance:", var(values), "\n")
    cat("  Range:", min(values), "to", max(values), "\n")
    cat("  NaN values:", any(is.nan(values)), "\n")
    cat("  Infinity values:", any(is.infinite(values)), "\n")
    
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

# Run test
box_muller_result <- box_muller_test()

if (box_muller_result) {
  cat("Normal distribution with Box-Muller PASSED!\n")
} else {
  cat("Normal distribution with Box-Muller FAILED!\n")
}

cat("===== Test completed =====\n")