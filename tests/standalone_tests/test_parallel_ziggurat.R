#!/usr/bin/env Rscript

# Test for parallel filling with Ziggurat method
library(qiprng)

cat("\n===== Parallel Filling with Ziggurat Test =====\n\n")

# Create PRNG with parallel filling and Ziggurat method (which will use Box-Muller automatically)
cat("Creating PRNG with parallel filling and Ziggurat method...\n")
cat("Note: When both threading and parallel filling are enabled, Ziggurat automatically falls back to Box-Muller\n")
createPRNG(list(
  distribution = "normal",
  normal_method = "ziggurat",  # This will automatically fall back to Box-Muller
  use_threading = TRUE,
  use_parallel_filling = TRUE,
  buffer_size = 4096,  # Larger buffer for better parallel performance
  debug = TRUE
))

# Generate values with error handling
cat("Generating values with error handling...\n")
result <- tryCatch({
  values <- generatePRNG(5000)
  
  # Calculate statistics
  mean_val <- mean(values)
  var_val <- var(values)
  min_val <- min(values)
  max_val <- max(values)
  has_nan <- any(is.nan(values))
  has_inf <- any(is.infinite(values))
  
  # Print results
  cat("Generated", length(values), "values successfully\n")
  cat("Mean:", mean_val, "(expect ~0)\n")
  cat("Variance:", var_val, "(expect ~1)\n")
  cat("Range: [", min_val, ",", max_val, "]\n")
  cat("NaN values:", has_nan, "\n")
  cat("Infinity values:", has_inf, "\n")
  
  TRUE
}, error = function(e) {
  cat("ERROR:", e$message, "\n")
  FALSE
})

# Clean up
cat("Cleaning up...\n")
cleanup_prng()

# Try with fallback Box-Muller if Ziggurat failed
if (!result) {
  cat("\nFalling back to Box-Muller with parallel filling...\n")
  
  createPRNG(list(
    distribution = "normal",
    normal_method = "box_muller",
    use_threading = TRUE,
    use_parallel_filling = TRUE,
    buffer_size = 4096,
    debug = TRUE
  ))
  
  tryCatch({
    values <- generatePRNG(5000)
    
    # Calculate statistics
    mean_val <- mean(values)
    var_val <- var(values)
    min_val <- min(values)
    max_val <- max(values)
    has_nan <- any(is.nan(values))
    has_inf <- any(is.infinite(values))
    
    # Print results
    cat("Generated", length(values), "values successfully with Box-Muller\n")
    cat("Mean:", mean_val, "(expect ~0)\n")
    cat("Variance:", var_val, "(expect ~1)\n")
    cat("Range: [", min_val, ",", max_val, "]\n")
    cat("NaN values:", has_nan, "\n")
    cat("Infinity values:", has_inf, "\n")
    
  }, error = function(e) {
    cat("ERROR with Box-Muller fallback:", e$message, "\n")
  })
  
  # Clean up
  cleanup_prng()
}

cat("\n===== Parallel Filling Test Completed! =====\n")