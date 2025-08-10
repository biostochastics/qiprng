#!/usr/bin/env Rscript

# Minimal test script for QIPRNG
library(qiprng)

cat("\n===== Minimal QIPRNG Test =====\n\n")

# Test the default configuration (uniform_01)
cat("Testing default configuration (uniform_01)...\n")
tryCatch({
  # Create PRNG with default parameters
  createPRNG()
  
  # Generate some values
  values <- generatePRNG(10)
  cat("Generated values:", paste(values, collapse=", "), "\n")
  
  # Clean up
  cleanup_prng()
  cat("SUCCESS\n\n")
}, error = function(e) {
  cat("ERROR:", e$message, "\n\n")
  cleanup_prng()
})

cat("===== Test completed =====\n")