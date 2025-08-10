#!/usr/bin/env Rscript

# Test script for Ziggurat method for normal distribution
library(qiprng)

cat("\n===== Ziggurat Method Test =====\n\n")

# Helper function to test normal distribution with different methods
test_normal_distribution <- function(method_name, thread_safe = FALSE, parallel = FALSE) {
  cat(paste0("Testing normal distribution with ", method_name, " method"))
  if (thread_safe) cat(" (thread-safe mode)")
  if (parallel) cat(" (parallel filling)")
  cat("...\n")
  
  # Create PRNG with the specified configuration
  cfg <- list(
    distribution = "normal",
    normal_method = method_name,
    use_threading = thread_safe,
    use_parallel_filling = parallel,
    debug = TRUE
  )
  
  # Try to create and use the PRNG
  result <- tryCatch({
    cat("  Creating PRNG... ")
    createPRNG(cfg)
    cat("SUCCESS\n")
    
    cat("  Generating values... ")
    values <- generatePRNG(10000)
    cat("SUCCESS\n")
    
    # Calculate statistics
    mean_val <- mean(values)
    var_val <- var(values)
    min_val <- min(values)
    max_val <- max(values)
    has_nan <- any(is.nan(values))
    has_inf <- any(is.infinite(values))
    
    cat("  Mean:", mean_val, "(expect ~0)\n")
    cat("  Variance:", var_val, "(expect ~1)\n")
    cat("  Range:", min_val, "to", max_val, "\n")
    cat("  NaN values:", has_nan, "\n")
    cat("  Infinity values:", has_inf, "\n")
    
    # Normality test
    if (requireNamespace("nortest", quietly = TRUE)) {
      ad_test <- nortest::ad.test(values)
      cat("  Anderson-Darling normality test p-value:", ad_test$p.value, "\n")
    }
    
    cat("  Cleaning up... ")
    cleanup_prng()
    cat("SUCCESS\n")
    
    list(
      success = TRUE,
      mean = mean_val,
      variance = var_val,
      min = min_val,
      max = max_val,
      has_nan = has_nan,
      has_inf = has_inf
    )
  }, error = function(e) {
    cat("ERROR:", e$message, "\n")
    list(
      success = FALSE,
      error = e$message
    )
  })
  
  cat("\n")
  return(result)
}

# Test Ziggurat method in various configurations
results <- list()

# Basic ziggurat test
results$ziggurat_basic <- test_normal_distribution("ziggurat")

# Box-Muller test for comparison
results$box_muller_basic <- test_normal_distribution("box_muller")

# Thread-safe Ziggurat test
results$ziggurat_threaded <- test_normal_distribution("ziggurat", thread_safe = TRUE)

# Thread-safe Box-Muller test
results$box_muller_threaded <- test_normal_distribution("box_muller", thread_safe = TRUE)

# Thread-safe Ziggurat with parallel filling (should auto-use Box-Muller)
cat("Note: When both threading and parallel filling are enabled, Ziggurat method automatically falls back to Box-Muller\n")
results$ziggurat_parallel <- test_normal_distribution("ziggurat", thread_safe = TRUE, parallel = TRUE)

# Thread-safe Box-Muller with parallel filling
results$box_muller_parallel <- test_normal_distribution("box_muller", thread_safe = TRUE, parallel = TRUE)

# Print summary
cat("\n===== Ziggurat Method Test Summary =====\n\n")

check_values <- function(result, name) {
  if (!result$success) {
    cat(name, ": FAILED - ", result$error, "\n")
    return(FALSE)
  }
  
  # Check for NaN/Inf
  if (result$has_nan || result$has_inf) {
    cat(name, ": FAILED - Contains NaN or Infinity values\n")
    return(FALSE)
  }
  
  # Check mean and variance are reasonable
  if (abs(result$mean) > 0.1) {
    cat(name, ": FAILED - Mean deviates too much from expected (", result$mean, ")\n")
    return(FALSE)
  }
  
  if (abs(result$variance - 1) > 0.2) {
    cat(name, ": FAILED - Variance deviates too much from expected (", result$variance, ")\n")
    return(FALSE)
  }
  
  cat(name, ": PASSED\n")
  return(TRUE)
}

all_passed <- TRUE
all_passed <- check_values(results$ziggurat_basic, "Ziggurat basic") && all_passed
all_passed <- check_values(results$box_muller_basic, "Box-Muller basic") && all_passed
all_passed <- check_values(results$ziggurat_threaded, "Ziggurat thread-safe") && all_passed
all_passed <- check_values(results$box_muller_threaded, "Box-Muller thread-safe") && all_passed
all_passed <- check_values(results$ziggurat_parallel, "Ziggurat with parallel filling") && all_passed
all_passed <- check_values(results$box_muller_parallel, "Box-Muller with parallel filling") && all_passed

cat("\nOverall Ziggurat Test:", ifelse(all_passed, "PASSED", "FAILED"), "\n")
cat("\n===== Test Completed! =====\n")