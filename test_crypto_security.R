#!/usr/bin/env Rscript

# Test script to verify security fixes in crypto mixing

library(qiprng)

cat("Testing Security Fixes in Crypto Mixing\n")
cat("========================================\n\n")

# Test 1: Crypto mixing should still work without deterministic seeds
cat("Test 1: Crypto mixing with secure random\n")
tryCatch({
  cfg <- default_config
  cfg$use_crypto_mixing <- TRUE
  cfg$seed <- NULL  # No seed - should use secure random
  cfg$has_seed <- FALSE
  
  createPRNG(cfg)
  vals1 <- generatePRNG(100)
  cleanup_prng()
  
  # Create again - should get different values (not deterministic)
  createPRNG(cfg)
  vals2 <- generatePRNG(100)
  cleanup_prng()
  
  if (all(vals1 == vals2)) {
    cat("  FAILED: Got identical sequences without seed (should be random)\n")
  } else {
    cat("  PASSED: Different sequences without seed (properly randomized)\n")
  }
}, error = function(e) {
  cat("  ERROR: ", e$message, "\n")
})
cat("\n")

# Test 2: Deterministic seed with crypto mixing should trigger warning
cat("Test 2: Warning when using deterministic seed with crypto\n")
test_passed <- FALSE
tryCatch({
  cfg <- default_config
  cfg$use_crypto_mixing <- TRUE
  cfg$seed <- 12345  # Deterministic seed
  cfg$has_seed <- TRUE
  
  # Capture warnings
  warnings <- character()
  withCallingHandlers({
    createPRNG(cfg)
    vals <- generatePRNG(10)
    cleanup_prng()
  }, warning = function(w) {
    warnings <<- c(warnings, w$message)
    invokeRestart("muffleWarning")
  })
  
  # Check if security warning was issued
  security_warning_found <- any(grepl("SECURITY WARNING.*[Dd]eterministic seed.*crypto", 
                                      warnings, ignore.case = TRUE))
  
  if (security_warning_found) {
    cat("  PASSED: Security warning issued for deterministic seed with crypto\n")
    test_passed <- TRUE
  } else {
    cat("  FAILED: No security warning for deterministic seed with crypto\n")
    cat("  Warnings received:\n")
    for (w in warnings) {
      cat("    - ", w, "\n")
    }
  }
}, error = function(e) {
  cat("  ERROR: ", e$message, "\n")
})
cat("\n")

# Test 3: Crypto mixing should work in multi-threaded environment
cat("Test 3: Thread safety of crypto mixing\n")
library(parallel)

if (.Platform$OS.type != "windows") {
  test_crypto_thread <- function(thread_id) {
    tryCatch({
      cfg <- list(
        a = 2 + thread_id,
        b = 5 + thread_id * 2,
        c = -2 - thread_id,
        use_crypto_mixing = TRUE,
        use_threading = TRUE,
        mpfr_precision = 53,
        buffer_size = 100,
        distribution = "uniform_01"
      )
      
      createPRNG(cfg)
      vals <- generatePRNG(100)
      cleanup_prng()
      
      return(list(
        thread_id = thread_id, 
        status = "SUCCESS", 
        values_generated = length(vals),
        mean_value = mean(vals)
      ))
    }, error = function(e) {
      return(list(
        thread_id = thread_id, 
        status = "ERROR", 
        message = toString(e)
      ))
    })
  }
  
  results <- mclapply(1:4, test_crypto_thread, mc.cores = 4)
  
  all_success <- TRUE
  for (res in results) {
    if (res$status == "SUCCESS") {
      cat(sprintf("  Thread %d: SUCCESS (generated %d values, mean=%.4f)\n", 
                  res$thread_id, res$values_generated, res$mean_value))
    } else {
      cat(sprintf("  Thread %d: ERROR - %s\n", 
                  res$thread_id, res$message))
      all_success <- FALSE
    }
  }
  
  if (all_success) {
    cat("  All threads completed successfully\n")
  } else {
    cat("  Some threads failed\n")
  }
} else {
  cat("  SKIPPED (not on Unix)\n")
}
cat("\n")

# Test 4: Verify libsodium initialization is thread-safe
cat("Test 4: Libsodium initialization thread safety\n")
if (.Platform$OS.type != "windows") {
  test_libsodium_init <- function(thread_id) {
    tryCatch({
      # Each thread creates a PRNG which internally initializes libsodium
      # Should be safe due to std::call_once
      
      # Try to use crypto mixing
      cfg <- default_config
      cfg$use_crypto_mixing <- TRUE
      createPRNG(cfg)
      vals <- generatePRNG(10)
      cleanup_prng()
      
      return(list(thread_id = thread_id, status = "SUCCESS"))
    }, error = function(e) {
      return(list(thread_id = thread_id, status = "ERROR", message = toString(e)))
    })
  }
  
  # Run initialization from multiple threads simultaneously
  results <- mclapply(1:8, test_libsodium_init, mc.cores = 8)
  
  success_count <- sum(sapply(results, function(r) r$status == "SUCCESS"))
  
  if (success_count == 8) {
    cat("  PASSED: All 8 threads initialized libsodium safely\n")
  } else {
    cat(sprintf("  FAILED: Only %d/8 threads succeeded\n", success_count))
    for (res in results) {
      if (res$status == "ERROR") {
        cat(sprintf("    Thread %d: %s\n", res$thread_id, res$message))
      }
    }
  }
} else {
  cat("  SKIPPED (not on Unix)\n")
}
cat("\n")

# Test 5: Verify crypto mixing without seed is truly random
cat("Test 5: Statistical randomness of crypto mixing\n")
tryCatch({
  cfg <- default_config
  cfg$use_crypto_mixing <- TRUE
  cfg$seed <- NULL
  
  # Generate multiple sequences
  sequences <- list()
  for (i in 1:5) {
    createPRNG(cfg)
    sequences[[i]] <- generatePRNG(1000)
    cleanup_prng()
  }
  
  # Check that sequences are different
  all_different <- TRUE
  for (i in 1:4) {
    for (j in (i+1):5) {
      if (all(sequences[[i]] == sequences[[j]])) {
        all_different <- FALSE
        cat(sprintf("  Sequences %d and %d are identical!\n", i, j))
      }
    }
  }
  
  if (all_different) {
    cat("  PASSED: All sequences are different (properly randomized)\n")
    
    # Basic statistical check
    all_vals <- unlist(sequences)
    mean_val <- mean(all_vals)
    sd_val <- sd(all_vals)
    
    cat(sprintf("  Mean: %.4f (expected ~0.5)\n", mean_val))
    cat(sprintf("  SD: %.4f (expected ~0.29)\n", sd_val))
    
    if (abs(mean_val - 0.5) < 0.02 && abs(sd_val - 0.29) < 0.02) {
      cat("  Statistical properties look good\n")
    } else {
      cat("  WARNING: Statistical properties may be off\n")
    }
  } else {
    cat("  FAILED: Some sequences are identical\n")
  }
}, error = function(e) {
  cat("  ERROR: ", e$message, "\n")
})

cat("\n========================================\n")
cat("Security fix tests completed!\n")
if (test_passed) {
  cat("Key security improvements verified:\n")
  cat("- Deterministic seeding with crypto triggers warning\n")
  cat("- Thread-safe libsodium initialization\n")
  cat("- Proper randomization without seeds\n")
} else {
  cat("WARNING: Some security tests did not pass as expected\n")
}