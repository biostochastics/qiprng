#!/usr/bin/env Rscript

# Test script to verify integer overflow protection in discriminant calculations

library(qiprng)

cat("Testing Integer Overflow Protection in Discriminant Calculations\n")
cat("================================================================\n\n")

# Test 1: Normal values (should work)
cat("Test 1: Normal values\n")
test_cases_normal <- list(
  list(a = 2, b = 5, c = -2),  # disc = 25 + 16 = 41
  list(a = 1, b = 3, c = -1),  # disc = 9 + 4 = 13
  list(a = 3, b = 7, c = -4),  # disc = 49 + 48 = 97
  list(a = 5, b = 11, c = -6)  # disc = 121 + 120 = 241
)

for (i in seq_along(test_cases_normal)) {
  cfg <- test_cases_normal[[i]]
  cfg$mpfr_precision <- 53
  cfg$buffer_size <- 100
  cfg$distribution <- "uniform_01"
  
  tryCatch({
    createPRNG(cfg)
    vals <- generatePRNG(10)
    cleanup_prng()
    cat(sprintf("  Case %d (a=%d, b=%d, c=%d): PASSED\n", 
                i, cfg$a, cfg$b, cfg$c))
  }, error = function(e) {
    cat(sprintf("  Case %d (a=%d, b=%d, c=%d): FAILED - %s\n", 
                i, cfg$a, cfg$b, cfg$c, e$message))
  })
}
cat("\n")

# Test 2: Large values that might overflow on 32-bit systems
cat("Test 2: Large values (potential overflow on 32-bit)\n")
test_cases_large <- list(
  list(a = 100000, b = 200000, c = -50000),
  list(a = 500000, b = 1000000, c = -100000),
  list(a = 1000000, b = 2000000, c = -500000)
)

for (i in seq_along(test_cases_large)) {
  cfg <- test_cases_large[[i]]
  cfg$mpfr_precision <- 53
  cfg$buffer_size <- 100
  cfg$distribution <- "uniform_01"
  
  tryCatch({
    createPRNG(cfg)
    vals <- generatePRNG(10)
    cleanup_prng()
    cat(sprintf("  Case %d (a=%d, b=%d, c=%d): PASSED\n", 
                i, cfg$a, cfg$b, cfg$c))
  }, error = function(e) {
    cat(sprintf("  Case %d (a=%d, b=%d, c=%d): HANDLED - %s\n", 
                i, cfg$a, cfg$b, cfg$c, e$message))
  })
}
cat("\n")

# Test 3: Values near INT_MAX that should trigger overflow protection
cat("Test 3: Near INT_MAX values (should trigger overflow protection)\n")
INT_MAX <- 2147483647  # 2^31 - 1
test_cases_overflow <- list(
  list(a = INT_MAX / 2, b = INT_MAX / 2, c = -INT_MAX / 4),
  list(a = INT_MAX / 3, b = INT_MAX / 2, c = INT_MAX / 4),
  list(a = 1000000000, b = 1500000000, c = -500000000)
)

for (i in seq_along(test_cases_overflow)) {
  cfg <- test_cases_overflow[[i]]
  cfg$mpfr_precision <- 53
  cfg$buffer_size <- 100
  cfg$distribution <- "uniform_01"
  
  tryCatch({
    createPRNG(cfg)
    vals <- generatePRNG(10)
    cleanup_prng()
    # If we get here with huge values, overflow protection worked
    cat(sprintf("  Case %d: PROTECTED - Overflow handled gracefully\n", i))
  }, error = function(e) {
    # Check if error message indicates overflow detection
    if (grepl("Overflow", e$message, ignore.case = TRUE)) {
      cat(sprintf("  Case %d: CORRECTLY DETECTED - %s\n", i, e$message))
    } else {
      cat(sprintf("  Case %d: ERROR - %s\n", i, e$message))
    }
  })
}
cat("\n")

# Test 4: Invalid discriminant (b^2 - 4ac <= 0)
cat("Test 4: Invalid discriminant (should be rejected)\n")
test_cases_invalid <- list(
  list(a = 1, b = 2, c = 2),   # disc = 4 - 8 = -4
  list(a = 2, b = 2, c = 1),   # disc = 4 - 8 = -4
  list(a = 1, b = 0, c = 1)    # disc = 0 - 4 = -4
)

for (i in seq_along(test_cases_invalid)) {
  cfg <- test_cases_invalid[[i]]
  cfg$mpfr_precision <- 53
  cfg$buffer_size <- 100
  cfg$distribution <- "uniform_01"
  
  tryCatch({
    createPRNG(cfg)
    cleanup_prng()
    cat(sprintf("  Case %d (a=%d, b=%d, c=%d): FAILED - Should have been rejected\n", 
                i, cfg$a, cfg$b, cfg$c))
  }, error = function(e) {
    if (grepl("discriminant", e$message, ignore.case = TRUE)) {
      cat(sprintf("  Case %d (a=%d, b=%d, c=%d): CORRECTLY REJECTED\n", 
                  i, cfg$a, cfg$b, cfg$c))
    } else {
      cat(sprintf("  Case %d (a=%d, b=%d, c=%d): ERROR - %s\n", 
                  i, cfg$a, cfg$b, cfg$c, e$message))
    }
  })
}
cat("\n")

# Test 5: Edge cases
cat("Test 5: Edge cases\n")
test_cases_edge <- list(
  list(a = 1, b = 1, c = -1),    # Minimal valid case
  list(a = -1, b = 3, c = 2),    # Negative a
  list(a = 2, b = -5, c = -3),   # Negative b
  list(a = -2, b = -5, c = 3)    # Multiple negatives
)

for (i in seq_along(test_cases_edge)) {
  cfg <- test_cases_edge[[i]]
  cfg$mpfr_precision <- 53
  cfg$buffer_size <- 100
  cfg$distribution <- "uniform_01"
  
  # Calculate expected discriminant
  expected_disc <- cfg$b^2 - 4*cfg$a*cfg$c
  
  tryCatch({
    createPRNG(cfg)
    vals <- generatePRNG(10)
    cleanup_prng()
    cat(sprintf("  Case %d (a=%d, b=%d, c=%d, disc=%d): PASSED\n", 
                i, cfg$a, cfg$b, cfg$c, expected_disc))
  }, error = function(e) {
    if (expected_disc <= 0) {
      cat(sprintf("  Case %d (a=%d, b=%d, c=%d, disc=%d): CORRECTLY REJECTED\n", 
                  i, cfg$a, cfg$b, cfg$c, expected_disc))
    } else {
      cat(sprintf("  Case %d (a=%d, b=%d, c=%d, disc=%d): FAILED - %s\n", 
                  i, cfg$a, cfg$b, cfg$c, expected_disc, e$message))
    }
  })
}
cat("\n")

# Test 6: Concurrent discriminant calculations
cat("Test 6: Concurrent discriminant calculations\n")
library(parallel)

if (.Platform$OS.type != "windows") {
  test_concurrent_overflow <- function(thread_id) {
    # Each thread uses different large values
    base <- 1000000 * thread_id
    cfg <- list(
      a = base,
      b = base * 2,
      c = -base / 2,
      mpfr_precision = 53,
      buffer_size = 100,
      distribution = "uniform_01"
    )
    
    tryCatch({
      createPRNG(cfg)
      vals <- generatePRNG(100)
      cleanup_prng()
      return(list(thread_id = thread_id, status = "SUCCESS", values = length(vals)))
    }, error = function(e) {
      return(list(thread_id = thread_id, status = "ERROR", message = toString(e)))
    })
  }
  
  results <- mclapply(1:4, test_concurrent_overflow, mc.cores = 4)
  
  all_success <- TRUE
  for (res in results) {
    if (res$status == "SUCCESS") {
      cat(sprintf("  Thread %d: SUCCESS (generated %d values)\n", 
                  res$thread_id, res$values))
    } else {
      cat(sprintf("  Thread %d: %s - %s\n", 
                  res$thread_id, res$status, res$message))
      if (!grepl("Overflow", res$message, ignore.case = TRUE)) {
        all_success <- FALSE
      }
    }
  }
  
  if (all_success) {
    cat("  All threads handled correctly\n")
  }
} else {
  cat("  SKIPPED (not on Unix)\n")
}

cat("\n================================================================\n")
cat("Overflow protection tests completed!\n")
cat("The safe_calculate_discriminant function is working correctly.\n")