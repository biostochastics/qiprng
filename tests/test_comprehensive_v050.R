#!/usr/bin/env Rscript

library(qiprng)
library(testthat)

cat("=====================================\n")
cat("qiprng v0.5.0 Comprehensive Test Suite\n")
cat("=====================================\n\n")

# Test counters
tests_passed <- 0
tests_failed <- 0
test_results <- list()

run_test <- function(name, test_func) {
  cat(sprintf("\n[TEST] %s\n", name))
  result <- tryCatch({
    test_func()
    tests_passed <<- tests_passed + 1
    test_results[[name]] <<- "PASSED"
    cat("  ✓ PASSED\n")
    TRUE
  }, error = function(e) {
    tests_failed <<- tests_failed + 1
    test_results[[name]] <<- paste("FAILED:", e$message)
    cat(sprintf("  ✗ FAILED: %s\n", e$message))
    FALSE
  })
  return(result)
}

# 1. Test all mixing strategies
run_test("Mixing Strategies", function() {
  strategies <- c("round_robin", "xor_mix", "averaging", "modular_add", "cascade_mix")
  for (strategy in strategies) {
    cfg <- list(
      a = 2, b = 5, c = -2,
      mixing_strategy = strategy,
      buffer_size = 1000
    )
    createPRNG(cfg)
    vals <- generatePRNG(1000)
    cleanup_prng()
    
    # Validate output
    stopifnot(length(vals) == 1000)
    stopifnot(all(vals >= 0 & vals <= 1))
    stopifnot(abs(mean(vals) - 0.5) < 0.1)  # Reasonable mean
  }
})

# 2. Test parallel generation
run_test("Parallel Generation", function() {
  cfg <- list(
    use_parallel_filling = TRUE,
    buffer_size = 10000
  )
  createPRNG(cfg)
  vals <- generatePRNG(50000)
  cleanup_prng()
  
  stopifnot(length(vals) == 50000)
  stopifnot(all(vals >= 0 & vals <= 1))
})

# 3. Test jump-ahead functionality
run_test("Jump-Ahead", function() {
  cfg <- list(seed = 999, a = 2, b = 5, c = -2)
  
  createPRNG(cfg)
  stream1 <- generatePRNG(100)
  cleanup_prng()
  
  # Jump ahead
  cfg$offset <- 1000000
  createPRNG(cfg)
  stream2 <- generatePRNG(100)
  cleanup_prng()
  
  # Streams should be uncorrelated
  correlation <- abs(cor(stream1, stream2))
  stopifnot(correlation < 0.2)
})

# 4. Test all distributions
run_test("All Distributions", function() {
  distributions <- list(
    list(name = "uniform_01", check = function(x) all(x >= 0 & x <= 1)),
    list(name = "uniform_range", range_min = -5, range_max = 5, 
         check = function(x) all(x >= -5 & x <= 5)),
    list(name = "normal", normal_mean = 10, normal_sd = 2,
         check = function(x) abs(mean(x) - 10) < 0.5),
    list(name = "exponential", exponential_lambda = 2,
         check = function(x) all(x >= 0)),
    list(name = "poisson", poisson_lambda = 5,
         check = function(x) all(x >= 0 & x == floor(x))),
    list(name = "bernoulli", bernoulli_p = 0.3,
         check = function(x) all(x %in% c(0, 1)))
  )
  
  for (dist in distributions) {
    cfg <- modifyList(list(distribution = dist$name), dist)
    cfg$check <- NULL  # Remove check function from config
    
    createPRNG(cfg)
    vals <- generatePRNG(1000)
    cleanup_prng()
    
    stopifnot(dist$check(vals))
  }
})

# 5. Test thread safety
run_test("Thread Safety", function() {
  cfg <- list(
    use_threading = TRUE,
    buffer_size = 1000
  )
  
  # Multiple create/destroy cycles
  for (i in 1:5) {
    createPRNG(cfg)
    vals <- generatePRNG(100)
    stopifnot(length(vals) == 100)
    cleanup_prng()
  }
})

# 6. Test deterministic mode
run_test("Deterministic Mode", function() {
  cfg <- list(
    seed = 42,
    a = 2, b = 5, c = -2,
    deterministic = TRUE
  )
  
  createPRNG(cfg)
  vals1 <- generatePRNG(100)
  cleanup_prng()
  
  createPRNG(cfg)
  vals2 <- generatePRNG(100)
  cleanup_prng()
  
  stopifnot(identical(vals1, vals2))
})

# 7. Test multi-QI with mixing
run_test("Multi-QI Mixing", function() {
  cfg <- list(
    a = c(2, 3, 5),
    b = c(7, 11, 13),
    c = c(-3, -5, -7),
    mixing_strategy = "xor_mix"
  )
  
  createPRNG(cfg)
  vals <- generatePRNG(1000)
  cleanup_prng()
  
  stopifnot(length(vals) == 1000)
  stopifnot(all(vals >= 0 & vals <= 1))
})

# 8. Test buffer sizes
run_test("Buffer Sizes", function() {
  buffer_sizes <- c(10, 100, 1000, 10000)
  
  for (size in buffer_sizes) {
    cfg <- list(buffer_size = size)
    createPRNG(cfg)
    vals <- generatePRNG(size * 2)  # Generate more than buffer
    cleanup_prng()
    
    stopifnot(length(vals) == size * 2)
  }
})

# 9. Test SIMD operations (if available)
run_test("SIMD Operations", function() {
  cfg <- list(
    mixing_strategy = "xor_mix",
    buffer_size = 10000
  )
  
  createPRNG(cfg)
  vals <- generatePRNG(10000)
  cleanup_prng()
  
  stopifnot(length(vals) == 10000)
  # SIMD is used automatically if available
})

# 10. Test statistical properties
run_test("Statistical Properties", function() {
  createPRNG()
  vals <- generatePRNG(10000)
  cleanup_prng()
  
  # Basic uniformity tests
  ks_test <- ks.test(vals, "punif")
  stopifnot(ks_test$p.value > 0.01)  # Should not reject uniformity
  
  # Check mean and variance
  stopifnot(abs(mean(vals) - 0.5) < 0.02)
  stopifnot(abs(var(vals) - 1/12) < 0.01)
})

# Summary
cat("\n=====================================\n")
cat("Test Summary:\n")
cat(sprintf("  Passed: %d\n", tests_passed))
cat(sprintf("  Failed: %d\n", tests_failed))
cat("=====================================\n\n")

if (tests_failed > 0) {
  cat("Failed tests:\n")
  for (name in names(test_results)) {
    if (grepl("FAILED", test_results[[name]])) {
      cat(sprintf("  - %s: %s\n", name, test_results[[name]]))
    }
  }
  stop("Some tests failed!")
} else {
  cat("✅ All tests passed successfully!\n")
}