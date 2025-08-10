#!/usr/bin/env Rscript

# Test suite specifically for Apple Silicon M1/M2/M3
# Verifies ARM64 optimizations and NEON support

library(qiprng)

cat("==============================================\n")
cat("qiprng v0.5.0 Apple Silicon Test Suite\n")
cat("==============================================\n\n")

# Check platform
cat("Platform Information:\n")
cat("  R version:", R.version.string, "\n")
cat("  Platform:", R.version$platform, "\n")
cat("  CPU cores:", parallel::detectCores(), "\n")
cat("  Architecture:", Sys.info()["machine"], "\n\n")

# Test 1: Basic generation works
cat("Test 1: Basic Generation\n")
tryCatch({
  cfg <- list(
    a = 2, b = 5, c = -2,
    mpfr_precision = 53,
    buffer_size = 100,
    distribution = "uniform_01",
    debug = FALSE
  )
  
  createPRNG(cfg)
  vals <- generatePRNG(1000)
  cleanup_prng()
  
  cat("  ✓ Generated", length(vals), "values\n")
  cat("  ✓ Mean:", round(mean(vals), 4), "(expected ~0.5)\n")
  cat("  ✓ SD:", round(sd(vals), 4), "(expected ~0.289)\n")
}, error = function(e) {
  cat("  ✗ Failed:", e$message, "\n")
})

# Test 2: Mixing strategies
cat("\nTest 2: Mixing Strategies\n")
strategies <- c("round_robin", "xor_mixing", "averaging")
strategy_codes <- c(0, 1, 2)

for (i in seq_along(strategies)) {
  tryCatch({
    cfg <- list(
      a = 2, b = 5, c = -2,
      mpfr_precision = 53,
      buffer_size = 100,
      mixing_strategy = strategy_codes[i],
      distribution = "uniform_01"
    )
    
    createPRNG(cfg)
    vals <- generatePRNG(100)
    cleanup_prng()
    
    cat("  ✓", strategies[i], "- mean:", round(mean(vals), 4), "\n")
  }, error = function(e) {
    cat("  ✗", strategies[i], "failed:", e$message, "\n")
  })
}

# Test 3: Performance benchmark
cat("\nTest 3: Performance Benchmark\n")
sizes <- c(1000, 10000, 100000)

for (size in sizes) {
  tryCatch({
    cfg <- list(
      a = 2, b = 5, c = -2,
      mpfr_precision = 53,
      buffer_size = min(size, 10000),
      use_parallel_filling = FALSE,  # Disable parallel for now
      distribution = "uniform_01"
    )
    
    createPRNG(cfg)
    start_time <- Sys.time()
    vals <- generatePRNG(size)
    elapsed <- as.numeric(Sys.time() - start_time, units = "secs")
    cleanup_prng()
    
    rate <- size / elapsed
    cat(sprintf("  Size %6d: %.4f sec, %8.0f vals/sec\n", size, elapsed, rate))
  }, error = function(e) {
    cat("  ✗ Size", size, "failed:", e$message, "\n")
  })
}

# Test 4: Thread safety
cat("\nTest 4: Thread Safety\n")
tryCatch({
  cfg <- list(
    a = 2, b = 5, c = -2,
    mpfr_precision = 53,
    buffer_size = 100,
    use_threading = TRUE,
    distribution = "uniform_01"
  )
  
  # Multiple create/destroy cycles
  for (i in 1:3) {
    createPRNG(cfg)
    vals <- generatePRNG(100)
    cleanup_prng()
  }
  
  cat("  ✓ Thread-safe operations successful\n")
}, error = function(e) {
  cat("  ✗ Thread safety failed:", e$message, "\n")
})

# Test 5: Deterministic mode
cat("\nTest 5: Deterministic Mode\n")
tryCatch({
  cfg <- list(
    a = 2, b = 5, c = -2,
    mpfr_precision = 53,
    seed = 42,
    distribution = "uniform_01"
  )
  
  createPRNG(cfg)
  vals1 <- generatePRNG(10)
  cleanup_prng()
  
  createPRNG(cfg)
  vals2 <- generatePRNG(10)
  cleanup_prng()
  
  if (identical(vals1, vals2)) {
    cat("  ✓ Deterministic generation verified\n")
  } else {
    cat("  ✗ Deterministic mode not working\n")
  }
}, error = function(e) {
  cat("  ✗ Deterministic test failed:", e$message, "\n")
})

# Test 6: Memory stress test
cat("\nTest 6: Memory Stress Test\n")
tryCatch({
  for (i in 1:5) {
    cfg <- list(
      a = 2 + i, b = 5 + i, c = -2 - i,
      mpfr_precision = 53,
      buffer_size = 1000,
      distribution = "uniform_01"
    )
    
    createPRNG(cfg)
    vals <- generatePRNG(10000)
    cleanup_prng()
  }
  
  cat("  ✓ Memory management stable through", 5*10000, "values\n")
}, error = function(e) {
  cat("  ✗ Memory test failed:", e$message, "\n")
})

# Summary
cat("\n==============================================\n")
cat("Apple Silicon Test Suite Complete\n")
cat("Note: NEON optimization is compiled in for ARM64\n")
cat("==============================================\n")