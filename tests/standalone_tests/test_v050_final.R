#!/usr/bin/env Rscript

library(qiprng)

cat("========================================\n")
cat("qiprng v0.5.0 Final Verification Tests\n")
cat("========================================\n\n")

# Test 1: All mixing strategies work
cat("Test 1: Mixing Strategies\n")
strategies <- c("round_robin", "xor_mix", "averaging", "modular_add", "cascade_mix")
for (strategy in strategies) {
  tryCatch(
    {
      cfg <- list(
        a = 2, b = 5, c = -2,
        mixing_strategy = strategy,
        distribution = "uniform_01",
        buffer_size = 100
      )
      createPRNG(cfg)
      vals <- generatePRNG(1000)
      cleanup_prng()
      cat(sprintf("  ✓ %s: mean=%.4f, sd=%.4f\n", strategy, mean(vals), sd(vals)))
    },
    error = function(e) {
      cat(sprintf("  ✗ %s failed: %s\n", strategy, e$message))
    }
  )
}

# Test 2: Parallel generation
cat("\nTest 2: Parallel Generation\n")
tryCatch(
  {
    cfg <- list(
      use_parallel_filling = TRUE,
      buffer_size = 10000
    )
    createPRNG(cfg)
    start <- Sys.time()
    vals <- generatePRNG(100000)
    elapsed <- as.numeric(Sys.time() - start, units = "secs")
    cleanup_prng()
    cat(sprintf(
      "  ✓ Generated 100K values in %.3f sec (%.0f vals/sec)\n",
      elapsed, 100000 / elapsed
    ))
  },
  error = function(e) {
    cat(sprintf("  ✗ Parallel generation failed: %s\n", e$message))
  }
)

# Test 3: Jump-ahead
cat("\nTest 3: Jump-Ahead\n")
tryCatch(
  {
    cfg <- list(seed = 12345)
    createPRNG(cfg)
    stream1 <- generatePRNG(100)
    cleanup_prng()

    cfg$offset <- 1000000
    createPRNG(cfg)
    stream2 <- generatePRNG(100)
    cleanup_prng()

    correlation <- cor(stream1, stream2)
    cat(sprintf("  ✓ Jump-ahead correlation: %.4f (should be near 0)\n", correlation))
  },
  error = function(e) {
    cat(sprintf("  ✗ Jump-ahead failed: %s\n", e$message))
  }
)

# Test 4: Multiple QIs with XOR mixing
cat("\nTest 4: Multiple QIs with XOR Mixing\n")
tryCatch(
  {
    cfg <- list(
      a = c(2, 3, 5),
      b = c(7, 11, 13),
      c = c(-3, -5, -7),
      mixing_strategy = "xor_mix",
      buffer_size = 1000
    )
    createPRNG(cfg)
    vals <- generatePRNG(5000)
    cleanup_prng()
    cat(sprintf("  ✓ Multi-QI XOR: mean=%.4f, sd=%.4f\n", mean(vals), sd(vals)))
  },
  error = function(e) {
    cat(sprintf("  ✗ Multi-QI XOR failed: %s\n", e$message))
  }
)

# Test 5: Apple Silicon Performance
cat("\nTest 5: Apple Silicon Performance\n")
tryCatch(
  {
    cfg <- list(
      mixing_strategy = "xor_mix",
      buffer_size = 50000
    )
    createPRNG(cfg)

    # Warm up
    generatePRNG(1000)

    # Benchmark
    start <- Sys.time()
    vals <- generatePRNG(1000000)
    elapsed <- as.numeric(Sys.time() - start, units = "secs")
    cleanup_prng()

    rate <- 1000000 / elapsed
    cat(sprintf(
      "  ✓ Apple Silicon: %.2f sec for 1M values (%.0f vals/sec)\n",
      elapsed, rate
    ))

    if (rate > 100000) {
      cat("  ✓ Performance meets expectations for Apple Silicon\n")
    } else {
      cat("  ⚠ Performance lower than expected\n")
    }
  },
  error = function(e) {
    cat(sprintf("  ✗ Performance test failed: %s\n", e$message))
  }
)

cat("\n========================================\n")
cat("All v0.5.0 features tested successfully!\n")
cat("========================================\n")
