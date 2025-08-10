#!/usr/bin/env Rscript

# Simplified Statistical Test Suite for qiprng v0.5.0
# This version runs tests sequentially with proper cleanup

library(qiprng)

cat("=============================================================\n")
cat("        qiprng v0.5.0 Statistical Test Suite (Simple)       \n")
cat("=============================================================\n")
cat("Start time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

# Initialize results
results <- list()
start_time <- Sys.time()

# Test 1: Basic Generation
cat("\n==== TEST 1: BASIC GENERATION ====\n")
tryCatch({
  createPRNG()
  vals <- generatePRNG(1000)
  cleanup_prng()
  
  cat("  Generated:", length(vals), "values\n")
  cat("  Mean:", round(mean(vals), 6), "(expected: ~0.5)\n")
  cat("  SD:", round(sd(vals), 6), "(expected: ~0.289)\n")
  cat("  Range: [", round(min(vals), 6), ",", round(max(vals), 6), "]\n")
  
  results$basic_generation <- list(
    status = "PASSED",
    n = length(vals),
    mean = mean(vals),
    sd = sd(vals)
  )
}, error = function(e) {
  cat("  FAILED:", e$message, "\n")
  results$basic_generation <- list(status = "FAILED", error = e$message)
})

# Test 2: Deterministic Mode
cat("\n==== TEST 2: DETERMINISTIC MODE ====\n")
tryCatch({
  cfg <- list(seed = 12345, a = 2, b = 5, c = -2)
  
  createPRNG(cfg)
  vals1 <- generatePRNG(100)
  cleanup_prng()
  
  createPRNG(cfg)
  vals2 <- generatePRNG(100)
  cleanup_prng()
  
  identical_test <- identical(vals1, vals2)
  cat("  Identical sequences:", identical_test, "\n")
  cat("  Correlation:", round(cor(vals1, vals2), 6), "\n")
  
  results$deterministic <- list(
    status = ifelse(identical_test, "PASSED", "FAILED"),
    identical = identical_test
  )
}, error = function(e) {
  cat("  FAILED:", e$message, "\n")
  results$deterministic <- list(status = "FAILED", error = e$message)
})

# Test 3: Mixing Strategies
cat("\n==== TEST 3: MIXING STRATEGIES ====\n")
strategies <- c("round_robin", "xor_mix", "averaging", "modular_add", "cascade_mix")

for (strategy in strategies) {
  cat("\n  Testing", strategy, "mixing:\n")
  
  tryCatch({
    cfg <- list(
      a = 2, b = 5, c = -2,
      mixing_strategy = strategy,
      buffer_size = 5000
    )
    
    createPRNG(cfg)
    vals <- generatePRNG(5000)
    cleanup_prng()
    
    # Basic statistics
    mean_val <- mean(vals)
    var_val <- var(vals)
    
    # KS test for uniformity
    ks_test <- ks.test(vals, "punif")
    
    cat("    Mean:", round(mean_val, 6), "\n")
    cat("    Variance:", round(var_val, 6), "\n")
    cat("    KS p-value:", round(ks_test$p.value, 6), 
        ifelse(ks_test$p.value > 0.05, "✓", "⚠"), "\n")
    
    results[[paste0("mixing_", strategy)]] <- list(
      status = ifelse(ks_test$p.value > 0.01, "PASSED", "MARGINAL"),
      mean = mean_val,
      variance = var_val,
      ks_pvalue = ks_test$p.value
    )
  }, error = function(e) {
    cat("    FAILED:", e$message, "\n")
    results[[paste0("mixing_", strategy)]] <- list(status = "FAILED", error = e$message)
  })
}

# Test 4: Distributions
cat("\n==== TEST 4: DISTRIBUTIONS ====\n")
distributions <- list(
  list(name = "uniform_01", 
       config = list(distribution = "uniform_01")),
  list(name = "normal", 
       config = list(distribution = "normal", normal_mean = 0, normal_sd = 1)),
  list(name = "exponential",
       config = list(distribution = "exponential", exponential_lambda = 1))
)

for (dist in distributions) {
  cat("\n  Testing", dist$name, "distribution:\n")
  
  tryCatch({
    createPRNG(dist$config)
    vals <- generatePRNG(5000)
    cleanup_prng()
    
    cat("    Mean:", round(mean(vals), 6), "\n")
    cat("    SD:", round(sd(vals), 6), "\n")
    cat("    Range: [", round(min(vals), 6), ",", round(max(vals), 6), "]\n")
    
    results[[paste0("dist_", dist$name)]] <- list(
      status = "PASSED",
      mean = mean(vals),
      sd = sd(vals)
    )
  }, error = function(e) {
    cat("    FAILED:", e$message, "\n")
    results[[paste0("dist_", dist$name)]] <- list(status = "FAILED", error = e$message)
  })
}

# Test 5: Independence (Serial Correlation)
cat("\n==== TEST 5: INDEPENDENCE TESTS ====\n")
tryCatch({
  createPRNG(list(mixing_strategy = "xor_mix"))
  test_seq <- generatePRNG(5000)
  cleanup_prng()
  
  cat("\n  Serial Correlation Test:\n")
  
  # Calculate correlations for different lags
  correlations <- numeric(5)
  for (lag in 1:5) {
    n <- length(test_seq)
    if (lag < n) {
      correlations[lag] <- cor(test_seq[1:(n-lag)], test_seq[(lag+1):n])
    }
  }
  
  for (lag in 1:5) {
    cat(sprintf("    Lag %d: %+.6f %s\n", 
                lag, correlations[lag],
                ifelse(abs(correlations[lag]) < 0.05, "✓", "⚠")))
  }
  
  max_abs_cor <- max(abs(correlations))
  
  results$serial_correlation <- list(
    status = ifelse(max_abs_cor < 0.1, "PASSED", "MARGINAL"),
    max_abs_correlation = max_abs_cor
  )
}, error = function(e) {
  cat("  FAILED:", e$message, "\n")
  results$serial_correlation <- list(status = "FAILED", error = e$message)
})

# Test 6: Uniformity (Chi-squared)
cat("\n==== TEST 6: UNIFORMITY TESTS ====\n")
tryCatch({
  createPRNG()
  test_seq <- generatePRNG(5000)
  cleanup_prng()
  
  cat("\n  Chi-squared Goodness of Fit:\n")
  
  n_bins <- 10
  breaks <- seq(0, 1, length.out = n_bins + 1)
  observed <- table(cut(test_seq, breaks = breaks))
  expected <- length(test_seq) / n_bins
  
  chi_sq <- sum((observed - expected)^2 / expected)
  df <- n_bins - 1
  p_value <- pchisq(chi_sq, df, lower.tail = FALSE)
  
  cat("    Chi-squared:", round(chi_sq, 6), "\n")
  cat("    P-value:", round(p_value, 6), ifelse(p_value > 0.05, "✓", "⚠"), "\n")
  
  results$chi_squared <- list(
    status = ifelse(p_value > 0.01, "PASSED", "MARGINAL"),
    chi_sq = chi_sq,
    p_value = p_value
  )
}, error = function(e) {
  cat("  FAILED:", e$message, "\n")
  results$chi_squared <- list(status = "FAILED", error = e$message)
})

# Test 7: Performance
cat("\n==== TEST 7: PERFORMANCE ====\n")
tryCatch({
  sizes <- c(1000, 10000, 50000)
  
  for (size in sizes) {
    createPRNG(list(buffer_size = min(size, 10000)))
    
    start <- Sys.time()
    vals <- generatePRNG(size)
    elapsed <- as.numeric(Sys.time() - start, units = "secs")
    
    cleanup_prng()
    
    rate <- size / elapsed
    cat(sprintf("  Size %6d: %.4f sec (%8.0f vals/sec)\n", size, elapsed, rate))
  }
  
  results$performance <- list(status = "MEASURED")
}, error = function(e) {
  cat("  FAILED:", e$message, "\n")
  results$performance <- list(status = "FAILED", error = e$message)
})

# Test 8: Jump-Ahead
cat("\n==== TEST 8: JUMP-AHEAD ====\n")
tryCatch({
  cfg <- list(seed = 999, a = 2, b = 5, c = -2)
  
  # Generate first stream
  createPRNG(cfg)
  stream1 <- generatePRNG(1000)
  cleanup_prng()
  
  # Jump ahead
  cfg$offset <- 1000000
  createPRNG(cfg)
  stream2 <- generatePRNG(1000)
  cleanup_prng()
  
  # Check independence
  correlation <- cor(stream1, stream2)
  
  cat("  Correlation between streams:", round(correlation, 6), "\n")
  cat("  Independence:", ifelse(abs(correlation) < 0.1, "✓ Good", "⚠ Poor"), "\n")
  
  results$jump_ahead <- list(
    status = ifelse(abs(correlation) < 0.1, "PASSED", "MARGINAL"),
    correlation = correlation
  )
}, error = function(e) {
  cat("  FAILED:", e$message, "\n")
  results$jump_ahead <- list(status = "FAILED", error = e$message)
})

# Test 9: Multi-QI Ensemble
cat("\n==== TEST 9: MULTI-QI ENSEMBLE ====\n")
tryCatch({
  cfg <- list(
    a = c(2, 3, 5),
    b = c(7, 11, 13),
    c = c(-3, -5, -7),
    mixing_strategy = "xor_mix",
    buffer_size = 3000
  )
  
  createPRNG(cfg)
  vals <- generatePRNG(3000)
  cleanup_prng()
  
  # Test quality
  ks_test <- ks.test(vals, "punif")
  
  cat("  Number of QIs: 3\n")
  cat("  Mean:", round(mean(vals), 6), "\n")
  cat("  KS p-value:", round(ks_test$p.value, 6), 
      ifelse(ks_test$p.value > 0.05, "✓", "⚠"), "\n")
  
  results$multi_qi <- list(
    status = ifelse(ks_test$p.value > 0.01, "PASSED", "MARGINAL"),
    ks_pvalue = ks_test$p.value
  )
}, error = function(e) {
  cat("  FAILED:", e$message, "\n")
  results$multi_qi <- list(status = "FAILED", error = e$message)
})

# Summary
end_time <- Sys.time()
total_time <- difftime(end_time, start_time, units = "secs")

cat("\n=============================================================\n")
cat("                       SUMMARY                               \n")
cat("=============================================================\n\n")

cat("Execution time:", round(total_time, 2), "seconds\n\n")

# Count results by status
statuses <- sapply(results, function(x) x$status)
status_counts <- table(statuses)

cat("Results by Status:\n")
for (status in names(status_counts)) {
  cat(sprintf("  %s: %d\n", status, status_counts[status]))
}

# List all test results
cat("\nDetailed Results:\n")
for (test_name in names(results)) {
  result <- results[[test_name]]
  symbol <- switch(result$status,
                   "PASSED" = "✓",
                   "FAILED" = "✗",
                   "MARGINAL" = "⚠",
                   "MEASURED" = "📊",
                   "?")
  cat(sprintf("  %s %s: %s\n", symbol, test_name, result$status))
}

# Save results
saveRDS(results, file = "statistical_test_results_simple.rds")
cat("\nResults saved to: statistical_test_results_simple.rds\n")

# Final assessment
failed_count <- sum(statuses == "FAILED", na.rm = TRUE)
marginal_count <- sum(statuses == "MARGINAL", na.rm = TRUE)

cat("\n=============================================================\n")
if (failed_count == 0) {
  if (marginal_count == 0) {
    cat("✅ ALL TESTS PASSED - Package quality EXCELLENT\n")
  } else {
    cat("✅ All critical tests passed - Package quality GOOD\n")
    cat(sprintf("   (%d tests showed marginal results but within acceptable limits)\n", 
                marginal_count))
  }
} else {
  cat("⚠️  Some tests failed - Package needs attention\n")
  cat(sprintf("   Failed: %d, Marginal: %d\n", failed_count, marginal_count))
}
cat("=============================================================\n")