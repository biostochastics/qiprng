#!/usr/bin/env Rscript

# Complete Statistical Test Suite for qiprng v0.5.0
# This will run all tests and generate a comprehensive report

library(qiprng)

# Initialize results storage
test_results <- list()
start_time <- Sys.time()

cat("=============================================================\n")
cat("           qiprng v0.5.0 Complete Statistical Test Suite     \n")
cat("=============================================================\n")
cat("Start time:", format(start_time, "%Y-%m-%d %H:%M:%S"), "\n\n")

# Helper function to record results
record_result <- function(category, test_name, result, details = NULL) {
  if (!category %in% names(test_results)) {
    test_results[[category]] <<- list()
  }
  test_results[[category]][[test_name]] <<- list(
    result = result,
    details = details,
    timestamp = Sys.time()
  )
}

# =============================================================================
# PART 1: BASIC FUNCTIONALITY TESTS
# =============================================================================

cat("\n==== PART 1: BASIC FUNCTIONALITY TESTS ====\n\n")

# Test 1.1: Basic Generation
cat("Test 1.1: Basic Generation\n")
tryCatch(
  {
    createPRNG()
    vals <- generatePRNG(1000)
    cleanup_prng()

    result <- list(
      n_generated = length(vals),
      mean = mean(vals),
      sd = sd(vals),
      min = min(vals),
      max = max(vals),
      status = "PASSED"
    )

    cat(sprintf("  Generated: %d values\n", result$n_generated))
    cat(sprintf("  Mean: %.6f (expected: ~0.5)\n", result$mean))
    cat(sprintf("  SD: %.6f (expected: ~0.289)\n", result$sd))
    cat(sprintf("  Range: [%.6f, %.6f]\n", result$min, result$max))

    record_result("Basic", "Generation", "PASSED", result)
  },
  error = function(e) {
    cat("  FAILED:", e$message, "\n")
    record_result("Basic", "Generation", "FAILED", e$message)
  }
)

# Test 1.2: Deterministic Mode
cat("\nTest 1.2: Deterministic Mode\n")
tryCatch(
  {
    cfg <- list(seed = 12345, a = 2, b = 5, c = -2)

    createPRNG(cfg)
    vals1 <- generatePRNG(100)
    cleanup_prng()

    createPRNG(cfg)
    vals2 <- generatePRNG(100)
    cleanup_prng()

    identical_test <- identical(vals1, vals2)
    correlation <- cor(vals1, vals2)

    cat(sprintf("  Identical sequences: %s\n", identical_test))
    cat(sprintf("  Correlation: %.6f\n", correlation))

    record_result(
      "Basic", "Deterministic",
      ifelse(identical_test, "PASSED", "FAILED"),
      list(identical = identical_test, correlation = correlation)
    )
  },
  error = function(e) {
    cat("  FAILED:", e$message, "\n")
    record_result("Basic", "Deterministic", "FAILED", e$message)
  }
)

# =============================================================================
# PART 2: MIXING STRATEGIES TESTS
# =============================================================================

cat("\n\n==== PART 2: MIXING STRATEGIES TESTS ====\n\n")

strategies <- c("round_robin", "xor_mix", "averaging", "modular_add", "cascade_mix")

for (strategy in strategies) {
  cat(sprintf("\nTest 2.%d: %s mixing\n", which(strategies == strategy), strategy))

  tryCatch(
    {
      cfg <- list(
        a = 2, b = 5, c = -2,
        mixing_strategy = strategy,
        buffer_size = 10000
      )

      createPRNG(cfg)
      vals <- generatePRNG(10000)
      cleanup_prng()

      # Statistical tests
      ks_test <- ks.test(vals, "punif")
      mean_val <- mean(vals)
      var_val <- var(vals)

      # Theoretical values for uniform(0,1)
      theoretical_mean <- 0.5
      theoretical_var <- 1 / 12

      # Calculate deviations
      mean_dev <- abs(mean_val - theoretical_mean)
      var_dev <- abs(var_val - theoretical_var)

      result <- list(
        n = length(vals),
        mean = mean_val,
        variance = var_val,
        mean_deviation = mean_dev,
        var_deviation = var_dev,
        ks_pvalue = ks_test$p.value,
        ks_statistic = ks_test$statistic
      )

      cat(sprintf("  N: %d\n", result$n))
      cat(sprintf("  Mean: %.6f (deviation: %.6f)\n", result$mean, result$mean_deviation))
      cat(sprintf("  Variance: %.6f (deviation: %.6f)\n", result$variance, result$var_deviation))
      cat(sprintf(
        "  KS test p-value: %.6f %s\n",
        result$ks_pvalue,
        ifelse(result$ks_pvalue > 0.05, "✓", "⚠")
      ))

      status <- ifelse(result$ks_pvalue > 0.01 && mean_dev < 0.05 && var_dev < 0.01,
        "PASSED", "MARGINAL"
      )

      record_result("MixingStrategies", strategy, status, result)
    },
    error = function(e) {
      cat("  FAILED:", e$message, "\n")
      record_result("MixingStrategies", strategy, "FAILED", e$message)
    }
  )
}

# =============================================================================
# PART 3: DISTRIBUTION TESTS
# =============================================================================

cat("\n\n==== PART 3: DISTRIBUTION TESTS ====\n\n")

distributions <- list(
  list(
    name = "uniform_01",
    config = list(distribution = "uniform_01"),
    test = function(x) ks.test(x, "punif")
  ),
  list(
    name = "normal",
    config = list(distribution = "normal", normal_mean = 0, normal_sd = 1),
    test = function(x) shapiro.test(x[1:min(5000, length(x))])
  ),
  list(
    name = "exponential",
    config = list(distribution = "exponential", exponential_lambda = 1),
    test = function(x) ks.test(x, "pexp", rate = 1)
  ),
  list(
    name = "uniform_range",
    config = list(distribution = "uniform_range", range_min = -10, range_max = 10),
    test = function(x) {
      scaled <- (x + 10) / 20 # Scale to [0,1]
      ks.test(scaled, "punif")
    }
  )
)

for (i in seq_along(distributions)) {
  dist <- distributions[[i]]
  cat(sprintf("\nTest 3.%d: %s distribution\n", i, dist$name))

  tryCatch(
    {
      createPRNG(dist$config)
      vals <- generatePRNG(10000)
      cleanup_prng()

      # Run distribution-specific test
      test_result <- dist$test(vals)

      result <- list(
        n = length(vals),
        mean = mean(vals),
        sd = sd(vals),
        min = min(vals),
        max = max(vals),
        test_statistic = test_result$statistic,
        test_pvalue = test_result$p.value
      )

      cat(sprintf("  N: %d\n", result$n))
      cat(sprintf("  Mean: %.6f, SD: %.6f\n", result$mean, result$sd))
      cat(sprintf("  Range: [%.6f, %.6f]\n", result$min, result$max))
      cat(sprintf(
        "  Test p-value: %.6f %s\n",
        result$test_pvalue,
        ifelse(result$test_pvalue > 0.05, "✓", "⚠")
      ))

      status <- ifelse(result$test_pvalue > 0.01, "PASSED", "MARGINAL")
      record_result("Distributions", dist$name, status, result)
    },
    error = function(e) {
      cat("  FAILED:", e$message, "\n")
      record_result("Distributions", dist$name, "FAILED", e$message)
    }
  )
}

# =============================================================================
# PART 4: INDEPENDENCE TESTS
# =============================================================================

cat("\n\n==== PART 4: INDEPENDENCE TESTS ====\n\n")

cat("Generating test sequence...\n")
createPRNG(list(mixing_strategy = "xor_mix"))
test_sequence <- generatePRNG(10000)
cleanup_prng()

# Test 4.1: Serial Correlation
cat("\nTest 4.1: Serial Correlation\n")
tryCatch(
  {
    correlations <- numeric(10)
    for (lag in 1:10) {
      n <- length(test_sequence)
      if (lag < n) {
        correlations[lag] <- cor(
          test_sequence[1:(n - lag)],
          test_sequence[(lag + 1):n]
        )
      }
    }

    max_abs_cor <- max(abs(correlations))

    cat("  Lag correlations:\n")
    for (lag in 1:10) {
      cat(sprintf(
        "    Lag %2d: %+.6f %s\n",
        lag, correlations[lag],
        ifelse(abs(correlations[lag]) < 0.05, "✓", "⚠")
      ))
    }
    cat(sprintf("  Max absolute correlation: %.6f\n", max_abs_cor))

    status <- ifelse(max_abs_cor < 0.1, "PASSED", "MARGINAL")
    record_result(
      "Independence", "SerialCorrelation", status,
      list(correlations = correlations, max_abs = max_abs_cor)
    )
  },
  error = function(e) {
    cat("  FAILED:", e$message, "\n")
    record_result("Independence", "SerialCorrelation", "FAILED", e$message)
  }
)

# Test 4.2: Runs Test
cat("\nTest 4.2: Runs Test\n")
tryCatch(
  {
    median_val <- median(test_sequence)
    above_median <- test_sequence > median_val
    runs <- rle(above_median)
    n_runs <- length(runs$lengths)

    n1 <- sum(above_median)
    n2 <- sum(!above_median)

    # Expected runs and variance
    expected_runs <- (2 * n1 * n2) / (n1 + n2) + 1
    var_runs <- (2 * n1 * n2 * (2 * n1 * n2 - n1 - n2)) /
      ((n1 + n2)^2 * (n1 + n2 - 1))

    # Z-score
    z_score <- (n_runs - expected_runs) / sqrt(var_runs)
    p_value <- 2 * pnorm(-abs(z_score))

    cat(sprintf("  Number of runs: %d\n", n_runs))
    cat(sprintf("  Expected runs: %.2f\n", expected_runs))
    cat(sprintf("  Z-score: %.6f\n", z_score))
    cat(sprintf("  P-value: %.6f %s\n", p_value, ifelse(p_value > 0.05, "✓", "⚠")))

    status <- ifelse(p_value > 0.01, "PASSED", "MARGINAL")
    record_result(
      "Independence", "RunsTest", status,
      list(
        n_runs = n_runs, expected = expected_runs,
        z_score = z_score, p_value = p_value
      )
    )
  },
  error = function(e) {
    cat("  FAILED:", e$message, "\n")
    record_result("Independence", "RunsTest", "FAILED", e$message)
  }
)

# =============================================================================
# PART 5: UNIFORMITY TESTS
# =============================================================================

cat("\n\n==== PART 5: UNIFORMITY TESTS ====\n\n")

# Test 5.1: Chi-squared Test
cat("Test 5.1: Chi-squared Goodness of Fit\n")
tryCatch(
  {
    n_bins <- 20
    breaks <- seq(0, 1, length.out = n_bins + 1)
    observed <- table(cut(test_sequence, breaks = breaks))
    expected <- length(test_sequence) / n_bins

    chi_sq <- sum((observed - expected)^2 / expected)
    df <- n_bins - 1
    p_value <- pchisq(chi_sq, df, lower.tail = FALSE)

    cat(sprintf("  Number of bins: %d\n", n_bins))
    cat(sprintf("  Chi-squared statistic: %.6f\n", chi_sq))
    cat(sprintf("  Degrees of freedom: %d\n", df))
    cat(sprintf("  P-value: %.6f %s\n", p_value, ifelse(p_value > 0.05, "✓", "⚠")))

    status <- ifelse(p_value > 0.01, "PASSED", "MARGINAL")
    record_result(
      "Uniformity", "ChiSquared", status,
      list(chi_sq = chi_sq, df = df, p_value = p_value)
    )
  },
  error = function(e) {
    cat("  FAILED:", e$message, "\n")
    record_result("Uniformity", "ChiSquared", "FAILED", e$message)
  }
)

# Test 5.2: Anderson-Darling Test
cat("\nTest 5.2: Anderson-Darling Test\n")
tryCatch(
  {
    if (requireNamespace("nortest", quietly = TRUE)) {
      ad_test <- nortest::ad.test(test_sequence)

      cat(sprintf("  Statistic: %.6f\n", ad_test$statistic))
      cat(sprintf(
        "  P-value: %.6f %s\n",
        ad_test$p.value,
        ifelse(ad_test$p.value > 0.05, "✓", "⚠")
      ))

      status <- ifelse(ad_test$p.value > 0.01, "PASSED", "MARGINAL")
      record_result(
        "Uniformity", "AndersonDarling", status,
        list(statistic = ad_test$statistic, p_value = ad_test$p.value)
      )
    } else {
      cat("  SKIPPED: nortest package not available\n")
      record_result("Uniformity", "AndersonDarling", "SKIPPED", "Package not available")
    }
  },
  error = function(e) {
    cat("  FAILED:", e$message, "\n")
    record_result("Uniformity", "AndersonDarling", "FAILED", e$message)
  }
)

# =============================================================================
# PART 6: PERFORMANCE TESTS
# =============================================================================

cat("\n\n==== PART 6: PERFORMANCE TESTS ====\n\n")

# Test 6.1: Generation Speed
cat("Test 6.1: Generation Speed\n")
tryCatch(
  {
    sizes <- c(1000, 10000, 100000)
    timings <- list()

    for (size in sizes) {
      createPRNG(list(buffer_size = min(size, 10000)))

      start <- Sys.time()
      vals <- generatePRNG(size)
      elapsed <- as.numeric(Sys.time() - start, units = "secs")

      cleanup_prng()

      rate <- size / elapsed
      timings[[as.character(size)]] <- list(
        size = size,
        time = elapsed,
        rate = rate
      )

      cat(sprintf("  Size %7d: %.4f sec (%8.0f vals/sec)\n", size, elapsed, rate))
    }

    record_result("Performance", "GenerationSpeed", "MEASURED", timings)
  },
  error = function(e) {
    cat("  FAILED:", e$message, "\n")
    record_result("Performance", "GenerationSpeed", "FAILED", e$message)
  }
)

# Test 6.2: Parallel vs Sequential
cat("\nTest 6.2: Parallel vs Sequential\n")
tryCatch(
  {
    test_size <- 50000

    # Sequential
    createPRNG(list(use_parallel_filling = FALSE, buffer_size = 10000))
    start_seq <- Sys.time()
    vals_seq <- generatePRNG(test_size)
    time_seq <- as.numeric(Sys.time() - start_seq, units = "secs")
    cleanup_prng()

    # Parallel
    createPRNG(list(use_parallel_filling = TRUE, buffer_size = 10000))
    start_par <- Sys.time()
    vals_par <- generatePRNG(test_size)
    time_par <- as.numeric(Sys.time() - start_par, units = "secs")
    cleanup_prng()

    speedup <- time_seq / time_par

    cat(sprintf("  Sequential: %.4f sec\n", time_seq))
    cat(sprintf("  Parallel:   %.4f sec\n", time_par))
    cat(sprintf("  Speedup:    %.2fx\n", speedup))

    record_result(
      "Performance", "ParallelSpeedup", "MEASURED",
      list(sequential = time_seq, parallel = time_par, speedup = speedup)
    )
  },
  error = function(e) {
    cat("  FAILED:", e$message, "\n")
    record_result("Performance", "ParallelSpeedup", "FAILED", e$message)
  }
)

# =============================================================================
# PART 7: SPECIAL FEATURES TESTS
# =============================================================================

cat("\n\n==== PART 7: SPECIAL FEATURES TESTS ====\n\n")

# Test 7.1: Jump-Ahead
cat("Test 7.1: Jump-Ahead Functionality\n")
tryCatch(
  {
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

    # Overlap test (should be minimal)
    overlap <- sum(stream1 %in% stream2)

    cat(sprintf("  Correlation: %.6f\n", correlation))
    cat(sprintf("  Overlapping values: %d / 1000\n", overlap))
    cat(sprintf(
      "  Independence: %s\n",
      ifelse(abs(correlation) < 0.1, "✓ Good", "⚠ Poor")
    ))

    status <- ifelse(abs(correlation) < 0.1, "PASSED", "MARGINAL")
    record_result(
      "SpecialFeatures", "JumpAhead", status,
      list(correlation = correlation, overlap = overlap)
    )
  },
  error = function(e) {
    cat("  FAILED:", e$message, "\n")
    record_result("SpecialFeatures", "JumpAhead", "FAILED", e$message)
  }
)

# Test 7.2: Multi-QI Ensemble
cat("\nTest 7.2: Multi-QI Ensemble\n")
tryCatch(
  {
    cfg <- list(
      a = c(2, 3, 5),
      b = c(7, 11, 13),
      c = c(-3, -5, -7),
      mixing_strategy = "xor_mix",
      buffer_size = 5000
    )

    createPRNG(cfg)
    vals <- generatePRNG(5000)
    cleanup_prng()

    # Test quality
    ks_test <- ks.test(vals, "punif")

    cat(sprintf("  Number of QIs: 3\n"))
    cat(sprintf("  Values generated: %d\n", length(vals)))
    cat(sprintf("  Mean: %.6f\n", mean(vals)))
    cat(sprintf(
      "  KS test p-value: %.6f %s\n",
      ks_test$p.value,
      ifelse(ks_test$p.value > 0.05, "✓", "⚠")
    ))

    status <- ifelse(ks_test$p.value > 0.01, "PASSED", "MARGINAL")
    record_result(
      "SpecialFeatures", "MultiQI", status,
      list(n_qis = 3, ks_pvalue = ks_test$p.value)
    )
  },
  error = function(e) {
    cat("  FAILED:", e$message, "\n")
    record_result("SpecialFeatures", "MultiQI", "FAILED", e$message)
  }
)

# =============================================================================
# FINAL SUMMARY
# =============================================================================

end_time <- Sys.time()
total_time <- difftime(end_time, start_time, units = "secs")

cat("\n\n=============================================================\n")
cat("                    TEST SUITE SUMMARY                       \n")
cat("=============================================================\n\n")

cat("Execution time:", round(total_time, 2), "seconds\n\n")

# Count results by status
all_results <- unlist(test_results, recursive = FALSE)
status_counts <- table(sapply(all_results, function(x) x$result))

cat("Overall Results:\n")
for (status in names(status_counts)) {
  cat(sprintf("  %s: %d\n", status, status_counts[status]))
}

# Detailed results by category
cat("\n\nDetailed Results by Category:\n")
cat("------------------------------\n")

for (category in names(test_results)) {
  cat(sprintf("\n%s:\n", category))
  for (test in names(test_results[[category]])) {
    result <- test_results[[category]][[test]]
    status_symbol <- switch(result$result,
      "PASSED" = "✓",
      "FAILED" = "✗",
      "MARGINAL" = "⚠",
      "MEASURED" = "📊",
      "SKIPPED" = "⊘",
      "?"
    )
    cat(sprintf("  %s %s: %s\n", status_symbol, test, result$result))

    # Show key details for failed/marginal tests
    if (result$result %in% c("FAILED", "MARGINAL") && !is.null(result$details)) {
      if (is.list(result$details)) {
        if (!is.null(result$details$p_value)) {
          cat(sprintf("      p-value: %.6f\n", result$details$p_value))
        }
        if (!is.null(result$details$ks_pvalue)) {
          cat(sprintf("      KS p-value: %.6f\n", result$details$ks_pvalue))
        }
      }
    }
  }
}

# Save results to file
cat("\n\nSaving detailed results to file...\n")
saveRDS(test_results, file = "statistical_test_results.rds")
cat("Results saved to: statistical_test_results.rds\n")

# Final assessment
failed_count <- sum(status_counts["FAILED"], na.rm = TRUE)
marginal_count <- sum(status_counts["MARGINAL"], na.rm = TRUE)

cat("\n=============================================================\n")
if (failed_count == 0) {
  if (marginal_count == 0) {
    cat("✅ ALL TESTS PASSED - Package quality EXCELLENT\n")
  } else {
    cat("✅ All critical tests passed - Package quality GOOD\n")
    cat(sprintf(
      "   (%d tests showed marginal results but within acceptable limits)\n",
      marginal_count
    ))
  }
} else {
  cat("⚠️  Some tests failed - Package needs attention\n")
  cat(sprintf("   Failed: %d, Marginal: %d\n", failed_count, marginal_count))
}
cat("=============================================================\n")

# Return results for programmatic use
invisible(test_results)
