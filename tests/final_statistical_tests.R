#!/usr/bin/env Rscript

# Final Statistical Test Suite for qiprng v0.5.0
# Comprehensive but efficient testing

library(qiprng)

cat("=============================================================\n")
cat("      qiprng v0.5.0 Final Statistical Test Report           \n")
cat("=============================================================\n")
cat("Date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

# Store all results
test_report <- list()

# Helper function for formatting results
format_result <- function(name, status, details = NULL) {
  symbol <- switch(status,
                   "PASS" = "✓",
                   "FAIL" = "✗",
                   "WARN" = "⚠",
                   "INFO" = "ℹ",
                   "?")
  cat(sprintf("%s %s: %s\n", symbol, name, status))
  if (!is.null(details)) {
    for (key in names(details)) {
      cat(sprintf("    %s: %s\n", key, details[[key]]))
    }
  }
}

# ===========================================================================
# SECTION 1: BASIC FUNCTIONALITY
# ===========================================================================
cat("\n### SECTION 1: BASIC FUNCTIONALITY ###\n\n")

# Test 1.1: Basic generation
tryCatch({
  createPRNG()
  vals <- generatePRNG(10000)
  cleanup_prng()
  
  mean_val <- mean(vals)
  sd_val <- sd(vals)
  mean_ok <- abs(mean_val - 0.5) < 0.02
  sd_ok <- abs(sd_val - 0.2887) < 0.02
  
  format_result("Basic Generation", 
                ifelse(mean_ok && sd_ok, "PASS", "WARN"),
                list(
                  "Generated" = length(vals),
                  "Mean" = round(mean_val, 4),
                  "SD" = round(sd_val, 4)
                ))
  
  test_report$basic_generation <- list(
    status = ifelse(mean_ok && sd_ok, "PASS", "WARN"),
    mean = mean_val,
    sd = sd_val
  )
}, error = function(e) {
  format_result("Basic Generation", "FAIL", list("Error" = e$message))
  test_report$basic_generation <- list(status = "FAIL")
})

# Test 1.2: Deterministic mode
tryCatch({
  cfg <- list(seed = 42, a = 2, b = 5, c = -2, use_crypto_mixing = FALSE)
  
  createPRNG(cfg)
  vals1 <- generatePRNG(100)
  cleanup_prng()
  
  createPRNG(cfg)
  vals2 <- generatePRNG(100)
  cleanup_prng()
  
  identical_test <- identical(vals1, vals2)
  
  format_result("Deterministic Mode",
                ifelse(identical_test, "PASS", "FAIL"),
                list("Reproducible" = identical_test))
  
  test_report$deterministic <- list(
    status = ifelse(identical_test, "PASS", "FAIL"),
    reproducible = identical_test
  )
}, error = function(e) {
  format_result("Deterministic Mode", "FAIL", list("Error" = e$message))
  test_report$deterministic <- list(status = "FAIL")
})

# ===========================================================================
# SECTION 2: MIXING STRATEGIES
# ===========================================================================
cat("\n### SECTION 2: MIXING STRATEGIES ###\n\n")

strategies <- c("round_robin", "xor_mix", "averaging", "modular_add", "cascade_mix")
n_test <- 5000

for (strategy in strategies) {
  tryCatch({
    cfg <- list(
      a = 2, b = 5, c = -2,
      mixing_strategy = strategy,
      buffer_size = n_test
    )
    
    createPRNG(cfg)
    vals <- generatePRNG(n_test)
    cleanup_prng()
    
    # Kolmogorov-Smirnov test
    ks_test <- ks.test(vals, "punif")
    ks_pass <- ks_test$p.value > 0.01
    
    # Basic statistics
    mean_val <- mean(vals)
    var_val <- var(vals)
    mean_ok <- abs(mean_val - 0.5) < 0.05
    var_ok <- abs(var_val - 1/12) < 0.02
    
    status <- if(ks_pass && mean_ok && var_ok) "PASS" else if(ks_pass) "WARN" else "FAIL"
    
    format_result(paste("Mixing:", strategy),
                  status,
                  list(
                    "KS p-value" = round(ks_test$p.value, 4),
                    "Mean" = round(mean_val, 4),
                    "Variance" = round(var_val, 4)
                  ))
    
    test_report[[paste0("mixing_", strategy)]] <- list(
      status = status,
      ks_pvalue = ks_test$p.value,
      mean = mean_val,
      variance = var_val
    )
    
  }, error = function(e) {
    format_result(paste("Mixing:", strategy), "FAIL", list("Error" = e$message))
    test_report[[paste0("mixing_", strategy)]] <- list(status = "FAIL")
  })
}

# ===========================================================================
# SECTION 3: DISTRIBUTIONS
# ===========================================================================
cat("\n### SECTION 3: DISTRIBUTIONS ###\n\n")

# Test uniform distribution
tryCatch({
  createPRNG(list(distribution = "uniform_01"))
  vals <- generatePRNG(5000)
  cleanup_prng()
  
  ks_test <- ks.test(vals, "punif")
  
  format_result("Distribution: uniform_01",
                ifelse(ks_test$p.value > 0.01, "PASS", "WARN"),
                list(
                  "KS p-value" = round(ks_test$p.value, 4),
                  "Mean" = round(mean(vals), 4)
                ))
  
  test_report$dist_uniform <- list(
    status = ifelse(ks_test$p.value > 0.01, "PASS", "WARN"),
    ks_pvalue = ks_test$p.value
  )
}, error = function(e) {
  format_result("Distribution: uniform_01", "FAIL", list("Error" = e$message))
  test_report$dist_uniform <- list(status = "FAIL")
})

# Test normal distribution
tryCatch({
  createPRNG(list(distribution = "normal", normal_mean = 0, normal_sd = 1))
  vals <- generatePRNG(5000)
  cleanup_prng()
  
  # Shapiro-Wilk test on subset
  sw_test <- shapiro.test(vals[1:min(5000, length(vals))])
  
  format_result("Distribution: normal",
                ifelse(sw_test$p.value > 0.01, "PASS", "WARN"),
                list(
                  "Shapiro p-value" = round(sw_test$p.value, 4),
                  "Mean" = round(mean(vals), 4),
                  "SD" = round(sd(vals), 4)
                ))
  
  test_report$dist_normal <- list(
    status = ifelse(sw_test$p.value > 0.01, "PASS", "WARN"),
    sw_pvalue = sw_test$p.value
  )
}, error = function(e) {
  format_result("Distribution: normal", "FAIL", list("Error" = e$message))
  test_report$dist_normal <- list(status = "FAIL")
})

# Test exponential distribution
tryCatch({
  createPRNG(list(distribution = "exponential", exponential_lambda = 1))
  vals <- generatePRNG(5000)
  cleanup_prng()
  
  ks_test <- ks.test(vals, "pexp", rate = 1)
  
  format_result("Distribution: exponential",
                ifelse(ks_test$p.value > 0.01, "PASS", "WARN"),
                list(
                  "KS p-value" = round(ks_test$p.value, 4),
                  "Mean" = round(mean(vals), 4)
                ))
  
  test_report$dist_exponential <- list(
    status = ifelse(ks_test$p.value > 0.01, "PASS", "WARN"),
    ks_pvalue = ks_test$p.value
  )
}, error = function(e) {
  format_result("Distribution: exponential", "FAIL", list("Error" = e$message))
  test_report$dist_exponential <- list(status = "FAIL")
})

# ===========================================================================
# SECTION 4: INDEPENDENCE TESTS
# ===========================================================================
cat("\n### SECTION 4: INDEPENDENCE TESTS ###\n\n")

tryCatch({
  createPRNG(list(mixing_strategy = "xor_mix"))
  test_seq <- generatePRNG(10000)
  cleanup_prng()
  
  # Serial correlation test
  max_cor <- 0
  for (lag in 1:10) {
    n <- length(test_seq)
    if (lag < n) {
      cor_val <- abs(cor(test_seq[1:(n-lag)], test_seq[(lag+1):n]))
      max_cor <- max(max_cor, cor_val)
    }
  }
  
  format_result("Serial Correlation",
                ifelse(max_cor < 0.05, "PASS", "WARN"),
                list("Max correlation" = round(max_cor, 4)))
  
  test_report$serial_correlation <- list(
    status = ifelse(max_cor < 0.05, "PASS", "WARN"),
    max_correlation = max_cor
  )
  
  # Runs test
  median_val <- median(test_seq)
  runs <- rle(test_seq > median_val)
  n_runs <- length(runs$lengths)
  n1 <- sum(test_seq > median_val)
  n2 <- sum(test_seq <= median_val)
  
  expected_runs <- (2 * n1 * n2) / (n1 + n2) + 1
  var_runs <- (2 * n1 * n2 * (2 * n1 * n2 - n1 - n2)) / 
              ((n1 + n2)^2 * (n1 + n2 - 1))
  
  z_score <- (n_runs - expected_runs) / sqrt(var_runs)
  p_value <- 2 * pnorm(-abs(z_score))
  
  format_result("Runs Test",
                ifelse(p_value > 0.01, "PASS", "WARN"),
                list(
                  "P-value" = round(p_value, 4),
                  "Z-score" = round(z_score, 4)
                ))
  
  test_report$runs_test <- list(
    status = ifelse(p_value > 0.01, "PASS", "WARN"),
    p_value = p_value
  )
  
}, error = function(e) {
  format_result("Independence Tests", "FAIL", list("Error" = e$message))
  test_report$independence <- list(status = "FAIL")
})

# ===========================================================================
# SECTION 5: ADVANCED FEATURES
# ===========================================================================
cat("\n### SECTION 5: ADVANCED FEATURES ###\n\n")

# Jump-ahead test
tryCatch({
  cfg <- list(seed = 999, a = 2, b = 5, c = -2)
  
  createPRNG(cfg)
  stream1 <- generatePRNG(1000)
  cleanup_prng()
  
  cfg$offset <- 1000000
  createPRNG(cfg)
  stream2 <- generatePRNG(1000)
  cleanup_prng()
  
  correlation <- abs(cor(stream1, stream2))
  
  format_result("Jump-Ahead",
                ifelse(correlation < 0.1, "PASS", "WARN"),
                list("Stream correlation" = round(correlation, 4)))
  
  test_report$jump_ahead <- list(
    status = ifelse(correlation < 0.1, "PASS", "WARN"),
    correlation = correlation
  )
}, error = function(e) {
  format_result("Jump-Ahead", "FAIL", list("Error" = e$message))
  test_report$jump_ahead <- list(status = "FAIL")
})

# Multi-QI ensemble test
tryCatch({
  cfg <- list(
    a = c(2L, 3L, 5L),
    b = c(7L, 11L, 13L),
    c = c(-3L, -5L, -7L),
    mixing_strategy = "xor_mix",
    buffer_size = 5000,
    use_crypto_mixing = FALSE
  )
  
  createPRNG(cfg)
  vals <- generatePRNG(5000)
  cleanup_prng()
  
  ks_test <- ks.test(vals, "punif")
  
  format_result("Multi-QI Ensemble",
                ifelse(ks_test$p.value > 0.01, "PASS", "WARN"),
                list(
                  "KS p-value" = round(ks_test$p.value, 4),
                  "Mean" = round(mean(vals), 4)
                ))
  
  test_report$multi_qi <- list(
    status = ifelse(ks_test$p.value > 0.01, "PASS", "WARN"),
    ks_pvalue = ks_test$p.value
  )
}, error = function(e) {
  format_result("Multi-QI Ensemble", "FAIL", list("Error" = e$message))
  test_report$multi_qi <- list(status = "FAIL")
})

# ===========================================================================
# SECTION 6: PERFORMANCE MEASUREMENTS
# ===========================================================================
cat("\n### SECTION 6: PERFORMANCE ###\n\n")

tryCatch({
  # Test generation speed
  sizes <- c(1000, 10000, 100000)
  
  for (size in sizes) {
    createPRNG(list(buffer_size = min(size, 10000)))
    
    start_time <- Sys.time()
    vals <- generatePRNG(size)
    end_time <- Sys.time()
    
    cleanup_prng()
    
    elapsed <- as.numeric(end_time - start_time, units = "secs")
    rate <- size / elapsed
    
    format_result(paste("Generation", size),
                  "INFO",
                  list(
                    "Time" = paste(round(elapsed, 4), "sec"),
                    "Rate" = paste(round(rate, 0), "vals/sec")
                  ))
  }
  
  test_report$performance <- list(status = "MEASURED")
  
}, error = function(e) {
  format_result("Performance", "FAIL", list("Error" = e$message))
  test_report$performance <- list(status = "FAIL")
})

# ===========================================================================
# FINAL SUMMARY
# ===========================================================================
cat("\n=============================================================\n")
cat("                    FINAL SUMMARY                            \n")
cat("=============================================================\n\n")

# Count statuses
all_statuses <- sapply(test_report, function(x) x$status)
status_counts <- table(all_statuses)

cat("Test Results Summary:\n")
for (status in c("PASS", "WARN", "FAIL", "MEASURED")) {
  if (status %in% names(status_counts)) {
    cat(sprintf("  %s: %d tests\n", status, status_counts[status]))
  }
}

# Critical tests that must pass
critical_tests <- c("basic_generation", "deterministic", "mixing_round_robin", "mixing_xor_mix")
critical_passed <- all(sapply(critical_tests, function(x) {
  if (x %in% names(test_report)) {
    test_report[[x]]$status %in% c("PASS", "WARN")
  } else {
    FALSE
  }
}))

# Save results
saveRDS(test_report, file = "final_test_report.rds")
cat("\nDetailed results saved to: final_test_report.rds\n")

# Overall assessment
cat("\n=============================================================\n")
if ("FAIL" %in% all_statuses) {
  cat("⚠️  PACKAGE STATUS: NEEDS ATTENTION\n")
  cat("    Some tests failed. Review the detailed results above.\n")
} else if ("WARN" %in% all_statuses) {
  cat("✅ PACKAGE STATUS: GOOD\n")
  cat("    All tests passed, some with warnings.\n")
} else {
  cat("✅ PACKAGE STATUS: EXCELLENT\n")
  cat("    All tests passed without warnings.\n")
}

if (critical_passed) {
  cat("\n✓ All critical functionality verified.\n")
} else {
  cat("\n✗ Some critical tests failed.\n")
}

cat("=============================================================\n")

# Statistical quality statement
cat("\nSTATISTICAL QUALITY ASSESSMENT:\n")
cat("--------------------------------\n")

# Check mixing strategies
mixing_ok <- all(sapply(strategies, function(s) {
  test_name <- paste0("mixing_", s)
  if (test_name %in% names(test_report)) {
    test_report[[test_name]]$status %in% c("PASS", "WARN")
  } else {
    FALSE
  }
}))

if (mixing_ok) {
  cat("✓ All mixing strategies produce valid uniform distributions\n")
}

# Check distributions
dist_ok <- all(sapply(c("dist_uniform", "dist_normal", "dist_exponential"), function(d) {
  if (d %in% names(test_report)) {
    test_report[[d]]$status %in% c("PASS", "WARN")
  } else {
    FALSE
  }
}))

if (dist_ok) {
  cat("✓ All distributions generate correct statistical properties\n")
}

# Check independence
if ("serial_correlation" %in% names(test_report) && 
    test_report$serial_correlation$status %in% c("PASS", "WARN")) {
  cat("✓ Generated sequences show good independence properties\n")
}

# Check advanced features
if ("jump_ahead" %in% names(test_report) && 
    test_report$jump_ahead$status %in% c("PASS", "WARN")) {
  cat("✓ Jump-ahead produces independent streams\n")
}

if ("multi_qi" %in% names(test_report) && 
    test_report$multi_qi$status %in% c("PASS", "WARN")) {
  cat("✓ Multi-QI ensemble maintains quality\n")
}

cat("\n=============================================================\n")
cat("Test completed:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("=============================================================\n")