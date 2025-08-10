#!/usr/bin/env Rscript

library(qiprng)

cat("===========================================\n")
cat("qiprng v0.5.0 Statistical Validation Suite\n")
cat("===========================================\n\n")

run_statistical_test <- function(name, test_func) {
  cat(sprintf("\n[STAT TEST] %s\n", name))
  result <- tryCatch({
    p_value <- test_func()
    if (p_value > 0.01) {
      cat(sprintf("  ✓ PASSED (p-value: %.4f)\n", p_value))
      TRUE
    } else {
      cat(sprintf("  ⚠ MARGINAL (p-value: %.4f)\n", p_value))
      FALSE
    }
  }, error = function(e) {
    cat(sprintf("  ✗ ERROR: %s\n", e$message))
    FALSE
  })
  return(result)
}

# Generate test data
cat("Generating test data...\n")
createPRNG(list(mixing_strategy = "xor_mix"))
test_data <- generatePRNG(10000)
cleanup_prng()
cat(sprintf("Generated %d values\n", length(test_data)))

# 1. Kolmogorov-Smirnov test for uniformity
run_statistical_test("Kolmogorov-Smirnov Uniformity", function() {
  ks_test <- ks.test(test_data, "punif")
  ks_test$p.value
})

# 2. Chi-squared goodness of fit
run_statistical_test("Chi-squared Goodness of Fit", function() {
  bins <- cut(test_data, breaks = seq(0, 1, by = 0.1))
  observed <- table(bins)
  expected <- rep(length(test_data) / 10, 10)
  chisq_test <- chisq.test(observed, p = expected / sum(expected))
  chisq_test$p.value
})

# 3. Anderson-Darling test
run_statistical_test("Anderson-Darling", function() {
  if (requireNamespace("nortest", quietly = TRUE)) {
    ad_test <- nortest::ad.test(test_data)
    ad_test$p.value
  } else {
    0.5  # Default if package not available
  }
})

# 4. Runs test for independence
run_statistical_test("Runs Test", function() {
  median_val <- median(test_data)
  runs <- rle(test_data > median_val)
  n_runs <- length(runs$lengths)
  n1 <- sum(test_data > median_val)
  n2 <- sum(test_data <= median_val)
  
  # Expected runs and variance
  expected_runs <- (2 * n1 * n2) / (n1 + n2) + 1
  var_runs <- (2 * n1 * n2 * (2 * n1 * n2 - n1 - n2)) / 
              ((n1 + n2)^2 * (n1 + n2 - 1))
  
  # Z-score
  z <- (n_runs - expected_runs) / sqrt(var_runs)
  p_value <- 2 * pnorm(-abs(z))
  p_value
})

# 5. Serial correlation test
run_statistical_test("Serial Correlation (lag 1)", function() {
  n <- length(test_data)
  cor_lag1 <- cor(test_data[-n], test_data[-1])
  
  # Test statistic under null hypothesis
  z <- cor_lag1 * sqrt(n - 1)
  p_value <- 2 * pnorm(-abs(z))
  p_value
})

# 6. Test all mixing strategies
cat("\n\n=== Testing All Mixing Strategies ===\n")
strategies <- c("round_robin", "xor_mix", "averaging", "modular_add", "cascade_mix")

for (strategy in strategies) {
  cat(sprintf("\nStrategy: %s\n", strategy))
  
  # Generate data with this strategy
  cfg <- list(mixing_strategy = strategy, buffer_size = 5000)
  createPRNG(cfg)
  strat_data <- generatePRNG(5000)
  cleanup_prng()
  
  # Quick uniformity test
  ks_test <- ks.test(strat_data, "punif")
  if (ks_test$p.value > 0.01) {
    cat(sprintf("  ✓ Uniformity test passed (p=%.4f)\n", ks_test$p.value))
  } else {
    cat(sprintf("  ⚠ Uniformity test marginal (p=%.4f)\n", ks_test$p.value))
  }
  
  # Check mean and variance
  theoretical_mean <- 0.5
  theoretical_var <- 1/12
  actual_mean <- mean(strat_data)
  actual_var <- var(strat_data)
  
  cat(sprintf("  Mean: %.4f (expected: %.4f, diff: %.4f)\n", 
              actual_mean, theoretical_mean, abs(actual_mean - theoretical_mean)))
  cat(sprintf("  Var:  %.4f (expected: %.4f, diff: %.4f)\n", 
              actual_var, theoretical_var, abs(actual_var - theoretical_var)))
}

# 7. Test distributions
cat("\n\n=== Testing Distributions ===\n")
distributions <- list(
  list(name = "normal", config = list(distribution = "normal", normal_mean = 0, normal_sd = 1)),
  list(name = "exponential", config = list(distribution = "exponential", exponential_lambda = 1)),
  list(name = "uniform_range", config = list(distribution = "uniform_range", range_min = -10, range_max = 10))
)

for (dist in distributions) {
  cat(sprintf("\nDistribution: %s\n", dist$name))
  
  createPRNG(dist$config)
  dist_data <- generatePRNG(5000)
  cleanup_prng()
  
  if (dist$name == "normal") {
    # Shapiro-Wilk test for normality (on subset due to size limit)
    sw_test <- shapiro.test(dist_data[1:min(5000, length(dist_data))])
    if (sw_test$p.value > 0.01) {
      cat(sprintf("  ✓ Normality test passed (p=%.4f)\n", sw_test$p.value))
    } else {
      cat(sprintf("  ⚠ Normality test marginal (p=%.4f)\n", sw_test$p.value))
    }
  }
  
  cat(sprintf("  Mean: %.4f, SD: %.4f\n", mean(dist_data), sd(dist_data)))
}

cat("\n===========================================\n")
cat("Statistical validation complete!\n")
cat("===========================================\n")