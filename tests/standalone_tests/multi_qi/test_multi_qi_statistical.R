#!/usr/bin/env Rscript

# Statistical test for Multi-QI configurations
library(qiprng)

cat("===========================================\n")
cat("Multi-QI Statistical Validation\n")
cat("===========================================\n\n")

# Test 1: Manual Multi-QI configuration
cat("Test 1: Manual Multi-QI (3 QIs)\n")
cfg <- list(
  a = c(2, 3, 5),
  b = c(5, 7, 11),
  c = c(-1, -2, -3),
  buffer_size = 1000,
  mpfr_precision = 53
)

createPRNG(cfg)
vals <- generatePRNG(10000)
cleanup_prng()

# Basic statistical tests
cat("  Mean:", mean(vals), "(expected: 0.5)\n")
cat("  SD:", sd(vals), "(expected: 0.289)\n")
cat("  Min:", min(vals), "Max:", max(vals), "\n")

# Kolmogorov-Smirnov test for uniformity
ks_test <- ks.test(vals, "punif")
cat("  KS test p-value:", format(ks_test$p.value, digits = 4), "\n")
cat("  Result:", ifelse(ks_test$p.value > 0.01, "✓ PASSED", "✗ FAILED"), "\n\n")

# Test 2: Single QI for comparison
cat("Test 2: Single QI\n")
cfg2 <- list(
  a = 2,
  b = 5,
  c = -1,
  buffer_size = 1000,
  mpfr_precision = 53
)

createPRNG(cfg2)
vals2 <- generatePRNG(10000)
cleanup_prng()

cat("  Mean:", mean(vals2), "(expected: 0.5)\n")
cat("  SD:", sd(vals2), "(expected: 0.289)\n")
cat("  Min:", min(vals2), "Max:", max(vals2), "\n")

ks_test2 <- ks.test(vals2, "punif")
cat("  KS test p-value:", format(ks_test2$p.value, digits = 4), "\n")
cat("  Result:", ifelse(ks_test2$p.value > 0.01, "✓ PASSED", "✗ FAILED"), "\n\n")

# Test 3: Different mixing strategies for Multi-QI
cat("Test 3: Multi-QI with different mixing strategies\n")
strategies <- c("round_robin", "xor_mix", "averaging", "modular_add", "cascade_mix")

for (i in seq_along(strategies)) {
  cat("\n  Strategy:", strategies[i], "\n")

  cfg3 <- list(
    a = c(2, 3, 5),
    b = c(5, 7, 11),
    c = c(-1, -2, -3),
    buffer_size = 1000,
    mpfr_precision = 53,
    mixing_strategy = strategies[i] # Use string name
  )

  createPRNG(cfg3)
  vals3 <- generatePRNG(10000)
  cleanup_prng()

  cat("    Mean:", format(mean(vals3), digits = 4), "\n")
  ks_test3 <- ks.test(vals3, "punif")
  cat("    KS p-value:", format(ks_test3$p.value, digits = 4), "\n")
  cat("    Result:", ifelse(ks_test3$p.value > 0.01, "✓ PASSED", "✗ FAILED"), "\n")
}

cat("\n===========================================\n")
cat("Multi-QI validation complete!\n")
cat("===========================================\n")
