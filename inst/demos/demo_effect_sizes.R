# Demo: Effect Size Calculations in qiprng
# ========================================

# Load required files
source("R/statisticaltests/effect_sizes.R")
source("R/statisticaltests/unified_reporting.R")
source("R/statisticaltests/basic_tests.R")

# Create test generators
generators <- list(
  good_rng = list(
    name = "Good RNG (R's runif)",
    func = function(n) runif(n),
    description = "R's built-in uniform RNG"
  ),
  biased_rng = list(
    name = "Biased RNG",
    func = function(n) {
      # Slightly biased towards higher values
      x <- runif(n)
      x^0.9 # Subtle bias
    },
    description = "RNG with subtle bias towards higher values"
  ),
  poor_rng = list(
    name = "Poor RNG",
    func = function(n) {
      # More obvious bias
      x <- runif(n)
      ifelse(x < 0.5, x * 0.8, x * 1.2 - 0.2)
    },
    description = "RNG with obvious distribution problems"
  )
)

# Run tests with effect sizes
cat("=== Testing RNGs with Effect Size Calculations ===\n\n")

for (gen_name in names(generators)) {
  gen <- generators[[gen_name]]

  cat(sprintf("\nTesting: %s\n", gen$name))
  cat(sprintf("Description: %s\n", gen$description))
  cat(rep("-", 60), "\n", sep = "")

  # Create minimal test suite
  suite <- list(
    prng_func = gen$func,
    config = list(
      basic_sample_size = 10000,
      chi_squared_bins = 20,
      significance_level = 0.05,
      save_visualizations = FALSE
    ),
    results = list()
  )

  # Run basic tests
  suite <- run_basic_tests(suite)

  # Display results with effect sizes
  tests <- c("ks_test", "chi_squared", "mean_test", "variance_test", "min_test", "max_test")

  for (test_name in tests) {
    test_result <- suite$results$basic[[test_name]]
    if (!is.null(test_result)) {
      cat(sprintf("\n%s:\n", test_result$description))
      cat(sprintf("  Result: %s\n", test_result$result))
      cat(sprintf("  p-value: %.4f\n", test_result$p_value))

      if (!is.null(test_result$effect_size) && !is.na(test_result$effect_size)) {
        cat(sprintf(
          "  Effect size: %.4f (%s)\n",
          test_result$effect_size,
          test_result$effect_size_interpretation
        ))
      }
    }
  }

  cat("\n")
}

# Demonstrate effect size interpretations
cat("\n=== Effect Size Interpretation Guide ===\n\n")

# Cohen's d examples
cat("Cohen's d (mean differences):\n")
d_values <- c(0.1, 0.3, 0.6, 1.0)
for (d in d_values) {
  interp <- interpret_effect_size(d, type = "d")
  cat(sprintf("  d = %.1f: %s\n", d, interp))
}

cat("\nCramér's V (chi-squared tests):\n")
v_values <- c(0.05, 0.15, 0.35, 0.6)
for (v in v_values) {
  interp <- interpret_effect_size(v, type = "v", df = 19) # 20 bins - 1
  cat(sprintf("  V = %.2f: %s (df=19)\n", v, interp))
}

cat("\nKS D statistic:\n")
d_values <- c(0.03, 0.08, 0.15, 0.25)
for (d in d_values) {
  interp <- interpret_effect_size(d, type = "ks")
  cat(sprintf("  D = %.2f: %s\n", d, interp))
}

cat("\n=== Summary ===\n")
cat("Effect sizes provide information about the magnitude of deviations,\n")
cat("complementing p-values which only indicate statistical significance.\n")
cat("This helps distinguish between:\n")
cat("- Statistically significant but practically negligible effects\n")
cat("- Large effects that may not reach significance due to small samples\n")
