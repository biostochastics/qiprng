# Demonstration of p-value adjustment in comprehensive generator comparison
# This shows how multiple testing correction prevents false discoveries

# library(qiprng) # Not needed - functions are available within package

# Function to demonstrate the effect of multiple testing
demonstrate_multiple_testing_correction <- function() {
  cat("=== P-value Adjustment Demonstration ===\n\n")

  # Simulate a scenario with 20 tests
  # Under null hypothesis (no real effect), we expect 5% false positives
  set.seed(42)
  n_tests <- 20
  p_values <- runif(n_tests) # Random p-values under null

  # Add a couple of truly significant results
  p_values[1:2] <- c(0.001, 0.002)

  cat("Simulated p-values from", n_tests, "tests:\n")
  cat(sprintf("%.4f ", sort(p_values)), "\n\n")

  # Count "significant" results without correction
  alpha <- 0.05
  sig_raw <- sum(p_values < alpha)
  cat("Without correction:\n")
  cat(sprintf(
    "  - Significant tests: %d/%d (%.1f%%)\n",
    sig_raw, n_tests, 100 * sig_raw / n_tests
  ))
  cat(sprintf("  - Expected false positives: %.1f\n\n", n_tests * alpha))

  # Apply different correction methods
  methods <- c("bonferroni", "holm", "BH", "BY")

  cat("With multiple testing correction:\n")
  for (method in methods) {
    p_adj <- p.adjust(p_values, method = method)
    sig_adj <- sum(p_adj < alpha)

    cat(sprintf("  %s method:\n", method))
    cat(sprintf("    - Significant tests: %d/%d\n", sig_adj, n_tests))
    cat(sprintf("    - Smallest adjusted p-value: %.4f\n", min(p_adj)))
  }

  cat("\n")
}

# Function to show the effect on generator comparison
demonstrate_generator_comparison <- function() {
  cat("=== Generator Comparison with P-value Adjustment ===\n\n")

  # Create mock generators for demonstration
  generators <- list(
    good_rng = list(
      name = "Good RNG",
      description = "A high-quality RNG",
      func = function(n) runif(n)
    ),
    bad_rng = list(
      name = "Bad RNG",
      description = "A poor quality RNG with bias",
      func = function(n) {
        # Simulate a biased RNG
        x <- runif(n)
        # Add slight bias
        x <- pmin(1, x + 0.02)
        return(x)
      }
    )
  )

  # Run quick comparison without adjustment
  cat("Running comparison WITHOUT p-value adjustment:\n")
  results_no_adj <- compare_generators_comprehensive(
    generators = generators,
    mode = "quick",
    p_adjustment_method = "none",
    verbose = FALSE
  )

  # Run with adjustment
  cat("\nRunning comparison WITH p-value adjustment (BH method):\n")
  results_with_adj <- compare_generators_comprehensive(
    generators = generators,
    mode = "quick",
    p_adjustment_method = "BH",
    verbose = FALSE
  )

  # Compare results
  cat("\nComparison of results:\n")
  cat("(This is a simplified demonstration - actual results would be more detailed)\n")

  # In practice, you would extract and compare the actual test results
  # This demonstrates the concept

  cat("\nKey takeaway: P-value adjustment reduces false discoveries,\n")
  cat("leading to more reliable generator comparisons.\n")
}

# Run demonstrations
if (interactive()) {
  demonstrate_multiple_testing_correction()

  cat("\n", paste(rep("=", 60), collapse = ""), "\n\n")

  # Note: The second demonstration requires the full framework
  # Uncomment to run if all components are available:
  # demonstrate_generator_comparison()
}
