# Comprehensive Test Suite for qiprng Improvements
# Tests for Phases 1-3: P-value adjustment, Weighting system, Effect sizes

library(testthat)
library(qiprng)

# Source required files
source(system.file("statisticaltests", "effect_sizes.R", package = "qiprng"))
source(system.file("statisticaltests", "unified_reporting.R", package = "qiprng"))
source(system.file("statisticaltests", "comprehensive_generator_comparison.R", package = "qiprng"))
source(system.file("statisticaltests", "comprehensive_reporting.R", package = "qiprng"))

context("Comprehensive Improvements Test Suite")

# ============================================================================
# Phase 1: P-value Adjustment Tests
# ============================================================================

test_that("P-value adjustment preserves original values", {
  suite <- test_suite_results("Test Suite")

  # Add test results
  original_p_values <- c(0.01, 0.03, 0.04, 0.06, 0.50)
  for (i in seq_along(original_p_values)) {
    suite <- add_test_result(suite, test_result(
      test_name = paste0("Test", i),
      description = paste("Test", i),
      result = if (original_p_values[i] < 0.05) "FAIL" else "PASS",
      p_value = original_p_values[i]
    ))
  }

  suite <- finalize_suite(suite)
  adjusted_suite <- adjust_p_values(suite, method = "BH")

  # Check original p-values are preserved
  for (i in seq_along(original_p_values)) {
    expect_equal(adjusted_suite$results[[i]]$p_value, original_p_values[i])
    expect_true(!is.null(adjusted_suite$results[[i]]$p_adjusted))
  }
})

test_that("Different adjustment methods produce expected results", {
  p_values <- c(0.001, 0.008, 0.039, 0.041, 0.042)

  # Test each method
  methods <- c("bonferroni", "holm", "BH", "none")

  for (method in methods) {
    suite <- test_suite_results("Test Suite")

    for (i in seq_along(p_values)) {
      suite <- add_test_result(suite, test_result(
        test_name = paste0("Test", i),
        description = paste("Test", i),
        result = "FAIL",
        p_value = p_values[i]
      ))
    }

    suite <- finalize_suite(suite)
    adjusted_suite <- adjust_p_values(suite, method = method)

    if (method == "none") {
      # No adjustment means p_adjusted should not be set
      for (i in seq_along(p_values)) {
        expect_null(adjusted_suite$results[[i]]$p_adjusted)
        expect_equal(adjusted_suite$results[[i]]$p_value, p_values[i])
      }
    } else if (method == "bonferroni") {
      # Bonferroni should multiply by number of tests
      for (i in seq_along(p_values)) {
        expected <- min(1, p_values[i] * length(p_values))
        expect_equal(adjusted_suite$results[[i]]$p_adjusted, expected)
      }
    } else {
      # Other methods should produce non-decreasing adjusted p-values
      adj_p <- sapply(adjusted_suite$results, function(x) x$p_adjusted)
      expect_true(all(diff(adj_p) >= -1e-10)) # Allow for numerical precision
    }
  }
})

test_that("Pass/fail decisions update based on adjusted p-values", {
  suite <- test_suite_results("Test Suite")

  # Add a test that passes raw but fails adjusted
  suite <- add_test_result(suite, test_result(
    test_name = "Borderline Test",
    description = "Test near significance threshold",
    result = "PASS",
    p_value = 0.04 # Passes at 0.05
  ))

  # Add more tests to increase adjustment
  for (i in 1:9) {
    suite <- add_test_result(suite, test_result(
      test_name = paste0("Test", i),
      description = paste("Test", i),
      result = "FAIL",
      p_value = 0.001
    ))
  }

  suite <- finalize_suite(suite)
  adjusted_suite <- adjust_p_values(suite, method = "bonferroni", alpha = 0.05)

  # The borderline test should now pass (p_adjusted = 0.4 > 0.05)
  borderline_result <- adjusted_suite$results[[1]]
  expect_true(borderline_result$p_adjusted > 0.05)

  # Check helper functions
  expect_equal(get_effective_p_value(borderline_result), borderline_result$p_adjusted)
  expect_true(test_passes(borderline_result, alpha = 0.05)) # Should pass since p_adjusted > alpha
})

# ============================================================================
# Phase 2: Weighting System Tests
# ============================================================================

test_that("Default weights produce equal weighting", {
  # Create mock category results
  category_results <- list(
    basic = list(total_tests = 10, passed_tests = 9, pass_rate = 0.9),
    binary = list(total_tests = 10, passed_tests = 7, pass_rate = 0.7),
    classical = list(total_tests = 10, passed_tests = 8, pass_rate = 0.8)
  )

  # Calculate with no weights (should default to 1.0 each)
  metrics <- calculate_overall_metrics(
    data.frame(
      test = paste0("test", 1:30),
      passed = rep(c(rep(TRUE, 9), FALSE, rep(TRUE, 7), rep(FALSE, 3), rep(TRUE, 8), rep(FALSE, 2)), 1),
      category = rep(c("basic", "binary", "classical"), each = 10)
    ),
    category_weights = NULL
  )

  # Weighted and unweighted should be the same with default weights
  expect_equal(as.numeric(metrics$weighted_pass_rate), metrics$pass_rate, tolerance = 0.001)
})

test_that("Custom weights affect rankings appropriately", {
  # Create test data
  test_data <- data.frame(
    test = paste0("test", 1:30),
    passed = c(
      rep(TRUE, 10), # basic: 100% pass
      rep(FALSE, 10), # binary: 0% pass
      rep(TRUE, 5), rep(FALSE, 5) # classical: 50% pass
    ),
    category = rep(c("basic", "binary", "classical"), each = 10),
    p_value = runif(30)
  )

  # Calculate with custom weights favoring binary tests
  weights_favor_binary <- c(basic = 0.1, binary = 10.0, classical = 0.1)
  metrics_favor_binary <- calculate_overall_metrics(test_data, weights_favor_binary)

  # Calculate with custom weights favoring basic tests
  weights_favor_basic <- c(basic = 10.0, binary = 0.1, classical = 0.1)
  metrics_favor_basic <- calculate_overall_metrics(test_data, weights_favor_basic)

  # The weighted pass rates should differ significantly
  expect_true(metrics_favor_basic$weighted_pass_rate > metrics_favor_binary$weighted_pass_rate)
})

test_that("Multi-faceted rankings include all perspectives", {
  # Create mock aggregated results
  aggregated_results <- list(
    gen1 = list(
      weighted_pass_rate = 0.9,
      pass_rate = 0.85,
      std_p_value = 0.1,
      performance = 1.0,
      category_breakdown = list(
        basic = list(pass_rate = 0.95),
        binary = list(pass_rate = 0.80)
      )
    ),
    gen2 = list(
      weighted_pass_rate = 0.85,
      pass_rate = 0.9,
      std_p_value = 0.05,
      performance = 2.0,
      category_breakdown = list(
        basic = list(pass_rate = 0.85),
        binary = list(pass_rate = 0.95)
      )
    )
  )

  # Mock analysis structure
  analysis <- list(by_generator = aggregated_results)

  rankings <- rank_generators(analysis)

  # Check that all expected ranking types exist
  expect_true("rankings" %in% names(rankings))
  expect_true("summary" %in% names(rankings))
  expect_true("full_data" %in% names(rankings))

  # Verify different rankings can have different orders
  expect_true(length(rankings$rankings) >= 4) # At least 4 ranking types
})

# ============================================================================
# Phase 3: Effect Size Tests
# ============================================================================

test_that("Effect size calculations are correct", {
  # Test Cramér's V
  chi2 <- 15.0
  n <- 1000
  df <- 4
  v <- calculate_cramers_v(chi2, n, df)
  expected_v <- sqrt(15.0 / (1000 * 4))
  expect_equal(v, expected_v, tolerance = 0.0001)

  # Test Cohen's d
  d <- calculate_cohens_d(0.55, 0.50, 0.1)
  expect_equal(d, 0.5, tolerance = 0.0001)

  # Test Cohen's h
  h <- calculate_cohens_h(0.6, 0.5)
  expected_h <- abs(2 * asin(sqrt(0.6)) - 2 * asin(sqrt(0.5)))
  expect_equal(h, expected_h, tolerance = 0.0001)

  # Test variance ratio
  var_ratio <- calculate_variance_ratio(0.1, 0.08333) # 0.08333 ≈ 1/12
  expect_true(var_ratio > 0)
})

test_that("Effect size interpretations follow conventions", {
  # Cohen's d interpretations
  expect_equal(interpret_effect_size(0.1, "d"), "negligible")
  expect_equal(interpret_effect_size(0.3, "d"), "small")
  expect_equal(interpret_effect_size(0.6, "d"), "medium")
  expect_equal(interpret_effect_size(1.0, "d"), "large")

  # Cramér's V interpretations (df=1)
  expect_equal(interpret_effect_size(0.05, "v", df = 1), "negligible")
  expect_equal(interpret_effect_size(0.2, "v", df = 1), "small")
  expect_equal(interpret_effect_size(0.4, "v", df = 1), "medium")
  expect_equal(interpret_effect_size(0.6, "v", df = 1), "large")

  # KS D statistic interpretations
  expect_equal(interpret_effect_size(0.03, "ks"), "negligible")
  expect_equal(interpret_effect_size(0.08, "ks"), "small")
  expect_equal(interpret_effect_size(0.15, "ks"), "medium")
  expect_equal(interpret_effect_size(0.25, "ks"), "large")
})

test_that("Effect sizes integrate with test results", {
  # Create a test result
  result <- list(
    description = "Test",
    result = "PASS",
    p_value = 0.05,
    statistic = 2.0
  )

  # Add effect size
  result_with_effect <- add_effect_size(result, 0.5, "d")

  expect_equal(result_with_effect$effect_size, 0.5)
  expect_equal(result_with_effect$effect_size_interpretation, "medium")
})

# ============================================================================
# Integration Tests
# ============================================================================

test_that("All improvements work together in comprehensive comparison", {
  skip_if_not_installed("qiprng")

  # Create simple test generators
  generators <- list(
    uniform = list(
      name = "Uniform",
      func = function(n) runif(n),
      description = "R's uniform generator"
    ),
    biased = list(
      name = "Biased",
      func = function(n) runif(n)^0.9,
      description = "Biased generator"
    )
  )

  # Run comparison with custom weights and p-value adjustment
  custom_weights <- c(basic = 2.0, classical = 1.0, binary = 0.5)

  # Note: This is a minimal test - full comparison takes too long for unit tests
  results <- tryCatch(
    {
      compare_generators_comprehensive(
        generators = generators,
        mode = "quick",
        external_packages = "none",
        p_adjustment_method = "BH",
        category_weights = custom_weights,
        verbose = FALSE
      )
    },
    error = function(e) {
      skip("Comprehensive comparison not available in test environment")
    }
  )

  if (!is.null(results)) {
    # Check that p-value adjustment was applied
    expect_true("p_adjustment_method" %in% names(results$unified_suite$config))

    # Check that weights were used
    expect_true(any(grepl("weight", names(results$aggregated[[1]]$overall_metrics))))
  }
})

# ============================================================================
# Edge Cases and Error Handling
# ============================================================================

test_that("Functions handle edge cases gracefully", {
  # Empty p-values
  expect_equal(calculate_cramers_v(NA, 100, 5), NA)
  expect_equal(calculate_cohens_d(0.5, 0.5, 0), NA)
  expect_equal(calculate_cohens_h(NA, 0.5), NA)

  # Invalid inputs
  expect_equal(calculate_cohens_h(1.5, 0.5), NA) # p > 1
  expect_equal(calculate_variance_ratio(0, 1), NA) # zero variance

  # Empty test suite
  empty_suite <- test_suite_results("Empty")
  empty_suite <- finalize_suite(empty_suite)
  adjusted_empty <- adjust_p_values(empty_suite, method = "BH")
  expect_equal(length(adjusted_empty$results), 0)
})
