# Test file for p-value adjustment functionality

library(testthat)
library(qiprng)

# Source required test helper files
unified_reporting_file <- system.file("statisticaltests", "unified_reporting.R", package = "qiprng")
if (file.exists(unified_reporting_file)) {
  source(unified_reporting_file)
} else {
  stop("Required file unified_reporting.R not found in package installation")
}

test_that("p-value adjustment function works correctly", {
  # Create a test suite with multiple test results
  suite <- test_suite_results("Test Suite")

  # Add test results with various p-values
  suite <- add_test_result(suite, test_result(
    test_name = "Test 1",
    description = "First test",
    result = "FAIL",
    p_value = 0.01
  ))

  suite <- add_test_result(suite, test_result(
    test_name = "Test 2",
    description = "Second test",
    result = "FAIL",
    p_value = 0.03
  ))

  suite <- add_test_result(suite, test_result(
    test_name = "Test 3",
    description = "Third test",
    result = "FAIL",
    p_value = 0.04
  ))

  suite <- add_test_result(suite, test_result(
    test_name = "Test 4",
    description = "Fourth test",
    result = "PASS",
    p_value = 0.06
  ))

  suite <- add_test_result(suite, test_result(
    test_name = "Test 5",
    description = "Fifth test (no p-value)",
    result = "PASS"
  ))

  # Finalize suite
  suite <- finalize_suite(suite)

  # Apply Benjamini-Hochberg adjustment
  adjusted_suite <- adjust_p_values(suite, method = "BH")

  # Check that adjusted p-values were added
  expect_true(!is.null(adjusted_suite$results[[1]]$p_adjusted))
  expect_true(!is.null(adjusted_suite$results[[2]]$p_adjusted))
  expect_true(!is.null(adjusted_suite$results[[3]]$p_adjusted))
  expect_true(!is.null(adjusted_suite$results[[4]]$p_adjusted))

  # Test 5 should not have adjusted p-value (no original p-value)
  expect_null(adjusted_suite$results[[5]]$p_adjusted)

  # Check that adjustment method is recorded
  expect_equal(adjusted_suite$results[[1]]$adjustment_method, "BH")
  expect_equal(adjusted_suite$config$p_adjustment_method, "BH")

  # Verify BH adjustment is less conservative than Bonferroni
  bonferroni_suite <- adjust_p_values(suite, method = "bonferroni")

  # BH adjusted p-values should be <= Bonferroni adjusted p-values
  for (i in 1:4) {
    expect_true(
      adjusted_suite$results[[i]]$p_adjusted <=
        bonferroni_suite$results[[i]]$p_adjusted
    )
  }
})

test_that("get_effective_p_value works correctly", {
  # Test with adjusted p-value available
  result1 <- test_result(
    test_name = "Test",
    description = "Test",
    result = "FAIL",
    p_value = 0.01,
    p_adjusted = 0.05
  )
  expect_equal(get_effective_p_value(result1), 0.05)

  # Test with only raw p-value
  result2 <- test_result(
    test_name = "Test",
    description = "Test",
    result = "FAIL",
    p_value = 0.01
  )
  expect_equal(get_effective_p_value(result2), 0.01)

  # Test with no p-value
  result3 <- test_result(
    test_name = "Test",
    description = "Test",
    result = "PASS"
  )
  expect_true(is.na(get_effective_p_value(result3)))
})

test_that("test_passes uses adjusted p-values correctly", {
  # Test that passes with adjusted p-value
  result1 <- test_result(
    test_name = "Test",
    description = "Test",
    result = "FAIL", # Original decision
    p_value = 0.01, # Would fail
    p_adjusted = 0.06 # Passes after adjustment
  )
  expect_true(test_passes(result1, alpha = 0.05))

  # Test that fails with adjusted p-value
  result2 <- test_result(
    test_name = "Test",
    description = "Test",
    result = "PASS", # Original decision
    p_value = 0.06, # Would pass
    p_adjusted = 0.04 # Fails after adjustment
  )
  expect_false(test_passes(result2, alpha = 0.05))
})

test_that("invalid adjustment methods are rejected", {
  suite <- test_suite_results("Test Suite")

  expect_error(
    adjust_p_values(suite, method = "invalid_method"),
    "Invalid p-value adjustment method"
  )
})

test_that("'none' adjustment method returns suite unchanged", {
  suite <- test_suite_results("Test Suite")
  suite <- add_test_result(suite, test_result(
    test_name = "Test",
    description = "Test",
    result = "FAIL",
    p_value = 0.01
  ))

  unchanged_suite <- adjust_p_values(suite, method = "none")

  # Should not have adjusted p-values
  expect_null(unchanged_suite$results[[1]]$p_adjusted)
  expect_null(unchanged_suite$config$p_adjustment_method)
})
