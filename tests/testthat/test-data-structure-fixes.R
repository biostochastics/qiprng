# Test file for data structure fixes
# Tests for Phase 3 - Data Issues fixes

library(testthat)
library(qiprng)

# Source the necessary files from inst/statisticaltests
# Use system.file() to locate files in the installed package structure
classical_tests_file <- system.file("statisticaltests", "classical_tests.R", package = "qiprng")
if (file.exists(classical_tests_file)) {
  source(classical_tests_file)
}

external_tests_file <- system.file("statisticaltests", "external_tests.R", package = "qiprng")
if (file.exists(external_tests_file)) {
  source(external_tests_file)
}

external_wrappers_file <- system.file("statisticaltests", "external_wrappers.R", package = "qiprng")
if (file.exists(external_wrappers_file)) {
  source(external_wrappers_file)
}

# Helper function to create test suite
create_test_suite <- function(prng_func, config) {
  list(
    prng_func = prng_func,
    config = config,
    results = list()
  )
}

test_that("Classical tests return standardized structure", {
  # Create a simple test suite
  suite <- create_test_suite(
    prng_func = function(n) runif(n),
    config = list(
      classical_sample_size = 10000,
      significance_level = 0.05,
      save_visualizations = FALSE
    )
  )

  # Run classical tests
  suite <- run_classical_tests(suite)

  # Check that classical results exist
  expect_true("classical" %in% names(suite$results))

  # Define expected structure
  expected_fields <- c("description", "result", "p_value", "statistic", "details")

  # Check each classical test
  classical_tests <- c("coupon_collector", "poker_hand", "birthday_spacing")

  for (test_name in classical_tests) {
    test_result <- suite$results$classical[[test_name]]

    # Check that test exists
    expect_true(test_name %in% names(suite$results$classical),
      info = paste("Test", test_name, "should exist")
    )

    # Check all required fields exist
    for (field in expected_fields) {
      expect_true(field %in% names(test_result),
        info = paste("Field", field, "missing from", test_name)
      )
    }

    # Check field types
    expect_type(test_result$description, "character")
    expect_type(test_result$result, "character")
    expect_true(test_result$result %in% c("PASS", "FAIL", "INCONCLUSIVE", "ERROR"))
    expect_type(test_result$details, "character")

    # p_value and statistic can be NA but should be numeric type
    if (!is.na(test_result$p_value)) {
      expect_type(test_result$p_value, "double")
      expect_true(test_result$p_value >= 0 && test_result$p_value <= 1)
    }

    if (!is.na(test_result$statistic)) {
      expect_type(test_result$statistic, "double")
    }
  }
})

test_that("External tests use wrapper implementation", {
  # Create a test suite
  suite <- create_test_suite(
    prng_func = function(n) runif(n),
    config = list(
      external_sample_size = 5000,
      significance_level = 0.05,
      save_visualizations = FALSE
    )
  )

  # Run external tests
  suite <- run_external_tests(suite)

  # Check that external results exist
  expect_true("external" %in% names(suite$results))

  # Check standard base R tests are present
  base_r_tests <- c("kolmogorov_smirnov", "chi_square", "shapiro_wilk")

  for (test_name in base_r_tests) {
    if (test_name %in% names(suite$results$external)) {
      test_result <- suite$results$external[[test_name]]

      # Check structure
      expect_true("description" %in% names(test_result))
      expect_true("result" %in% names(test_result))
      expect_true("details" %in% names(test_result))

      # Check result status
      expect_true(test_result$result %in% c("PASS", "FAIL", "ERROR", "SKIPPED"))

      # If not error/skipped, should have p_value and statistic
      if (test_result$result %in% c("PASS", "FAIL")) {
        expect_true("p_value" %in% names(test_result))
        expect_true("statistic" %in% names(test_result))
      }
    }
  }
})

test_that("Error handling works correctly", {
  # Test with insufficient data
  suite <- create_test_suite(
    prng_func = function(n) runif(n),
    config = list(
      classical_sample_size = 10, # Too small for most tests
      significance_level = 0.05,
      save_visualizations = FALSE
    )
  )

  # Run classical tests
  suite <- run_classical_tests(suite)

  # Check that tests handle insufficient data gracefully
  for (test_name in names(suite$results$classical)) {
    test_result <- suite$results$classical[[test_name]]

    # Should have a result status
    expect_true("result" %in% names(test_result))

    # If INCONCLUSIVE, should have NA p_value
    if (test_result$result == "INCONCLUSIVE") {
      expect_true(is.na(test_result$p_value))
    }

    # Should always have details explaining the issue
    expect_true("details" %in% names(test_result))
    expect_type(test_result$details, "character")
    expect_true(nchar(test_result$details) > 0)
  }
})

test_that("Visualization function works without errors", {
  skip_if_not_installed("ggplot2")

  # Create test suite with visualization enabled
  temp_dir <- tempdir()
  suite <- create_test_suite(
    prng_func = function(n) runif(n),
    config = list(
      classical_sample_size = 1000,
      significance_level = 0.05,
      save_visualizations = TRUE,
      output_dir = temp_dir
    )
  )

  # Run classical tests
  expect_no_error(suite <- run_classical_tests(suite))

  # Check visualization directory was created
  viz_dir <- file.path(temp_dir, "visualizations", "classical")
  expect_true(dir.exists(viz_dir))

  # Clean up
  unlink(file.path(temp_dir, "visualizations"), recursive = TRUE)
})

test_that("External wrapper integration works correctly", {
  # Skip test if external wrapper functions are not available
  skip_if_not(exists("run_external_wrapper_tests"), "External wrapper functions not available")

  # This tests that the wrapper functions are properly integrated
  suite <- create_test_suite(
    prng_func = function(n) runif(n),
    config = list(
      external_sample_size = 1000,
      significance_level = 0.05,
      save_visualizations = FALSE
    )
  )

  # Check that run_external_wrapper_tests function exists
  expect_true(exists("run_external_wrapper_tests"))

  # Run external tests (which should use wrappers)
  expect_no_error(suite <- run_external_tests(suite))

  # Verify results structure
  if (!is.null(suite$results$external_wrappers)) {
    # Check summary exists
    expect_true("summary" %in% names(suite$results$external_wrappers))

    summary <- suite$results$external_wrappers$summary
    expect_true("total_tests" %in% names(summary))
    expect_true("passed" %in% names(summary))
    expect_true("failed" %in% names(summary))
    expect_true("errors" %in% names(summary))
  }
})
