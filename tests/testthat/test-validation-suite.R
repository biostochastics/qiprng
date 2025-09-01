# Test file for validation_suite.R
# Tests the comprehensive validation framework

test_that("validation suite can be loaded and initialized", {
  # Source the validation suite
  source("../../R/validation_suite.R")

  # Test that main function exists
  expect_true(exists("validate_qiprng_framework"))

  # Test configuration creation
  config <- get_validation_config("quick")
  expect_type(config, "list")
  expect_equal(config$sample_sizes, c(100, 1000))
  expect_equal(config$num_iterations, 10)
})

test_that("validation framework runs with quick level", {
  skip_if_not(file.exists("../../R/validation_suite.R"))

  source("../../R/validation_suite.R")

  # Run quick validation
  suppressWarnings({
    report <- validate_qiprng_framework(level = "quick", verbose = FALSE)
  })

  # Check report structure
  expect_s3_class(report, "qiprng_validation_report")
  expect_type(report$summary, "list")
  expect_true(all(c("total_tests", "passed", "failed", "warnings") %in% names(report$summary)))
})

test_that("test structure validation works correctly", {
  source("../../R/validation_suite.R")

  # Valid test structure
  valid_test <- list(
    description = "Test description",
    result = "PASS",
    p_value = 0.5,
    statistic = 1.234,
    details = "Test details"
  )
  expect_true(validate_test_structure(valid_test))

  # Invalid structures
  expect_false(validate_test_structure(list())) # Empty list
  expect_false(validate_test_structure(list(description = "Test"))) # Missing fields
  expect_false(validate_test_structure(list(
    description = 123, # Wrong type
    result = "PASS",
    p_value = 0.5,
    statistic = 1.234,
    details = "Test details"
  )))

  # Invalid result value
  invalid_result <- valid_test
  invalid_result$result <- "MAYBE"
  expect_false(validate_test_structure(invalid_result))

  # Out of range p-value
  invalid_pvalue <- valid_test
  invalid_pvalue$p_value <- 1.5
  expect_false(validate_test_structure(invalid_pvalue))

  # NA values should be allowed
  na_test <- valid_test
  na_test$p_value <- NA
  na_test$statistic <- NA
  expect_true(validate_test_structure(na_test))
})

test_that("edge case validation handles small samples correctly", {
  source("../../R/validation_suite.R")

  # Create minimal config
  config <- list(
    sample_sizes = c(10),
    num_iterations = 1,
    test_timeout = 60,
    significance_level = 0.05,
    save_visualizations = FALSE,
    output_dir = tempdir()
  )

  # Run edge case validation
  suppressWarnings({
    results <- validate_edge_cases(config, verbose = FALSE)
  })

  expect_type(results, "list")
  expect_true(all(c("total_tests", "passed", "failed") %in% names(results)))
})

test_that("performance validation measures execution time", {
  source("../../R/validation_suite.R")

  # Create config for performance testing
  config <- list(
    sample_sizes = c(1000, 10000),
    significance_level = 0.05,
    save_visualizations = FALSE
  )

  # Run performance validation
  suppressWarnings({
    results <- validate_performance(config, verbose = FALSE)
  })

  expect_type(results, "list")
  expect_true("timings" %in% names(results))
  expect_true(all(sapply(results$timings, is.numeric)))
})

test_that("recommendation generation works correctly", {
  source("../../R/validation_suite.R")

  # Create mock report with failures
  report <- list(
    summary = list(
      total_tests = 100,
      passed = 80,
      failed = 20
    ),
    categories = list(
      basic = list(failed = 5),
      classical = list(failed = 10)
    ),
    edge_cases = list(failed = 3),
    performance_tests = list(
      timings = list(slow_test = 10, fast_test = 0.1)
    )
  )

  recommendations <- generate_recommendations(report)
  expect_type(recommendations, "character")
  expect_true(length(recommendations) > 0)

  # Test with perfect report
  perfect_report <- list(
    summary = list(
      total_tests = 100,
      passed = 100,
      failed = 0
    ),
    categories = list()
  )

  perfect_recs <- generate_recommendations(perfect_report)
  expect_true(grepl("No issues found", perfect_recs[1]))
})

test_that("modifyList helper function works correctly", {
  source("../../R/validation_suite.R")

  base <- list(a = 1, b = 2, c = 3)
  mods <- list(b = 20, d = 4)

  result <- modifyList(base, mods)
  expect_equal(result$a, 1)
  expect_equal(result$b, 20)
  expect_equal(result$c, 3)
  expect_equal(result$d, 4)
})

test_that("validation summary printing works", {
  source("../../R/validation_suite.R")

  # Create mock report
  report <- list(
    level = "quick",
    performance = list(
      execution_time = 5.2
    ),
    summary = list(
      total_tests = 50,
      passed = 45,
      failed = 5,
      warnings = 0
    ),
    recommendations = c("Test recommendation 1", "Test recommendation 2")
  )

  # Capture output
  output <- capture.output(print_validation_summary(report))

  expect_true(any(grepl("QIPRNG Validation Report Summary", output)))
  expect_true(any(grepl("Level: quick", output)))
  expect_true(any(grepl("Total tests: 50", output)))
  expect_true(any(grepl("Passed: 45", output)))
})

test_that("validator functions handle missing test files gracefully", {
  source("../../R/validation_suite.R")

  config <- list(
    sample_sizes = c(100),
    significance_level = 0.05,
    test_timeout = 60,
    save_visualizations = FALSE,
    output_dir = tempdir()
  )

  # Test runs validator when function doesn't exist
  suppressWarnings({
    results <- validate_runs_tests(config, verbose = FALSE)
  })

  expect_equal(results$warnings, 1)
  expect_true("missing" %in% names(results$details))

  # Test correlation validator
  suppressWarnings({
    results <- validate_correlation_tests(config, verbose = FALSE)
  })

  expect_equal(results$warnings, 1)
  expect_true("missing" %in% names(results$details))
})

test_that("timeout functionality works", {
  source("../../R/validation_suite.R")

  # Test successful execution
  result <- withTimeout(
    {
      Sys.sleep(0.1)
      "success"
    },
    timeout = 1
  )

  expect_equal(result, "success")

  # Test timeout (this might be system-dependent)
  expect_error({
    withTimeout(
      {
        Sys.sleep(2)
        "should not reach here"
      },
      timeout = 0.5
    )
  })
})
