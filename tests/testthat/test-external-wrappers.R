# Test suite for external wrapper functions
# Tests the integration of CryptRndTest and randtests packages

test_that("external wrapper functions handle package availability correctly", {
  # Source the wrapper functions
  source_path <- system.file("R/statisticaltests/external_wrappers.R", package = "qiprng")
  if (source_path == "") {
    source_path <- "../../R/statisticaltests/external_wrappers.R"
  }
  if (file.exists(source_path)) {
    source(source_path)
  }

  # Test error handling when packages are not available
  if (!requireNamespace("CryptRndTest", quietly = TRUE)) {
    expect_error(
      run_cryptrndtest_suite(runif(100)),
      "CryptRndTest package is not installed"
    )
  }

  if (!requireNamespace("randtests", quietly = TRUE)) {
    expect_error(
      run_randtests_suite(runif(100)),
      "randtests package is not installed"
    )
  }
})

test_that("CryptRndTest wrapper functions work correctly", {
  skip_if_not(
    requireNamespace("CryptRndTest", quietly = TRUE),
    "CryptRndTest package not available"
  )

  # Source the wrapper functions
  source_path <- system.file("R/statisticaltests/external_wrappers.R", package = "qiprng")
  if (source_path == "") {
    source_path <- "../../R/statisticaltests/external_wrappers.R"
  }
  if (file.exists(source_path)) {
    source(source_path)
  }

  # Test with random data
  set.seed(42)
  x <- runif(1000)

  # Run all tests
  results <- run_cryptrndtest_suite(x, test_type = "all")

  # Check structure
  expect_type(results, "list")

  # Check individual tests if they ran
  if ("adaptive_chi_square" %in% names(results)) {
    expect_true("p_value" %in% names(results$adaptive_chi_square) ||
      "error" %in% names(results$adaptive_chi_square))
    expect_true("result" %in% names(results$adaptive_chi_square))
  }

  # Test with specific tests
  results_specific <- run_cryptrndtest_suite(x, test_type = c("birthday_spacings", "gcd"))
  expect_true(all(names(results_specific) %in% c("birthday_spacings", "gcd")))
})

test_that("randtests wrapper functions work correctly", {
  skip_if_not(
    requireNamespace("randtests", quietly = TRUE),
    "randtests package not available"
  )

  # Source the wrapper functions
  source_path <- system.file("R/statisticaltests/external_wrappers.R", package = "qiprng")
  if (source_path == "") {
    source_path <- "../../R/statisticaltests/external_wrappers.R"
  }
  if (file.exists(source_path)) {
    source(source_path)
  }

  # Test with random data
  set.seed(42)
  x <- runif(1000)

  # Run all tests
  results <- run_randtests_suite(x, test_type = "all")

  # Check structure
  expect_type(results, "list")

  # Check that tests have expected fields
  for (test_name in names(results)) {
    expect_true("p_value" %in% names(results[[test_name]]) ||
      "error" %in% names(results[[test_name]]))
    expect_true("result" %in% names(results[[test_name]]))
    expect_true("description" %in% names(results[[test_name]]))
  }

  # Test runs test specifically
  results_runs <- run_randtests_suite(x, test_type = "runs")
  expect_equal(names(results_runs), "runs")
})

test_that("external wrapper integration function works correctly", {
  skip_if_not(
    requireNamespace("CryptRndTest", quietly = TRUE) ||
      requireNamespace("randtests", quietly = TRUE),
    "No external packages available"
  )

  # Source necessary files
  source_path <- system.file("R/statisticaltests/external_wrappers.R", package = "qiprng")
  if (source_path == "") {
    source_path <- "../../R/statisticaltests/external_wrappers.R"
  }
  if (file.exists(source_path)) {
    source(source_path)
  }

  # Create a mock test suite
  suite <- list(
    config = list(
      external_sample_size = 1000,
      significance_level = 0.05
    ),
    prng_func = function(n) runif(n),
    results = list()
  )

  # Run external wrapper tests
  suite_with_results <- run_external_wrapper_tests(
    suite,
    include_cryptrndtest = requireNamespace("CryptRndTest", quietly = TRUE),
    include_randtests = requireNamespace("randtests", quietly = TRUE)
  )

  # Check that results were added
  expect_true("external_wrappers" %in% names(suite_with_results$results))
  expect_true("summary" %in% names(suite_with_results$results$external_wrappers))

  # Check summary structure
  summary <- suite_with_results$results$external_wrappers$summary
  expect_true(all(c("total_tests", "passed", "failed", "errors", "pass_rate") %in% names(summary)))
  expect_true(summary$total_tests >= 0)
  expect_true(summary$pass_rate >= 0 && summary$pass_rate <= 1)
})

test_that("external wrappers handle edge cases correctly", {
  # Source the wrapper functions
  source_path <- system.file("R/statisticaltests/external_wrappers.R", package = "qiprng")
  if (source_path == "") {
    source_path <- "../../R/statisticaltests/external_wrappers.R"
  }
  if (file.exists(source_path)) {
    source(source_path)
  }

  # Test with very small sample
  if (requireNamespace("randtests", quietly = TRUE)) {
    x_small <- runif(10)
    results_small <- run_randtests_suite(x_small, test_type = "runs")
    expect_type(results_small, "list")
  }

  # Test with constant data
  if (requireNamespace("CryptRndTest", quietly = TRUE)) {
    x_const <- rep(0.5, 100)
    results_const <- run_cryptrndtest_suite(x_const, test_type = "adaptive_chi_square")
    expect_type(results_const, "list")
  }

  # Test with different alpha levels
  if (requireNamespace("randtests", quietly = TRUE)) {
    x <- runif(100)
    results_alpha <- run_randtests_suite(x, alpha = 0.01)
    expect_type(results_alpha, "list")
  }
})

test_that("external wrappers produce consistent results", {
  skip_if_not(
    requireNamespace("CryptRndTest", quietly = TRUE) ||
      requireNamespace("randtests", quietly = TRUE),
    "No external packages available"
  )

  # Source the wrapper functions
  source_path <- system.file("R/statisticaltests/external_wrappers.R", package = "qiprng")
  if (source_path == "") {
    source_path <- "../../R/statisticaltests/external_wrappers.R"
  }
  if (file.exists(source_path)) {
    source(source_path)
  }

  # Generate deterministic data
  set.seed(12345)
  x <- runif(500)

  # Run tests twice with same data
  if (requireNamespace("randtests", quietly = TRUE)) {
    results1 <- run_randtests_suite(x, test_type = "runs")
    results2 <- run_randtests_suite(x, test_type = "runs")

    # Should get same p-value
    if ("runs" %in% names(results1) && "runs" %in% names(results2)) {
      if (!is.null(results1$runs$p_value) && !is.null(results2$runs$p_value)) {
        expect_equal(results1$runs$p_value, results2$runs$p_value)
      }
    }
  }
})
