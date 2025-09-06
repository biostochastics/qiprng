# Test suite for numerical stability edge cases
# Addresses external review concern about edge case coverage

library(qiprng)
library(testthat)

context("Numerical Stability Edge Cases")

test_that("Quadratic irrational handles extreme discriminants", {
  # Test near-overflow discriminants
  expect_error(
    qiprng_engine(abc_list = list(c(1e9, 1e9, -1e9))),
    "discriminant.*overflow"
  )

  # Test very small discriminants
  expect_no_error({
    eng <- qiprng_engine(abc_list = list(c(1, 1, 0)))
    vals <- eng$generate(100)
    expect_true(all(vals >= 0 & vals < 1))
  })

  # Test non-square-free discriminants (should error with new exception handling)
  expect_error(
    qiprng_engine(abc_list = list(c(1, 0, -4))), # discriminant = 16 = 4^2
    "not square-free"
  )
})

test_that("CFE period computation handles edge cases", {
  # Test QI with very long period
  eng <- qiprng_engine(abc_list = list(c(1, 1, -100001)))
  expect_no_error(eng$generate(10))

  # Test QI with short period
  eng <- qiprng_engine(abc_list = list(c(1, 1, -2)))
  expect_no_error(eng$generate(10))
})

test_that("Distribution generation maintains precision", {
  eng <- qiprng_engine(
    abc_list = list(c(2, 3, -5)),
    config = list(
      mpfr_precision = 256,
      distribution = "normal",
      normal_mean = 1e-10, # Very small mean
      normal_sd = 1e-12 # Very small SD
    )
  )

  vals <- eng$generate(1000)

  # Check that we maintain precision for small values
  expect_true(sd(vals) > 0) # Not all identical
  expect_true(abs(mean(vals) - 1e-10) < 1e-9) # Close to expected mean
})

test_that("Thread safety under concurrent access", {
  skip_if_not(parallel::detectCores() > 1, "Single core system")

  library(parallel)

  # Create shared engine
  eng <- qiprng_engine(
    abc_list = list(c(2, 3, -5), c(3, 7, -11)),
    config = list(use_threading = TRUE)
  )

  # Parallel generation should not crash or produce invalid values
  cl <- makeCluster(4)
  clusterExport(cl, "eng")

  results <- parLapply(cl, 1:100, function(i) {
    vals <- eng$generate(100)
    c(
      all_valid = all(vals >= 0 & vals < 1),
      has_variance = var(vals) > 0
    )
  })

  stopCluster(cl)

  # All results should be valid
  expect_true(all(sapply(results, function(r) r["all_valid"])))
  expect_true(all(sapply(results, function(r) r["has_variance"])))
})

test_that("Precision loss is tracked and mitigated", {
  # High precision engine
  eng <- qiprng_engine(
    abc_list = list(c(2, 3, -5)),
    config = list(mpfr_precision = 512)
  )

  # Generate many values to accumulate precision loss
  vals <- eng$generate(10000)

  # Check precision metrics if available
  if (!is.null(eng$get_precision_stats)) {
    stats <- eng$get_precision_stats()

    # Verify precision loss is tracked
    expect_true(stats$total_bits_lost >= 0)
    expect_true(stats$conversion_count > 0)

    # Average precision loss should be reasonable
    avg_loss <- stats$total_bits_lost / stats$conversion_count
    expect_true(avg_loss < 100) # Less than 100 bits lost on average
  }
})

test_that("Crypto mixer rejects deterministic seeds", {
  # This should now throw an error, not just warn
  expect_error(
    qiprng_engine(
      abc_list = list(c(2, 3, -5)),
      config = list(
        use_crypto_mixing = TRUE,
        seed = 12345 # Deterministic seed
      )
    ),
    "SECURITY ERROR.*deterministic"
  )
})

test_that("Matrix exponentiation handles large jumps", {
  eng <- qiprng_engine(abc_list = list(c(2, 3, -5)))

  # Jump ahead by large amount
  eng$jump_ahead(1e15)

  # Should still produce valid values
  vals <- eng$generate(100)
  expect_true(all(vals >= 0 & vals < 1))
  expect_true(var(vals) > 0)
})
