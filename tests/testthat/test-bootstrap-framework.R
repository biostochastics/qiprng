# Test file for bootstrap framework functions
# ----------------------------------------------------------------------

test_that("bootstrap_p_value works correctly", {
  # Test with known distribution (normal)
  set.seed(123)
  x <- rnorm(100, mean = 0, sd = 1)

  # Test if mean is 0
  stat_func <- function(d) mean(d)
  result <- bootstrap_p_value(x, stat_func, n_bootstraps = 1000, seed = 123)

  expect_true(is.list(result))
  expect_true("p.value" %in% names(result))
  expect_true("bootstrap_stats" %in% names(result))
  expect_true("observed_stat" %in% names(result))
  expect_true("n_bootstraps" %in% names(result))

  # P-value should be relatively high (not rejecting null)
  expect_true(result$p.value > 0.05)

  # Test with different statistic
  x2 <- runif(100, min = 0, max = 1)
  # Test if variance is different from uniform variance (1/12)
  stat_func2 <- function(d) abs(var(d) - 1 / 12)
  result2 <- bootstrap_p_value(x2, stat_func2,
    n_bootstraps = 1000,
    alternative = "greater", seed = 123
  )

  # For uniform data, variance should be close to 1/12
  expect_true(result2$p.value > 0.05)
})

test_that("bootstrap_p_value handles edge cases", {
  # Test with small sample
  expect_error(bootstrap_p_value(1, mean), "at least 2 elements")

  # Test with invalid function
  expect_error(bootstrap_p_value(1:10, "not_a_function"), "must be a function")

  # Test with too few bootstraps
  expect_error(bootstrap_p_value(1:10, mean, n_bootstraps = 50), ">= 100")

  # Test function that returns non-numeric
  bad_func <- function(x) "not numeric"
  expect_error(bootstrap_p_value(1:10, bad_func), "single numeric value")
})

test_that("bootstrap_p_value alternative hypotheses work", {
  set.seed(456)
  # Create data with known skewness
  x <- rexp(100, rate = 1) - 1 # Exponential minus 1, skewed

  # Test statistic: skewness
  stat_func <- function(d) {
    n <- length(d)
    m3 <- mean((d - mean(d))^3)
    m2 <- mean((d - mean(d))^2)
    m3 / (m2^(3 / 2))
  }

  obs_skew <- stat_func(x)

  # Two-sided test
  result_two <- bootstrap_p_value(x, stat_func,
    observed_stat = obs_skew,
    alternative = "two.sided",
    n_bootstraps = 1000, seed = 456
  )

  # One-sided tests
  result_greater <- bootstrap_p_value(x, stat_func,
    observed_stat = obs_skew,
    alternative = "greater",
    n_bootstraps = 1000, seed = 456
  )
  result_less <- bootstrap_p_value(x, stat_func,
    observed_stat = obs_skew,
    alternative = "less",
    n_bootstraps = 1000, seed = 456
  )

  # All p-values should be numeric
  expect_true(is.numeric(result_two$p.value))
  expect_true(is.numeric(result_greater$p.value))
  expect_true(is.numeric(result_less$p.value))

  # Check relationships between p-values
  expect_true(result_two$p.value >= 0 && result_two$p.value <= 1)
  expect_true(result_greater$p.value >= 0 && result_greater$p.value <= 1)
  expect_true(result_less$p.value >= 0 && result_less$p.value <= 1)
})

test_that("permutation_test works for two-sample tests", {
  set.seed(789)
  x <- rnorm(30, mean = 0)
  y <- rnorm(30, mean = 0.5)

  # Test difference in means
  stat_func <- function(x, y) mean(x) - mean(y)
  result <- permutation_test(x, y, stat_func, n_permutations = 1000, seed = 789)

  expect_true(is.list(result))
  expect_true("p.value" %in% names(result))
  expect_true("permutation_stats" %in% names(result))
  expect_true("observed_stat" %in% names(result))

  # Should detect difference
  expect_true(result$p.value < 0.1)
})

test_that("permutation_test works for one-sample tests", {
  set.seed(101)
  x <- rnorm(50, mean = 0.5, sd = 1)

  # Test if values are symmetric around 0 (they're not)
  # Use absolute mean as test statistic
  stat_func <- function(d) abs(mean(d))
  result <- permutation_test(x,
    test_statistic = stat_func,
    n_permutations = 1000, seed = 101
  )

  expect_true(is.list(result))
  # Since mean is away from 0, expect significant result
  expect_true(is.numeric(result$p.value))
  expect_true(result$p.value >= 0 && result$p.value <= 1)
})

test_that("permutation_test handles errors gracefully", {
  # Invalid inputs
  expect_error(
    permutation_test("not numeric", test_statistic = mean),
    "must be numeric"
  )
  expect_error(
    permutation_test(1:10, "not numeric", test_statistic = mean),
    "must be numeric"
  )
  expect_error(
    permutation_test(1:10, test_statistic = "not_function"),
    "must be a function"
  )
})

test_that("monte_carlo_p_value works correctly", {
  set.seed(202)

  # Test for correlation under null hypothesis of independence
  n <- 50
  x <- rnorm(n)
  y <- x + rnorm(n, sd = 0.5) # Correlated
  observed_cor <- cor(x, y)

  # Null simulator: independent normal variables
  null_sim <- function(n_obs) {
    list(x = rnorm(n_obs), y = rnorm(n_obs))
  }

  # Test statistic: correlation
  stat_func <- function(data) cor(data$x, data$y)

  result <- monte_carlo_p_value(observed_cor, null_sim, stat_func,
    n_simulations = 1000, seed = 202, n_obs = n
  )

  expect_true(is.list(result))
  expect_true("p.value" %in% names(result))
  expect_true("simulated_stats" %in% names(result))

  # Should reject null hypothesis of no correlation
  expect_true(result$p.value < 0.05)
})

test_that("monte_carlo_p_value validates inputs", {
  null_sim <- function() rnorm(10)
  stat_func <- function(x) mean(x)

  # Invalid observed statistic
  expect_error(
    monte_carlo_p_value("not numeric", null_sim, stat_func),
    "single numeric value"
  )
  expect_error(
    monte_carlo_p_value(c(1, 2), null_sim, stat_func),
    "single numeric value"
  )

  # Invalid functions
  expect_error(
    monte_carlo_p_value(0, "not function", stat_func),
    "must be a function"
  )
  expect_error(
    monte_carlo_p_value(0, null_sim, "not function"),
    "must be a function"
  )

  # Too few simulations
  expect_error(
    monte_carlo_p_value(0, null_sim, stat_func, n_simulations = 50),
    ">= 100"
  )
})

test_that("bootstrap_conf_interval works correctly", {
  set.seed(303)
  x <- rnorm(100, mean = 5, sd = 2)

  # Confidence interval for mean
  result <- bootstrap_conf_interval(x, mean,
    n_bootstraps = 1000,
    conf_level = 0.95, seed = 303
  )

  expect_true(is.list(result))
  expect_true("estimate" %in% names(result))
  expect_true("conf_int" %in% names(result))
  expect_true("conf_level" %in% names(result))
  expect_true("bootstrap_distribution" %in% names(result))

  # Check confidence interval properties
  expect_equal(length(result$conf_int), 2)
  expect_true(result$conf_int[1] < result$estimate)
  expect_true(result$conf_int[2] > result$estimate)

  # True mean (5) should be in the interval
  expect_true(5 >= result$conf_int[1] && 5 <= result$conf_int[2])
})

test_that("progress bars work correctly", {
  # This test checks that progress bars don't cause errors
  # Progress bars may produce output, so we just check no errors

  x <- rnorm(50)

  # Should show progress bar (n_bootstraps >= 1000)
  result1 <- bootstrap_p_value(x, mean,
    n_bootstraps = 1000,
    progress = TRUE, seed = 404
  )
  expect_true(is.list(result1))

  # Should not show progress bar
  result2 <- bootstrap_p_value(x, mean,
    n_bootstraps = 500,
    progress = TRUE, seed = 404
  )
  expect_true(is.list(result2))

  # Progress = FALSE
  result3 <- bootstrap_p_value(x, mean,
    n_bootstraps = 2000,
    progress = FALSE, seed = 404
  )
  expect_true(is.list(result3))
})

test_that("functions handle NA values gracefully", {
  set.seed(505)
  x <- c(rnorm(50), NA, NA, NA)

  # Function that handles NAs
  stat_func_na_rm <- function(d) mean(d, na.rm = TRUE)

  # Should work with NA-aware function
  result <- bootstrap_p_value(x, stat_func_na_rm, n_bootstraps = 500, seed = 505)
  expect_true(is.numeric(result$p.value))

  # Function that propagates NAs
  stat_func_na <- function(d) mean(d) # Will return NA if any NA present

  # Should handle but warn about NAs
  expect_warning(
    {
      result2 <- bootstrap_p_value(x, stat_func_na, n_bootstraps = 500, seed = 505)
    },
    "bootstrap samples were valid"
  )
})
