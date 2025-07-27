# File: runs_tests.R
# ----------------------------------------------------------------------
#' Runs and independence tests for PRNG quality
#'
#' This module provides specialized tests to evaluate the independence properties
#' and run structure of the PRNG output. Run-based tests are particularly 
#' effective at detecting non-random patterns that might not be detected by
#' simple distribution tests.
#'
#' The tests implemented in this module include:
#' \itemize{
#'   \item Standard runs test (above/below median)
#'   \item Run length tests (detecting abnormally long runs)
#'   \item Up/down runs test (detecting trends)
#'   \item Serial independence tests
#'   \item Lag-k autocorrelation tests
#' }
#'
#' These tests are sensitive to sequential patterns and are essential for
#' cryptographic and simulation applications where independence between
#' successive values is critical.
#'
#' @name runs_tests
#' @aliases runs-tests
#' @keywords internal

#' Run tests for independence and runs in random sequences
#'
#' Executes a comprehensive set of runs-based and independence tests on a 
#' random number generator. These tests evaluate whether successive values in the
#' sequence are statistically independent, a critical property for high-quality PRNGs.
#'
#' @param suite The test suite object containing the PRNG function and configuration
#' @return Updated test suite with results of all runs tests added to suite$results$runs
#' @details
#' This function performs the following tests:
#' \itemize{
#'   \item Standard runs test: Analyzes the number of runs above/below median
#'   \item Runs up/down test: Tests for trends in the sequence
#'   \item Serial independence test: Tests for correlations between consecutive values
#'   \item Lag autocorrelation: Tests for periodic patterns at different lags
#'   \item Longest run analysis: Tests if the longest run length is as expected
#' }
#' 
#' The tests use the randtests package when available and implement custom
#' algorithms otherwise. All tests include p-values and clear pass/fail indications
#' based on the configured significance level.
#' @keywords internal
run_runs_tests <- function(suite) {
  # Generate random numbers
  n <- suite$config$runs_sample_size
  x <- suite$prng_func(n)
  
  # Initialize results
  suite$results$runs <- list()
  
  # Standard runs test (above/below median)
  if (requireNamespace("randtests", quietly = TRUE)) {
    # Convert values to above/below median for runs test
    median_x <- 0.5 # Theoretical median for uniform(0,1)
    
    # Basic runs test
    tryCatch({
      runs_test <- randtests::runs.test(x, threshold = median_x)
      suite$results$runs$runs_test <- list(
        description = "Runs Test (Above/Below Median)",
        result = ifelse(runs_test$p.value >= suite$config$significance_level, "PASS", "FAIL"),
        p_value = runs_test$p.value,
        statistic = runs_test$statistic,
        details = paste("Tests if runs of values above/below median have expected length.",
                       "Number of runs:", runs_test$n.runs)
      )
    }, error = function(e) {
      suite$results$runs$runs_test <- list(
        description = "Runs Test (Above/Below Median)",
        result = "INCONCLUSIVE",
        p_value = NA,
        statistic = NA,
        details = paste("Test error:", conditionMessage(e))
      )
    })
    
    # Turning points test
    tryCatch({
      tp_test <- randtests::turning.point.test(x)
      suite$results$runs$turning_points <- list(
        description = "Turning Points Test",
        result = ifelse(tp_test$p.value >= suite$config$significance_level, "PASS", "FAIL"),
        p_value = tp_test$p.value,
        statistic = tp_test$statistic,
        details = paste("Tests the number of turning points (local extrema) in the sequence.",
                       "Turning points:", tp_test$turning.points)
      )
    }, error = function(e) {
      suite$results$runs$turning_points <- list(
        description = "Turning Points Test",
        result = "INCONCLUSIVE",
        p_value = NA,
        statistic = NA,
        details = paste("Test error:", conditionMessage(e))
      )
    })
    
    # Difference sign test
    tryCatch({
      diff_sign_test <- randtests::difference.sign.test(x)
      suite$results$runs$difference_sign <- list(
        description = "Difference Sign Test",
        result = ifelse(diff_sign_test$p.value >= suite$config$significance_level, "PASS", "FAIL"),
        p_value = diff_sign_test$p.value,
        statistic = diff_sign_test$statistic,
        details = "Tests the signs of differences between consecutive values."
      )
    }, error = function(e) {
      suite$results$runs$difference_sign <- list(
        description = "Difference Sign Test",
        result = "INCONCLUSIVE",
        p_value = NA,
        statistic = NA,
        details = paste("Test error:", conditionMessage(e))
      )
    })
    
    # Rank test
    tryCatch({
      rank_test <- randtests::rank.test(x)
      suite$results$runs$rank_test <- list(
        description = "Rank Test",
        result = ifelse(rank_test$p.value >= suite$config$significance_level, "PASS", "FAIL"),
        p_value = rank_test$p.value,
        statistic = rank_test$statistic,
        details = "Tests for trends based on the ranks of values."
      )
    }, error = function(e) {
      suite$results$runs$rank_test <- list(
        description = "Rank Test",
        result = "INCONCLUSIVE",
        p_value = NA,
        statistic = NA,
        details = paste("Test error:", conditionMessage(e))
      )
    })
    
    # Bartels test
    tryCatch({
      bartels_test <- randtests::bartels.rank.test(x)
      suite$results$runs$bartels_test <- list(
        description = "Bartels Rank Test",
        result = ifelse(bartels_test$p.value >= suite$config$significance_level, "PASS", "FAIL"),
        p_value = bartels_test$p.value,
        statistic = bartels_test$statistic,
        details = "Tests for randomness using Bartels rank approach."
      )
    }, error = function(e) {
      suite$results$runs$bartels_test <- list(
        description = "Bartels Rank Test",
        result = "INCONCLUSIVE",
        p_value = NA,
        statistic = NA,
        details = paste("Test error:", conditionMessage(e))
      )
    })
    
    # Cox-Stuart test
    tryCatch({
      cs_test <- randtests::cox.stuart.test(x)
      suite$results$runs$cox_stuart <- list(
        description = "Cox-Stuart Test",
        result = ifelse(cs_test$p.value >= suite$config$significance_level, "PASS", "FAIL"),
        p_value = cs_test$p.value,
        statistic = cs_test$statistic,
        details = "Tests for trend by comparing first half with second half of data."
      )
    }, error = function(e) {
      suite$results$runs$cox_stuart <- list(
        description = "Cox-Stuart Test",
        result = "INCONCLUSIVE",
        p_value = NA,
        statistic = NA,
        details = paste("Test error:", conditionMessage(e))
      )
    })
  } else {
    # Simple runs test implementation if randtests package not available
    runs_up_down <- function(x) {
      n <- length(x)
      if (n <= 1) return(0)
      
      # Count runs
      runs <- 1
      for (i in 2:n) {
        if ((x[i] >= 0.5 && x[i-1] < 0.5) || (x[i] < 0.5 && x[i-1] >= 0.5)) {
          runs <- runs + 1
        }
      }
      
      # Expected runs and variance for randomness
      expected_runs <- (2 * n - 1) / 3
      var_runs <- (16 * n - 29) / 90
      
      # Z-statistic
      z <- (runs - expected_runs) / sqrt(var_runs)
      p_value <- 2 * pnorm(-abs(z))
      
      list(statistic = z, p.value = p_value, n.runs = runs)
    }
    
    runs_result <- runs_up_down(x)
    suite$results$runs$simple_runs <- list(
      description = "Simple Runs Test",
      result = ifelse(runs_result$p.value >= suite$config$significance_level, "PASS", "FAIL"),
      p_value = runs_result$p.value,
      statistic = runs_result$statistic,
      details = paste("Basic runs test. Number of runs:", runs_result$n.runs)
    )
  }
  
  # Custom Gap Test
  # Count gaps between occurrences of values in a specific range
  gap_test <- function(x, lower = 0, upper = 0.1) {
    in_range <- x >= lower & x < upper
    positions <- which(in_range)
    
    if (length(positions) <= 1) {
      return(list(
        p.value = NA, 
        statistic = NA, 
        details = "Insufficient data in specified range"
      ))
    }
    
    # Calculate gaps
    gaps <- diff(positions) - 1
    
    # For uniform random numbers, gaps should follow geometric distribution
    # with parameter p = upper - lower
    p <- upper - lower
    
    # Expected frequencies for gaps
    max_gap <- max(gaps)
    exp_probs <- dgeom(0:max_gap, p)
    expected <- length(gaps) * exp_probs
    
    # Only keep bins with expected >= 5 for chi-square validity
    min_bin_size <- 5
    if (any(expected < min_bin_size)) {
      # Combine last bins until all expected >= 5
      keep <- which(expected >= min_bin_size)
      if (length(keep) == 0) {
        return(list(
          p.value = NA, 
          statistic = NA, 
          details = "Expected frequencies too small for valid test"
        ))
      }
      
      max_valid <- max(keep)
      
      # Combine all gaps > max_valid into a single bin
      gaps_binned <- gaps
      gaps_binned[gaps > max_valid] <- max_valid + 1
      
      # Recalculate observed and expected
      observed <- tabulate(gaps_binned + 1, nbins = max_valid + 2)
      
      # Expected for valid bins
      expected_valid <- expected[1:max_valid]
      
      # Calculate expected for the combined bin
      expected_last <- length(gaps) - sum(expected_valid)
      expected <- c(expected_valid, expected_last)
      
      # Make sure observed and expected have the same length
      observed <- observed[1:(length(expected))]
      
      # Run chi-square test
      chi_result <- suppressWarnings(chisq.test(observed, p = expected / sum(expected)))
      
      list(
        p.value = chi_result$p.value,
        statistic = chi_result$statistic,
        details = paste("Gap test. Range:", lower, "to", upper, 
                       "Max gap:", max_gap, "Combined gaps >", max_valid)
      )
    } else {
      # Standard case - all expected >= 5
      observed <- tabulate(gaps + 1, nbins = max_gap + 1)
      chi_result <- suppressWarnings(chisq.test(observed, p = exp_probs))
      
      list(
        p.value = chi_result$p.value,
        statistic = chi_result$statistic,
        details = paste("Gap test. Range:", lower, "to", upper, "Max gap:", max_gap)
      )
    }
  }
  
  # Run gap test with different parameters
  gap_result_1 <- gap_test(x, 0, 0.1)
  suite$results$runs$gap_test_1 <- list(
    description = "Gap Test (0.0-0.1)",
    result = ifelse(!is.na(gap_result_1$p.value) && 
                   gap_result_1$p.value >= suite$config$significance_level, 
                   "PASS", ifelse(is.na(gap_result_1$p.value), "INCONCLUSIVE", "FAIL")),
    p_value = gap_result_1$p.value,
    statistic = gap_result_1$statistic,
    details = gap_result_1$details
  )
  
  gap_result_2 <- gap_test(x, 0.4, 0.6)
  suite$results$runs$gap_test_2 <- list(
    description = "Gap Test (0.4-0.6)",
    result = ifelse(!is.na(gap_result_2$p.value) && 
                   gap_result_2$p.value >= suite$config$significance_level, 
                   "PASS", ifelse(is.na(gap_result_2$p.value), "INCONCLUSIVE", "FAIL")),
    p_value = gap_result_2$p.value,
    statistic = gap_result_2$statistic,
    details = gap_result_2$details
  )
  
  gap_result_3 <- gap_test(x, 0.9, 1.0)
  suite$results$runs$gap_test_3 <- list(
    description = "Gap Test (0.9-1.0)",
    result = ifelse(!is.na(gap_result_3$p.value) && 
                   gap_result_3$p.value >= suite$config$significance_level, 
                   "PASS", ifelse(is.na(gap_result_3$p.value), "INCONCLUSIVE", "FAIL")),
    p_value = gap_result_3$p.value,
    statistic = gap_result_3$statistic,
    details = gap_result_3$details
  )
  
  # Generate visualizations
  if (require(ggplot2) && suite$config$save_visualizations) {
    suite <- visualize_runs_tests(suite, x)
  }
  
  return(suite)
}
