# Advanced Statistical Tests for PRNG Quality Assessment
# Additional sophisticated tests for randomness evaluation

# Load entropy package with installation if needed
suppressPackageStartupMessages({
  # Set CRAN mirror if not already set
  if (length(getOption("repos")) == 0 || getOption("repos")["CRAN"] == "@CRAN@") {
    options(repos = c(CRAN = "https://cran.rstudio.com/"))
  }
  
  if (!require(entropy, quietly = TRUE)) {
    cat("Installing entropy package...\n")
    install.packages("entropy", quiet = TRUE)
    library(entropy)
  }
})

#' Shannon entropy test
#'
#' @param samples Vector of random samples
#' @param bins Number of bins for discretization
#' @return List with entropy test results
test_entropy <- function(samples, bins = 256) {
  # Discretize samples into bins
  discretized <- cut(samples, breaks = bins, labels = FALSE)
  
  # Calculate Shannon entropy
  shannon_ent <- entropy(table(discretized), unit = "log2")
  
  # Maximum possible entropy for uniform distribution
  max_entropy <- log2(bins)
  
  # Normalized entropy (0 to 1)
  normalized_entropy <- shannon_ent / max_entropy
  
  # Good randomness should have high entropy (close to 1)
  passed <- normalized_entropy > 0.95
  
  return(list(
    test_name = "Shannon Entropy Test",
    shannon_entropy = shannon_ent,
    max_entropy = max_entropy,
    normalized_entropy = normalized_entropy,
    passed = passed,
    interpretation = ifelse(passed,
                           paste("High entropy detected (", round(normalized_entropy, 4), ") - good randomness"),
                           paste("Low entropy detected (", round(normalized_entropy, 4), ") - poor randomness"))
  ))
}

#' Gap test for randomness
#'
#' @param samples Vector of random samples
#' @param alpha Lower bound for gap
#' @param beta Upper bound for gap
#' @return List with gap test results
test_gaps <- function(samples, alpha = 0.3, beta = 0.7) {
  n <- length(samples)
  
  # Find gaps between alpha and beta
  in_range <- samples >= alpha & samples <= beta
  gaps <- diff(which(in_range))
  
  if (length(gaps) < 10) {
    return(list(
      test_name = "Gap Test",
      passed = FALSE,
      p_value = NA,
      interpretation = "Insufficient gaps for reliable test"
    ))
  }
  
  # Expected gap length for uniform distribution
  p <- beta - alpha
  expected_gap <- 1/p
  
  # Chi-square test on gap lengths
  max_gap <- min(20, max(gaps))
  gap_counts <- table(factor(pmin(gaps, max_gap), levels = 1:max_gap))
  
  # Expected frequencies based on geometric distribution
  expected_freq <- length(gaps) * (1-p)^(1:max_gap - 1) * p
  expected_freq[max_gap] <- length(gaps) * (1-p)^(max_gap - 1)  # Last bin includes all larger gaps
  
  # Perform chi-square test
  chi_result <- tryCatch({
    chisq.test(gap_counts, p = expected_freq/sum(expected_freq))
  }, error = function(e) list(p.value = NA, statistic = NA))
  
  passed <- !is.na(chi_result$p.value) && chi_result$p.value > 0.05
  
  return(list(
    test_name = "Gap Test",
    statistic = chi_result$statistic,
    p_value = chi_result$p.value,
    passed = passed,
    gaps_analyzed = length(gaps),
    interpretation = ifelse(passed,
                           "Gap distribution consistent with randomness",
                           "Gap distribution suggests non-randomness")
  ))
}

#' Serial correlation test at multiple lags
#'
#' @param samples Vector of random samples
#' @param max_lag Maximum lag to test
#' @return List with serial correlation test results
test_serial_correlation <- function(samples, max_lag = 50) {
  n <- length(samples)
  correlations <- numeric(max_lag)
  p_values <- numeric(max_lag)
  
  for (lag in 1:max_lag) {
    if (lag < n - 1) {
      # Calculate correlation at this lag
      x1 <- samples[1:(n-lag)]
      x2 <- samples[(lag+1):n]
      
      cor_result <- tryCatch({
        cor.test(x1, x2)
      }, error = function(e) list(estimate = NA, p.value = NA))
      
      correlations[lag] <- cor_result$estimate
      p_values[lag] <- cor_result$p.value
    } else {
      correlations[lag] <- NA
      p_values[lag] <- NA
    }
  }
  
  # Count significant correlations
  significant_lags <- which(p_values < 0.05 & !is.na(p_values))
  
  # Test passes if less than 5% of lags show significant correlation
  expected_significant <- max_lag * 0.05
  passed <- length(significant_lags) <= expected_significant
  
  return(list(
    test_name = "Serial Correlation Test",
    correlations = correlations,
    p_values = p_values,
    significant_lags = significant_lags,
    num_significant = length(significant_lags),
    expected_significant = expected_significant,
    passed = passed,
    interpretation = ifelse(passed,
                           paste("Serial correlation within expected range (", length(significant_lags), "/", max_lag, " significant)"),
                           paste("Excessive serial correlation detected (", length(significant_lags), "/", max_lag, " significant)"))
  ))
}

#' Poker test for randomness
#'
#' @param samples Vector of random samples
#' @param m Length of each hand (default 5)
#' @return List with poker test results
test_poker <- function(samples, m = 5) {
  n <- length(samples)
  
  # Convert to digits (0-9)
  digits <- floor(samples * 10)
  digits[digits == 10] <- 9  # Handle edge case
  
  # Number of complete hands
  num_hands <- floor(n / m)
  
  if (num_hands < 100) {
    return(list(
      test_name = "Poker Test",
      passed = FALSE,
      p_value = NA,
      interpretation = "Insufficient data for poker test"
    ))
  }
  
  # Count patterns in each hand
  patterns <- numeric(num_hands)
  
  for (i in 1:num_hands) {
    start_idx <- (i-1) * m + 1
    end_idx <- i * m
    hand <- digits[start_idx:end_idx]
    
    # Count unique digits in hand
    unique_count <- length(unique(hand))
    patterns[i] <- unique_count
  }
  
  # Expected frequencies for different patterns (for m=5)
  if (m == 5) {
    # Probabilities for 1,2,3,4,5 distinct digits
    expected_probs <- c(
      0.0001,      # All same (5 of a kind)
      0.0045,      # 2 distinct
      0.0270,      # 3 distinct  
      0.1080,      # 4 distinct
      0.3024       # All different
    )
  } else {
    # Simplified expected probabilities for other hand sizes
    expected_probs <- rep(1/m, m)
  }
  
  # Observed frequencies
  observed <- table(factor(patterns, levels = 1:m))
  expected <- num_hands * expected_probs
  
  # Chi-square test
  chi_result <- tryCatch({
    chisq.test(observed, p = expected_probs)
  }, error = function(e) list(p.value = NA, statistic = NA))
  
  passed <- !is.na(chi_result$p.value) && chi_result$p.value > 0.05
  
  return(list(
    test_name = "Poker Test",
    statistic = chi_result$statistic,
    p_value = chi_result$p.value,
    passed = passed,
    hands_analyzed = num_hands,
    interpretation = ifelse(passed,
                           "Digit patterns consistent with randomness",
                           "Digit patterns suggest non-randomness")
  ))
}

#' Comprehensive advanced test suite
#'
#' @param samples Vector of random samples
#' @return List with all advanced test results
run_advanced_tests <- function(samples) {
  results <- list()
  
  # Run all advanced tests with error handling
  results$entropy <- tryCatch(test_entropy(samples), 
                             error = function(e) list(test_name = "Shannon Entropy Test", 
                                                     passed = FALSE, 
                                                     interpretation = paste("Error:", e$message)))
  
  results$gaps <- tryCatch(test_gaps(samples), 
                          error = function(e) list(test_name = "Gap Test", 
                                                  passed = FALSE, 
                                                  interpretation = paste("Error:", e$message)))
  
  results$serial_correlation <- tryCatch(test_serial_correlation(samples), 
                                        error = function(e) list(test_name = "Serial Correlation Test", 
                                                                passed = FALSE, 
                                                                interpretation = paste("Error:", e$message)))
  
  results$poker <- tryCatch(test_poker(samples), 
                           error = function(e) list(test_name = "Poker Test", 
                                                   passed = FALSE, 
                                                   interpretation = paste("Error:", e$message)))
  
  return(results)
}
