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
#' Tests the information content of a random sequence using Shannon entropy.
#' High entropy indicates good randomness, while low entropy suggests patterns
#' or predictability in the data.
#'
#' @param samples Numeric vector of random samples to test
#' @param bins Number of bins for discretization (default: 256)
#' 
#' @return A list containing entropy test results:
#'   \describe{
#'     \item{test_name}{Name of the test}
#'     \item{shannon_entropy}{Calculated Shannon entropy in bits}
#'     \item{max_entropy}{Maximum possible entropy for given bin count}
#'     \item{normalized_entropy}{Ratio of observed to maximum entropy (0-1)}
#'     \item{passed}{Logical; TRUE if normalized entropy > 0.95}
#'     \item{interpretation}{Human-readable interpretation of results}
#'   }
#'   
#' @details
#' The Shannon entropy is calculated as:
#' \deqn{H = -\sum_{i=1}^{n} p_i \log_2(p_i)}
#' 
#' where p_i is the probability of bin i. The samples are discretized into
#' the specified number of bins, and the entropy is calculated from the
#' resulting frequency distribution.
#' 
#' For a perfectly uniform distribution, the entropy equals log2(bins).
#' The test passes if the normalized entropy (H/H_max) exceeds 0.95,
#' indicating high randomness.
#' 
#' @note The choice of bin count affects the test sensitivity. Too few bins
#' may miss fine-grained patterns, while too many bins may lead to sparse
#' data issues.
#' 
#' @examples
#' \dontrun{
#' # Test entropy of uniform random numbers
#' samples <- runif(10000)
#' result <- test_entropy(samples)
#' print(paste("Normalized entropy:", round(result$normalized_entropy, 4)))
#' 
#' # Test with fewer bins
#' result <- test_entropy(samples, bins = 128)
#' }
#' 
#' @importFrom entropy entropy
#' @export
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
#' Tests the distribution of gaps between occurrences of values in a specified
#' range. The test analyzes how far apart values within [alpha, beta] appear
#' in the sequence, which should follow a geometric distribution for truly
#' random data.
#'
#' @param samples Numeric vector of random samples to test
#' @param alpha Lower bound for the gap range (default: 0.3)
#' @param beta Upper bound for the gap range (default: 0.7)
#' 
#' @return A list containing gap test results:
#'   \describe{
#'     \item{test_name}{Name of the test}
#'     \item{statistic}{Chi-squared test statistic}
#'     \item{p_value}{P-value from chi-squared test}
#'     \item{passed}{Logical; TRUE if p-value > 0.05}
#'     \item{gaps_analyzed}{Number of gaps found and analyzed}
#'     \item{interpretation}{Human-readable interpretation of results}
#'   }
#'   
#' @details
#' The gap test examines the distances between consecutive occurrences of
#' values within the range [alpha, beta]. For a uniform random sequence,
#' these gaps should follow a geometric distribution with parameter p = beta - alpha.
#' 
#' The test procedure:
#' 1. Identifies all positions where samples fall within [alpha, beta]
#' 2. Calculates gaps between consecutive positions
#' 3. Compares observed gap distribution to expected geometric distribution
#' 4. Uses chi-squared test to assess goodness-of-fit
#' 
#' The expected frequency for gap length k is:
#' \deqn{E[k] = n \cdot (1-p)^{k-1} \cdot p}
#' 
#' where n is the number of gaps and p = beta - alpha.
#' 
#' @note The test requires at least 10 gaps for reliable results. If fewer
#' gaps are found, the test returns INCONCLUSIVE.
#' 
#' @examples
#' \dontrun{
#' # Test gaps in uniform random numbers
#' samples <- runif(10000)
#' result <- test_gaps(samples)
#' print(paste("Gap test p-value:", round(result$p_value, 4)))
#' 
#' # Test with different range
#' result <- test_gaps(samples, alpha = 0.2, beta = 0.8)
#' }
#' 
#' @export
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
#' Tests for serial correlation (autocorrelation) in a random sequence at
#' multiple lag values. This test detects dependencies between values
#' separated by various distances in the sequence.
#'
#' @param samples Numeric vector of random samples to test
#' @param max_lag Maximum lag to test (default: 50)
#' 
#' @return A list containing serial correlation test results:
#'   \describe{
#'     \item{test_name}{Name of the test}
#'     \item{correlations}{Vector of correlation coefficients for each lag}
#'     \item{p_values}{Vector of p-values for each lag}
#'     \item{significant_lags}{Vector of lag indices with significant correlation}
#'     \item{num_significant}{Count of lags with significant correlation}
#'     \item{expected_significant}{Expected number of significant lags by chance}
#'     \item{passed}{Logical; TRUE if num_significant <= expected_significant}
#'     \item{interpretation}{Human-readable interpretation of results}
#'   }
#'   
#' @details
#' Serial correlation measures the relationship between values in a sequence
#' and their lagged counterparts. For a truly random sequence, correlations
#' at all lags should be near zero.
#' 
#' For each lag k from 1 to max_lag, the test:
#' 1. Computes Pearson correlation between x[1:(n-k)] and x[(k+1):n]
#' 2. Tests if the correlation is significantly different from zero
#' 3. Records correlations and p-values
#' 
#' The test passes if the proportion of significant correlations (p < 0.05)
#' does not exceed 5%, which is the expected false positive rate.
#' 
#' High serial correlation indicates:
#' - Predictable patterns in the sequence
#' - Poor randomness quality
#' - Potential issues with the PRNG algorithm
#' 
#' @note Lags approaching the sample size may have unreliable results due
#' to reduced sample size for correlation calculation.
#' 
#' @examples
#' \dontrun{
#' # Test serial correlation in uniform random numbers
#' samples <- runif(10000)
#' result <- test_serial_correlation(samples)
#' print(paste("Significant correlations found:", result$num_significant))
#' 
#' # Test with more lags
#' result <- test_serial_correlation(samples, max_lag = 100)
#' 
#' # Plot correlation values
#' plot(result$correlations, type = "h", 
#'      main = "Serial Correlations by Lag",
#'      xlab = "Lag", ylab = "Correlation")
#' }
#' 
#' @export
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
#' Tests randomness by examining patterns in "hands" of digits, similar to
#' poker hands. The test converts continuous values to digits and checks if
#' the distribution of unique digits per hand matches theoretical expectations.
#'
#' @param samples Numeric vector of random samples to test (values in [0,1])
#' @param m Length of each hand in digits (default: 5)
#' 
#' @return A list containing poker test results:
#'   \describe{
#'     \item{test_name}{Name of the test}
#'     \item{statistic}{Chi-squared test statistic}
#'     \item{p_value}{P-value from chi-squared test}
#'     \item{passed}{Logical; TRUE if p-value > 0.05}
#'     \item{hands_analyzed}{Number of complete hands analyzed}
#'     \item{interpretation}{Human-readable interpretation of results}
#'   }
#'   
#' @details
#' The poker test is inspired by the card game and examines patterns in
#' groups of digits:
#' 
#' 1. Converts each sample to a digit (0-9) by multiplying by 10 and flooring
#' 2. Groups consecutive digits into "hands" of size m
#' 3. Counts unique digits in each hand
#' 4. Compares observed distribution to theoretical probabilities
#' 
#' For m=5 (default), the theoretical probabilities are:
#' - 1 unique digit (five of a kind): 0.0001
#' - 2 unique digits: 0.0045
#' - 3 unique digits: 0.0270
#' - 4 unique digits: 0.1080
#' - 5 unique digits (all different): 0.3024
#' 
#' These probabilities are based on the multinomial distribution for
#' sampling with replacement from 10 digits.
#' 
#' @note The test requires at least 100 hands for reliable results. For
#' hand sizes other than 5, simplified uniform probabilities are used.
#' 
#' @examples
#' \dontrun{
#' # Standard poker test with 5-digit hands
#' samples <- runif(10000)
#' result <- test_poker(samples)
#' print(paste("Poker test p-value:", round(result$p_value, 4)))
#' 
#' # Test with 3-digit hands
#' result <- test_poker(samples, m = 3)
#' }
#' 
#' @references
#' Knuth, D. E. (1997). The Art of Computer Programming, Volume 2:
#' Seminumerical Algorithms (3rd ed.). Addison-Wesley.
#' 
#' @export
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
#' Runs all advanced statistical tests on a set of random samples, including
#' entropy, gap, serial correlation, and poker tests. Each test evaluates
#' different aspects of randomness quality.
#'
#' @param samples Numeric vector of random samples to test (values in [0,1])
#' 
#' @return A list containing results from all advanced tests:
#'   \describe{
#'     \item{entropy}{Results from Shannon entropy test}
#'     \item{gaps}{Results from gap test}
#'     \item{serial_correlation}{Results from serial correlation test}
#'     \item{poker}{Results from poker test}
#'   }
#'   Each test result includes test_name, passed status, and interpretation.
#'   Failed tests include error messages in the interpretation field.
#'   
#' @details
#' This function serves as a comprehensive test battery for advanced
#' randomness evaluation. It runs the following tests:
#' 
#' \itemize{
#'   \item **Shannon Entropy Test**: Measures information content and
#'     unpredictability. High entropy indicates good randomness.
#'   \item **Gap Test**: Analyzes spacing between values in a range.
#'     Tests if gaps follow expected geometric distribution.
#'   \item **Serial Correlation Test**: Checks for dependencies between
#'     values at various lags. Low correlation indicates independence.
#'   \item **Poker Test**: Examines digit patterns in groups.
#'     Tests if patterns match theoretical probabilities.
#' }
#' 
#' All tests include error handling to ensure the suite completes even
#' if individual tests fail. Failed tests return an error message in the
#' interpretation field.
#' 
#' @note For best results, provide at least 10,000 samples. Smaller sample
#' sizes may lead to inconclusive results for some tests.
#' 
#' @examples
#' \dontrun{
#' # Run all advanced tests on uniform random data
#' samples <- runif(50000)
#' results <- run_advanced_tests(samples)
#' 
#' # Check which tests passed
#' lapply(results, function(test) {
#'   paste(test$test_name, "-", test$result)
#' })
#' 
#' # Run tests on PRNG output
#' library(qiprng)
#' createPRNG()
#' samples <- generatePRNG(50000)
#' results <- run_advanced_tests(samples)
#' }
#' 
#' @seealso 
#' \code{\link{test_entropy}}, \code{\link{test_gaps}},
#' \code{\link{test_serial_correlation}}, \code{\link{test_poker}}
#' 
#' @export
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
