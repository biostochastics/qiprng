# File: additional_wrappers.R
# ----------------------------------------------------------------------
#' Additional Wrappers for External Randomness Test Packages
#'
#' This module provides unified interfaces to additional randomness tests from
#' various R packages beyond CryptRndTest and randtests.
#'
#' Tests from tseries:
#' \itemize{
#'   \item runs.test - Runs test for randomness
#'   \item adf.test - Augmented Dickey-Fuller test
#'   \item jarque.bera.test - Jarque-Bera normality test
#' }
#'
#' Tests from randtoolbox:
#' \itemize{
#'   \item serial.test - Serial correlation test
#'   \item poker.test - Poker test
#'   \item gap.test - Gap test
#'   \item order.test - Order test
#' }
#'
#' Tests from lawstat:
#' \itemize{
#'   \item runs.test - Wald-Wolfowitz runs test
#'   \item levene.test - Levene's test for homogeneity
#' }

# Check package availability
has_tseries <- requireNamespace("tseries", quietly = TRUE)
has_randtoolbox <- requireNamespace("randtoolbox", quietly = TRUE)
has_lawstat <- requireNamespace("lawstat", quietly = TRUE)
has_random <- requireNamespace("random", quietly = TRUE)
has_randomness <- requireNamespace("randomness", quietly = TRUE)

#' Run tseries Test Suite
#'
#' Runs randomness tests from the tseries package on the provided data
#'
#' @param x Numeric vector of random numbers
#' @param test_type Character vector of test names to run
#' @param alpha Significance level (default: 0.05)
#' @param ... Additional parameters passed to individual tests
#' @return List of test results
#' @export
run_tseries_suite <- function(x, test_type = "all", alpha = 0.05, ...) {
  if (!has_tseries) {
    stop("tseries package is not installed. Install with: install.packages('tseries')")
  }
  
  results <- list()
  available_tests <- c("runs", "jarque_bera", "adf")
  
  if ("all" %in% test_type) {
    test_type <- available_tests
  }
  
  # Runs Test (different implementation than randtests)
  if ("runs" %in% test_type) {
    tryCatch({
      result <- tseries::runs.test(x, ...)
      results$runs_tseries <- list(
        description = "Runs Test (tseries)",
        p_value = result$p.value,
        statistic = result$statistic,
        result = if (is.na(result$p.value)) {
      "INCONCLUSIVE"
    } else if (result$p.value >= alpha) {
      "PASS"
    } else {
      "FAIL"
    },
        details = "Tests randomness using runs above and below median"
      )
    }, error = function(e) {
      results$runs_tseries <- list(
        description = "Runs Test (tseries)",
        error = as.character(e),
        result = "ERROR"
      )
    })
  }
  
  # Jarque-Bera Normality Test (on transformed data)
  if ("jarque_bera" %in% test_type) {
    tryCatch({
      # Transform uniform to normal for normality testing
      normal_x <- qnorm(x)
      # Remove any infinite values
      normal_x <- normal_x[is.finite(normal_x)]
      
      result <- tseries::jarque.bera.test(normal_x)
      results$jarque_bera <- list(
        description = "Jarque-Bera Normality Test (on transformed data)",
        p_value = result$p.value,
        statistic = result$statistic,
        result = if (is.na(result$p.value)) {
      "INCONCLUSIVE"
    } else if (result$p.value >= alpha) {
      "PASS"
    } else {
      "FAIL"
    },
        details = "Tests if transformed data follows normal distribution"
      )
    }, error = function(e) {
      results$jarque_bera <- list(
        description = "Jarque-Bera Normality Test",
        error = as.character(e),
        result = "ERROR"
      )
    })
  }
  
  # Augmented Dickey-Fuller Test
  if ("adf" %in% test_type) {
    tryCatch({
      # ADF test checks for stationarity/unit root
      result <- tseries::adf.test(x, alternative = "stationary", ...)
      results$adf <- list(
        description = "Augmented Dickey-Fuller Test",
        p_value = result$p.value,
        statistic = result$statistic,
        result = if (is.na(result$p.value)) {
      "INCONCLUSIVE"
    } else if (result$p.value < alpha) {
      "PASS"
    } else {
      "FAIL"
    }, # Note: reversed logic
        details = "Tests for stationarity (low p-value indicates randomness)"
      )
    }, error = function(e) {
      results$adf <- list(
        description = "Augmented Dickey-Fuller Test",
        error = as.character(e),
        result = "ERROR"
      )
    })
  }
  
  return(results)
}

#' Run randtoolbox Test Suite
#'
#' Runs randomness tests from the randtoolbox package on the provided data
#'
#' @param x Numeric vector of random numbers in [0,1)
#' @param test_type Character vector of test names to run
#' @param alpha Significance level (default: 0.05)
#' @param ... Additional parameters passed to individual tests
#' @return List of test results
#' @export
run_randtoolbox_suite <- function(x, test_type = "all", alpha = 0.05, ...) {
  if (!has_randtoolbox) {
    stop("randtoolbox package is not installed. Install with: install.packages('randtoolbox')")
  }
  
  results <- list()
  available_tests <- c("serial", "poker", "gap", "order")
  
  if ("all" %in% test_type) {
    test_type <- available_tests
  }
  
  # Serial Test
  if ("serial" %in% test_type) {
    tryCatch({
      # serial.test expects a matrix, so we need to arrange data
      n <- length(x)
      # Create overlapping pairs
      if (n > 1) {
        x_pairs <- cbind(x[-n], x[-1])
        result <- randtoolbox::serial.test(x_pairs, ...)
        
        results$serial <- list(
          description = "Serial Correlation Test",
          p_value = result$pvalue,
          statistic = result$statistic,
          result = if (is.na(result$pvalue)) {
      "INCONCLUSIVE"
    } else if (result$pvalue >= alpha) {
      "PASS"
    } else {
      "FAIL"
    },
          details = "Tests for serial correlation in adjacent values"
        )
      } else {
        results$serial <- list(
          description = "Serial Correlation Test",
          error = "Insufficient data for serial test",
          result = "ERROR"
        )
      }
    }, error = function(e) {
      results$serial <- list(
        description = "Serial Correlation Test",
        error = as.character(e),
        result = "ERROR"
      )
    })
  }
  
  # Poker Test
  if ("poker" %in% test_type) {
    tryCatch({
      result <- randtoolbox::poker.test(x, ...)
      results$poker <- list(
        description = "Poker Test",
        p_value = result$pvalue,
        statistic = result$statistic,
        result = if (is.na(result$pvalue)) {
      "INCONCLUSIVE"
    } else if (result$pvalue >= alpha) {
      "PASS"
    } else {
      "FAIL"
    },
        details = "Tests randomness using poker hand frequencies"
      )
    }, error = function(e) {
      results$poker <- list(
        description = "Poker Test",
        error = as.character(e),
        result = "ERROR"
      )
    })
  }
  
  # Gap Test
  if ("gap" %in% test_type) {
    tryCatch({
      result <- randtoolbox::gap.test(x, ...)
      results$gap <- list(
        description = "Gap Test",
        p_value = result$pvalue,
        statistic = result$statistic,
        result = if (is.na(result$pvalue)) {
      "INCONCLUSIVE"
    } else if (result$pvalue >= alpha) {
      "PASS"
    } else {
      "FAIL"
    },
        details = "Tests gaps between occurrences in ranges"
      )
    }, error = function(e) {
      results$gap <- list(
        description = "Gap Test",
        error = as.character(e),
        result = "ERROR"
      )
    })
  }
  
  # Order Test
  if ("order" %in% test_type) {
    tryCatch({
      result <- randtoolbox::order.test(x, ...)
      results$order <- list(
        description = "Order Test",
        p_value = result$pvalue,
        statistic = result$statistic,
        result = if (is.na(result$pvalue)) {
      "INCONCLUSIVE"
    } else if (result$pvalue >= alpha) {
      "PASS"
    } else {
      "FAIL"
    },
        details = "Tests for patterns in ordered sequences"
      )
    }, error = function(e) {
      results$order <- list(
        description = "Order Test",
        error = as.character(e),
        result = "ERROR"
      )
    })
  }
  
  return(results)
}

#' Run lawstat Test Suite
#'
#' Runs randomness and homogeneity tests from the lawstat package
#'
#' @param x Numeric vector of random numbers
#' @param test_type Character vector of test names to run
#' @param alpha Significance level (default: 0.05)
#' @param ... Additional parameters passed to individual tests
#' @return List of test results
#' @export
run_lawstat_suite <- function(x, test_type = "all", alpha = 0.05, ...) {
  if (!has_lawstat) {
    stop("lawstat package is not installed. Install with: install.packages('lawstat')")
  }
  
  results <- list()
  available_tests <- c("runs_ww", "levene")
  
  if ("all" %in% test_type) {
    test_type <- available_tests
  }
  
  # Wald-Wolfowitz Runs Test
  if ("runs_ww" %in% test_type) {
    tryCatch({
      # Need to split data into two groups for WW test
      n <- length(x)
      if (n >= 4) {
        # Split at median
        med <- median(x)
        group <- ifelse(x <= med, 1, 2)
        
        result <- lawstat::runs.test(x, group, ...)
        results$runs_ww <- list(
          description = "Wald-Wolfowitz Runs Test",
          p_value = result$p.value,
          statistic = result$statistic,
          result = if (is.na(result$p.value)) {
      "INCONCLUSIVE"
    } else if (result$p.value >= alpha) {
      "PASS"
    } else {
      "FAIL"
    },
          details = "Tests randomness using runs between groups"
        )
      } else {
        results$runs_ww <- list(
          description = "Wald-Wolfowitz Runs Test",
          error = "Insufficient data",
          result = "ERROR"
        )
      }
    }, error = function(e) {
      results$runs_ww <- list(
        description = "Wald-Wolfowitz Runs Test",
        error = as.character(e),
        result = "ERROR"
      )
    })
  }
  
  # Levene's Test for Homogeneity of Variance
  if ("levene" %in% test_type) {
    tryCatch({
      # Split data into groups for variance comparison
      n <- length(x)
      if (n >= 4) {
        # Create groups based on quartiles
        q <- quantile(x, probs = c(0.25, 0.5, 0.75))
        group <- cut(x, breaks = c(-Inf, q, Inf), labels = 1:4)
        
        result <- lawstat::levene.test(x, group, location = "median", ...)
        results$levene <- list(
          description = "Levene's Test for Homogeneity",
          p_value = result$p.value,
          statistic = result$statistic,
          result = if (is.na(result$p.value)) {
      "INCONCLUSIVE"
    } else if (result$p.value >= alpha) {
      "PASS"
    } else {
      "FAIL"
    },
          details = "Tests for equal variances across groups"
        )
      } else {
        results$levene <- list(
          description = "Levene's Test",
          error = "Insufficient data",
          result = "ERROR"
        )
      }
    }, error = function(e) {
      results$levene <- list(
        description = "Levene's Test",
        error = as.character(e),
        result = "ERROR"
      )
    })
  }
  
  return(results)
}

#' Run Entropy and Complexity Tests
#'
#' Placeholder for entropy-based tests (when randomness package is available)
#'
#' @param x Numeric vector or binary sequence
#' @param test_type Character vector of test names to run
#' @param alpha Significance level (default: 0.05)
#' @param ... Additional parameters
#' @return List of test results
#' @export
run_entropy_suite <- function(x, test_type = "all", alpha = 0.05, ...) {
  results <- list()
  
  # Shannon Entropy (manual implementation)
  if ("shannon" %in% test_type || "all" %in% test_type) {
    tryCatch({
      # Convert to discrete values if continuous
      if (all(x >= 0 & x <= 1)) {
        # Discretize into bins
        bins <- 16
        x_discrete <- cut(x, breaks = seq(0, 1, length.out = bins + 1), 
                          include.lowest = TRUE, labels = FALSE)
      } else {
        x_discrete <- x
      }
      
      # Calculate frequencies
      freq <- table(x_discrete) / length(x_discrete)
      
      # Shannon entropy
      entropy <- -sum(freq * log2(freq + 1e-10))
      max_entropy <- log2(length(unique(x_discrete)))
      normalized_entropy <- entropy / max_entropy
      
      # Simple test: normalized entropy should be close to 1 for random data
      p_value <- 2 * min(normalized_entropy, 1 - normalized_entropy)
      
      results$shannon_entropy <- list(
        description = "Shannon Entropy Test",
        entropy = entropy,
        normalized_entropy = normalized_entropy,
        p_value = p_value,
        result = if (is.na(normalized_entropy)) {
      "INCONCLUSIVE"
    } else if (normalized_entropy > 0.95) {
      "PASS"
    } else {
      "FAIL"
    },
        details = sprintf("Entropy: %.4f, Normalized: %.4f", entropy, normalized_entropy)
      )
    }, error = function(e) {
      results$shannon_entropy <- list(
        description = "Shannon Entropy Test",
        error = as.character(e),
        result = "ERROR"
      )
    })
  }
  
  # Serial Correlation Coefficient
  if ("serial_correlation" %in% test_type || "all" %in% test_type) {
    tryCatch({
      n <- length(x)
      if (n > 1) {
        # Calculate serial correlation
        mean_x <- mean(x)
        num <- sum((x[-n] - mean_x) * (x[-1] - mean_x))
        den <- sum((x - mean_x)^2)
        
        serial_corr <- num / den
        
        # Approximate standard error
        se <- 1 / sqrt(n - 1)
        z_stat <- serial_corr / se
        p_value <- 2 * (1 - pnorm(abs(z_stat)))
        
        results$serial_correlation <- list(
          description = "Serial Correlation Coefficient",
          correlation = serial_corr,
          p_value = p_value,
          statistic = z_stat,
          result = if (is.na(p_value)) {
      "INCONCLUSIVE"
    } else if (p_value >= alpha) {
      "PASS"
    } else {
      "FAIL"
    },
          details = sprintf("Serial correlation: %.4f", serial_corr)
        )
      } else {
        results$serial_correlation <- list(
          description = "Serial Correlation",
          error = "Insufficient data",
          result = "ERROR"
        )
      }
    }, error = function(e) {
      results$serial_correlation <- list(
        description = "Serial Correlation",
        error = as.character(e),
        result = "ERROR"
      )
    })
  }
  
  return(results)
}

#' Integrate Additional Tests into qiprng Suite
#'
#' Adds additional external package tests to the qiprng test suite results
#'
#' @param suite The qiprng test suite object
#' @param include_tseries Whether to include tseries tests
#' @param include_randtoolbox Whether to include randtoolbox tests
#' @param include_lawstat Whether to include lawstat tests
#' @param include_entropy Whether to include entropy tests
#' @return Updated suite with additional test results
#' @export
run_additional_wrapper_tests <- function(suite, 
                                        include_tseries = TRUE,
                                        include_randtoolbox = TRUE,
                                        include_lawstat = TRUE,
                                        include_entropy = TRUE) {
  
  # Generate data if not already present
  n <- suite$config$external_sample_size %||% 1e5
  x <- suite$prng_func(n)
  
  # Initialize additional results section
  if (is.null(suite$results$additional_wrappers)) {
    suite$results$additional_wrappers <- list()
  }
  
  # Run tseries suite
  if (include_tseries && has_tseries) {
    message("Running tseries test suite...")
    tseries_results <- run_tseries_suite(x, 
                                         alpha = suite$config$significance_level)
    suite$results$additional_wrappers$tseries <- tseries_results
  }
  
  # Run randtoolbox suite
  if (include_randtoolbox && has_randtoolbox) {
    message("Running randtoolbox test suite...")
    randtoolbox_results <- run_randtoolbox_suite(x,
                                                 alpha = suite$config$significance_level)
    suite$results$additional_wrappers$randtoolbox <- randtoolbox_results
  }
  
  # Run lawstat suite
  if (include_lawstat && has_lawstat) {
    message("Running lawstat test suite...")
    lawstat_results <- run_lawstat_suite(x,
                                        alpha = suite$config$significance_level)
    suite$results$additional_wrappers$lawstat <- lawstat_results
  }
  
  # Run entropy tests
  if (include_entropy) {
    message("Running entropy test suite...")
    entropy_results <- run_entropy_suite(x,
                                        alpha = suite$config$significance_level)
    suite$results$additional_wrappers$entropy <- entropy_results
  }
  
  # Summary statistics
  all_results <- c(
    if (include_tseries && has_tseries) tseries_results else list(),
    if (include_randtoolbox && has_randtoolbox) randtoolbox_results else list(),
    if (include_lawstat && has_lawstat) lawstat_results else list(),
    if (include_entropy) entropy_results else list()
  )
  
  passed <- sum(sapply(all_results, function(r) r$result == "PASS"))
  failed <- sum(sapply(all_results, function(r) r$result == "FAIL"))
  errors <- sum(sapply(all_results, function(r) r$result == "ERROR"))
  
  suite$results$additional_wrappers$summary <- list(
    total_tests = length(all_results),
    passed = passed,
    failed = failed,
    errors = errors,
    pass_rate = if (length(all_results) > 0) passed / length(all_results) else 0
  )
  
  return(suite)
}

# Default export
if (!exists("additional_export")) {
  additional_export <- list(
    has_tseries = has_tseries,
    has_randtoolbox = has_randtoolbox,
    has_lawstat = has_lawstat,
    has_random = has_random,
    has_randomness = has_randomness,
    run_tseries_suite = run_tseries_suite,
    run_randtoolbox_suite = run_randtoolbox_suite,
    run_lawstat_suite = run_lawstat_suite,
    run_entropy_suite = run_entropy_suite,
    run_additional_wrapper_tests = run_additional_wrapper_tests
  )
}

# Null operator for default values
`%||%` <- function(a, b) if (is.null(a)) b else a
