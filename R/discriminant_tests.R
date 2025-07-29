# Enhanced Discriminant Property Testing Framework
# Comprehensive statistical analysis of random numbers generated using different discriminants
# Incorporates advanced tests for cryptographic-quality randomness assessment

library(qiprng)
library(moments)
library(nortest)

# Load additional packages for enhanced testing
suppressPackageStartupMessages({
  # Set CRAN mirror if not already set
  if (length(getOption("repos")) == 0 || getOption("repos")["CRAN"] == "@CRAN@") {
    options(repos = c(CRAN = "https://cran.rstudio.com/"))
  }
  
  if (!require(randtests, quietly = TRUE)) {
    cat("Installing randtests package...\n")
    install.packages("randtests", quiet = TRUE)
    library(randtests)
  }
  
  if (!require(tseries, quietly = TRUE)) {
    cat("Installing tseries package...\n")
    install.packages("tseries", quiet = TRUE)
    library(tseries)
  }
  
  # Skip CryptRndTest for now due to installation issues
  # if (!require(CryptRndTest, quietly = TRUE)) {
  #   cat("Installing CryptRndTest package...\n")
  #   install.packages("CryptRndTest", quiet = TRUE)
  #   library(CryptRndTest)
  # }
})

#' Load discriminants from CSV file
#'
#' @param file_path Path to discriminants.csv file
#' @return Data frame with discriminant parameters
load_discriminants <- function(file_path = "discriminants.csv") {
  if (!file.exists(file_path)) {
    stop("Discriminants file not found: ", file_path)
  }
  
  discriminants <- read.csv(file_path, stringsAsFactors = FALSE)
  
  # Validate the discriminant calculation
  calculated_discriminant <- discriminants$b^2 - 4 * discriminants$a * discriminants$c
  if (!all(abs(calculated_discriminant - discriminants$Discriminant) < 1e-10)) {
    warning("Some discriminants may not be correctly calculated")
  }
  
  # Verify constraints
  invalid_a <- discriminants$a <= 0
  invalid_c <- discriminants$c >= 0
  invalid_discriminant <- discriminants$Discriminant <= 0
  
  if (any(invalid_a)) {
    warning("Found ", sum(invalid_a), " entries with a <= 0")
  }
  if (any(invalid_c)) {
    warning("Found ", sum(invalid_c), " entries with c >= 0")
  }
  if (any(invalid_discriminant)) {
    warning("Found ", sum(invalid_discriminant), " entries with discriminant <= 0")
  }
  
  return(discriminants)
}

#' Generate random numbers using specific discriminant parameters
#'
#' @param a Parameter a
#' @param b Parameter b  
#' @param c Parameter c
#' @param n Number of samples to generate
#' @param precision MPFR precision (default 128)
#' @return Vector of random numbers
generate_with_discriminant <- function(a, b, c, n = 50000, precision = 256) {
  # Create PRNG with specific parameters
  config <- list(
    a = a,
    b = b, 
    c = c,
    precision = precision,
    use_parallel_filling = FALSE,  # Based on memory about performance issue
    use_cryptographic_mixing = TRUE  # Enable for better quality
  )
  
  # Initialize PRNG
  createPRNG(config = config)
  
  # Generate samples
  samples <- generatePRNG(n)
  
  return(samples)
}

#' Test uniformity using Kolmogorov-Smirnov test
#'
#' @param samples Vector of random samples
#' @return List with test results
test_uniformity <- function(samples, bins = 20) {
  # 1. Kolmogorov-Smirnov test
  ks_test <- ks.test(samples, "punif", 0, 1)
  
  # 2. Chi-Squared Goodness-of-Fit test
  breaks <- seq(0, 1, length.out = bins + 1)
  observed_counts <- hist(samples, breaks = breaks, plot = FALSE)$counts
  expected_counts <- length(samples) / bins
  
  # Warning for low expected counts
  if (expected_counts < 5) {
    warning("Chi-squared test may be inaccurate with expected counts < 5")
  }
  
  chi_sq_test <- chisq.test(observed_counts, p = rep(1/bins, bins))
  
  # Combine results
  ks_passed <- ks_test$p.value > 0.05
  chi_sq_passed <- chi_sq_test$p.value > 0.05
  
  return(list(
    test_name = "Uniformity Tests (KS & Chi-Squared)",
    ks_statistic = ks_test$statistic,
    ks_p_value = ks_test$p.value,
    chi_sq_statistic = chi_sq_test$statistic,
    chi_sq_p_value = chi_sq_test$p.value,
    passed = ks_passed && chi_sq_passed,
    interpretation = ifelse(ks_passed && chi_sq_passed, 
                           "Passes both uniformity tests (good)",
                           paste("Fails uniformity tests (KS:", 
                                 ifelse(ks_passed, "Pass", "Fail"), ", Chi-Sq:",
                                 ifelse(chi_sq_passed, "Pass", "Fail"), ")"))
  ))
}

#' Test independence using runs test
#'
#' @param samples Vector of random samples
#' @return List with test results
test_independence <- function(samples) {
  # Convert to binary sequence (above/below median)
  binary_seq <- as.numeric(samples > median(samples))
  
  # Count runs
  runs <- rle(binary_seq)
  n_runs <- length(runs$lengths)
  n1 <- sum(binary_seq == 1)
  n0 <- sum(binary_seq == 0)
  n <- length(binary_seq)
  
  # Expected runs and variance
  expected_runs <- (2 * n1 * n0) / n + 1
  var_runs <- (2 * n1 * n0 * (2 * n1 * n0 - n)) / (n^2 * (n - 1))
  
  # Z-score
  z_score <- (n_runs - expected_runs) / sqrt(var_runs)
  p_value <- 2 * (1 - pnorm(abs(z_score)))
  
  return(list(
    test_name = "Runs Test for Independence",
    statistic = z_score,
    p_value = p_value,
    passed = p_value > 0.05,
    interpretation = ifelse(p_value > 0.05,
                           "Independent sequence (good)",
                           "Dependent sequence (poor)")
  ))
}

#' Test for autocorrelation
#'
#' @param samples Vector of random samples
#' @param max_lag Maximum lag to test
#' @return List with test results
test_autocorrelation <- function(samples, max_lag = 50) {
  autocorr <- acf(samples, lag.max = max_lag, plot = FALSE)
  
  # Test using both statistical significance (99% confidence) and empirical threshold
  n <- length(samples)
  
  # Statistical significance bound (99% confidence)
  stat_bound <- qnorm(0.995) / sqrt(n)  # 99% confidence = 2.576
  
  # Empirical threshold from comprehensive analysis (single threshold for excellence)
  empirical_threshold <- 0.010  # Based on analysis findings
  
  # Use the more permissive of the two thresholds
  final_threshold <- max(stat_bound, empirical_threshold)
  
  # Find significant lags using final threshold
  significant_lags <- which(abs(autocorr$acf[-1]) > final_threshold)
  
  # Calculate max absolute autocorrelation for reporting
  max_abs_acf <- max(abs(autocorr$acf[-1]), na.rm = TRUE)
  
  return(list(
    test_name = "Autocorrelation Test (Enhanced)",
    autocorrelations = autocorr$acf[-1],  # Exclude lag 0
    significant_lags = significant_lags,
    n_sig_lags = length(significant_lags),
    max_abs_acf = max_abs_acf,
    threshold_used = final_threshold,
    stat_bound = stat_bound,
    empirical_threshold = empirical_threshold,
    passed = length(significant_lags) == 0,
    interpretation = ifelse(length(significant_lags) == 0,
                           sprintf("No significant autocorrelation (max |ACF| = %.4f < %.4f)", max_abs_acf, final_threshold),
                           sprintf("Significant autocorrelation at %d lags (max |ACF| = %.4f > %.4f): %s", 
                                  length(significant_lags), max_abs_acf, final_threshold,
                                  paste(significant_lags, collapse = ", ")))
  ))
}

#' Test moments (mean, variance, skewness, kurtosis)
#'
#' @param samples Vector of random samples
#' @return List with test results
test_moments <- function(samples) {
  sample_mean <- mean(samples)
  sample_var <- var(samples)
  sample_skew <- skewness(samples)
  sample_kurt <- kurtosis(samples)
  
  # For uniform [0,1]: mean=0.5, var=1/12≈0.0833, skew=0, kurt=1.8
  expected_mean <- 0.5
  expected_var <- 1/12
  expected_skew <- 0
  expected_kurt <- 1.8
  
  return(list(
    test_name = "Moment Analysis",
    sample_mean = sample_mean,
    sample_variance = sample_var,
    sample_skewness = sample_skew,
    sample_kurtosis = sample_kurt,
    expected_mean = expected_mean,
    expected_variance = expected_var,
    expected_skewness = expected_skew,
    expected_kurtosis = expected_kurt,
    mean_error = abs(sample_mean - expected_mean),
    var_error = abs(sample_var - expected_var),
    skew_error = abs(sample_skew - expected_skew),
    kurt_error = abs(sample_kurt - expected_kurt)
  ))
}

#' Enhanced periodicity testing using multiple spectral methods
#'
#' @param samples Vector of random samples
#' @return List with comprehensive periodicity test results
test_periodicity <- function(samples) {
  n <- length(samples)
  results <- list(test_name = "Enhanced Periodicity Analysis")
  
  tryCatch({
    # 1. Fisher's g-test for hidden periodicities
    periodogram <- abs(fft(samples - mean(samples)))^2
    periodogram <- periodogram[2:(n/2 + 1)]  # Only positive frequencies
    
    g_stat <- max(periodogram) / sum(periodogram)
    alpha <- 0.05
    critical_value <- -log(alpha) / length(periodogram)
    fisher_p_value <- exp(-length(periodogram) * g_stat)
    fisher_passed <- fisher_p_value > alpha  # Pass if p-value > significance level
    
    # 2. Bartels rank test for randomness (from randtests)
    bartels_result <- tryCatch({
      bartels.rank.test(samples)
    }, error = function(e) list(p.value = NA, statistic = NA))
    
    # 3. Cox-Stuart test for trend (from randtests)
    cox_stuart_result <- tryCatch({
      cox.stuart.test(samples)
    }, error = function(e) list(p.value = NA, statistic = NA))
    
    # 4. Turning points test (from randtests)
    turning_points_result <- tryCatch({
      turning.point.test(samples)
    }, error = function(e) list(p.value = NA, statistic = NA))
    
    # 5. Ljung-Box test for serial correlation (from tseries)
    ljung_box_result <- tryCatch({
      Box.test(samples, lag = min(20, floor(n/5)), type = "Ljung-Box")
    }, error = function(e) list(p.value = NA, statistic = NA))
    
    # Combine results
    all_p_values <- c(fisher_p_value, bartels_result$p.value, 
                     cox_stuart_result$p.value, turning_points_result$p.value,
                     ljung_box_result$p.value)
    valid_p_values <- all_p_values[!is.na(all_p_values)]
    
    # Overall assessment: pass if majority of tests pass
    if (length(valid_p_values) > 0) {
      passed_tests <- sum(valid_p_values > 0.05)
      total_tests <- length(valid_p_values)
      overall_passed <- passed_tests >= (total_tests / 2)
      combined_p_value <- mean(valid_p_values, na.rm = TRUE)
    } else {
      overall_passed <- FALSE
      combined_p_value <- NA
    }
    
    results <- list(
      test_name = "Enhanced Periodicity Analysis",
      fisher_g_stat = g_stat,
      fisher_p_value = fisher_p_value,
      bartels_p_value = bartels_result$p.value,
      cox_stuart_p_value = cox_stuart_result$p.value,
      turning_points_p_value = turning_points_result$p.value,
      ljung_box_p_value = ljung_box_result$p.value,
      combined_p_value = combined_p_value,
      passed = overall_passed,
      tests_passed = paste(passed_tests, "of", total_tests),
      interpretation = ifelse(overall_passed,
                             paste("No significant periodicity/patterns detected (", passed_tests, "/", total_tests, " tests passed)"),
                             paste("Potential periodicity/patterns detected (", passed_tests, "/", total_tests, " tests passed)"))
    )
    
  }, error = function(e) {
    results <- list(
      test_name = "Enhanced Periodicity Analysis",
      passed = FALSE,
      p_value = NA,
      interpretation = paste("Error in periodicity analysis:", e$message)
    )
  })
  
  return(results)
}

#' Comprehensive test suite for a single discriminant
#'
#' @param a Parameter a
#' @param b Parameter b
#' @param c Parameter c
#' @param discriminant Discriminant value
#' @param n Number of samples to generate
#' @return List with all test results
test_discriminant <- function(a, b, c, discriminant, n = 50000) {
  cat("Testing discriminant: a=", a, ", b=", b, ", c=", c, ", Δ=", discriminant, "\n")
  
  # Robust error handling wrapper
  tryCatch({
    # Generate
    samples <- generate_with_discriminant(a, b, c, n)
    
    # Validate
    if (is.null(samples) || length(samples) == 0 || any(is.na(samples)) || any(is.infinite(samples))) {
      stop("Invalid samples generated")
    }
    
    # Run all
    uniformity_result <- tryCatch(test_uniformity(samples), error = function(e) {
      list(test_name = "Uniformity Tests", passed = FALSE, ks_p_value = NA, chi_sq_p_value = NA, 
           interpretation = paste("Error:", e$message))
    })
    
    independence_result <- tryCatch(test_independence(samples), error = function(e) {
      list(test_name = "Independence Test", passed = FALSE, p_value = NA, 
           interpretation = paste("Error:", e$message))
    })
    
    autocorr_result <- tryCatch(test_autocorrelation(samples), error = function(e) {
      list(test_name = "Autocorrelation Test", passed = FALSE, 
           interpretation = paste("Error:", e$message))
    })
    
    moments_result <- tryCatch(test_moments(samples), error = function(e) {
      list(test_name = "Moment Analysis", mean_error = NA, var_error = NA, 
           skew_error = NA, kurt_error = NA, interpretation = paste("Error:", e$message))
    })
    
    periodicity_result <- tryCatch(test_periodicity(samples), error = function(e) {
      list(test_name = "Periodicity Test", passed = FALSE, p_value = NA, 
           interpretation = paste("Error:", e$message))
    })
    
    # Run advanced tests
    source("R/advanced_tests.R")
    advanced_results <- tryCatch(run_advanced_tests(samples), 
                                error = function(e) list(error = paste("Advanced tests failed:", e$message)))
    
    # Compile results
    results <- list(
      parameters = list(a = a, b = b, c = c, discriminant = discriminant),
      sample_size = n,
      uniformity = uniformity_result,
      independence = independence_result,
      autocorrelation = autocorr_result,
      moments = moments_result,
      periodicity = periodicity_result,
      advanced = advanced_results
    )
    
    # Calculate weighted overall score with safe handling (including advanced tests)
    weights <- list(uniformity = 0.25, independence = 0.25, autocorrelation = 0.15, periodicity = 0.15, advanced = 0.20)
    
    # Safe scalar extraction to prevent length 0 vectors
    uniformity_score <- 0
    if (!is.null(results$uniformity$passed) && length(results$uniformity$passed) > 0 && !is.na(results$uniformity$passed)) {
      uniformity_score <- as.numeric(results$uniformity$passed)
    }
    
    independence_score <- 0
    if (!is.null(results$independence$passed) && length(results$independence$passed) > 0 && !is.na(results$independence$passed)) {
      independence_score <- as.numeric(results$independence$passed)
    }
    
    autocorr_score <- 0
    if (!is.null(results$autocorrelation$passed) && length(results$autocorrelation$passed) > 0 && !is.na(results$autocorrelation$passed)) {
      autocorr_score <- as.numeric(results$autocorrelation$passed)
    }
    
    periodicity_score <- 0
    if (!is.null(results$periodicity$passed) && length(results$periodicity$passed) > 0 && !is.na(results$periodicity$passed)) {
      periodicity_score <- as.numeric(results$periodicity$passed)
    }
    
    # Advanced tests score
    advanced_score <- 0
    if (!is.null(results$advanced) && !is.null(results$advanced$entropy) && !is.null(results$advanced$entropy$passed)) {
      entropy_pass <- as.numeric(results$advanced$entropy$passed)
      gaps_pass <- as.numeric(results$advanced$gaps$passed %||% FALSE)
      serial_pass <- as.numeric(results$advanced$serial_correlation$passed %||% FALSE)
      poker_pass <- as.numeric(results$advanced$poker$passed %||% FALSE)
      
      # Average of advanced test results
      advanced_score <- mean(c(entropy_pass, gaps_pass, serial_pass, poker_pass), na.rm = TRUE)
    }
    
    score <- uniformity_score * weights$uniformity + 
             independence_score * weights$independence + 
             autocorr_score * weights$autocorrelation + 
             periodicity_score * weights$periodicity + 
             advanced_score * weights$advanced
    
    results$overall_score <- score
    
    # Safe quality rating assignment
    if (is.null(score) || length(score) == 0 || is.na(score)) {
      results$quality_rating <- "Error"
    } else if (score >= 0.90) {
      results$quality_rating <- "Excellent"
    } else if (score >= 0.80) {
      results$quality_rating <- "Very-Good"
    } else if (score >= 0.65) {
      results$quality_rating <- "Good"
    } else if (score >= 0.50) {
      results$quality_rating <- "Fair"
    } else {
      results$quality_rating <- "Poor"
    }
    
    return(results)
    
  }, error = function(e) {
    # Return error result if everything fails
    return(list(
      parameters = list(a = a, b = b, c = c, discriminant = discriminant),
      sample_size = n,
      error = paste("Analysis failed:", e$message),
      overall_score = 0,
      quality_rating = "Error",
      uniformity = list(passed = FALSE),
      independence = list(passed = FALSE),
      autocorrelation = list(passed = FALSE),
      periodicity = list(passed = FALSE)
    ))
  })
}

#' Run tests on all discriminants and generate report (Parallel Implementation)
#'
#' @param discriminants_file Path to discriminants CSV file
#' @param sample_size Number of samples per discriminant
#' @param max_discriminants Maximum number of discriminants to test (for speed)
#' @param n_cores Number of cores to use (defaults to detected cores - 1)
#' @param chunk_size Discriminants per parallel chunk (for progress tracking)
#' @return List with all results
run_discriminant_analysis <- function(discriminants_file = "discriminants.csv", 
                                    sample_size = 50000, 
                                    max_discriminants = NULL,
                                    n_cores = NULL,
                                    chunk_size = 50) {
  
  # Load required packages for parallel processing
  suppressPackageStartupMessages({
    if (!require(parallel, quietly = TRUE)) {
      stop("parallel package required for efficient processing")
    }
  })
  
  # Load discriminants
  discriminants <- load_discriminants(discriminants_file)
  
  if (!is.null(max_discriminants)) {
    discriminants <- discriminants[seq_len(min(max_discriminants, nrow(discriminants))), ]
  }
  
  # Auto-detect cores (M4Pro has 10-14 cores typically)
  if (is.null(n_cores)) {
    n_cores <- max(1, detectCores() - 1)  # Leave 1 core free
  }
  n_cores <- min(n_cores, nrow(discriminants))  # Don't use more cores than discriminants
  
  cat("=== PARALLEL DISCRIMINANT ANALYSIS ===\n")
  cat("Testing", nrow(discriminants), "discriminants with", sample_size, "samples each\n")
  cat("Using", n_cores, "cores in parallel\n")
  cat("Chunk size:", chunk_size, "discriminants per batch\n")
  
  # Estimate runtime
  est_time_per_discriminant <- ifelse(sample_size <= 50000, 15, 
                                     ifelse(sample_size <= 100000, 25, 
                                           sample_size * 0.00025))  # seconds
  est_total_mins <- (nrow(discriminants) * est_time_per_discriminant) / (n_cores * 60)
  cat("Estimated runtime:", round(est_total_mins, 1), "minutes\n\n")
  
  start_time <- Sys.time()
  
  # Create parallel worker function with timeout protection
  parallel_test_worker <- function(i) {
    tryCatch({
      # Add timeout wrapper for individual discriminants
      R.utils::withTimeout({
        row <- discriminants[i, ]
        result <- test_discriminant(row$a, row$b, row$c, row$Discriminant, sample_size)
        result$index <- i
        return(result)
      }, timeout = max(300, sample_size * 0.001))  # Dynamic timeout based on sample size
    }, error = function(e) {
      # Return error result if test fails or times out
      return(list(
        index = i,
        parameters = list(
          a = discriminants[i, "a"], 
          b = discriminants[i, "b"], 
          c = discriminants[i, "c"], 
          discriminant = discriminants[i, "Discriminant"]
        ),
        sample_size = sample_size,
        error = paste("Parallel worker error:", e$message),
        overall_score = 0,
        quality_rating = "Error",
        uniformity = list(passed = FALSE),
        independence = list(passed = FALSE),
        autocorrelation = list(passed = FALSE),
        periodicity = list(passed = FALSE)
      ))
    })
  }
  
  # Process in chunks to provide progress updates and handle memory
  all_results <- list()
  n_chunks <- ceiling(nrow(discriminants) / chunk_size)
  
  for (chunk_idx in seq_len(n_chunks)) {
    chunk_start <- (chunk_idx - 1) * chunk_size + 1
    chunk_end <- min(chunk_idx * chunk_size, nrow(discriminants))
    chunk_indices <- chunk_start:chunk_end
    
    cat("Processing chunk", chunk_idx, "of", n_chunks, 
        "(discriminants", chunk_start, "to", chunk_end, ")...\n")
    
    # Initialize cluster for this chunk
    cl <- makeCluster(n_cores, type = "PSOCK")
    
    tryCatch({
      # Export necessary objects to workers
      clusterExport(cl, c("discriminants", "sample_size", "test_discriminant", 
                          "generate_with_discriminant", "test_uniformity", 
                          "test_independence", "test_autocorrelation", 
                          "test_periodicity", "test_moments"), 
                   envir = environment())
      
      # Load required libraries on each worker
      clusterEvalQ(cl, {
        library(qiprng)
        library(moments)
        library(nortest)
        suppressPackageStartupMessages({
          if (require(randtests, quietly = TRUE)) library(randtests)
          if (require(tseries, quietly = TRUE)) library(tseries)
          if (require(R.utils, quietly = TRUE)) library(R.utils)
        })
      })
      
      # Run parallel computation for this chunk
      chunk_results <- parLapply(cl, chunk_indices, parallel_test_worker)
      
      # Add chunk results to overall results
      all_results <- c(all_results, chunk_results)
      
      # Progress update
      elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))
      progress_pct <- chunk_end / nrow(discriminants) * 100
      remaining_time <- elapsed * (100 / progress_pct - 1)
      
      cat(sprintf("Chunk %d complete: %.1f%% done, %.1f min elapsed, ~%.1f min remaining\n", 
                  chunk_idx, progress_pct, elapsed, remaining_time))
      
    }, finally = {
      # Always stop the cluster
      stopCluster(cl)
    })
    
    # Force garbage collection between chunks
    gc()
  }
  
  end_time <- Sys.time()
  total_time <- difftime(end_time, start_time, units = "mins")
  
  cat("\n=== PARALLEL ANALYSIS COMPLETE ===\n")
  cat("Total runtime:", round(as.numeric(total_time), 2), "minutes\n")
  cat("Average time per discriminant:", round(as.numeric(total_time) * 60 / nrow(discriminants), 2), "seconds\n")
  cat("Speedup factor: ~", round(n_cores * 0.8, 1), "x (assuming 80% parallel efficiency)\n")
  
  return(all_results)
}
