# File: bootstrap_framework.R
# ----------------------------------------------------------------------
#' Bootstrap Framework for Statistical P-value Calculations
#'
#' This module provides bootstrap-based methods for calculating p-values
#' and performing hypothesis tests, especially for situations where
#' analytical distributions are unknown or complex.
#'
#' The framework includes:
#' \itemize{
#'   \item Bootstrap p-value calculation using resampling
#'   \item Permutation testing for non-parametric hypothesis tests
#'   \item Monte Carlo p-value estimation via simulation
#' }
#'
#' These methods are particularly useful for compression tests and other
#' statistical tests where the null distribution is not well-defined.
#'
#' @name bootstrap_framework
#' @aliases bootstrap-framework
#' @keywords internal

#' Calculate bootstrap p-value
#'
#' Estimates p-values using bootstrap resampling with replacement.
#' This method is useful when the theoretical distribution of a test
#' statistic is unknown or complex.
#'
#' @param data Numeric vector of observations
#' @param test_statistic Function that computes the test statistic from data
#' @param observed_stat The observed test statistic (if NULL, will be computed)
#' @param n_bootstraps Number of bootstrap samples (default: 10000)
#' @param alternative Character string specifying the alternative hypothesis:
#'   "two.sided", "less", or "greater" (default: "two.sided")
#' @param progress Logical; show progress bar for long computations (default: TRUE)
#' @param seed Random seed for reproducibility (default: NULL)
#'
#' @return A list containing:
#'   \item{p.value}{The bootstrap p-value}
#'   \item{bootstrap_stats}{Vector of bootstrap test statistics}
#'   \item{observed_stat}{The observed test statistic}
#'   \item{n_bootstraps}{Number of bootstrap samples used}
#'
#' @examples
#' \dontrun{
#' # Test if mean differs from 0.5
#' x <- runif(100)
#' stat_func <- function(d) mean(d) - 0.5
#' result <- bootstrap_p_value(x, stat_func)
#' print(result$p.value)
#' }
#'
#' @export
bootstrap_p_value <- function(data, test_statistic, observed_stat = NULL,
                              n_bootstraps = 10000, alternative = "two.sided",
                              progress = TRUE, seed = NULL) {
  # Input validation
  if (!is.numeric(data) || length(data) < 2) {
    stop("'data' must be a numeric vector with at least 2 elements")
  }

  if (!is.function(test_statistic)) {
    stop("'test_statistic' must be a function")
  }

  if (!is.numeric(n_bootstraps) || n_bootstraps < 100) {
    stop("'n_bootstraps' must be a numeric value >= 100")
  }

  alternative <- match.arg(alternative, c("two.sided", "less", "greater"))

  # Set seed if provided
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Calculate observed statistic if not provided
  if (is.null(observed_stat)) {
    observed_stat <- test_statistic(data)
  }

  if (!is.numeric(observed_stat) || length(observed_stat) != 1) {
    stop("Test statistic must return a single numeric value")
  }

  n <- length(data)
  bootstrap_stats <- numeric(n_bootstraps)

  # Progress bar setup
  if (progress && n_bootstraps >= 1000) {
    pb <- txtProgressBar(min = 0, max = n_bootstraps, style = 3)
  }

  # Perform bootstrap resampling
  for (i in seq_len(n_bootstraps)) {
    # Resample with replacement
    bootstrap_sample <- sample(data, n, replace = TRUE)

    # Calculate test statistic
    tryCatch(
      {
        bootstrap_stats[i] <- test_statistic(bootstrap_sample)
      },
      error = function(e) {
        warning(sprintf("Error in bootstrap iteration %d: %s", i, e$message))
        bootstrap_stats[i] <- NA
      }
    )

    # Update progress
    if (progress && n_bootstraps >= 1000 && i %% 100 == 0) {
      setTxtProgressBar(pb, i)
    }
  }

  # Close progress bar
  if (progress && n_bootstraps >= 1000) {
    close(pb)
  }

  # Remove any NA values
  valid_stats <- bootstrap_stats[!is.na(bootstrap_stats)]
  n_valid <- length(valid_stats)

  if (n_valid < n_bootstraps * 0.9) {
    warning(sprintf("Only %d of %d bootstrap samples were valid", n_valid, n_bootstraps))
  }

  # Calculate p-value based on alternative hypothesis
  if (alternative == "two.sided") {
    # Two-sided test: proportion of bootstrap stats at least as extreme
    p_value <- mean(abs(valid_stats) >= abs(observed_stat))
  } else if (alternative == "greater") {
    # One-sided test: proportion of bootstrap stats >= observed
    p_value <- mean(valid_stats >= observed_stat)
  } else { # alternative == "less"
    # One-sided test: proportion of bootstrap stats <= observed
    p_value <- mean(valid_stats <= observed_stat)
  }

  # Return results
  list(
    p.value = p_value,
    bootstrap_stats = valid_stats,
    observed_stat = observed_stat,
    n_bootstraps = n_valid,
    alternative = alternative
  )
}

#' Permutation test for hypothesis testing
#'
#' Performs a permutation test to assess the significance of a test statistic
#' by comparing it to the distribution of the statistic under random
#' permutations of the data.
#'
#' @param x First sample or full data vector
#' @param y Second sample (for two-sample tests) or NULL (for one-sample tests)
#' @param test_statistic Function that computes test statistic from data
#' @param n_permutations Number of permutations (default: 10000)
#' @param alternative Character string specifying the alternative hypothesis
#' @param progress Logical; show progress bar (default: TRUE)
#' @param seed Random seed for reproducibility (default: NULL)
#'
#' @return A list containing:
#'   \item{p.value}{The permutation p-value}
#'   \item{permutation_stats}{Vector of permutation test statistics}
#'   \item{observed_stat}{The observed test statistic}
#'   \item{n_permutations}{Number of permutations used}
#'
#' @examples
#' \dontrun{
#' # Two-sample test
#' x <- rnorm(50, mean = 0)
#' y <- rnorm(50, mean = 0.5)
#' stat_func <- function(x, y) mean(x) - mean(y)
#' result <- permutation_test(x, y, stat_func)
#' }
#'
#' @export
permutation_test <- function(x, y = NULL, test_statistic,
                             n_permutations = 10000,
                             alternative = "two.sided",
                             progress = TRUE, seed = NULL) {
  # Input validation
  if (!is.numeric(x)) {
    stop("'x' must be numeric")
  }

  two_sample <- !is.null(y)

  if (two_sample && !is.numeric(y)) {
    stop("'y' must be numeric when provided")
  }

  if (!is.function(test_statistic)) {
    stop("'test_statistic' must be a function")
  }

  if (!is.numeric(n_permutations) || n_permutations < 100) {
    stop("'n_permutations' must be a numeric value >= 100")
  }

  alternative <- match.arg(alternative, c("two.sided", "less", "greater"))

  # Set seed if provided
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Calculate observed statistic
  if (two_sample) {
    observed_stat <- test_statistic(x, y)
    n_total <- length(x) + length(y)
    n_x <- length(x)
    combined <- c(x, y)
  } else {
    observed_stat <- test_statistic(x)
    n_total <- length(x)
  }

  if (!is.numeric(observed_stat) || length(observed_stat) != 1) {
    stop("Test statistic must return a single numeric value")
  }

  permutation_stats <- numeric(n_permutations)

  # Progress bar setup
  if (progress && n_permutations >= 1000) {
    pb <- txtProgressBar(min = 0, max = n_permutations, style = 3)
  }

  # Perform permutations
  for (i in seq_len(n_permutations)) {
    if (two_sample) {
      # Permute combined data
      perm_indices <- sample(n_total)
      perm_x <- combined[perm_indices[1:n_x]]
      perm_y <- combined[perm_indices[(n_x + 1):n_total]]

      tryCatch(
        {
          permutation_stats[i] <- test_statistic(perm_x, perm_y)
        },
        error = function(e) {
          warning(sprintf("Error in permutation %d: %s", i, e$message))
          permutation_stats[i] <- NA
        }
      )
    } else {
      # One-sample: permute signs or order
      perm_x <- sample(x)

      tryCatch(
        {
          permutation_stats[i] <- test_statistic(perm_x)
        },
        error = function(e) {
          warning(sprintf("Error in permutation %d: %s", i, e$message))
          permutation_stats[i] <- NA
        }
      )
    }

    # Update progress
    if (progress && n_permutations >= 1000 && i %% 100 == 0) {
      setTxtProgressBar(pb, i)
    }
  }

  # Close progress bar
  if (progress && n_permutations >= 1000) {
    close(pb)
  }

  # Remove any NA values
  valid_stats <- permutation_stats[!is.na(permutation_stats)]
  n_valid <- length(valid_stats)

  if (n_valid < n_permutations * 0.9) {
    warning(sprintf("Only %d of %d permutations were valid", n_valid, n_permutations))
  }

  # Calculate p-value
  if (alternative == "two.sided") {
    p_value <- mean(abs(valid_stats) >= abs(observed_stat))
  } else if (alternative == "greater") {
    p_value <- mean(valid_stats >= observed_stat)
  } else { # alternative == "less"
    p_value <- mean(valid_stats <= observed_stat)
  }

  # Return results
  list(
    p.value = p_value,
    permutation_stats = valid_stats,
    observed_stat = observed_stat,
    n_permutations = n_valid,
    alternative = alternative
  )
}

#' Monte Carlo p-value calculation
#'
#' Estimates p-values by simulating the null distribution of a test statistic
#' using Monte Carlo methods. This is useful when the null distribution can
#' be simulated but not analytically derived.
#'
#' @param observed_stat The observed test statistic
#' @param null_simulator Function that generates data under null hypothesis
#' @param test_statistic Function that computes test statistic from simulated data
#' @param n_simulations Number of Monte Carlo simulations (default: 10000)
#' @param alternative Character string specifying the alternative hypothesis
#' @param progress Logical; show progress bar (default: TRUE)
#' @param seed Random seed for reproducibility (default: NULL)
#' @param ... Additional arguments passed to null_simulator
#'
#' @return A list containing:
#'   \item{p.value}{The Monte Carlo p-value}
#'   \item{simulated_stats}{Vector of simulated test statistics}
#'   \item{observed_stat}{The observed test statistic}
#'   \item{n_simulations}{Number of simulations used}
#'
#' @examples
#' \dontrun{
#' # Test for correlation
#' observed_cor <- cor(x, y)
#' null_sim <- function(n) {
#'   list(x = rnorm(n), y = rnorm(n))
#' }
#' stat_func <- function(data) cor(data$x, data$y)
#' result <- monte_carlo_p_value(observed_cor, null_sim, stat_func, n = 100)
#' }
#'
#' @export
monte_carlo_p_value <- function(observed_stat, null_simulator, test_statistic,
                                n_simulations = 10000,
                                alternative = "two.sided",
                                progress = TRUE, seed = NULL, ...) {
  # Input validation
  if (!is.numeric(observed_stat) || length(observed_stat) != 1) {
    stop("'observed_stat' must be a single numeric value")
  }

  if (!is.function(null_simulator)) {
    stop("'null_simulator' must be a function")
  }

  if (!is.function(test_statistic)) {
    stop("'test_statistic' must be a function")
  }

  if (!is.numeric(n_simulations) || n_simulations < 100) {
    stop("'n_simulations' must be a numeric value >= 100")
  }

  alternative <- match.arg(alternative, c("two.sided", "less", "greater"))

  # Set seed if provided
  if (!is.null(seed)) {
    set.seed(seed)
  }

  simulated_stats <- numeric(n_simulations)

  # Progress bar setup
  if (progress && n_simulations >= 1000) {
    pb <- txtProgressBar(min = 0, max = n_simulations, style = 3)
  }

  # Perform simulations
  for (i in seq_len(n_simulations)) {
    # Generate data under null hypothesis
    tryCatch(
      {
        null_data <- null_simulator(...)
        simulated_stats[i] <- test_statistic(null_data)
      },
      error = function(e) {
        warning(sprintf("Error in simulation %d: %s", i, e$message))
        simulated_stats[i] <- NA
      }
    )

    # Update progress
    if (progress && n_simulations >= 1000 && i %% 100 == 0) {
      setTxtProgressBar(pb, i)
    }
  }

  # Close progress bar
  if (progress && n_simulations >= 1000) {
    close(pb)
  }

  # Remove any NA values
  valid_stats <- simulated_stats[!is.na(simulated_stats)]
  n_valid <- length(valid_stats)

  if (n_valid < n_simulations * 0.9) {
    warning(sprintf("Only %d of %d simulations were valid", n_valid, n_simulations))
  }

  if (n_valid == 0) {
    stop("All simulations failed. Check null_simulator and test_statistic functions.")
  }

  # Calculate p-value
  if (alternative == "two.sided") {
    p_value <- mean(abs(valid_stats) >= abs(observed_stat))
  } else if (alternative == "greater") {
    p_value <- mean(valid_stats >= observed_stat)
  } else { # alternative == "less"
    p_value <- mean(valid_stats <= observed_stat)
  }

  # Return results
  list(
    p.value = p_value,
    simulated_stats = valid_stats,
    observed_stat = observed_stat,
    n_simulations = n_valid,
    alternative = alternative
  )
}

#' Bootstrap confidence interval
#'
#' Calculates bootstrap confidence intervals using the percentile method.
#' This function is a utility that can be used alongside p-value calculations.
#'
#' @param data Numeric vector of observations
#' @param statistic Function that computes the statistic of interest
#' @param n_bootstraps Number of bootstrap samples (default: 10000)
#' @param conf_level Confidence level (default: 0.95)
#' @param progress Logical; show progress bar (default: TRUE)
#' @param seed Random seed for reproducibility (default: NULL)
#'
#' @return A list containing:
#'   \item{estimate}{Point estimate of the statistic}
#'   \item{conf_int}{Confidence interval as a numeric vector of length 2}
#'   \item{conf_level}{The confidence level used}
#'   \item{bootstrap_distribution}{Vector of bootstrap statistics}
#'
#' @export
bootstrap_conf_interval <- function(data, statistic, n_bootstraps = 10000,
                                    conf_level = 0.95, progress = TRUE,
                                    seed = NULL) {
  # Input validation
  if (!is.numeric(data) || length(data) < 2) {
    stop("'data' must be a numeric vector with at least 2 elements")
  }

  if (!is.function(statistic)) {
    stop("'statistic' must be a function")
  }

  if (!is.numeric(conf_level) || conf_level <= 0 || conf_level >= 1) {
    stop("'conf_level' must be between 0 and 1")
  }

  # Calculate point estimate
  estimate <- statistic(data)

  # Get bootstrap distribution
  boot_result <- bootstrap_p_value(data, statistic,
    observed_stat = estimate,
    n_bootstraps = n_bootstraps,
    progress = progress, seed = seed
  )

  # Calculate confidence interval
  alpha <- 1 - conf_level
  lower_quantile <- alpha / 2
  upper_quantile <- 1 - alpha / 2

  conf_int <- quantile(boot_result$bootstrap_stats,
    probs = c(lower_quantile, upper_quantile),
    na.rm = TRUE
  )

  list(
    estimate = estimate,
    conf_int = as.numeric(conf_int),
    conf_level = conf_level,
    bootstrap_distribution = boot_result$bootstrap_stats
  )
}
