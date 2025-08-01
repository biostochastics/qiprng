# File: effect_sizes.R
# ----------------------------------------------------------------------
#' Effect Size Calculations for Statistical Tests
#'
#' This module provides functions to calculate and interpret effect sizes
#' for various statistical tests used in the qiprng package.

#' Calculate Cramér's V for chi-squared tests
#' 
#' @param chi_squared The chi-squared statistic
#' @param n Sample size
#' @param df Degrees of freedom (min(rows-1, cols-1))
#' @return Cramér's V effect size
#' @export
calculate_cramers_v <- function(chi_squared, n, df) {
  if (is.na(chi_squared) || is.na(n) || n == 0 || df == 0) {
    return(NA)
  }
  
  # Cramér's V = sqrt(chi^2 / (n * df))
  # For goodness-of-fit tests, df = k - 1 where k is number of categories
  v <- sqrt(chi_squared / (n * df))
  
  # Ensure V is between 0 and 1
  return(min(1, max(0, v)))
}

#' Calculate Cohen's d for mean comparisons
#' 
#' @param observed_mean Observed mean
#' @param expected_mean Expected mean under null hypothesis
#' @param sd Standard deviation (pooled or theoretical)
#' @return Cohen's d effect size
#' @export
calculate_cohens_d <- function(observed_mean, expected_mean, sd) {
  if (is.na(observed_mean) || is.na(expected_mean) || is.na(sd) || sd == 0) {
    return(NA)
  }
  
  # Cohen's d = (mean1 - mean2) / sd
  d <- abs(observed_mean - expected_mean) / sd
  
  return(d)
}

#' Calculate Cohen's h for proportion comparisons
#' 
#' @param p1 First proportion
#' @param p2 Second proportion (or expected proportion)
#' @return Cohen's h effect size
#' @export
calculate_cohens_h <- function(p1, p2) {
  if (is.na(p1) || is.na(p2) || p1 < 0 || p1 > 1 || p2 < 0 || p2 > 1) {
    return(NA)
  }
  
  # Cohen's h = 2 * arcsin(sqrt(p1)) - 2 * arcsin(sqrt(p2))
  h <- abs(2 * asin(sqrt(p1)) - 2 * asin(sqrt(p2)))
  
  return(h)
}

#' Calculate variance ratio effect size
#' 
#' @param observed_var Observed variance
#' @param expected_var Expected variance under null hypothesis
#' @return Variance ratio (log scale for symmetry)
#' @export
calculate_variance_ratio <- function(observed_var, expected_var) {
  if (is.na(observed_var) || is.na(expected_var) || 
      observed_var <= 0 || expected_var <= 0) {
    return(NA)
  }
  
  # Use log ratio for symmetry around 0
  # Positive values = more variance than expected
  # Negative values = less variance than expected
  log_ratio <- log(observed_var / expected_var)
  
  return(abs(log_ratio))
}

#' Calculate standardized range effect size
#' 
#' @param observed Value observed (min or max)
#' @param expected Expected value under null hypothesis
#' @param sd Standard deviation of the distribution
#' @return Standardized effect size
#' @export
calculate_standardized_range <- function(observed, expected, sd) {
  if (is.na(observed) || is.na(expected) || is.na(sd) || sd == 0) {
    return(NA)
  }
  
  # Standardized difference
  z <- abs(observed - expected) / sd
  
  return(z)
}

#' Interpret effect size based on conventional thresholds
#' 
#' @param effect_size The calculated effect size
#' @param type Type of effect size ("d", "v", "h", "r", "variance_ratio", "ks")
#' @param df Degrees of freedom (needed for Cramér's V interpretation)
#' @return Character string interpretation
#' @export
interpret_effect_size <- function(effect_size, type = "d", df = NULL) {
  if (is.na(effect_size)) {
    return(NA)
  }
  
  interpretation <- switch(type,
    "d" = {
      # Cohen's d thresholds
      if (effect_size < 0.2) "negligible"
      else if (effect_size < 0.5) "small"
      else if (effect_size < 0.8) "medium"
      else "large"
    },
    
    "v" = {
      # Cramér's V thresholds depend on df
      if (is.null(df)) df <- 1
      
      if (df == 1) {
        # For 2x2 tables
        if (effect_size < 0.1) "negligible"
        else if (effect_size < 0.3) "small"
        else if (effect_size < 0.5) "medium"
        else "large"
      } else if (df == 2) {
        # For 3x2 or 2x3 tables
        if (effect_size < 0.07) "negligible"
        else if (effect_size < 0.21) "small"
        else if (effect_size < 0.35) "medium"
        else "large"
      } else {
        # For larger tables
        if (effect_size < 0.05) "negligible"
        else if (effect_size < 0.15) "small"
        else if (effect_size < 0.25) "medium"
        else "large"
      }
    },
    
    "h" = {
      # Cohen's h thresholds
      if (effect_size < 0.2) "negligible"
      else if (effect_size < 0.5) "small"
      else if (effect_size < 0.8) "medium"
      else "large"
    },
    
    "r" = {
      # Correlation coefficient thresholds
      if (abs(effect_size) < 0.1) "negligible"
      else if (abs(effect_size) < 0.3) "small"
      else if (abs(effect_size) < 0.5) "medium"
      else "large"
    },
    
    "variance_ratio" = {
      # Log variance ratio thresholds
      if (effect_size < 0.1) "negligible"
      else if (effect_size < 0.3) "small"
      else if (effect_size < 0.7) "medium"
      else "large"
    },
    
    "ks" = {
      # Kolmogorov-Smirnov D statistic
      # These are rough guidelines as KS interpretation depends on sample size
      if (effect_size < 0.05) "negligible"
      else if (effect_size < 0.10) "small"
      else if (effect_size < 0.20) "medium"
      else "large"
    },
    
    # Default
    {
      if (effect_size < 0.2) "small"
      else if (effect_size < 0.5) "medium"
      else "large"
    }
  )
  
  return(interpretation)
}

#' Add effect size to test result
#' 
#' Helper function to add effect size and interpretation to a test result list
#' 
#' @param test_result The test result list
#' @param effect_size The calculated effect size
#' @param effect_type Type of effect size for interpretation
#' @param df Degrees of freedom (if needed)
#' @return Updated test result with effect size information
#' @export
add_effect_size <- function(test_result, effect_size, effect_type, df = NULL) {
  test_result$effect_size <- effect_size
  test_result$effect_size_interpretation <- interpret_effect_size(
    effect_size, 
    type = effect_type, 
    df = df
  )
  
  return(test_result)
}