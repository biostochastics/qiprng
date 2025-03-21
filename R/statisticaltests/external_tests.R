# File: external_tests.R
# ----------------------------------------------------------------------
#' External tests for PRNG quality
#'
#' This module provides wrappers for tests from external packages
#' to evaluate randomness quality.

#' Run external package tests for randomness
#'
#' @param suite The test suite object
#' @return Updated test suite with results
#' @export
run_external_tests <- function(suite) {
  # Generate random numbers
  n <- suite$config$external_sample_size
  x <- suite$prng_func(n)
  
  # Initialize results
  suite$results$external <- list()
  
  # 1. Tests from randtests package
  if (requireNamespace("randtests", quietly = TRUE)) {
    # Try to run Bartels rank test
    tryCatch({
      bartels_test <- randtests::bartels.rank.test(x)
      suite$results$external$bartels_rank <- list(
        description = "Bartels Rank Test",
        result = ifelse(bartels_test$p.value >= suite$config$significance_level, "PASS", "FAIL"),
        p_value = bartels_test$p.value,
        statistic = bartels_test$statistic,
        details = "Tests for randomness using Bartels rank method"
      )
    }, error = function(e) {
      suite$results$external$bartels_rank <- list(
        description = "Bartels Rank Test",
        result = "ERROR",
        details = paste("Test failed with error:", e$message)
      )
    })
    
    # Try to run Cox-Stuart test
    tryCatch({
      cox_stuart_test <- randtests::cox.stuart.test(x)
      suite$results$external$cox_stuart <- list(
        description = "Cox-Stuart Test",
        result = ifelse(cox_stuart_test$p.value >= suite$config$significance_level, "PASS", "FAIL"),
        p_value = cox_stuart_test$p.value,
        statistic = cox_stuart_test$statistic,
        details = "Tests for trend using Cox-Stuart method"
      )
    }, error = function(e) {
      suite$results$external$cox_stuart <- list(
        description = "Cox-Stuart Test",
        result = "ERROR",
        details = paste("Test failed with error:", e$message)
      )
    })
    
    # Try to run difference sign test
    tryCatch({
      diff_sign_test <- randtests::difference.sign.test(x)
      suite$results$external$difference_sign <- list(
        description = "Difference Sign Test",
        result = ifelse(diff_sign_test$p.value >= suite$config$significance_level, "PASS", "FAIL"),
        p_value = diff_sign_test$p.value,
        statistic = diff_sign_test$statistic,
        details = "Tests randomness using difference sign method"
      )
    }, error = function(e) {
      suite$results$external$difference_sign <- list(
        description = "Difference Sign Test",
        result = "ERROR",
        details = paste("Test failed with error:", e$message)
      )
    })
    
    # Try to run turning point test
    tryCatch({
      turning_point_test <- randtests::turning.point.test(x)
      suite$results$external$turning_point <- list(
        description = "Turning Point Test",
        result = ifelse(turning_point_test$p.value >= suite$config$significance_level, "PASS", "FAIL"),
        p_value = turning_point_test$p.value,
        statistic = turning_point_test$statistic,
        details = "Tests randomness using turning point method"
      )
    }, error = function(e) {
      suite$results$external$turning_point <- list(
        description = "Turning Point Test",
        result = "ERROR",
        details = paste("Test failed with error:", e$message)
      )
    })
    
    # Runs test (above and below median)
    tryCatch({
      runs_test <- randtests::runs.test(x)
      suite$results$external$runs_above_below <- list(
        description = "Runs Test (Above/Below Median)",
        result = ifelse(runs_test$p.value >= suite$config$significance_level, "PASS", "FAIL"),
        p_value = runs_test$p.value,
        statistic = runs_test$statistic,
        details = "Tests randomness using runs above and below median"
      )
    }, error = function(e) {
      suite$results$external$runs_above_below <- list(
        description = "Runs Test (Above/Below Median)",
        result = "ERROR",
        details = paste("Test failed with error:", e$message)
      )
    })
  } else {
    suite$results$external$package_missing <- list(
      description = "External Package Tests",
      result = "SKIPPED",
      details = "The 'randtests' package is not installed. Install it for additional randomness tests."
    )
  }
  
  # 2. Tests from nortest package
  if (requireNamespace("nortest", quietly = TRUE)) {
    # Anderson-Darling normality test on uniform-to-normal transformed data
    # Note: For uniformly distributed data, applying qnorm(uniform data) should give normal data
    tryCatch({
      normal_x <- stats::qnorm(x)
      ad_test <- nortest::ad.test(normal_x)
      suite$results$external$anderson_darling <- list(
        description = "Anderson-Darling Normality Test (on transformed data)",
        result = ifelse(ad_test$p.value >= suite$config$significance_level, "PASS", "FAIL"),
        p_value = ad_test$p.value,
        statistic = ad_test$statistic,
        details = paste("Tests if transformed data follows normal distribution.",
                        "Low p-value suggests non-uniformity in original data.")
      )
    }, error = function(e) {
      suite$results$external$anderson_darling <- list(
        description = "Anderson-Darling Normality Test",
        result = "ERROR",
        details = paste("Test failed with error:", e$message)
      )
    })
    
    # Lilliefors test on uniform-to-normal transformed data
    tryCatch({
      normal_x <- stats::qnorm(x)
      lillie_test <- nortest::lillie.test(normal_x)
      suite$results$external$lilliefors <- list(
        description = "Lilliefors Normality Test (on transformed data)",
        result = ifelse(lillie_test$p.value >= suite$config$significance_level, "PASS", "FAIL"),
        p_value = lillie_test$p.value,
        statistic = lillie_test$statistic,
        details = paste("Tests if transformed data follows normal distribution.",
                        "Low p-value suggests non-uniformity in original data.")
      )
    }, error = function(e) {
      suite$results$external$lilliefors <- list(
        description = "Lilliefors Normality Test",
        result = "ERROR",
        details = paste("Test failed with error:", e$message)
      )
    })
  } else {
    if (!exists("suite$results$external$package_missing")) {
      suite$results$external$package_missing <- list(
        description = "External Package Tests",
        result = "SKIPPED",
        details = "The 'nortest' package is not installed. Install it for additional normality tests."
      )
    } else {
      suite$results$external$package_missing$details <- paste(
        suite$results$external$package_missing$details,
        "The 'nortest' package is not installed. Install it for additional normality tests.",
        sep = " "
      )
    }
  }
  
  # 3. Tests from the base stats package
  
  # Kolmogorov-Smirnov test (comparing to uniform distribution)
  tryCatch({
    ks_test <- stats::ks.test(x, "punif")
    suite$results$external$kolmogorov_smirnov <- list(
      description = "Kolmogorov-Smirnov Test",
      result = ifelse(ks_test$p.value >= suite$config$significance_level, "PASS", "FAIL"),
      p_value = ks_test$p.value,
      statistic = ks_test$statistic,
      details = "Tests if data follows uniform distribution"
    )
  }, error = function(e) {
    suite$results$external$kolmogorov_smirnov <- list(
      description = "Kolmogorov-Smirnov Test",
      result = "ERROR",
      details = paste("Test failed with error:", e$message)
    )
  })
  
  # Chi-square test (dividing [0,1] into 100 equal bins)
  tryCatch({
    bins <- 100
    bin_breaks <- seq(0, 1, length.out = bins + 1)
    bin_counts <- table(cut(x, breaks = bin_breaks, include.lowest = TRUE))
    expected_counts <- rep(length(x) / bins, bins)
    
    chi_test <- stats::chisq.test(as.numeric(bin_counts), p = rep(1/bins, bins))
    suite$results$external$chi_square <- list(
      description = "Chi-Square Test",
      result = ifelse(chi_test$p.value >= suite$config$significance_level, "PASS", "FAIL"),
      p_value = chi_test$p.value,
      statistic = chi_test$statistic,
      details = paste("Tests uniformity using chi-square test with", bins, "bins")
    )
  }, error = function(e) {
    suite$results$external$chi_square <- list(
      description = "Chi-Square Test",
      result = "ERROR",
      details = paste("Test failed with error:", e$message)
    )
  })
  
  # Shapiro-Wilk normality test on uniform-to-normal transformed data
  tryCatch({
    # Limit sample size to avoid exceeding the max length for this test
    max_shapiro_n <- min(length(x), 5000)
    normal_x <- stats::qnorm(x[1:max_shapiro_n])
    shapiro_test <- stats::shapiro.test(normal_x)
    suite$results$external$shapiro_wilk <- list(
      description = "Shapiro-Wilk Normality Test (on transformed data)",
      result = ifelse(shapiro_test$p.value >= suite$config$significance_level, "PASS", "FAIL"),
      p_value = shapiro_test$p.value,
      statistic = shapiro_test$statistic,
      details = paste("Tests if transformed data follows normal distribution.",
                      "Low p-value suggests non-uniformity in original data.",
                      "Limited to first", max_shapiro_n, "samples due to test constraints.")
    )
  }, error = function(e) {
    suite$results$external$shapiro_wilk <- list(
      description = "Shapiro-Wilk Normality Test",
      result = "ERROR",
      details = paste("Test failed with error:", e$message)
    )
  })
  
  # Generate visualizations if requested
  if (suite$config$save_visualizations) {
    suite <- visualize_external_tests(suite, x)
  }
  
  return(suite)
}

#' Visualize external test results
#'
#' @param suite The test suite object
#' @param x The random number sample
#' @return Updated test suite with visualization paths
#' @keywords internal
visualize_external_tests <- function(suite, x) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    return(suite)
  }
  
  # To avoid R CMD check NOTEs
  Test <- PValue <- Result <- PassFail <- NULL
  
  # Set up output directory
  output_dir <- file.path(suite$config$output_dir, "visualizations", "external")
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # 1. Create summary plot of p-values from external tests
  if (length(suite$results$external) > 0) {
    test_results <- data.frame(
      Test = character(),
      PValue = numeric(),
      Result = character(),
      stringsAsFactors = FALSE
    )
    
    # Extract p-values from test results
    for (test_name in names(suite$results$external)) {
      test <- suite$results$external[[test_name]]
      if (!is.null(test$p_value)) {
        test_results <- rbind(test_results, data.frame(
          Test = test$description,
          PValue = test$p_value,
          Result = test$result,
          stringsAsFactors = FALSE
        ))
      }
    }
    
    if (nrow(test_results) > 0) {
      # Create p-value bar chart
      p1 <- ggplot2::ggplot(test_results, ggplot2::aes(x = Test, y = PValue, fill = Result)) +
        ggplot2::geom_bar(stat = "identity") +
        ggplot2::geom_hline(yintercept = suite$config$significance_level, 
                            linetype = "dashed", color = "red") +
        ggplot2::scale_fill_manual(values = c("PASS" = "green", "FAIL" = "red")) +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
        ggplot2::labs(
          title = "External Test Results",
          subtitle = paste("Red dashed line indicates significance level =", 
                           suite$config$significance_level),
          x = "Test",
          y = "P-Value"
        )
      
      # Save plot
      p_value_path <- file.path(output_dir, "external_tests_p_values.png")
      ggplot2::ggsave(p_value_path, p1, width = 10, height = 6)
      
      # Store visualization path
      suite$visualizations$external <- list(
        p_value_plot = p_value_path
      )
    }
  }
  
  # 2. Create a QQ plot of uniform-to-normal transformed data
  normal_x <- stats::qnorm(x)
  normal_df <- data.frame(Value = normal_x)
  
  p2 <- ggplot2::ggplot(normal_df, ggplot2::aes(sample = Value)) +
    ggplot2::stat_qq() +
    ggplot2::stat_qq_line() +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "QQ Plot of Transformed Data",
      subtitle = "Uniform data transformed to normal using qnorm",
      x = "Theoretical Quantiles",
      y = "Sample Quantiles"
    )
  
  # Save plot
  qq_path <- file.path(output_dir, "transformed_qq_plot.png")
  ggplot2::ggsave(qq_path, p2, width = 8, height = 6)
  
  # Add to visualizations list
  if (!is.null(suite$visualizations$external)) {
    suite$visualizations$external$qq_plot <- qq_path
  } else {
    suite$visualizations$external <- list(qq_plot = qq_path)
  }
  
  # 3. Create a histogram of uniform-to-normal transformed data
  p3 <- ggplot2::ggplot(normal_df, ggplot2::aes(x = Value)) +
    ggplot2::geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7) +
    ggplot2::stat_function(
      fun = function(x) dnorm(x) * length(normal_x) * (max(normal_x) - min(normal_x)) / 30,
      color = "red"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "Histogram of Transformed Data",
      subtitle = "Uniform data transformed to normal using qnorm (normal curve in red)",
      x = "Value",
      y = "Count"
    )
  
  # Save plot
  hist_path <- file.path(output_dir, "transformed_histogram.png")
  ggplot2::ggsave(hist_path, p3, width = 8, height = 6)
  
  # Add to visualizations list
  suite$visualizations$external$histogram <- hist_path
  
  return(suite)
}
