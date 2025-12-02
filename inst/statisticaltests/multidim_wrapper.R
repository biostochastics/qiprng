# File: multidim_wrapper.R
# ----------------------------------------------------------------------
#' Wrapper functions for running multidimensional tests
#'
#' This module provides high-level wrapper functions to run comprehensive
#' multidimensional tests on PRNGs with various configurations.

#' Run comprehensive multidimensional tests
#' @export
run_multidimensional_tests <- function(suite, dimensions = c(2, 3),
                                       apply_fixes = TRUE,
                                       run_specific = TRUE) {
  # Apply fixes if requested
  if (apply_fixes) {
    apply_multidim_test_fixes()
  }

  # Store original suite state
  original_results <- suite$results

  # Run tests for each dimension
  for (d in dimensions) {
    cat(sprintf("\n=== Running %dD Tests ===\n", d))

    # Configure for current dimension
    config <- multidim_config(
      dimensions = d,
      sample_size = suite$config$multidim_sample_size %||% 10000,
      use_kdtree = TRUE,
      significance_level = suite$config$significance_level %||% 0.05
    )

    # Run basic multidimensional tests
    suite <- run_multidim_tests(suite, config)

    # Run specific tests if requested
    if (run_specific) {
      suite <- run_specific_multidim_tests(suite, dimensions = d)
    }

    # Visualize if enabled
    if (suite$config$visualize %||% FALSE) {
      suite <- visualize_multidim_results(suite)
    }
  }

  # Restore original test functions if fixes were applied
  if (apply_fixes) {
    restore_original_multidim_tests()
  }

  return(suite)
}

#' Run multidimensional tests with bootstrap validation
#' @export
run_multidim_tests_bootstrap <- function(suite, dimensions = 2,
                                         n_bootstrap = 100,
                                         tests = NULL) {
  if (is.null(tests)) {
    tests <- c(
      "grid_uniformity", "nearest_neighbor", "ripleys_k",
      "minimum_distance", "convex_hull"
    )
  }

  # Configure
  config <- multidim_config(
    dimensions = dimensions,
    sample_size = suite$config$multidim_sample_size %||% 10000
  )

  # Generate points
  points <- generate_multidim_points(
    config$sample_size,
    config$dimensions,
    suite$prng_func
  )

  # Initialize results
  bootstrap_results <- list()

  # Run each test with bootstrap
  for (test_name in tests) {
    cat(sprintf("Running bootstrap for %s test...\n", test_name))

    test_func <- switch(test_name,
      grid_uniformity = grid_uniformity_test,
      nearest_neighbor = nearest_neighbor_test,
      ripleys_k = ripleys_k_test_fixed,
      minimum_distance = minimum_distance_test,
      convex_hull = convex_hull_volume_test,
      scatter_3d = if (dimensions == 3) scatter_test_3d_fixed else NULL
    )

    if (is.null(test_func)) next

    # Run test on original data
    if (test_name == "convex_hull") {
      original_result <- test_func(points, n_simulations = 100)
    } else if (test_name == "scatter_3d") {
      original_result <- test_func(points)
    } else {
      original_result <- test_func(points, config = config)
    }

    # Bootstrap
    boot_stats <- numeric(n_bootstrap)
    boot_pvals <- numeric(n_bootstrap)

    for (b in 1:n_bootstrap) {
      # Resample with replacement
      boot_idx <- sample(nrow(points), replace = TRUE)
      boot_points <- points[boot_idx, ]

      # Run test
      if (test_name == "convex_hull") {
        boot_result <- test_func(boot_points, n_simulations = 50)
      } else if (test_name == "scatter_3d") {
        boot_result <- test_func(boot_points)
      } else {
        boot_result <- test_func(boot_points, config = config)
      }

      boot_stats[b] <- boot_result$statistic
      boot_pvals[b] <- boot_result$p_value
    }

    # Calculate bootstrap CI
    ci_lower <- quantile(boot_stats, 0.025, na.rm = TRUE)
    ci_upper <- quantile(boot_stats, 0.975, na.rm = TRUE)

    bootstrap_results[[test_name]] <- list(
      original_statistic = original_result$statistic,
      original_pvalue = original_result$p_value,
      bootstrap_mean = mean(boot_stats, na.rm = TRUE),
      bootstrap_sd = sd(boot_stats, na.rm = TRUE),
      ci_95 = c(ci_lower, ci_upper),
      pvalue_distribution = boot_pvals,
      result = original_result$result
    )
  }

  # Store in suite
  suite$multidim_bootstrap <- bootstrap_results

  return(suite)
}

#' Compare multidimensional test results across generators
#' @export
compare_multidim_results <- function(results_list, test_names = NULL) {
  if (is.null(test_names)) {
    # Extract all available test names
    test_names <- unique(unlist(lapply(results_list, function(r) {
      names(r$results$multidimensional)
    })))
  }

  # Create comparison matrix
  generators <- names(results_list)
  comparison <- matrix(NA, length(generators), length(test_names))
  rownames(comparison) <- generators
  colnames(comparison) <- test_names

  # Fill comparison matrix with p-values
  pvalue_matrix <- comparison

  for (i in seq_along(generators)) {
    gen <- generators[i]
    results <- results_list[[gen]]$results$multidimensional

    for (j in seq_along(test_names)) {
      test <- test_names[j]
      if (!is.null(results[[test]])) {
        comparison[i, j] <- results[[test]]$result
        pvalue_matrix[i, j] <- results[[test]]$p_value
      }
    }
  }

  # Summary statistics
  pass_rates <- colMeans(comparison == "PASS", na.rm = TRUE)
  mean_pvalues <- colMeans(pvalue_matrix, na.rm = TRUE)

  # Identify problematic tests
  problematic_tests <- test_names[pass_rates < 0.5]

  return(list(
    comparison = comparison,
    pvalues = pvalue_matrix,
    pass_rates = pass_rates,
    mean_pvalues = mean_pvalues,
    problematic_tests = problematic_tests,
    summary = data.frame(
      test = test_names,
      pass_rate = pass_rates,
      mean_pvalue = mean_pvalues,
      status = ifelse(pass_rates >= 0.8, "Good",
        ifelse(pass_rates >= 0.5, "Warning", "Failed")
      )
    )
  ))
}

#' Run quick multidimensional uniformity check
#' @export
quick_multidim_check <- function(prng_func, n = 1000, dimensions = 2) {
  # Generate points
  points <- generate_multidim_points(n, dimensions, prng_func)

  # Quick visual check for 2D
  if (dimensions == 2) {
    plot(points[, 1], points[, 2],
      main = "2D Uniformity Check",
      xlab = "Dimension 1", ylab = "Dimension 2",
      pch = 19, cex = 0.5, col = rgb(0, 0, 0, 0.5)
    )

    # Add grid
    abline(h = seq(0, 1, 0.1), col = "gray80", lty = 2)
    abline(v = seq(0, 1, 0.1), col = "gray80", lty = 2)
  }

  # Run basic tests
  config <- multidim_config(dimensions = dimensions, sample_size = n)

  grid_result <- grid_uniformity_test(points, config = config)
  nn_result <- nearest_neighbor_test(points, config = config)

  # Summary
  cat("\nQuick Multidimensional Check Results:\n")
  cat(sprintf("Dimensions: %d, Sample size: %d\n", dimensions, n))
  cat(sprintf(
    "Grid uniformity: %s (p = %.4f)\n",
    grid_result$result, grid_result$p_value
  ))
  cat(sprintf(
    "Nearest neighbor: %s (p = %.4f)\n",
    nn_result$result, nn_result$p_value
  ))
  cat(sprintf("Grid details: %s\n", grid_result$details))
  cat(sprintf("NN details: %s\n", nn_result$details))

  invisible(list(grid = grid_result, nn = nn_result))
}

# Null operator if not defined
if (!exists("%||%")) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
}
