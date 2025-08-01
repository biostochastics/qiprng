# File: multidim_reftools_compat.R
# ----------------------------------------------------------------------
#' RefTools Compatibility Layer for Multidimensional Tests
#'
#' This module provides compatibility functions to ensure multidimensional
#' tests work with refTools package if available.

# Check if refTools is available
has_reftools <- requireNamespace("refTools", quietly = TRUE)

#' Enhanced distance calculation using refTools if available
#' @export
calculate_distances_enhanced <- function(points, metric = "euclidean", 
                                       use_kdtree = TRUE, k_nearest = 1,
                                       use_reftools = NULL) {
  
  # Auto-detect refTools usage
  if (is.null(use_reftools)) {
    use_reftools <- has_reftools && nrow(points) > 1000
  }
  
  if (use_reftools && has_reftools) {
    # Use refTools for large-scale distance calculations
    if (metric == "euclidean") {
      # refTools::fastdist for efficient distance calculation
      dist_matrix <- refTools::fastdist(points, method = "euclidean")
      
      n <- nrow(points)
      distances <- matrix(NA, n, k_nearest)
      indices <- matrix(NA, n, k_nearest)
      
      for (i in 1:n) {
        dists <- dist_matrix[i, -i]
        sorted_idx <- order(dists)[1:k_nearest]
        distances[i, ] <- dists[sorted_idx]
        indices[i, ] <- ifelse(sorted_idx >= i, sorted_idx + 1, sorted_idx)
      }
      
      return(list(
        distances = distances,
        indices = indices,
        method = "reftools"
      ))
    }
  }
  
  # Fallback to original implementation
  calculate_distances(points, metric, use_kdtree, k_nearest)
}

#' Enhanced grid uniformity test using refTools
#' @export
grid_uniformity_test_enhanced <- function(points, grid_resolution = NULL, 
                                        config = NULL, use_reftools = NULL) {
  
  if (is.null(use_reftools)) {
    use_reftools <- has_reftools
  }
  
  if (use_reftools && has_reftools && requireNamespace("refTools", quietly = TRUE)) {
    # Use refTools for optimized grid operations
    n <- nrow(points)
    k <- ncol(points)
    
    if (is.null(config)) {
      config <- multidim_config(dimensions = k)
    }
    
    if (is.null(grid_resolution)) {
      grid_resolution <- config$grid_resolution
    }
    
    # Use refTools::discretize for efficient grid assignment
    grid_indices <- refTools::discretize(points, bins = grid_resolution)
    
    # Count cells
    cell_counts <- table(grid_indices)
    total_cells <- grid_resolution^k
    
    # Ensure all cells are represented
    all_cells <- 1:total_cells
    cell_counts_full <- rep(0, total_cells)
    cell_counts_full[as.numeric(names(cell_counts))] <- as.vector(cell_counts)
    
    # Expected count per cell
    expected_count <- n / total_cells
    
    # Chi-square test
    chi_sq_stat <- sum((cell_counts_full - expected_count)^2 / expected_count)
    df <- total_cells - 1
    p_value <- pchisq(chi_sq_stat, df, lower.tail = FALSE)
    
    # Additional statistics
    empty_cells <- sum(cell_counts_full == 0)
    max_count <- max(cell_counts_full)
    
    return(list(
      statistic = chi_sq_stat,
      p_value = p_value,
      df = df,
      total_cells = total_cells,
      empty_cells = empty_cells,
      max_count = max_count,
      expected_count = expected_count,
      grid_resolution = grid_resolution,
      result = if (is.na(p_value)) {
      "INCONCLUSIVE"
    } else if (p_value >= config$significance_level) {
      "PASS"
    } else {
      "FAIL"
    },
      details = sprintf("Grid: %d^%d, Empty cells: %d (%.1f%%) [refTools]", 
                       grid_resolution, k, empty_cells, 
                       100 * empty_cells / total_cells),
      method = "reftools"
    ))
  }
  
  # Fallback to original implementation
  grid_uniformity_test(points, grid_resolution, config)
}

#' Run multidimensional tests with refTools enhancements
#' @export
run_multidim_tests_reftools <- function(suite, config = NULL) {
  
  if (!has_reftools) {
    # Use standard implementation
    return(run_multidim_tests(suite, config))
  }
  
  if (is.null(config)) {
    config <- multidim_config()
  }
  
  # Generate multi-dimensional points
  points <- generate_multidim_points(config$sample_size, 
                                   config$dimensions,
                                   suite$prng_func)
  
  # Store points for visualization
  suite$multidim_data <- points
  
  # Initialize results
  suite$results$multidimensional <- list()
  
  # 1. Enhanced grid uniformity test
  grid_result <- grid_uniformity_test_enhanced(points, config = config)
  suite$results$multidimensional$grid_uniformity <- list(
    description = sprintf("Grid-based Uniformity Test (%dD) [Enhanced]", config$dimensions),
    result = grid_result$result,
    p_value = grid_result$p_value,
    statistic = grid_result$statistic,
    details = grid_result$details
  )
  
  # 2. Enhanced nearest neighbor test
  nn_result <- nearest_neighbor_test_enhanced(points, config = config)
  suite$results$multidimensional$nearest_neighbor <- list(
    description = sprintf("Nearest Neighbor Test (%dD) [Enhanced]", config$dimensions),
    result = nn_result$result,
    p_value = nn_result$p_value,
    statistic = nn_result$statistic,
    details = nn_result$details
  )
  
  # 3. Standard Ripley's K function test (or enhanced if available)
  ripley_result <- if (exists("ripleys_k_test_enhanced")) {
    ripleys_k_test_enhanced(points, config = config)
  } else {
    ripleys_k_test(points, config = config)
  }
  
  suite$results$multidimensional$ripleys_k <- list(
    description = sprintf("Ripley's K Function Test (%dD)", config$dimensions),
    result = ripley_result$result,
    p_value = ripley_result$p_value,
    statistic = ripley_result$statistic,
    details = ripley_result$details
  )
  
  # Store detailed results
  suite$multidim_results <- list(
    grid = grid_result,
    nearest_neighbor = nn_result,
    ripleys_k = ripley_result
  )
  
  return(suite)
}

#' Enhanced nearest neighbor test with refTools
nearest_neighbor_test_enhanced <- function(points, k_nearest = 1, config = NULL) {
  
  if (is.null(config)) {
    config <- multidim_config(dimensions = ncol(points))
  }
  
  n <- nrow(points)
  dims <- ncol(points)
  
  # Calculate nearest neighbor distances with enhancement
  dist_result <- calculate_distances_enhanced(points, 
                                            metric = config$distance_metric,
                                            use_kdtree = config$use_kdtree,
                                            k_nearest = k_nearest)
  
  nn_distances <- dist_result$distances[, 1]
  
  # Expected distribution calculations (same as original)
  if (dims == 2) {
    expected_mean <- 0.5 / sqrt(n)
  } else {
    expected_mean <- gamma(1 + 1/dims) / (n^(1/dims))
  }
  
  observed_mean <- mean(nn_distances)
  se <- sd(nn_distances) / sqrt(n)
  z_stat <- (observed_mean - expected_mean) / se
  p_value <- 2 * pnorm(-abs(z_stat))
  cv <- sd(nn_distances) / mean(nn_distances)
  
  method_desc <- if (dist_result$method == "reftools") "[refTools]" else ""
  
  return(list(
    statistic = z_stat,
    p_value = p_value,
    observed_mean = observed_mean,
    expected_mean = expected_mean,
    cv = cv,
    min_distance = min(nn_distances),
    max_distance = max(nn_distances),
    method = dist_result$method,
    result = if (is.na(p_value)) {
      "INCONCLUSIVE"
    } else if (p_value >= config$significance_level) {
      "PASS"
    } else {
      "FAIL"
    },
    details = sprintf("Mean NN dist: %.4f (expected: %.4f), CV: %.3f %s", 
                     observed_mean, expected_mean, cv, method_desc)
  ))
}

#' Check refTools availability and version
#' @export
check_reftools_status <- function() {
  if (has_reftools) {
    version <- packageVersion("refTools")
    cat("refTools is available (version", as.character(version), ")\n")
    cat("Enhanced multidimensional tests are enabled.\n")
    
    # Check specific functions
    funcs <- c("fastdist", "discretize")
    available <- sapply(funcs, function(f) {
      exists(f, where = asNamespace("refTools"))
    })
    
    cat("\nAvailable refTools functions:\n")
    for (i in seq_along(funcs)) {
      cat(sprintf("  %s: %s\n", funcs[i], 
                  ifelse(available[i], "YES", "NO")))
    }
  } else {
    cat("refTools is not available.\n")
    cat("Using standard implementations for multidimensional tests.\n")
    cat("\nTo enable enhanced features, install refTools:\n")
    cat("  install.packages('refTools')\n")
  }
}

#' Enable/disable refTools usage
#' @export
use_reftools_for_multidim <- function(enable = TRUE) {
  options(qiprng.use_reftools = enable)
  
  if (enable && !has_reftools) {
    warning("refTools is not available. Install it to use enhanced features.")
    return(FALSE)
  }
  
  cat(sprintf("refTools usage for multidimensional tests: %s\n",
              ifelse(enable, "ENABLED", "DISABLED")))
  return(enable)
}
