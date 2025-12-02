# File: multidim_tests.R
# ----------------------------------------------------------------------
#' Multi-dimensional Statistical Test Framework
#'
#' This module provides a framework for implementing multi-dimensional
#' statistical tests with efficient algorithms for k-dimensional data analysis.
#' Includes utilities for point generation, distance calculations, and
#' grid-based uniformity testing.

# Check for required packages
has_fnn <- requireNamespace("FNN", quietly = TRUE)
has_plotly <- requireNamespace("plotly", quietly = TRUE)
has_scatterplot3d <- requireNamespace("scatterplot3d", quietly = TRUE)

#' S3 class for multi-dimensional test configuration
#' @export
multidim_config <- function(dimensions = 2,
                            sample_size = 1000,
                            distance_metric = "euclidean",
                            grid_resolution = NULL,
                            use_kdtree = TRUE,
                            significance_level = 0.05) {
  # Auto-determine grid resolution if not specified
  if (is.null(grid_resolution)) {
    # Sturges' rule adapted for multi-dimensional data
    grid_resolution <- ceiling(log2(sample_size) + 1)
    # Adjust for dimensions to avoid too many cells
    grid_resolution <- max(3, min(grid_resolution, floor(sample_size^(1 / dimensions) / 2)))
  }

  structure(
    list(
      dimensions = dimensions,
      sample_size = sample_size,
      distance_metric = distance_metric,
      grid_resolution = grid_resolution,
      use_kdtree = use_kdtree && has_fnn,
      significance_level = significance_level
    ),
    class = "multidim_config"
  )
}

#' Generate k-dimensional points from PRNG
#' @export
generate_multidim_points <- function(n, k, prng_func = runif) {
  # Generate n points in k dimensions
  # Each row is a point, each column is a dimension
  matrix(prng_func(n * k), nrow = n, ncol = k)
}

#' Calculate distances between points
#' @export
calculate_distances <- function(points, metric = "euclidean",
                                use_kdtree = TRUE, k_nearest = 1) {
  n <- nrow(points)

  if (metric == "euclidean") {
    if (use_kdtree && has_fnn && n > 100) {
      # Use FNN for efficient k-d tree based nearest neighbor search
      nn_result <- FNN::get.knn(points, k = k_nearest)
      distances <- nn_result$nn.dist
      indices <- nn_result$nn.index

      return(list(
        distances = distances,
        indices = indices,
        method = "kdtree"
      ))
    } else {
      # Fallback to base R distance calculation
      dist_matrix <- as.matrix(dist(points, method = "euclidean"))

      # Get k nearest neighbors for each point
      distances <- matrix(NA, n, k_nearest)
      indices <- matrix(NA, n, k_nearest)

      for (i in 1:n) {
        # Exclude self-distance
        dists <- dist_matrix[i, -i]
        sorted_idx <- order(dists)[1:k_nearest]
        distances[i, ] <- dists[sorted_idx]
        # Adjust indices to account for removed self
        indices[i, ] <- ifelse(sorted_idx >= i, sorted_idx + 1, sorted_idx)
      }

      return(list(
        distances = distances,
        indices = indices,
        method = "base"
      ))
    }
  } else if (metric == "manhattan") {
    dist_matrix <- as.matrix(dist(points, method = "manhattan"))
  } else if (metric == "chebyshev") {
    # Maximum absolute difference
    dist_matrix <- matrix(0, n, n)
    for (i in 1:(n - 1)) {
      for (j in (i + 1):n) {
        dist_matrix[i, j] <- max(abs(points[i, ] - points[j, ]))
        dist_matrix[j, i] <- dist_matrix[i, j]
      }
    }
  } else {
    stop("Unsupported distance metric: ", metric)
  }

  # For non-Euclidean metrics, extract k nearest neighbors
  if (metric != "euclidean") {
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
      method = "base"
    ))
  }
}

#' Grid-based uniformity test
#' @export
grid_uniformity_test <- function(points, grid_resolution = NULL,
                                 config = NULL) {
  if (is.null(config)) {
    config <- multidim_config(dimensions = ncol(points))
  }

  if (is.null(grid_resolution)) {
    grid_resolution <- config$grid_resolution
  }

  n <- nrow(points)
  k <- ncol(points)

  # Create grid cells
  total_cells <- grid_resolution^k

  # Discretize points into grid cells
  cell_indices <- matrix(0, n, k)
  for (d in 1:k) {
    # Map [0,1] to grid indices
    cell_indices[, d] <- floor(points[, d] * grid_resolution) + 1
    # Handle edge case where point = 1
    cell_indices[cell_indices[, d] > grid_resolution, d] <- grid_resolution
  }

  # Convert multi-dimensional indices to single index
  cell_numbers <- rep(0, n)
  for (i in 1:n) {
    idx <- 0
    for (d in 1:k) {
      idx <- idx + (cell_indices[i, d] - 1) * grid_resolution^(d - 1)
    }
    cell_numbers[i] <- idx + 1
  }

  # Count points in each cell
  cell_counts <- table(factor(cell_numbers, levels = 1:total_cells))

  # Expected count per cell under uniformity
  expected_count <- n / total_cells

  # Chi-square test
  chi_sq_stat <- sum((cell_counts - expected_count)^2 / expected_count)
  df <- total_cells - 1
  p_value <- pchisq(chi_sq_stat, df, lower.tail = FALSE)

  # Additional statistics
  empty_cells <- sum(cell_counts == 0)
  max_count <- max(cell_counts)

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
    details = sprintf(
      "Grid: %d^%d, Empty cells: %d (%.1f%%)",
      grid_resolution, k, empty_cells,
      100 * empty_cells / total_cells
    )
  ))
}

#' Nearest neighbor uniformity test
#' @export
nearest_neighbor_test <- function(points, k_nearest = 1, config = NULL) {
  if (is.null(config)) {
    config <- multidim_config(dimensions = ncol(points))
  }

  n <- nrow(points)
  dims <- ncol(points)

  # Calculate nearest neighbor distances
  dist_result <- calculate_distances(points,
    metric = config$distance_metric,
    use_kdtree = config$use_kdtree,
    k_nearest = k_nearest
  )

  nn_distances <- dist_result$distances[, 1] # First nearest neighbor

  # Expected distribution of nearest neighbor distances
  # For uniform distribution in unit hypercube
  # Expected value of NN distance
  if (dims == 2) {
    # For 2D, theoretical mean ≈ 0.5 / sqrt(n)
    expected_mean <- 0.5 / sqrt(n)
  } else {
    # General approximation
    expected_mean <- gamma(1 + 1 / dims) / (n^(1 / dims))
  }

  # Empirical mean
  observed_mean <- mean(nn_distances)

  # Simple z-test (approximation)
  # Standard error estimation
  se <- sd(nn_distances) / sqrt(n)
  z_stat <- (observed_mean - expected_mean) / se
  p_value <- 2 * pnorm(-abs(z_stat))

  # Additional statistics
  cv <- sd(nn_distances) / mean(nn_distances) # Coefficient of variation

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
    details = sprintf(
      "Mean NN dist: %.4f (expected: %.4f), CV: %.3f",
      observed_mean, expected_mean, cv
    )
  ))
}

#' Ripley's K function test
#' @export
ripleys_k_test <- function(points, radii = NULL, config = NULL) {
  if (is.null(config)) {
    config <- multidim_config(dimensions = ncol(points))
  }

  n <- nrow(points)
  dims <- ncol(points)

  # Default radii if not specified
  if (is.null(radii)) {
    max_radius <- 0.5 # Half the unit hypercube
    radii <- seq(0.01, max_radius, length.out = 20)
  }

  # Calculate all pairwise distances
  if (config$use_kdtree && has_fnn && n > 100) {
    # For large datasets, use range queries
    k_values <- numeric(length(radii))

    for (i in seq_along(radii)) {
      r <- radii[i]
      # Count points within radius r of each point
      counts <- numeric(n)

      # This is approximated - full implementation would need range queries
      dist_result <- calculate_distances(points,
        metric = config$distance_metric,
        use_kdtree = TRUE,
        k_nearest = min(n - 1, 50)
      )

      for (j in 1:n) {
        counts[j] <- sum(dist_result$distances[j, ] <= r)
      }

      k_values[i] <- mean(counts)
    }
  } else {
    # Full pairwise calculation for smaller datasets
    dist_matrix <- as.matrix(dist(points))

    k_values <- numeric(length(radii))
    for (i in seq_along(radii)) {
      r <- radii[i]
      # Count pairs within distance r
      count <- sum(dist_matrix <= r & dist_matrix > 0) / 2
      k_values[i] <- count / n
    }
  }

  # Expected K function for uniform distribution
  if (dims == 2) {
    expected_k <- pi * radii^2 * (n - 1)
  } else if (dims == 3) {
    expected_k <- (4 / 3) * pi * radii^3 * (n - 1)
  } else {
    # General d-dimensional sphere volume
    expected_k <- (pi^(dims / 2) / gamma(dims / 2 + 1)) * radii^dims * (n - 1)
  }

  # Normalize
  expected_k <- expected_k / n

  # Kolmogorov-Smirnov test on K function
  ks_stat <- max(abs(k_values - expected_k))

  # Approximate p-value (would need permutation test for accuracy)
  # Using simplified approximation
  lambda <- sqrt(n) * ks_stat
  p_value <- 2 * exp(-2 * lambda^2)

  return(list(
    statistic = ks_stat,
    p_value = p_value,
    radii = radii,
    observed_k = k_values,
    expected_k = expected_k,
    result = if (is.na(p_value)) {
      "INCONCLUSIVE"
    } else if (p_value >= config$significance_level) {
      "PASS"
    } else {
      "FAIL"
    },
    details = sprintf(
      "Max deviation: %.4f at radius %.3f",
      ks_stat, radii[which.max(abs(k_values - expected_k))]
    )
  ))
}

#' Run multi-dimensional test suite
#' @export
run_multidim_tests <- function(suite, config = NULL) {
  if (is.null(config)) {
    config <- multidim_config()
  }

  # Generate multi-dimensional points
  points <- generate_multidim_points(
    config$sample_size,
    config$dimensions,
    suite$prng_func
  )

  # Store points for visualization
  suite$multidim_data <- points

  # Initialize results
  suite$results$multidimensional <- list()

  # 1. Grid uniformity test
  grid_result <- grid_uniformity_test(points, config = config)
  suite$results$multidimensional$grid_uniformity <- list(
    description = sprintf("Grid-based Uniformity Test (%dD)", config$dimensions),
    result = grid_result$result,
    p_value = grid_result$p_value,
    statistic = grid_result$statistic,
    details = grid_result$details
  )

  # 2. Nearest neighbor test
  nn_result <- nearest_neighbor_test(points, config = config)
  suite$results$multidimensional$nearest_neighbor <- list(
    description = sprintf("Nearest Neighbor Test (%dD)", config$dimensions),
    result = nn_result$result,
    p_value = nn_result$p_value,
    statistic = nn_result$statistic,
    details = nn_result$details
  )

  # 3. Ripley's K function test
  ripley_result <- ripleys_k_test(points, config = config)
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

#' Visualize 2D multi-dimensional test results
#' @export
visualize_2d_results <- function(points, test_results, output_dir = NULL) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    warning("ggplot2 not available for visualization")
    return(NULL)
  }

  plots <- list()

  # 1. Scatter plot with density
  df <- data.frame(x = points[, 1], y = points[, 2])

  p1 <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_point(alpha = 0.5, size = 1) +
    ggplot2::geom_density_2d(color = "blue", alpha = 0.5) +
    ggplot2::coord_fixed() +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "2D Point Distribution with Density Contours",
      x = "Dimension 1",
      y = "Dimension 2"
    )

  plots$scatter_density <- p1

  # 2. Grid cell occupancy heatmap
  if (!is.null(test_results$grid)) {
    grid_res <- test_results$grid$grid_resolution

    # Create grid
    grid_df <- expand.grid(
      x = seq(0, 1, length.out = grid_res + 1),
      y = seq(0, 1, length.out = grid_res + 1)
    )

    # Count points in each cell
    counts <- matrix(0, grid_res, grid_res)
    for (i in 1:nrow(points)) {
      x_idx <- min(grid_res, max(1, ceiling(points[i, 1] * grid_res)))
      y_idx <- min(grid_res, max(1, ceiling(points[i, 2] * grid_res)))
      counts[x_idx, y_idx] <- counts[x_idx, y_idx] + 1
    }

    # Convert to long format
    heatmap_df <- expand.grid(x = 1:grid_res, y = 1:grid_res)
    heatmap_df$count <- as.vector(counts)

    p2 <- ggplot2::ggplot(heatmap_df, ggplot2::aes(x = x, y = y, fill = count)) +
      ggplot2::geom_tile() +
      ggplot2::scale_fill_gradient(low = "white", high = "red") +
      ggplot2::coord_fixed() +
      ggplot2::theme_minimal() +
      ggplot2::labs(
        title = "Grid Cell Occupancy Heatmap",
        x = "Grid X",
        y = "Grid Y",
        fill = "Count"
      )

    plots$grid_heatmap <- p2
  }

  # 3. Nearest neighbor distances histogram
  if (!is.null(test_results$nearest_neighbor)) {
    # Recalculate for visualization
    dist_result <- calculate_distances(points, use_kdtree = TRUE)
    nn_distances <- dist_result$distances[, 1]

    nn_df <- data.frame(distance = nn_distances)
    expected_mean <- test_results$nearest_neighbor$expected_mean

    p3 <- ggplot2::ggplot(nn_df, ggplot2::aes(x = distance)) +
      ggplot2::geom_histogram(
        bins = 30, fill = "lightblue",
        color = "black", alpha = 0.7
      ) +
      ggplot2::geom_vline(
        xintercept = expected_mean,
        color = "red", linetype = "dashed", size = 1
      ) +
      ggplot2::theme_minimal() +
      ggplot2::labs(
        title = "Nearest Neighbor Distance Distribution",
        subtitle = sprintf("Red line: Expected mean (%.4f)", expected_mean),
        x = "Distance to Nearest Neighbor",
        y = "Frequency"
      )

    plots$nn_histogram <- p3
  }

  # Save plots if output directory specified
  if (!is.null(output_dir)) {
    for (name in names(plots)) {
      filename <- file.path(output_dir, paste0("multidim_2d_", name, ".png"))
      ggplot2::ggsave(filename, plots[[name]], width = 8, height = 6, dpi = 120)
    }
  }

  return(plots)
}

#' Visualize 3D multi-dimensional test results
#' @export
visualize_3d_results <- function(points, test_results, output_dir = NULL) {
  if (ncol(points) != 3) {
    warning("3D visualization requires exactly 3 dimensions")
    return(NULL)
  }

  plots <- list()

  # Use plotly for interactive 3D if available
  if (has_plotly) {
    p1 <- plotly::plot_ly(
      x = points[, 1],
      y = points[, 2],
      z = points[, 3],
      type = "scatter3d",
      mode = "markers",
      marker = list(
        size = 3,
        opacity = 0.6,
        color = points[, 3],
        colorscale = "Viridis"
      )
    ) %>%
      plotly::layout(
        title = "3D Point Distribution",
        scene = list(
          xaxis = list(title = "Dimension 1"),
          yaxis = list(title = "Dimension 2"),
          zaxis = list(title = "Dimension 3")
        )
      )

    plots$scatter3d <- p1

    # Save as HTML if output directory specified
    if (!is.null(output_dir)) {
      htmlwidgets::saveWidget(
        p1,
        file.path(output_dir, "multidim_3d_interactive.html"),
        selfcontained = TRUE
      )
    }
  } else if (has_scatterplot3d) {
    # Fallback to static 3D plot
    if (!is.null(output_dir)) {
      png(file.path(output_dir, "multidim_3d_static.png"),
        width = 800, height = 600
      )
    }

    scatterplot3d::scatterplot3d(
      points[, 1], points[, 2], points[, 3],
      color = "blue",
      pch = 16,
      main = "3D Point Distribution",
      xlab = "Dimension 1",
      ylab = "Dimension 2",
      zlab = "Dimension 3"
    )

    if (!is.null(output_dir)) {
      dev.off()
    }
  }

  return(plots)
}

#' Visualize multi-dimensional test results
#' @export
visualize_multidim_results <- function(suite) {
  if (is.null(suite$multidim_data) || is.null(suite$multidim_results)) {
    warning("No multi-dimensional test data available")
    return(NULL)
  }

  points <- suite$multidim_data
  dims <- ncol(points)

  output_dir <- NULL
  if (suite$config$save_visualizations) {
    output_dir <- file.path(suite$config$output_dir, "visualizations", "multidim")
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }
  }

  if (dims == 2) {
    plots <- visualize_2d_results(points, suite$multidim_results, output_dir)
  } else if (dims == 3) {
    plots <- visualize_3d_results(points, suite$multidim_results, output_dir)
  } else {
    # For higher dimensions, visualize first 2-3 principal components
    if (requireNamespace("ggplot2", quietly = TRUE)) {
      # PCA projection
      pca <- prcomp(points, scale. = TRUE)

      if (dims >= 2) {
        # 2D projection
        df <- data.frame(
          PC1 = pca$x[, 1],
          PC2 = pca$x[, 2]
        )

        p <- ggplot2::ggplot(df, ggplot2::aes(x = PC1, y = PC2)) +
          ggplot2::geom_point(alpha = 0.5) +
          ggplot2::theme_minimal() +
          ggplot2::labs(
            title = sprintf("%dD Data - First Two Principal Components", dims),
            subtitle = sprintf(
              "Explained variance: %.1f%%",
              sum(pca$sdev[1:2]^2) / sum(pca$sdev^2) * 100
            )
          )

        if (!is.null(output_dir)) {
          ggplot2::ggsave(file.path(output_dir, "multidim_pca_2d.png"),
            p,
            width = 8, height = 6
          )
        }

        plots <- list(pca_2d = p)
      }
    }
  }

  # Store visualization paths
  suite$visualizations$multidimensional <- output_dir

  return(suite)
}
