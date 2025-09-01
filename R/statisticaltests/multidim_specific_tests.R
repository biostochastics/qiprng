# File: multidim_specific_tests.R
# ----------------------------------------------------------------------
#' Specific Multi-dimensional Tests for 2D/3D Uniformity
#'
#' This module implements specific tests for 2D and 3D uniformity including
#' grid-based chi-square tests, scatter tests, minimum distance tests,
#' and convex hull volume tests.

# Check for geometry packages
has_geometry <- requireNamespace("geometry", quietly = TRUE)
has_spatstat <- requireNamespace("spatstat.geom", quietly = TRUE)

#' 2D Uniformity Test with Configurable Grid
#' @export
uniformity_test_2d <- function(points, grid_size = NULL,
                               visual_output = FALSE) {
  if (ncol(points) != 2) {
    stop("uniformity_test_2d requires exactly 2 dimensions")
  }

  n <- nrow(points)

  # Determine optimal grid size if not specified
  if (is.null(grid_size)) {
    # Use rule of thumb: approximately sqrt(n/5) cells per dimension
    grid_size <- max(3, min(20, ceiling(sqrt(n / 5))))
  }

  # Create 2D grid
  x_breaks <- seq(0, 1, length.out = grid_size + 1)
  y_breaks <- seq(0, 1, length.out = grid_size + 1)

  # Count points in each cell
  x_cell <- cut(points[, 1], breaks = x_breaks, include.lowest = TRUE)
  y_cell <- cut(points[, 2], breaks = y_breaks, include.lowest = TRUE)

  # Create contingency table
  cell_counts <- table(x_cell, y_cell)

  # Expected count per cell
  expected_count <- n / (grid_size^2)

  # Chi-square test
  chi_sq_stat <- sum((cell_counts - expected_count)^2 / expected_count)
  df <- (grid_size^2) - 1
  p_value <- pchisq(chi_sq_stat, df, lower.tail = FALSE)

  # Additional uniformity metrics
  empty_cells <- sum(cell_counts == 0)
  max_count <- max(cell_counts)
  cv <- sd(as.vector(cell_counts)) / mean(as.vector(cell_counts))

  # Moran's I for spatial autocorrelation (simplified)
  if (grid_size >= 3) {
    # Create weight matrix for adjacent cells
    morans_i <- calculate_morans_i_2d(cell_counts, grid_size)
  } else {
    morans_i <- list(statistic = NA, p_value = NA)
  }

  result <- list(
    chi_square = chi_sq_stat,
    p_value = p_value,
    df = df,
    grid_size = grid_size,
    empty_cells = empty_cells,
    empty_ratio = empty_cells / (grid_size^2),
    max_count = max_count,
    cv = cv,
    morans_i = morans_i$statistic,
    morans_p = morans_i$p_value,
    cell_counts = cell_counts,
    result = if (is.na(p_value)) {
      "INCONCLUSIVE"
    } else if (p_value >= 0.05) {
      "PASS"
    } else {
      "FAIL"
    },
    details = sprintf(
      "Grid: %dx%d, Empty: %.1f%%, CV: %.3f",
      grid_size, grid_size,
      100 * empty_cells / (grid_size^2), cv
    )
  )

  if (visual_output) {
    result$visualization <- plot_2d_uniformity(points, cell_counts, grid_size)
  }

  return(result)
}

#' 3D Scatter Test using Spatial Statistics
#' @export
scatter_test_3d <- function(points, method = "triplet") {
  if (ncol(points) != 3) {
    stop("scatter_test_3d requires exactly 3 dimensions")
  }

  n <- nrow(points)

  if (method == "triplet") {
    # Test based on triplet distances
    triplet_stats <- analyze_triplets_3d(points, n_samples = min(1000, n))

    # Compare to expected distribution under uniformity
    expected_mean <- 0.5 # Approximate expected mean triplet area
    observed_mean <- mean(triplet_stats$areas)

    # Z-test
    se <- sd(triplet_stats$areas) / sqrt(length(triplet_stats$areas))
    z_stat <- (observed_mean - expected_mean) / se
    p_value <- 2 * pnorm(-abs(z_stat))

    result <- list(
      statistic = z_stat,
      p_value = p_value,
      observed_mean = observed_mean,
      expected_mean = expected_mean,
      min_area = min(triplet_stats$areas),
      max_area = max(triplet_stats$areas),
      cv = sd(triplet_stats$areas) / mean(triplet_stats$areas),
      result = if (is.na(p_value)) {
        "INCONCLUSIVE"
      } else if (p_value >= 0.05) {
        "PASS"
      } else {
        "FAIL"
      },
      details = sprintf(
        "Mean triplet area: %.4f (expected: %.4f)",
        observed_mean, expected_mean
      )
    )
  } else if (method == "octant") {
    # Divide 3D space into 8 octants
    octant_counts <- table(
      points[, 1] > 0.5,
      points[, 2] > 0.5,
      points[, 3] > 0.5
    )

    expected_count <- n / 8
    chi_sq_stat <- sum((octant_counts - expected_count)^2 / expected_count)
    p_value <- pchisq(chi_sq_stat, df = 7, lower.tail = FALSE)

    result <- list(
      statistic = chi_sq_stat,
      p_value = p_value,
      octant_counts = as.vector(octant_counts),
      expected_count = expected_count,
      result = if (is.na(p_value)) {
        "INCONCLUSIVE"
      } else if (p_value >= 0.05) {
        "PASS"
      } else {
        "FAIL"
      },
      details = sprintf(
        "Chi-square: %.3f, Min count: %d, Max count: %d",
        chi_sq_stat, min(octant_counts), max(octant_counts)
      )
    )
  }

  return(result)
}

#' Minimum Distance Test
#' @export
minimum_distance_test <- function(points, theoretical_dist = "poisson") {
  n <- nrow(points)
  dims <- ncol(points)

  # Calculate minimum distances using efficient k-d tree
  dist_result <- calculate_distances(points, use_kdtree = TRUE, k_nearest = 1)
  min_distances <- dist_result$distances[, 1]

  # Theoretical distribution of minimum distances
  if (theoretical_dist == "poisson") {
    # For Poisson process in unit hypercube
    if (dims == 2) {
      # Expected CDF: F(r) = 1 - exp(-λπr²) where λ = n
      lambda <- n
      expected_cdf <- function(r) {
        1 - exp(-lambda * pi * r^2)
      }
    } else if (dims == 3) {
      # Expected CDF: F(r) = 1 - exp(-λ(4/3)πr³)
      lambda <- n
      expected_cdf <- function(r) {
        1 - exp(-lambda * (4 / 3) * pi * r^3)
      }
    } else {
      # General d-dimensional
      lambda <- n
      vol_const <- pi^(dims / 2) / gamma(dims / 2 + 1)
      expected_cdf <- function(r) {
        1 - exp(-lambda * vol_const * r^dims)
      }
    }

    # Kolmogorov-Smirnov test
    ks_result <- ks.test(min_distances, expected_cdf)

    result <- list(
      statistic = ks_result$statistic,
      p_value = ks_result$p.value,
      mean_distance = mean(min_distances),
      median_distance = median(min_distances),
      min_distance = min(min_distances),
      max_distance = max(min_distances),
      theoretical = theoretical_dist,
      result = if (is.na(ks_result$p.value)) {
        "INCONCLUSIVE"
      } else if (ks_result$p.value >= 0.05) {
        "PASS"
      } else {
        "FAIL"
      },
      details = sprintf(
        "KS test vs %s: D = %.4f",
        theoretical_dist, ks_result$statistic
      )
    )
  } else if (theoretical_dist == "empirical") {
    # Bootstrap test
    n_boot <- 1000
    boot_stats <- numeric(n_boot)

    for (i in 1:n_boot) {
      # Generate uniform random points
      boot_points <- matrix(runif(n * dims), n, dims)
      boot_dist <- calculate_distances(boot_points, use_kdtree = TRUE, k_nearest = 1)
      boot_stats[i] <- mean(boot_dist$distances[, 1])
    }

    observed_mean <- mean(min_distances)
    p_value <- 2 * min(
      mean(boot_stats <= observed_mean),
      mean(boot_stats >= observed_mean)
    )

    result <- list(
      statistic = (observed_mean - mean(boot_stats)) / sd(boot_stats),
      p_value = p_value,
      observed_mean = observed_mean,
      bootstrap_mean = mean(boot_stats),
      bootstrap_sd = sd(boot_stats),
      result = if (is.na(p_value)) {
        "INCONCLUSIVE"
      } else if (p_value >= 0.05) {
        "PASS"
      } else {
        "FAIL"
      },
      details = sprintf(
        "Bootstrap test: Observed mean = %.4f, Expected = %.4f",
        observed_mean, mean(boot_stats)
      )
    )
  }

  return(result)
}

#' Convex Hull Volume Test
#' @export
convex_hull_volume_test <- function(points, n_simulations = 1000) {
  if (!has_geometry) {
    stop("geometry package required for convex hull calculations")
  }

  n <- nrow(points)
  dims <- ncol(points)

  # Calculate convex hull volume
  if (dims == 2) {
    # 2D convex hull area
    hull <- geometry::convhulln(points, options = "FA")
    observed_volume <- hull$area
  } else if (dims == 3) {
    # 3D convex hull volume
    hull <- geometry::convhulln(points, options = "FA")
    observed_volume <- hull$vol
  } else {
    # Higher dimensions
    hull <- geometry::convhulln(points)
    observed_volume <- hull$vol
  }

  # Expected volume under uniformity (via simulation)
  sim_volumes <- numeric(n_simulations)

  for (i in 1:n_simulations) {
    sim_points <- matrix(runif(n * dims), n, dims)
    if (dims == 2) {
      sim_hull <- geometry::convhulln(sim_points, options = "FA")
      sim_volumes[i] <- sim_hull$area
    } else {
      sim_hull <- geometry::convhulln(sim_points, options = "FA")
      sim_volumes[i] <- sim_hull$vol
    }
  }

  # Calculate p-value
  p_value <- 2 * min(
    mean(sim_volumes <= observed_volume),
    mean(sim_volumes >= observed_volume)
  )

  # Z-score
  z_score <- (observed_volume - mean(sim_volumes)) / sd(sim_volumes)

  # Expected theoretical volume (approximation)
  if (dims == 2) {
    # For large n, expected area ≈ 1 - O(log(n)/n)
    expected_theoretical <- 1 - 2 * log(n) / n
  } else if (dims == 3) {
    # For large n, expected volume ≈ 1 - O(log(n)²/n)
    expected_theoretical <- 1 - 3 * log(n)^2 / n
  } else {
    expected_theoretical <- NA
  }

  result <- list(
    statistic = z_score,
    p_value = p_value,
    observed_volume = observed_volume,
    expected_volume = mean(sim_volumes),
    sd_volume = sd(sim_volumes),
    theoretical_expected = expected_theoretical,
    dimensions = dims,
    n_vertices = nrow(hull$hull),
    result = if (is.na(p_value)) {
      "INCONCLUSIVE"
    } else if (p_value >= 0.05) {
      "PASS"
    } else {
      "FAIL"
    },
    details = sprintf(
      "Volume: %.4f (expected: %.4f ± %.4f)",
      observed_volume, mean(sim_volumes), sd(sim_volumes)
    )
  )

  return(result)
}

#' Serial Test in Multiple Dimensions
#' @export
serial_test_multidim <- function(points, lag = 1, max_dim = NULL) {
  n <- nrow(points)
  dims <- ncol(points)

  if (is.null(max_dim)) {
    max_dim <- min(dims, 3) # Limit to 3D for computational efficiency
  }

  results <- list()

  # Test serial correlation in each dimension
  for (d in 1:max_dim) {
    if (n - lag < 10) {
      results[[paste0("dim", d)]] <- list(
        correlation = NA,
        p_value = NA,
        result = "SKIP",
        details = "Insufficient data for lag"
      )
      next
    }

    # Serial correlation
    x <- points[1:(n - lag), d]
    y <- points[(lag + 1):n, d]

    cor_test <- cor.test(x, y)

    results[[paste0("dim", d)]] <- list(
      correlation = cor_test$estimate,
      p_value = cor_test$p.value,
      result = if (is.na(cor_test$p.value)) {
        "INCONCLUSIVE"
      } else if (cor_test$p.value >= 0.05) {
        "PASS"
      } else {
        "FAIL"
      },
      details = sprintf("Lag %d correlation: %.4f", lag, cor_test$estimate)
    )
  }

  # Combined test across dimensions
  if (max_dim >= 2) {
    # Test for serial correlation in distance to origin
    distances <- sqrt(rowSums(points[, 1:max_dim]^2))
    if (n - lag >= 10) {
      x <- distances[1:(n - lag)]
      y <- distances[(lag + 1):n]

      cor_test <- cor.test(x, y)

      results$combined <- list(
        correlation = cor_test$estimate,
        p_value = cor_test$p.value,
        result = if (is.na(cor_test$p.value)) {
          "INCONCLUSIVE"
        } else if (cor_test$p.value >= 0.05) {
          "PASS"
        } else {
          "FAIL"
        },
        details = sprintf(
          "Distance lag %d correlation: %.4f",
          lag, cor_test$estimate
        )
      )
    }
  }

  return(results)
}

# Helper functions

#' Calculate Moran's I for 2D grid
calculate_morans_i_2d <- function(cell_counts, grid_size) {
  n <- grid_size^2
  x <- as.vector(cell_counts)
  x_mean <- mean(x)

  # Create adjacency matrix (rook's case - 4 neighbors)
  W <- matrix(0, n, n)

  for (i in 1:grid_size) {
    for (j in 1:grid_size) {
      idx <- (i - 1) * grid_size + j

      # Right neighbor
      if (j < grid_size) {
        W[idx, idx + 1] <- 1
      }
      # Bottom neighbor
      if (i < grid_size) {
        W[idx, idx + grid_size] <- 1
      }
      # Left neighbor
      if (j > 1) {
        W[idx, idx - 1] <- 1
      }
      # Top neighbor
      if (i > 1) {
        W[idx, idx - grid_size] <- 1
      }
    }
  }

  # Calculate Moran's I
  W_sum <- sum(W)

  numerator <- 0
  for (i in 1:n) {
    for (j in 1:n) {
      numerator <- numerator + W[i, j] * (x[i] - x_mean) * (x[j] - x_mean)
    }
  }

  denominator <- sum((x - x_mean)^2)

  I <- (n / W_sum) * (numerator / denominator)

  # Expected value and variance under null hypothesis
  E_I <- -1 / (n - 1)

  # Simplified variance calculation
  b2 <- sum((x - x_mean)^4) / n / (sum((x - x_mean)^2) / n)^2

  V_I <- ((n * ((n^2 - 3 * n + 3) - (n - 1) * b2) +
    3 * W_sum^2 - n * W_sum^2 / (n - 1)) /
    ((n - 1) * (n - 2) * (n - 3) * W_sum^2)) - E_I^2

  # Z-score
  z_score <- (I - E_I) / sqrt(V_I)
  p_value <- 2 * pnorm(-abs(z_score))

  return(list(
    statistic = I, expected = E_I, variance = V_I,
    z_score = z_score, p_value = p_value
  ))
}

#' Analyze triplets in 3D
analyze_triplets_3d <- function(points, n_samples = 1000) {
  n <- nrow(points)

  # Sample random triplets
  areas <- numeric(n_samples)

  for (i in 1:n_samples) {
    # Select 3 random points
    idx <- sample(n, 3)
    p1 <- points[idx[1], ]
    p2 <- points[idx[2], ]
    p3 <- points[idx[3], ]

    # Calculate area of triangle using cross product
    v1 <- p2 - p1
    v2 <- p3 - p1

    # Cross product
    cross_prod <- c(
      v1[2] * v2[3] - v1[3] * v2[2],
      v1[3] * v2[1] - v1[1] * v2[3],
      v1[1] * v2[2] - v1[2] * v2[1]
    )

    # Area is half the magnitude of cross product
    areas[i] <- 0.5 * sqrt(sum(cross_prod^2))
  }

  return(list(areas = areas))
}

#' Plot 2D uniformity test results
plot_2d_uniformity <- function(points, cell_counts, grid_size) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    return(NULL)
  }

  # Create heatmap data
  heatmap_data <- expand.grid(
    x = 1:grid_size,
    y = 1:grid_size
  )
  heatmap_data$count <- as.vector(t(cell_counts))

  p <- ggplot2::ggplot() +
    ggplot2::geom_tile(
      data = heatmap_data,
      ggplot2::aes(x = x, y = y, fill = count)
    ) +
    ggplot2::scale_fill_gradient(low = "white", high = "darkblue") +
    ggplot2::geom_point(
      data = data.frame(
        x = points[, 1] * grid_size,
        y = points[, 2] * grid_size
      ),
      ggplot2::aes(x = x, y = y),
      alpha = 0.3, size = 0.5
    ) +
    ggplot2::coord_fixed() +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "2D Uniformity Test - Grid Cell Occupancy",
      x = "X Grid",
      y = "Y Grid",
      fill = "Count"
    )

  return(p)
}

#' Run specific multi-dimensional tests
#' @export
run_specific_multidim_tests <- function(suite, dimensions = 2) {
  # Generate points if not already present
  if (is.null(suite$multidim_data) || ncol(suite$multidim_data) != dimensions) {
    n <- suite$config$external_sample_size %||% 10000
    points <- generate_multidim_points(n, dimensions, suite$prng_func)
    suite$multidim_data <- points
  } else {
    points <- suite$multidim_data
  }

  # Initialize results if needed
  if (is.null(suite$results$multidim_specific)) {
    suite$results$multidim_specific <- list()
  }

  if (dimensions == 2) {
    # 2D specific tests
    uniform_2d <- uniformity_test_2d(points)
    suite$results$multidim_specific$uniformity_2d <- list(
      description = "2D Grid Uniformity Test",
      result = uniform_2d$result,
      p_value = uniform_2d$p_value,
      statistic = uniform_2d$chi_square,
      details = uniform_2d$details
    )

    # Serial test
    serial_2d <- serial_test_multidim(points, lag = 1, max_dim = 2)
    suite$results$multidim_specific$serial_2d <- list(
      description = "2D Serial Correlation Test",
      result = serial_2d$combined$result,
      p_value = serial_2d$combined$p_value,
      statistic = serial_2d$combined$correlation,
      details = serial_2d$combined$details
    )
  } else if (dimensions == 3) {
    # 3D specific tests
    scatter_3d <- scatter_test_3d(points)
    suite$results$multidim_specific$scatter_3d <- list(
      description = "3D Scatter Test (Triplet Analysis)",
      result = scatter_3d$result,
      p_value = scatter_3d$p_value,
      statistic = scatter_3d$statistic,
      details = scatter_3d$details
    )
  }

  # Tests for any dimension
  min_dist <- minimum_distance_test(points)
  suite$results$multidim_specific$minimum_distance <- list(
    description = sprintf("%dD Minimum Distance Test", dimensions),
    result = min_dist$result,
    p_value = min_dist$p_value,
    statistic = min_dist$statistic,
    details = min_dist$details
  )

  if (has_geometry) {
    hull_test <- convex_hull_volume_test(points, n_simulations = 500)
    suite$results$multidim_specific$convex_hull <- list(
      description = sprintf("%dD Convex Hull Volume Test", dimensions),
      result = hull_test$result,
      p_value = hull_test$p_value,
      statistic = hull_test$statistic,
      details = hull_test$details
    )
  }

  # Store detailed results
  suite$multidim_specific_results <- list(
    uniformity_2d = if (dimensions == 2) uniform_2d else NULL,
    scatter_3d = if (dimensions == 3) scatter_3d else NULL,
    minimum_distance = min_dist,
    convex_hull = if (has_geometry) hull_test else NULL
  )

  return(suite)
}

# Null operator
`%||%` <- function(a, b) if (is.null(a)) b else a
