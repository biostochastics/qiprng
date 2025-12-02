# File: multidim_visualizations.R
# ----------------------------------------------------------------------
#' Enhanced Visualization Functions for Multi-dimensional Tests
#'
#' This module provides comprehensive visualization capabilities for
#' multi-dimensional test results including static and interactive plots.

# Check for visualization packages
has_ggplot2 <- requireNamespace("ggplot2", quietly = TRUE)
has_plotly <- requireNamespace("plotly", quietly = TRUE)
has_scatterplot3d <- requireNamespace("scatterplot3d", quietly = TRUE)
has_gridExtra <- requireNamespace("gridExtra", quietly = TRUE)
has_viridis <- requireNamespace("viridis", quietly = TRUE)

#' Create comprehensive 2D visualization panel
#' @export
visualize_2d_comprehensive <- function(points, test_results, output_dir = NULL,
                                       show_plots = TRUE) {
  if (!has_ggplot2) {
    warning("ggplot2 not available for visualization")
    return(NULL)
  }

  library(ggplot2)
  plots <- list()

  # 1. Basic scatter plot with marginal densities
  p1 <- create_2d_scatter_with_marginals(points)
  plots$scatter_marginal <- p1

  # 2. Hexbin density plot
  p2 <- ggplot(
    data.frame(x = points[, 1], y = points[, 2]),
    aes(x = x, y = y)
  ) +
    geom_hex(bins = 30) +
    scale_fill_gradient(low = "lightblue", high = "darkblue") +
    coord_fixed() +
    theme_minimal() +
    labs(
      title = "2D Hexbin Density Plot",
      x = "Dimension 1", y = "Dimension 2"
    )

  if (has_viridis) {
    p2 <- p2 + viridis::scale_fill_viridis()
  }

  plots$hexbin <- p2

  # 3. Grid test visualization
  if (!is.null(test_results$grid)) {
    p3 <- visualize_grid_test_2d(points, test_results$grid)
    plots$grid_test <- p3
  }

  # 4. Nearest neighbor visualization
  if (!is.null(test_results$nearest_neighbor)) {
    p4 <- visualize_nn_test_2d(points, test_results$nearest_neighbor)
    plots$nn_test <- p4
  }

  # 5. Ripley's K function plot
  if (!is.null(test_results$ripleys_k)) {
    p5 <- visualize_ripleys_k(test_results$ripleys_k)
    plots$ripleys_k <- p5
  }

  # 6. Combined diagnostic plot
  if (has_gridExtra && length(plots) >= 4) {
    combined <- gridExtra::grid.arrange(
      plots[[1]], plots[[2]], plots[[3]], plots[[4]],
      ncol = 2, nrow = 2,
      top = "2D Uniformity Test Results"
    )
    plots$combined <- combined
  }

  # Save plots if requested
  if (!is.null(output_dir)) {
    save_multidim_plots(plots, output_dir, prefix = "2d")
  }

  # Display plots if requested
  if (show_plots && interactive()) {
    for (name in names(plots)) {
      if (name != "combined") {
        print(plots[[name]])
        if (name != names(plots)[length(plots)]) {
          readline(prompt = "Press [enter] to continue to next plot...")
        }
      }
    }
  }

  return(plots)
}

#' Create scatter plot with marginal densities
create_2d_scatter_with_marginals <- function(points) {
  df <- data.frame(x = points[, 1], y = points[, 2])

  # Main scatter plot
  p_main <- ggplot(df, aes(x = x, y = y)) +
    geom_point(alpha = 0.5, size = 1, color = "darkblue") +
    theme_minimal() +
    theme(plot.margin = margin(0, 0, 0, 0))

  # Marginal density for x
  p_x <- ggplot(df, aes(x = x)) +
    geom_density(fill = "lightblue", alpha = 0.7) +
    theme_void() +
    theme(plot.margin = margin(0, 0, 0, 0))

  # Marginal density for y
  p_y <- ggplot(df, aes(x = y)) +
    geom_density(fill = "lightblue", alpha = 0.7) +
    coord_flip() +
    theme_void() +
    theme(plot.margin = margin(0, 0, 0, 0))

  # Combine if gridExtra available
  if (has_gridExtra) {
    empty <- ggplot() +
      theme_void()

    combined <- gridExtra::grid.arrange(
      p_x, empty,
      p_main, p_y,
      ncol = 2, nrow = 2,
      widths = c(4, 1),
      heights = c(1, 4)
    )

    return(combined)
  }

  return(p_main)
}

#' Visualize grid test results
visualize_grid_test_2d <- function(points, grid_result) {
  grid_res <- grid_result$grid_resolution
  n <- nrow(points)

  # Create grid cell assignments
  x_cell <- ceiling(points[, 1] * grid_res)
  y_cell <- ceiling(points[, 2] * grid_res)

  # Handle edge cases
  x_cell[x_cell > grid_res] <- grid_res
  y_cell[y_cell > grid_res] <- grid_res
  x_cell[x_cell < 1] <- 1
  y_cell[y_cell < 1] <- 1

  # Count points in each cell
  cell_data <- expand.grid(x = 1:grid_res, y = 1:grid_res)
  cell_data$count <- 0

  for (i in 1:n) {
    idx <- which(cell_data$x == x_cell[i] & cell_data$y == y_cell[i])
    if (length(idx) > 0) {
      cell_data$count[idx] <- cell_data$count[idx] + 1
    }
  }

  # Expected count
  expected <- n / (grid_res^2)
  cell_data$deviation <- (cell_data$count - expected) / sqrt(expected)

  p <- ggplot(cell_data, aes(x = x, y = y, fill = deviation)) +
    geom_tile() +
    scale_fill_gradient2(
      low = "blue", mid = "white", high = "red",
      midpoint = 0, limits = c(-3, 3)
    ) +
    coord_fixed() +
    theme_minimal() +
    labs(
      title = sprintf(
        "Grid Test: Chi-square = %.2f, p = %.3f",
        grid_result$statistic, grid_result$p_value
      ),
      subtitle = sprintf(
        "%d×%d grid, %d empty cells",
        grid_res, grid_res, grid_result$empty_cells
      ),
      x = "Grid X", y = "Grid Y",
      fill = "Std Dev"
    )

  return(p)
}

#' Visualize nearest neighbor test results
visualize_nn_test_2d <- function(points, nn_result) {
  # Calculate nearest neighbors
  dist_result <- calculate_distances(points, k_nearest = 5)
  nn_indices <- dist_result$indices[, 1]
  nn_distances <- dist_result$distances[, 1]

  # Create dataframe for plotting
  df <- data.frame(
    x1 = points[, 1],
    y1 = points[, 2],
    x2 = points[nn_indices, 1],
    y2 = points[nn_indices, 2],
    distance = nn_distances
  )

  # Color by distance percentile
  df$dist_percentile <- cut(df$distance,
    quantile(df$distance, probs = seq(0, 1, 0.2)),
    labels = c(
      "0-20%", "20-40%", "40-60%",
      "60-80%", "80-100%"
    ),
    include.lowest = TRUE
  )

  p <- ggplot(df) +
    geom_segment(
      aes(
        x = x1, y = y1, xend = x2, yend = y2,
        color = dist_percentile
      ),
      alpha = 0.3, size = 0.5
    ) +
    geom_point(aes(x = x1, y = y1), size = 1, alpha = 0.7) +
    scale_color_brewer(palette = "RdYlBu") +
    coord_fixed() +
    theme_minimal() +
    labs(
      title = sprintf(
        "Nearest Neighbor Test: z = %.2f, p = %.3f",
        nn_result$statistic, nn_result$p_value
      ),
      subtitle = sprintf(
        "Mean distance: %.4f (expected: %.4f)",
        nn_result$observed_mean, nn_result$expected_mean
      ),
      x = "Dimension 1", y = "Dimension 2",
      color = "Distance\nPercentile"
    )

  return(p)
}

#' Visualize Ripley's K function results
visualize_ripleys_k <- function(ripley_result) {
  df <- data.frame(
    radius = ripley_result$radii,
    observed = ripley_result$observed_k,
    expected = ripley_result$expected_k
  )

  # Calculate confidence band (approximate)
  df$lower <- df$expected * 0.95
  df$upper <- df$expected * 1.05

  p <- ggplot(df, aes(x = radius)) +
    geom_ribbon(aes(ymin = lower, ymax = upper),
      fill = "gray80", alpha = 0.5
    ) +
    geom_line(aes(y = expected),
      color = "red", size = 1,
      linetype = "dashed"
    ) +
    geom_line(aes(y = observed), color = "blue", size = 1) +
    theme_minimal() +
    labs(
      title = sprintf(
        "Ripley's K Function: KS = %.3f, p = %.3f",
        ripley_result$statistic, ripley_result$p_value
      ),
      x = "Radius", y = "K(r)",
      caption = "Blue: Observed, Red: Expected, Gray: 95% band"
    )

  return(p)
}

#' Create comprehensive 3D visualization
#' @export
visualize_3d_comprehensive <- function(points, test_results, output_dir = NULL,
                                       show_plots = TRUE) {
  plots <- list()

  # 1. Interactive 3D scatter plot
  if (has_plotly) {
    p1 <- create_3d_scatter_interactive(points, test_results)
    plots$scatter3d <- p1
  }

  # 2. Static 3D plot with projections
  if (has_scatterplot3d) {
    p2 <- create_3d_static_with_projections(points, test_results)
    plots$static3d <- p2
  }

  # 3. 2D projections grid
  if (has_ggplot2) {
    p3 <- create_3d_projections(points, test_results)
    plots$projections <- p3
  }

  # 4. 3D density slices
  if (has_ggplot2) {
    p4 <- create_3d_density_slices(points, n_slices = 5)
    plots$density_slices <- p4
  }

  # Save plots if requested
  if (!is.null(output_dir)) {
    save_multidim_plots(plots, output_dir, prefix = "3d")
  }

  return(plots)
}

#' Create interactive 3D scatter plot
create_3d_scatter_interactive <- function(points, test_results) {
  # Color by local density
  library(FNN)
  nn_result <- get.knn(points, k = 10)
  local_density <- 1 / rowMeans(nn_result$nn.dist)

  p <- plotly::plot_ly(
    x = points[, 1],
    y = points[, 2],
    z = points[, 3],
    type = "scatter3d",
    mode = "markers",
    marker = list(
      size = 3,
      color = local_density,
      colorscale = "Viridis",
      showscale = TRUE,
      colorbar = list(title = "Local Density")
    ),
    text = paste(
      "Point:", 1:nrow(points),
      "<br>Density:", round(local_density, 3)
    )
  )

  # Add test results to title
  title_text <- "3D Point Distribution"
  if (!is.null(test_results)) {
    test_summary <- sapply(test_results, function(x) {
      if (!is.null(x$result)) {
        return(x$result)
      }
      return(NA)
    })
    pass_rate <- mean(test_summary == "PASS", na.rm = TRUE)
    title_text <- paste(
      title_text,
      sprintf("(%.0f%% tests passed)", pass_rate * 100)
    )
  }

  p <- p %>% plotly::layout(
    title = title_text,
    scene = list(
      xaxis = list(title = "Dimension 1", range = c(0, 1)),
      yaxis = list(title = "Dimension 2", range = c(0, 1)),
      zaxis = list(title = "Dimension 3", range = c(0, 1)),
      aspectmode = "cube"
    )
  )

  return(p)
}

#' Create static 3D plot with projections
create_3d_static_with_projections <- function(points, test_results) {
  # Create plot
  par(mfrow = c(2, 2), mar = c(4, 4, 2, 2))

  # Main 3D plot
  scatterplot3d::scatterplot3d(
    points[, 1], points[, 2], points[, 3],
    pch = 19, cex = 0.5, color = "darkblue",
    main = "3D Distribution",
    xlab = "Dim 1", ylab = "Dim 2", zlab = "Dim 3",
    grid = TRUE, box = TRUE
  )

  # XY projection
  plot(points[, 1], points[, 2],
    pch = 19, cex = 0.3,
    main = "XY Projection", xlab = "Dim 1", ylab = "Dim 2",
    xlim = c(0, 1), ylim = c(0, 1)
  )
  grid()

  # XZ projection
  plot(points[, 1], points[, 3],
    pch = 19, cex = 0.3,
    main = "XZ Projection", xlab = "Dim 1", ylab = "Dim 3",
    xlim = c(0, 1), ylim = c(0, 1)
  )
  grid()

  # YZ projection
  plot(points[, 2], points[, 3],
    pch = 19, cex = 0.3,
    main = "YZ Projection", xlab = "Dim 2", ylab = "Dim 3",
    xlim = c(0, 1), ylim = c(0, 1)
  )
  grid()

  par(mfrow = c(1, 1))

  return(recordPlot())
}

#' Create 2D projections grid
create_3d_projections <- function(points, test_results) {
  library(ggplot2)

  # Create all pairwise projections
  projections <- list(
    list(dims = c(1, 2), name = "Dims 1-2"),
    list(dims = c(1, 3), name = "Dims 1-3"),
    list(dims = c(2, 3), name = "Dims 2-3")
  )

  plots <- list()

  for (proj in projections) {
    df <- data.frame(
      x = points[, proj$dims[1]],
      y = points[, proj$dims[2]]
    )

    p <- ggplot(df, aes(x = x, y = y)) +
      geom_point(alpha = 0.5, size = 0.8, color = "darkblue") +
      geom_density_2d(color = "red", alpha = 0.5) +
      coord_fixed() +
      theme_minimal() +
      labs(
        title = proj$name,
        x = sprintf("Dimension %d", proj$dims[1]),
        y = sprintf("Dimension %d", proj$dims[2])
      )

    plots[[proj$name]] <- p
  }

  if (has_gridExtra) {
    combined <- gridExtra::grid.arrange(
      grobs = plots,
      ncol = 2,
      top = "3D Projections"
    )
    return(combined)
  }

  return(plots)
}

#' Create 3D density slices
create_3d_density_slices <- function(points, n_slices = 5) {
  library(ggplot2)

  # Create slices along z-axis
  z_breaks <- seq(0, 1, length.out = n_slices + 1)

  plots <- list()

  for (i in 1:n_slices) {
    # Select points in this slice
    z_min <- z_breaks[i]
    z_max <- z_breaks[i + 1]

    slice_idx <- which(points[, 3] >= z_min & points[, 3] < z_max)

    if (length(slice_idx) > 10) {
      slice_points <- points[slice_idx, ]

      df <- data.frame(
        x = slice_points[, 1],
        y = slice_points[, 2]
      )

      p <- ggplot(df, aes(x = x, y = y)) +
        geom_hex(bins = 15) +
        scale_fill_gradient(low = "white", high = "darkred") +
        coord_fixed() +
        theme_minimal() +
        labs(
          title = sprintf(
            "Slice %d: z ∈ [%.2f, %.2f]",
            i, z_min, z_max
          ),
          subtitle = sprintf("%d points", length(slice_idx)),
          x = "Dimension 1", y = "Dimension 2"
        )

      plots[[paste0("slice_", i)]] <- p
    }
  }

  if (has_gridExtra && length(plots) > 0) {
    combined <- gridExtra::grid.arrange(
      grobs = plots,
      ncol = 2,
      top = "3D Density Slices"
    )
    return(combined)
  }

  return(plots)
}

#' Save multidimensional plots
save_multidim_plots <- function(plots, output_dir, prefix = "multidim") {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  for (name in names(plots)) {
    if (!is.null(plots[[name]])) {
      filename <- file.path(
        output_dir,
        sprintf("%s_%s.png", prefix, name)
      )

      if (inherits(plots[[name]], "plotly")) {
        # Save plotly as HTML
        html_file <- sub("\\.png$", ".html", filename)
        htmlwidgets::saveWidget(plots[[name]], html_file,
          selfcontained = TRUE
        )
      } else if (inherits(plots[[name]], "ggplot") ||
        inherits(plots[[name]], "gtable")) {
        # Save ggplot
        ggplot2::ggsave(filename, plots[[name]],
          width = 8, height = 6, dpi = 120
        )
      } else if (inherits(plots[[name]], "recordedplot")) {
        # Save base R plot
        png(filename, width = 800, height = 600)
        replayPlot(plots[[name]])
        dev.off()
      }
    }
  }

  cat(sprintf("Plots saved to: %s\n", output_dir))
}

#' Create diagnostic summary plot
#' @export
create_multidim_diagnostic_plot <- function(results_list, output_file = NULL) {
  if (!has_ggplot2) {
    warning("ggplot2 required for diagnostic plots")
    return(NULL)
  }

  library(ggplot2)

  # Extract p-values and test names
  plot_data <- data.frame()

  for (gen_name in names(results_list)) {
    results <- results_list[[gen_name]]$results$multidimensional

    for (test_name in names(results)) {
      if (!is.null(results[[test_name]]$p_value)) {
        plot_data <- rbind(plot_data, data.frame(
          generator = gen_name,
          test = test_name,
          p_value = results[[test_name]]$p_value,
          result = results[[test_name]]$result
        ))
      }
    }
  }

  # Create heatmap
  p <- ggplot(plot_data, aes(x = test, y = generator, fill = p_value)) +
    geom_tile(color = "white", size = 0.5) +
    geom_text(aes(label = sprintf("%.3f", p_value)), size = 3) +
    scale_fill_gradient2(
      low = "red", mid = "yellow", high = "green",
      midpoint = 0.05, limits = c(0, 0.2)
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(
      title = "Multi-dimensional Test Results",
      subtitle = "P-values by Generator and Test",
      x = "Test", y = "Generator",
      fill = "P-value"
    )

  if (!is.null(output_file)) {
    ggsave(output_file, p, width = 10, height = 6, dpi = 120)
  }

  return(p)
}

#' Animation of points over time (if temporal data available)
#' @export
animate_multidim_evolution <- function(points_list, dimension = 2,
                                       output_file = NULL) {
  if (!requireNamespace("gganimate", quietly = TRUE)) {
    warning("gganimate package required for animations")
    return(NULL)
  }

  library(ggplot2)
  library(gganimate)

  # Combine all time steps
  all_data <- data.frame()

  for (t in seq_along(points_list)) {
    points <- points_list[[t]]
    if (ncol(points) >= dimension) {
      df <- data.frame(
        x = points[, 1],
        y = points[, min(2, dimension)],
        time = t
      )
      all_data <- rbind(all_data, df)
    }
  }

  # Create animated plot
  p <- ggplot(all_data, aes(x = x, y = y)) +
    geom_point(alpha = 0.5, size = 1, color = "darkblue") +
    coord_fixed() +
    theme_minimal() +
    labs(
      title = "Point Evolution: Time {frame_time}",
      x = "Dimension 1", y = sprintf("Dimension %d", min(2, dimension))
    ) +
    transition_time(time) +
    ease_aes("linear")

  # Save animation
  if (!is.null(output_file)) {
    anim_save(output_file, p, width = 600, height = 600, fps = 10)
  }

  return(p)
}
