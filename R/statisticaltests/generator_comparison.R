# File: generator_comparison.R
# ----------------------------------------------------------------------
#' Comprehensive Generator Comparison Framework
#'
#' This module provides functionality to compare multiple random number generators
#' across ALL implemented statistical tests, including external package tests,
#' multi-dimensional tests, and all other test suites.

# Source required modules
source_if_exists <- function(file) {
  if (file.exists(file)) {
    source(file)
  } else {
    # Try in package installation
    pkg_file <- system.file(file, package = "qiprng")
    if (file.exists(pkg_file)) {
      source(pkg_file)
    }
  }
}

# Source all test modules
source_if_exists("R/statisticaltests/unified_reporting.R")
source_if_exists("R/statisticaltests/comprehensive_external_tests.R")
source_if_exists("R/statisticaltests/multidim_tests.R")
source_if_exists("R/statisticaltests/multidim_specific_tests.R")

# Apply test fixes to ensure proper behavior
source_if_exists("R/statisticaltests/apply_test_fixes.R")

#' Define standard generators for comparison
#' @export
get_standard_generators <- function() {
  generators <- list(
    qiprng = list(
      name = "QIPRNG",
      description = "Quantum-inspired PRNG",
      func = function(n) {
        if (requireNamespace("qiprng", quietly = TRUE)) {
          qiprng::generatePRNG(n)
        } else {
          runif(n)  # Fallback
        }
      }
    ),
    
    base_r = list(
      name = "Base R",
      description = "R's built-in Mersenne Twister",
      func = function(n) runif(n)
    ),
    
    dqrng = list(
      name = "dqrng",
      description = "Fast RNG based on PCG family",
      func = function(n) {
        if (requireNamespace("dqrng", quietly = TRUE)) {
          dqrng::dqrunif(n)
        } else {
          NULL
        }
      }
    ),
    
    rngWELL = list(
      name = "rngWELL",
      description = "WELL generator",
      func = function(n) {
        if (requireNamespace("rngWELL", quietly = TRUE)) {
          # Set WELL generator
          old_kind <- RNGkind()[1]
          RNGkind("WELL")
          x <- runif(n)
          RNGkind(old_kind)  # Restore
          x
        } else {
          NULL
        }
      }
    ),
    
    random = list(
      name = "random.org",
      description = "True random numbers from random.org",
      func = function(n) {
        if (requireNamespace("random", quietly = TRUE) && n <= 10000) {
          tryCatch({
            random::randomNumbers(n = n, min = 0, max = 1, col = 1)[,1]
          }, error = function(e) NULL)
        } else {
          NULL
        }
      }
    )
  )
  
  # Remove unavailable generators
  available_generators <- list()
  for (gen_name in names(generators)) {
    gen <- generators[[gen_name]]
    # Test if generator works
    test_result <- tryCatch({
      x <- gen$func(10)
      !is.null(x) && length(x) == 10
    }, error = function(e) FALSE)
    
    if (test_result) {
      available_generators[[gen_name]] <- gen
      cat(sprintf("✓ %s generator available\n", gen$name))
    } else {
      cat(sprintf("✗ %s generator not available\n", gen$name))
    }
  }
  
  return(available_generators)
}

#' Compare generators across all external tests
#' @export
compare_generators_external <- function(generators, sample_size = 10000, 
                                      packages = "all", verbose = TRUE) {
  
  if (verbose) {
    cat("\n=== Running External Test Comparison ===\n")
  }
  
  # Initialize results storage
  comparison_results <- list()
  
  # Run tests for each generator
  for (gen_name in names(generators)) {
    gen <- generators[[gen_name]]
    
    if (verbose) {
      cat(sprintf("\nTesting %s...\n", gen$name))
    }
    
    # Generate data
    set.seed(42)  # Same seed for fair comparison
    data <- gen$func(sample_size)
    
    if (is.null(data)) {
      comparison_results[[gen_name]] <- list(
        error = "Generator failed to produce data"
      )
      next
    }
    
    # Run comprehensive external tests
    if (exists("run_comprehensive_external_tests")) {
      test_results <- run_comprehensive_external_tests(
        data, 
        packages = packages,
        verbose = FALSE
      )
      comparison_results[[gen_name]] <- test_results
    } else {
      comparison_results[[gen_name]] <- list(
        error = "External test runner not available"
      )
    }
  }
  
  return(comparison_results)
}

#' Compare generators across multi-dimensional tests
#' @export
compare_generators_multidim <- function(generators, dimensions = c(2, 3), 
                                      sample_size = 5000, verbose = TRUE) {
  
  if (verbose) {
    cat("\n=== Running Multi-dimensional Test Comparison ===\n")
  }
  
  # Initialize results storage
  comparison_results <- list()
  
  # Run tests for each generator
  for (gen_name in names(generators)) {
    gen <- generators[[gen_name]]
    
    if (verbose) {
      cat(sprintf("\nTesting %s...\n", gen$name))
    }
    
    comparison_results[[gen_name]] <- list()
    
    # Test each dimension
    for (d in dimensions) {
      if (verbose) {
        cat(sprintf("  %dD tests...\n", d))
      }
      
      # Generate multi-dimensional points
      set.seed(42)
      points <- tryCatch({
        generate_multidim_points(sample_size, d, gen$func)
      }, error = function(e) NULL)
      
      if (is.null(points)) {
        comparison_results[[gen_name]][[paste0(d, "D")]] <- list(
          error = "Failed to generate multi-dimensional data"
        )
        next
      }
      
      # Run tests based on dimension
      dim_results <- list()
      
      # General multi-dimensional tests
      if (exists("grid_uniformity_test")) {
        dim_results$grid_uniformity <- grid_uniformity_test(points)
      }
      
      if (exists("nearest_neighbor_test")) {
        dim_results$nearest_neighbor <- nearest_neighbor_test(points)
      }
      
      if (exists("ripleys_k_test")) {
        dim_results$ripleys_k <- ripleys_k_test(points)
      }
      
      # Dimension-specific tests
      if (d == 2 && exists("uniformity_test_2d")) {
        dim_results$uniformity_2d <- uniformity_test_2d(points)
      }
      
      if (d == 3) {
        if (exists("scatter_test_3d")) {
          dim_results$scatter_3d <- scatter_test_3d(points)
        }
        
        if (exists("convex_hull_volume_test") && 
            requireNamespace("geometry", quietly = TRUE)) {
          dim_results$convex_hull <- convex_hull_volume_test(points, 
                                                            n_simulations = 500)
        }
      }
      
      # Always run minimum distance test
      if (exists("minimum_distance_test")) {
        dim_results$minimum_distance <- minimum_distance_test(points)
      }
      
      comparison_results[[gen_name]][[paste0(d, "D")]] <- dim_results
    }
  }
  
  return(comparison_results)
}

#' Run comprehensive comparison across all tests
#' @export
compare_all_generators <- function(generators = NULL, 
                                 sample_size = 10000,
                                 external_packages = "all",
                                 dimensions = c(2, 3),
                                 output_dir = "generator_comparison",
                                 verbose = TRUE) {
  
  # Use default generators if none provided
  if (is.null(generators)) {
    generators <- get_standard_generators()
  }
  
  if (length(generators) == 0) {
    stop("No generators available for comparison")
  }
  
  # Create output directory
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Initialize unified test suite
  unified_suite <- test_suite_results(
    suite_name = "Comprehensive Generator Comparison",
    prng_info = list(
      comparison_date = Sys.time(),
      sample_size = sample_size,
      generators_tested = names(generators)
    )
  )
  
  cat("=== Comprehensive Generator Comparison ===\n")
  cat(sprintf("Comparing %d generators with %d samples\n", 
              length(generators), sample_size))
  cat("Generators:", paste(names(generators), collapse = ", "), "\n")
  
  # 1. External Package Tests
  external_results <- compare_generators_external(
    generators, sample_size, external_packages, verbose
  )
  
  # Convert to unified format
  for (gen_name in names(external_results)) {
    gen_results <- external_results[[gen_name]]
    
    if (!is.null(gen_results$error)) {
      result <- test_result(
        test_name = paste(gen_name, "external", sep = "_"),
        description = paste("External tests for", generators[[gen_name]]$name),
        result = "ERROR",
        details = gen_results$error,
        category = "External Tests",
        subcategory = gen_name
      )
      unified_suite <- add_test_result(unified_suite, result)
      next
    }
    
    # Add individual test results
    if (!is.null(gen_results$tests)) {
      for (pkg in names(gen_results$tests)) {
        pkg_results <- gen_results$tests[[pkg]]
        
        if (!is.null(pkg_results$error)) {
          next
        }
        
        for (test_name in names(pkg_results)) {
          test <- pkg_results[[test_name]]
          
          if (!is.list(test) || is.null(test$result)) next
          
          result <- test_result(
            test_name = paste(gen_name, pkg, test_name, sep = "_"),
            description = test$description,
            result = test$result,
            p_value = test$p_value,
            statistic = test$statistic,
            details = test$details,
            category = "External Tests",
            subcategory = paste(gen_name, pkg, sep = " - ")
          )
          
          unified_suite <- add_test_result(unified_suite, result)
        }
      }
    }
  }
  
  # 2. Multi-dimensional Tests
  multidim_results <- compare_generators_multidim(
    generators, dimensions, sample_size, verbose
  )
  
  # Convert to unified format
  for (gen_name in names(multidim_results)) {
    gen_results <- multidim_results[[gen_name]]
    
    for (dim_key in names(gen_results)) {
      dim_results <- gen_results[[dim_key]]
      
      if (!is.null(dim_results$error)) {
        result <- test_result(
          test_name = paste(gen_name, dim_key, sep = "_"),
          description = paste(dim_key, "tests for", generators[[gen_name]]$name),
          result = "ERROR",
          details = dim_results$error,
          category = "Multi-dimensional Tests",
          subcategory = paste(gen_name, dim_key, sep = " - ")
        )
        unified_suite <- add_test_result(unified_suite, result)
        next
      }
      
      # Add individual test results
      for (test_name in names(dim_results)) {
        test <- dim_results[[test_name]]
        
        if (!is.list(test) || is.null(test$result)) next
        
        result <- test_result(
          test_name = paste(gen_name, dim_key, test_name, sep = "_"),
          description = paste(test_name, "in", dim_key),
          result = test$result,
          p_value = test$p_value,
          statistic = test$statistic,
          details = test$details,
          category = "Multi-dimensional Tests",
          subcategory = paste(gen_name, dim_key, sep = " - ")
        )
        
        unified_suite <- add_test_result(unified_suite, result)
      }
    }
  }
  
  # Finalize suite
  unified_suite <- finalize_suite(unified_suite)
  
  # Generate reports
  cat("\n=== Generating Comparison Reports ===\n")
  
  # HTML report
  html_file <- file.path(output_dir, "generator_comparison.html")
  generate_unified_report(unified_suite, format = "html", 
                         output_file = html_file, theme = "default")
  
  # Markdown report
  md_file <- file.path(output_dir, "generator_comparison.md")
  generate_unified_report(unified_suite, format = "markdown", 
                         output_file = md_file)
  
  # JSON report
  json_file <- file.path(output_dir, "generator_comparison.json")
  generate_unified_report(unified_suite, format = "json", 
                         output_file = json_file)
  
  # Create summary comparison matrix
  comparison_matrix <- create_comparison_matrix(unified_suite, generators)
  
  # Save comparison matrix
  matrix_file <- file.path(output_dir, "comparison_matrix.csv")
  write.csv(comparison_matrix$matrix, matrix_file, row.names = FALSE)
  
  cat("\n=== Comparison Summary ===\n")
  print(comparison_matrix$summary)
  
  return(list(
    suite = unified_suite,
    external_results = external_results,
    multidim_results = multidim_results,
    comparison_matrix = comparison_matrix,
    output_dir = output_dir
  ))
}

#' Create comparison matrix from results
#' @export
create_comparison_matrix <- function(suite, generators) {
  
  # Initialize matrix
  test_names <- unique(sapply(suite$results, function(r) {
    # Extract test type (remove generator prefix)
    parts <- strsplit(r$test_name, "_")[[1]]
    if (length(parts) > 1) {
      paste(parts[-1], collapse = "_")
    } else {
      r$test_name
    }
  }))
  
  gen_names <- names(generators)
  
  # Create matrix
  matrix_data <- data.frame(
    test = test_names,
    stringsAsFactors = FALSE
  )
  
  # Add columns for each generator
  for (gen in gen_names) {
    matrix_data[[gen]] <- NA
  }
  
  # Fill matrix
  for (result in suite$results) {
    # Extract generator and test
    parts <- strsplit(result$test_name, "_")[[1]]
    if (length(parts) > 0) {
      # Handle both "base_r" and "base" as generator names
      gen <- parts[1]
      if (gen == "base" && length(parts) > 1 && parts[2] == "r") {
        gen <- "base_r"
        test <- paste(parts[-c(1,2)], collapse = "_")
      } else if (gen %in% gen_names) {
        test <- paste(parts[-1], collapse = "_")
      } else {
        next
      }
      
      # Find or create row
      row_idx <- which(matrix_data$test == test)
      if (length(row_idx) == 0) {
        # Add new row if test doesn't exist
        new_row <- data.frame(test = test, stringsAsFactors = FALSE)
        for (g in gen_names) {
          new_row[[g]] <- NA
        }
        matrix_data <- rbind(matrix_data, new_row)
        row_idx <- nrow(matrix_data)
      }
      
      # Store result
      matrix_data[row_idx[1], gen] <- result$result
    }
  }
  
  # Calculate summary statistics
  summary_stats <- data.frame(
    generator = gen_names,
    total_tests = 0,
    passed = 0,
    failed = 0,
    errors = 0,
    pass_rate = 0,
    stringsAsFactors = FALSE
  )
  
  for (i in seq_along(gen_names)) {
    gen <- gen_names[i]
    col_data <- matrix_data[[gen]]
    
    summary_stats$total_tests[i] <- sum(!is.na(col_data))
    summary_stats$passed[i] <- sum(col_data == "PASS", na.rm = TRUE)
    summary_stats$failed[i] <- sum(col_data == "FAIL", na.rm = TRUE)
    summary_stats$errors[i] <- sum(col_data == "ERROR", na.rm = TRUE)
    
    if (summary_stats$total_tests[i] > 0) {
      summary_stats$pass_rate[i] <- summary_stats$passed[i] / 
                                    summary_stats$total_tests[i]
    }
  }
  
  # Sort by pass rate
  summary_stats <- summary_stats[order(summary_stats$pass_rate, 
                                      decreasing = TRUE), ]
  
  return(list(
    matrix = matrix_data,
    summary = summary_stats
  ))
}

#' Generate visual comparison plots
#' @export
plot_generator_comparison <- function(comparison_results, output_dir = NULL) {
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    warning("ggplot2 not available for plotting")
    return(NULL)
  }
  
  # Extract summary data
  summary_data <- comparison_results$comparison_matrix$summary
  
  plots <- list()
  
  # Pass rate comparison
  plots$pass_rate <- ggplot2::ggplot(summary_data, 
                                     ggplot2::aes(x = reorder(generator, pass_rate), 
                                                  y = pass_rate)) +
    ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.1f%%", pass_rate * 100)), 
                       hjust = -0.1) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = "Generator Pass Rate Comparison",
      x = "Generator",
      y = "Pass Rate"
    ) +
    ggplot2::scale_y_continuous(limits = c(0, 1.1), 
                                labels = scales::percent) +
    ggplot2::theme_minimal()
  
  # Test count breakdown
  # Create long format data manually without tidyr
  test_breakdown <- data.frame()
  for (i in 1:nrow(summary_data)) {
    test_breakdown <- rbind(test_breakdown,
      data.frame(
        generator = summary_data$generator[i],
        status = "passed",
        count = summary_data$passed[i]
      ),
      data.frame(
        generator = summary_data$generator[i],
        status = "failed", 
        count = summary_data$failed[i]
      ),
      data.frame(
        generator = summary_data$generator[i],
        status = "errors",
        count = summary_data$errors[i]
      )
    )
  }
  
  plots$test_breakdown <- ggplot2::ggplot(test_breakdown,
                                          ggplot2::aes(x = generator, 
                                                       y = count, 
                                                       fill = status)) +
    ggplot2::geom_bar(stat = "identity", position = "stack") +
    ggplot2::scale_fill_manual(values = c(passed = "#27ae60", 
                                          failed = "#e74c3c", 
                                          errors = "#f39c12")) +
    ggplot2::labs(
      title = "Test Results by Generator",
      x = "Generator",
      y = "Number of Tests",
      fill = "Status"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  
  # Save plots if output directory provided
  if (!is.null(output_dir)) {
    for (plot_name in names(plots)) {
      plot_file <- file.path(output_dir, paste0("comparison_", plot_name, ".png"))
      ggplot2::ggsave(plot_file, plots[[plot_name]], 
                      width = 10, height = 6, dpi = 300)
    }
  }
  
  return(plots)
}