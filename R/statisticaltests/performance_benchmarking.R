# File: performance_benchmarking.R
# ----------------------------------------------------------------------
#' Comprehensive Performance Benchmarking Module
#'
#' This module provides detailed performance analysis and benchmarking
#' capabilities for the PRNG testing framework, including execution time
#' analysis, memory profiling, and scalability testing.
#'
#' @name performance_benchmarking

#' Run comprehensive performance benchmarks
#'
#' Performs detailed performance analysis across all test categories
#'
#' @param config Validation configuration
#' @param verbose Print progress messages
#' @return Performance benchmark results
#' @export
run_performance_benchmarks <- function(config, verbose = TRUE) {
  if (verbose) cat("\nRunning comprehensive performance benchmarks...\n")

  results <- list(
    timestamp = Sys.time(),
    system_info = get_system_info(),
    benchmarks = list(),
    scalability = list(),
    memory_profile = list(),
    summary = list()
  )

  # 1. Category-specific benchmarks
  if (verbose) cat("  Running category benchmarks...\n")
  results$benchmarks <- benchmark_all_categories(config, verbose)

  # 2. Scalability analysis
  if (verbose) cat("  Running scalability analysis...\n")
  results$scalability <- analyze_scalability(config, verbose)

  # 3. Memory profiling
  if (verbose) cat("  Running memory profiling...\n")
  results$memory_profile <- profile_memory_usage(config, verbose)

  # 4. Parallel performance (if enabled)
  if (config$parallel && requireNamespace("parallel", quietly = TRUE)) {
    if (verbose) cat("  Running parallel performance tests...\n")
    results$parallel <- benchmark_parallel_performance(config, verbose)
  }

  # 5. Generate summary statistics
  results$summary <- summarize_performance(results)

  class(results) <- "qiprng_performance_report"
  return(results)
}

#' Get system information
#'
#' @return List of system details
get_system_info <- function() {
  list(
    platform = R.version$platform,
    r_version = paste(R.version$major, R.version$minor, sep = "."),
    cpu_count = parallel::detectCores(),
    memory_limit = utils::memory.limit(),
    os = Sys.info()["sysname"],
    machine = Sys.info()["machine"]
  )
}

#' Benchmark all test categories
#'
#' @param config Validation configuration
#' @param verbose Print progress
#' @return Benchmark results by category
benchmark_all_categories <- function(config, verbose) {
  categories <- c(
    "basic", "classical", "external", "runs",
    "correlation", "binary", "compression", "multidim"
  )

  benchmarks <- list()

  for (category in categories) {
    if (verbose) cat(sprintf("    Benchmarking %s tests...\n", category))

    # Skip if test function doesn't exist
    test_func_name <- paste0("run_", category, "_tests")
    if (!exists(test_func_name)) {
      if (verbose) cat("      Skipped (function not found)\n")
      next
    }

    benchmarks[[category]] <- benchmark_category(category, config, verbose)
  }

  return(benchmarks)
}

#' Benchmark a specific test category
#'
#' @param category Test category name
#' @param config Validation configuration
#' @param verbose Print progress
#' @return Category benchmark results
benchmark_category <- function(category, config, verbose) {
  results <- list(
    sample_sizes = numeric(),
    execution_times = numeric(),
    throughput = numeric(),
    memory_used = numeric()
  )

  # Define sample sizes based on category
  sample_sizes <- switch(category,
    "basic" = c(100, 1000, 10000, 100000),
    "classical" = c(100, 1000, 10000),
    "external" = c(100, 1000, 10000),
    "compression" = c(1000, 10000, 100000),
    "multidim" = c(100, 1000, 10000),
    c(100, 1000, 10000) # default
  )

  # Filter sample sizes based on config level
  if (config$level == "quick") {
    sample_sizes <- sample_sizes[sample_sizes <= 10000]
  } else if (config$level == "standard") {
    sample_sizes <- sample_sizes[sample_sizes <= 100000]
  }

  test_func <- get(paste0("run_", category, "_tests"))

  for (n in sample_sizes) {
    if (verbose) cat(sprintf("      n=%d: ", n))

    # Create test suite
    suite <- list(
      prng_func = function(n) runif(n),
      config = list(
        significance_level = 0.05,
        save_visualizations = FALSE
      ),
      results = list()
    )

    # Set appropriate sample size config
    suite$config[[paste0(category, "_sample_size")]] <- n

    # Measure memory before
    gc()
    mem_before <- gc()[2, 2]

    # Time the test execution
    timing <- system.time({
      test_result <- tryCatch(
        {
          suite <- test_func(suite)
          TRUE
        },
        error = function(e) {
          FALSE
        }
      )
    })

    # Measure memory after
    gc()
    mem_after <- gc()[2, 2]

    if (test_result) {
      exec_time <- timing["elapsed"]
      mem_used <- mem_after - mem_before

      results$sample_sizes <- c(results$sample_sizes, n)
      results$execution_times <- c(results$execution_times, exec_time)
      results$throughput <- c(results$throughput, n / exec_time)
      results$memory_used <- c(results$memory_used, mem_used)

      if (verbose) {
        cat(sprintf("%.3fs, %.1fMB\n", exec_time, mem_used))
      }
    } else {
      if (verbose) cat("FAILED\n")
    }
  }

  # Add complexity analysis
  if (length(results$sample_sizes) >= 3) {
    results$complexity <- analyze_complexity(
      results$sample_sizes,
      results$execution_times
    )
  }

  return(results)
}

#' Analyze algorithm complexity
#'
#' @param sizes Sample sizes
#' @param times Execution times
#' @return Complexity analysis results
analyze_complexity <- function(sizes, times) {
  # Log-transform for linear regression
  log_sizes <- log(sizes)
  log_times <- log(times)

  # Fit linear model: log(time) = a + b*log(n)
  fit <- lm(log_times ~ log_sizes)

  # Extract complexity exponent
  complexity_exp <- coef(fit)[2]

  # Determine complexity class
  complexity_class <- if (complexity_exp < 0.5) {
    "O(1) - Constant"
  } else if (complexity_exp < 1.5) {
    "O(n) - Linear"
  } else if (complexity_exp < 2.5) {
    "O(n²) - Quadratic"
  } else if (complexity_exp < 3.5) {
    "O(n³) - Cubic"
  } else {
    sprintf("O(n^%.1f)", complexity_exp)
  }

  list(
    exponent = complexity_exp,
    class = complexity_class,
    r_squared = summary(fit)$r.squared,
    model = fit
  )
}

#' Analyze scalability across sample sizes
#'
#' @param config Validation configuration
#' @param verbose Print progress
#' @return Scalability analysis results
analyze_scalability <- function(config, verbose) {
  if (verbose) cat("    Testing scalability...\n")

  # Define exponential sample sizes
  sample_sizes <- 10^seq(1, 6, by = 0.5)

  # Limit based on config level
  if (config$level == "quick") {
    sample_sizes <- sample_sizes[sample_sizes <= 10000]
  } else if (config$level == "standard") {
    sample_sizes <- sample_sizes[sample_sizes <= 100000]
  }

  results <- list(
    sample_sizes = sample_sizes,
    generation_times = numeric(),
    test_times = numeric(),
    total_times = numeric()
  )

  for (n in sample_sizes) {
    if (verbose) cat(sprintf("      n=%g: ", n))

    # Time data generation
    gen_time <- system.time({
      data <- runif(n)
    })["elapsed"]

    # Time basic statistical test
    test_time <- system.time({
      ks.test(data, "punif", 0, 1)
      mean(data)
      var(data)
    })["elapsed"]

    total_time <- gen_time + test_time

    results$generation_times <- c(results$generation_times, gen_time)
    results$test_times <- c(results$test_times, test_time)
    results$total_times <- c(results$total_times, total_time)

    if (verbose) {
      cat(sprintf(
        "gen=%.3fs, test=%.3fs, total=%.3fs\n",
        gen_time, test_time, total_time
      ))
    }

    # Stop if taking too long
    if (total_time > 10 && config$level != "comprehensive") {
      if (verbose) cat("      (stopping due to time limit)\n")
      break
    }
  }

  # Analyze scaling behavior
  if (length(results$sample_sizes) >= 3) {
    results$generation_complexity <- analyze_complexity(
      results$sample_sizes[1:length(results$generation_times)],
      results$generation_times
    )
    results$test_complexity <- analyze_complexity(
      results$sample_sizes[1:length(results$test_times)],
      results$test_times
    )
  }

  return(results)
}

#' Profile memory usage
#'
#' @param config Validation configuration
#' @param verbose Print progress
#' @return Memory profiling results
profile_memory_usage <- function(config, verbose) {
  if (verbose) cat("    Profiling memory usage...\n")

  results <- list(
    baseline = numeric(),
    peak_usage = numeric(),
    data_sizes = numeric(),
    operations = character()
  )

  # Test different operations
  operations <- list(
    generate_uniform = function(n) runif(n),
    generate_normal = function(n) rnorm(n),
    basic_stats = function(n) {
      x <- runif(n)
      list(mean = mean(x), var = var(x), quantiles = quantile(x))
    },
    ks_test = function(n) {
      x <- runif(n)
      ks.test(x, "punif", 0, 1)
    },
    correlation_matrix = function(n) {
      m <- matrix(runif(n * 10), ncol = 10)
      cor(m)
    }
  )

  sample_sizes <- c(1000, 10000, 100000)
  if (config$level == "comprehensive") {
    sample_sizes <- c(sample_sizes, 1000000)
  }

  for (op_name in names(operations)) {
    if (verbose) cat(sprintf("      %s: ", op_name))

    op_func <- operations[[op_name]]

    for (n in sample_sizes) {
      # Force garbage collection
      gc()
      baseline <- gc()[2, 2]

      # Run operation
      tryCatch(
        {
          result <- op_func(n)

          # Measure peak usage
          gc()
          peak <- gc()[2, 2]

          results$operations <- c(results$operations, op_name)
          results$data_sizes <- c(results$data_sizes, n)
          results$baseline <- c(results$baseline, baseline)
          results$peak_usage <- c(results$peak_usage, peak - baseline)
        },
        error = function(e) {
          # Memory allocation failure
          results$operations <- c(results$operations, op_name)
          results$data_sizes <- c(results$data_sizes, n)
          results$baseline <- c(results$baseline, baseline)
          results$peak_usage <- c(results$peak_usage, NA)
        }
      )
    }

    if (verbose) cat("done\n")
  }

  # Create summary by operation
  results$summary <- aggregate(
    peak_usage ~ operations,
    data = data.frame(
      operations = results$operations,
      peak_usage = results$peak_usage
    ),
    FUN = function(x) {
      c(
        mean = mean(x, na.rm = TRUE),
        max = max(x, na.rm = TRUE)
      )
    }
  )

  return(results)
}

#' Benchmark parallel performance
#'
#' @param config Validation configuration
#' @param verbose Print progress
#' @return Parallel performance results
benchmark_parallel_performance <- function(config, verbose) {
  if (verbose) cat("    Testing parallel performance...\n")

  results <- list(
    cores_tested = numeric(),
    execution_times = numeric(),
    speedup = numeric(),
    efficiency = numeric()
  )

  # Test with different number of cores
  max_cores <- min(parallel::detectCores(), 8)
  cores_to_test <- unique(c(1, 2, 4, max_cores))
  cores_to_test <- cores_to_test[cores_to_test <= max_cores]

  # Define a parallelizable task
  parallel_task <- function(n_tasks, n_cores) {
    if (n_cores == 1) {
      # Sequential execution
      lapply(1:n_tasks, function(i) {
        x <- runif(10000)
        ks.test(x, "punif", 0, 1)$p.value
      })
    } else {
      # Parallel execution
      cl <- parallel::makeCluster(n_cores)
      on.exit(parallel::stopCluster(cl))

      parallel::parLapply(cl, 1:n_tasks, function(i) {
        x <- runif(10000)
        ks.test(x, "punif", 0, 1)$p.value
      })
    }
  }

  # Number of tasks to run
  n_tasks <- 100

  # Baseline with 1 core
  baseline_time <- NA

  for (cores in cores_to_test) {
    if (verbose) cat(sprintf("      %d cores: ", cores))

    # Time execution
    exec_time <- system.time({
      tryCatch(
        {
          parallel_task(n_tasks, cores)
          TRUE
        },
        error = function(e) {
          FALSE
        }
      )
    })["elapsed"]

    results$cores_tested <- c(results$cores_tested, cores)
    results$execution_times <- c(results$execution_times, exec_time)

    if (cores == 1) {
      baseline_time <- exec_time
      results$speedup <- c(results$speedup, 1)
      results$efficiency <- c(results$efficiency, 1)
    } else if (!is.na(baseline_time)) {
      speedup <- baseline_time / exec_time
      efficiency <- speedup / cores
      results$speedup <- c(results$speedup, speedup)
      results$efficiency <- c(results$efficiency, efficiency)
    }

    if (verbose) {
      cat(sprintf("%.3fs", exec_time))
      if (cores > 1 && !is.na(baseline_time)) {
        cat(sprintf(
          " (speedup: %.2fx, efficiency: %.1f%%)",
          speedup, efficiency * 100
        ))
      }
      cat("\n")
    }
  }

  return(results)
}

#' Summarize performance results
#'
#' @param results Performance benchmark results
#' @return Summary statistics
summarize_performance <- function(results) {
  summary <- list()

  # Overall execution time statistics
  all_times <- unlist(lapply(results$benchmarks, function(x) x$execution_times))
  if (length(all_times) > 0) {
    summary$execution_time <- list(
      min = min(all_times),
      mean = mean(all_times),
      median = median(all_times),
      max = max(all_times),
      total = sum(all_times)
    )
  }

  # Memory usage statistics
  all_memory <- unlist(lapply(results$benchmarks, function(x) x$memory_used))
  if (length(all_memory) > 0) {
    summary$memory_usage <- list(
      min = min(all_memory),
      mean = mean(all_memory),
      median = median(all_memory),
      max = max(all_memory),
      total = sum(all_memory)
    )
  }

  # Scalability summary
  if (!is.null(results$scalability$generation_complexity)) {
    summary$scalability <- list(
      generation = results$scalability$generation_complexity$class,
      testing = results$scalability$test_complexity$class
    )
  }

  # Parallel efficiency
  if (!is.null(results$parallel)) {
    summary$parallel <- list(
      max_speedup = max(results$parallel$speedup),
      avg_efficiency = mean(results$parallel$efficiency)
    )
  }

  # Performance grade
  summary$grade <- calculate_performance_grade(summary)

  return(summary)
}

#' Calculate overall performance grade
#'
#' @param summary Performance summary
#' @return Performance grade (A-F)
calculate_performance_grade <- function(summary) {
  score <- 100

  # Deduct points for slow execution
  if (!is.null(summary$execution_time)) {
    if (summary$execution_time$mean > 1) score <- score - 10
    if (summary$execution_time$max > 5) score <- score - 10
  }

  # Deduct points for high memory usage
  if (!is.null(summary$memory_usage)) {
    if (summary$memory_usage$mean > 100) score <- score - 10 # > 100MB average
    if (summary$memory_usage$max > 1000) score <- score - 10 # > 1GB max
  }

  # Deduct points for poor scalability
  if (!is.null(summary$scalability)) {
    if (grepl("Quadratic|Cubic", summary$scalability$generation)) score <- score - 15
    if (grepl("Quadratic|Cubic", summary$scalability$testing)) score <- score - 15
  }

  # Deduct points for poor parallel efficiency
  if (!is.null(summary$parallel)) {
    if (summary$parallel$avg_efficiency < 0.5) score <- score - 10
  }

  # Convert to grade
  if (score >= 90) {
    return("A")
  }
  if (score >= 80) {
    return("B")
  }
  if (score >= 70) {
    return("C")
  }
  if (score >= 60) {
    return("D")
  }
  return("F")
}

#' Generate performance report visualization
#'
#' @param perf_results Performance benchmark results
#' @param output_dir Directory to save plots
#' @return List of plot file paths
#' @export
visualize_performance_results <- function(perf_results, output_dir = "performance_plots") {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    warning("ggplot2 required for visualizations")
    return(NULL)
  }

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  plots <- list()

  # 1. Execution time by category and sample size
  exec_data <- do.call(rbind, lapply(names(perf_results$benchmarks), function(cat) {
    bench <- perf_results$benchmarks[[cat]]
    if (length(bench$sample_sizes) > 0) {
      data.frame(
        category = cat,
        sample_size = bench$sample_sizes,
        execution_time = bench$execution_times,
        stringsAsFactors = FALSE
      )
    }
  }))

  if (!is.null(exec_data) && nrow(exec_data) > 0) {
    p1 <- ggplot2::ggplot(
      exec_data,
      ggplot2::aes(
        x = sample_size, y = execution_time,
        color = category
      )
    ) +
      ggplot2::geom_line() +
      ggplot2::geom_point() +
      ggplot2::scale_x_log10() +
      ggplot2::scale_y_log10() +
      ggplot2::theme_minimal() +
      ggplot2::labs(
        title = "Execution Time by Test Category",
        x = "Sample Size",
        y = "Execution Time (seconds)",
        color = "Test Category"
      )

    plot_file <- file.path(output_dir, "execution_time_by_category.png")
    ggplot2::ggsave(plot_file, p1, width = 10, height = 6)
    plots$execution_time <- plot_file
  }

  # 2. Memory usage heatmap
  if (!is.null(perf_results$memory_profile$operations)) {
    mem_data <- data.frame(
      operation = perf_results$memory_profile$operations,
      sample_size = factor(perf_results$memory_profile$data_sizes),
      memory_mb = perf_results$memory_profile$peak_usage
    )

    p2 <- ggplot2::ggplot(
      mem_data,
      ggplot2::aes(
        x = sample_size, y = operation,
        fill = memory_mb
      )
    ) +
      ggplot2::geom_tile() +
      ggplot2::scale_fill_gradient(low = "green", high = "red", na.value = "gray") +
      ggplot2::theme_minimal() +
      ggplot2::labs(
        title = "Memory Usage by Operation and Sample Size",
        x = "Sample Size",
        y = "Operation",
        fill = "Memory (MB)"
      )

    plot_file <- file.path(output_dir, "memory_usage_heatmap.png")
    ggplot2::ggsave(plot_file, p2, width = 10, height = 6)
    plots$memory_usage <- plot_file
  }

  # 3. Scalability plot
  if (!is.null(perf_results$scalability$sample_sizes)) {
    scale_data <- data.frame(
      sample_size = perf_results$scalability$sample_sizes[
        1:length(perf_results$scalability$total_times)
      ],
      generation = perf_results$scalability$generation_times,
      testing = perf_results$scalability$test_times,
      total = perf_results$scalability$total_times
    )

    scale_long <- reshape2::melt(scale_data,
      id.vars = "sample_size",
      variable.name = "phase", value.name = "time"
    )

    p3 <- ggplot2::ggplot(
      scale_long,
      ggplot2::aes(x = sample_size, y = time, color = phase)
    ) +
      ggplot2::geom_line() +
      ggplot2::geom_point() +
      ggplot2::scale_x_log10() +
      ggplot2::scale_y_log10() +
      ggplot2::theme_minimal() +
      ggplot2::labs(
        title = "Scalability Analysis",
        x = "Sample Size",
        y = "Time (seconds)",
        color = "Phase"
      )

    plot_file <- file.path(output_dir, "scalability_analysis.png")
    ggplot2::ggsave(plot_file, p3, width = 10, height = 6)
    plots$scalability <- plot_file
  }

  # 4. Parallel performance
  if (!is.null(perf_results$parallel)) {
    parallel_data <- data.frame(
      cores = perf_results$parallel$cores_tested,
      speedup = perf_results$parallel$speedup,
      efficiency = perf_results$parallel$efficiency * 100
    )

    p4 <- ggplot2::ggplot(parallel_data, ggplot2::aes(x = cores)) +
      ggplot2::geom_line(ggplot2::aes(y = speedup, color = "Speedup")) +
      ggplot2::geom_point(ggplot2::aes(y = speedup, color = "Speedup")) +
      ggplot2::geom_line(ggplot2::aes(y = cores, color = "Ideal"), linetype = "dashed") +
      ggplot2::scale_color_manual(values = c("Speedup" = "blue", "Ideal" = "gray")) +
      ggplot2::theme_minimal() +
      ggplot2::labs(
        title = "Parallel Performance",
        x = "Number of Cores",
        y = "Speedup Factor",
        color = ""
      )

    plot_file <- file.path(output_dir, "parallel_performance.png")
    ggplot2::ggsave(plot_file, p4, width = 10, height = 6)
    plots$parallel <- plot_file
  }

  return(plots)
}
