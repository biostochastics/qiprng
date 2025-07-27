#' QIPRNG Benchmarking Module
#' 
#' A comprehensive set of functions to benchmark the performance of the QIPRNG
#' package against other random number generators in R. This module provides tools
#' for measuring execution time, memory usage, and scaling characteristics across
#' different configurations and distributions.

#' Benchmark QIPRNG against other random number generators
#' 
#' Performs comprehensive benchmarking of QIPRNG against other random number
#' generators for various distributions, sample sizes, and configurations.
#' Uses microbenchmark for precise timing measurements and creates visualizations
#' to compare performance characteristics.
#' 
#' @param n_values Vector of sample sizes to benchmark (e.g., c(10, 100, 1000))
#' @param generators List of generator functions to compare; each function should take n as input and return n random numbers
#' @param distributions Character vector of distributions to test ("uniform", "normal", "exponential")
#' @param repetitions Number of repetitions for each benchmark to ensure reliable measurements
#' @param configs List of QIPRNG configurations to test (named list of configuration parameters)
#' @param export_data Whether to export raw benchmark data to an RDS file
#' @param file Path to save results if export_data is TRUE
#' @return A list containing benchmark results and ggplot2 visualization objects
#' @examples
#' \dontrun{
#' # Initialize QIPRNG
#' createPRNG()
#' 
#' # Define generators to compare
#' generators <- list(
#'   "qiprng" = function(n) generatePRNG(n),
#'   "base_r" = function(n) runif(n)
#' )
#' 
#' # Run benchmark with small sample sizes
#' results <- benchmark_qiprng(
#'   n_values = c(100, 1000),
#'   generators = generators,
#'   repetitions = 5
#' )
#' 
#' # Display results
#' print(results$plots$scaling)
#' }
#' @export
benchmark_qiprng <- function(n_values = c(10, 100, 1000, 10000, 100000, 1000000),
                            generators = list("qiprng" = function(n) qiprng::generatePRNG(n),
                                              "base_r" = function(n) stats::runif(n),
                                              "dqrng" = function(n) if(requireNamespace("dqrng", quietly = TRUE)) dqrng::dqrunif(n) else stats::runif(n)),
                            distributions = c("uniform", "normal", "exponential"),
                            repetitions = 10,
                            configs = list(
                              "default" = list(),
                              "crypto" = list(use_crypto_mixing = TRUE),
                              "high_precision" = list(mpfr_precision = 128)
                            ),
                            export_data = FALSE,
                            file = "qiprng_benchmark.rds") {
  # For diagnostic reporting
  debug_mode <- TRUE
  
  # Check required packages
  if (!requireNamespace("microbenchmark", quietly = TRUE)) {
    stop("Package 'microbenchmark' is needed for benchmarking. Please install it.")
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is needed for plotting. Please install it.")
  }
  
  # Set up error logging
  log_error <- function(msg) {
    cat("ERROR: ", msg, "\n")
    # Try to use external log_message if available
    if (exists("log_message", envir = .GlobalEnv) && is.function(get("log_message", envir = .GlobalEnv))) {
      get("log_message", envir = .GlobalEnv)(paste0("ERROR in benchmark: ", msg, "\n"))
    }
  }
  
  # Test the QIPRNG system initialization
  tryCatch({
    if (!requireNamespace("qiprng", quietly = TRUE)) {
      log_error("qiprng package not available")
    } else if (!exists("createPRNG", where = asNamespace("qiprng"))) {
      log_error("Function 'createPRNG' not found in qiprng namespace")
    } else {
      if (debug_mode) cat("DEBUG: QIPRNG system available\n")
    }
  }, error = function(e) {
    log_error(paste0("QIPRNG availability check failed: ", e$message))
  })
  
  # Verify dqrng if included in generators
  if ("dqrng" %in% names(generators) && !requireNamespace("dqrng", quietly = TRUE)) {
    warning("Package 'dqrng' not available, removing from generators list")
    generators <- generators[names(generators) != "dqrng"]
  }
  
  # Results storage
  all_results <- list()
  
  # Run benchmarks for each distribution
  for (dist in distributions) {
    cat(sprintf("\nBenchmarking %s distribution...\n", dist))
    
    # Setup distribution-specific generators
    dist_generators <- generators
    if (dist == "normal") {
      dist_generators <- list(
        "qiprng" = function(n) {
          qiprng::createPRNG(list(distribution = "normal"))
          qiprng::generatePRNG(n)
        },
        "base_r" = function(n) rnorm(n)
      )
      if ("dqrng" %in% names(generators)) {
        dist_generators$dqrng <- function(n) dqrng::dqrnorm(n)
      }
    } else if (dist == "exponential") {
      dist_generators <- list(
        "qiprng" = function(n) {
          qiprng::createPRNG(list(distribution = "exponential"))
          qiprng::generatePRNG(n)
        },
        "base_r" = function(n) rexp(n)
      )
      if ("dqrng" %in% names(generators)) {
        dist_generators$dqrng <- function(n) dqrng::dqrexp(n)
      }
    }
    
    # Run benchmarks for each configuration
    for (config_name in names(configs)) {
      cat(sprintf("  Testing configuration: %s\n", config_name))
      
      # Update QIPRNG generator with this configuration
      config <- configs[[config_name]]
      dist_generators$qiprng <- if (dist == "uniform") {
        function(n) {
          tryCatch({
            # Print detailed debugging information
            if (debug_mode) {
              cat("DEBUG: Creating QIPRNG uniform generator with config:\n")
              print(config)
            }
            
            # Create a fresh configuration for this run
            current_config <- config
            current_config$distribution <- "uniform_01"
            
            # Create and generate
            qiprng::createPRNG(current_config)
            result <- qiprng::generatePRNG(n)
            if (debug_mode) cat("DEBUG: QIPRNG uniform generation successful\n")
            result
          }, error = function(e) {
            log_error(paste0("Error in qiprng uniform generator: ", e$message))
            stats::runif(n) # Fallback to base R
          })
        }
      } else if (dist == "normal") {
        function(n) {
          tryCatch({
            # Print detailed debugging information
            if (debug_mode) {
              cat("DEBUG: Creating QIPRNG normal generator with config:\n")
              print(config)
            }
            
            # Create a fresh configuration for this run
            current_config <- config
            current_config$distribution <- "normal"
            
            # Create and generate
            qiprng::createPRNG(current_config)
            result <- qiprng::generatePRNG(n)
            if (debug_mode) cat("DEBUG: QIPRNG normal generation successful\n")
            result
          }, error = function(e) {
            log_error(paste0("Error in qiprng normal generator: ", e$message))
            stats::rnorm(n) # Fallback to base R
          })
        }
      } else if (dist == "exponential") {
        function(n) {
          tryCatch({
            # Print detailed debugging information
            if (debug_mode) {
              cat("DEBUG: Creating QIPRNG exponential generator with config:\n")
              print(config)
            }
            
            # Create a fresh configuration for this run
            current_config <- config
            current_config$distribution <- "exponential"
            
            # Create and generate
            qiprng::createPRNG(current_config)
            result <- qiprng::generatePRNG(n)
            if (debug_mode) cat("DEBUG: QIPRNG exponential generation successful\n")
            result
          }, error = function(e) {
            log_error(paste0("Error in qiprng exponential generator: ", e$message))
            stats::rexp(n) # Fallback to base R
          })
        }
      }
      
      # Benchmark for each sample size
      for (n in n_values) {
        cat(sprintf("    n = %d: ", n))
        
        # Run benchmark
        bench_result <- try({
          # First check if generators actually work
          for (gen_name in names(dist_generators)) {
            if (debug_mode) cat("DEBUG: Testing generator ", gen_name, "\n")
            tryCatch({
              test_result <- dist_generators[[gen_name]](10)
              if (length(test_result) != 10) {
                log_error(paste0("Generator ", gen_name, " returned wrong length: ", length(test_result)))
              }
            }, error = function(e) {
              log_error(paste0("Generator ", gen_name, " failed test: ", e$message))
            })
          }
          
          # Run the actual benchmark using a better approach that avoids scoping issues
          if (debug_mode) cat("DEBUG: Running microbenchmark\n")
          
          # Create a temporary environment with our generators
          benchmark_env <- new.env(parent = emptyenv())
          
          # Define the benchmark expressions directly
          benchmark_calls <- list()
          for (name in names(dist_generators)) {
            # Assign the generator to the environment with a unique name
            generator_name <- paste0("generator_", name)
            benchmark_env[[generator_name]] <- dist_generators[[name]]
            
            # Create the call using the assigned generator
            benchmark_calls[[name]] <- bquote(.(as.name(generator_name))(.(n)))
          }
          
          # Run the benchmark in our custom environment
          # Make sure microbenchmark is loaded
          if (!requireNamespace("microbenchmark", quietly = TRUE)) {
            stop("Package 'microbenchmark' is needed for benchmarking")
          }
          
          # A simpler and more direct approach - use the benchmark function directly
          # Create a list of expressions that directly reference the generator functions
          expr_list <- list()
          for (name in names(dist_generators)) {
            expr_list[[name]] <- substitute(GEN(N), list(GEN = dist_generators[[name]], N = n))
          }
          
          # Run microbenchmark with the direct expressions
          mb <- microbenchmark::microbenchmark(list = expr_list, times = repetitions)
          cat("done\n")
          mb
        }, silent = !debug_mode)
        
        if (inherits(bench_result, "try-error")) {
          cat("ERROR\n")
          if (debug_mode) {
            cat("DEBUG: Benchmark error details: ", attr(bench_result, "condition")$message, "\n")
          }
          next
        }
        
        # Store results
        result_key <- sprintf("%s_%s_%d", dist, config_name, n)
        all_results[[result_key]] <- bench_result
      }
    }
  }
  
  # Process and visualize results
  plots <- create_benchmark_plots(all_results, n_values, distributions, configs)
  
  # Export data if requested
  if (export_data) {
    saveRDS(all_results, file)
  }
  
  # Return all results
  return(list(
    results = all_results,
    plots = plots
  ))
}

#' Create plots from benchmark results
#' 
#' @param results List of microbenchmark results
#' @param n_values Vector of sample sizes used
#' @param distributions Character vector of distributions tested
#' @param configs List of QIPRNG configurations tested
#' @return A list of ggplot objects
#' @keywords internal
create_benchmark_plots <- function(results, n_values, distributions, configs) {
  
  # Extract and format data
  plot_data <- data.frame()
  
  for (dist in distributions) {
    for (config_name in names(configs)) {
      for (n in n_values) {
        result_key <- sprintf("%s_%s_%d", dist, config_name, n)
        
        if (!result_key %in% names(results)) {
          next
        }
        
        # Extract benchmark data
        mb <- results[[result_key]]
        mb_df <- data.frame(
          time = mb$time / 1e6,  # Convert to milliseconds
          expr = mb$expr,
          distribution = dist,
          config = config_name,
          n = n
        )
        
        plot_data <- rbind(plot_data, mb_df)
      }
    }
  }
  
  if (nrow(plot_data) == 0) {
    warning("No benchmark data available for plotting.")
    return(list())
  }
  
  # Create plots
  plots <- list()
  
  # Overall performance by sample size
  plots$size_comparison <- ggplot2::ggplot(
    plot_data, 
    ggplot2::aes(x = factor(n), y = time, color = expr)
  ) +
    ggplot2::geom_boxplot() +
    ggplot2::scale_y_log10() +
    ggplot2::facet_wrap(~ distribution) +
    ggplot2::labs(
      x = "Sample Size (n)",
      y = "Time (ms)",
      color = "Generator",
      title = "RNG Performance by Sample Size"
    ) +
    ggplot2::theme_minimal()
  
  # QIPRNG config comparison
  # Filter for only QIPRNG
  qiprng_data <- plot_data[grep("qiprng", as.character(plot_data$expr)), ]
  if (nrow(qiprng_data) > 0) {
    plots$config_comparison <- ggplot2::ggplot(
      qiprng_data, 
      ggplot2::aes(x = factor(n), y = time, color = config)
    ) +
      ggplot2::geom_boxplot() +
      ggplot2::scale_y_log10() +
      ggplot2::facet_wrap(~ distribution) +
      ggplot2::labs(
        x = "Sample Size (n)",
        y = "Time (ms)",
        color = "Configuration",
        title = "QIPRNG Performance by Configuration"
      ) +
      ggplot2::theme_minimal()
  }
  
  # Performance comparison across generators (log-log plot)
  # Calculate median time for each generator/n combination
  summary_data <- aggregate(
    time ~ expr + distribution + n, 
    data = plot_data,
    FUN = median
  )
  
  plots$scaling <- ggplot2::ggplot(
    summary_data, 
    ggplot2::aes(x = n, y = time, color = expr)
  ) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::scale_x_log10() +
    ggplot2::scale_y_log10() +
    ggplot2::facet_wrap(~ distribution) +
    ggplot2::labs(
      x = "Sample Size (n, log scale)",
      y = "Median Time (ms, log scale)",
      color = "Generator",
      title = "RNG Scaling Performance"
    ) +
    ggplot2::theme_minimal()
  
  return(plots)
}

#' Profile QIPRNG with different configurations
#' 
#' Profiles the performance of QIPRNG across different configuration settings.
#' This function is useful for identifying optimal configurations for specific
#' use cases and understanding performance trade-offs between different settings.
#' 
#' The function can optionally generate detailed profiling information using
#' the profvis package if it is installed, providing insights into which parts
#' of the code consume the most resources.
#' 
#' @param config_list Named list of configurations to test, where each configuration is a list of parameters
#' @param n Sample size to use for testing (number of random values to generate)
#' @param repetitions Number of repetitions for each test to ensure reliable measurements
#' @param output_dir Directory to save detailed profiling results if profvis is available
#' @return A data frame with profiling results including median time, mean time, min and max times for each configuration
#' @examples
#' \dontrun{
#' # Profile different configurations
#' profiles <- profile_qiprng_config(
#'   config_list = list(
#'     "default" = list(),
#'     "high_precision" = list(mpfr_precision = 128),
#'     "normal" = list(distribution = "normal"),
#'     "large_buffer" = list(buffer_size = 10000)
#'   )
#' )
#' 
#' # Examine results
#' print(profiles)
#' }
#' @export
profile_qiprng_config <- function(config_list = list(
                                  "default" = list(),
                                  "crypto" = list(use_crypto_mixing = TRUE),
                                  "high_precision" = list(mpfr_precision = 128),
                                  "normal" = list(distribution = "normal"),
                                  "exponential" = list(distribution = "exponential")),
                                n = 10000,
                                repetitions = 10,
                                output_dir = "test_results") {
  
  # Check required packages
  if (!requireNamespace("microbenchmark", quietly = TRUE)) {
    stop("Package 'microbenchmark' is needed for benchmarking. Please install it.")
  }
  if (!requireNamespace("profvis", quietly = TRUE)) {
    warning("Package 'profvis' is recommended for detailed profiling. Please install it.")
  }
  
  # Results storage
  profile_results <- data.frame(
    config_name = character(),
    median_time_ms = numeric(),
    mean_time_ms = numeric(),
    min_time_ms = numeric(),
    max_time_ms = numeric()
  )
  
  # Run profiling for each config
  for (config_name in names(config_list)) {
    cat(sprintf("Profiling configuration: %s\n", config_name))
    
    config <- config_list[[config_name]]
    
    # Create test function
    test_fn <- function() {
      createPRNG(config)
      generatePRNG(n)
    }
    
    # Run benchmark
    mb <- microbenchmark::microbenchmark(test_fn(), times = repetitions)
    
    # Convert results to ms
    times_ms <- mb$time / 1e6
    
    # Store summary statistics
    profile_results <- rbind(profile_results, data.frame(
      config_name = config_name,
      median_time_ms = median(times_ms),
      mean_time_ms = mean(times_ms),
      min_time_ms = min(times_ms),
      max_time_ms = max(times_ms)
    ))
    
    # Optional detailed profiling with profvis if available
    if (requireNamespace("profvis", quietly = TRUE)) {
      # Create output directory if it doesn't exist
      if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE)
      }
      
      # Create a profile file for this config in the output directory
      profile_file <- file.path(output_dir, sprintf("qiprng_profile_%s.html", gsub("[^a-zA-Z0-9]", "_", config_name)))
      cat(sprintf("  Creating detailed profile: %s\n", profile_file))
      
      # Run profvis
      pv <- profvis::profvis({
        createPRNG(config)
        generatePRNG(n)
      })
      
      # Save profile
      try({
        if (requireNamespace("profvis", quietly = TRUE) && requireNamespace("htmlwidgets", quietly = TRUE)) {
          htmlwidgets::saveWidget(pv, profile_file)
        }
      }, silent = TRUE)
    }
  }
  
  # Sort by median time
  profile_results <- profile_results[order(profile_results$median_time_ms), ]
  
  # Create a simple bar plot of results
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    p <- ggplot2::ggplot(profile_results, ggplot2::aes(x = reorder(config_name, median_time_ms), y = median_time_ms)) +
      ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = min_time_ms, ymax = max_time_ms), width = 0.2) +
      ggplot2::labs(
        x = "Configuration",
        y = "Median Time (ms)",
        title = "QIPRNG Configuration Performance Comparison"
      ) +
      ggplot2::coord_flip() +
      ggplot2::theme_minimal()
    
    print(p)
  }
  
  return(profile_results)
}

#' Benchmark generation of large quantities of random numbers
#' 
#' Tests the performance and memory characteristics of random number generators when
#' generating very large quantities of values. This function is particularly useful for
#' assessing memory efficiency, scaling behavior, and practical limits for applications
#' requiring millions or billions of random numbers.
#' 
#' The function measures both execution time and memory consumption for each generator
#' and configuration, providing insights into real-world performance for data-intensive
#' applications.
#' 
#' @param n_values Vector of large sample sizes to benchmark (e.g., c(1e6, 5e6, 1e7))
#' @param configs List of configurations to test, where each configuration is a list of parameters
#' @param generators List of generator functions to compare, each taking a single argument n
#' @param repetitions Number of repetitions for each benchmark to ensure reliable measurements
#' @return A list containing raw benchmark results and summary statistics with visualizations
#' @examples
#' \dontrun{
#' # Initialize QIPRNG
#' createPRNG()
#' 
#' # Define generators and configurations
#' results <- benchmark_large_generation(
#'   n_values = c(1e6, 5e6),  # 1 million and 5 million values
#'   configs = list(
#'     "default" = list(),
#'     "large_buffer" = list(buffer_size = 1e6)
#'   )
#' )
#' 
#' # Examine summary statistics
#' print(results$summary)
#' }
#' @export
benchmark_large_generation <- function(n_values = c(1e6, 5e6, 1e7, 5e7),
                                     configs = list(
                                       "default" = list(),
                                       "optimized" = list(buffer_size = 1e6)
                                     ),
                                     generators = list(
                                       "qiprng" = function(n) generatePRNG(n),
                                       "base_r" = function(n) runif(n)
                                     ),
                                     repetitions = 3) {
  
  # Results storage
  large_results <- data.frame(
    generator = character(),
    config = character(),
    n = numeric(),
    rep = numeric(),
    time_sec = numeric(),
    memory_mb = numeric()
  )
  
  # Run benchmarks
  for (gen_name in names(generators)) {
    for (config_name in names(configs)) {
      for (n in n_values) {
        cat(sprintf("Benchmarking %s with %s configuration for n=%d\n", 
                    gen_name, config_name, n))
        
        # Set configuration for QIPRNG if that's the generator
        if (gen_name == "qiprng") {
          qiprng::createPRNG(configs[[config_name]])
        }
        
        gen_func <- generators[[gen_name]]
        
        # Run multiple repetitions
        for (rep in 1:repetitions) {
          # Clean up memory
          gc()
          
          # Record memory before
          mem_before <- sum(gc()[, 2])
          
          # Time the generation
          start_time <- proc.time()
          result <- gen_func(n)
          end_time <- proc.time()
          
          # Record memory after
          mem_after <- sum(gc()[, 2])
          
          # Calculate metrics
          time_sec <- (end_time - start_time)["elapsed"]
          memory_mb <- (mem_after - mem_before)
          
          # Store results
          large_results <- rbind(large_results, data.frame(
            generator = gen_name,
            config = config_name,
            n = n,
            rep = rep,
            time_sec = time_sec,
            memory_mb = memory_mb
          ))
          
          cat(sprintf("  Rep %d: %.2f seconds, %.2f MB\n", rep, time_sec, memory_mb))
        }
      }
    }
  }
  
  # Calculate statistics
  large_summary <- aggregate(
    cbind(time_sec, memory_mb) ~ generator + config + n, 
    data = large_results,
    FUN = function(x) c(mean = mean(x), sd = sd(x), min = min(x), max = max(x))
  )
  
  # Reshape for easier access
  large_summary_flat <- data.frame(
    generator = large_summary$generator,
    config = large_summary$config,
    n = large_summary$n,
    time_mean = large_summary$time_sec[, "mean"],
    time_sd = large_summary$time_sec[, "sd"],
    time_min = large_summary$time_sec[, "min"],
    time_max = large_summary$time_sec[, "max"],
    memory_mean = large_summary$memory_mb[, "mean"],
    memory_sd = large_summary$memory_mb[, "sd"],
    memory_min = large_summary$memory_mb[, "min"],
    memory_max = large_summary$memory_mb[, "max"]
  )
  
  # Create performance plots if ggplot2 is available
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    # Time performance
    p1 <- ggplot2::ggplot(
      large_summary_flat, 
      ggplot2::aes(x = n, y = time_mean, color = interaction(generator, config))
    ) +
      ggplot2::geom_point() +
      ggplot2::geom_line() +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = time_mean - time_sd, ymax = time_mean + time_sd), width = 0.1) +
      ggplot2::scale_x_log10(labels = scales::comma) +
      ggplot2::labs(
        x = "Sample Size (n)",
        y = "Mean Execution Time (seconds)",
        color = "Generator/Config",
        title = "RNG Execution Time for Large Sample Sizes"
      ) +
      ggplot2::theme_minimal()
    
    # Memory performance  
    p2 <- ggplot2::ggplot(
      large_summary_flat, 
      ggplot2::aes(x = n, y = memory_mean, color = interaction(generator, config))
    ) +
      ggplot2::geom_point() +
      ggplot2::geom_line() +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = memory_mean - memory_sd, ymax = memory_mean + memory_sd), width = 0.1) +
      ggplot2::scale_x_log10(labels = scales::comma) +
      ggplot2::labs(
        x = "Sample Size (n)",
        y = "Memory Usage (MB)",
        color = "Generator/Config",
        title = "RNG Memory Usage for Large Sample Sizes"
      ) +
      ggplot2::theme_minimal()
    
    print(p1)
    print(p2)
  }
  
  return(list(
    raw = large_results,
    summary = large_summary_flat
  ))
}

#' Run a comprehensive benchmark suite and generate a report
#' 
#' Executes a full suite of benchmarks including standard performance tests,
#' configuration profiling, and large-scale generation tests. The function
#' also generates an attractive HTML report with visualizations and detailed
#' performance metrics for easy sharing and analysis.
#' 
#' This is the recommended entry point for comprehensive benchmarking as it
#' combines multiple benchmark types into a single workflow.
#' 
#' @param output_dir Directory to save results and report
#' @param distributions Distributions to test (e.g., c("uniform", "normal"))
#' @param generators List of generators to compare, each taking a single argument n
#' @param create_html Whether to create an HTML report with visualizations
#' @return A list containing all benchmark results and paths to the report and log files
#' @examples
#' \dontrun{
#' # Initialize QIPRNG
#' createPRNG()
#' 
#' # Run a comprehensive benchmark suite
#' benchmark_results <- benchmark_suite(
#'   output_dir = "qiprng_benchmarks",
#'   distributions = c("uniform", "normal")
#' )
#' 
#' # Open the generated HTML report
#' if (!is.null(benchmark_results$report_file)) {
#'   browseURL(benchmark_results$report_file)
#' }
#' }
#' @export
benchmark_suite <- function(output_dir = "qiprng_benchmark",
                          distributions = c("uniform", "normal", "exponential"),
                          generators = list(
                            "qiprng" = function(n) generatePRNG(n),
                            "base_r" = function(n) runif(n)
                          ),
                          create_html = TRUE) {
  
  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Check for required packages
  if (create_html && !requireNamespace("rmarkdown", quietly = TRUE)) {
    warning("Package 'rmarkdown' is needed for HTML report generation. Skipping report creation.")
    create_html <- FALSE
  }
  
  # Run time/date for report
  run_time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  
  # Set up configurations to test
  configs <- list(
    "default" = list(),
    "crypto" = list(use_crypto_mixing = TRUE),
    "high_precision" = list(mpfr_precision = 128),
    "large_buffer" = list(buffer_size = 10000)
  )
  
  # Log file for capturing output
  log_file <- file.path(output_dir, "benchmark_log.txt")
  sink(log_file, split = TRUE)
  
  cat("QIPRNG Benchmark Suite\n")
  cat("=====================\n")
  cat("Run time:", run_time, "\n\n")
  
  # Standard benchmarks
  cat("Running standard benchmarks...\n")
  std_results <- benchmark_qiprng(
    n_values = c(100, 1000, 10000, 100000),
    generators = generators,
    distributions = distributions,
    configs = configs,
    export_data = TRUE,
    file = file.path(output_dir, "standard_benchmark.rds")
  )
  
  # Save plots
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    for (plot_name in names(std_results$plots)) {
      cat(sprintf("Saving plot: %s\n", plot_name))
      plot_file <- file.path(output_dir, sprintf("%s.png", plot_name))
      ggplot2::ggsave(plot_file, std_results$plots[[plot_name]], width = 10, height = 7)
    }
  }
  
  # Configuration profiling
  cat("\nRunning configuration profiling...\n")
  profile_results <- profile_qiprng_config(
    config_list = configs,
    n = 50000
  )
  saveRDS(profile_results, file.path(output_dir, "profile_results.rds"))
  
  # Large scale benchmarks (limited)
  cat("\nRunning large-scale generation test...\n")
  large_results <- benchmark_large_generation(
    n_values = c(1e6, 5e6),
    configs = list(
      "default" = list(),
      "large_buffer" = list(buffer_size = 1e6)
    ),
    generators = generators,
    repetitions = 2
  )
  saveRDS(large_results, file.path(output_dir, "large_benchmark.rds"))
  
  # End log capture
  sink()
  
  # Generate HTML report if requested
  report_file <- NULL
  if (create_html && requireNamespace("rmarkdown", quietly = TRUE)) {
    # Create a temporary Rmd file
    rmd_file <- file.path(output_dir, "benchmark_report.Rmd")
    
    # Write Rmd content
    rmd_content <- c(
      "---",
      "title: \"QIPRNG Benchmark Report\"",
      sprintf("date: \"%s\"", run_time),
      "output: html_document",
      "---",
      "",
      "```{r setup, include=FALSE}",
      "knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE)",
      "library(ggplot2)",
      sprintf("std_results <- readRDS('%s')", file.path(output_dir, "standard_benchmark.rds")),
      sprintf("profile_results <- readRDS('%s')", file.path(output_dir, "profile_results.rds")),
      sprintf("large_results <- readRDS('%s')", file.path(output_dir, "large_benchmark.rds")),
      "```",
      "",
      "## Benchmark Summary",
      "",
      "This report presents performance benchmarks for the QIPRNG package compared to other random number generators.",
      "",
      "### Performance by Sample Size",
      "",
      "```{r}",
      "std_results$plots$size_comparison",
      "```",
      "",
      "### QIPRNG Configuration Comparison",
      "",
      "```{r}",
      "std_results$plots$config_comparison",
      "```",
      "",
      "### Scaling Performance",
      "",
      "```{r}",
      "std_results$plots$scaling",
      "```",
      "",
      "### Configuration Profiling",
      "",
      "```{r}",
      "ggplot(profile_results, aes(x = reorder(config_name, median_time_ms), y = median_time_ms)) +",
      "  geom_bar(stat = 'identity', fill = 'steelblue') +",
      "  geom_errorbar(aes(ymin = min_time_ms, ymax = max_time_ms), width = 0.2) +",
      "  labs(",
      "    x = 'Configuration',",
      "    y = 'Median Time (ms)',",
      "    title = 'QIPRNG Configuration Performance Comparison'",
      "  ) +",
      "  coord_flip() +",
      "  theme_minimal()",
      "```",
      "",
      "### Large-Scale Generation Performance",
      "",
      "```{r}",
      "ggplot(",
      "  large_results$summary, ",
      "  aes(x = n, y = time_mean, color = interaction(generator, config))",
      ") +",
      "  geom_point() +",
      "  geom_line() +",
      "  geom_errorbar(aes(ymin = time_mean - time_sd, ymax = time_mean + time_sd), width = 0.1) +",
      "  scale_x_log10(labels = scales::comma) +",
      "  labs(",
      "    x = 'Sample Size (n)',",
      "    y = 'Mean Execution Time (seconds)',",
      "    color = 'Generator/Config',",
      "    title = 'RNG Execution Time for Large Sample Sizes'",
      "  ) +",
      "  theme_minimal()",
      "```",
      "",
      "### Memory Usage",
      "",
      "```{r}",
      "ggplot(",
      "  large_results$summary, ",
      "  aes(x = n, y = memory_mean, color = interaction(generator, config))",
      ") +",
      "  geom_point() +",
      "  geom_line() +",
      "  geom_errorbar(aes(ymin = memory_mean - memory_sd, ymax = memory_mean + memory_sd), width = 0.1) +",
      "  scale_x_log10(labels = scales::comma) +",
      "  labs(",
      "    x = 'Sample Size (n)',",
      "    y = 'Memory Usage (MB)',",
      "    color = 'Generator/Config',",
      "    title = 'RNG Memory Usage for Large Sample Sizes'",
      "  ) +",
      "  theme_minimal()",
      "```",
      "",
      "## Detailed Results",
      "",
      "### Performance Statistics",
      "",
      "```{r}",
      "knitr::kable(profile_results)",
      "```",
      "",
      "### Large-Scale Generation Statistics",
      "",
      "```{r}",
      "knitr::kable(large_results$summary)",
      "```"
    )
    
    # Write to file
    writeLines(rmd_content, rmd_file)
    
    # Render report
    cat("Generating HTML report...\n")
    report_file <- rmarkdown::render(rmd_file, quiet = TRUE)
    cat(sprintf("Report generated: %s\n", report_file))
  }
  
  return(list(
    standard_results = std_results,
    profile_results = profile_results,
    large_results = large_results,
    report_file = report_file,
    log_file = log_file
  ))
}

#' Compare the quality of random numbers from different generators
#' 
#' Performs statistical quality assessment of different random number generators,
#' comparing their uniformity, independence, and distributional properties.
#' The function runs multiple statistical tests including Kolmogorov-Smirnov,
#' chi-squared, and runs tests, and generates visualizations for comparing
#' the distributions.
#' 
#' This function is particularly useful for evaluating the statistical quality
#' of QIPRNG compared to other generators before using them in simulation or
#' statistical applications.
#' 
#' @param n Sample size to use for quality assessment (larger is more sensitive)
#' @param generators List of generator functions to compare, each taking a single argument n
#' @param save_plots Whether to save visualization plots to files
#' @param output_dir Directory to save plots if save_plots is TRUE
#' @return A list containing quality metrics, visualization plots, and raw samples
#' @examples
#' \dontrun{
#' # Initialize QIPRNG
#' createPRNG()
#' 
#' # Define generators to compare
#' generators <- list(
#'   "qiprng" = function(n) generatePRNG(n),
#'   "base_r" = function(n) runif(n)
#' )
#' 
#' # Run quality comparison
#' quality <- compare_rng_quality(
#'   n = 10000,
#'   generators = generators
#' )
#' 
#' # Examine quality metrics
#' print(quality$metrics)
#' }
#' @export
compare_rng_quality <- function(n = 100000,
                              generators = list(
                                "qiprng" = function(n) generatePRNG(n),
                                "base_r" = function(n) runif(n)
                              ),
                              save_plots = FALSE,
                              output_dir = "qiprng_quality") {
  
  # Check for required packages
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is needed for quality comparison. Please install it.")
  }
  
  # Create output directory if needed
  if (save_plots && !dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Generate samples from each generator
  samples <- list()
  for (gen_name in names(generators)) {
    cat(sprintf("Generating %d samples from %s...\n", n, gen_name))
    samples[[gen_name]] <- generators[[gen_name]](n)
  }
  
  # Quality metrics
  quality_results <- data.frame(
    generator = character(),
    ks_pvalue = numeric(),
    chi_sq_pvalue = numeric(),
    runs_pvalue = numeric(),
    mean = numeric(),
    variance = numeric(),
    min = numeric(),
    max = numeric()
  )
  
  # Calculate quality metrics for each generator
  for (gen_name in names(samples)) {
    cat(sprintf("Calculating quality metrics for %s...\n", gen_name))
    
    x <- samples[[gen_name]]
    
    # Kolmogorov-Smirnov test
    ks_test <- ks.test(x, "punif")
    
    # Chi-square test
    bins <- 100
    breaks <- seq(0, 1, length.out = bins + 1)
    counts <- hist(x, breaks = breaks, plot = FALSE)$counts
    expected <- n / bins
    chi_sq <- sum((counts - expected)^2 / expected)
    chi_sq_pvalue <- 1 - pchisq(chi_sq, bins - 1)
    
    # Runs test
    median_x <- median(x)
    runs_seq <- x > median_x
    runs_count <- sum(runs_seq[-1] != runs_seq[-length(runs_seq)]) + 1
    n1 <- sum(runs_seq)
    n2 <- length(runs_seq) - n1
    expected_runs <- 1 + (2 * n1 * n2) / (n1 + n2)
    var_runs <- (2 * n1 * n2 * (2 * n1 * n2 - n1 - n2)) / 
                ((n1 + n2)^2 * (n1 + n2 - 1))
    runs_z <- (runs_count - expected_runs) / sqrt(var_runs)
    runs_pvalue <- 2 * pnorm(-abs(runs_z))
    
    # Basic statistics
    quality_results <- rbind(quality_results, data.frame(
      generator = gen_name,
      ks_pvalue = ks_test$p.value,
      chi_sq_pvalue = chi_sq_pvalue,
      runs_pvalue = runs_pvalue,
      mean = mean(x),
      variance = var(x),
      min = min(x),
      max = max(x)
    ))
  }
  
  # Create a combined dataframe for plotting
  plot_data <- data.frame()
  for (gen_name in names(samples)) {
    plot_data <- rbind(plot_data, data.frame(
      generator = gen_name,
      value = samples[[gen_name]]
    ))
  }
  
  # Create plots
  plots <- list()
  
  # Histogram comparison
  plots$histogram <- ggplot2::ggplot(plot_data, ggplot2::aes(x = value, fill = generator)) +
    ggplot2::geom_histogram(position = "dodge", bins = 50, alpha = 0.7) +
    ggplot2::labs(
      x = "Value",
      y = "Count",
      title = "Distribution of Random Numbers"
    ) +
    ggplot2::theme_minimal()
  
  # QQ-plot
  plots$qqplot <- ggplot2::ggplot(plot_data, ggplot2::aes(sample = value, color = generator)) +
    ggplot2::stat_qq() +
    ggplot2::stat_qq_line() +
    ggplot2::labs(
      x = "Theoretical Quantiles",
      y = "Sample Quantiles",
      title = "Q-Q Plot against Normal Distribution"
    ) +
    ggplot2::theme_minimal()
  
  # Density plot
  plots$density <- ggplot2::ggplot(plot_data, ggplot2::aes(x = value, color = generator)) +
    ggplot2::geom_density() +
    ggplot2::labs(
      x = "Value",
      y = "Density",
      title = "Density of Random Numbers"
    ) +
    ggplot2::theme_minimal()
  
  # Save plots if requested
  if (save_plots) {
    for (plot_name in names(plots)) {
      plot_file <- file.path(output_dir, sprintf("quality_%s.png", plot_name))
      ggplot2::ggsave(plot_file, plots[[plot_name]], width = 10, height = 7)
    }
  }
  
  # Show plots
  for (plot in plots) {
    print(plot)
  }
  
  # Return results
  return(list(
    metrics = quality_results,
    plots = plots,
    samples = samples
  ))
}

