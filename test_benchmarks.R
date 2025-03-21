#!/usr/bin/env Rscript

# QIPRNG Benchmarking Example
# This script demonstrates how to use the benchmarking module and generate reports

# Load required packages
library(qiprng)
source("R/statisticaltests/benchmarking.R")  # Load the benchmarking module
library(ggplot2)
library(microbenchmark)

#' Generate a comprehensive benchmark report using Quarto or RMarkdown
#' @param config The benchmark configuration
#' @param small_bench Small benchmark results
#' @param dist_bench Distribution benchmark results
#' @param config_profile Configuration profile results
#' @param quality_results Quality assessment results
#' @param large_bench Large-scale benchmark results
#' @return Path to the generated report
generate_benchmark_report <- function(config, small_bench, dist_bench, config_profile, quality_results, large_bench) {
  # Create report directory
  report_dir <- file.path(config$output_dir, "report")
  dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Save plots for the report if requested
  if (config$save_visualizations && requireNamespace("ggplot2", quietly = TRUE)) {
    # Save basic comparison plots
    if (!is.null(small_bench$plots)) {
      for (plot_name in names(small_bench$plots)) {
        plot_file <- file.path(report_dir, paste0("small_", plot_name, ".png"))
        ggplot2::ggsave(plot_file, small_bench$plots[[plot_name]], width = 8, height = 6)
      }
    }
    
    # Save distribution benchmark plots
    if (!is.null(dist_bench$plots)) {
      for (plot_name in names(dist_bench$plots)) {
        plot_file <- file.path(report_dir, paste0("dist_", plot_name, ".png"))
        ggplot2::ggsave(plot_file, dist_bench$plots[[plot_name]], width = 8, height = 6)
      }
    }
    
    # Save quality assessment plots if available
    if (!is.null(quality_results$plots)) {
      for (plot_name in names(quality_results$plots)) {
        plot_file <- file.path(report_dir, paste0("quality_", plot_name, ".png"))
        ggplot2::ggsave(plot_file, quality_results$plots[[plot_name]], width = 8, height = 6)
      }
    }
  }
  
  # Determine report format
  # Access global variables
  has_report_packages_var <- if(exists("has_report_packages")) has_report_packages else FALSE
  has_quarto_var <- if(exists("has_quarto")) has_quarto else FALSE
  
  use_quarto <- has_quarto_var && require("quarto", quietly = TRUE)
  
  if (use_quarto) {
    # Create Quarto document
    quarto_file <- file.path(report_dir, "benchmark_report.qmd")
    generate_quarto_report(quarto_file, config, report_dir)
    
    # Render the Quarto report
    log_message("Rendering Quarto report...\n")
    quarto::quarto_render(quarto_file, output_format = config$report_format)
    report_file <- file.path(report_dir, paste0("benchmark_report.", config$report_format))
  } else if (has_report_packages_var) {
    # Create R Markdown document as fallback
    rmd_file <- file.path(report_dir, "benchmark_report.Rmd")
    generate_rmd_report(rmd_file, config, report_dir)
    
    # Render the R Markdown report
    log_message("Rendering R Markdown report...\n")
    output_format <- switch(config$report_format,
                          "pdf" = "pdf_document",
                          "html" = "html_document",
                          "docx" = "word_document",
                          "html_document")
    rmarkdown::render(rmd_file, output_format = output_format)
    report_file <- file.path(report_dir, paste0("benchmark_report.", config$report_format))
  } else {
    log_message("Cannot generate report: neither Quarto nor RMarkdown is available\n")
    return(NULL)
  }
  
  log_message(paste0("Report generated: ", report_file, "\n"))
  return(report_file)
}

#' Generate Quarto report content
#' @param file_path Path to write the Quarto document
#' @param config The benchmark configuration
#' @param report_dir Directory where report assets are stored
#' @keywords internal
generate_quarto_report <- function(file_path, config, report_dir) {
  quarto_content <- c(
    "---",
    paste0("title: \"", config$report_title, "\""),
    paste0("date: \"", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\""),
    "format:",
    paste0("  ", config$report_format, ":"),
    "    toc: true",
    "    number-sections: true",
    "    highlight-style: github",
    "---",
    "",
    "```{r setup, include=FALSE}",
    "knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)",
    "library(ggplot2)",
    "library(knitr)",
    "if (requireNamespace('kableExtra', quietly = TRUE)) library(kableExtra)",
    "if (requireNamespace('dplyr', quietly = TRUE)) library(dplyr)",
    "```",
    "",
    "# Executive Summary",
    "",
    "This report presents comprehensive benchmarking results for the QIPRNG package compared to other random number generators in R. The benchmarks evaluate performance across different:",
    "",
    "- Sample sizes",
    "- PRNG configurations",
    "- Probability distributions",
    "- Quality metrics",
    "",
    "# Methodology",
    "",
    "The benchmarking suite performs the following tests:",
    "",
    "1. **Basic Performance:** Compares QIPRNG against standard R RNGs across multiple sample sizes",
    "2. **Configuration Testing:** Evaluates different QIPRNG parameter configurations",
    "3. **Distribution Performance:** Tests generation speed for different probability distributions",
    "4. **Large-Scale Generation:** Assesses performance for large sample sizes",
    "5. **Quality Assessment:** Analyzes statistical properties of generated numbers",
    "",
    "# Performance Results",
    "",
    "## Sample Size Comparison",
    "",
    "![Performance by Sample Size](small_size_comparison.png)",
    "",
    "## QIPRNG Configuration Comparison",
    "",
    "![Configuration Comparison](small_config_comparison.png)",
    "",
    "## Distribution Performance",
    "",
    "![Distribution Performance](dist_scaling.png)",
    "",
    "## Statistical Quality",
    "",
    "The following metrics were used to assess the statistical quality of each RNG:",
    "",
    "- **Uniformity:** Chi-square test for uniform distribution",
    "- **Independence:** Tests for serial correlation",
    "- **Period Length:** Estimation of repetition cycle",
    "",
    "# Conclusions and Recommendations",
    "",
    "Based on the benchmark results, the following conclusions can be drawn:",
    "",
    "1. QIPRNG shows competitive performance compared to base R for [small/medium/large] sample sizes.",
    "2. The most efficient QIPRNG configuration for general use is the [default/large buffer/other] configuration.",
    "3. For applications requiring the highest quality random numbers, the [crypto-mixed] configuration is recommended.",
    "4. Resource-constrained applications should consider [specific recommendation].",
    "",
    "## Appendix: Detailed Results",
    "",
    "```{r}",
    "# Display detailed performance metrics as tables",
    "if (exists('config_profile') && is.data.frame(config_profile)) {",
    "  kable(config_profile, caption = 'Configuration Profile Results')",
    "}",
    "```"
  )
  
  # Write to file
  writeLines(quarto_content, file_path)
  log_message(paste0("Quarto report content written to: ", file_path, "\n"))
}

#' Generate R Markdown report content as fallback
#' @param file_path Path to write the R Markdown document
#' @param config The benchmark configuration
#' @param report_dir Directory where report assets are stored
#' @keywords internal
generate_rmd_report <- function(file_path, config, report_dir) {
  rmd_content <- c(
    "---",
    paste0("title: \"", config$report_title, "\""),
    paste0("date: \"", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\""),
    "output:",
    paste0("  ", switch(config$report_format,
                     "pdf" = "pdf_document",
                     "html" = "html_document",
                     "docx" = "word_document",
                     "html_document"), ":"),
    "    toc: true",
    "    number_sections: true",
    "---",
    "",
    "```{r setup, include=FALSE}",
    "knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)",
    "library(ggplot2)",
    "library(knitr)",
    "```",
    "",
    "# Executive Summary",
    "",
    "This report presents benchmark results for the QIPRNG package compared to other random number generators.",
    "",
    "# Performance Results",
    "",
    "## Sample Size Comparison",
    "",
    "![Performance by Sample Size](small_size_comparison.png)",
    "",
    "## Configuration Comparison",
    "",
    "![Configuration Comparison](small_config_comparison.png)",
    "",
    "## Statistical Quality",
    "",
    "```{r}",
    "# Display detailed performance metrics as tables",
    "if (exists('config_profile') && is.data.frame(config_profile)) {",
    "  kable(config_profile, caption = 'Configuration Profile Results')",
    "}",
    "```"
  )
  
  # Write to file
  writeLines(rmd_content, file_path)
  log_message(paste0("R Markdown report content written to: ", file_path, "\n"))
}

# Check for additional packages needed for reporting
if (requireNamespace("knitr", quietly = TRUE) && requireNamespace("rmarkdown", quietly = TRUE)) {
  library(knitr)
  library(rmarkdown)
  has_report_packages <- TRUE
} else {
  has_report_packages <- FALSE
  warning("Packages 'knitr' and 'rmarkdown' are recommended for report generation.")
}

# Check for quarto
has_quarto <- FALSE
try({
  if (requireNamespace("quarto", quietly = TRUE)) {
    has_quarto <- TRUE
  } else {
    # Try system quarto command
    quarto_check <- try(system("quarto --version", intern = TRUE), silent = TRUE)
    if (!inherits(quarto_check, "try-error")) {
      has_quarto <- TRUE
    }
  }
}, silent = TRUE)

if (!has_quarto) {
  warning("Quarto not found. Report generation will use RMarkdown instead.")
}

# Set a seed for reproducibility
set.seed(42)

# Benchmark configuration
BENCHMARK_CONFIG <- list(
  output_dir = "test_results",
  save_results = TRUE,
  save_visualizations = TRUE,
  generate_report = TRUE,
  report_format = "pdf",    # 'pdf', 'html', or 'docx'
  report_title = "QIPRNG Benchmarking Report",
  log_file = "test_results/benchmark_log.txt"  # File to save benchmark output log
)

# Ensure output directory exists for all results
if (!dir.exists(BENCHMARK_CONFIG$output_dir)) {
  dir.create(BENCHMARK_CONFIG$output_dir, recursive = TRUE)
  cat("Created output directory:", BENCHMARK_CONFIG$output_dir, "\n")
}

# Ensure log directory exists (may be the same as output_dir)
log_dir <- dirname(BENCHMARK_CONFIG$log_file)
if (!dir.exists(log_dir) && log_dir != "") {
  dir.create(log_dir, recursive = TRUE)
  cat("Created log directory:", log_dir, "\n")
}

# Create or open the log file in write mode with immediate output
log_file <- file(BENCHMARK_CONFIG$log_file, open = "w")

# Function to write to both console and log file
log_message <- function(msg) {
  # Write to console
  cat(msg)
  
  # Write to log file and flush immediately to ensure it's written
  tryCatch({
    cat(msg, file = log_file, append = TRUE)
    flush(log_file)
  }, error = function(e) {
    cat("ERROR: Failed to write to log file: ", e$message, "\n")
  })
}

# Add a closing handler to ensure file is closed when script ends
on.exit({
  tryCatch({
    if (exists("log_file") && isOpen(log_file)) {
      cat("\n\nBenchmarking completed. Closing log file.\n", file = log_file)
      flush(log_file)
      close(log_file)
      cat("Log written to: ", BENCHMARK_CONFIG$log_file, "\n")
    }
  }, error = function(e) {
    cat("Error closing log file: ", e$message, "\n")
  })
})

# Redirect output to both console and log file
log_message("QIPRNG Benchmarking Example\n")
log_message("==========================\n\n")

# QIPRNG will be initialized when createPRNG is called
tryCatch({
  log_message(paste0("Using qiprng package version: ", as.character(packageVersion("qiprng")), "\n"))
}, error = function(e) {
  log_message("Could not determine qiprng package version\n")
})

# Define generators to compare
generators <- list(
  "qiprng_standard" = function(n) {
    tryCatch({
      # Create a new PRNG instance with uniform distribution
      qiprng::createPRNG(list(distribution = "uniform_01", use_crypto_mixing = FALSE))
      # Generate the numbers
      qiprng::generatePRNG(n)
    }, error = function(e) {
      log_message(paste0("QIPRNG Standard error: ", e$message, "\n"))
      # Fallback to base R if QIPRNG fails
      stats::runif(n)
    })
  },
  "qiprng_crypto" = function(n) {
    tryCatch({
      # Create a new PRNG instance with uniform distribution and crypto mixing
      qiprng::createPRNG(list(distribution = "uniform_01", use_crypto_mixing = TRUE))
      # Generate the numbers
      qiprng::generatePRNG(n)
    }, error = function(e) {
      log_message(paste0("QIPRNG Crypto error: ", e$message, "\n"))
      # Fallback to base R if QIPRNG fails
      stats::runif(n)
    })
  },
  "base_r" = function(n) stats::runif(n),
  
  # A basic linear congruential generator (LCG)
  "lcg" = function(n, seed = 12345) {
    a <- 1664525
    c <- 1013904223
    m <- 2^32
    
    # Initialize
    set.seed(seed)
    x <- as.numeric(sample.int(.Machine$integer.max, 1) %% m)
    result <- numeric(n)
    
    # Generate sequence
    for (i in seq_len(n)) {
      x <- (a * x + c) %% m
      result[i] <- x / m
    }
    
    return(result)
  },
  
  # A poor PRNG for comparison
  "poor" = function(n, seed = 12345) {
    set.seed(seed)
    # This PRNG has very poor randomness properties as it
    # simply repeats every 10 values with small variations
    base <- rep(seq(0, 0.9, by = 0.1), length.out = n)
    noise <- runif(n, -0.01, 0.01)
    result <- base + noise
    # Ensure values stay in [0,1]
    result[result > 1] <- 1
    result[result < 0] <- 0
    return(result)
  }
)

# Add dqrng to the comparison with error handling
generators$dqrng <- function(n) {
  if (requireNamespace("dqrng", quietly = TRUE)) {
    tryCatch({
      dqrng::dqrunif(n)
    }, error = function(e) {
      log_message(paste0("dqrng error: ", e$message, "\n"))
      # Fallback to base R
      stats::runif(n)
    })
  } else {
    log_message("Package 'dqrng' not available, using base R instead\n")
    stats::runif(n)
  }
}
log_message(paste0("Using generators: ", paste(names(generators), collapse=", "), "\n"))

# Define test configurations
configs <- list(
  "default" = list(),
  "crypto" = list(use_crypto_mixing = TRUE),
  "high_precision" = list(mpfr_precision = 128),
  "large_buffer" = list(buffer_size = 10000)
)

log_message("\n1. Basic QIPRNG Performance Comparison\n")
log_message("-------------------------------------\n")
# Compare performance for small sample sizes
small_bench <- tryCatch({
  log_message("Running basic performance comparison...\n")
  
  # Define simple benchmark test
  for (n_val in c(100, 1000, 10000)) {
    log_message(sprintf("  n = %d: ", n_val))
    tryCatch({
      # Test each generator manually
      for (name in names(generators)) {
        # Just run each generator once to verify it works
        result <- generators[[name]](n_val)
        if (length(result) != n_val) {
          stop(paste0("Generator ", name, " returned incorrect number of values"))
        }
      }
      log_message("OK\n")
    }, error = function(e) {
      log_message(paste0("ERROR (details: ", e$message, ")\n"))
    })
  }
  
  # Run the full benchmarking function if basic tests pass
  log_message("Benchmarking uniform distribution...\n")
  
  # Create a modified generator list with explicit namespace handling
  benchmark_generators <- list(
    "qiprng" = function(n) {
      tryCatch({
        qiprng::createPRNG(list(distribution = "uniform_01"))
        qiprng::generatePRNG(n)
      }, error = function(e) {
        log_message(paste0("QIPRNG error: ", e$message, "\n"))
        stats::runif(n)
      })
    },
    "base_r" = function(n) stats::runif(n)
  )
  
  # Add dqrng if available
  if (requireNamespace("dqrng", quietly = TRUE)) {
    benchmark_generators$dqrng <- function(n) {
      tryCatch({
        dqrng::dqrunif(n)
      }, error = function(e) {
        log_message(paste0("dqrng error: ", e$message, "\n"))
        stats::runif(n)
      })
    }
  }
  
  bench_result <- tryCatch({
    benchmark_qiprng(
      n_values = c(100, 1000, 10000, 100000),
      generators = benchmark_generators,
      distributions = c("uniform"),
      repetitions = 5,
      configs = list("default" = list()),
      export_data = FALSE
    )
  }, error = function(e) {
    log_message(paste0("Benchmark error: ", e$message, "\n"))
    NULL
  })
  log_message("Basic performance comparison completed\n")
  bench_result
}, error = function(e) {
  log_message(paste0("ERROR in basic benchmark: ", e$message, "\n"))
  list(results = list(), summary = data.frame(), plots = list())
})

log_message("\n2. Testing Different QIPRNG Configurations\n")
log_message("----------------------------------------\n")
# Compare different QIPRNG configurations
config_profile <- tryCatch({
  log_message("Testing different configuration profiles...\n")
  profile_result <- profile_qiprng_config(
    config_list = configs,
    n = 1000000,
    repetitions = 5,
    output_dir = BENCHMARK_CONFIG$output_dir
  )
  log_message("Configuration profile testing completed\n")
  profile_result
}, error = function(e) {
  log_message(paste0("ERROR in configuration profiling: ", e$message, "\n"))
  list(results = list(), summary = data.frame(), plots = list())
})

log_message("\n3. Distribution Performance Comparison\n")
log_message("------------------------------------\n")
# Ensure QIPRNG is properly loaded for distribution testing
library(qiprng)

# Focus on uniform distribution with both crypto and non-crypto
log_message("\nBenchmarking with and without crypto mixing...\n")

# Define simple benchmarking config that will work
uniforms_bench <- tryCatch({
  # Use the same generators we defined earlier to ensure consistency
  uniform_generators <- list()
  
  # Make sure all required generators are included
  # Standard qiprng
  uniform_generators$"qiprng_standard" <- function(n) {
    tryCatch({
      # Ensure QIPRNG system is initialized
      if (!requireNamespace("qiprng", quietly = TRUE)) {
        stop("qiprng package not available")
      }
      # Direct call to createPRNG with error handling
      qiprng::createPRNG(list(distribution = "uniform_01", use_crypto_mixing = FALSE))
      qiprng::generatePRNG(n)
    }, error = function(e) {
      log_message(paste0("Standard QIPRNG error: ", e$message, "\n"))
      # Fallback to base R
      stats::runif(n)
    })
  }
  
  # Crypto qiprng
  uniform_generators$"qiprng_crypto" <- function(n) {
    tryCatch({
      # Ensure QIPRNG system is initialized
      if (!requireNamespace("qiprng", quietly = TRUE)) {
        stop("qiprng package not available")
      }
      # Direct call to createPRNG with error handling
      qiprng::createPRNG(list(distribution = "uniform_01", use_crypto_mixing = TRUE))
      qiprng::generatePRNG(n)
    }, error = function(e) {
      log_message(paste0("Crypto QIPRNG error: ", e$message, "\n"))
      # Fallback to base R
      stats::runif(n)
    })
  }
  
  # Base R
  uniform_generators$"base_r" <- function(n) stats::runif(n)
  
  # dqrng generator with error handling
  uniform_generators$"dqrng" <- function(n) {
    if (requireNamespace("dqrng", quietly = TRUE)) {
      tryCatch({
        dqrng::dqrunif(n)
      }, error = function(e) {
        log_message(paste0("dqrng error: ", e$message, "\n"))
        # Fallback to base R
        stats::runif(n)
      })
    } else {
      log_message("Package 'dqrng' not available, using base R instead\n")
      stats::runif(n)
    }
  }
  
  # A basic linear congruential generator (LCG)
  uniform_generators$"lcg" <- function(n, seed = 12345) {
    a <- 1664525
    c <- 1013904223
    m <- 2^32
    
    # Initialize
    set.seed(seed)
    x <- as.numeric(sample.int(.Machine$integer.max, 1) %% m)
    result <- numeric(n)
    
    # Generate sequence
    for (i in seq_len(n)) {
      x <- (a * x + c) %% m
      result[i] <- x / m
    }
    
    return(result)
  }
  
  # A poor PRNG for comparison
  uniform_generators$"poor" <- function(n, seed = 12345) {
    set.seed(seed)
    # This PRNG has very poor randomness properties as it
    # simply repeats every 10 values with small variations
    base <- rep(seq(0, 0.9, by = 0.1), length.out = n)
    noise <- runif(n, -0.01, 0.01)
    result <- base + noise
    # Ensure values stay in [0,1]
    result[result > 1] <- 1
    result[result < 0] <- 0
    return(result)
  }
  
  # Log the generators being used
  log_message(paste0("Using generators: ", paste(names(uniform_generators), collapse=", "), "\n"))
  
  # Run simple benchmark manually to ensure it works
  results <- list()
  all_times <- list()
  
  # Test sample size
  n <- 1000000
  log_message(sprintf("Benchmarking with n=%d...\n", n))
  
  for (gen_name in names(uniform_generators)) {
    log_message(sprintf("  Testing %s: ", gen_name))
    gen_func <- uniform_generators[[gen_name]]
    
    # Run benchmark with error handling
    bench_result <- tryCatch({
      # Use system.time for more reliable benchmarking
      times <- replicate(5, {
        start_time <- Sys.time()
        result <- gen_func(n)
        end_time <- Sys.time()
        as.numeric(difftime(end_time, start_time, units = "secs")) * 1000  # ms
      })
      log_message("done\n")
      times
    }, error = function(e) {
      log_message(paste0("ERROR: ", e$message, "\n"))
      NULL
    })
    
    if (!is.null(bench_result)) {
      all_times[[gen_name]] <- bench_result
    }
  }
  
  # Create simple comparison data
  if (length(all_times) > 0) {
    # Create summary statistics
    summary_data <- data.frame(
      generator = names(all_times),
      mean_time_ms = sapply(all_times, mean),
      median_time_ms = sapply(all_times, median),
      min_time_ms = sapply(all_times, min),
      max_time_ms = sapply(all_times, max),
      stringsAsFactors = FALSE
    )
    
    # Simple bar plot if ggplot2 is available
    plots <- list()
    if (requireNamespace("ggplot2", quietly = TRUE)) {
      p <- ggplot2::ggplot(summary_data, ggplot2::aes(x = generator, y = median_time_ms)) +
        ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
        ggplot2::labs(title = "PRNG Performance Comparison",
                      x = "Generator", y = "Median Time (ms)") +
        ggplot2::theme_minimal()
      plots$performance_bar <- p
    }
    
    list(results = all_times, summary = summary_data, plots = plots)
  } else {
    list(results = list(), summary = data.frame(), plots = list())
  }
}, error = function(e) {
  log_message(paste0("ERROR in benchmarking: ", e$message, "\n"))
  list(results = list(), summary = data.frame(), plots = list())
})

# Format the results to match what create_benchmark_plots expects
all_results <- list()

# Use basic structure needed for the plotting function
if (length(uniforms_bench$results) > 0) {
  # Create a synthetic microbenchmark-like structure for each generator
  for (gen_name in names(uniforms_bench$results)) {
    # Create uniform_01_default_10000 style key that the plotting function expects
    result_key <- paste0("uniform_01_default_10000")
    
    # Convert our timing data to microbenchmark-like format
    times_ns <- uniforms_bench$results[[gen_name]] * 1000000  # Convert ms to ns
    mb_like_data <- list(
      time = times_ns,  # time in nanoseconds
      expr = rep(gen_name, length(times_ns)),
      distribution = rep("uniform_01", length(times_ns)),
      config = rep("default", length(times_ns)),
      n = rep(1000000, length(times_ns))
    )
    
    all_results[[result_key]] <- mb_like_data
  }
}

# Use the formatted data for the distribution benchmark
dist_bench <- list(
  results = all_results,
  plots = uniforms_bench$plots
)

log_message("\n4. Large-Scale Generation Test\n")
log_message("----------------------------\n")
# Test large-scale generation
large_bench <- benchmark_large_generation(
  n_values = c(1e6, 5e6),
  configs = list(
    "default" = list(),
    "large_buffer" = list(buffer_size = 1000000)
  ),
  generators = generators,
  repetitions = 2
)

log_message("\n5. Quality Assessment\n")
log_message("-------------------\n")
# Compare statistical quality
quality_results <- compare_rng_quality(
  n = 5000000,
  generators = generators,
  save_plots = FALSE
)

log_message("\n6. Summary of Results\n")
log_message("------------------\n")
log_message("Configuration Profile Results:\n")
print(config_profile)

log_message("\nQuality Metrics:\n")
print(quality_results$metrics)

log_message("\nLarge-Scale Generation Summary:\n")
print(large_bench$summary)

# Create results directory if it doesn't exist
if (!dir.exists(BENCHMARK_CONFIG$output_dir)) {
  dir.create(BENCHMARK_CONFIG$output_dir, recursive = TRUE, showWarnings = FALSE)
}

# Save results and visualizations if requested
if (BENCHMARK_CONFIG$save_results) {
  saveRDS(config_profile, file.path(BENCHMARK_CONFIG$output_dir, "config_profile.rds"))
  saveRDS(quality_results, file.path(BENCHMARK_CONFIG$output_dir, "quality_results.rds"))
  saveRDS(large_bench, file.path(BENCHMARK_CONFIG$output_dir, "large_bench.rds"))
}

# Generate benchmark report if requested
if (BENCHMARK_CONFIG$generate_report) {
  log_message("\n7. Generating Benchmark Report\n")
  log_message("----------------------------\n")
  generate_benchmark_report(BENCHMARK_CONFIG, small_bench, dist_bench, config_profile, quality_results, large_bench)
}

# This function has been moved to the beginning of the file
# Keeping this comment to maintain the original line structure
  # Create report directory
  report_dir <- file.path(config$output_dir, "report")
  dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Save plots for the report if requested
  if (config$save_visualizations && requireNamespace("ggplot2", quietly = TRUE)) {
    # Save basic comparison plots
    if (!is.null(small_bench$plots)) {
      for (plot_name in names(small_bench$plots)) {
        plot_file <- file.path(report_dir, paste0("small_", plot_name, ".png"))
        ggplot2::ggsave(plot_file, small_bench$plots[[plot_name]], width = 8, height = 6)
      }
    }
    
    # Save distribution benchmark plots
    if (!is.null(dist_bench$plots)) {
      for (plot_name in names(dist_bench$plots)) {
        plot_file <- file.path(report_dir, paste0("dist_", plot_name, ".png"))
        ggplot2::ggsave(plot_file, dist_bench$plots[[plot_name]], width = 8, height = 6)
      }
    }
    
    # Save quality assessment plots if available
    if (!is.null(quality_results$plots)) {
      for (plot_name in names(quality_results$plots)) {
        plot_file <- file.path(report_dir, paste0("quality_", plot_name, ".png"))
        ggplot2::ggsave(plot_file, quality_results$plots[[plot_name]], width = 8, height = 6)
      }
    }
  }
  
  # Determine report format
  # Access global variables
  has_report_packages_var <- if(exists("has_report_packages")) has_report_packages else FALSE
  has_quarto_var <- if(exists("has_quarto")) has_quarto else FALSE
  
  use_quarto <- has_quarto_var && require("quarto", quietly = TRUE)
  
  if (use_quarto) {
    # Create Quarto document
    quarto_file <- file.path(report_dir, "benchmark_report.qmd")
    generate_quarto_report(quarto_file, config, report_dir)
    
    # Render the Quarto report
    log_message("Rendering Quarto report...\n")
    quarto::quarto_render(quarto_file, output_format = config$report_format)
    report_file <- file.path(report_dir, paste0("benchmark_report.", config$report_format))
  } else if (has_report_packages_var) {
    # Create R Markdown document as fallback
    rmd_file <- file.path(report_dir, "benchmark_report.Rmd")
    generate_rmd_report(rmd_file, config, report_dir)
    
    # Render the R Markdown report
    log_message("Rendering R Markdown report...\n")
    output_format <- switch(config$report_format,
                           "pdf" = "pdf_document",
                           "html" = "html_document",
                           "docx" = "word_document",
                           "html_document")
    rmarkdown::render(rmd_file, output_format = output_format)
    report_file <- file.path(report_dir, paste0("benchmark_report.", config$report_format))
  } else {
    log_message("Cannot generate report: neither Quarto nor RMarkdown is available\n")
    return(NULL)
  }
  
  log_message(paste0("Report generated: ", report_file, "\n"))
  return(report_file)
}

#' Generate Quarto report content
#' @param file_path Path to write the Quarto document
#' @param config The benchmark configuration
#' @param report_dir Directory where report assets are stored
#' @keywords internal
generate_quarto_report <- function(file_path, config, report_dir) {
  quarto_content <- c(
    "---",
    paste0("title: \"", config$report_title, "\""),
    paste0("date: \"", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\""),
    "format:",
    paste0("  ", config$report_format, ":"),
    "    toc: true",
    "    number-sections: true",
    "    highlight-style: github",
    "---",
    "",
    "```{r setup, include=FALSE}",
    "knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)",
    "library(ggplot2)",
    "library(knitr)",
    "if (requireNamespace('kableExtra', quietly = TRUE)) library(kableExtra)",
    "if (requireNamespace('dplyr', quietly = TRUE)) library(dplyr)",
    "```",
    "",
    "# Executive Summary",
    "",
    "This report presents comprehensive benchmarking results for the QIPRNG package compared to other random number generators in R. The benchmarks evaluate performance across different:",
    "",
    "- Sample sizes",
    "- PRNG configurations",
    "- Probability distributions",
    "- Quality metrics",
    "",
    "# Methodology",
    "",
    "The benchmarking suite performs the following tests:",
    "",
    "1. **Basic Performance:** Compares QIPRNG against standard R RNGs across multiple sample sizes",
    "2. **Configuration Testing:** Evaluates different QIPRNG parameter configurations",
    "3. **Distribution Performance:** Tests generation speed for different probability distributions",
    "4. **Large-Scale Generation:** Assesses performance for large sample sizes",
    "5. **Quality Assessment:** Analyzes statistical properties of generated numbers",
    "",
    "# Performance Results",
    "",
    "## Sample Size Comparison",
    "",
    "![Performance by Sample Size](small_size_comparison.png)",
    "",
    "## QIPRNG Configuration Comparison",
    "",
    "![Configuration Comparison](small_config_comparison.png)",
    "",
    "## Distribution Performance",
    "",
    "![Distribution Performance](dist_scaling.png)",
    "",
    "## Statistical Quality",
    "",
    "The following metrics were used to assess the statistical quality of each RNG:",
    "",
    "- **Uniformity:** Chi-square test for uniform distribution",
    "- **Independence:** Tests for serial correlation",
    "- **Period Length:** Estimation of repetition cycle",
    "",
    "# Conclusions and Recommendations",
    "",
    "Based on the benchmark results, the following conclusions can be drawn:",
    "",
    "1. QIPRNG shows competitive performance compared to base R for [small/medium/large] sample sizes.",
    "2. The most efficient QIPRNG configuration for general use is the [default/large buffer/other] configuration.",
    "3. For applications requiring the highest quality random numbers, the [crypto-mixed] configuration is recommended.",
    "4. Resource-constrained applications should consider [specific recommendation].",
    "",
    "## Appendix: Detailed Results",
    "",
    "```{r}",
    "# Display detailed performance metrics as tables",
    "if (exists('config_profile') && is.data.frame(config_profile)) {",
    "  kable(config_profile, caption = 'Configuration Profile Results')",
    "}",
    "```"
  )
  
  # Write to file
  writeLines(quarto_content, file_path)
  log_message(paste0("Quarto report content written to: ", file_path, "\n"))
}

#' Generate R Markdown report content as fallback
#' @param file_path Path to write the R Markdown document
#' @param config The benchmark configuration
#' @param report_dir Directory where report assets are stored
#' @keywords internal
generate_rmd_report <- function(file_path, config, report_dir) {
  rmd_content <- c(
    "---",
    paste0("title: \"", config$report_title, "\""),
    paste0("date: \"", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\""),
    "output:",
    paste0("  ", switch(config$report_format,
                      "pdf" = "pdf_document",
                      "html" = "html_document",
                      "docx" = "word_document",
                      "html_document"), ":"),
    "    toc: true",
    "    number_sections: true",
    "---",
    "",
    "```{r setup, include=FALSE}",
    "knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)",
    "library(ggplot2)",
    "library(knitr)",
    "```",
    "",
    "# Executive Summary",
    "",
    "This report presents benchmark results for the QIPRNG package compared to other random number generators.",
    "",
    "# Performance Results",
    "",
    "## Sample Size Comparison",
    "",
    "![Performance by Sample Size](small_size_comparison.png)",
    "",
    "## Configuration Comparison",
    "",
    "![Configuration Comparison](small_config_comparison.png)",
    "",
    "# Appendix: Detailed Results",
    "",
    "```{r}",
    "# Display detailed performance metrics as tables",
    "if (exists('config_profile') && is.data.frame(config_profile)) {",
    "  kable(config_profile, caption = 'Configuration Profile Results')",
    "}",
    "```"
  )
  
  # Write to file
  writeLines(rmd_content, file_path)
  log_message(paste0("R Markdown report content written to: ", file_path, "\n"))
}

log_message("\nBenchmark Complete! ")
if (BENCHMARK_CONFIG$generate_report) {
  log_message(paste0("View the generated report in '", BENCHMARK_CONFIG$output_dir, "/report' for detailed results.\n"))
} else {
  log_message("View the plots above for visual comparisons.\n")
}

# Write final message
log_message(paste0("\nBenchmarking complete - log saved to: ", BENCHMARK_CONFIG$log_file, "\n"))

# Close the log file when the script is done
close(log_file)