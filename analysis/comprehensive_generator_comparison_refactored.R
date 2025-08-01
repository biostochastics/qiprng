#!/usr/bin/env Rscript

# Comprehensive Random Number Generator Comparison - Refactored Version
# This version uses persistent generator instances to avoid repeated init/cleanup cycles

# Load required packages
suppressPackageStartupMessages({
  library(qiprng)
  library(parallel)
  library(ggplot2)
  library(tidyr)
  library(dplyr)
  library(gridExtra)
  library(scales)
})

# Check for optional packages
check_packages <- function() {
  cat("Checking required packages...\n")
  
  # Check for cryptographic and enhanced RNG packages
  crypto_packages <- c("openssl", "dqrng")
  for (pkg in crypto_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      cat(sprintf("  - Package '%s' not found. Installing...\n", pkg))
      tryCatch({
        install.packages(pkg, quiet = TRUE)
      }, error = function(e) {
        cat(sprintf("    ERROR: Failed to install %s: %s\n", pkg, e$message))
      })
    }
  }
  
  # Libsodium will be initialized when we create the first PRNG
  
  optional_packages <- c("ADGofTest", "randtests", "knitr", "kableExtra", "rmarkdown")
  cat("Checking optional packages...\n")
  
  pkg_status <- list()
  for (pkg in optional_packages) {
    if (requireNamespace(pkg, quietly = TRUE)) {
      cat(sprintf("  - Optional package '%s' found.\n", pkg))
      pkg_status[[pkg]] <- TRUE
    } else {
      cat(sprintf("  - Optional package '%s' not found. Some features will be disabled.\n", pkg))
      pkg_status[[pkg]] <- FALSE
    }
  }
  cat("\n")
  return(pkg_status)
}

# Configuration
SAMPLE_SIZES <- c(1e4, 1e5, 1e6, 5e6)  # Extended sample sizes for more robust testing
N_RUNS <- 100  # Increased number of runs for better statistical power
RESULTS_DIR <- paste0("analysis/results/comprehensive_comparison_", 
                      format(Sys.time(), "%Y%m%d_%H%M%S"))

# Create output directories
dir.create(RESULTS_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(RESULTS_DIR, "plots"), showWarnings = FALSE)
dir.create(file.path(RESULTS_DIR, "tables"), showWarnings = FALSE)
dir.create(file.path(RESULTS_DIR, "data"), showWarnings = FALSE)

# Generator Manager Class - Handles lifecycle of all generators
GeneratorManager <- setRefClass("GeneratorManager",
  fields = list(
    generators = "list",
    active_generator = "character",
    is_initialized = "logical"
  ),
  methods = list(
    initialize = function() {
      generators <<- list()
      active_generator <<- ""
      is_initialized <<- FALSE
    },
    
    add_generator = function(name, display_name, init_func, generate_func) {
      generators[[name]] <<- list(
        name = display_name,
        init = init_func,
        generate = generate_func,
        initialized = FALSE
      )
    },
    
    init_all = function() {
      cat("Initializing all generators...\n")
      r_generators <- c("R_mersenne", "R_lecuyer", "R_wichmann", "R_marsaglia", 
                        "R_superduper", "R_knuth", "R_knuth2002")
      
      for (gen_name in names(generators)) {
        if (!gen_name %in% r_generators) {
          cat(sprintf("  - Initializing %s\n", generators[[gen_name]]$name))
          tryCatch({
            generators[[gen_name]]$init()
            generators[[gen_name]]$initialized <<- TRUE
          }, error = function(e) {
            cat(sprintf("    ERROR: Failed to initialize %s: %s\n", gen_name, e$message))
          })
        } else {
          generators[[gen_name]]$initialized <<- TRUE
        }
      }
      is_initialized <<- TRUE
      cat("Generator initialization complete.\n\n")
    },
    
    set_active = function(name) {
      if (!name %in% names(generators)) {
        stop(sprintf("Generator '%s' not found", name))
      }
      if (!generators[[name]]$initialized && name != "R_default") {
        stop(sprintf("Generator '%s' not initialized", name))
      }
      active_generator <<- name
    },
    
    generate = function(n) {
      if (active_generator == "") {
        stop("No active generator set")
      }
      generators[[active_generator]]$generate(n)
    },
    
    cleanup_all = function() {
      if (is_initialized) {
        cat("\nCleaning up generators...\n")
        # Only call cleanup once at the very end
        tryCatch({
          cleanupPRNG()
          cat("Cleanup complete.\n")
        }, error = function(e) {
          cat("Cleanup encountered an error (non-critical):", e$message, "\n")
        })
        is_initialized <<- FALSE
      }
    },
    
    finalize = function() {
      cleanup_all()
    }
  )
)

# Create global generator manager
gen_manager <- GeneratorManager$new()

# Define generators using the manager
setup_generators <- function() {
  # R Default Generator (Mersenne Twister)
  gen_manager$add_generator(
    "R_mersenne",
    "R Mersenne Twister",
    function() { set.seed(NULL); RNGkind("Mersenne-Twister") },
    function(n) runif(n)
  )
  
  # R L'Ecuyer-CMRG Generator
  gen_manager$add_generator(
    "R_lecuyer",
    "R L'Ecuyer-CMRG",
    function() { set.seed(NULL); RNGkind("L'Ecuyer-CMRG") },
    function(n) runif(n)
  )
  
  # R Wichmann-Hill Generator
  gen_manager$add_generator(
    "R_wichmann",
    "R Wichmann-Hill",
    function() { set.seed(NULL); RNGkind("Wichmann-Hill") },
    function(n) runif(n)
  )
  
  # R Marsaglia-Multicarry Generator
  gen_manager$add_generator(
    "R_marsaglia",
    "R Marsaglia-Multicarry",
    function() { set.seed(NULL); RNGkind("Marsaglia-Multicarry") },
    function(n) runif(n)
  )
  
  # R Super-Duper Generator
  gen_manager$add_generator(
    "R_superduper",
    "R Super-Duper",
    function() { set.seed(NULL); RNGkind("Super-Duper") },
    function(n) runif(n)
  )
  
  # R Knuth-TAOCP Generator
  gen_manager$add_generator(
    "R_knuth",
    "R Knuth-TAOCP",
    function() { set.seed(NULL); RNGkind("Knuth-TAOCP") },
    function(n) runif(n)
  )
  
  # R Knuth-TAOCP-2002 Generator
  gen_manager$add_generator(
    "R_knuth2002",
    "R Knuth-TAOCP-2002",
    function() { set.seed(NULL); RNGkind("Knuth-TAOCP-2002") },
    function(n) runif(n)
  )
  
  # QIPRNG Default Configuration
  prng_default <- NULL
  gen_manager$add_generator(
    "qiprng_default",
    "QIPRNG (Default Config)",
    function() {
      createPRNG()
      prng_default <<- function(n) generatePRNG(n)
    },
    function(n) prng_default(n)
  )
  
  # QIPRNG No Crypto
  prng_nocrypto <- NULL
  gen_manager$add_generator(
    "qiprng_nocrypto",
    "QIPRNG (No Crypto)",
    function() {
      createPRNG(list(use_crypto_mixing = FALSE))
      prng_nocrypto <<- function(n) generatePRNG(n)
    },
    function(n) prng_nocrypto(n)
  )
  
  # QIPRNG With Crypto
  prng_crypto <- NULL
  gen_manager$add_generator(
    "qiprng_crypto",
    "QIPRNG (With Crypto)",
    function() {
      createPRNG(list(use_crypto_mixing = TRUE))
      prng_crypto <<- function(n) generatePRNG(n)
    },
    function(n) prng_crypto(n)
  )
  
  # QIPRNG High Precision
  prng_highprec <- NULL
  gen_manager$add_generator(
    "qiprng_highprec",
    "QIPRNG (High Precision)",
    function() {
      createPRNG(list(mpfr_precision = 256))
      prng_highprec <<- function(n) generatePRNG(n)
    },
    function(n) prng_highprec(n)
  )
  
  # Cryptographically Secure RNG (openssl)
  if (requireNamespace("openssl", quietly = TRUE)) {
    gen_manager$add_generator(
      "crypto_openssl",
      "Crypto (OpenSSL)",
      function() {
        # No initialization needed for openssl
      },
      function(n) {
        # Generate cryptographically secure random bytes and convert to uniform [0,1)
        # Use 4 bytes per number for standard precision
        bytes_needed <- n * 4
        raw_bytes <- openssl::rand_bytes(bytes_needed)
        # Convert bytes to integers more efficiently
        integers <- readBin(raw_bytes, what = "integer", n = n, size = 4, signed = TRUE)
        # Convert to unsigned and then to [0,1)
        # Handle signed integer conversion properly
        unsigned <- ifelse(integers < 0, integers + 2^32, integers)
        return(unsigned / 2^32)
      }
    )
  }
  
  # dqrng generators
  if (requireNamespace("dqrng", quietly = TRUE)) {
    # PCG64 (default dqrng generator)
    gen_manager$add_generator(
      "dqrng_pcg64",
      "dqrng (PCG64)",
      function() {
        dqrng::dqset.seed(NULL)
      },
      function(n) {
        dqrng::dqRNGkind("pcg64")
        dqrng::dqrunif(n)
      }
    )
    
    # Xoroshiro128++
    gen_manager$add_generator(
      "dqrng_xoroshiro128pp",
      "dqrng (Xoroshiro128++)",
      function() {
        dqrng::dqset.seed(NULL)
      },
      function(n) {
        dqrng::dqRNGkind("Xoroshiro128++")
        dqrng::dqrunif(n)
      }
    )
    
    # Xoshiro256++
    gen_manager$add_generator(
      "dqrng_xoshiro256pp",
      "dqrng (Xoshiro256++)",
      function() {
        dqrng::dqset.seed(NULL)
      },
      function(n) {
        dqrng::dqRNGkind("Xoshiro256++")
        dqrng::dqrunif(n)
      }
    )
    
    # Threefry (via sitmo)
    gen_manager$add_generator(
      "dqrng_threefry",
      "dqrng (Threefry)",
      function() {
        dqrng::dqset.seed(NULL)
      },
      function(n) {
        dqrng::dqRNGkind("Threefry")
        dqrng::dqrunif(n)
      }
    )
    
    # Legacy generators for completeness (not recommended)
    gen_manager$add_generator(
      "dqrng_xoroshiro128p",
      "dqrng (Xoroshiro128+)",
      function() {
        dqrng::dqset.seed(NULL)
      },
      function(n) {
        dqrng::dqRNGkind("Xoroshiro128+")
        dqrng::dqrunif(n)
      }
    )
    
    gen_manager$add_generator(
      "dqrng_xoshiro256p",
      "dqrng (Xoshiro256+)",
      function() {
        dqrng::dqset.seed(NULL)
      },
      function(n) {
        dqrng::dqRNGkind("Xoshiro256+")
        dqrng::dqrunif(n)
      }
    )
  }
}

# Validation function
validate_rng_output <- function(x) {
  if (!is.numeric(x) || length(x) == 0) return(FALSE)
  if (any(is.na(x)) || any(is.nan(x)) || any(is.infinite(x))) return(FALSE)
  if (any(x < 0) || any(x > 1)) return(FALSE)
  return(TRUE)
}

# Test implementations
source_test_implementations <- function() {
  # Basic statistical tests
  kolmogorov_smirnov_test <- function(x) {
    tryCatch({
      test <- ks.test(x, "punif")
      return(test$p.value)
    }, error = function(e) NA)
  }
  
  chi_squared_test <- function(x) {
    tryCatch({
      n_bins <- ceiling(sqrt(length(x)))
      breaks <- seq(0, 1, length.out = n_bins + 1)
      observed <- table(cut(x, breaks = breaks, include.lowest = TRUE))
      expected <- length(x) / n_bins
      
      if (any(expected < 5)) return(NA)
      
      chi_stat <- sum((observed - expected)^2 / expected)
      p_value <- pchisq(chi_stat, df = n_bins - 1, lower.tail = FALSE)
      return(p_value)
    }, error = function(e) NA)
  }
  
  runs_test <- function(x) {
    tryCatch({
      median_val <- median(x)
      binary <- as.integer(x > median_val)
      runs <- rle(binary)
      n_runs <- length(runs$lengths)
      n1 <- sum(binary == 1)
      n0 <- sum(binary == 0)
      
      if (n1 == 0 || n0 == 0) return(NA)
      
      expected_runs <- (2 * n1 * n0) / (n1 + n0) + 1
      variance_runs <- (2 * n1 * n0 * (2 * n1 * n0 - n1 - n0)) / 
                       ((n1 + n0)^2 * (n1 + n0 - 1))
      
      if (variance_runs <= 0) return(NA)
      
      z_stat <- (n_runs - expected_runs) / sqrt(variance_runs)
      p_value <- 2 * pnorm(-abs(z_stat))
      return(p_value)
    }, error = function(e) NA)
  }
  
  autocorrelation_test <- function(x, lag = 1) {
    tryCatch({
      n <- length(x)
      if (n < 2 * lag) return(NA)
      
      acf_result <- acf(x, lag.max = lag, plot = FALSE)
      acf_value <- acf_result$acf[lag + 1]
      
      # Standard error for autocorrelation
      se <- 1 / sqrt(n)
      z_stat <- acf_value / se
      p_value <- 2 * pnorm(-abs(z_stat))
      return(p_value)
    }, error = function(e) NA)
  }
  
  # Additional statistical metrics
  compute_moments <- function(x) {
    n <- length(x)
    m1 <- mean(x)
    m2 <- mean((x - m1)^2)
    m3 <- mean((x - m1)^3)
    m4 <- mean((x - m1)^4)
    
    list(
      mean = m1,
      variance = m2,
      skewness = m3 / (m2^(3/2)),
      kurtosis = m4 / (m2^2) - 3,
      expected_mean = 0.5,
      expected_variance = 1/12,
      expected_skewness = 0,
      expected_kurtosis = -1.2
    )
  }
  
  # Spectral test (proper implementation)
  spectral_test <- function(x) {
    tryCatch({
      # Calculate spectrum
      spec_result <- spectrum(x, plot = FALSE, method = "pgram")
      
      # In a random sequence, spectral densities should be approximately exponential
      # We'll use Kolmogorov-Smirnov test to compare with exponential distribution
      spec_values <- spec_result$spec
      scaled_spec <- spec_values / mean(spec_values)
      
      # KS test against exponential(1)
      ks_result <- suppressWarnings(ks.test(scaled_spec, "pexp", 1))
      
      return(ks_result$p.value)
    }, error = function(e) NA)
  }
  
  # Gap test (proper implementation)
  gap_test <- function(x) {
    tryCatch({
      # Define gap range [0.3, 0.7] (default from advanced_tests.R)
      alpha <- 0.3
      beta <- 0.7
      
      # Find positions where values are in range
      in_range <- x >= alpha & x <= beta
      positions <- which(in_range)
      
      if (length(positions) < 2) return(NA)
      
      # Calculate gaps between consecutive occurrences
      gaps <- diff(positions)
      
      if (length(gaps) < 10) return(NA)  # Need sufficient gaps
      
      # Expected gap distribution is geometric with p = beta - alpha
      p <- beta - alpha
      
      # Limit gap size for chi-square test
      max_gap <- min(20, max(gaps))
      gap_counts <- table(factor(pmin(gaps, max_gap), levels = 1:max_gap))
      
      # Expected frequencies based on geometric distribution
      # For gaps of length k=1,2,...,max_gap-1: P(gap=k) = (1-p)^(k-1) * p
      expected_freq <- length(gaps) * (1-p)^((1:max_gap) - 1) * p
      # For the last bin (gaps >= max_gap): sum of all P(gap>=max_gap)
      expected_freq[max_gap] <- length(gaps) * (1-p)^(max_gap - 1)
      
      # Only proceed if expected frequencies are sufficient
      if (any(expected_freq < 5)) {
        # Combine bins to ensure sufficient expected frequencies
        bins_to_keep <- which(expected_freq >= 5)
        if (length(bins_to_keep) < 2) return(NA)
        
        gap_counts <- gap_counts[bins_to_keep]
        expected_freq <- expected_freq[bins_to_keep]
      }
      
      # Perform chi-square test
      chi_stat <- sum((gap_counts - expected_freq)^2 / expected_freq)
      df <- length(gap_counts) - 1
      
      if (df < 1) return(NA)
      
      p_value <- pchisq(chi_stat, df = df, lower.tail = FALSE)
      return(p_value)
    }, error = function(e) NA)
  }
  
  # Return test categories
  list(
    basic_tests = list(
      "Kolmogorov-Smirnov" = kolmogorov_smirnov_test,
      "Chi-squared" = chi_squared_test,
      "Runs Test" = runs_test,
      "Autocorrelation" = autocorrelation_test
    ),
    advanced_tests = list(
      "Spectral Test" = spectral_test,
      "Gap Test" = gap_test
    ),
    moment_function = compute_moments
  )
}

# Benchmark a single generator
benchmark_generator <- function(gen_name, sizes = SAMPLE_SIZES) {
  gen_manager$set_active(gen_name)
  
  results <- data.frame(
    size = sizes,
    time = numeric(length(sizes)),
    rate = numeric(length(sizes))
  )
  
  for (i in seq_along(sizes)) {
    tryCatch({
      time_taken <- system.time({
        sample <- gen_manager$generate(sizes[i])
        if (!validate_rng_output(sample)) {
          stop("Invalid generator output")
        }
      })[3]  # elapsed time
      
      results$time[i] <- time_taken
      results$rate[i] <- sizes[i] / time_taken
    }, error = function(e) {
      warning(sprintf("Benchmarking error for %s size %d: %s", gen_name, sizes[i], e$message))
      results$time[i] <- NA
      results$rate[i] <- NA
    })
  }
  
  return(results)
}

# Run tests for a single configuration
run_tests_for_config <- function(gen_name, n_samples, test_categories, n_runs) {
  gen_manager$set_active(gen_name)
  
  results <- list()
  moments_data <- list()
  successful_runs <- 0
  
  # Use parallel processing for runs (cross-platform)
  n_cores <- getOption("mc.cores", detectCores() - 1)
  
  # Cross-platform parallel execution
  if (.Platform$OS.type == "windows") {
    cl <- makeCluster(n_cores)
    on.exit(stopCluster(cl), add = TRUE)
    # Export necessary objects to cluster
    clusterExport(cl, c("gen_manager", "test_categories", "validate_rng_output"), 
                  envir = environment())
    run_results <- parLapply(cl, 1:n_runs, function(run) {
    run_result <- list()
    
    tryCatch({
      # Generate sample once per run
      sample <- gen_manager$generate(n_samples)
      
      if (!validate_rng_output(sample)) {
        return(list(success = FALSE, results = list(), moments = NULL))
      }
      
      # Compute moments
      moments <- NULL
      if (!is.null(test_categories$moment_function)) {
        moments <- test_categories$moment_function(sample)
      }
      
      # Run all tests on this sample
      for (cat_name in names(test_categories)) {
        if (cat_name != "moment_function") {
          for (test_name in names(test_categories[[cat_name]])) {
            test_func <- test_categories[[cat_name]][[test_name]]
            
            result <- tryCatch(
              test_func(sample),
              error = function(e) NA
            )
            
            key <- paste(cat_name, test_name, sep = "_")
            run_result[[key]] <- result
          }
        }
      }
      
      return(list(success = TRUE, results = run_result, moments = moments))
    }, error = function(e) {
      return(list(success = FALSE, results = list(), moments = NULL, error = e$message))
    })
    })
  } else {
    run_results <- mclapply(1:n_runs, function(run) {
      run_result <- list()
      
      tryCatch({
        # Generate sample once per run
        sample <- gen_manager$generate(n_samples)
        
        if (!validate_rng_output(sample)) {
          return(list(success = FALSE, results = list(), moments = NULL))
        }
        
        # Compute moments
        moments <- NULL
        if (!is.null(test_categories$moment_function)) {
          moments <- test_categories$moment_function(sample)
        }
        
        # Run all tests on this sample
        for (cat_name in names(test_categories)) {
          if (cat_name != "moment_function") {
            for (test_name in names(test_categories[[cat_name]])) {
              test_func <- test_categories[[cat_name]][[test_name]]
              
              result <- tryCatch(
                test_func(sample),
                error = function(e) NA
              )
              
              key <- paste(cat_name, test_name, sep = "_")
              run_result[[key]] <- result
            }
          }
        }
        
        return(list(success = TRUE, results = run_result, moments = moments))
      }, error = function(e) {
        return(list(success = FALSE, results = list(), moments = NULL, error = e$message))
      })
    }, mc.cores = n_cores)
  }
  
  # Aggregate results from parallel runs
  for (run_data in run_results) {
    if (run_data$success) {
      successful_runs <- successful_runs + 1
      
      # Aggregate test results
      for (key in names(run_data$results)) {
        if (!key %in% names(results)) {
          results[[key]] <- numeric(0)
        }
        
        value <- run_data$results[[key]]
        if (!is.na(value) && is.finite(value)) {
          results[[key]] <- c(results[[key]], value)
        }
      }
      
      # Aggregate moments
      if (!is.null(run_data$moments)) {
        moments_data[[length(moments_data) + 1]] <- run_data$moments
      }
    }
  }
  
  # Process results
  processed_results <- list()
  for (test_key in names(results)) {
    if (length(results[[test_key]]) > 0) {
      processed_results[[test_key]] <- list(
        p_values = results[[test_key]],
        pass_rate = mean(results[[test_key]] > 0.05, na.rm = TRUE),
        mean_p_value = mean(results[[test_key]], na.rm = TRUE),
        n_valid = length(results[[test_key]])
      )
    }
  }
  
  # Process moments data
  moments_summary <- NULL
  if (length(moments_data) > 0) {
    # Extract values for each moment
    mean_vals <- sapply(moments_data, function(m) m$mean)
    var_vals <- sapply(moments_data, function(m) m$variance)
    skew_vals <- sapply(moments_data, function(m) m$skewness)
    kurt_vals <- sapply(moments_data, function(m) m$kurtosis)
    
    moments_summary <- list(
      mean = list(
        observed = mean(mean_vals),
        expected = 0.5,
        deviation = abs(mean(mean_vals) - 0.5)
      ),
      variance = list(
        observed = mean(var_vals),
        expected = 1/12,
        deviation = abs(mean(var_vals) - 1/12)
      ),
      skewness = list(
        observed = mean(skew_vals),
        expected = 0,
        deviation = abs(mean(skew_vals))
      ),
      kurtosis = list(
        observed = mean(kurt_vals),
        expected = -1.2,
        deviation = abs(mean(kurt_vals) + 1.2)
      )
    )
  }
  
  return(list(
    generator = gen_name,
    n_samples = n_samples,
    successful_runs = successful_runs,
    test_results = processed_results,
    moments_summary = moments_summary
  ))
}

# Main analysis function
run_comprehensive_analysis <- function() {
  cat("Starting Comprehensive Generator Comparison\n")
  cat("==========================================\n\n")
  
  # Get test categories
  test_categories <- source_test_implementations()
  
  all_results <- list()
  performance_results <- list()
  
  # Benchmark performance
  cat("Benchmarking generator performance...\n")
  for (gen_name in names(gen_manager$generators)) {
    cat(sprintf("  - %s\n", gen_manager$generators[[gen_name]]$name))
    performance_results[[gen_name]] <- benchmark_generator(gen_name)
  }
  cat("\n")
  
  # Run statistical tests
  cat("Running statistical tests...\n")
  cat(sprintf("Total test iterations: %d\n\n", 
              length(SAMPLE_SIZES) * length(gen_manager$generators) * N_RUNS))
  
  start_time <- Sys.time()
  
  for (i in seq_along(SAMPLE_SIZES)) {
    sample_size <- SAMPLE_SIZES[i]
    cat(sprintf("Sample size: %s\n", format(sample_size, scientific = FALSE)))
    
    for (gen_name in names(gen_manager$generators)) {
      cat(sprintf("  Testing %s", gen_manager$generators[[gen_name]]$name))
      
      # Run tests
      config_results <- run_tests_for_config(gen_name, sample_size, test_categories, N_RUNS)
      all_results[[length(all_results) + 1]] <- config_results
      
      cat(sprintf("...... (%d/%d successful)\n", 
                  config_results$successful_runs, N_RUNS))
    }
    
    # Progress update
    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))
    progress <- (i / length(SAMPLE_SIZES)) * 100
    cat(sprintf("Progress: %.1f%% complete (%.1f minutes elapsed)\n\n", progress, elapsed))
  }
  
  return(list(
    test_results = all_results,
    performance_results = performance_results
  ))
}


# Generate comprehensive report
generate_report <- function(results) {
  # Process results into summary format
  summary_data <- process_test_results(results$test_results)
  
  # Create summary tables
  create_summary_tables(summary_data)
  
  # Create plots
  create_comprehensive_plots(summary_data, results$performance_results, results$test_results)
  
  # Generate markdown report with moments data
  create_detailed_report_with_moments(summary_data, results$performance_results, results$test_results)
  
  # Save all data
  saveRDS(summary_data, file.path(RESULTS_DIR, "data", "summary_data.rds"))
  saveRDS(results, file.path(RESULTS_DIR, "data", "full_results.rds"))
  
  # Create additional tables
  create_comprehensive_tables(summary_data, results$test_results)
}

# Process test results into summary format
process_test_results <- function(test_results) {
  summary_list <- list()
  
  for (result in test_results) {
    gen_name <- result$generator
    n_samples <- result$n_samples
    
    for (test_key in names(result$test_results)) {
      test_data <- result$test_results[[test_key]]
      
      summary_list[[length(summary_list) + 1]] <- data.frame(
        generator = gen_name,
        generator_name = gen_manager$generators[[gen_name]]$name,
        n_samples = n_samples,
        test = test_key,
        pass_rate = test_data$pass_rate,
        mean_p_value = test_data$mean_p_value,
        n_valid = test_data$n_valid,
        stringsAsFactors = FALSE
      )
    }
  }
  
  if (length(summary_list) > 0) {
    summary_df <- do.call(rbind, summary_list)
    return(summary_df)
  } else {
    return(data.frame())
  }
}

# Extract moments data from test results
extract_moments_data <- function(test_results) {
  moments_list <- list()
  
  for (result in test_results) {
    if (!is.null(result$moments_summary)) {
      moments_list[[length(moments_list) + 1]] <- data.frame(
        generator = result$generator,
        generator_name = gen_manager$generators[[result$generator]]$name,
        n_samples = result$n_samples,
        mean_observed = result$moments_summary$mean$observed,
        mean_deviation = result$moments_summary$mean$deviation,
        variance_observed = result$moments_summary$variance$observed,
        variance_deviation = result$moments_summary$variance$deviation,
        skewness_observed = result$moments_summary$skewness$observed,
        skewness_deviation = result$moments_summary$skewness$deviation,
        kurtosis_observed = result$moments_summary$kurtosis$observed,
        kurtosis_deviation = result$moments_summary$kurtosis$deviation,
        stringsAsFactors = FALSE
      )
    }
  }
  
  if (length(moments_list) > 0) {
    return(do.call(rbind, moments_list))
  } else {
    return(NULL)
  }
}

# Process performance data for plotting
process_performance_data <- function(performance_data) {
  perf_list <- list()
  
  for (gen_name in names(performance_data)) {
    df <- performance_data[[gen_name]]
    if (!all(is.na(df$rate))) {
      # Calculate mean and sd for each size
      for (i in 1:nrow(df)) {
        perf_list[[length(perf_list) + 1]] <- data.frame(
          generator = gen_manager$generators[[gen_name]]$name,
          size = df$size[i],
          mean_rate = df$rate[i],
          sd_rate = 0,  # Would need multiple runs to get SD
          stringsAsFactors = FALSE
        )
      }
    }
  }
  
  if (length(perf_list) > 0) {
    return(do.call(rbind, perf_list))
  } else {
    return(data.frame())
  }
}

# Create summary tables
create_summary_tables <- function(summary_data) {
  if (nrow(summary_data) == 0) return()
  
  # Overall pass rates by generator
  overall_pass_rates <- summary_data %>%
    group_by(generator_name) %>%
    summarize(
      overall_pass_rate = mean(pass_rate, na.rm = TRUE),
      tests_run = n(),
      .groups = "drop"
    ) %>%
    arrange(desc(overall_pass_rate))
  
  write.csv(overall_pass_rates, 
            file.path(RESULTS_DIR, "tables", "overall_pass_rates.csv"), 
            row.names = FALSE)
  
  # Pass rates by test
  test_pass_rates <- summary_data %>%
    group_by(test) %>%
    summarize(
      mean_pass_rate = mean(pass_rate, na.rm = TRUE),
      sd_pass_rate = sd(pass_rate, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(mean_pass_rate))
  
  write.csv(test_pass_rates, 
            file.path(RESULTS_DIR, "tables", "test_pass_rates.csv"), 
            row.names = FALSE)
  
  # Detailed results
  write.csv(summary_data, 
            file.path(RESULTS_DIR, "tables", "detailed_results.csv"), 
            row.names = FALSE)
}

# Create comprehensive plots
create_comprehensive_plots <- function(summary_data, performance_data, all_results = NULL) {
  if (nrow(summary_data) == 0) return()
  
  # Theme for plots
  theme_set(theme_minimal() + 
            theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
                  legend.position = "bottom"))
  
  # 1. Pass rates by generator
  p1 <- ggplot(summary_data, aes(x = generator_name, y = pass_rate, fill = test)) +
    geom_boxplot() +
    labs(title = "Pass Rates by Generator", 
         x = "Generator", y = "Pass Rate",
         fill = "Test") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ylim(0, 1)
  
  ggsave(file.path(RESULTS_DIR, "plots", "pass_rates_by_generator.png"), 
         p1, width = 10, height = 6, dpi = 300)
  
  # 2. Pass rates by test
  p2 <- ggplot(summary_data, aes(x = test, y = pass_rate, fill = generator_name)) +
    geom_boxplot() +
    labs(title = "Pass Rates by Test", 
         x = "Test", y = "Pass Rate",
         fill = "Generator") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ylim(0, 1)
  
  ggsave(file.path(RESULTS_DIR, "plots", "pass_rates_by_test.png"), 
         p2, width = 10, height = 6, dpi = 300)
  
  # 3. Pass rates vs sample size
  p3 <- ggplot(summary_data, 
               aes(x = factor(n_samples), y = pass_rate, 
                   color = generator_name, group = generator_name)) +
    stat_summary(fun = mean, geom = "line", linewidth = 1) +
    stat_summary(fun = mean, geom = "point", size = 3) +
    facet_wrap(~test, scales = "free_y") +
    labs(title = "Pass Rates vs Sample Size", 
         x = "Sample Size", y = "Pass Rate",
         color = "Generator") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggsave(file.path(RESULTS_DIR, "plots", "pass_rates_vs_sample_size.png"), 
         p3, width = 12, height = 8, dpi = 300)
  
  # 4. Performance comparison
  if (length(performance_data) > 0) {
    perf_list <- list()
    for (gen_name in names(performance_data)) {
      df <- performance_data[[gen_name]]
      df$generator <- gen_manager$generators[[gen_name]]$name
      perf_list[[length(perf_list) + 1]] <- df
    }
    perf_df <- do.call(rbind, perf_list)
    
    p4 <- ggplot(perf_df, aes(x = factor(size), y = rate, fill = generator)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Generator Performance (Numbers/Second)", 
           x = "Sample Size", y = "Generation Rate (numbers/sec)",
           fill = "Generator") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_y_continuous(labels = scales::comma)
    
    ggsave(file.path(RESULTS_DIR, "plots", "generator_performance.png"), 
           p4, width = 10, height = 6, dpi = 300)
  }
  
  # 5. Test results heatmap
  heatmap_data <- summary_data %>%
    group_by(generator_name, test) %>%
    summarize(mean_pass_rate = mean(pass_rate, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = test, values_from = mean_pass_rate)
  
  if (ncol(heatmap_data) > 1) {
    heatmap_matrix <- as.matrix(heatmap_data[,-1])
    rownames(heatmap_matrix) <- heatmap_data$generator_name
    
    png(file.path(RESULTS_DIR, "plots", "test_results_heatmap.png"), 
        width = 10, height = 6, units = "in", res = 300)
    heatmap(heatmap_matrix, 
            scale = "none",
            col = colorRampPalette(c("red", "yellow", "green"))(100),
            main = "Test Pass Rates Heatmap",
            xlab = "Test", ylab = "Generator",
            margins = c(10, 10))
    dev.off()
  }
  
  # 6. Moments deviation plot
  if (!is.null(all_results)) {
    moments_df <- extract_moments_data(all_results)
    if (!is.null(moments_df) && nrow(moments_df) > 0) {
      # Mean deviation plot
      p6 <- ggplot(moments_df, aes(x = generator_name, y = mean_deviation, 
                                    fill = factor(n_samples))) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(title = "Mean Deviation from Expected (0.5)",
             x = "Generator", y = "Absolute Deviation",
             fill = "Sample Size") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_y_continuous(labels = scales::scientific)
      
      ggsave(file.path(RESULTS_DIR, "plots", "mean_deviation.png"), 
             p6, width = 12, height = 6, dpi = 300)
      
      # Variance deviation plot
      p7 <- ggplot(moments_df, aes(x = generator_name, y = variance_deviation, 
                                    fill = factor(n_samples))) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(title = "Variance Deviation from Expected (1/12)",
             x = "Generator", y = "Absolute Deviation",
             fill = "Sample Size") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_y_continuous(labels = scales::scientific)
      
      ggsave(file.path(RESULTS_DIR, "plots", "variance_deviation.png"), 
             p7, width = 12, height = 6, dpi = 300)
      
      # Combined moments plot
      moments_long <- moments_df %>%
        pivot_longer(cols = c(mean_deviation, variance_deviation, 
                              skewness_deviation, kurtosis_deviation),
                     names_to = "moment", values_to = "deviation") %>%
        mutate(moment = gsub("_deviation", "", moment))
      
      p8 <- ggplot(moments_long, aes(x = generator_name, y = deviation, 
                                      color = moment, group = moment)) +
        stat_summary(fun = mean, geom = "line", linewidth = 1) +
        stat_summary(fun = mean, geom = "point", size = 3) +
        facet_wrap(~n_samples, scales = "free_y", 
                   labeller = labeller(n_samples = function(x) paste("N =", x))) +
        labs(title = "Statistical Moments Deviation from Expected Values",
             x = "Generator", y = "Absolute Deviation",
             color = "Moment") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_y_log10(labels = scales::scientific)
      
      ggsave(file.path(RESULTS_DIR, "plots", "moments_deviation_combined.png"), 
             p8, width = 14, height = 8, dpi = 300)
    }
  }
  
  # 7. Performance per sample size with error bars
  if (length(performance_data) > 0) {
    perf_summary <- process_performance_data(performance_data)
    
    p9 <- ggplot(perf_summary, aes(x = factor(size), y = mean_rate, 
                                    fill = generator)) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_errorbar(aes(ymin = mean_rate - sd_rate, 
                        ymax = mean_rate + sd_rate),
                    position = position_dodge(0.9), width = 0.2) +
      labs(title = "Generator Performance with Variability",
           x = "Sample Size", y = "Generation Rate (numbers/sec)",
           fill = "Generator") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_y_continuous(labels = scales::comma)
    
    ggsave(file.path(RESULTS_DIR, "plots", "performance_with_error.png"), 
           p9, width = 12, height = 6, dpi = 300)
    
    # Log-scale performance plot
    p10 <- ggplot(perf_summary, aes(x = size, y = mean_rate, 
                                     color = generator, group = generator)) +
      geom_line(linewidth = 1) +
      geom_point(size = 3) +
      scale_x_log10(labels = scales::comma) +
      scale_y_log10(labels = scales::comma) +
      labs(title = "Generator Performance Scaling (Log-Log)",
           x = "Sample Size (log scale)", 
           y = "Generation Rate (numbers/sec, log scale)",
           color = "Generator") +
      theme(legend.position = "right")
    
    ggsave(file.path(RESULTS_DIR, "plots", "performance_loglog.png"), 
           p10, width = 10, height = 6, dpi = 300)
  }
}

# Create comprehensive tables
create_comprehensive_tables <- function(summary_data, test_results) {
  # Moments summary table
  moments_df <- extract_moments_data(test_results)
  if (!is.null(moments_df)) {
    # Aggregate moments by generator
    moments_summary <- moments_df %>%
      group_by(generator_name) %>%
      summarize(
        mean_dev = mean(mean_deviation),
        var_dev = mean(variance_deviation),
        skew_dev = mean(skewness_deviation),
        kurt_dev = mean(kurtosis_deviation),
        total_dev = mean_dev + var_dev + skew_dev + kurt_dev,
        .groups = "drop"
      ) %>%
      arrange(total_dev)
    
    write.csv(moments_summary,
              file.path(RESULTS_DIR, "tables", "moments_summary.csv"),
              row.names = FALSE)
    
    # Detailed moments by sample size
    write.csv(moments_df,
              file.path(RESULTS_DIR, "tables", "moments_detailed.csv"),
              row.names = FALSE)
  }
  
  # Performance ranking table
  perf_ranking <- summary_data %>%
    group_by(generator_name) %>%
    summarize(
      avg_pass_rate = mean(pass_rate, na.rm = TRUE),
      tests_passed = sum(pass_rate > 0.95, na.rm = TRUE),
      tests_failed = sum(pass_rate <= 0.05, na.rm = TRUE),
      total_tests = n(),
      .groups = "drop"
    ) %>%
    mutate(
      success_ratio = tests_passed / total_tests,
      rank = rank(-avg_pass_rate)
    ) %>%
    arrange(rank)
  
  write.csv(perf_ranking,
            file.path(RESULTS_DIR, "tables", "generator_rankings.csv"),
            row.names = FALSE)
  
  # Test difficulty analysis
  test_difficulty <- summary_data %>%
    group_by(test) %>%
    summarize(
      avg_pass_rate = mean(pass_rate, na.rm = TRUE),
      sd_pass_rate = sd(pass_rate, na.rm = TRUE),
      generators_failed = sum(pass_rate < 0.5, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(avg_pass_rate)
  
  write.csv(test_difficulty,
            file.path(RESULTS_DIR, "tables", "test_difficulty.csv"),
            row.names = FALSE)
}

# Create detailed markdown report with moments
create_detailed_report_with_moments <- function(summary_data, performance_data, test_results) {
  report_lines <- c(
    "# Comprehensive Random Number Generator Analysis",
    "",
    sprintf("Generated on: %s", Sys.time()),
    "",
    "## Configuration",
    sprintf("- Sample sizes tested: %s", 
            paste(format(SAMPLE_SIZES, scientific = FALSE), collapse = ", ")),
    sprintf("- Runs per configuration: %d", N_RUNS),
    sprintf("- Total generators tested: %d", length(gen_manager$generators)),
    "",
    "## Generators Tested",
    ""
  )
  
  for (gen_name in names(gen_manager$generators)) {
    report_lines <- c(report_lines,
                      sprintf("- %s", gen_manager$generators[[gen_name]]$name))
  }
  
  # Overall results
  if (nrow(summary_data) > 0) {
    overall_rates <- summary_data %>%
      group_by(generator_name) %>%
      summarize(
        overall_pass_rate = mean(pass_rate, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(overall_pass_rate))
    
    report_lines <- c(report_lines,
                      "",
                      "## Overall Pass Rates",
                      "",
                      "| Generator | Pass Rate |",
                      "|-----------|-----------|")
    
    for (i in 1:nrow(overall_rates)) {
      report_lines <- c(report_lines,
                        sprintf("| %s | %.2f%% |", 
                                overall_rates$generator_name[i],
                                overall_rates$overall_pass_rate[i] * 100))
    }
  }
  
  # Add moments data if available
  moments_df <- extract_moments_data(test_results)
  if (!is.null(moments_df) && nrow(moments_df) > 0) {
    # Aggregate moments by generator
    moments_summary <- moments_df %>%
      group_by(generator_name) %>%
      summarize(
        mean_dev = mean(mean_deviation),
        var_dev = mean(variance_deviation),
        skew_dev = mean(skewness_deviation),
        kurt_dev = mean(kurtosis_deviation),
        .groups = "drop"
      ) %>%
      arrange(mean_dev + var_dev + skew_dev + kurt_dev)
    
    report_lines <- c(report_lines,
                      "",
                      "## Statistical Moments Analysis",
                      "",
                      "### Deviation from Expected Values",
                      "",
                      "| Generator | Mean Dev | Var Dev | Skew Dev | Kurt Dev | Total Dev |",
                      "|-----------|----------|---------|----------|----------|-----------|")
    
    for (i in 1:nrow(moments_summary)) {
      total_dev <- moments_summary$mean_dev[i] + moments_summary$var_dev[i] + 
                   moments_summary$skew_dev[i] + moments_summary$kurt_dev[i]
      report_lines <- c(report_lines,
                        sprintf("| %s | %.2e | %.2e | %.2e | %.2e | %.2e |",
                                moments_summary$generator_name[i],
                                moments_summary$mean_dev[i],
                                moments_summary$var_dev[i],
                                moments_summary$skew_dev[i],
                                moments_summary$kurt_dev[i],
                                total_dev))
    }
    
    report_lines <- c(report_lines,
                      "",
                      "### Expected Values",
                      "- Mean: 0.5",
                      "- Variance: 1/12 ≈ 0.0833",
                      "- Skewness: 0",
                      "- Kurtosis: -1.2")
  }
  
  # Add performance summary
  if (length(performance_data) > 0) {
    report_lines <- c(report_lines,
                      "",
                      "## Performance Summary",
                      "",
                      "Generation rates shown for largest sample size tested.",
                      "")
    
    # Find max sample size performance
    perf_summary <- process_performance_data(performance_data)
    if (nrow(perf_summary) > 0) {
      max_size_perf <- perf_summary %>%
        filter(size == max(size)) %>%
        arrange(desc(mean_rate))
      
      report_lines <- c(report_lines,
                        "| Generator | Rate (numbers/sec) |",
                        "|-----------|-------------------|")
      
      for (i in 1:nrow(max_size_perf)) {
        report_lines <- c(report_lines,
                          sprintf("| %s | %s |",
                                  max_size_perf$generator[i],
                                  format(max_size_perf$mean_rate[i], 
                                         big.mark = ",", scientific = FALSE)))
      }
    }
  }
  
  # Add test summary by category
  test_categories <- unique(gsub("_.*", "", summary_data$test))
  if (length(test_categories) > 0) {
    report_lines <- c(report_lines,
                      "",
                      "## Test Categories Performance",
                      "")
    
    for (category in test_categories) {
      category_data <- summary_data %>%
        filter(grepl(paste0("^", category), test)) %>%
        group_by(generator_name) %>%
        summarize(
          category_pass_rate = mean(pass_rate, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        arrange(desc(category_pass_rate))
      
      if (nrow(category_data) > 0) {
        report_lines <- c(report_lines,
                          sprintf("### %s Tests", tools::toTitleCase(category)),
                          "",
                          "| Generator | Pass Rate |",
                          "|-----------|-----------|")
        
        for (i in 1:min(5, nrow(category_data))) {  # Top 5 only
          report_lines <- c(report_lines,
                            sprintf("| %s | %.2f%% |",
                                    category_data$generator_name[i],
                                    category_data$category_pass_rate[i] * 100))
        }
        report_lines <- c(report_lines, "")
      }
    }
  }
  
  # Write report
  writeLines(report_lines, file.path(RESULTS_DIR, "comprehensive_report.md"))
}

# Main execution function
main <- function() {
  # Check packages
  pkg_status <- check_packages()
  
  cat("\n====================================================\n")
  cat("   Comprehensive Random Number Generator Analysis   \n")
  cat("====================================================\n\n")
  
  # Set up generators
  setup_generators()
  
  # Initialize all generators once
  gen_manager$init_all()
  
  # Set up parallel processing
  n_cores <- detectCores()
  if (n_cores > 1) {
    n_cores <- n_cores - 1  # Leave one core free
  }
  cat(sprintf("Using %d CPU cores for parallel processing\n\n", n_cores))
  
  # Run analysis
  results <- run_comprehensive_analysis()
  
  # Generate report
  if (length(results$test_results) > 0) {
    generate_report(results)
    
    # Save raw results
    saveRDS(results, file.path(RESULTS_DIR, "data", "raw_results.rds"))
    
    cat("\n====================================================\n")
    cat("Analysis complete! Results saved to:\n")
    cat(sprintf("  %s\n", RESULTS_DIR))
    cat("\nKey outputs:\n")
    cat("  - comprehensive_report.md: Full analysis report\n")
    cat("  - plots/: All visualization files\n")
    cat("  - tables/: Statistical summaries in CSV format\n")
    cat("  - data/: R data files for further analysis\n")
    cat("====================================================\n\n")
  } else {
    cat("\nERROR: No test results were generated.\n")
  }
  
  # Clean up generators at the very end
  gen_manager$cleanup_all()
  
  return(invisible(RESULTS_DIR))
}

# Run if executed directly
if (!interactive()) {
  main()
}