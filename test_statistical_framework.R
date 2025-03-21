#!/usr/bin/env Rscript
# Load required packages
library(stats)
tryCatch({
  library(qiprng)
  message("Successfully loaded qiprng package")
}, error = function(e) {
  message("Could not load qiprng package: ", e$message, ". Will try to use devtools.")
  # Try loading the package using devtools if it's not installed
  if (requireNamespace("devtools", quietly = TRUE)) {
    tryCatch({
      devtools::load_all(".")
      message("Successfully loaded qiprng package using devtools")
    }, error = function(e) {
      message("Could not load qiprng using devtools: ", e$message)
    })
  }
})

# Integrated Statistical Testing Framework for QPRNG
# This script provides a unified testing framework with configurable parameters
# for comparing different PRNGs

# ======================= CONFIGURATION SECTION =======================
# You can easily modify these parameters to control the testing

# Test sample sizes - adjust these to speed up testing or get more thorough results
CONFIG <- list(
  # Sample sizes (adjust these to control test duration)
  basic_sample_size = 10000,     # For distribution tests
  runs_sample_size = 10000,      # For runs and independence tests
  correlation_sample_size = 10000, # For correlation tests
  binary_sample_size = 10000,    # For binary and bitwise tests
  classical_sample_size = 10000, # For classical statistical tests
  compression_sample_size = 10000, # For compression-based tests
  
  # Test parameters
  chi_squared_bins = 100,
  significance_level = 0.01,
  
  # Output options
  output_dir = "test_results",
  save_results = TRUE,
  save_visualizations = TRUE,
  verbose = TRUE,
  
  # Quarto report options
  generate_report = TRUE,     # Set to TRUE to generate a Quarto report
  report_format = "pdf",      # 'pdf', 'html', or 'docx'
  report_title = "PRNG Statistical Testing Report"
)

# PRNGs to test (TRUE = include in test, FALSE = skip)
TEST_PRNGS <- list(
  r_builtin = TRUE,     # R's built-in runif
  lcg = TRUE,           # Simple Linear Congruential Generator
  poor = TRUE,          # Intentionally poor PRNG (for comparison)
  qiprng = TRUE,        # Our QIPRNG implementation
  qiprng_nocrypto = TRUE, # QIPRNG without cryptomixing
  dqrng = TRUE          # dqrng package (if available)
)

# Tests to run (TRUE = run test, FALSE = skip)
TEST_CATEGORIES <- list(
  basic = TRUE,         # Basic distribution tests
  runs = TRUE,          # Runs and independence tests
  correlation = TRUE,   # Correlation tests
  binary = TRUE,        # Binary and bitwise tests
  classical = TRUE,     # Classical statistical tests
  compression = TRUE    # Compression tests
)

# ========================= HELPER FUNCTIONS =========================

# Create output directory
output_dir <- CONFIG$output_dir
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# ========================= PRNG DEFINITIONS =========================

# 1. Base R's runif (built-in)
runif_prng <- function(n) {
  return(runif(n))
}

# 2. A basic linear congruential generator (LCG)
lcg_prng <- function(n, seed = 12345) {
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

# 3. A poor PRNG for comparison
poor_prng <- function(n, seed = 12345) {
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

# 3.5 dqrng implementation (if available)
dqrng_prng <- function(n) {
  # Check if dqrng package is available
  if (requireNamespace("dqrng", quietly = TRUE)) {
    log_info("Using dqrng::dqrunif for random number generation")
    return(dqrng::dqrunif(n))
  } else {
    log_warning("dqrng package not available, using base R runif as fallback")
    return(stats::runif(n))
  }
}

# 4. QIPRNG implementation with default settings (cryptomixing enabled)
qiprng_func <- function(n) {
  # Check the most direct way to use qiprng package
  if (requireNamespace("qiprng", quietly = TRUE)) {
    log_info("Using qiprng package with crypto mixing enabled")
    tryCatch({
      # Create a fresh PRNG configuration with crypto mixing ENABLED
      config <- list(distribution = "uniform_01", use_crypto_mixing = TRUE)
      
      # Create the PRNG instance
      qiprng::createPRNG(config)
      
      # Generate the random numbers
      result <- qiprng::generatePRNG(n)
      log_info(paste("Successfully generated", n, "random numbers with QIPRNG (crypto)."))
      return(result)
    }, error = function(e) {
      log_warning(paste("Error using qiprng package:", conditionMessage(e)))
    })
  }
  
  # Check if we have direct access to the C++ API in the environment
  if (exists("createPRNG", mode = "function") && 
      exists("updatePRNG", mode = "function") && 
      exists("generatePRNG", mode = "function")) {
    log_info("Using direct C++ QIPRNG implementation with crypto mixing enabled")
    tryCatch({
      # Create or update PRNG with crypto mixing ENABLED
      config <- list(distribution = "uniform_01", use_crypto_mixing = TRUE)
      createPRNG(config)
      
      # Generate random numbers
      result <- generatePRNG(n)
      log_info("Successfully used direct C++ API for QIPRNG")
      return(result)
    }, error = function(e) {
      log_warning(paste("Error using C++ QIPRNG implementation:", conditionMessage(e)))
    })
  }
  
  # If we couldn't load the package directly, try loading with devtools
  tryCatch({
    if (requireNamespace("devtools", quietly = TRUE)) {
      log_info("Attempting to load qiprng using devtools")
      devtools::load_all(".")
      
      if (exists("createPRNG", mode = "function") && 
          exists("generatePRNG", mode = "function")) {
        config <- list(distribution = "uniform_01", use_crypto_mixing = TRUE)
        createPRNG(config)
        result <- generatePRNG(n)
        log_info("Successfully loaded and used qiprng via devtools")
        return(result)
      }
    }
  }, error = function(e) {
    log_warning(paste("Error loading qiprng via devtools:", conditionMessage(e)))
  })
  
  # Fallback to base R if all else fails
  log_warning("QIPRNG implementation not found or failed, using runif as placeholder")
  return(stats::runif(n))
}

# 5. QIPRNG implementation WITHOUT cryptomixing
qiprng_nocrypto_func <- function(n) {
  # Check the most direct way to use qiprng package
  if (requireNamespace("qiprng", quietly = TRUE)) {
    log_info("Using qiprng package with crypto mixing DISABLED")
    tryCatch({
      # Create a fresh PRNG configuration with crypto mixing DISABLED
      config <- list(distribution = "uniform_01", use_crypto_mixing = FALSE)
      
      # Create the PRNG instance
      qiprng::createPRNG(config)
      
      # Generate the random numbers
      result <- qiprng::generatePRNG(n)
      log_info(paste("Successfully generated", n, "random numbers with QIPRNG (no crypto)."))
      return(result)
    }, error = function(e) {
      log_warning(paste("Error using qiprng package (no crypto):", conditionMessage(e)))
    })
  }
  
  # Check if we have direct access to the C++ API in the environment
  if (exists("createPRNG", mode = "function") && 
      exists("updatePRNG", mode = "function") && 
      exists("generatePRNG", mode = "function")) {
    log_info("Using direct C++ QIPRNG implementation with crypto mixing DISABLED")
    tryCatch({
      # Create PRNG with crypto mixing DISABLED
      config <- list(distribution = "uniform_01", use_crypto_mixing = FALSE)
      createPRNG(config)
      
      # Generate random numbers
      result <- generatePRNG(n)
      log_info("Successfully generated random numbers using C++ API (no crypto)")
      return(result)
    }, error = function(e) {
      log_warning(paste("Error using C++ QIPRNG implementation (no crypto):", conditionMessage(e)))
    })
  }
  
  # If we couldn't load the package directly, try loading with devtools
  tryCatch({
    if (requireNamespace("devtools", quietly = TRUE)) {
      log_info("Attempting to load qiprng using devtools (no crypto)")
      devtools::load_all(".")
      
      if (exists("createPRNG", mode = "function") && 
          exists("generatePRNG", mode = "function")) {
        config <- list(distribution = "uniform_01", use_crypto_mixing = FALSE)
        createPRNG(config)
        result <- generatePRNG(n)
        log_info("Successfully loaded and used qiprng via devtools (no crypto)")
        return(result)
      }
    }
  }, error = function(e) {
    log_warning(paste("Error loading qiprng via devtools (no crypto):", conditionMessage(e)))
  })
  
  # Fall back to runif
  log_warning("QIPRNG no-crypto implementation not found or failed, using runif as placeholder")
  return(stats::runif(n))
}

# ======================== TESTING FUNCTIONS ========================

# Set up logging
log_info <- function(msg) {
  cat(paste0("[INFO] ", format(Sys.time(), "%H:%M:%S"), ": ", msg, "\n"))
}

log_warning <- function(msg) {
  cat(paste0("[WARNING] ", format(Sys.time(), "%H:%M:%S"), ": ", msg, "\n"))
}

log_error <- function(msg) {
  cat(paste0("[ERROR] ", format(Sys.time(), "%H:%M:%S"), ": ", msg, "\n"))
}

# Helper function to source modules safely
source_modules <- function() {
  log_info("Loading test modules...")
  
  # First, load or install necessary packages
  required_packages <- c("ggplot2", "randtests", "stats")
  
  # Additional packages for report generation
  if (CONFIG$generate_report) {
    required_packages <- c(required_packages, "knitr", "rmarkdown", "kableExtra", "dplyr", "tidyr", "patchwork", "gridExtra")
    
    # Check if quarto is needed and available
    if (CONFIG$report_format == "pdf" || CONFIG$report_format == "html" || CONFIG$report_format == "docx") {
      if (!requireNamespace("quarto", quietly = TRUE)) {
        log_warning("Quarto package not available. Will attempt to use quarto CLI if installed on system.")
        log_warning("If report generation fails, please install quarto: https://quarto.org/docs/get-started/")
      }
    }
  }
  
  for(pkg in required_packages) {
    if(!requireNamespace(pkg, quietly = TRUE)) {
      log_info(paste("Installing package:", pkg))
      # Set a CRAN mirror before installing
      repos <- "https://cloud.r-project.org"
      install.packages(pkg, repos = repos, quiet = TRUE)
    }
    library(pkg, character.only = TRUE)
  }
  
  modules <- c("basic_tests.R", "runs_tests.R", "correlation_tests.R", 
              "binary_tests.R", "classical_tests.R", "compression_tests.R", 
              "visualization.R")
  
  # Also look for a consolidated module if available
  if (file.exists("R/statistical_testing.R")) {
    log_info("Found consolidated statistical_testing.R module")
    source("R/statistical_testing.R", local = FALSE)
    return()
  }
  
  # Otherwise load individual modules
  for (module in modules) {
    file_path <- file.path("R/statisticaltests", module)
    if (file.exists(file_path)) {
      log_info(paste("Loading", module))
      tryCatch({
        source(file_path, local = FALSE) # Make functions globally available
      }, error = function(e) {
        log_error(paste("Failed to load", module, ":", conditionMessage(e)))
      })
    } else {
      log_warning(paste("Could not find", module))
    }
  }
  
  # Try to load C++ based QIPRNG code if available
  tryCatch({
    if (file.exists("src/rcpp_interface.cpp")) {
      log_info("Found C++ implementation, attempting to load...")
      dyn.load(paste0("src/", .Platform$dynlib.ext))
    }
  }, error = function(e) {
    log_warning(paste("Could not load C++ implementation:", conditionMessage(e)))
  })
}

# Function to run with diagnostic output
run_with_diagnostics <- function(name, func, suite) {
  log_info(paste("Running", name, "..."))
  start_time <- Sys.time()
  
  result <- tryCatch({
    suite <- func(suite)
    log_info(paste("✓", name, "completed in", 
              round(difftime(Sys.time(), start_time, units = "secs"), 2), 
              "seconds"))
    suite
  }, error = function(e) {
    log_error(paste("✗", name, "failed:", conditionMessage(e)))
    suite
  })
  
  return(result)
}

# Function to run a selected test suite
run_test_suite <- function(prng_name, prng_func, config, categories) {
  cat("\n===== Testing", prng_name, "=====\n")
  
  # Create the output directory for this PRNG
  prng_output_dir <- file.path(output_dir, tolower(gsub("[^[:alnum:]]", "_", prng_name)))
  dir.create(prng_output_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Update config with PRNG-specific settings
  prng_config <- config
  prng_config$output_dir <- prng_output_dir
  prng_config$prng_type <- prng_name
  
  # Create test suite structure
  suite <- list(
    prng_func = prng_func,
    config = prng_config,
    categories = names(categories)[unlist(categories)],
    results = list()
  )
  
  # Run each enabled test category with diagnostics
  if (categories$basic) {
    suite <- run_with_diagnostics("Basic Distribution Tests", run_basic_tests, suite)
  }
  
  if (categories$runs) {
    suite <- run_with_diagnostics("Runs and Independence Tests", run_runs_tests, suite)
  }
  
  if (categories$correlation) {
    suite <- run_with_diagnostics("Correlation Tests", run_correlation_tests, suite)
  }
  
  if (categories$binary) {
    suite <- run_with_diagnostics("Binary and Bitwise Tests", run_binary_tests, suite)
  }
  
  if (categories$classical) {
    suite <- run_with_diagnostics("Classical Tests", run_classical_tests, suite)
  }
  
  if (categories$compression) {
    suite <- run_with_diagnostics("Compression Tests", run_compression_tests, suite)
  }
  
  return(suite)
}
# ========================= VISUALIZATION ============================

# Function to print test summary
print_test_summary <- function(suite) {
  cat("Test Results Summary for", suite$config$prng_type, ":\n")
  total_pass <- 0
  total_fail <- 0
  total_tests <- 0
  
  for (category in names(suite$results)) {
    cat("\n", toupper(category), " TESTS:\n", sep="")
    if (length(suite$results[[category]]) == 0) {
      cat("  No results available\n")
      next
    }
    
    for (test_name in names(suite$results[[category]])) {
      test_result <- suite$results[[category]][[test_name]]
      result_str <- if (is.null(test_result$result)) "UNKNOWN" else test_result$result
      p_val_str <- if (is.null(test_result$p_value) || is.na(test_result$p_value)) 
                   "N/A" else sprintf("%.4f", test_result$p_value)
      
      cat(sprintf("  %s: %s (p-value: %s)\n", 
               test_result$description, result_str, p_val_str))
      
      total_tests <- total_tests + 1
      if (result_str == "PASS") total_pass <- total_pass + 1
      else if (result_str == "FAIL") total_fail <- total_fail + 1
    }
  }
  
  # Print overall summary
  if (total_tests > 0) {
    pass_rate <- round(100 * total_pass / total_tests, 1)
    cat("\nOverall results: ", total_pass, "/", total_tests, " tests passed (", 
        pass_rate, "%)\n", sep="")
  }
}

# Function to compare test results between PRNGs
compare_prng_results <- function(suites) {
  if (length(suites) < 2) {
    cat("Need at least 2 PRNGs to compare\n")
    return()
  }
  
  cat("\n=========================================\n")
  cat("PRNG COMPARISON SUMMARY\n")
  cat("=========================================\n\n")
  
  # Get all unique test categories across all suites
  all_categories <- list()
  for (name in names(suites)) {
    for (category in names(suites[[name]]$results)) {
      all_categories[[category]] <- TRUE
    }
  }
  
  # For each category and test, show results from all PRNGs
  for (category in names(all_categories)) {
    cat("\n", toupper(category), " TESTS:\n", sep="")
    
    # Get all unique tests in this category across all PRNGs
    all_tests <- list()
    for (name in names(suites)) {
      if (!is.null(suites[[name]]$results[[category]])) {
        for (test_name in names(suites[[name]]$results[[category]])) {
          test_result <- suites[[name]]$results[[category]][[test_name]]
          all_tests[[test_result$description]] <- TRUE
        }
      }
    }
    
    # Print comparison for each test
    for (test_desc in names(all_tests)) {
      cat("  ", test_desc, ":\n", sep="")
      
      # Print results for each PRNG
      for (name in names(suites)) {
        found_test <- FALSE
        if (!is.null(suites[[name]]$results[[category]])) {
          for (test_name in names(suites[[name]]$results[[category]])) {
            test_result <- suites[[name]]$results[[category]][[test_name]]
            if (test_result$description == test_desc) {
              result_str <- if (is.null(test_result$result)) "UNKNOWN" else test_result$result
              p_val_str <- if (is.null(test_result$p_value) || is.na(test_result$p_value)) 
                           "N/A" else sprintf("%.4f", test_result$p_value)
              
              cat(sprintf("    %s: %s (p-value: %s)\n", 
                         suites[[name]]$config$prng_type, result_str, p_val_str))
              found_test <- TRUE
              break
            }
          }
        }
        
        if (!found_test) {
          cat(sprintf("    %s: Not tested\n", suites[[name]]$config$prng_type))
        }
      }
    }
  }
  
  cat("\n=========================================\n")
}

# ========================= MAIN EXECUTION ==========================

# Start timer for overall execution
total_start_time <- Sys.time()
log_info(paste("Starting PRNG test framework at", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
log_info(paste("Output directory:", output_dir))

# Source the test modules
source_modules()

# Define the test functions if not already defined by the modules
if (!exists("run_basic_tests")) {
  # Fallback implementation for basic tests if not defined in modules
  run_basic_tests <- function(suite) {
    log_warning("Using fallback implementation for run_basic_tests")
    suite$results$basic <- list(
      basic_test = list(
        description = "Basic Distribution Test",
        result = "UNKNOWN",
        p_value = NA,
        details = "Fallback implementation used"
      )
    )
    return(suite)
  }
  utils::assignInNamespace("run_basic_tests", run_basic_tests, "package:base")
}

if (!exists("run_runs_tests")) {
  run_runs_tests <- function(suite) {
    log_warning("Using fallback implementation for run_runs_tests")
    suite$results$runs <- list(
      runs_test = list(
        description = "Runs Test",
        result = "UNKNOWN",
        p_value = NA,
        details = "Fallback implementation used"
      )
    )
    return(suite)
  }
  utils::assignInNamespace("run_runs_tests", run_runs_tests, "package:base")
}

if (!exists("run_correlation_tests")) {
  run_correlation_tests <- function(suite) {
    log_warning("Using fallback implementation for run_correlation_tests")
    suite$results$correlation <- list(
      correlation_test = list(
        description = "Correlation Test",
        result = "UNKNOWN",
        p_value = NA,
        details = "Fallback implementation used"
      )
    )
    return(suite)
  }
  utils::assignInNamespace("run_correlation_tests", run_correlation_tests, "package:base")
}

if (!exists("run_binary_tests")) {
  run_binary_tests <- function(suite) {
    log_warning("Using fallback implementation for run_binary_tests")
    suite$results$binary <- list(
      binary_test = list(
        description = "Binary Test",
        result = "UNKNOWN",
        p_value = NA,
        details = "Fallback implementation used"
      )
    )
    return(suite)
  }
  utils::assignInNamespace("run_binary_tests", run_binary_tests, "package:base")
}

if (!exists("run_classical_tests")) {
  run_classical_tests <- function(suite) {
    log_warning("Using fallback implementation for run_classical_tests")
    suite$results$classical <- list(
      classical_test = list(
        description = "Classical Test",
        result = "UNKNOWN",
        p_value = NA,
        details = "Fallback implementation used"
      )
    )
    return(suite)
  }
  utils::assignInNamespace("run_classical_tests", run_classical_tests, "package:base")
}

if (!exists("run_compression_tests")) {
  run_compression_tests <- function(suite) {
    log_warning("Using fallback implementation for run_compression_tests")
    suite$results$compression <- list(
      compression_test = list(
        description = "Compression Test",
        result = "UNKNOWN",
        p_value = NA,
        details = "Fallback implementation used"
      )
    )
    return(suite)
  }
  utils::assignInNamespace("run_compression_tests", run_compression_tests, "package:base")
}

# Check if external test function exists and create a fallback if not
if (!exists("run_external_tests")) {
  run_external_tests <- function(suite) {
    log_warning("External tests not implemented")
    return(suite)
  }
  utils::assignInNamespace("run_external_tests", run_external_tests, "package:base")
}

# Store results for all tested PRNGs
prng_results <- list()

# Run tests for each enabled PRNG
if (TEST_PRNGS$r_builtin) {
  prng_results$r_builtin <- run_test_suite(
    "R's Built-in runif", 
    runif_prng, 
    CONFIG, 
    TEST_CATEGORIES
  )
}

if (TEST_PRNGS$lcg) {
  prng_results$lcg <- run_test_suite(
    "Linear Congruential Generator", 
    lcg_prng, 
    CONFIG, 
    TEST_CATEGORIES
  )
}

if (TEST_PRNGS$poor) {
  prng_results$poor <- run_test_suite(
    "Poor PRNG (pattern with noise)", 
    poor_prng, 
    CONFIG, 
    TEST_CATEGORIES
  )
}

if (TEST_PRNGS$qiprng) {
  prng_results$qiprng <- run_test_suite(
    "QIPRNG Implementation", 
    qiprng_func, 
    CONFIG, 
    TEST_CATEGORIES
  )
}

if (TEST_PRNGS$qiprng_nocrypto) {
  prng_results$qiprng_nocrypto <- run_test_suite(
    "QIPRNG Without Cryptomixing", 
    qiprng_nocrypto_func, 
    CONFIG, 
    TEST_CATEGORIES
  )
}

if (TEST_PRNGS$dqrng) {
  prng_results$dqrng <- run_test_suite(
    "dqrng Package", 
    dqrng_prng, 
    CONFIG, 
    TEST_CATEGORIES
  )
}

# Print individual summaries
for (name in names(prng_results)) {
  cat("\n=========================================\n")
  print_test_summary(prng_results[[name]])
  
  # Save detailed results to CSV for further analysis
  if (CONFIG$save_results) {
    results_file <- file.path(output_dir, paste0(name, "_results.csv"))
    log_info(paste("Saving detailed results to", results_file))
    
    # Create a data frame with all test results
    results_df <- data.frame(Category = character(),
                            Test = character(),
                            Result = character(),
                            PValue = numeric(),
                            Details = character(),
                            stringsAsFactors = FALSE)
    
    for (cat_name in names(prng_results[[name]]$results)) {
      for (test_name in names(prng_results[[name]]$results[[cat_name]])) {
        test_result <- prng_results[[name]]$results[[cat_name]][[test_name]]
        results_df <- rbind(results_df, data.frame(
          Category = cat_name,
          Test = test_result$description,
          Result = ifelse(is.null(test_result$result), "UNKNOWN", test_result$result),
          PValue = ifelse(is.null(test_result$p_value) || is.na(test_result$p_value), NA, test_result$p_value),
          Details = ifelse(is.null(test_result$details), "", as.character(test_result$details)),
          stringsAsFactors = FALSE
        ))
      }
    }
    
    # Write to CSV
    tryCatch({
      write.csv(results_df, results_file, row.names = FALSE)
    }, error = function(e) {
      log_error(paste("Failed to save results to CSV:", conditionMessage(e)))
    })
  }
}

# Compare all tested PRNGs
compare_prng_results(prng_results)

# Calculate total execution time
total_time <- difftime(Sys.time(), total_start_time, units = "mins")
log_info(paste("All tests completed in", round(total_time, 2), "minutes"))
log_info(paste("Results saved to:", output_dir))

# Function to generate Quarto report
generate_quarto_report <- function(prng_results, config) {
  log_info("Generating Quarto report...")
  
  # Create report directory
  report_dir <- file.path(config$output_dir, "report")
  dir.create(report_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Create figures directory
  figures_dir <- file.path(report_dir, "figures")
  dir.create(figures_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Copy existing figures if they exist
  for (name in names(prng_results)) {
    prng_dir <- file.path(config$output_dir, tolower(gsub("[^[:alnum:]]", "_", name)))
    if (dir.exists(prng_dir)) {
      # Find all PNG files
      figure_files <- list.files(prng_dir, pattern = "\\.png$", full.names = TRUE)
      for (file in figure_files) {
        file.copy(file, figures_dir, overwrite = TRUE)
      }
    }
  }
  
  # Create data directory and save summaries
  data_dir <- file.path(report_dir, "data")
  dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Prepare summary data
  all_results <- data.frame(PRNG = character(),
                          Category = character(),
                          Test = character(),
                          Statistic = numeric(),
                          p_value = numeric(),
                          Result = character(),
                          stringsAsFactors = FALSE)
  
  for (name in names(prng_results)) {
    prng_name <- prng_results[[name]]$config$prng_type
    
    for (cat_name in names(prng_results[[name]]$results)) {
      for (test_name in names(prng_results[[name]]$results[[cat_name]])) {
        test_result <- prng_results[[name]]$results[[cat_name]][[test_name]]
        
        # Extract p-value and statistic if available
        p_value <- NA
        statistic <- NA
        
        if (!is.null(test_result$p_value) && !is.na(test_result$p_value)) {
          p_value <- as.numeric(test_result$p_value)
        }
        
        if (!is.null(test_result$statistic) && !is.na(test_result$statistic)) {
          statistic <- as.numeric(test_result$statistic)
        }
        
        all_results <- rbind(all_results, data.frame(
          PRNG = prng_name,
          Category = cat_name,
          Test = ifelse(!is.null(test_result$description), test_result$description, test_name),
          Statistic = statistic,
          p_value = p_value,
          Result = ifelse(!is.null(test_result$result), test_result$result, "UNKNOWN"),
          stringsAsFactors = FALSE
        ))
      }
    }
  }
  
  # Save all results
  saveRDS(all_results, file.path(data_dir, "all_test_results.rds"))
  
  # Calculate summary by category
  if (requireNamespace("dplyr", quietly = TRUE) && requireNamespace("tidyr", quietly = TRUE)) {
    library(dplyr)
    library(tidyr)
    
    # Create category summary
    category_summary <- all_results %>%
      group_by(PRNG, Category) %>%
      summarize(
        Total = n(),
        Passed = sum(Result == "PASS", na.rm = TRUE),
        Failed = sum(Result == "FAIL", na.rm = TRUE),
        Inconclusive = sum(Result != "PASS" & Result != "FAIL", na.rm = TRUE),
        PassRate = Passed / Total,
        .groups = "drop"
      )
    
    # Create overall summary
    overall_summary <- all_results %>%
      group_by(PRNG) %>%
      summarize(
        Total = n(),
        Passed = sum(Result == "PASS", na.rm = TRUE),
        Failed = sum(Result == "FAIL", na.rm = TRUE),
        Inconclusive = sum(Result != "PASS" & Result != "FAIL", na.rm = TRUE),
        PassRate = Passed / Total,
        .groups = "drop"
      ) %>%
      mutate(
        Rating = case_when(
          PassRate >= 0.9 ~ "★★★★★",
          PassRate >= 0.8 ~ "★★★★☆",
          PassRate >= 0.7 ~ "★★★☆☆",
          PassRate >= 0.6 ~ "★★☆☆☆",
          PassRate >= 0.4 ~ "★☆☆☆☆",
          TRUE ~ "☆☆☆☆☆"
        )
      )
    
    saveRDS(category_summary, file.path(data_dir, "category_summary.rds"))
    saveRDS(overall_summary, file.path(data_dir, "overall_summary.rds"))
  } else {
    log_warning("dplyr and tidyr packages required for detailed summary statistics in report")
  }
  
  # Create Quarto document
  quarto_file <- file.path(report_dir, "prng_report.qmd")
  
  quarto_content <- paste0('---
title: "', config$report_title, '"
author: "Statistical Testing Framework"
date: "', format(Sys.time(), "%Y-%m-%d"), '"
format: 
  ', config$report_format, ':
    toc: true
    number-sections: true
    colorlinks: true
execute:
  echo: false
  warning: false
---

```{r setup}
# Load required packages
if (!requireNamespace("knitr", quietly = TRUE)) install.packages("knitr")
if (!requireNamespace("kableExtra", quietly = TRUE)) install.packages("kableExtra")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")

library(knitr)
library(kableExtra)
library(dplyr)
library(ggplot2)

# Load result data
all_results <- readRDS("data/all_test_results.rds")
category_summary <- readRDS("data/category_summary.rds")
overall_summary <- readRDS("data/overall_summary.rds")
```

## Executive Summary

This report presents the results of statistical tests performed on multiple pseudo-random number generators (PRNGs). The tests evaluate various aspects of randomness including distribution properties, sequential independence, correlation characteristics, binary patterns, and more.

### Overview of Tested PRNGs

```{r}
# Show overall results table
overall_summary %>%
  mutate(PassRate = paste0(round(PassRate * 100, 1), "%")) %>%
  kable(caption = "Overall PRNG Performance", 
        col.names = c("PRNG", "Total Tests", "Tests Passed", "Tests Failed", 
                     "Inconclusive Tests", "Pass Rate", "Rating")) %>%
  kable_styling()
```

## Test Categories and Methodology

The statistical testing framework evaluates PRNGs across multiple categories:

1. **Basic Distribution Tests**: Evaluate whether the output follows the expected uniform distribution
2. **Runs and Independence Tests**: Analyze sequential patterns and independence
3. **Correlation Tests**: Measure relationships between values at various lags
4. **Binary and Bitwise Tests**: Examine randomness at the bit level 
5. **Classical Tests**: Apply traditional statistical approaches for randomness evaluation
6. **Compression Tests**: Leverage the principle that truly random data should not be significantly compressible

### Performance by Test Category

```{r}
# Show category summary
category_summary %>%
  mutate(PassRate = paste0(round(PassRate * 100, 1), "%")) %>%
  arrange(PRNG, Category) %>%
  kable(caption = "Performance by Test Category",
        col.names = c("PRNG", "Category", "Total Tests", "Tests Passed", 
                     "Tests Failed", "Inconclusive Tests", "Pass Rate")) %>%
  kable_styling()
```

## Detailed Test Results

```{r}
# Function to display results for a specific category
display_category_results <- function(category_name) {
  all_results %>%
    filter(Category == category_name) %>%
    select(PRNG, Test, p_value, Result) %>%
    mutate(p_value = round(p_value, 4)) %>%
    kable(caption = paste(category_name, "Test Results"),
          col.names = c("PRNG", "Test", "p-value", "Result")) %>%
    kable_styling() %>%
    row_spec(which(all_results$Result[all_results$Category == category_name] == "FAIL"), 
             background = "#FFDDDD")
}
```

### Basic Distribution Tests

The basic distribution tests evaluate whether the output values follow the expected uniform distribution.

```{r, fig.align="center", out.width="90%"}
# Display any distribution histograms if available
files <- list.files(path = "figures", pattern = "histogram|distribution", full.names = TRUE)
if (length(files) > 0) {
  include_graphics(files[1])
} else {
  cat("No distribution visualizations available.")
}
```

```{r}
display_category_results("basic")
```

### Runs and Independence Tests

These tests evaluate the independence of consecutive values in the sequence.

```{r, fig.align="center", out.width="90%"}
# Display any runs visualizations if available
files <- list.files(path = "figures", pattern = "runs|independence", full.names = TRUE)
if (length(files) > 0) {
  include_graphics(files[1])
} else {
  cat("No runs test visualizations available.")
}
```

```{r}
display_category_results("runs")
```

### Correlation Tests

Correlation tests examine relationships between values at different lags to detect any linear dependencies.

```{r, fig.align="center", out.width="90%"}
# Display any correlation visualizations if available
files <- list.files(path = "figures", pattern = "correlation|acf|lag", full.names = TRUE)
if (length(files) > 0) {
  include_graphics(files[1])
} else {
  cat("No correlation visualizations available.")
}
```

```{r}
display_category_results("correlation")
```

### Binary and Bitwise Tests

Binary tests examine randomness at the bit level, which is particularly important for cryptographic applications.

```{r, fig.align="center", out.width="90%"}
# Display any binary visualizations if available
files <- list.files(path = "figures", pattern = "bit|binary", full.names = TRUE)
if (length(files) > 0) {
  include_graphics(files[1])
} else {
  cat("No binary test visualizations available.")
}
```

```{r}
display_category_results("binary")
```

### Classical Tests

Classical tests include traditional approaches such as the coupon collector test and poker hand analysis.

```{r}
display_category_results("classical")
```

### Compression Tests

Compression tests evaluate the information content of the PRNG output.

```{r, fig.align="center", out.width="90%"}
# Display any entropy visualizations if available
files <- list.files(path = "figures", pattern = "entropy|compression", full.names = TRUE)
if (length(files) > 0) {
  include_graphics(files[1])
} else {
  cat("No entropy or compression visualizations available.")
}
```

```{r}
display_category_results("compression")
```

## Conclusions and Recommendations

Based on the test results, the following conclusions can be drawn:

1. **Best Overall Performance**: The PRNG with the highest overall pass rate is the most suitable for general-purpose applications.

2. **Category-Specific Performance**: Some PRNGs excel in specific test categories, making them more suitable for specialized applications:
   - Distribution tests: Important for statistical simulations
   - Binary tests: Critical for cryptographic applications
   - Correlation tests: Vital for Monte Carlo simulations

3. **Recommendations**:
   - For cryptographic applications, choose PRNGs with strong bit-level randomness
   - For statistical simulations, prioritize PRNGs with excellent distribution properties
   - For general applications, select PRNGs with balanced performance across all categories

## Appendix: Test Definitions

### Basic Distribution Tests
- **Kolmogorov-Smirnov Test**: Compares the empirical distribution with the expected uniform distribution
- **Chi-Squared Test**: Evaluates the goodness-of-fit to a uniform distribution
- **Mean Test**: Verifies that the mean is close to the expected value (0.5 for uniform[0,1])
- **Variance Test**: Confirms that the variance matches the expected value (1/12 for uniform[0,1])

### Runs Tests
- **Runs Test**: Analyzes sequences of values above/below the median
- **Turning Points Test**: Examines the frequency of local maxima and minima

### Correlation Tests
- **Serial Correlation**: Measures the correlation between consecutive values
- **ACF Test**: Evaluates autocorrelation at various lags
- **Ljung-Box Test**: Tests for the presence of autocorrelation across multiple lags

### Binary Tests
- **Monobit Test**: Checks if the proportion of 0s and 1s is balanced
- **Bit Position Test**: Verifies that each bit position has balanced 0s and 1s
- **Bit Runs Test**: Analyzes runs of 0s and 1s in the bit sequence
')
  
  # Write the Quarto document
  writeLines(quarto_content, quarto_file)
  
  # Render the report
  tryCatch({
    if (requireNamespace("quarto", quietly = TRUE)) {
      log_info("Rendering report using quarto R package...")
      quarto::quarto_render(quarto_file)
    } else {
      # Try using system quarto command
      log_info("Attempting to render report using system quarto command...")
      cmd <- sprintf("quarto render %s", quarto_file)
      
      if (.Platform$OS.type == "windows") {
        shell(cmd)
      } else {
        system(cmd)
      }
    }
    
    # Check if the output file exists
    expected_output <- file.path(report_dir, paste0("prng_report.", config$report_format))
    if (file.exists(expected_output)) {
      log_info(paste("Report successfully generated at:", expected_output))
    } else {
      log_warning("Report generation may have failed. Check for errors above.")
    }
    
  }, error = function(e) {
    log_error(paste("Failed to render report:", conditionMessage(e)))
    log_info("You can manually render the report by opening prng_report.qmd in RStudio or using quarto CLI")
  })
  
  return(invisible(NULL))
}

# Print overall state
cat("\n=====================================================================\n")
cat("PRNG STATISTICAL TESTING FRAMEWORK - EXECUTION SUMMARY\n")
cat("=====================================================================\n\n")

cat(paste("Total PRNGs tested: ", length(prng_results), "\n"))
cat(paste("Sample size: ", CONFIG$basic_sample_size, "\n"))
cat(paste("Test categories enabled: ", sum(unlist(TEST_CATEGORIES)), "/", length(TEST_CATEGORIES), "\n"))
cat(paste("Test duration: ", round(total_time, 2), " minutes\n", sep=""))
cat(paste("Output directory: ", output_dir, "\n\n", sep=""))

cat("PRNGs tested:\n")
for (name in names(prng_results)) {
  total_tests <- 0
  total_pass <- 0
  for (cat_name in names(prng_results[[name]]$results)) {
    for (test_name in names(prng_results[[name]]$results[[cat_name]])) {
      test_result <- prng_results[[name]]$results[[cat_name]][[test_name]]
      total_tests <- total_tests + 1
      if (!is.null(test_result$result) && test_result$result == "PASS") {
        total_pass <- total_pass + 1
      }
    }
  }
  
  pass_rate <- if(total_tests > 0) round(100 * total_pass / total_tests, 1) else 0
  cat(sprintf("  - %s: %d/%d tests passed (%.1f%%)\n", 
            prng_results[[name]]$config$prng_type, 
            total_pass, total_tests, pass_rate))
}

cat("\nFor detailed results, see the CSV files in the output directory.\n")
cat("=====================================================================\n")

# Generate Quarto report if enabled
if (CONFIG$generate_report) {
  generate_quarto_report(prng_results, CONFIG)
}

