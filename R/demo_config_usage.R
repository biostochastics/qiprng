# Demo: Using the Enhanced Configuration Management System
# --------------------------------------------------------

# Load the configuration manager
source("R/statisticaltests/config_manager.R")

# Example 1: Create and save a configuration file
cat("=== Creating Example Configuration ===\n")
create_example_config("my_qiprng_config.json")

# Example 2: Load configuration
cat("\n=== Loading Configuration ===\n")
config <- load_config("my_qiprng_config.json")

# Example 3: Access internal test parameters
cat("\n=== Accessing Internal Test Parameters ===\n")

# Get global parameters
sample_size <- get_test_param("global.default_sample_size", config)
sig_level <- get_test_param("global.significance_level", config)
cat(sprintf("Default sample size: %d\n", sample_size))
cat(sprintf("Significance level: %.3f\n", sig_level))

# Get test-specific parameters
chi_square_bins <- get_test_param("classical_tests.chi_square.bins", config)
compression_algos <- get_test_param("compression_tests.algorithms", config)
cat(sprintf("Chi-square test bins: %d\n", chi_square_bins))
cat(sprintf("Compression algorithms: %s\n", paste(compression_algos, collapse = ", ")))

# Get performance settings
parallel_enabled <- get_test_param("performance.parallel_enabled", config)
max_cores <- get_test_param("performance.max_cores", config)
cat(sprintf("Parallel processing: %s\n", ifelse(parallel_enabled, "Enabled", "Disabled")))
cat(sprintf("Max cores: %d\n", max_cores))

# Example 4: Modify configuration at runtime
cat("\n=== Modifying Configuration ===\n")

# Change significance level
config <- set_test_param("global.significance_level", 0.05, config)
new_sig_level <- get_test_param("global.significance_level", config)
cat(sprintf("Updated significance level: %.3f\n", new_sig_level))

# Disable parallel processing
config <- set_test_param("performance.parallel_enabled", FALSE, config)
cat("Parallel processing disabled\n")

# Example 5: Get entire test category configuration
cat("\n=== Getting Test Category Configuration ===\n")
binary_config <- get_test_config("binary_tests", config)
cat("Binary tests configuration:\n")
print(str(binary_config))

# Example 6: Environment variable overrides
cat("\n=== Environment Variable Overrides ===\n")
Sys.setenv(QIPRNG_SAMPLE_SIZE = "50000")
Sys.setenv(QIPRNG_PARALLEL_ENABLED = "TRUE")
Sys.setenv(QIPRNG_MAX_CORES = "4")

# Reload config with env vars
config_with_env <- load_config()
env_sample_size <- get_test_param("global.default_sample_size", config_with_env)
env_parallel <- get_test_param("performance.parallel_enabled", config_with_env)
env_cores <- get_test_param("performance.max_cores", config_with_env)

cat(sprintf("Sample size from env: %d\n", env_sample_size))
cat(sprintf("Parallel from env: %s\n", env_parallel))
cat(sprintf("Max cores from env: %d\n", env_cores))

# Example 7: Validate configuration
cat("\n=== Validating Configuration ===\n")
if (validate_internal_config(config)) {
  cat("Internal configuration is valid\n")
} else {
  cat("Internal configuration has issues\n")
}

# Example 8: Using configuration in tests
cat("\n=== Using Configuration in Tests ===\n")

# Function that uses configuration
run_chi_square_test_with_config <- function(data, config = NULL) {
  if (is.null(config)) {
    config <- load_config()
  }

  # Get test parameters from config
  bins <- get_test_param("classical_tests.chi_square.bins", config, default = 10)
  sig_level <- get_test_param("classical_tests.chi_square.significance_level", config, default = 0.01)
  min_count <- get_test_param("classical_tests.chi_square.min_expected_count", config, default = 5)

  cat(sprintf("Running chi-square test with:\n"))
  cat(sprintf("  Bins: %d\n", bins))
  cat(sprintf("  Significance level: %.3f\n", sig_level))
  cat(sprintf("  Min expected count: %d\n", min_count))

  # Test implementation would go here...
}

# Example usage
test_data <- runif(1000)
run_chi_square_test_with_config(test_data, config)

# Clean up
file.remove("my_qiprng_config.json")
cat("\n=== Demo Complete ===\n")
