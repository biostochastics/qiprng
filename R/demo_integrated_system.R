# Demo: Integrated Configuration, Progress Monitoring, and Result Aggregation
# --------------------------------------------------------------------------

# Load required modules
source("R/statisticaltests/config_manager.R")
source("R/statisticaltests/progress_monitor.R")
source("R/statisticaltests/result_aggregator.R")

# Demo 1: Configuration Setup
cat("=== Demo 1: Configuration Setup ===\n")

# Create configuration
config <- load_config()

# Show some configuration values
cat("\nTest Configuration:\n")
cat(sprintf("- Sample size: %d\n", get_test_param("global.default_sample_size", config)))
cat(sprintf("- Significance level: %.3f\n", get_test_param("global.significance_level", config)))
cat(sprintf("- Parallel enabled: %s\n", get_test_param("performance.parallel_enabled", config)))
cat(sprintf("- Max cores: %d\n", get_test_param("performance.max_cores", config)))

# Demo 2: Progress Monitoring
cat("\n=== Demo 2: Progress Monitoring ===\n")

# Create progress monitor
monitor <- ProgressMonitor$new("console", list(use_cli = FALSE))

# Simulate test execution
total_tests <- 20
categories <- c("basic", "classical", "binary")

monitor$start(total_tests, categories)

# Simulate running tests
set.seed(123)
for (cat in categories) {
  monitor$update_category(cat, total = 7, status = "running")
  
  for (i in 1:7) {
    test_name <- paste0(cat, "_test_", i)
    
    # Start test
    monitor$update_test(test_name, "running")
    
    # Simulate test execution
    Sys.sleep(0.1)
    
    # Complete test
    status <- if (runif(1) > 0.1) "completed" else "failed"
    monitor$update_test(test_name, status)
  }
}

monitor$complete()

# Demo 3: Result Aggregation
cat("\n=== Demo 3: Result Aggregation ===\n")

# Create result aggregator
aggregator <- ResultAggregator$new()

# Add simulated test results
set.seed(123)
for (cat in categories) {
  for (i in 1:7) {
    test_name <- paste0(cat, "_test_", i)
    p_value <- runif(1)
    
    result <- list(
      result = if (p_value > 0.01) "PASS" else "FAIL",
      p_value = p_value,
      statistic = rnorm(1),
      details = list(
        method = paste("Simulated", cat, "test"),
        sample_size = get_test_param("global.default_sample_size", config)
      )
    )
    
    aggregator$add_result(cat, test_name, result)
  }
}

# Compute statistics
aggregator$compute_statistics()

# Show summary
cat("\n", aggregator$get_summary("text"), "\n")

# Demo 4: Integrated Usage with Callbacks
cat("\n=== Demo 4: Integrated System with Callbacks ===\n")

# Create new instances for integrated demo
config2 <- load_config()
monitor2 <- ProgressMonitor$new("console", list(use_cli = FALSE, quiet = TRUE))
aggregator2 <- ResultAggregator$new()

# Add callback that connects monitor to aggregator
monitor2$add_callback(function(event, data, monitor) {
  if (event == "test" && data$status %in% c("completed", "failed")) {
    # Create mock result based on status
    result <- list(
      result = if (data$status == "completed") "PASS" else "FAIL",
      p_value = if (data$status == "completed") runif(1, 0.05, 0.95) else runif(1, 0, 0.01),
      statistic = rnorm(1)
    )
    
    # Extract category from test name
    category <- strsplit(data$test, "_")[[1]][1]
    
    # Add to aggregator
    aggregator2$add_result(category, data$test, result)
  }
}, events = "test")

# Run integrated test simulation
total_tests2 <- 15
monitor2$start(total_tests2)

cat("\nRunning integrated test simulation...\n")
for (i in 1:total_tests2) {
  category <- sample(c("basic", "classical", "binary"), 1)
  test_name <- paste0(category, "_test_", i)
  
  monitor2$update_test(test_name, "running")
  Sys.sleep(0.05)
  
  status <- if (runif(1) > 0.15) "completed" else "failed"
  monitor2$update_test(test_name, status)
}

monitor2$complete()

# Show integrated results
aggregator2$compute_statistics()
cat("\nIntegrated Results Summary:\n")
summary_df <- aggregator2$get_summary("data.frame")
print(summary_df)

# Demo 5: Using Configuration in Test Functions
cat("\n=== Demo 5: Configuration-Driven Test Function ===\n")

# Example test function that uses configuration
run_configured_test <- function(data, test_type, config = NULL) {
  if (is.null(config)) {
    config <- load_config()
  }
  
  # Get test-specific parameters
  test_config <- get_test_config(paste0(test_type, "_tests"), config)
  
  # Get global parameters
  sig_level <- get_test_param("global.significance_level", config, 0.01)
  
  cat(sprintf("\nRunning %s test with:\n", test_type))
  cat(sprintf("- Significance level: %.3f\n", sig_level))
  
  if (test_type == "classical" && !is.null(test_config$chi_square)) {
    cat(sprintf("- Chi-square bins: %d\n", test_config$chi_square$bins))
    cat(sprintf("- Min expected count: %d\n", test_config$chi_square$min_expected_count))
  }
  
  # Simulate test result
  p_value <- runif(1)
  result <- list(
    test = test_type,
    p_value = p_value,
    result = if (p_value > sig_level) "PASS" else "FAIL",
    parameters_used = list(
      significance_level = sig_level,
      test_config = test_config
    )
  )
  
  return(result)
}

# Run example
test_data <- runif(1000)
result <- run_configured_test(test_data, "classical", config)
cat(sprintf("\nTest result: %s (p-value: %.4f)\n", result$result, result$p_value))

# Demo 6: Performance Monitoring
cat("\n=== Demo 6: Performance Configuration ===\n")

# Check performance settings
perf_config <- get_test_config("performance", config)
cat("\nPerformance Configuration:\n")
cat(sprintf("- Parallel enabled: %s\n", perf_config$parallel_enabled))
cat(sprintf("- Max cores: %d\n", perf_config$max_cores))
cat(sprintf("- Cache enabled: %s\n", perf_config$cache_enabled))
cat(sprintf("- Memory limit: %d MB\n", perf_config$memory_limit_mb))

# Demo complete
cat("\n=== Integrated System Demo Complete ===\n")
cat("\nThe integrated system provides:\n")
cat("1. Flexible configuration management for tools and tests\n")
cat("2. Real-time progress monitoring with multiple backends\n")
cat("3. Comprehensive result aggregation and statistics\n")
cat("4. Seamless integration between all components\n")
cat("5. Export capabilities for results and reports\n")