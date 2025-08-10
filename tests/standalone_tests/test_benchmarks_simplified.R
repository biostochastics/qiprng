# Simplified benchmark tests

library(testthat)
library(qiprng)

# Basic performance test
test_that("PRNG has reasonable performance", {
  skip_on_cran()
  skip_on_ci()
  
  # Create a PRNG with default settings
  createPRNG()
  
  # Measure time to generate numbers
  n_values <- 100000
  start_time <- Sys.time()
  values <- generatePRNG(n_values)
  end_time <- Sys.time()
  
  # Calculate generation rate (values per second)
  elapsed <- as.numeric(difftime(end_time, start_time, units="secs"))
  rate <- n_values / elapsed
  
  # Clean up
  cleanupPRNG()
  
  # Log results
  cat("\nPerformance test results:\n")
  cat("Generated", n_values, "values in", round(elapsed, 4), "seconds\n")
  cat("Generation rate:", format(rate, scientific=FALSE, big.mark=","), "values/second\n")
  
  # Very basic assertion - should be able to generate at least 10,000 values per second
  # on any modern hardware
  expect_gt(rate, 10000)
})

# Compare distributions
test_that("Different distribution types have reasonable performance", {
  skip_on_cran()
  skip_on_ci()
  
  # Helper function
  benchmark_distribution <- function(dist_name, dist_config) {
    createPRNG(dist_config)
    n_values <- 10000
    start_time <- Sys.time()
    values <- generatePRNG(n_values)
    end_time <- Sys.time()
    elapsed <- as.numeric(difftime(end_time, start_time, units="secs"))
    rate <- n_values / elapsed
    cleanupPRNG()
    
    cat("Distribution:", dist_name, "- Rate:", 
        format(rate, scientific=FALSE, big.mark=","), "values/second\n")
    
    return(rate)
  }
  
  # Test different distributions
  cat("\nDistribution performance comparison:\n")
  
  uniform_rate <- benchmark_distribution("Uniform", list(distribution = "uniform_01"))
  normal_rate <- benchmark_distribution("Normal", list(distribution = "normal"))
  
  # Normal should not be drastically slower than uniform (within reason)
  expect_gt(normal_rate, uniform_rate * 0.2)
})