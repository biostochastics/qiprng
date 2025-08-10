#!/usr/bin/env Rscript

# Stress test for normal distribution in threaded mode
library(qiprng)
library(parallel)

cat("\n===== Stress Testing Normal Distribution With Threading =====\n\n")

# Create a stress test function that generates many values
stress_test <- function(num_cores = 2, iterations = 5, samples_per_iter = 10000) {
  # Create PRNG with threading
  cat("Creating PRNG with threading enabled\n")
  createPRNG(list(
    distribution = "normal",
    normal_mean = 0,
    normal_sd = 1,
    use_threading = TRUE,
    debug = TRUE
  ))
  
  # Make cluster and export qiprng
  cl <- makeCluster(num_cores)
  clusterEvalQ(cl, {
    library(qiprng)
    # Need to initialize PRNG in each worker
    createPRNG(list(
      distribution = "normal",
      normal_mean = 0,
      normal_sd = 1,
      debug = TRUE
    ))
  })
  
  # Function to run in parallel
  test_fun <- function(id, iterations, samples) {
    results <- list()
    for (i in 1:iterations) {
      # Generate values
      values <- generatePRNG(samples)
      
      # Calculate statistics
      stats <- list(
        thread = id,
        iteration = i,
        mean = mean(values),
        var = var(values),
        min = min(values),
        max = max(values),
        identical = all(values == values[1])
      )
      
      results[[i]] <- stats
    }
    return(results)
  }
  
  # Run the stress test in parallel
  cat(sprintf("Running test with %d cores, %d iterations each, %d samples per iteration\n", 
              num_cores, iterations, samples_per_iter))
  
  results <- parLapply(cl, 1:num_cores, test_fun, iterations, samples_per_iter)
  
  # Stop the cluster
  stopCluster(cl)
  
  # Analyze results
  cat("\nResults Summary:\n")
  
  # Flatten results
  all_stats <- list()
  for (thread_results in results) {
    for (iter_result in thread_results) {
      all_stats <- c(all_stats, list(iter_result))
    }
  }
  
  # Check for identical values (indicating error)
  identical_count <- sum(sapply(all_stats, function(x) x$identical))
  if (identical_count > 0) {
    cat(sprintf("  WARNING: %d / %d runs had all identical values\n", 
                identical_count, length(all_stats)))
  } else {
    cat("  All runs generated unique values (good!)\n")
  }
  
  # Calculate mean statistics
  means <- sapply(all_stats, function(x) x$mean)
  variances <- sapply(all_stats, function(x) x$var)
  
  cat(sprintf("  Mean of means: %.6f (expect ~0)\n", mean(means)))
  cat(sprintf("  SD of means: %.6f\n", sd(means)))
  cat(sprintf("  Mean of variances: %.6f (expect ~1)\n", mean(variances)))
  cat(sprintf("  SD of variances: %.6f\n", sd(variances)))
  
  # Clean up
  cleanup_prng()
  return(all_stats)
}

# Run the stress test
stress_results <- stress_test(num_cores = 2, iterations = 3, samples_per_iter = 5000)

cat("\n===== Stress test completed! =====\n")