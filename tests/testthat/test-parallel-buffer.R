context("Parallel Buffer Filling Tests")

library(qiprng)

# Helper function to measure execution time
time_execution <- function(expr) {
  start_time <- Sys.time()
  result <- eval(expr)
  end_time <- Sys.time()
  list(
    result = result, 
    time = as.numeric(end_time - start_time, units = "secs")
  )
}

test_that("Parallel buffer filling is functional", {
  # Skip on CRAN
  skip_on_cran()
  
  # Create PRNG with parallel filling enabled
  cfg_parallel <- list(
    a = 2,
    b = 5,
    c = -2,
    mpfr_precision = 64,
    distribution = "uniform_01",
    buffer_size = 50000,  # Large buffer to benefit from parallelism
    use_parallel_filling = TRUE
  )
  
  createPRNG(cfg_parallel)
  parallel_nums <- generatePRNG(10000)
  
  # Basic validity checks
  expect_equal(length(parallel_nums), 10000)
  expect_true(all(parallel_nums >= 0 & parallel_nums <= 1))
  
  # Distribution checks
  ks_result <- ks.test(parallel_nums, "punif")
  expect_gt(ks_result$p.value, 0.01)
  
  # Clean up
  cleanup_prng()
})

test_that("Parallel buffer filling improves performance for large buffers", {
  # Skip on CRAN and CI environments
  skip_on_cran()
  
  # Only run benchmark tests in interactive mode or explicitly enabled
  skip_if(!interactive() && Sys.getenv("RUN_PERFORMANCE_TESTS") != "true", 
          "Skipping performance tests in non-interactive mode")
  
  # Parameters for testing
  buffer_sizes <- c(10000, 100000, 500000)
  sample_count <- 1000000
  
  # Results storage
  results <- data.frame(
    buffer_size = integer(),
    parallel = logical(),
    time = numeric()
  )
  
  for (size in buffer_sizes) {
    # Test with parallel filling
    cfg_parallel <- list(
      a = 2,
      b = 5,
      c = -2,
      mpfr_precision = 53,
      distribution = "uniform_01",
      buffer_size = size,
      use_parallel_filling = TRUE
    )
    
    createPRNG(cfg_parallel)
    time_parallel <- time_execution(quote(generatePRNG(sample_count)))$time
    cleanup_prng()
    
    # Test with sequential filling
    cfg_sequential <- list(
      a = 2,
      b = 5,
      c = -2,
      mpfr_precision = 53,
      distribution = "uniform_01",
      buffer_size = size,
      use_parallel_filling = FALSE
    )
    
    createPRNG(cfg_sequential)
    time_sequential <- time_execution(quote(generatePRNG(sample_count)))$time
    cleanup_prng()
    
    # Store results
    results <- rbind(results, 
                     data.frame(buffer_size = size, 
                                parallel = TRUE, 
                                time = time_parallel))
    
    results <- rbind(results, 
                     data.frame(buffer_size = size, 
                                parallel = FALSE, 
                                time = time_sequential))
    
    # For large buffer sizes, parallel should be faster
    if (size >= 100000) {
      expect_lt(time_parallel, time_sequential, 
                info = paste("Parallel filling should be faster with buffer size", size))
    }
  }
  
  # Print performance results
  if (interactive()) {
    print(results)
  }
  
  # At least one buffer size should show significant improvement
  parallel_times <- results$time[results$parallel]
  sequential_times <- results$time[!results$parallel]
  
  # At least one buffer size should show at least 10% improvement
  performance_gain <- (sequential_times - parallel_times) / sequential_times
  expect_true(any(performance_gain > 0.1), 
              "Parallel filling should provide at least 10% improvement for some buffer size")
})

test_that("Parallel buffer filling is thread-safe", {
  skip_on_cran()
  
  # Test with threading enabled
  cfg <- list(
    a = 2,
    b = 5,
    c = -2,
    mpfr_precision = 64,
    distribution = "uniform_01",
    buffer_size = 50000,
    use_parallel_filling = TRUE,
    use_threading = TRUE  # Enable thread-local PRNGs
  )
  
  createPRNG(cfg)
  
  # Test with parallel package
  if (requireNamespace("parallel", quietly = TRUE)) {
    # Determine number of cores to use (limit to 4 for testing)
    cores <- min(4, parallel::detectCores())
    
    # Function to generate numbers in each thread
    thread_fn <- function(i) {
      qiprng::generatePRNG(10000)
    }
    
    # Create a cluster
    cl <- parallel::makeCluster(cores)
    
    # Load the qiprng package on all workers
    parallel::clusterEvalQ(cl, {
      library(qiprng)
    })
    
    # Send the configuration to all workers, explicitly providing the environment
    parallel::clusterExport(cl, "cfg", envir = environment())
    
    # Initialize PRNG on all workers
    parallel::clusterEvalQ(cl, {
      createPRNG(cfg)
    })
    
    # Run parallel generation
    tryCatch({
      results <- parallel::parLapply(cl, 1:cores, thread_fn)
      
      # Verify all threads produced valid results
      for (i in 1:cores) {
        expect_equal(length(results[[i]]), 10000)
        expect_true(all(results[[i]] >= 0 & results[[i]] <= 1))
      }
      
      # Verify statistical independence between threads
      for (i in 1:(cores-1)) {
        for (j in (i+1):cores) {
          correlation <- cor(results[[i]], results[[j]])
          expect_lt(abs(correlation), 0.1, 
                    label = paste("Correlation between thread", i, "and", j, "should be low"))
        }
      }
    }, finally = {
      # Clean up cluster
      parallel::stopCluster(cl)
    })
  }
  
  # Clean up
  cleanup_prng()
})

test_that("Parallel buffer filling maintains statistical properties", {
  skip_on_cran()
  
  # Create PRNG with parallel filling
  cfg_parallel <- list(
    a = 2,
    b = 5,
    c = -2,
    mpfr_precision = 64,
    distribution = "uniform_01",
    buffer_size = 50000,
    use_parallel_filling = TRUE
  )
  
  createPRNG(cfg_parallel)
  parallel_nums <- generatePRNG(100000)
  cleanup_prng()
  
  # Create PRNG with sequential filling
  cfg_sequential <- list(
    a = 2,
    b = 5,
    c = -2,
    mpfr_precision = 64,
    distribution = "uniform_01",
    buffer_size = 50000,
    use_parallel_filling = FALSE
  )
  
  createPRNG(cfg_sequential)
  sequential_nums <- generatePRNG(100000)
  cleanup_prng()
  
  # Test uniform distribution properties for both
  expect_gt(ks.test(parallel_nums, "punif")$p.value, 0.01, 
            "Parallel filling should maintain uniform distribution")
  expect_gt(ks.test(sequential_nums, "punif")$p.value, 0.01, 
            "Sequential filling should maintain uniform distribution")
  
  # Test mean and variance
  expect_lt(abs(mean(parallel_nums) - 0.5), 0.01, 
            "Parallel filling should maintain correct mean")
  expect_lt(abs(var(parallel_nums) - 1/12), 0.01, 
            "Parallel filling should maintain correct variance")
  
  # Test autocorrelation (no strong patterns)
  if (requireNamespace("stats", quietly = TRUE)) {
    acf_result <- stats::acf(parallel_nums, lag.max = 10, plot = FALSE)
    # Ignore lag 0 (always 1) and check that others are small
    expect_true(all(abs(acf_result$acf[-1]) < 0.1), 
                "Parallel filling should not introduce autocorrelation")
  }
})
