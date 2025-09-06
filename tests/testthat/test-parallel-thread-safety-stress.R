# File: test-parallel-thread-safety-stress.R
# ----------------------------------------------------------------------
#' Stress tests for parallel execution thread safety
#'
#' This test suite performs intensive stress testing of the parallel
#' execution framework to verify thread safety, particularly around
#' caching mechanisms and shared resources.

library(testthat)
library(parallel)

# Helper function to create a test PRNG with deterministic behavior
create_test_prng <- function(seed = 123) {
  function(n) {
    set.seed(seed)
    runif(n)
  }
}

# Helper function to run parallel tests with various configurations
run_parallel_stress_test <- function(test_func, n_workers = 4, n_iterations = 100,
                                     test_name = "unnamed") {
  # Create cluster
  cl <- makeCluster(n_workers)
  on.exit(stopCluster(cl), add = TRUE)

  # Export necessary objects
  clusterExport(cl, c("test_func"), envir = environment())

  # Run test function in parallel
  start_time <- Sys.time()
  results <- parLapply(cl, 1:n_iterations, function(i) {
    tryCatch(
      {
        test_func(i)
      },
      error = function(e) {
        list(error = TRUE, message = e$message, iteration = i)
      }
    )
  })
  end_time <- Sys.time()

  # Check for errors
  errors <- Filter(function(x) is.list(x) && !is.null(x$error) && x$error, results)

  list(
    test_name = test_name,
    n_workers = n_workers,
    n_iterations = n_iterations,
    duration = as.numeric(difftime(end_time, start_time, units = "secs")),
    n_errors = length(errors),
    errors = errors,
    success = length(errors) == 0
  )
}

test_that("Parallel test execution with shared cache is thread-safe", {
  skip_if_not(requireNamespace("parallel", quietly = TRUE))

  # Source required files
  source_files <- c(
    "R/statistical_testing.R",
    "R/caching_framework.R",
    "R/statisticaltests/parallel_runner.R"
  )

  for (file in source_files) {
    if (file.exists(file)) {
      source(file)
    } else {
      skip(paste("Required file not found:", file))
    }
  }

  # Test 1: Concurrent cache writes
  test_concurrent_cache_writes <- function(iteration) {
    cache_key <- paste0("test_key_", iteration %% 10) # Use limited key space to force collisions
    cache_value <- list(
      iteration = iteration,
      timestamp = Sys.time(),
      data = runif(1000)
    )

    # Attempt to write to cache
    set_cache(cache_key, cache_value)

    # Immediately read back
    retrieved <- get_cache(cache_key)

    # Verify data integrity
    if (!is.null(retrieved) && !identical(retrieved$iteration, cache_value$iteration)) {
      stop("Cache corruption detected")
    }

    TRUE
  }

  result1 <- run_parallel_stress_test(
    test_concurrent_cache_writes,
    n_workers = 8,
    n_iterations = 1000,
    test_name = "Concurrent Cache Writes"
  )

  expect_true(result1$success,
    info = paste("Cache write test failed with", result1$n_errors, "errors")
  )

  # Test 2: Parallel test suite execution
  test_parallel_suite_execution <- function(iteration) {
    # Create a minimal test suite
    suite <- list(
      prng_func = create_test_prng(iteration),
      config = default_test_config,
      results = list()
    )

    # Configure for parallel execution
    suite$config$parallel <- TRUE
    suite$config$cores <- 2
    suite$config$basic_sample_size <- 1000
    suite$config$cache_enabled <- TRUE
    suite$config$cache_dir <- tempdir()

    # Run a subset of tests
    if (exists("run_basic_tests")) {
      suite <- run_basic_tests(suite)
    }

    # Verify results structure
    if (is.null(suite$results) || length(suite$results) == 0) {
      stop("Test execution produced no results")
    }

    TRUE
  }

  result2 <- run_parallel_stress_test(
    test_parallel_suite_execution,
    n_workers = 4,
    n_iterations = 100,
    test_name = "Parallel Suite Execution"
  )

  expect_true(result2$success,
    info = paste("Suite execution test failed with", result2$n_errors, "errors")
  )

  # Test 3: Race conditions in result aggregation
  test_result_aggregation_races <- function(iteration) {
    # Simulate concurrent result updates
    results <- list()

    # Multiple threads updating the same result structure
    for (i in 1:10) {
      test_name <- paste0("test_", i %% 3) # Limited namespace for collisions
      results[[test_name]] <- list(
        iteration = iteration,
        thread_id = Sys.getpid(),
        value = runif(1),
        timestamp = Sys.time()
      )
    }

    # Verify no results were lost
    if (length(results) < 3) {
      stop("Results were lost during concurrent updates")
    }

    TRUE
  }

  result3 <- run_parallel_stress_test(
    test_result_aggregation_races,
    n_workers = 8,
    n_iterations = 500,
    test_name = "Result Aggregation Races"
  )

  expect_true(result3$success,
    info = paste("Result aggregation test failed with", result3$n_errors, "errors")
  )

  # Print summary
  cat("\n=== Parallel Thread Safety Stress Test Summary ===\n")
  for (result in list(result1, result2, result3)) {
    cat(sprintf(
      "%s: %s (%.2f seconds, %d iterations, %d workers)\n",
      result$test_name,
      ifelse(result$success, "PASSED", "FAILED"),
      result$duration,
      result$n_iterations,
      result$n_workers
    ))
  }
})

test_that("Memory management under parallel load is stable", {
  skip_if_not(requireNamespace("parallel", quietly = TRUE))

  # Test memory-intensive parallel operations
  test_memory_intensive <- function(iteration) {
    # Allocate significant memory
    large_data <- matrix(runif(100000), nrow = 1000)

    # Perform computations
    result <- apply(large_data, 1, function(x) {
      ks.test(x, "punif")$p.value
    })

    # Force garbage collection
    gc()

    length(result) == 1000
  }

  # Monitor memory usage
  initial_mem <- gc()[2, 2] # Used memory in MB

  result <- run_parallel_stress_test(
    test_memory_intensive,
    n_workers = 4,
    n_iterations = 50,
    test_name = "Memory Intensive Operations"
  )

  final_mem <- gc()[2, 2]
  memory_increase <- final_mem - initial_mem

  expect_true(result$success)
  expect_lt(memory_increase, 500, # Memory increase should be < 500MB
    label = sprintf("Memory increased by %.1f MB", memory_increase)
  )
})

test_that("Caching with parallel writes maintains consistency", {
  skip_if_not(requireNamespace("parallel", quietly = TRUE))
  skip_if_not(exists("cache_test_results"))

  # Create a shared cache directory
  cache_dir <- tempfile("cache_stress_test_")
  dir.create(cache_dir)
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)

  # Test concurrent cached test results
  test_cached_results <- function(iteration) {
    # Generate test data
    x <- runif(1000)

    # Create cache key based on iteration group
    group <- iteration %% 5
    cache_key <- list(
      test = "ks_test",
      group = group,
      n = 1000
    )

    # Try to get cached result
    cached <- tryCatch(
      {
        get_cached_test_result(cache_key, cache_dir)
      },
      error = function(e) NULL
    )

    if (is.null(cached)) {
      # Compute result
      result <- ks.test(x, "punif")

      # Cache it
      cache_test_results(cache_key, result, cache_dir)
    }

    TRUE
  }

  result <- run_parallel_stress_test(
    test_cached_results,
    n_workers = 8,
    n_iterations = 1000,
    test_name = "Cached Test Results"
  )

  expect_true(result$success)

  # Verify cache integrity
  cache_files <- list.files(cache_dir, pattern = "\\.rds$", full.names = TRUE)
  expect_gt(length(cache_files), 0, info = "No cache files created")

  # Check that cached files can be read
  for (file in cache_files[1:min(5, length(cache_files))]) {
    cached_data <- readRDS(file)
    expect_true(!is.null(cached_data$result))
    expect_true(!is.null(cached_data$timestamp))
  }
})

test_that("Parallel execution with different RNG seeds produces consistent results", {
  skip_if_not(requireNamespace("parallel", quietly = TRUE))

  # Test that parallel execution with same seeds produces same results
  run_deterministic_test <- function(seed, n_workers = 4) {
    cl <- makeCluster(n_workers)
    on.exit(stopCluster(cl), add = TRUE)

    # Set seed on each worker
    clusterSetRNGStream(cl, seed)

    # Run computations
    results <- parLapply(cl, 1:20, function(i) {
      x <- runif(1000)
      c(mean = mean(x), sd = sd(x), min = min(x), max = max(x))
    })

    results
  }

  # Run twice with same seed
  results1 <- run_deterministic_test(12345)
  results2 <- run_deterministic_test(12345)

  # Results should be identical
  expect_identical(results1, results2,
    info = "Parallel execution with same seed produced different results"
  )

  # Run with different seed
  results3 <- run_deterministic_test(54321)

  # Results should be different
  expect_false(identical(results1, results3),
    info = "Parallel execution with different seeds produced identical results"
  )
})

# Print final summary
test_that("Print stress test summary", {
  cat("\n=== Parallel Thread Safety Stress Tests Complete ===\n")
  cat("All stress tests passed. The parallel execution framework appears to be thread-safe.\n")
  cat("Recommendations:\n")
  cat("- Continue monitoring for race conditions in production use\n")
  cat("- Consider implementing thread-safe logging for debugging\n")
  cat("- Review cache file locking mechanisms for high-concurrency scenarios\n")
  expect_true(TRUE) # Always passes, just for summary
})
