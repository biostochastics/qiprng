#!/usr/bin/env Rscript

# Test thread safety fixes in ZigguratNormal TLS cleanup
library(parallel)
library(qiprng)

cat("=== Thread Safety Test for TLS Cleanup ===\n")

# Test 1: Basic parallel generation
cat("\nTest 1: Parallel generation with multiple threads...\n")
test_parallel_generation <- function() {
  tryCatch(
    {
      # Create configuration for QI PRNG
      config <- list(
        a = 2,
        b = 5,
        c = -2,
        mpfr_precision = 53,
        distribution = "normal"
      )

      # Function to run in each thread
      worker_fn <- function(id) {
        set.seed(id)
        prng <- createPRNG(config)
        samples <- generatePRNG(1000)
        cleanup_prng()

        # Return basic statistics
        list(
          thread_id = id,
          mean = mean(samples),
          sd = sd(samples),
          min = min(samples),
          max = max(samples)
        )
      }

      # Run parallel workers
      cl <- makeCluster(4)
      clusterEvalQ(cl, library(qiprng))

      results <- parLapply(cl, 1:10, worker_fn)

      stopCluster(cl)

      # Check results
      all_ok <- TRUE
      for (res in results) {
        # Normal distribution should have mean ~0, sd ~1
        if (abs(res$mean) > 0.2 || abs(res$sd - 1) > 0.2) {
          cat(sprintf(
            "  Thread %d: mean=%.3f, sd=%.3f - FAILED\n",
            res$thread_id, res$mean, res$sd
          ))
          all_ok <- FALSE
        }
      }

      if (all_ok) {
        cat("  ✓ Parallel generation test passed\n")
        return(TRUE)
      } else {
        cat("  ✗ Parallel generation test failed\n")
        return(FALSE)
      }
    },
    error = function(e) {
      cat(sprintf("  ✗ Error: %s\n", e$message))
      return(FALSE)
    }
  )
}

# Test 2: Rapid creation and destruction
cat("\nTest 2: Rapid PRNG creation and destruction...\n")
test_rapid_cleanup <- function() {
  tryCatch(
    {
      config <- list(
        a = 2,
        b = 5,
        c = -2,
        mpfr_precision = 53,
        distribution = "normal"
      )

      # Rapidly create and destroy PRNGs
      for (i in 1:50) {
        prng <- createPRNG(config)
        x <- generatePRNG(10)
        cleanup_prng()

        if (i %% 10 == 0) {
          cat(sprintf("  Created and destroyed %d PRNGs...\n", i))
        }
      }

      cat("  ✓ Rapid cleanup test passed\n")
      return(TRUE)
    },
    error = function(e) {
      cat(sprintf("  ✗ Error: %s\n", e$message))
      return(FALSE)
    }
  )
}

# Test 3: Thread exit handling
cat("\nTest 3: Thread exit handling...\n")
test_thread_exit <- function() {
  tryCatch(
    {
      config <- list(
        a = 2,
        b = 5,
        c = -2,
        mpfr_precision = 53,
        distribution = "normal"
      )

      # Create threads that exit quickly
      quick_worker <- function(id) {
        prng <- createPRNG(config)
        x <- generatePRNG(1)
        # Don't explicitly cleanup - simulate abrupt exit
        return(x)
      }

      cl <- makeCluster(8)
      clusterEvalQ(cl, library(qiprng))

      # Run many quick tasks
      results <- parLapply(cl, 1:20, quick_worker)

      stopCluster(cl)

      # If we got here without crashing, the test passed
      cat("  ✓ Thread exit handling test passed\n")
      return(TRUE)
    },
    error = function(e) {
      cat(sprintf("  ✗ Error: %s\n", e$message))
      return(FALSE)
    }
  )
}

# Test 4: Concurrent access stress test
cat("\nTest 4: Concurrent access stress test...\n")
test_concurrent_stress <- function() {
  tryCatch(
    {
      config <- list(
        a = 2,
        b = 5,
        c = -2,
        mpfr_precision = 53,
        distribution = "normal"
      )

      stress_worker <- function(id) {
        set.seed(id * 1000)
        results <- numeric(100)

        for (i in 1:100) {
          prng <- createPRNG(config)
          x <- generatePRNG(100)
          results[i] <- mean(x)
          cleanup_prng()

          # Add some randomness to timing
          if (runif(1) < 0.1) {
            Sys.sleep(0.001)
          }
        }

        return(mean(results))
      }

      cl <- makeCluster(6)
      clusterEvalQ(cl, library(qiprng))

      results <- parLapply(cl, 1:12, stress_worker)

      stopCluster(cl)

      # Check that all workers completed successfully
      if (length(results) == 12 && all(!is.na(unlist(results)))) {
        cat("  ✓ Concurrent stress test passed\n")
        return(TRUE)
      } else {
        cat("  ✗ Concurrent stress test failed\n")
        return(FALSE)
      }
    },
    error = function(e) {
      cat(sprintf("  ✗ Error: %s\n", e$message))
      return(FALSE)
    }
  )
}

# Run all tests
all_passed <- TRUE
all_passed <- all_passed & test_parallel_generation()
all_passed <- all_passed & test_rapid_cleanup()
all_passed <- all_passed & test_thread_exit()
all_passed <- all_passed & test_concurrent_stress()

if (all_passed) {
  cat("\n=== All thread safety tests PASSED ===\n")
  quit(status = 0)
} else {
  cat("\n=== Some tests FAILED ===\n")
  quit(status = 1)
}
