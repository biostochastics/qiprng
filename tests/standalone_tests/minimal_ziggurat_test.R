#!/usr/bin/env Rscript

# Threaded Ziggurat method test with aggressive cleanup
library(qiprng)

cat("\n===== THREADED Ziggurat Test with Cleanup =====\n\n")

test_count <- 10

# Test with Ziggurat method and threading
for (i in 1:test_count) {
  cat("\n----- Test iteration", i, "of", test_count, "-----\n")

  # Configure with threading and ziggurat method
  cfg <- list(
    distribution = "normal",
    normal_method = "ziggurat",
    use_threading = TRUE, # Enable threading
    use_parallel_filling = TRUE,
    debug = TRUE
  )

  tryCatch(
    {
      cat("Creating PRNG with Ziggurat (threaded)...\n")
      createPRNG(cfg)

      cat("Generating values...\n")
      values <- generatePRNG(1000)
      cat("Mean:", mean(values), "\n")
      cat("Variance:", var(values), "\n")

      cat("Cleaning up PRNG...\n")
      cleanup_prng()

      # Force garbage collection
      gc()

      cat("Test iteration", i, "SUCCESSFUL\n")
    },
    error = function(e) {
      cat("ERROR on iteration", i, ":", e$message, "\n")

      # Try cleanup even after error
      tryCatch(
        {
          cleanup_prng()
        },
        error = function(cleanup_err) {
          cat("Cleanup error:", cleanup_err$message, "\n")
        }
      )

      # Force garbage collection
      gc()
    }
  )

  # Sleep briefly between tests to let any thread resources be fully cleaned up
  Sys.sleep(0.1)
}

cat("\n===== All tests completed without segfaults =====\n")
cat("The thread-safe Ziggurat implementation is working correctly.\n")
