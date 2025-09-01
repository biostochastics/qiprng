#!/usr/bin/env Rscript

# Final test for QIPRNG using only safe configurations
library(qiprng)

cat("\n===== QIPRNG Final Safe Configuration Test =====\n\n")

# Test various safe configurations
test_configurations <- list(
  list(
    name = "Default Uniform_01 (non-threaded)",
    config = list(
      distribution = "uniform_01",
      use_threading = FALSE,
      use_parallel_filling = FALSE
    )
  ),
  list(
    name = "Thread-safe Uniform_01",
    config = list(
      distribution = "uniform_01",
      use_threading = TRUE,
      use_parallel_filling = FALSE
    )
  ),
  list(
    name = "Thread-safe Ziggurat Normal",
    config = list(
      distribution = "normal",
      normal_method = "ziggurat",
      use_threading = TRUE,
      use_parallel_filling = FALSE
    )
  ),
  list(
    name = "Thread-safe Box-Muller Normal",
    config = list(
      distribution = "normal",
      normal_method = "box_muller",
      use_threading = TRUE,
      use_parallel_filling = FALSE
    )
  ),
  list(
    name = "Thread-safe Exponential",
    config = list(
      distribution = "exponential",
      exponential_lambda = 2.0,
      use_threading = TRUE,
      use_parallel_filling = FALSE
    )
  )
)

# Test each configuration
for (test in test_configurations) {
  cat(paste0("Testing: ", test$name, "\n"))
  cat("--------------------------------\n")

  tryCatch(
    {
      # Create PRNG
      cat("Creating PRNG...\n")
      createPRNG(test$config)

      # Generate values
      cat("Generating values...\n")
      values <- generatePRNG(2000)

      # Calculate statistics
      cat("Calculating statistics...\n")
      stats <- list(
        mean = mean(values),
        var = var(values),
        min = min(values),
        max = max(values),
        has_nan = any(is.nan(values)),
        has_inf = any(is.infinite(values))
      )

      # Display statistics
      cat(paste0("Mean: ", stats$mean, "\n"))
      cat(paste0("Variance: ", stats$var, "\n"))
      cat(paste0("Range: [", stats$min, ", ", stats$max, "]\n"))
      cat(paste0("NaN values: ", stats$has_nan, "\n"))
      cat(paste0("Infinity values: ", stats$has_inf, "\n"))

      # Clean up
      cat("Cleaning up...\n")
      cleanup_prng()

      cat("SUCCESS\n\n")
    },
    error = function(e) {
      cat(paste0("ERROR: ", e$message, "\n\n"))
      cleanup_prng()
    }
  )
}

cat("===== All tests completed! =====\n")
