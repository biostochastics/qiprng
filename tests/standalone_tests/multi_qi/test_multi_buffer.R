#!/usr/bin/env Rscript

# Test Multi-QI with buffer filling
library(qiprng)

cat("Multi-QI Buffer Test\n")

# Test creating Multi-QI config with various buffer sizes
for (buffer_size in c(10, 100, 1000)) {
  cat("\nTesting with buffer_size =", buffer_size, "\n")

  cfg <- list(
    a = c(2, 3, 5),
    b = c(5, 7, 11),
    c = c(-1, -2, -3),
    buffer_size = buffer_size,
    mpfr_precision = 53
  )

  cat("Creating PRNG...\n")
  createPRNG(cfg)

  # Generate values equal to buffer size
  cat("Generating", buffer_size, "values...\n")
  vals <- generatePRNG(buffer_size)
  cat("Generated", length(vals), "values, mean =", mean(vals), "\n")

  # Generate more values to test buffer refill
  cat("Generating", buffer_size * 2, "more values...\n")
  vals2 <- generatePRNG(buffer_size * 2)
  cat("Generated", length(vals2), "values, mean =", mean(vals2), "\n")

  cleanup_prng()
}

cat("\nAll tests completed successfully!\n")
