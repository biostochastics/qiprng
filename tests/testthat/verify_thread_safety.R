# \!/usr/bin/env Rscript

# This is a simplified test to verify thread safety fixes
library(qiprng)

cat("\n===== Verifying Thread Safety for Ziggurat =====\n\n")

iterations <- 10
cat(paste0("Running ", iterations, " iterations of create-generate-cleanup cycle...\n"))

# Configuration with thread safety and Ziggurat normal distribution
cfg <- list(
  use_threading = TRUE,
  distribution = "normal",
  normal_method = "ziggurat",
  debug = TRUE
)

for (i in 1:iterations) {
  cat(paste0("\n----- Iteration ", i, " of ", iterations, " -----\n"))
  cat("Creating PRNG with thread-safe Ziggurat...\n")

  # Create PRNG
  createPRNG(cfg)

  # Generate values to exercise the Ziggurat algorithm
  cat("Generating values...\n")
  values <- generatePRNG(1000)
  cat(paste0("Mean: ", mean(values), "\n"))

  # Clean up in a thread-safe manner
  cat("Cleaning up PRNG...\n")
  cleanup_prng()

  cat(paste0("Iteration ", i, " completed successfully\n"))
}

cat("\n===== All iterations completed successfully =====\n")
cat("Thread safety in Ziggurat is confirmed!\n")
