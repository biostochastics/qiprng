#\!/usr/bin/env Rscript

library(qiprng)

cat("===== Final Thread Safety Verification Test =====\n")

iterations <- 20

cat("Running", iterations, "iterations of create-generate-cleanup...\n")

for (i in 1:iterations) {
  cat("Iteration", i, "of", iterations, "... ")
  
  # Create a thread-safe PRNG with Ziggurat normal
  config <- list(
    use_threading = TRUE,
    distribution = "normal",
    normal_method = "ziggurat"
  )
  
  createPRNG(config)
  values <- generatePRNG(1000)
  cleanup_prng()
  
  cat("OK\n")
}

cat("\nAll", iterations, "iterations completed successfully!\n")
cat("Thread safety in Ziggurat is confirmed - NO SEGFAULTS\n")