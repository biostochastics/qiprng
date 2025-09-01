#\!/usr/bin/env Rscript

library(qiprng)

cat("===== Basic Thread Safety Test =====\n")

# Create a thread-safe PRNG with Ziggurat normal
config <- list(
  use_threading = TRUE,
  distribution = "normal",
  normal_method = "ziggurat"
)

createPRNG(config)
values <- generatePRNG(1000)
cat("Generated values with mean:", mean(values), "\n")
cleanup_prng()

cat("Test completed successfully\n")