#!/usr/bin/env Rscript

# Stress test for ziggurat normal generation with threading

library(qiprng)
library(parallel)

# Suppress warnings
suppressMPFRWarnings(TRUE)

# Number of threads to use (adjust as needed)
num_cores <- parallel::detectCores()
cat("Using", num_cores, "cores for stress test\n")

# Function to generate and clean up
generate_and_cleanup <- function(i) {
  cat("Thread", i, "creating PRNG\n")
  createPRNG(list(
    distribution = "normal",
    normal_method = "ziggurat",
    use_threading = TRUE,
    debug = FALSE
  ))
  
  cat("Thread", i, "generating values\n")
  values <- generatePRNG(10000)
  
  cat("Thread", i, "cleaning up\n")
  cleanupPRNG()
  
  cat("Thread", i, "done\n")
  return(mean(values))
}

# Run parallel tests
cat("Running parallel tests...\n")
results <- parallel::mclapply(1:num_cores, generate_and_cleanup, mc.cores = num_cores)

cat("All tests completed successfully!\n")
cat("Mean of means:", mean(unlist(results)), "\n")

# Now test a large sequential operation
cat("\nTesting large sequential generation in thread-safe mode...\n")
createPRNG(list(
  distribution = "normal",
  normal_method = "ziggurat",
  use_threading = TRUE,
  debug = FALSE,
  buffer_size = 1024 * 16,  # Large buffer to trigger parallel filling
  use_parallel_filling = TRUE  # Enable parallel buffer filling
))

cat("Generating 1 million values...\n")
start_time <- Sys.time()
values <- generatePRNG(1000000)
end_time <- Sys.time()

cat("Generation completed in", difftime(end_time, start_time, units="secs"), "seconds\n")
cat("Mean:", mean(values), "SD:", sd(values), "\n")

cat("Cleaning up...\n")
cleanupPRNG()

cat("All done!\n")