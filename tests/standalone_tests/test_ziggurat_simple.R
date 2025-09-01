#!/usr/bin/env Rscript

# Simple test script for ziggurat normal generation

library(qiprng)

cat("Creating PRNG with ziggurat method\n")
createPRNG(list(
  distribution = "normal",
  normal_method = "ziggurat",
  use_threading = FALSE,
  debug = FALSE
))

cat("Generating 10 values\n")
values <- generatePRNG(10)
print(values)

cat("Cleaning up\n")
cleanupPRNG()

cat("Creating PRNG in thread-safe mode\n")
createPRNG(list(
  distribution = "normal",
  normal_method = "ziggurat",
  use_threading = TRUE,
  debug = FALSE
))

cat("Generating 10 values in thread-safe mode\n")
values <- generatePRNG(10)
print(values)

cat("Cleaning up again\n")
cleanupPRNG()

cat("Done!\n")
