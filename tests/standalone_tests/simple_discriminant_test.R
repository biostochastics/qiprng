#!/usr/bin/env Rscript

# Simple test for discriminant uniqueness
library(qiprng)

cat("\n===== Testing discriminant uniqueness =====\n\n")

# Create a set of PRNGs and check if they get unique discriminants
n_prngs <- 10
cat(paste0("Creating ", n_prngs, " PRNGs with threading enabled...\n"))

discriminants <- numeric(n_prngs)

# Create configuration with threading
cfg <- list(
  use_threading = TRUE,
  debug = TRUE
)

# Create PRNGs and collect discriminants
for (i in 1:n_prngs) {
  createPRNG(cfg)
  config <- .getPRNGConfig_()
  discriminant <- config$b * config$b - 4 * config$a * config$c
  discriminants[i] <- discriminant
  cat(paste0("PRNG ", i, " discriminant: ", discriminant, "\n"))
  cleanup_prng()
}

# Check for duplicates
if (length(unique(discriminants)) == n_prngs) {
  cat("\nSUCCESS: All discriminants are unique!\n")
} else {
  cat("\nFAILURE: Found duplicate discriminants!\n")
  print(table(discriminants))
}

cat("\n===== Test completed! =====\n")
