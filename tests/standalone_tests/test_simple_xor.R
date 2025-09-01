#!/usr/bin/env Rscript

library(qiprng)

cat("Testing simple generation first...\n")

# First test with round_robin
cfg <- list(
  a = 2, b = 5, c = -2,
  mpfr_precision = 53,
  buffer_size = 10,
  mixing_strategy = "round_robin",
  distribution = "uniform_01"
)

createPRNG(cfg)
vals <- generatePRNG(5)
cat("Round-robin generated", length(vals), "values, mean:", mean(vals), "\n")
cleanup_prng()

# Now test with averaging (simpler than XOR)
cat("\nTesting averaging mixing...\n")
cfg$mixing_strategy <- "averaging"
createPRNG(cfg)

setTimeLimit(cpu = 2, elapsed = 2)
tryCatch(
  {
    vals <- generatePRNG(5)
    cat("Averaging generated", length(vals), "values, mean:", mean(vals), "\n")
  },
  error = function(e) {
    cat("Averaging failed:", e$message, "\n")
  }
)
setTimeLimit(cpu = Inf, elapsed = Inf)
cleanup_prng()

# Finally test with XOR
cat("\nTesting XOR mixing...\n")
cfg$mixing_strategy <- "xor_mix"
createPRNG(cfg)

setTimeLimit(cpu = 2, elapsed = 2)
tryCatch(
  {
    vals <- generatePRNG(5)
    cat("XOR generated", length(vals), "values, mean:", mean(vals), "\n")
  },
  error = function(e) {
    cat("XOR failed:", e$message, "\n")
  }
)
setTimeLimit(cpu = Inf, elapsed = Inf)
cleanup_prng()

cat("\nTest complete.\n")
