#!/usr/bin/env Rscript

library(qiprng)

cat("qiprng v0.5.0 Quick Validation\n")
cat("================================\n\n")

# 1. Basic generation
cat("1. Basic generation: ")
createPRNG()
vals <- generatePRNG(100)
cleanup_prng()
cat(sprintf("✓ Generated %d values, mean=%.3f\n", length(vals), mean(vals)))

# 2. Mixing strategies
cat("2. Mixing strategies:\n")
for (strategy in c("round_robin", "xor_mix", "averaging")) {
  cfg <- list(mixing_strategy = strategy, buffer_size = 100)
  createPRNG(cfg)
  vals <- generatePRNG(100)
  cleanup_prng()
  cat(sprintf("   - %s: ✓ mean=%.3f\n", strategy, mean(vals)))
}

# 3. Distributions
cat("3. Distributions:\n")
dists <- c("uniform_01", "normal", "exponential")
for (dist in dists) {
  cfg <- list(distribution = dist)
  createPRNG(cfg)
  vals <- generatePRNG(100)
  cleanup_prng()
  cat(sprintf("   - %s: ✓\n", dist))
}

# 4. Deterministic mode
cat("4. Deterministic mode: ")
cfg <- list(seed = 123)
createPRNG(cfg)
v1 <- generatePRNG(10)
cleanup_prng()
createPRNG(cfg)
v2 <- generatePRNG(10)
cleanup_prng()
if (identical(v1, v2)) {
  cat("✓ Reproducible\n")
} else {
  cat("✗ Not reproducible\n")
}

cat("\n✅ Quick validation complete!\n")
