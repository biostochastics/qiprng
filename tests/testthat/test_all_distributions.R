#!/usr/bin/env Rscript

# Comprehensive test script for all qiprng distributions
library(qiprng)

cat("\n===== Testing all qiprng distributions =====\n\n")

# Function to run basic statistics test on a distribution
test_distribution <- function(dist_name, cfg, expected_mean, expected_var, min_val = NULL, max_val = NULL, integer_only = FALSE, n_sample = 5000) {
  cat(paste0("Testing '", dist_name, "' distribution...\n"))
  createPRNG(cfg)
  
  # Generate samples
  samples <- generatePRNG(n_sample)
  
  # Basic validation
  cat(paste0("  Mean: ", mean(samples), " (expected: ", expected_mean, ")\n"))
  cat(paste0("  Variance: ", var(samples), " (expected: ", expected_var, ")\n"))
  
  # Check value constraints if provided
  if (!is.null(min_val)) {
    min_check <- all(samples >= min_val)
    cat(paste0("  All values >= ", min_val, ": ", min_check, "\n"))
    if (!min_check) cat("  WARNING: Some values below minimum!\n")
  }
  
  if (!is.null(max_val)) {
    max_check <- all(samples <= max_val)
    cat(paste0("  All values <= ", max_val, ": ", max_check, "\n"))
    if (!max_check) cat("  WARNING: Some values above maximum!\n")
  }
  
  # Check integer constraint if required
  if (integer_only) {
    int_check <- all(round(samples) == samples)
    cat(paste0("  All values are integers: ", int_check, "\n"))
    if (!int_check) cat("  WARNING: Some values are not integers!\n")
  }
  
  # Return samples for further analysis if needed
  invisible(samples)
}

# 1. Test Uniform(0,1) distribution
cat("\n----- Uniform(0,1) -----\n")
cfg_uniform <- list(
  a = 2,
  b = 5, 
  c = -2,
  distribution = "uniform_01"
)
test_distribution("Uniform(0,1)", cfg_uniform, 0.5, 1/12, 0, 1)

# 2. Test Uniform Range distribution
cat("\n----- Uniform(10,20) -----\n")
cfg_uniform_range <- list(
  distribution = "uniform_range",
  range_min = 10,
  range_max = 20
)
test_distribution("Uniform(10,20)", cfg_uniform_range, 15, 100/12, 10, 20)

# 3. Test Normal distribution
cat("\n----- Normal(0,1) -----\n")
cfg_normal <- list(
  distribution = "normal",
  normal_mean = 0,
  normal_sd = 1
)
normal_samples <- test_distribution("Normal(0,1)", cfg_normal, 0, 1)

# 4. Test Exponential distribution
cat("\n----- Exponential(2) -----\n")
cfg_exp <- list(
  distribution = "exponential",
  exponential_lambda = 2
)
test_distribution("Exponential(2)", cfg_exp, 1/2, 1/4, 0)

# 5. Test Poisson distribution
cat("\n----- Poisson(5) -----\n")
cfg_poisson <- list(
  distribution = "poisson",
  poisson_lambda = 5
)
poisson_samples <- test_distribution("Poisson(5)", cfg_poisson, 5, 5, 0, NULL, TRUE)

# 6. Test Gamma distribution with shape > 1
cat("\n----- Gamma(shape=2, scale=0.5) -----\n")
cfg_gamma1 <- list(
  distribution = "gamma",
  gamma_shape = 2,
  gamma_scale = 0.5
)
test_distribution("Gamma(2,0.5)", cfg_gamma1, 2*0.5, 2*0.5^2, 0)

# 7. Test Gamma distribution with shape < 1
cat("\n----- Gamma(shape=0.5, scale=1) -----\n")
cfg_gamma2 <- list(
  distribution = "gamma",
  gamma_shape = 0.5,
  gamma_scale = 1
)
test_distribution("Gamma(0.5,1)", cfg_gamma2, 0.5, 0.5, 0)

# 8. Test Beta distribution
cat("\n----- Beta(2,5) -----\n")
cfg_beta1 <- list(
  distribution = "beta",
  beta_alpha = 2,
  beta_beta = 5
)
# Mean = alpha/(alpha+beta) = 2/7
# Var = (alpha*beta)/((alpha+beta)^2 * (alpha+beta+1)) = (2*5)/((2+5)^2 * (2+5+1)) = 10/392
test_distribution("Beta(2,5)", cfg_beta1, 2/7, 10/392, 0, 1)

# 9. Test special case: Beta(1,1) = Uniform(0,1)
cat("\n----- Beta(1,1) [Uniform] -----\n")
cfg_beta2 <- list(
  distribution = "beta",
  beta_alpha = 1,
  beta_beta = 1
)
test_distribution("Beta(1,1)", cfg_beta2, 0.5, 1/12, 0, 1)

cat("\n===== All tests completed! =====\n")

# Clean up
cleanupPRNG()
