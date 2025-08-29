#!/usr/bin/env Rscript

library(qiprng)

cat("Testing Extended Distributions\n")
cat("===============================\n\n")

# Initialize PRNG
cfg <- list(
    a = 5,
    b = 7,
    c = -3,
    mpfr_precision = 128,
    buffer_size = 10000,
    distribution = "uniform_01"
)
createPRNG(cfg)

# Test 1: Levy Stable Distribution
cat("1. Testing Levy Stable Distribution...\n")
tryCatch({
    levy_samples <- generate_levy_stable(1000, alpha = 1.5, beta = 0.5, mu = 0, sigma = 1)
    cat(sprintf("   Generated %d samples, mean = %.4f, sd = %.4f\n", 
                length(levy_samples), mean(levy_samples), sd(levy_samples)))
    cat("   Sample range: [", min(levy_samples), ",", max(levy_samples), "]\n")
}, error = function(e) {
    cat("   ERROR:", e$message, "\n")
})

# Test 2: Pareto Distribution
cat("\n2. Testing Pareto Distribution...\n")
tryCatch({
    pareto_samples <- generate_pareto(1000, xm = 1, alpha = 2.5)
    cat(sprintf("   Generated %d samples, mean = %.4f, sd = %.4f\n", 
                length(pareto_samples), mean(pareto_samples), sd(pareto_samples)))
    cat("   Sample range: [", min(pareto_samples), ",", max(pareto_samples), "]\n")
    # Theoretical mean = xm * alpha / (alpha - 1) = 1 * 2.5 / 1.5 = 1.667
    cat("   Theoretical mean = 1.667\n")
}, error = function(e) {
    cat("   ERROR:", e$message, "\n")
})

# Test 3: Cauchy Distribution
cat("\n3. Testing Cauchy Distribution...\n")
tryCatch({
    cauchy_samples <- generate_cauchy(1000, location = 2, scale = 1)
    cat(sprintf("   Generated %d samples, median = %.4f\n", 
                length(cauchy_samples), median(cauchy_samples)))
    cat("   Sample range: [", min(cauchy_samples), ",", max(cauchy_samples), "]\n")
    # Cauchy has undefined mean and variance, but median = location
    cat("   Theoretical median = 2.000\n")
}, error = function(e) {
    cat("   ERROR:", e$message, "\n")
})

# Test 4: Multivariate Normal Distribution
cat("\n4. Testing Multivariate Normal Distribution...\n")
tryCatch({
    mean_vec <- c(1, 2, 3)
    cov_mat <- matrix(c(1, 0.5, 0.2,
                       0.5, 1, 0.3,
                       0.2, 0.3, 1), 3, 3)
    mvn_samples <- generate_multivariate_normal(1000, mean_vec, cov_mat)
    
    sample_means <- colMeans(mvn_samples)
    cat(sprintf("   Generated %d x %d matrix\n", nrow(mvn_samples), ncol(mvn_samples)))
    cat("   Sample means:", paste(round(sample_means, 4), collapse = ", "), "\n")
    cat("   Theoretical means:", paste(mean_vec, collapse = ", "), "\n")
    
    # Check correlation structure
    sample_cor <- cor(mvn_samples)
    cat("   Sample correlation:\n")
    print(round(sample_cor, 3))
}, error = function(e) {
    cat("   ERROR:", e$message, "\n")
})

# Test 5: Gaussian Copula
cat("\n5. Testing Gaussian Copula...\n")
tryCatch({
    # Define correlation structure
    correlation <- matrix(c(1, 0.7, 0.7, 1), 2, 2)
    
    # Define marginal distributions
    marginals <- list(
        list(type = "cauchy", location = 0, scale = 1),
        list(type = "pareto", xm = 1, alpha = 3)
    )
    
    copula_samples <- generate_with_copula(1000, correlation, marginals)
    cat(sprintf("   Generated %d x %d matrix\n", nrow(copula_samples), ncol(copula_samples)))
    
    # Check correlation (should be approximately 0.7)
    sample_cor <- cor(copula_samples, method = "spearman")
    cat("   Spearman correlation:\n")
    print(round(sample_cor, 3))
    cat("   Target correlation: 0.7\n")
}, error = function(e) {
    cat("   ERROR:", e$message, "\n")
})

# Test 6: Performance test
cat("\n6. Performance Test...\n")
tryCatch({
    start_time <- Sys.time()
    large_levy <- generate_levy_stable(10000, alpha = 1.8, beta = 0)
    levy_time <- as.numeric(Sys.time() - start_time)
    
    start_time <- Sys.time()
    large_pareto <- generate_pareto(10000, xm = 1, alpha = 2)
    pareto_time <- as.numeric(Sys.time() - start_time)
    
    cat(sprintf("   Levy Stable (10k samples): %.3f seconds\n", levy_time))
    cat(sprintf("   Pareto (10k samples): %.3f seconds\n", pareto_time))
}, error = function(e) {
    cat("   ERROR:", e$message, "\n")
})

# Cleanup
cleanupPRNG()
cat("\nAll tests completed!\n")