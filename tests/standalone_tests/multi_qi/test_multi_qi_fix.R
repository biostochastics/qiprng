#!/usr/bin/env Rscript

# Test Multi-QI Ensemble fix
library(qiprng)

cat("Testing Multi-QI Ensemble Fix\n")
cat("==============================\n\n")

# Test 1: Basic Multi-QI configuration
cat("Test 1: Creating Multi-QI ensemble with 3 QIs\n")
tryCatch({
    cfg <- list(
        a = c(2, 3, 5),
        b = c(7, 11, 13),
        c = c(-3, -5, -7),
        mixing_strategy = "xor_mix",
        buffer_size = 1000
    )
    
    # This should now work without the logical coercion error
    createPRNG(cfg)
    cat("  ✓ Multi-QI configuration created successfully\n")
    
    # Generate some values
    vals <- generatePRNG(100)
    cat(sprintf("  ✓ Generated %d values\n", length(vals)))
    cat(sprintf("  Mean: %.6f (expected ~0.5)\n", mean(vals)))
    cat(sprintf("  SD: %.6f (expected ~0.289)\n", sd(vals)))
    
    # Test distribution
    ks_test <- ks.test(vals, "punif")
    cat(sprintf("  KS test p-value: %.6f %s\n", 
                ks_test$p.value,
                ifelse(ks_test$p.value > 0.05, "✓", "⚠")))
    
    cleanup_prng()
    cat("  ✓ Cleanup successful\n\n")
    
}, error = function(e) {
    cat(sprintf("  ✗ FAILED: %s\n\n", e$message))
})

# Test 2: Different mixing strategies with Multi-QI
cat("Test 2: Multi-QI with different mixing strategies\n")
strategies <- c("round_robin", "xor_mix", "averaging", "modular_add", "cascade_mix")

for (strategy in strategies) {
    cat(sprintf("  Testing %s: ", strategy))
    tryCatch({
        cfg <- list(
            a = c(2, 3),
            b = c(5, 7),
            c = c(-1, -2),
            mixing_strategy = strategy,
            buffer_size = 500
        )
        
        createPRNG(cfg)
        vals <- generatePRNG(500)
        cleanup_prng()
        
        mean_val <- mean(vals)
        in_range <- (mean_val > 0.45 && mean_val < 0.55)
        cat(sprintf("mean=%.4f %s\n", mean_val, ifelse(in_range, "✓", "⚠")))
        
    }, error = function(e) {
        cat(sprintf("FAILED: %s\n", e$message))
    })
}

cat("\n==============================\n")
cat("Multi-QI Ensemble test complete\n")