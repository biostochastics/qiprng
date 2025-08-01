# Test suite for deterministic mode functionality
library(qiprng)

test_that("Same seed produces identical sequences", {
    # Test 1: Basic reproducibility
    cfg1 <- list(seed = 12345)
    createPRNG(cfg1)
    seq1 <- generatePRNG(1000)
    cleanupPRNG()
    
    createPRNG(cfg1)
    seq2 <- generatePRNG(1000)
    cleanupPRNG()
    
    expect_identical(seq1, seq2)
})

test_that("Different seeds produce different sequences", {
    cfg1 <- list(seed = 12345)
    cfg2 <- list(seed = 54321)
    
    createPRNG(cfg1)
    seq1 <- generatePRNG(1000)
    cleanupPRNG()
    
    createPRNG(cfg2)
    seq2 <- generatePRNG(1000)
    cleanupPRNG()
    
    expect_false(all(seq1 == seq2))
})

test_that("Deterministic mode works with jump-ahead", {
    cfg <- list(seed = 12345)
    
    # Generate reference sequence
    createPRNG(cfg)
    ref_seq <- generatePRNG(2000)
    cleanupPRNG()
    
    # Generate with jump
    createPRNG(cfg)
    jumpAheadPRNG(1000)
    jump_seq <- generatePRNG(1000)
    cleanupPRNG()
    
    # Should match the second half of reference
    expect_equal(jump_seq, ref_seq[1001:2000])
})

test_that("Same seed works across different R sessions", {
    # This test verifies that the seed produces the same sequence
    # even after restarting R (simulated by cleanup and recreate)
    cfg <- list(seed = 98765)
    
    # "Session 1"
    createPRNG(cfg)
    seq1_part1 <- generatePRNG(500)
    seq1_part2 <- generatePRNG(500)
    cleanupPRNG()
    
    # "Session 2" - complete restart
    createPRNG(cfg)
    seq2_part1 <- generatePRNG(500)
    seq2_part2 <- generatePRNG(500)
    cleanupPRNG()
    
    expect_identical(seq1_part1, seq2_part1)
    expect_identical(seq1_part2, seq2_part2)
})

test_that("Deterministic mode works with all distributions", {
    base_cfg <- list(seed = 11111)
    
    distributions <- list(
        list(distribution = "uniform_01"),
        list(distribution = "uniform_range", range_min = -5, range_max = 5),
        list(distribution = "normal", normal_mean = 10, normal_sd = 2, normal_method = "box_muller"),
        list(distribution = "exponential", exponential_lambda = 0.5),
        list(distribution = "poisson", poisson_lambda = 3),
        list(distribution = "gamma", gamma_shape = 2, gamma_scale = 0.5),
        list(distribution = "beta", beta_alpha = 2, beta_beta = 3)
    )
    
    for (dist_cfg in distributions) {
        cfg <- modifyList(base_cfg, dist_cfg)
        
        # Generate first sequence
        createPRNG(cfg)
        seq1 <- generatePRNG(100)
        cleanupPRNG()
        
        # Generate second sequence with same config
        createPRNG(cfg)
        seq2 <- generatePRNG(100)
        cleanupPRNG()
        
        expect_identical(seq1, seq2, 
            info = paste("Failed for distribution:", dist_cfg$distribution))
    }
})

test_that("Thread-safe deterministic mode", {
    skip_if_not(capabilities("long.double"), "Platform doesn't support long double")
    
    cfg <- list(seed = 22222, use_threading = TRUE)
    
    # Generate reference sequence without threading
    cfg_no_thread <- cfg
    cfg_no_thread$use_threading <- FALSE
    createPRNG(cfg_no_thread)
    ref_seq <- generatePRNG(1000)
    cleanupPRNG()
    
    # Generate with threading enabled
    createPRNG(cfg)
    thread_seq <- generatePRNG(1000)
    cleanupPRNG()
    
    # Results should be identical even with threading
    expect_identical(ref_seq, thread_seq)
})

test_that("Invalid seed values are rejected", {
    expect_error(createPRNG(list(seed = -1)), 
                 "Seed must be between 0 and 2\\^53-1")
    expect_error(createPRNG(list(seed = 2^53)), 
                 "Seed must be between 0 and 2\\^53-1")
    expect_error(createPRNG(list(seed = "not_a_number")), 
                 "Seed must be between 0 and 2\\^53-1")
})

test_that("Zero seed is valid", {
    cfg <- list(seed = 0)
    
    createPRNG(cfg)
    seq1 <- generatePRNG(100)
    cleanupPRNG()
    
    createPRNG(cfg)
    seq2 <- generatePRNG(100)
    cleanupPRNG()
    
    expect_identical(seq1, seq2)
    expect_true(all(seq1 >= 0 & seq1 <= 1))
})

test_that("Large seeds work correctly", {
    # Test with maximum valid seed
    cfg <- list(seed = 2^53 - 1)
    
    createPRNG(cfg)
    seq1 <- generatePRNG(100)
    cleanupPRNG()
    
    createPRNG(cfg)
    seq2 <- generatePRNG(100)
    cleanupPRNG()
    
    expect_identical(seq1, seq2)
    expect_true(all(seq1 >= 0 & seq1 <= 1))
})

test_that("Deterministic mode with custom parameters", {
    cfg <- list(
        seed = 33333,
        a = 3L,
        b = 7L,
        c = -5L,
        mpfr_precision = 256L,
        buffer_size = 2048L
    )
    
    createPRNG(cfg)
    seq1 <- generatePRNG(500)
    cleanupPRNG()
    
    createPRNG(cfg)
    seq2 <- generatePRNG(500)
    cleanupPRNG()
    
    expect_identical(seq1, seq2)
})

test_that("Deterministic mode with reseeding", {
    cfg <- list(seed = 44444, reseed_interval = 100L)
    
    createPRNG(cfg)
    seq1 <- generatePRNG(300)  # Will trigger reseeds at 100 and 200
    cleanupPRNG()
    
    createPRNG(cfg)
    seq2 <- generatePRNG(300)
    cleanupPRNG()
    
    # Even with reseeding, deterministic mode should produce same sequence
    expect_identical(seq1, seq2)
})

test_that("NULL seed uses random initialization", {
    # Without seed, sequences should be different
    cfg <- list(seed = NULL)  # Explicitly NULL
    
    createPRNG(cfg)
    seq1 <- generatePRNG(100)
    cleanupPRNG()
    
    createPRNG(cfg)
    seq2 <- generatePRNG(100)
    cleanupPRNG()
    
    # With very high probability, these should be different
    expect_false(all(seq1 == seq2))
})

test_that("Default config (no seed) uses random initialization", {
    # Using default config without seed specification
    createPRNG()
    seq1 <- generatePRNG(100)
    cleanupPRNG()
    
    createPRNG()
    seq2 <- generatePRNG(100)
    cleanupPRNG()
    
    # With very high probability, these should be different
    expect_false(all(seq1 == seq2))
})