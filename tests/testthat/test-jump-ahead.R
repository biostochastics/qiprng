context("Jump-Ahead Functionality Tests")

library(qiprng)

# Helper function to measure execution time
time_execution <- function(expr) {
  start_time <- Sys.time()
  result <- eval(expr)
  end_time <- Sys.time()
  list(
    result = result, 
    time = as.numeric(end_time - start_time, units = "secs")
  )
}

test_that("Jump-ahead produces correct results compared to sequential iteration", {
  skip_on_cran()
  
  # Create PRNG with fixed seed for reproducibility
  cfg <- list(
    a = 2,
    b = 5,
    c = -2,
    mpfr_precision = 53,
    distribution = "uniform_01"
  )
  
  # Test small jump sizes
  jump_sizes <- c(10, 50, 100)
  
  for (jump_size in jump_sizes) {
    # Method 1: Generate numbers sequentially, then pick the jump_size-th number
    createPRNG(cfg)
    # Generate jump_size+1 numbers (including the first one)
    seq_numbers <- generatePRNG(jump_size + 1)
    last_seq_number <- seq_numbers[jump_size + 1]
    cleanup_prng()
    
    # Method 2: Jump ahead jump_size steps, then get the next number
    createPRNG(cfg)
    jumpAheadPRNG(jump_size)
    jump_number <- generatePRNG(1)
    cleanup_prng()
    
    # The results should be identical
    expect_equal(jump_number, last_seq_number, 
                 info = paste("Jump ahead by", jump_size, "should match sequential generation"))
  }
})

test_that("Jump-ahead performs better than sequential generation for large jumps", {
  skip_on_cran()
  
  # Only run benchmark tests in interactive mode or explicitly enabled
  skip_if(!interactive() && Sys.getenv("RUN_PERFORMANCE_TESTS") != "true", 
          "Skipping performance tests in non-interactive mode")
  
  # Test with various jump sizes
  jump_sizes <- c(1000, 10000, 100000, 1000000)
  
  results <- data.frame(
    jump_size = integer(),
    method = character(),
    time = numeric()
  )
  
  cfg <- list(
    a = 2,
    b = 5,
    c = -2,
    mpfr_precision = 53,
    distribution = "uniform_01"
  )
  
  for (jump_size in jump_sizes) {
    # Method 1: Time sequential generation
    createPRNG(cfg)
    time_seq <- time_execution(quote({
      generatePRNG(jump_size)
      generatePRNG(1)
    }))$time
    cleanup_prng()
    
    # Method 2: Time jump ahead
    createPRNG(cfg)
    time_jump <- time_execution(quote({
      jumpAheadPRNG(jump_size)
      generatePRNG(1)
    }))$time
    cleanup_prng()
    
    # Store results
    results <- rbind(results, 
                     data.frame(jump_size = jump_size, 
                                method = "sequential", 
                                time = time_seq))
    
    results <- rbind(results, 
                     data.frame(jump_size = jump_size, 
                                method = "jump_ahead", 
                                time = time_jump))
    
    # For larger jump sizes, jump-ahead should be significantly faster
    if (jump_size >= 10000) {
      expect_lt(time_jump, time_seq * 0.5, 
                info = paste("Jump-ahead should be at least twice as fast for size", jump_size))
    }
  }
  
  # Print performance results
  if (interactive()) {
    print(results)
  }
})

test_that("Jump-ahead works with different distributions", {
  skip_on_cran()
  
  # Test different distributions
  distributions <- list(
    "uniform_01" = list(distribution = "uniform_01"),
    "uniform_range" = list(distribution = "uniform_range", range_min = -5, range_max = 5),
    "normal" = list(distribution = "normal", normal_mean = 3, normal_sd = 2),
    "exponential" = list(distribution = "exponential", exponential_lambda = 0.5)
  )
  
  for (dist_name in names(distributions)) {
    # Create configuration with this distribution
    cfg <- c(
      list(a = 2, b = 5, c = -2, mpfr_precision = 53),
      distributions[[dist_name]]
    )
    
    # Test jump-ahead with this distribution
    jump_size <- 500
    
    # Method 1: Generate numbers sequentially
    createPRNG(cfg)
    seq_numbers <- generatePRNG(jump_size + 1)
    last_seq_number <- seq_numbers[jump_size + 1]
    cleanup_prng()
    
    # Method 2: Jump ahead
    createPRNG(cfg)
    jumpAheadPRNG(jump_size)
    jump_number <- generatePRNG(1)
    cleanup_prng()
    
    # The results should be identical
    expect_equal(jump_number, last_seq_number, 
                 info = paste("Jump ahead with distribution", dist_name, "should match sequential"))
  }
})

test_that("Jump-ahead works at reseed boundaries", {
  skip_on_cran()
  
  # Create PRNG with small reseed interval
  reseed_interval <- 1000
  cfg <- list(
    a = 2,
    b = 5,
    c = -2,
    mpfr_precision = 53,
    distribution = "uniform_01",
    reseed_interval = reseed_interval
  )
  
  # Test jumping exactly to the reseed boundary
  createPRNG(cfg)
  # Generate until just before reseed
  generatePRNG(reseed_interval - 1)
  # Next call would trigger reseed
  standard_next <- generatePRNG(1)
  cleanup_prng()
  
  # Jump directly to the boundary
  createPRNG(cfg)
  jumpAheadPRNG(reseed_interval - 1)
  jump_next <- generatePRNG(1)
  cleanup_prng()
  
  expect_equal(standard_next, jump_next, 
               "Jump to reseed boundary should match sequential generation")
  
  # Test jumping across the reseed boundary
  jump_size <- reseed_interval + 100
  
  createPRNG(cfg)
  seq_numbers <- generatePRNG(jump_size + 1)
  last_seq_number <- seq_numbers[jump_size + 1]
  cleanup_prng()
  
  createPRNG(cfg)
  jumpAheadPRNG(jump_size)
  jump_number <- generatePRNG(1)
  cleanup_prng()
  
  expect_equal(jump_number, last_seq_number, 
               "Jump across reseed boundary should match sequential generation")
})

test_that("Jump-ahead works with very large jumps", {
  skip_on_cran()
  
  skip_if(!interactive() && Sys.getenv("RUN_LARGE_JUMPS") != "true", 
          "Skipping very large jump tests in non-interactive mode")
  
  # Test a very large jump
  large_jump <- 1e7  # 10 million steps
  
  cfg <- list(
    a = 2,
    b = 5,
    c = -2,
    mpfr_precision = 53,
    distribution = "uniform_01"
  )
  
  # Test jump-ahead with this large value
  createPRNG(cfg)
  # This shouldn't take too long if the jump-ahead is efficient
  timing <- time_execution(quote(jumpAheadPRNG(large_jump)))
  jump_result <- generatePRNG(10)
  cleanup_prng()
  
  # Simply check that the numbers are valid uniform [0,1)
  expect_equal(length(jump_result), 10)
  expect_true(all(jump_result >= 0 & jump_result < 1))
  
  # Also verify the time is reasonable (should be much less than doing it sequentially)
  # A very efficient implementation should do this in under a second
  expect_lt(timing$time, 5, 
            "Very large jump should complete in a reasonable time")
})

test_that("Jump-ahead maintains statistical properties", {
  skip_on_cran()
  
  cfg <- list(
    a = 2,
    b = 5,
    c = -2,
    mpfr_precision = 53,
    distribution = "uniform_01"
  )
  
  # Generate a sequence normally
  createPRNG(cfg)
  normal_sequence <- generatePRNG(10000)
  cleanup_prng()
  
  # Generate a sequence with jumps
  createPRNG(cfg)
  jumped_sequence <- numeric(10000)
  
  # Use different jump sizes
  jump_sizes <- c(1, 10, 100, 1000)
  jump_idx <- 1
  
  for (i in 1:10000) {
    jumped_sequence[i] <- generatePRNG(1)
    # Apply jumps at regular intervals
    if (i %% 1000 == 0 && i < 9000) {
      jump_size <- jump_sizes[jump_idx]
      jump_idx <- (jump_idx %% length(jump_sizes)) + 1
      jumpAheadPRNG(jump_size)
    }
  }
  cleanup_prng()
  
  # Statistical tests
  
  # 1. Check distribution
  ks_normal <- ks.test(normal_sequence, "punif")
  ks_jumped <- ks.test(jumped_sequence, "punif")
  
  expect_gt(ks_normal$p.value, 0.01, "Normal sequence should be uniform")
  expect_gt(ks_jumped$p.value, 0.01, "Jumped sequence should be uniform")
  
  # 2. Check first four moments
  if (requireNamespace("moments", quietly = TRUE)) {
    # Mean
    expect_lt(abs(mean(jumped_sequence) - 0.5), 0.02, "Jumped sequence should have correct mean")
    
    # Variance
    expect_lt(abs(var(jumped_sequence) - 1/12), 0.01, "Jumped sequence should have correct variance")
    
    # Skewness (uniform should be ~0)
    expect_lt(abs(moments::skewness(jumped_sequence)), 0.1, 
              "Jumped sequence should have correct skewness")
    
    # Kurtosis (uniform should be ~1.8)
    expect_lt(abs(moments::kurtosis(jumped_sequence) - 1.8), 0.3, 
              "Jumped sequence should have correct kurtosis")
  }
  
  # 3. Autocorrelation
  if (requireNamespace("stats", quietly = TRUE)) {
    acf_result <- stats::acf(jumped_sequence, lag.max = 10, plot = FALSE)
    # Ignore lag 0 (always 1) and check that others are small
    expect_true(all(abs(acf_result$acf[-1]) < 0.1), 
                "Jumped sequence should not have significant autocorrelation")
  }
})
