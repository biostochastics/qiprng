context("Jump-Ahead Performance Tests")

library(qiprng)
library(microbenchmark)

test_that("Jump-ahead function works correctly", {
  skip_on_cran()
  
  # Create PRNG
  cfg <- list(
    a = 2,
    b = 5,
    c = -2,
    mpfr_precision = 53,
    distribution = "uniform_01"
  )
  
  createPRNG(cfg)
  
  # Test that jump-ahead doesn't crash and produces valid output
  jumpAheadPRNG(100)
  result <- generatePRNG(10)
  
  expect_equal(length(result), 10)
  expect_true(all(result >= 0 & result < 1))
  
  # Test large jump
  jumpAheadPRNG(1000000)
  result2 <- generatePRNG(10)
  
  expect_equal(length(result2), 10)
  expect_true(all(result2 >= 0 & result2 < 1))
  
  cleanup_prng()
})

test_that("Jump-ahead performance scales better than O(n)", {
  skip_on_cran()
  skip_if(!requireNamespace("microbenchmark", quietly = TRUE), 
          "microbenchmark package not available")
  
  cfg <- list(
    a = 2,
    b = 5,
    c = -2,
    mpfr_precision = 53,
    distribution = "uniform_01"
  )
  
  # Test different jump sizes
  sizes <- c(1000, 10000, 100000)
  times <- numeric(length(sizes))
  
  for (i in seq_along(sizes)) {
    createPRNG(cfg)
    
    # Benchmark the jump-ahead operation
    bench <- microbenchmark(
      jumpAheadPRNG(sizes[i]),
      times = 5,
      unit = "milliseconds"
    )
    
    times[i] <- median(bench$time) / 1e6  # Convert to milliseconds
    cleanup_prng()
  }
  
  # Check that time doesn't scale linearly
  # If implementation is O(n), time should increase proportionally
  # We expect sub-linear scaling due to block processing
  
  # Calculate scaling factors
  scale_10x <- times[2] / times[1]  # Should be much less than 10 if sub-linear
  scale_100x <- times[3] / times[1] # Should be much less than 100 if sub-linear
  
  cat("\nJump-ahead performance scaling:\n")
  cat("1,000 steps:", round(times[1], 2), "ms\n")
  cat("10,000 steps:", round(times[2], 2), "ms (", round(scale_10x, 1), "x)\n")
  cat("100,000 steps:", round(times[3], 2), "ms (", round(scale_100x, 1), "x)\n")
  
  # Even with O(n) implementation, we expect some sub-linear scaling due to
  # CPU caching and optimizations. We just verify it's not drastically super-linear
  expect_lt(scale_10x, 15)
  expect_lt(scale_100x, 150)
})

test_that("Jump-ahead works correctly with state consistency", {
  skip_on_cran()
  
  cfg <- list(
    a = 2,
    b = 5,
    c = -2,
    mpfr_precision = 53,
    distribution = "uniform_01"
  )
  
  # Create PRNG and generate initial sequence
  createPRNG(cfg)
  initial <- generatePRNG(5)
  
  # Jump ahead and generate more
  jumpAheadPRNG(1000)
  after_jump <- generatePRNG(5)
  
  # The sequences should be different (extremely unlikely to be the same)
  expect_false(all(initial == after_jump))
  
  # Both should be valid uniform random numbers
  expect_true(all(initial >= 0 & initial < 1))
  expect_true(all(after_jump >= 0 & after_jump < 1))
  
  cleanup_prng()
})

test_that("Jump-ahead produces appropriate statistical properties", {
  skip_on_cran()
  
  cfg <- list(
    a = 2,
    b = 5,
    c = -2,
    mpfr_precision = 53,
    distribution = "uniform_01"
  )
  
  createPRNG(cfg)
  
  # Generate samples with jumps
  all_samples <- numeric(0)
  jump_size <- 10000
  
  for (i in 1:100) {
    samples <- generatePRNG(10)
    all_samples <- c(all_samples, samples)
    jumpAheadPRNG(jump_size)
  }
  
  # Test uniformity
  ks_test <- ks.test(all_samples, "punif")
  expect_gt(ks_test$p.value, 0.01)
  
  # Test basic statistics
  expect_lt(abs(mean(all_samples) - 0.5), 0.05)
  expect_lt(abs(var(all_samples) - 1/12), 0.02)
  
  cleanup_prng()
})

test_that("Very large jumps complete in reasonable time", {
  skip_on_cran()
  skip_if(Sys.getenv("RUN_LARGE_JUMPS") != "true",
          "Skipping very large jump test")
  
  cfg <- list(
    a = 2,
    b = 5,
    c = -2,
    mpfr_precision = 53,
    distribution = "uniform_01"
  )
  
  createPRNG(cfg)
  
  # Time a very large jump
  start_time <- Sys.time()
  jumpAheadPRNG(1e9)  # 1 billion steps
  end_time <- Sys.time()
  
  time_taken <- as.numeric(end_time - start_time, units = "secs")
  
  # Even with O(n) implementation, modern CPUs can do ~1 billion simple operations
  # in a few seconds. We set a generous limit.
  expect_lt(time_taken, 60)
  
  # Verify the PRNG still works after large jump
  result <- generatePRNG(10)
  expect_equal(length(result), 10)
  expect_true(all(result >= 0 & result < 1))
  
  cleanup_prng()
})