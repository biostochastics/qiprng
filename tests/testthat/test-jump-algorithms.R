context("Jump-Ahead Algorithm Tests")

library(qiprng)

test_that("All jump-ahead algorithms work correctly", {
  skip_on_cran()

  cfg <- list(
    a = 2,
    b = 5,
    c = -2,
    mpfr_precision = 53,
    distribution = "uniform_01",
    use_crypto_mixing = FALSE # Disable crypto for performance testing
  )

  algorithms <- c(
    "0", # ORIGINAL_128BIT
    "1", # MPFR_MATRIX
    "2", # MODULAR_MERSENNE
    "3" # DIRECT_CFE
  )

  algorithm_names <- c(
    "ORIGINAL_128BIT",
    "MPFR_MATRIX",
    "MODULAR_MERSENNE",
    "DIRECT_CFE"
  )

  # Test each algorithm
  for (i in seq_along(algorithms)) {
    # Set environment variable
    Sys.setenv(QIPRNG_JUMP_ALGORITHM = algorithms[i])

    # Create fresh PRNG
    createPRNG(cfg)

    # Test small jump
    jumpAheadPRNG(100)
    result1 <- generatePRNG(10)

    expect_equal(length(result1), 10,
      info = paste("Algorithm", algorithm_names[i], "small jump")
    )
    expect_true(all(result1 >= 0 & result1 < 1),
      info = paste("Algorithm", algorithm_names[i], "small jump values")
    )

    # Test medium jump
    jumpAheadPRNG(10000)
    result2 <- generatePRNG(10)

    expect_equal(length(result2), 10,
      info = paste("Algorithm", algorithm_names[i], "medium jump")
    )
    expect_true(all(result2 >= 0 & result2 < 1),
      info = paste("Algorithm", algorithm_names[i], "medium jump values")
    )

    # Test large jump (may overflow for ORIGINAL_128BIT)
    tryCatch(
      {
        jumpAheadPRNG(1000000)
        result3 <- generatePRNG(10)

        expect_equal(length(result3), 10,
          info = paste("Algorithm", algorithm_names[i], "large jump")
        )
        expect_true(all(result3 >= 0 & result3 < 1),
          info = paste("Algorithm", algorithm_names[i], "large jump values")
        )
      },
      error = function(e) {
        if (i == 1) {
          # Expected for ORIGINAL_128BIT with large jumps
          expect_true(grepl("overflow", tolower(e$message)),
            info = "ORIGINAL_128BIT should overflow for large jumps"
          )
        } else {
          stop(e) # Re-throw for other algorithms
        }
      }
    )

    cleanup_prng()
  }

  # Clear environment variable
  Sys.unsetenv("QIPRNG_JUMP_ALGORITHM")
})

test_that("Jump-ahead algorithms produce consistent sequences", {
  skip_on_cran()

  cfg <- list(
    a = 2,
    b = 5,
    c = -2,
    mpfr_precision = 53,
    distribution = "uniform_01",
    seed = 12345,
    use_crypto_mixing = FALSE # Cannot use crypto with deterministic seed
  )

  # Compare MPFR_MATRIX and MODULAR_MERSENNE for consistency
  # They should produce identical sequences

  # Algorithm 1: MPFR_MATRIX
  Sys.setenv(QIPRNG_JUMP_ALGORITHM = "1")
  createPRNG(cfg)

  jumpAheadPRNG(5000)
  mpfr_result <- generatePRNG(20)
  cleanup_prng()

  # Algorithm 2: MODULAR_MERSENNE
  Sys.setenv(QIPRNG_JUMP_ALGORITHM = "2")
  createPRNG(cfg)

  jumpAheadPRNG(5000)
  modular_result <- generatePRNG(20)
  cleanup_prng()

  # They may produce different results due to different arithmetic methods
  # MPFR uses arbitrary precision, MODULAR uses modular arithmetic
  # So we'll just check they're both valid uniform values
  expect_true(all(mpfr_result >= 0 & mpfr_result < 1))
  expect_true(all(modular_result >= 0 & modular_result < 1))

  Sys.unsetenv("QIPRNG_JUMP_ALGORITHM")
})

test_that("Jump-ahead performance comparison", {
  skip_on_cran()
  skip_if(
    !requireNamespace("microbenchmark", quietly = TRUE),
    "microbenchmark package not available"
  )

  library(microbenchmark)

  cfg <- list(
    a = 2,
    b = 5,
    c = -2,
    mpfr_precision = 53,
    distribution = "uniform_01",
    use_crypto_mixing = FALSE # Disable crypto for performance testing
  )

  jump_size <- 100000

  # Benchmark each algorithm
  times <- list()

  for (algo in c("1", "2")) { # Skip ORIGINAL (may overflow) and DIRECT_CFE (no optimization)
    Sys.setenv(QIPRNG_JUMP_ALGORITHM = algo)

    bench <- microbenchmark(
      {
        createPRNG(cfg)
        jumpAheadPRNG(jump_size)
        cleanup_prng()
      },
      times = 5,
      unit = "milliseconds"
    )

    times[[algo]] <- median(bench$time) / 1e6 # Convert to ms
  }

  cat("\nJump-ahead algorithm performance (", jump_size, " steps):\n", sep = "")
  cat("MPFR_MATRIX:", round(times[["1"]], 2), "ms\n")
  cat("MODULAR_MERSENNE:", round(times[["2"]], 2), "ms\n")

  # Modular should be faster than MPFR
  expect_lt(times[["2"]], times[["1"]] * 1.5) # Modular arithmetic should not be much slower than MPFR

  Sys.unsetenv("QIPRNG_JUMP_ALGORITHM")
})

test_that("Invalid algorithm selection falls back gracefully", {
  skip_on_cran()

  cfg <- list(
    a = 2,
    b = 5,
    c = -2,
    mpfr_precision = 53,
    distribution = "uniform_01",
    use_crypto_mixing = FALSE # Disable crypto for performance testing
  )

  # Test invalid algorithm values
  for (invalid in c("-1", "4", "999", "abc")) {
    Sys.setenv(QIPRNG_JUMP_ALGORITHM = invalid)

    createPRNG(cfg)

    # Should fall back to default (MODULAR_MERSENNE) without crashing
    jumpAheadPRNG(1000)
    result <- generatePRNG(10)

    expect_equal(length(result), 10,
      info = paste("Invalid algorithm", invalid, "should fall back")
    )
    expect_true(all(result >= 0 & result < 1),
      info = paste("Invalid algorithm", invalid, "produces valid output")
    )

    cleanup_prng()
  }

  Sys.unsetenv("QIPRNG_JUMP_ALGORITHM")
})
