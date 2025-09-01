# Comprehensive Test Suite for qiprng
# Tests all critical fixes: TLS cleanup, overflow protection, crypto security

test_that("Basic PRNG functionality works", {
  createPRNG()
  vals <- generatePRNG(1000)
  cleanup_prng()

  expect_equal(length(vals), 1000)
  expect_true(all(vals >= 0))
  expect_true(all(vals <= 1))
  expect_true(abs(mean(vals) - 0.5) < 0.1)
})

test_that("Thread safety with threading enabled", {
  skip_on_os("windows")

  test_thread <- function(id) {
    cfg <- default_config
    cfg$use_threading <- TRUE
    createPRNG(cfg)
    vals <- generatePRNG(100)
    cleanup_prng()
    list(id = id, ok = length(vals) == 100)
  }

  results <- parallel::mclapply(1:4, test_thread, mc.cores = 4)
  expect_true(all(sapply(results, function(r) r$ok)))
})

test_that("TLS cleanup works without crashes", {
  for (i in 1:10) {
    cfg <- default_config
    cfg$distribution <- "normal"
    createPRNG(cfg)
    vals <- generatePRNG(100)
    cleanup_prng()
  }
  expect_true(TRUE) # If we get here without crash, test passed
})

test_that("Overflow protection handles large values", {
  large_cfg <- list(
    a = 1000000L,
    b = 2000000L,
    c = -500000L,
    mpfr_precision = 53L,
    buffer_size = 100L,
    distribution = "uniform_01"
  )

  result <- tryCatch(
    {
      createPRNG(large_cfg)
      vals <- generatePRNG(10)
      cleanup_prng()
      TRUE
    },
    error = function(e) {
      grepl("overflow", e$message, ignore.case = TRUE)
    }
  )

  expect_true(result)
})

test_that("Invalid discriminant is rejected", {
  invalid_cfg <- list(
    a = 1L,
    b = 2L,
    c = 2L, # b^2 - 4ac = 4 - 8 = -4 (invalid)
    mpfr_precision = 53L,
    distribution = "uniform_01"
  )

  expect_error(
    createPRNG(invalid_cfg),
    "discriminant"
  )
})

test_that("Crypto mixing is non-deterministic", {
  cfg <- default_config
  cfg$use_crypto_mixing <- TRUE
  cfg$seed <- NULL

  createPRNG(cfg)
  vals1 <- generatePRNG(100)
  cleanup_prng()

  createPRNG(cfg)
  vals2 <- generatePRNG(100)
  cleanup_prng()

  expect_false(all(vals1 == vals2))
})

test_that("Deterministic seed with crypto triggers warning", {
  cfg <- default_config
  cfg$use_crypto_mixing <- TRUE
  cfg$seed <- 12345

  expect_warning(
    {
      createPRNG(cfg)
      cleanup_prng()
    },
    "SECURITY WARNING.*deterministic"
  )
})

test_that("Multiple distributions work", {
  distributions <- c("uniform_01", "normal", "exponential")

  for (dist in distributions) {
    cfg <- default_config
    cfg$distribution <- dist
    createPRNG(cfg)
    vals <- generatePRNG(100)
    cleanup_prng()

    expect_equal(length(vals), 100)
  }
})

test_that("Reseed produces different values", {
  createPRNG()
  vals1 <- generatePRNG(10)
  reseedPRNG()
  vals2 <- generatePRNG(10)
  cleanup_prng()

  expect_false(all(vals1 == vals2))
})

test_that("Jump ahead produces different values", {
  createPRNG()
  vals1 <- generatePRNG(10)

  cleanup_prng()
  createPRNG()
  jumpAheadPRNG(10)
  vals2 <- generatePRNG(10)
  cleanup_prng()

  expect_false(all(vals1 == vals2))
})

test_that("Memory stress test passes", {
  for (i in 1:20) {
    createPRNG()
    vals <- generatePRNG(1000)
    cleanup_prng()
  }
  expect_true(TRUE)
})

test_that("Concurrent operations work", {
  skip_on_os("windows")

  concurrent_test <- function(id) {
    for (i in 1:5) {
      cfg <- default_config
      cfg$a <- 2L + id
      cfg$b <- 5L + id * 2
      cfg$c <- -2L - id
      createPRNG(cfg)
      vals <- generatePRNG(100)
      cleanup_prng()
    }
    TRUE
  }

  results <- parallel::mclapply(1:4, concurrent_test, mc.cores = 4)
  expect_true(all(unlist(results)))
})

test_that("Deterministic mode is reproducible", {
  cfg <- default_config
  cfg$seed <- 42
  cfg$use_crypto_mixing <- FALSE

  createPRNG(cfg)
  vals1 <- generatePRNG(100)
  cleanup_prng()

  createPRNG(cfg)
  vals2 <- generatePRNG(100)
  cleanup_prng()

  expect_equal(vals1, vals2)
})

test_that("Statistical properties are maintained", {
  createPRNG()
  vals <- generatePRNG(10000)
  cleanup_prng()

  m <- mean(vals)
  s <- sd(vals)

  expect_true(abs(m - 0.5) < 0.02)
  expect_true(abs(s - sqrt(1 / 12)) < 0.02)
})

test_that("Edge cases are handled", {
  cfg <- default_config
  cfg$buffer_size <- 1L
  createPRNG(cfg)
  vals <- generatePRNG(10)
  cleanup_prng()

  expect_equal(length(vals), 10)
})
