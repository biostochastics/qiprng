context("Configuration tests")

test_that("Configuration updates work", {
  # Create initial config
  setup_prng()
  
  # Test updating buffer size
  new_cfg <- list(
    buffer_size = 100L
  )
  expect_no_error(updatePRNG(new_cfg))
  
  # Test updating crypto mixing
  new_cfg <- list(
    use_crypto_mixing = FALSE
  )
  expect_no_error(updatePRNG(new_cfg))
  
  # Test updating MPFR precision
  new_cfg <- list(
    mpfr_precision = 128L
  )
  expect_no_error(updatePRNG(new_cfg))
})

test_that("Invalid configurations are rejected", {
  # Test invalid MPFR precision
  cfg <- list(
    a = 1L,
    b = 5L,
    c = -1L,
    mpfr_precision = 1L  # Too small
  )
  expect_error(createPRNG(cfg))
  
  cfg$mpfr_precision <- 20000L  # Too large
  expect_error(createPRNG(cfg))
  
  # Test invalid buffer size
  cfg <- list(
    a = 1L,
    b = 5L,
    c = -1L,
    buffer_size = 0L  # Must be positive
  )
  expect_error(createPRNG(cfg))
  
  # Test invalid quadratic parameters
  cfg <- list(
    a = 0L,  # Must be positive
    b = 5L,
    c = -1L
  )
  expect_error(createPRNG(cfg))
  
  cfg <- list(
    a = 1L,
    b = 5L,
    c = 1L  # Must be negative
  )
  expect_error(createPRNG(cfg))
})