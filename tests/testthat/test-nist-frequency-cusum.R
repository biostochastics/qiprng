# Test file for NIST Frequency within Blocks and Cumulative Sums tests

# Source the binary_tests.R file to get access to the functions
source("../../R/statisticaltests/binary_tests.R")

test_that("frequency_test_within_blocks works correctly", {
  # Create a test suite object
  suite <- list(
    config = list(
      binary_sample_size = 1000,
      significance_level = 0.01,
      save_visualizations = FALSE
    ),
    prng_func = function(n) runif(n),
    results = list()
  )

  # Test with known bit sequence
  # All ones - should fail the test
  all_ones <- rep(1, 1000)
  result_ones <- frequency_test_within_blocks(all_ones, block_size = 100)
  expect_true(!is.na(result_ones$p.value))
  expect_true(result_ones$p.value < 0.01) # Should fail

  # All zeros - should fail the test
  all_zeros <- rep(0, 1000)
  result_zeros <- frequency_test_within_blocks(all_zeros, block_size = 100)
  expect_true(!is.na(result_zeros$p.value))
  expect_true(result_zeros$p.value < 0.01) # Should fail

  # Random sequence - should likely pass
  set.seed(123)
  random_bits <- sample(0:1, 1000, replace = TRUE)
  result_random <- frequency_test_within_blocks(random_bits, block_size = 100)
  expect_true(!is.na(result_random$p.value))
  expect_true(result_random$block_count == 10) # 1000 / 100 = 10 blocks

  # Test with insufficient data
  short_seq <- rep(0, 50)
  result_short <- frequency_test_within_blocks(short_seq, block_size = 100)
  expect_true(is.na(result_short$p.value))
})

test_that("cumulative_sums_test works correctly", {
  # Test forward mode
  # Balanced sequence - should pass
  set.seed(456)
  balanced_bits <- sample(0:1, 1000, replace = TRUE)
  result_forward <- cumulative_sums_test(balanced_bits, mode = "forward")
  expect_true(!is.na(result_forward$p.value))
  expect_true(result_forward$mode == "forward")

  # Test backward mode
  result_backward <- cumulative_sums_test(balanced_bits, mode = "backward")
  expect_true(!is.na(result_backward$p.value))
  expect_true(result_backward$mode == "backward")

  # Biased sequence - more ones
  biased_bits <- sample(0:1, 1000, replace = TRUE, prob = c(0.3, 0.7))
  result_biased <- cumulative_sums_test(biased_bits, mode = "forward")
  expect_true(!is.na(result_biased$p.value))
  expect_true(result_biased$max_excursion > 0)

  # Test with insufficient data
  short_seq <- rep(0, 5)
  result_short <- cumulative_sums_test(short_seq, mode = "forward")
  expect_true(is.na(result_short$p.value))

  # Test error handling for invalid mode
  expect_error(cumulative_sums_test(balanced_bits, mode = "invalid"))
})

test_that("NIST tests integrate properly with binary_tests suite", {
  # Create a minimal test suite
  suite <- list(
    config = list(
      binary_sample_size = 2000,
      significance_level = 0.01,
      save_visualizations = FALSE
    ),
    prng_func = function(n) runif(n),
    results = list()
  )

  # Run the full binary tests
  suite_result <- run_binary_tests(suite)

  # Check that new tests are included
  expect_true("frequency_within_blocks" %in% names(suite_result$results$binary))
  expect_true("cumulative_sums_forward" %in% names(suite_result$results$binary))
  expect_true("cumulative_sums_backward" %in% names(suite_result$results$binary))

  # Check result structure
  freq_result <- suite_result$results$binary$frequency_within_blocks
  expect_true(!is.null(freq_result$p_value))
  expect_true(!is.null(freq_result$result))
  expect_true(!is.null(freq_result$description))

  cusum_f_result <- suite_result$results$binary$cumulative_sums_forward
  expect_true(!is.null(cusum_f_result$p_value))
  expect_true(!is.null(cusum_f_result$result))
  expect_true(!is.null(cusum_f_result$description))
})

# Helper function definitions for standalone testing
frequency_test_within_blocks <- function(bit_sequence, block_size = 100) {
  n <- length(bit_sequence)

  if (n < block_size) {
    return(list(p.value = NA, statistic = NA, block_count = 0))
  }

  num_blocks <- n %/% block_size

  if (num_blocks < 1) {
    return(list(p.value = NA, statistic = NA, block_count = 0))
  }

  block_proportions <- numeric(num_blocks)
  for (i in 1:num_blocks) {
    start_idx <- (i - 1) * block_size + 1
    end_idx <- i * block_size
    block <- bit_sequence[start_idx:end_idx]
    block_proportions[i] <- sum(block) / block_size
  }

  chi_square <- 4 * block_size * sum((block_proportions - 0.5)^2)
  df <- num_blocks
  p_value <- 1 - pchisq(chi_square, df)

  list(
    p.value = p_value,
    statistic = chi_square,
    block_count = num_blocks,
    block_size = block_size,
    mean_proportion = mean(block_proportions),
    sd_proportion = sd(block_proportions)
  )
}

cumulative_sums_test <- function(bit_sequence, mode = "forward") {
  n <- length(bit_sequence)

  if (n < 10) {
    return(list(p.value = NA, statistic = NA, max_excursion = 0))
  }

  X <- ifelse(bit_sequence == 0, -1, 1)

  if (mode == "forward") {
    S <- cumsum(X)
  } else if (mode == "backward" || mode == "reverse") {
    S <- cumsum(rev(X))
  } else {
    stop("Mode must be 'forward' or 'backward'/'reverse'")
  }

  max_excursion <- max(abs(S))
  z <- max_excursion / sqrt(n)

  k_start <- floor(-n / max_excursion) + 1
  k_end <- floor(n / max_excursion) - 1

  if (k_end < k_start) {
    k_end <- k_start
  }

  sum_terms <- 0
  for (k in k_start:k_end) {
    term1 <- pnorm((4 * k + 1) * max_excursion / sqrt(n))
    term2 <- pnorm((4 * k - 1) * max_excursion / sqrt(n))
    sum_terms <- sum_terms + term1 - term2
  }

  p_value <- 1 - sum_terms
  p_value <- max(0, min(1, p_value))

  list(
    p.value = p_value,
    statistic = z,
    max_excursion = max_excursion,
    mode = mode,
    n = n
  )
}
