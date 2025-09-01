# File: binary_tests.R
# ----------------------------------------------------------------------
#' Binary and bitwise tests for PRNG quality
#'
#' This module provides tests to evaluate the bit-level properties
#' of the PRNG output, including bit frequency, runs of bits, etc.

# Source vectorized operations if available
if (file.exists(system.file("R/statisticaltests/vectorized_operations.R", package = "qiprng"))) {
  source(system.file("R/statisticaltests/vectorized_operations.R", package = "qiprng"))
} else if (file.exists("R/statisticaltests/vectorized_operations.R")) {
  source("R/statisticaltests/vectorized_operations.R")
}

# Source effect size calculations if available
if (file.exists(system.file("R/statisticaltests/effect_sizes.R", package = "qiprng"))) {
  source(system.file("R/statisticaltests/effect_sizes.R", package = "qiprng"))
} else if (file.exists("R/statisticaltests/effect_sizes.R")) {
  source("R/statisticaltests/effect_sizes.R")
}

#' Run binary and bitwise tests
#'
#' @param suite The test suite object
#' @return Updated test suite with results
#' @keywords internal
run_binary_tests <- function(suite) {
  # Generate random numbers
  n <- suite$config$binary_sample_size

  # Handle very small sample sizes gracefully
  if (n < 10) {
    n <- 10 # Enforce a minimum sample size for statistical validity
  }

  x <- suite$prng_func(n)

  # Initialize results
  suite$results$binary <- list()

  # Convert floating point values to binary representation
  # We'll work with 64 bits per value for better precision
  numeric_to_bits <- function(x, bits_per_value = 64) {
    # Handle potential NA or NULL values
    if (is.null(x) || length(x) == 0) {
      return(matrix(0, nrow = 0, ncol = bits_per_value))
    }

    # Remove any NA values and warn about them
    na_count <- sum(is.na(x))
    if (na_count > 0) {
      warning(paste("Found", na_count, "NA values in input to numeric_to_bits. These will be removed."))
      x <- x[!is.na(x)]
    }

    # Scale to integers in [0, 2^bits_per_value - 1]
    x <- pmin(x, 0.9999999) # Ensure values are < 1 to prevent overflow
    x <- pmax(x, 0) # Ensure values are >= 0

    # Use full 64-bit representation when possible
    # This fixes the issue of truncating information from higher bits
    max_int <- if (.Machine$sizeof.pointer >= 8) 2^53 - 1 else 2^31 - 1

    # Scale carefully to avoid NA values or overflow
    scaled <- as.integer(x * min(2^bits_per_value - 1, max_int))

    # Convert to binary
    result <- matrix(0, nrow = length(x), ncol = bits_per_value)
    for (i in seq_along(x)) {
      value <- scaled[i]
      if (is.na(value)) {
        value <- 0 # Handle NA values
      }
      for (j in seq_len(bits_per_value)) {
        result[i, bits_per_value - j + 1] <- value %% 2
        value <- value %/% 2
      }
    }

    return(result)
  }

  # Convert binary matrix to a long bit sequence
  binary_matrix_to_sequence <- function(binary_matrix) {
    as.vector(t(binary_matrix))
  }

  # Generate binary representation
  # Use vectorized version if available
  if (exists("numeric_to_bits_vectorized")) {
    bit_matrix <- numeric_to_bits_vectorized(x)
  } else {
    bit_matrix <- numeric_to_bits(x)
  }
  bit_sequence <- binary_matrix_to_sequence(bit_matrix)

  # Remove any NA values from bit sequence
  bit_sequence <- bit_sequence[!is.na(bit_sequence)]

  # Check if we have enough data after NA removal
  if (length(bit_sequence) < 100) {
    suite$results$binary <- list(
      monobit = list(
        description = "Monobit Test", result = "INCONCLUSIVE",
        p_value = NA, details = "Insufficient data after NA removal"
      ),
      block_frequency = list(
        description = "Block Frequency Test", result = "INCONCLUSIVE",
        p_value = NA, details = "Insufficient data after NA removal"
      ),
      runs = list(
        description = "Runs Test", result = "INCONCLUSIVE",
        p_value = NA, details = "Insufficient data after NA removal"
      ),
      longest_run = list(
        description = "Longest Run Test", result = "INCONCLUSIVE",
        p_value = NA, details = "Insufficient data after NA removal"
      ),
      serial = list(
        description = "Serial Test", result = "INCONCLUSIVE",
        p_value = NA, details = "Insufficient data after NA removal"
      ),
      approximate_entropy = list(
        description = "Approximate Entropy Test", result = "INCONCLUSIVE",
        p_value = NA, details = "Insufficient data after NA removal"
      )
    )
    return(suite)
  }

  # 1. Monobit (frequency) test
  monobit_test <- function(bits) {
    n <- length(bits)
    ones_count <- sum(bits)
    proportion <- ones_count / n

    # Expected proportion is 0.5
    z <- (ones_count - n / 2) / sqrt(n / 4)
    p_value <- 2 * (1 - pnorm(abs(z)))

    list(
      p.value = p_value,
      statistic = z,
      proportion = proportion,
      ones_count = ones_count,
      zeros_count = n - ones_count
    )
  }

  # Run monobit test
  monobit_result <- monobit_test(bit_sequence)
  monobit_test_result <- list(
    description = "Monobit (Frequency) Test",
    result = if (is.na(monobit_result$p.value)) {
      "INCONCLUSIVE"
    } else if (monobit_result$p.value >= suite$config$significance_level) {
      "PASS"
    } else {
      "FAIL"
    },
    p_value = monobit_result$p.value,
    statistic = monobit_result$statistic,
    details = paste(
      "Tests if the proportion of 1s is close to 0.5.",
      "Proportion:", round(monobit_result$proportion, 4),
      "Ones:", monobit_result$ones_count,
      "Zeros:", monobit_result$zeros_count
    )
  )

  # Calculate Cohen's h for proportion test
  if (exists("calculate_cohens_h") && !is.na(monobit_result$proportion)) {
    cohens_h <- calculate_cohens_h(monobit_result$proportion, 0.5)
    monobit_test_result <- add_effect_size(monobit_test_result, cohens_h, "h")
  }

  suite$results$binary$monobit <- monobit_test_result

  # 2. Bit frequency by position test
  bit_frequency_by_position <- function(bit_matrix) {
    bits_per_value <- ncol(bit_matrix)
    n <- nrow(bit_matrix)

    # For each bit position, count ones
    counts <- colSums(bit_matrix)
    proportions <- counts / n

    # Calculate z-statistic for each position
    z_values <- (counts - n / 2) / sqrt(n / 4)
    p_values <- 2 * (1 - pnorm(abs(z_values)))

    # Combine p-values using Fisher's method
    # -2 * sum(log(p-values)) follows chi-square with 2k degrees of freedom
    chi_square <- -2 * sum(log(p_values))
    combined_p <- 1 - pchisq(chi_square, 2 * bits_per_value)

    # Count failures (positions that fail the individual test)
    failures <- sum(p_values < suite$config$significance_level)
    expected_failures <- bits_per_value * suite$config$significance_level

    list(
      p.value = combined_p,
      statistic = chi_square,
      proportions = proportions,
      z_values = z_values,
      p_values = p_values,
      failures = failures,
      expected_failures = expected_failures
    )
  }

  # Run bit frequency by position test
  bit_pos_result <- bit_frequency_by_position(bit_matrix)
  suite$results$binary$bit_position <- list(
    description = "Bit Frequency by Position Test",
    result = if (is.na(bit_pos_result$p.value)) {
      "INCONCLUSIVE"
    } else if (bit_pos_result$p.value >= suite$config$significance_level) {
      "PASS"
    } else {
      "FAIL"
    },
    p_value = bit_pos_result$p.value,
    statistic = bit_pos_result$statistic,
    details = paste(
      "Tests if each bit position has proper 0/1 distribution.",
      "Failing positions:", bit_pos_result$failures,
      "/ Expected:", round(bit_pos_result$expected_failures, 2)
    )
  )

  # 3. Bit runs test
  bit_runs_test <- function(bits) {
    n <- length(bits)
    if (n <= 1) {
      return(list(p.value = NA, statistic = NA, runs = 0, expected_runs = 0, n_clean = 0))
    }

    # Remove NA values if any
    bits_clean <- bits[!is.na(bits)]
    n_clean <- length(bits_clean)

    if (n_clean <= 1) {
      return(list(p.value = NA, statistic = NA, runs = 0, expected_runs = 0, n_clean = n_clean))
    }

    # Count runs
    runs <- 1
    for (i in 2:n_clean) { # Using 2:n_clean since we start at second element
      if (bits_clean[i] != bits_clean[i - 1]) {
        runs <- runs + 1
      }
    }

    # Count proportions of ones and zeros for more accurate expected runs calculation
    n1 <- sum(bits_clean)
    n0 <- n_clean - n1

    # Calculate the proportion of ones and zeros
    p1 <- n1 / n_clean
    p0 <- n0 / n_clean

    # More accurate formula for expected runs based on proportions
    # This addresses the concern about accounting for the proportion of ones and zeros separately
    expected_runs <- 2 * n_clean * p0 * p1 + 1

    # Improved variance calculation with more appropriate sample size thresholds
    # For samples larger than 100, use asymptotic formula
    # For smaller samples, use a more accurate approximation
    if (n_clean > 100) {
      var_runs <- 2 * n_clean * (2 * n_clean * p0 * p1 - 1) * p0 * p1 / (p0 + p1)
    } else if (n_clean > 50) {
      var_runs <- max(1, 4 * n1 * n0 * (2 * n1 * n0 - n_clean) / (n_clean^2 * (n_clean - 1)))
    } else {
      # For very small samples, use this conservative estimate
      var_runs <- max(1, n_clean * p0 * p1)
    }

    # Z-statistic
    z <- (runs - expected_runs) / sqrt(var_runs)
    p_value <- 2 * (1 - pnorm(abs(z)))

    list(
      p.value = p_value,
      statistic = z,
      runs = runs,
      expected_runs = expected_runs,
      proportion_ones = p1,
      proportion_zeros = p0,
      n_clean = n_clean
    )
  }

  # Run bit runs test
  bit_runs_result <- bit_runs_test(bit_sequence)
  suite$results$binary$bit_runs <- list(
    description = "Bit Runs Test",
    result = if (is.na(bit_runs_result$p.value)) {
      "INCONCLUSIVE"
    } else if (bit_runs_result$p.value >= suite$config$significance_level) {
      "PASS"
    } else {
      "FAIL"
    },
    p_value = bit_runs_result$p.value,
    statistic = bit_runs_result$statistic,
    diagnostics = list(
      runs = bit_runs_result$runs,
      expected_runs = bit_runs_result$expected_runs,
      sample_size = bit_runs_result$n_clean
    ),
    details = paste(
      "Tests if runs of 0s and 1s have expected length.",
      "Runs:", bit_runs_result$runs,
      "/ Expected:", round(bit_runs_result$expected_runs, 2),
      "(Sample size:", bit_runs_result$n_clean, ")"
    )
  )

  # 4. Poker test (k-bit patterns)
  poker_test <- function(bit_matrix, k = 4) {
    bits_per_value <- ncol(bit_matrix)
    n <- nrow(bit_matrix)

    # Check for empty input
    if (n == 0 || bits_per_value == 0) {
      return(list(p.value = NA, statistic = NA, df = NA, pattern_counts = NULL))
    }

    # Determine how to handle when bits_per_value is not a multiple of k
    if (bits_per_value %% k != 0) {
      # Calculate how many complete k-bit blocks we can form
      blocks_per_value <- bits_per_value %/% k
      extra_bits <- bits_per_value %% k

      # Improved approach to handle the remaining bits
      # Include partial blocks by padding with zeros to complete the block size
      use_partial_blocks <- TRUE
    } else {
      blocks_per_value <- bits_per_value / k
      extra_bits <- 0
      use_partial_blocks <- FALSE
    }

    # Split each value into k-bit blocks
    blocks <- list()
    for (i in seq_len(n)) {
      bits <- bit_matrix[i, ]

      # Process complete blocks
      for (j in 1:blocks_per_value) {
        start_idx <- (j - 1) * k + 1
        end_idx <- j * k
        block <- bits[start_idx:end_idx]

        # Convert block to decimal
        decimal <- sum(block * (2^(rev(seq_along(block) - 1))))
        blocks <- c(blocks, decimal)
      }

      # Process partial block if needed
      if (use_partial_blocks && extra_bits > 0) {
        start_idx <- blocks_per_value * k + 1
        end_idx <- bits_per_value
        partial_block <- bits[start_idx:end_idx]

        # Pad with zeros to complete k bits
        padded_block <- c(partial_block, rep(0, k - extra_bits))

        # Convert padded block to decimal
        decimal <- sum(padded_block * (2^(rev(seq_along(padded_block) - 1))))
        blocks <- c(blocks, decimal)
      }
    }

    # Count pattern frequencies
    total_patterns <- 2^k
    levels_vector <- 0:(total_patterns - 1)

    # Handle case when no blocks were processed
    if (length(blocks) == 0) {
      return(list(p.value = NA, statistic = NA, df = total_patterns - 1, pattern_counts = NULL))
    }

    pattern_counts <- table(factor(unlist(blocks), levels = levels_vector))

    # Calculate total blocks including partial blocks if used
    total_blocks <- n * blocks_per_value
    if (use_partial_blocks && extra_bits > 0) {
      total_blocks <- total_blocks + n
    }

    # Expected frequency for each pattern
    expected <- rep(total_blocks / total_patterns, total_patterns)

    # Chi-square test
    # Check if expected frequencies are sufficient for chi-square test
    if (min(expected) < 5 && length(expected) > 1) {
      # Combine patterns with low expected counts
      combined_pattern_counts <- numeric(0)
      combined_expected <- numeric(0)
      current_count <- 0
      current_expected <- 0

      for (i in seq_along(pattern_counts)) {
        if (expected[i] < 5) {
          current_count <- current_count + pattern_counts[i]
          current_expected <- current_expected + expected[i]

          if (current_expected >= 5 || i == length(pattern_counts)) {
            combined_pattern_counts <- c(combined_pattern_counts, current_count)
            combined_expected <- c(combined_expected, current_expected)
            current_count <- 0
            current_expected <- 0
          }
        } else {
          combined_pattern_counts <- c(combined_pattern_counts, pattern_counts[i])
          combined_expected <- c(combined_expected, expected[i])
        }
      }

      chi_square <- sum((combined_pattern_counts - combined_expected)^2 / combined_expected)
      df <- length(combined_pattern_counts) - 1
    } else {
      chi_square <- sum((pattern_counts - expected)^2 / expected)
      df <- total_patterns - 1
    }

    p_value <- 1 - pchisq(chi_square, df)

    list(
      p.value = p_value,
      statistic = chi_square,
      df = df,
      pattern_counts = pattern_counts,
      expected = expected
    )
  }

  # Run poker test with k=4
  poker_result <- poker_test(bit_matrix, k = 4)
  suite$results$binary$poker <- list(
    description = "Poker Test (4-bit patterns)",
    result = if (is.na(poker_result$p.value)) {
      "INCONCLUSIVE"
    } else if (poker_result$p.value >= suite$config$significance_level) {
      "PASS"
    } else {
      "FAIL"
    },
    p_value = poker_result$p.value,
    statistic = poker_result$statistic,
    details = paste(
      "Tests distribution of 4-bit patterns.",
      "Chi-square:", round(poker_result$statistic, 2),
      "with", poker_result$df, "degrees of freedom"
    )
  )

  # 5. Longest run of ones test
  longest_run_test <- function(bit_sequence) {
    n <- length(bit_sequence)

    if (n < 10) {
      return(list(p.value = NA, statistic = NA, longest_run = 0, expected_longest = 0))
    }

    # Find longest run of ones
    current_run <- 0
    longest_run <- 0
    for (i in seq_len(n)) {
      if (is.na(bit_sequence[i])) {
        current_run <- 0 # Reset on NA
      } else if (bit_sequence[i] == 1) {
        current_run <- current_run + 1
        if (current_run > longest_run) longest_run <- current_run
      } else {
        current_run <- 0
      }
    }

    # Improved expected longest run calculation based on the probability distribution
    # This formula is more accurate than the simple log2(n) + 1 approximation
    # The formula is based on extreme value theory for runs in Bernoulli sequences
    p <- mean(bit_sequence) # Proportion of ones

    # Ensuring p is not 0 or 1 to avoid log issues
    if (p <= 0 || p >= 1) {
      p <- 0.5 # Default to 0.5 in extreme cases
    }

    # More accurate calculation for expected longest run
    # Based on Schilling (1990) - "The Longest Run of Heads"
    expected_longest <- log(n * (1 - p), base = 1 / p) +
      (0.5772 + log(log(1 / (1 - p)), base = exp(1))) / log(1 / p, base = exp(1))

    # Better variance approximation that scales with sequence length
    # pi^2/(6 * ln^2(1/p)) is the asymptotic variance term from extreme value theory
    var_longest <- (pi^2) / (6 * (log(1 / p, base = exp(1)))^2)

    # Z-statistic with improved approximation
    z <- (longest_run - expected_longest) / sqrt(var_longest)
    p_value <- 2 * (1 - pnorm(abs(z)))

    list(
      p.value = p_value,
      statistic = z,
      longest_run = longest_run,
      expected_longest = expected_longest,
      proportion_ones = p,
      variance = var_longest
    )
  }

  # Run longest run test
  longest_run_result <- longest_run_test(bit_sequence)
  suite$results$binary$longest_run <- list(
    description = "Longest Run of Ones Test",
    result = if (is.na(longest_run_result$p.value)) {
      "INCONCLUSIVE"
    } else if (longest_run_result$p.value >= suite$config$significance_level) {
      "PASS"
    } else {
      "FAIL"
    },
    p_value = longest_run_result$p.value,
    statistic = longest_run_result$statistic,
    details = paste(
      "Tests if longest run of ones is as expected.",
      "Longest run:", longest_run_result$longest_run,
      "/ Expected:", round(longest_run_result$expected_longest, 2)
    )
  )

  # 6. Serial test (examining adjacency patterns)
  serial_test <- function(bit_sequence, pattern_length = 2) {
    n <- length(bit_sequence)

    # Need at least pattern_length+1 bits
    if (n <= pattern_length) {
      return(list(p.value = NA, statistic = NA))
    }

    # Count occurrences of each pattern
    patterns <- list()
    for (i in 1:(n - pattern_length + 1)) {
      pattern <- bit_sequence[i:(i + pattern_length - 1)]
      pattern_str <- paste(pattern, collapse = "")
      patterns <- c(patterns, pattern_str)
    }

    # Count frequencies
    pattern_counts <- table(factor(unlist(patterns),
      levels = apply(expand.grid(rep(list(0:1), pattern_length)),
        1, paste,
        collapse = ""
      )
    ))

    # Expected frequency
    expected <- rep((n - pattern_length + 1) / (2^pattern_length), 2^pattern_length)

    # Chi-square test
    chi_square <- sum((pattern_counts - expected)^2 / expected)
    df <- 2^pattern_length - 1
    p_value <- 1 - pchisq(chi_square, df)

    list(
      p.value = p_value,
      statistic = chi_square,
      df = df
    )
  }

  # Run serial test
  serial_result <- serial_test(bit_sequence, pattern_length = 2)
  suite$results$binary$serial <- list(
    description = "Serial Test (2-bit patterns)",
    result = if (is.na(serial_result$p.value)) {
      "INCONCLUSIVE"
    } else if (serial_result$p.value >= suite$config$significance_level) {
      "PASS"
    } else {
      "FAIL"
    },
    p_value = serial_result$p.value,
    statistic = serial_result$statistic,
    details = paste(
      "Tests if 2-bit overlapping patterns are uniformly distributed.",
      "Chi-square:", round(serial_result$statistic, 2),
      "with", serial_result$df, "degrees of freedom"
    )
  )

  # 7. Frequency Test within Blocks (NIST SP 800-22)
  frequency_test_within_blocks <- function(bit_sequence, block_size = 100) {
    n <- length(bit_sequence)

    # Check if we have enough bits
    if (n < block_size) {
      return(list(p.value = NA, statistic = NA, block_count = 0))
    }

    # Calculate number of blocks (discard partial block)
    num_blocks <- n %/% block_size

    # If we have too few blocks, return NA
    if (num_blocks < 1) {
      return(list(p.value = NA, statistic = NA, block_count = 0))
    }

    # Calculate proportion of ones in each block
    block_proportions <- numeric(num_blocks)
    for (i in 1:num_blocks) {
      start_idx <- (i - 1) * block_size + 1
      end_idx <- i * block_size
      block <- bit_sequence[start_idx:end_idx]
      block_proportions[i] <- sum(block) / block_size
    }

    # Expected proportion is 0.5 for each block
    # Chi-square statistic: sum((pi - 0.5)^2) / (0.5 * 0.5 / M)
    # Where pi is proportion of ones in block i, M is block size
    chi_square <- 4 * block_size * sum((block_proportions - 0.5)^2)

    # Degrees of freedom = number of blocks
    df <- num_blocks

    # Calculate p-value using chi-square distribution
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

  # Run frequency test within blocks
  freq_blocks_result <- frequency_test_within_blocks(bit_sequence, block_size = 100)
  suite$results$binary$frequency_within_blocks <- list(
    description = "Frequency Test within Blocks (NIST)",
    result = if (is.na(freq_blocks_result$p.value)) {
      "INCONCLUSIVE"
    } else if (freq_blocks_result$p.value >= suite$config$significance_level) {
      "PASS"
    } else {
      "FAIL"
    },
    p_value = freq_blocks_result$p.value,
    statistic = freq_blocks_result$statistic,
    details = paste(
      "Tests if proportion of ones within blocks is close to 0.5.",
      "Blocks:", freq_blocks_result$block_count,
      "Block size:", freq_blocks_result$block_size,
      "Mean proportion:", round(freq_blocks_result$mean_proportion, 4)
    )
  )

  # 8. Cumulative Sums Test (NIST SP 800-22)
  cumulative_sums_test <- function(bit_sequence, mode = "forward") {
    n <- length(bit_sequence)

    if (n < 10) {
      return(list(p.value = NA, statistic = NA, max_excursion = 0))
    }

    # Convert 0s to -1s, keep 1s as 1s
    X <- ifelse(bit_sequence == 0, -1, 1)

    # Calculate cumulative sums based on mode
    if (mode == "forward") {
      S <- cumsum(X)
    } else if (mode == "backward" || mode == "reverse") {
      S <- cumsum(rev(X))
    } else {
      stop("Mode must be 'forward' or 'backward'/'reverse'")
    }

    # Find maximum absolute excursion from zero
    max_excursion <- max(abs(S))

    # Calculate test statistic using the formula from NIST
    # The p-value is calculated using the normal distribution approximation
    # Based on the random walk theory

    # For large n, the distribution of max|S_k| can be approximated
    # Using the formula from NIST SP 800-22

    # Calculate z values for the positive and negative excursions
    z <- max_excursion / sqrt(n)

    # Calculate p-value using the approximation from NIST
    # This uses the cumulative distribution function for the maximum of a random walk
    # The formula involves summing over k from -∞ to ∞

    # Simplified calculation using normal approximation for large n
    # This is based on the reflection principle and Brownian motion theory
    k_start <- floor(-n / max_excursion) + 1
    k_end <- floor(n / max_excursion) - 1

    if (k_end < k_start) {
      k_end <- k_start
    }

    # Sum the series for p-value calculation
    sum_terms <- 0
    for (k in k_start:k_end) {
      term1 <- pnorm((4 * k + 1) * max_excursion / sqrt(n))
      term2 <- pnorm((4 * k - 1) * max_excursion / sqrt(n))
      sum_terms <- sum_terms + term1 - term2
    }

    p_value <- 1 - sum_terms

    # Ensure p-value is in valid range
    p_value <- max(0, min(1, p_value))

    list(
      p.value = p_value,
      statistic = z,
      max_excursion = max_excursion,
      mode = mode,
      n = n
    )
  }

  # Run cumulative sums test in both modes
  cusum_forward_result <- cumulative_sums_test(bit_sequence, mode = "forward")
  suite$results$binary$cumulative_sums_forward <- list(
    description = "Cumulative Sums Test - Forward (NIST)",
    result = if (is.na(cusum_forward_result$p.value)) {
      "INCONCLUSIVE"
    } else if (cusum_forward_result$p.value >= suite$config$significance_level) {
      "PASS"
    } else {
      "FAIL"
    },
    p_value = cusum_forward_result$p.value,
    statistic = cusum_forward_result$statistic,
    details = paste(
      "Tests maximum excursion of cumulative sum (forward).",
      "Max excursion:", cusum_forward_result$max_excursion,
      "Z-statistic:", round(cusum_forward_result$statistic, 4)
    )
  )

  cusum_backward_result <- cumulative_sums_test(bit_sequence, mode = "backward")
  suite$results$binary$cumulative_sums_backward <- list(
    description = "Cumulative Sums Test - Backward (NIST)",
    result = if (is.na(cusum_backward_result$p.value)) {
      "INCONCLUSIVE"
    } else if (cusum_backward_result$p.value >= suite$config$significance_level) {
      "PASS"
    } else {
      "FAIL"
    },
    p_value = cusum_backward_result$p.value,
    statistic = cusum_backward_result$statistic,
    details = paste(
      "Tests maximum excursion of cumulative sum (backward).",
      "Max excursion:", cusum_backward_result$max_excursion,
      "Z-statistic:", round(cusum_backward_result$statistic, 4)
    )
  )

  # 9. Random Excursions Test (NIST SP 800-22)
  random_excursions_test <- function(bit_sequence) {
    n <- length(bit_sequence)

    # NIST recommends at least 1,000,000 bits for this test
    if (n < 1000000) {
      return(list(
        p.values = rep(NA, 8),
        statistics = rep(NA, 8),
        cycles = 0,
        states = -4:3,
        message = "Insufficient data: Random Excursions Test requires at least 1,000,000 bits"
      ))
    }

    # Convert 0s to -1s, keep 1s as 1s
    X <- ifelse(bit_sequence == 0, -1, 1)

    # Calculate cumulative sums
    S <- c(0, cumsum(X)) # Start with 0

    # Find cycles (returns to zero)
    cycles <- which(S == 0)
    num_cycles <- length(cycles) - 1 # Number of complete cycles

    # NIST requires at least 500 cycles
    if (num_cycles < 500) {
      return(list(
        p.values = rep(NA, 8),
        statistics = rep(NA, 8),
        cycles = num_cycles,
        states = -4:3,
        message = paste("Insufficient cycles:", num_cycles, "< 500 required")
      ))
    }

    # States to examine: -4, -3, -2, -1, 1, 2, 3, 4
    states <- c(-4:-1, 1:4)
    p_values <- numeric(8)
    statistics <- numeric(8)

    # For each state, count occurrences in each cycle
    for (i in seq_along(states)) {
      state <- states[i]

      # Count visits to this state in each cycle
      state_counts <- numeric(6) # 0, 1, 2, 3, 4, 5+ visits

      for (j in 1:(num_cycles)) {
        cycle_start <- cycles[j]
        cycle_end <- cycles[j + 1]
        cycle_values <- S[(cycle_start + 1):(cycle_end - 1)]

        if (length(cycle_values) > 0) {
          visits <- sum(cycle_values == state)
          if (visits >= 5) {
            state_counts[6] <- state_counts[6] + 1
          } else if (visits >= 0) {
            state_counts[visits + 1] <- state_counts[visits + 1] + 1
          }
        }
      }

      # Expected probabilities for each count (from NIST)
      # These are theoretical probabilities for random walks
      if (abs(state) == 1) {
        expected_probs <- c(0.5000, 0.2500, 0.1250, 0.0625, 0.0312, 0.0313)
      } else if (abs(state) == 2) {
        expected_probs <- c(0.7500, 0.0625, 0.0469, 0.0352, 0.0264, 0.0390)
      } else if (abs(state) == 3) {
        expected_probs <- c(0.8333, 0.0278, 0.0231, 0.0193, 0.0161, 0.0804)
      } else { # abs(state) == 4
        expected_probs <- c(0.8750, 0.0156, 0.0137, 0.0120, 0.0105, 0.0732)
      }

      # Chi-square test
      expected_counts <- expected_probs * num_cycles
      chi_square <- sum((state_counts - expected_counts)^2 / expected_counts)

      # p-value with 5 degrees of freedom
      p_value <- 1 - pchisq(chi_square, df = 5)

      p_values[i] <- p_value
      statistics[i] <- chi_square
    }

    list(
      p.values = p_values,
      statistics = statistics,
      cycles = num_cycles,
      states = states,
      message = "Test completed successfully"
    )
  }

  # Run random excursions test
  excursions_result <- random_excursions_test(bit_sequence)

  # Store results for all states
  suite$results$binary$random_excursions <- list(
    description = "Random Excursions Test (NIST)",
    result = if (all(is.na(excursions_result$p.values))) {
      "INCONCLUSIVE"
    } else if (all(excursions_result$p.values[!is.na(excursions_result$p.values)] >= suite$config$significance_level)) {
      "PASS"
    } else {
      "FAIL"
    },
    p_values = excursions_result$p.values,
    statistics = excursions_result$statistics,
    states = excursions_result$states,
    cycles = excursions_result$cycles,
    details = paste(
      excursions_result$message,
      "| Cycles:", excursions_result$cycles,
      "| Failed states:", sum(excursions_result$p.values < suite$config$significance_level, na.rm = TRUE)
    )
  )

  # 10. Random Excursions Variant Test (NIST SP 800-22)
  random_excursions_variant_test <- function(bit_sequence) {
    n <- length(bit_sequence)

    # NIST recommends at least 1,000,000 bits for this test
    if (n < 1000000) {
      return(list(
        p.values = rep(NA, 18),
        statistics = rep(NA, 18),
        cycles = 0,
        states = -9:9,
        message = "Insufficient data: Random Excursions Variant Test requires at least 1,000,000 bits"
      ))
    }

    # Convert 0s to -1s, keep 1s as 1s
    X <- ifelse(bit_sequence == 0, -1, 1)

    # Calculate cumulative sums
    S <- cumsum(X)

    # Count total visits to each state
    states <- -9:9
    states <- states[states != 0] # Remove zero state
    p_values <- numeric(18)
    statistics <- numeric(18)

    for (i in seq_along(states)) {
      state <- states[i]

      # Count total occurrences of this state
      state_count <- sum(S == state)

      # Calculate p-value using the formula from NIST
      # Based on the arc sine law for random walks
      abs_state <- abs(state)

      # Theoretical probability for state visits in random walk
      # Using the approximation from NIST SP 800-22
      if (state_count == 0) {
        p_value <- 1.0
      } else {
        # Calculate the test statistic
        J <- n # Total number of steps

        # Expected number of visits for this state
        # Based on random walk theory
        if (abs_state == 1) {
          # Special case for states ±1
          p_value <- erfc(abs(state_count - J) / (2 * J * 0.045))
        } else {
          # General case for other states
          # Using the asymptotic distribution
          denominator <- 2 * sqrt(2 * J * (4 * abs_state - 2))
          if (denominator > 0) {
            p_value <- erfc(abs(state_count - J) / denominator)
          } else {
            p_value <- 1.0
          }
        }
      }

      p_values[i] <- p_value
      statistics[i] <- state_count
    }

    # Calculate erfc function (complementary error function)
    erfc <- function(x) {
      2 * pnorm(-x * sqrt(2))
    }

    # Recalculate p-values with proper erfc
    for (i in seq_along(states)) {
      state <- states[i]
      state_count <- statistics[i]
      abs_state <- abs(state)
      J <- n

      if (state_count == 0) {
        p_values[i] <- 1.0
      } else if (abs_state == 1) {
        p_values[i] <- erfc(abs(state_count - J) / (2 * J * 0.045))
      } else {
        denominator <- 2 * sqrt(2 * J * (4 * abs_state - 2))
        if (denominator > 0) {
          p_values[i] <- erfc(abs(state_count - J) / denominator)
        } else {
          p_values[i] <- 1.0
        }
      }
    }

    list(
      p.values = p_values,
      statistics = statistics,
      states = states,
      message = "Test completed successfully"
    )
  }

  # Run random excursions variant test
  excursions_var_result <- random_excursions_variant_test(bit_sequence)

  # Store results for all states
  suite$results$binary$random_excursions_variant <- list(
    description = "Random Excursions Variant Test (NIST)",
    result = if (all(is.na(excursions_var_result$p.values))) {
      "INCONCLUSIVE"
    } else if (all(excursions_var_result$p.values[!is.na(excursions_var_result$p.values)] >= suite$config$significance_level)) {
      "PASS"
    } else {
      "FAIL"
    },
    p_values = excursions_var_result$p.values,
    statistics = excursions_var_result$statistics,
    states = excursions_var_result$states,
    details = paste(
      excursions_var_result$message,
      "| Failed states:", sum(excursions_var_result$p.values < suite$config$significance_level, na.rm = TRUE)
    )
  )

  # 11. Linear Complexity Test (NIST SP 800-22)
  linear_complexity_test <- function(bit_sequence, M = 500) {
    n <- length(bit_sequence)

    # NIST recommends specific parameters
    if (n < 1000000) {
      return(list(
        p.value = NA,
        statistic = NA,
        message = "Insufficient data: Linear Complexity Test requires at least 1,000,000 bits"
      ))
    }

    # Number of blocks
    N <- floor(n / M)
    if (N < 200) {
      return(list(
        p.value = NA,
        statistic = NA,
        message = paste("Insufficient blocks:", N, "< 200 required")
      ))
    }

    # Berlekamp-Massey algorithm implementation
    berlekamp_massey <- function(s) {
      n <- length(s)
      C <- c(1, rep(0, n - 1)) # Connection polynomial
      B <- c(1, rep(0, n - 1)) # Previous C
      L <- 0 # Linear complexity
      m <- -1
      N <- 0

      while (N < n) {
        # Calculate discrepancy
        d <- s[N + 1]
        for (i in 1:L) {
          if (N - i >= 0) {
            d <- (d + C[i + 1] * s[N - i + 1]) %% 2
          }
        }

        if (d == 1) {
          T <- C # Temporary copy

          # Update C
          j <- 0
          while (j <= n - 1 && N - m + j <= n - 1) {
            if (N - m + j >= 0) {
              C[j + 1] <- (C[j + 1] + B[j + 1]) %% 2
            }
            j <- j + 1
          }

          if (L <= N / 2) {
            L <- N + 1 - L
            m <- N
            B <- T
          }
        }
        N <- N + 1
      }

      return(L)
    }

    # Calculate linear complexity for each block
    complexities <- numeric(N)

    for (i in 1:N) {
      start_idx <- (i - 1) * M + 1
      end_idx <- i * M
      block <- bit_sequence[start_idx:end_idx]

      # Apply Berlekamp-Massey algorithm
      # Use optimized version if available
      if (exists("berlekamp_massey_optimized")) {
        complexities[i] <- berlekamp_massey_optimized(block)
      } else {
        complexities[i] <- berlekamp_massey(block)
      }
    }

    # Expected theoretical mean
    mu <- M / 2 + (9 + (-1)^(M + 1)) / 36 - (M / 3 + 2 / 9) / 2^M

    # Categories for chi-square test
    # T_i values from NIST
    K <- 6
    pi <- c(0.010417, 0.03125, 0.125, 0.5, 0.25, 0.0625, 0.020833)

    # Calculate T_i for each complexity
    T <- numeric(N)
    for (i in 1:N) {
      T[i] <- (-1)^M * (complexities[i] - mu) + 2 / 9
    }

    # Categorize T values
    v <- numeric(K + 1)
    for (i in 1:N) {
      if (T[i] <= -2.5) {
        v[1] <- v[1] + 1
      } else if (T[i] <= -1.5) {
        v[2] <- v[2] + 1
      } else if (T[i] <= -0.5) {
        v[3] <- v[3] + 1
      } else if (T[i] <= 0.5) {
        v[4] <- v[4] + 1
      } else if (T[i] <= 1.5) {
        v[5] <- v[5] + 1
      } else if (T[i] <= 2.5) {
        v[6] <- v[6] + 1
      } else {
        v[7] <- v[7] + 1
      }
    }

    # Chi-square test
    chi_square <- 0
    for (i in 1:(K + 1)) {
      expected <- N * pi[i]
      if (expected > 0) {
        chi_square <- chi_square + (v[i] - expected)^2 / expected
      }
    }

    # p-value with K degrees of freedom
    p_value <- 1 - pchisq(chi_square, df = K)

    list(
      p.value = p_value,
      statistic = chi_square,
      blocks = N,
      block_size = M,
      mean_complexity = mean(complexities),
      message = "Test completed successfully"
    )
  }

  # Run linear complexity test
  linear_result <- linear_complexity_test(bit_sequence)

  suite$results$binary$linear_complexity <- list(
    description = "Linear Complexity Test (NIST)",
    result = if (is.na(linear_result$p.value)) {
      "INCONCLUSIVE"
    } else if (linear_result$p.value >= suite$config$significance_level) {
      "PASS"
    } else {
      "FAIL"
    },
    p_value = linear_result$p.value,
    statistic = linear_result$statistic,
    details = paste(
      linear_result$message,
      "| Blocks:", linear_result$blocks,
      "| Mean complexity:", round(linear_result$mean_complexity, 2)
    )
  )

  # 12. Approximate Entropy Test (NIST SP 800-22)
  approximate_entropy_test <- function(bit_sequence, m = 2) {
    n <- length(bit_sequence)

    # Check minimum sequence length
    # NIST recommends n >= 100 for meaningful results
    if (n < 100) {
      return(list(
        p.value = NA,
        statistic = NA,
        message = "Insufficient data: Approximate Entropy Test requires at least 100 bits"
      ))
    }

    # Adjust m if necessary based on sequence length
    # Rule of thumb: 2^m << n
    max_m <- floor(log2(n) - 5) # Conservative estimate
    if (m > max_m) {
      warning(paste("Block size m =", m, "too large for sequence length. Adjusting to", max_m))
      m <- max_m
    }

    if (m < 1) {
      m <- 1
    }

    # Function to count pattern frequencies
    count_patterns <- function(sequence, pattern_length) {
      # Number of possible patterns
      num_patterns <- 2^pattern_length

      # Initialize pattern counts
      pattern_counts <- rep(0, num_patterns)

      # Count overlapping patterns
      # For approximate entropy, we use circular extension
      extended_sequence <- c(sequence, sequence[1:(pattern_length - 1)])

      for (i in 1:n) {
        # Extract pattern
        if (i + pattern_length - 1 <= length(extended_sequence)) {
          pattern <- extended_sequence[i:(i + pattern_length - 1)]

          # Convert pattern to decimal
          pattern_decimal <- 0
          for (j in 1:pattern_length) {
            pattern_decimal <- pattern_decimal * 2 + pattern[j]
          }

          # Increment count
          pattern_counts[pattern_decimal + 1] <- pattern_counts[pattern_decimal + 1] + 1
        }
      }

      return(pattern_counts)
    }

    # Count patterns of length m
    # Use vectorized version if available
    if (exists("count_patterns_vectorized")) {
      C_m <- count_patterns_vectorized(bit_sequence, m, circular = TRUE)
      C_m_plus_1 <- count_patterns_vectorized(bit_sequence, m + 1, circular = TRUE)
    } else {
      C_m <- count_patterns(bit_sequence, m)
      C_m_plus_1 <- count_patterns(bit_sequence, m + 1)
    }

    # Calculate phi(m)
    # phi(m) = (1/n) * sum(pi * log(pi)) where pi = Ci/n
    phi_m <- 0
    for (i in 1:length(C_m)) {
      if (C_m[i] > 0) {
        pi <- C_m[i] / n
        phi_m <- phi_m + pi * log(pi)
      }
    }

    # Calculate phi(m+1)
    phi_m_plus_1 <- 0
    for (i in 1:length(C_m_plus_1)) {
      if (C_m_plus_1[i] > 0) {
        pi <- C_m_plus_1[i] / n
        phi_m_plus_1 <- phi_m_plus_1 + pi * log(pi)
      }
    }

    # Calculate approximate entropy
    ApEn <- phi_m - phi_m_plus_1

    # Calculate chi-square statistic
    # Chi-square = 2 * n * (log(2) - ApEn)
    chi_square <- 2 * n * (log(2) - ApEn)

    # Calculate p-value
    # Degrees of freedom = 2^m
    df <- 2^m
    p_value <- 1 - pchisq(chi_square, df)

    list(
      p.value = p_value,
      statistic = chi_square,
      ApEn = ApEn,
      block_size = m,
      message = "Test completed successfully"
    )
  }

  # Run approximate entropy test
  approx_entropy_result <- approximate_entropy_test(bit_sequence, m = 2)

  suite$results$binary$approximate_entropy <- list(
    description = "Approximate Entropy Test (NIST)",
    result = if (is.na(approx_entropy_result$p.value)) {
      "INCONCLUSIVE"
    } else if (approx_entropy_result$p.value >= suite$config$significance_level) {
      "PASS"
    } else {
      "FAIL"
    },
    p_value = approx_entropy_result$p.value,
    statistic = approx_entropy_result$statistic,
    details = paste(
      approx_entropy_result$message,
      "| ApEn:", round(approx_entropy_result$ApEn, 6),
      "| Block size m:", approx_entropy_result$block_size
    )
  )

  # Generate visualizations
  if (require(ggplot2) && suite$config$save_visualizations) {
    suite <- visualize_binary_tests(suite, bit_matrix, bit_pos_result$proportions)
  }

  return(suite)
}
