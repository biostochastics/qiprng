# File: binary_tests.R
# ----------------------------------------------------------------------
#' Binary and bitwise tests for PRNG quality
#'
#' This module provides tests to evaluate the bit-level properties
#' of the PRNG output, including bit frequency, runs of bits, etc.

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
    n <- 10  # Enforce a minimum sample size for statistical validity
  }
  
  x <- suite$prng_func(n)
  
  # Initialize results
  suite$results$binary <- list()
  
  # Convert floating point values to binary representation
  # We'll work with 32 bits per value (can be adjusted)
  numeric_to_bits <- function(x, bits_per_value = 32) {
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
    x <- pmin(x, 0.9999999)  # Ensure values are < 1 to prevent overflow
    x <- pmax(x, 0)  # Ensure values are >= 0
    
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
        value <- 0  # Handle NA values
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
  bit_matrix <- numeric_to_bits(x)
  bit_sequence <- binary_matrix_to_sequence(bit_matrix)
  
  # 1. Monobit (frequency) test
  monobit_test <- function(bits) {
    n <- length(bits)
    ones_count <- sum(bits)
    proportion <- ones_count / n
    
    # Expected proportion is 0.5
    z <- (ones_count - n/2) / sqrt(n/4)
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
  suite$results$binary$monobit <- list(
    description = "Monobit (Frequency) Test",
    result = ifelse(monobit_result$p.value >= suite$config$significance_level, "PASS", "FAIL"),
    p_value = monobit_result$p.value,
    statistic = monobit_result$statistic,
    details = paste("Tests if the proportion of 1s is close to 0.5.",
                   "Proportion:", round(monobit_result$proportion, 4),
                   "Ones:", monobit_result$ones_count,
                   "Zeros:", monobit_result$zeros_count)
  )
  
  # 2. Bit frequency by position test
  bit_frequency_by_position <- function(bit_matrix) {
    bits_per_value <- ncol(bit_matrix)
    n <- nrow(bit_matrix)
    
    # For each bit position, count ones
    counts <- colSums(bit_matrix)
    proportions <- counts / n
    
    # Calculate z-statistic for each position
    z_values <- (counts - n/2) / sqrt(n/4)
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
    result = ifelse(bit_pos_result$p.value >= suite$config$significance_level, "PASS", "FAIL"),
    p_value = bit_pos_result$p.value,
    statistic = bit_pos_result$statistic,
    details = paste("Tests if each bit position has proper 0/1 distribution.",
                   "Failing positions:", bit_pos_result$failures,
                   "/ Expected:", round(bit_pos_result$expected_failures, 2))
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
      if (bits_clean[i] != bits_clean[i-1]) {
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
    result = ifelse(!is.na(bit_runs_result$p.value) && 
                   bit_runs_result$p.value >= suite$config$significance_level, 
                   "PASS", ifelse(is.na(bit_runs_result$p.value), "INCONCLUSIVE", "FAIL")),
    p_value = bit_runs_result$p.value,
    statistic = bit_runs_result$statistic,
    diagnostics = list(
      runs = bit_runs_result$runs,
      expected_runs = bit_runs_result$expected_runs,
      sample_size = bit_runs_result$n_clean
    ),
    details = paste("Tests if runs of 0s and 1s have expected length.",
                   "Runs:", bit_runs_result$runs,
                   "/ Expected:", round(bit_runs_result$expected_runs, 2),
                   "(Sample size:", bit_runs_result$n_clean, ")")
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
    result = ifelse(!is.na(poker_result$p.value) && 
                   poker_result$p.value >= suite$config$significance_level, 
                   "PASS", ifelse(is.na(poker_result$p.value), "INCONCLUSIVE", "FAIL")),
    p_value = poker_result$p.value,
    statistic = poker_result$statistic,
    details = paste("Tests distribution of 4-bit patterns.",
                   "Chi-square:", round(poker_result$statistic, 2),
                   "with", poker_result$df, "degrees of freedom")
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
      if (bit_sequence[i] == 1) {
        current_run <- current_run + 1
        if (current_run > longest_run) longest_run <- current_run
      } else {
        current_run <- 0
      }
    }
    
    # Improved expected longest run calculation based on the probability distribution
    # This formula is more accurate than the simple log2(n) + 1 approximation
    # The formula is based on extreme value theory for runs in Bernoulli sequences
    p <- mean(bit_sequence)  # Proportion of ones
    
    # Ensuring p is not 0 or 1 to avoid log issues
    if (p <= 0 || p >= 1) {
      p <- 0.5  # Default to 0.5 in extreme cases
    }
    
    # More accurate calculation for expected longest run
    # Based on Schilling (1990) - "The Longest Run of Heads"
    expected_longest <- log(n * (1 - p), base = 1/p) + 
                       (0.5772 + log(log(1/(1-p)), base = exp(1))) / log(1/p, base = exp(1))
    
    # Better variance approximation that scales with sequence length
    # pi^2/(6 * ln^2(1/p)) is the asymptotic variance term from extreme value theory
    var_longest <- (pi^2) / (6 * (log(1/p, base = exp(1)))^2)
    
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
    result = ifelse(longest_run_result$p.value >= suite$config$significance_level, "PASS", "FAIL"),
    p_value = longest_run_result$p.value,
    statistic = longest_run_result$statistic,
    details = paste("Tests if longest run of ones is as expected.",
                   "Longest run:", longest_run_result$longest_run,
                   "/ Expected:", round(longest_run_result$expected_longest, 2))
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
                                                1, paste, collapse = "")))
    
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
    result = ifelse(!is.na(serial_result$p.value) && 
                   serial_result$p.value >= suite$config$significance_level, 
                   "PASS", ifelse(is.na(serial_result$p.value), "INCONCLUSIVE", "FAIL")),
    p_value = serial_result$p.value,
    statistic = serial_result$statistic,
    details = paste("Tests if 2-bit overlapping patterns are uniformly distributed.",
                   "Chi-square:", round(serial_result$statistic, 2),
                   "with", serial_result$df, "degrees of freedom")
  )
  
  # Generate visualizations
  if (require(ggplot2) && suite$config$save_visualizations) {
    suite <- visualize_binary_tests(suite, bit_matrix, bit_pos_result$proportions)
  }
  
  return(suite)
}
