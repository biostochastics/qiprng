# File: vectorized_operations.R
# ----------------------------------------------------------------------
#' Vectorized bit operations for improved performance
#'
#' This module provides vectorized implementations of bit manipulation
#' operations to improve performance of binary tests.

#' Vectorized numeric to bits conversion
#'
#' Converts floating point values to binary representation using vectorized operations.
#' This is significantly faster than the loop-based approach for large datasets.
#'
#' @param x Numeric vector of values in [0,1]
#' @param bits_per_value Number of bits per value (default: 64)
#' @return Binary matrix with dimensions length(x) by bits_per_value
#' @export
numeric_to_bits_vectorized <- function(x, bits_per_value = 64) {
  # Handle edge cases
  if (is.null(x) || length(x) == 0) {
    return(matrix(0, nrow = 0, ncol = bits_per_value))
  }
  
  # Remove NA values
  na_mask <- !is.na(x)
  x_clean <- x[na_mask]
  n <- length(x_clean)
  
  if (n == 0) {
    return(matrix(0, nrow = 0, ncol = bits_per_value))
  }
  
  # Ensure values are in [0, 1)
  x_clean <- pmin(pmax(x_clean, 0), 0.9999999)
  
  # Scale to integers
  max_val <- 2^bits_per_value - 1
  scaled <- as.integer(x_clean * max_val)
  
  # Vectorized bit extraction using bitwise operations
  # Create power of 2 matrix for bit positions
  powers <- 2^(0:(bits_per_value - 1))
  powers_matrix <- matrix(rep(powers, each = n), nrow = n)
  
  # Replicate scaled values for each bit position
  scaled_matrix <- matrix(rep(scaled, bits_per_value), nrow = n)
  
  # Extract bits using bitwise AND and division
  # This is equivalent to (scaled >> i) & 1 for each bit position
  bit_matrix <- ((scaled_matrix %/% powers_matrix) %% 2)
  
  # Reverse columns to match original order (MSB first)
  bit_matrix <- bit_matrix[, bits_per_value:1, drop = FALSE]
  
  # If we had NA values, expand the result
  if (sum(!na_mask) > 0) {
    full_matrix <- matrix(0, nrow = length(x), ncol = bits_per_value)
    full_matrix[na_mask, ] <- bit_matrix
    return(full_matrix)
  }
  
  return(bit_matrix)
}

#' Vectorized pattern counting for block-based tests
#'
#' Counts occurrences of bit patterns efficiently using vectorized operations.
#'
#' @param bit_sequence Binary sequence (vector of 0s and 1s)
#' @param pattern_length Length of patterns to count
#' @param circular Whether to use circular extension (for approximate entropy)
#' @return Vector of pattern counts indexed by pattern decimal value
#' @export
count_patterns_vectorized <- function(bit_sequence, pattern_length, circular = FALSE) {
  n <- length(bit_sequence)
  
  if (n < pattern_length) {
    return(rep(0, 2^pattern_length))
  }
  
  # Prepare sequence
  if (circular) {
    # Circular extension for approximate entropy
    extended_seq <- c(bit_sequence, bit_sequence[1:(pattern_length - 1)])
    num_patterns <- n
  } else {
    extended_seq <- bit_sequence
    num_patterns <- n - pattern_length + 1
  }
  
  # Create matrix of sliding windows
  # Each row is a pattern
  pattern_matrix <- matrix(0, nrow = num_patterns, ncol = pattern_length)
  for (i in 1:pattern_length) {
    pattern_matrix[, i] <- extended_seq[i:(i + num_patterns - 1)]
  }
  
  # Convert patterns to decimal values using matrix multiplication
  powers <- 2^((pattern_length - 1):0)
  pattern_decimals <- pattern_matrix %*% powers
  
  # Count occurrences using tabulate (faster than table for integers)
  # tabulate expects 1-based indexing
  counts <- tabulate(pattern_decimals + 1, nbins = 2^pattern_length)
  
  return(counts)
}

#' Vectorized run length calculation
#'
#' Calculates run lengths efficiently using vectorized operations.
#'
#' @param bit_sequence Binary sequence
#' @return List with run counts and statistics
#' @export
calculate_runs_vectorized <- function(bit_sequence) {
  n <- length(bit_sequence)
  
  if (n <= 1) {
    return(list(
      total_runs = n,
      run_lengths = n,
      ones_runs = sum(bit_sequence),
      zeros_runs = n - sum(bit_sequence)
    ))
  }
  
  # Find run boundaries using diff
  # diff will be non-zero where bits change
  transitions <- c(1, which(diff(bit_sequence) != 0) + 1, n + 1)
  
  # Calculate run lengths
  run_lengths <- diff(transitions)
  
  # Get run values (0 or 1)
  run_values <- bit_sequence[transitions[-length(transitions)]]
  
  # Count runs by type
  ones_runs <- sum(run_values == 1)
  zeros_runs <- sum(run_values == 0)
  
  return(list(
    total_runs = length(run_lengths),
    run_lengths = run_lengths,
    ones_runs = ones_runs,
    zeros_runs = zeros_runs,
    run_values = run_values
  ))
}

#' Vectorized cumulative sum for random walk analysis
#'
#' Computes cumulative sums efficiently for random walk tests.
#'
#' @param bit_sequence Binary sequence
#' @return Cumulative sum sequence with 0->-1, 1->1 mapping
#' @export
cumsum_binary_vectorized <- function(bit_sequence) {
  # Convert 0s to -1s, keep 1s as 1s
  walk_sequence <- 2 * bit_sequence - 1
  
  # Compute cumulative sum including initial 0
  c(0, cumsum(walk_sequence))
}

#' Vectorized bit block extraction
#'
#' Extracts non-overlapping blocks of bits efficiently.
#'
#' @param bit_sequence Binary sequence
#' @param block_size Size of each block
#' @return Matrix where each row is a block
#' @export
extract_blocks_vectorized <- function(bit_sequence, block_size) {
  n <- length(bit_sequence)
  num_blocks <- n %/% block_size
  
  if (num_blocks == 0) {
    return(matrix(0, nrow = 0, ncol = block_size))
  }
  
  # Trim sequence to exact multiple of block_size
  trimmed_length <- num_blocks * block_size
  trimmed_seq <- bit_sequence[1:trimmed_length]
  
  # Reshape into matrix
  block_matrix <- matrix(trimmed_seq, nrow = num_blocks, ncol = block_size, byrow = TRUE)
  
  return(block_matrix)
}

#' Fast bit frequency calculation by position
#'
#' Calculates bit frequencies at each position using vectorized operations.
#'
#' @param bit_matrix Binary matrix from numeric_to_bits
#' @return Vector of frequencies for each bit position
#' @export
bit_frequency_by_position_vectorized <- function(bit_matrix) {
  # Simply use colMeans for proportion of 1s
  colMeans(bit_matrix, na.rm = TRUE)
}

#' Optimized Berlekamp-Massey algorithm
#'
#' Implements the Berlekamp-Massey algorithm with performance optimizations.
#' While the core algorithm remains iterative, this version minimizes allocations.
#'
#' @param s Binary sequence
#' @return Linear complexity of the sequence
#' @export
berlekamp_massey_optimized <- function(s) {
  n <- length(s)
  
  # Pre-allocate vectors
  C <- integer(n)
  B <- integer(n)
  C[1] <- 1
  B[1] <- 1
  
  L <- 0L
  m <- -1L
  N <- 0L
  
  while (N < n) {
    # Calculate discrepancy
    d <- s[N + 1]
    
    if (L > 0) {
      # Vectorized calculation of discrepancy
      indices <- seq_len(L)
      valid_indices <- (N - indices + 1) > 0
      if (any(valid_indices)) {
        d <- (d + sum(C[indices[valid_indices] + 1] * 
                     s[N - indices[valid_indices] + 1])) %% 2
      }
    }
    
    if (d == 1) {
      # Store old C
      T <- C
      
      # Update C efficiently
      if (N - m >= 0) {
        update_length <- min(n, N - m + n) - (N - m)
        if (update_length > 0) {
          indices <- seq_len(update_length)
          C[(N - m + 1):(N - m + update_length)] <- 
            (C[(N - m + 1):(N - m + update_length)] + B[indices]) %% 2
        }
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