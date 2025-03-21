# File: compression_tests.R
# ----------------------------------------------------------------------
#' Compression tests for PRNG quality
#'
#' This module provides tests that evaluate the compressibility
#' of PRNG output, which is an important measure of randomness.

#' Run compression tests
#'
#' @param suite The test suite object
#' @return Updated test suite with results
#' @export
run_compression_tests <- function(suite) {
  # Generate random numbers
  n <- suite$config$compression_sample_size
  x <- suite$prng_func(n)
  
  # Initialize results
  suite$results$compression <- list()
  
  # 1. Simple compression ratio test
  # For truly random data, compression should not reduce size significantly
  compression_ratio_test <- function(x) {
    # Convert to binary representation (8 bits per value)
    x_scaled <- as.integer(x * 255)
    x_raw <- as.raw(x_scaled)
    
    # Compress using different algorithms
    if (requireNamespace("memCompress", quietly = TRUE)) {
      # Use base R's memCompress
      comp_gzip <- memCompress(x_raw, "gzip")
      comp_bzip2 <- memCompress(x_raw, "bzip2")
      
      # Calculate compression ratios
      ratio_gzip <- length(comp_gzip) / length(x_raw)
      ratio_bzip2 <- length(comp_bzip2) / length(x_raw)
      
      # For random data, ratio should be close to 1
      # We'll use a simple test: is ratio > 0.95?
      p_value_gzip <- ifelse(ratio_gzip > 0.95, 1.0, 0.0)
      p_value_bzip2 <- ifelse(ratio_bzip2 > 0.95, 1.0, 0.0)
      
      list(
        p.value = min(p_value_gzip, p_value_bzip2),
        ratio_gzip = ratio_gzip,
        ratio_bzip2 = ratio_bzip2
      )
    } else {
      # Fall back to a simpler approach
      # Convert to character strings and use a basic RLE encoding
      x_str <- paste(x_scaled, collapse = ",")
      x_rle <- rle(x_scaled)
      rle_size <- length(x_rle$lengths) + length(x_rle$values)
      ratio_rle <- rle_size / length(x_scaled)
      
      # For random data, RLE should not compress much
      p_value_rle <- ifelse(ratio_rle > 0.9, 1.0, 0.0)
      
      list(
        p.value = p_value_rle,
        ratio_rle = ratio_rle
      )
    }
  }
  
  # Run compression ratio test
  comp_result <- compression_ratio_test(x)
  suite$results$compression$compression_ratio <- list(
    description = "Compression Ratio Test",
    result = ifelse(comp_result$p.value >= suite$config$significance_level, "PASS", "FAIL"),
    p_value = comp_result$p.value,
    details = if (requireNamespace("memCompress", quietly = TRUE)) {
      paste("Tests if compression ratio is close to 1.",
            "GZIP ratio:", round(comp_result$ratio_gzip, 3),
            "BZIP2 ratio:", round(comp_result$ratio_bzip2, 3))
    } else {
      paste("Tests if RLE compression ratio is close to 1.",
            "RLE ratio:", round(comp_result$ratio_rle, 3))
    }
  )
  
  # 2. Entropy test
  entropy_test <- function(x) {
    # Discretize to 8-bit values
    x_scaled <- as.integer(x * 255)
    
    # Calculate entropy
    bin_counts <- table(factor(x_scaled, levels = 0:255))
    probs <- bin_counts / sum(bin_counts)
    entropy <- -sum(probs * log2(probs), na.rm = TRUE)
    
    # Maximum entropy for 8-bit values is 8
    # For good randomness, should be close to 8
    # We'll say it passes if entropy > 7.9
    max_entropy <- 8
    p_value <- ifelse(entropy > 7.9, 1.0, 0.0)
    
    list(
      p.value = p_value,
      entropy = entropy,
      max_entropy = max_entropy,
      ratio = entropy / max_entropy
    )
  }
  
  # Run entropy test
  entropy_result <- entropy_test(x)
  suite$results$compression$entropy <- list(
    description = "Entropy Test",
    result = ifelse(entropy_result$p.value >= suite$config$significance_level, "PASS", "FAIL"),
    p_value = entropy_result$p.value,
    details = paste("Tests if entropy is close to maximum.",
                   "Entropy:", round(entropy_result$entropy, 3),
                   "/ Max:", entropy_result$max_entropy,
                   "Ratio:", round(entropy_result$ratio, 4))
  )
  
  # 3. Chi-square test of byte frequencies
  chi_square_bytes_test <- function(x) {
    # Discretize to 8-bit values
    x_scaled <- as.integer(x * 255)
    
    # Count occurrences of each byte value
    byte_counts <- table(factor(x_scaled, levels = 0:255))
    
    # Expected count for each byte value
    expected <- length(x) / 256
    
    # Calculate chi-square statistic
    chi_square <- sum((byte_counts - expected)^2 / expected)
    
    # Degrees of freedom = 256 - 1 = 255
    df <- 255
    
    # P-value from chi-square distribution
    p_value <- 1 - pchisq(chi_square, df)
    
    list(
      p.value = p_value,
      statistic = chi_square,
      df = df
    )
  }
  
  # Run chi-square test for byte frequencies
  chi_bytes_result <- chi_square_bytes_test(x)
  suite$results$compression$chi_square_bytes <- list(
    description = "Chi-Square Test for Byte Frequencies",
    result = ifelse(chi_bytes_result$p.value >= suite$config$significance_level, "PASS", "FAIL"),
    p_value = chi_bytes_result$p.value,
    statistic = chi_bytes_result$statistic,
    details = paste("Tests distribution of byte values.",
                   "Chi-square:", round(chi_bytes_result$statistic, 2),
                   "with", chi_bytes_result$df, "degrees of freedom")
  )
  
  # Generate visualizations
  if (require(ggplot2) && suite$config$save_visualizations) {
    suite <- visualize_compression_tests(suite, x)
  }
  
  return(suite)
}

#' Visualize compression tests
#'
#' @param suite The test suite object
#' @param x The generated random numbers
#' @return Updated test suite with visualization paths
#' @keywords internal
visualize_compression_tests <- function(suite, x) {
  # To avoid R CMD check NOTEs
  Value <- Frequency <- Byte <- Count <- Probability <- NULL
  
  # Set up output directory if it doesn't exist
  output_dir <- file.path(suite$config$output_dir, "visualizations", "compression")
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # 1. Byte frequency distribution
  # Discretize to 8-bit values
  x_scaled <- as.integer(x * 255)
  
  # Count occurrences of each byte value
  byte_counts <- table(factor(x_scaled, levels = 0:255))
  
  # Create data frame for plotting
  byte_data <- data.frame(
    Byte = as.integer(names(byte_counts)),
    Count = as.numeric(byte_counts)
  )
  
  # Expected count
  expected <- length(x) / 256
  
  # Create plot
  p1 <- ggplot2::ggplot(byte_data, ggplot2::aes(x = Byte, y = Count)) +
    ggplot2::geom_bar(stat = "identity", fill = "steelblue", alpha = 0.7) +
    ggplot2::geom_hline(yintercept = expected, color = "red", linetype = "dashed") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)) +
    ggplot2::labs(
      title = "Byte Frequency Distribution",
      subtitle = "Distribution of byte values (expected value in red)",
      x = "Byte Value",
      y = "Frequency"
    )
  
  # Save plot
  byte_freq_path <- file.path(output_dir, "byte_frequency_plot.png")
  ggplot2::ggsave(byte_freq_path, p1, width = 10, height = 6)
  
  # 2. Entropy calculation by chunk
  # Divide sequence into chunks and calculate entropy for each
  chunk_size <- 1000
  num_chunks <- floor(length(x) / chunk_size)
  
  chunk_entropies <- numeric(num_chunks)
  for (i in seq_len(num_chunks)) {
    start_idx <- (i - 1) * chunk_size + 1
    end_idx <- i * chunk_size
    chunk <- x[start_idx:end_idx]
    
    # Discretize
    chunk_scaled <- as.integer(chunk * 255)
    
    # Calculate entropy
    bin_counts <- table(factor(chunk_scaled, levels = 0:255))
    probs <- bin_counts / sum(bin_counts)
    chunk_entropies[i] <- -sum(probs * log2(probs), na.rm = TRUE)
  }
  
  # Create data frame for plotting
  entropy_data <- data.frame(
    Chunk = seq_len(num_chunks),
    Entropy = chunk_entropies
  )
  
  # Create plot
  p2 <- ggplot2::ggplot(entropy_data, ggplot2::aes(x = Chunk, y = Entropy)) +
    ggplot2::geom_line(color = "steelblue") +
    ggplot2::geom_hline(yintercept = 8, color = "red", linetype = "dashed") +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "Entropy by Chunk",
      subtitle = "Entropy of each 1000-value chunk (max entropy = 8)",
      x = "Chunk Number",
      y = "Entropy"
    )
  
  # Save plot
  entropy_path <- file.path(output_dir, "entropy_plot.png")
  ggplot2::ggsave(entropy_path, p2, width = 10, height = 6)
  
  # 3. Compression ratio by chunk
  chunk_comp_ratios <- numeric(num_chunks)
  for (i in seq_len(num_chunks)) {
    start_idx <- (i - 1) * chunk_size + 1
    end_idx <- i * chunk_size
    chunk <- x[start_idx:end_idx]
    
    # Convert to string
    chunk_str <- paste(as.integer(chunk * 255), collapse = ",")
    
    # Compress using base R's memCompress or RLE
    if (requireNamespace("memCompress", quietly = TRUE)) {
      chunk_raw <- as.raw(as.integer(chunk * 255))
      comp_chunk <- memCompress(chunk_raw, "gzip")
      chunk_comp_ratios[i] <- length(comp_chunk) / length(chunk_raw)
    } else {
      chunk_scaled <- as.integer(chunk * 255)
      chunk_rle <- rle(chunk_scaled)
      rle_size <- length(chunk_rle$lengths) + length(chunk_rle$values)
      chunk_comp_ratios[i] <- rle_size / length(chunk_scaled)
    }
  }
  
  # Create data frame for plotting
  comp_data <- data.frame(
    Chunk = seq_len(num_chunks),
    CompressionRatio = chunk_comp_ratios
  )
  
  # Create plot
  p3 <- ggplot2::ggplot(comp_data, ggplot2::aes(x = Chunk, y = CompressionRatio)) +
    ggplot2::geom_line(color = "steelblue") +
    ggplot2::geom_hline(yintercept = 1.0, color = "red", linetype = "dashed") +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "Compression Ratio by Chunk",
      subtitle = if (requireNamespace("memCompress", quietly = TRUE)) {
        "GZIP compression ratio of each 1000-value chunk"
      } else {
        "RLE compression ratio of each 1000-value chunk"
      },
      x = "Chunk Number",
      y = "Compression Ratio"
    )
  
  # Save plot
  comp_ratio_path <- file.path(output_dir, "compression_ratio_plot.png")
  ggplot2::ggsave(comp_ratio_path, p3, width = 10, height = 6)
  
  # Store visualization paths in suite
  suite$visualizations$compression <- list(
    byte_frequency_plot = byte_freq_path,
    entropy_plot = entropy_path,
    compression_ratio_plot = comp_ratio_path
  )
  
  return(suite)
}
