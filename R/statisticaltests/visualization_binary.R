# File: visualization_binary.R
# ----------------------------------------------------------------------
#' Visualization functions for binary tests
#'
#' This file contains functions for visualizing the results of
#' binary and bitwise tests for PRNG quality evaluation.

#' Visualize binary tests
#'
#' @param suite The test suite object
#' @param bit_matrix The matrix of bit representations
#' @param bit_proportions The proportions of 1s at each bit position
#' @return Updated test suite with visualization paths
#' @keywords internal
visualize_binary_tests <- function(suite, bit_matrix, bit_proportions) {
  # Set up output directory if it doesn't exist
  output_dir <- file.path(suite$config$output_dir, "visualizations", "binary")
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # 1. Bit frequency by position
  position_data <- data.frame(
    Position = 1:length(bit_proportions),
    Proportion = bit_proportions,
    Expected = 0.5
  )
  
  # Calculate critical values
  n <- nrow(bit_matrix)
  critical_value <- 0.5 + 1.96 * sqrt(0.25 / n)
  lower_critical <- 0.5 - 1.96 * sqrt(0.25 / n)
  upper_critical <- 0.5 + 1.96 * sqrt(0.25 / n)
  
  p1 <- ggplot2::ggplot(position_data, ggplot2::aes(x = Position, y = Proportion)) +
    ggplot2::geom_bar(stat = "identity", fill = "steelblue", width = 0.7) +
    ggplot2::geom_hline(yintercept = 0.5, color = "red", linetype = "dashed") +
    ggplot2::geom_hline(yintercept = c(lower_critical, upper_critical), 
                     color = "orange", linetype = "dotted") +
    ggplot2::scale_x_continuous(breaks = seq(0, length(bit_proportions), by = 4)) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "Bit Frequency by Position",
      subtitle = "Proportion of 1s at each bit position (with 95% confidence bounds)",
      x = "Bit Position",
      y = "Proportion of 1s"
    )
  
  # Save plot
  bit_freq_path <- file.path(output_dir, "bit_frequency_plot.png")
  ggplot2::ggsave(bit_freq_path, p1, width = 10, height = 6)
  
  # 2. Bit pattern heatmap
  # For visualization, use a sample of the data if it's too large
  max_rows <- 100
  sample_idx <- if (nrow(bit_matrix) > max_rows) {
    sample(1:nrow(bit_matrix), max_rows)
  } else {
    1:nrow(bit_matrix)
  }
  
  # Transform bit matrix into a format suitable for ggplot2
  bit_data <- reshape2::melt(bit_matrix[sample_idx, ])
  colnames(bit_data) <- c("Row", "Position", "Value")
  
  p2 <- ggplot2::ggplot(bit_data, ggplot2::aes(x = Position, y = Row, fill = factor(Value))) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_manual(values = c("white", "black"), labels = c("0", "1")) +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid = ggplot2::element_blank()) +
    ggplot2::labs(
      title = "Bit Pattern Visualization",
      subtitle = paste("Sample of", length(sample_idx), "random numbers (black = 1, white = 0)"),
      x = "Bit Position",
      y = "Random Number Index",
      fill = "Bit Value"
    )
  
  # Save plot
  bit_pattern_path <- file.path(output_dir, "bit_pattern_plot.png")
  ggplot2::ggsave(bit_pattern_path, p2, width = 10, height = 8)
  
  # 3. Runs of bits visualization
  # Extract a sample of consecutive bits for visualization
  bit_sequence <- as.vector(t(bit_matrix[1:min(10, nrow(bit_matrix)), ]))
  n_bits <- length(bit_sequence)
  max_bits_to_show <- 200
  
  if (n_bits > max_bits_to_show) {
    bit_sequence <- bit_sequence[1:max_bits_to_show]
    n_bits <- max_bits_to_show
  }
  
  # Prepare data for runs visualization
  runs_data <- data.frame(
    Position = 1:n_bits,
    Value = bit_sequence
  )
  
  # Identify runs
  run_id <- cumsum(c(1, diff(bit_sequence) != 0))
  runs_data$Run <- run_id
  
  # Calculate run lengths
  run_lengths <- table(run_id)
  
  # Alternating colors for runs
  run_colors <- rep(c("steelblue", "orange"), length(run_lengths))
  
  p3 <- ggplot2::ggplot(runs_data, ggplot2::aes(x = Position, y = Value, fill = factor(Run))) +
    ggplot2::geom_bar(stat = "identity", width = 1) +
    ggplot2::scale_fill_manual(values = run_colors[1:length(unique(run_id))]) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::labs(
      title = "Runs of Bits",
      subtitle = paste("First", n_bits, "bits with alternating colors for runs"),
      x = "Bit Position",
      y = "Bit Value"
    )
  
  # Save plot
  bit_runs_path <- file.path(output_dir, "bit_runs_plot.png")
  ggplot2::ggsave(bit_runs_path, p3, width = 12, height = 4)
  
  # 4. Run length distribution
  # Calculate all run lengths in the full sequence
  bit_full_sequence <- as.vector(t(bit_matrix))
  run_boundaries <- c(0, which(diff(bit_full_sequence) != 0), length(bit_full_sequence))
  run_lengths_full <- diff(run_boundaries)
  
  # Distribution of run lengths
  max_run_length <- min(20, max(run_lengths_full))
  run_length_counts <- table(factor(pmin(run_lengths_full, max_run_length), 
                                   levels = 1:max_run_length))
  
  # Expected geometric distribution for run lengths
  # P(run length = k) = 2^(-k)
  expected_probs <- 0.5^(1:max_run_length)
  expected_probs <- expected_probs / sum(expected_probs)
  expected_counts <- length(run_lengths_full) * expected_probs
  
  run_length_data <- data.frame(
    Length = as.integer(names(run_length_counts)),
    Observed = as.numeric(run_length_counts),
    Expected = expected_counts[1:length(run_length_counts)]
  )
  
  # Convert to long format for grouped bar chart
  run_length_data_long <- reshape2::melt(run_length_data, id.vars = "Length", 
                                      variable.name = "Type", 
                                      value.name = "Count")
  
  p4 <- ggplot2::ggplot(run_length_data_long, 
                     ggplot2::aes(x = factor(Length), y = Count, fill = Type)) +
    ggplot2::geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
    ggplot2::scale_fill_manual(values = c("steelblue", "orange")) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5)) +
    ggplot2::labs(
      title = "Distribution of Run Lengths",
      subtitle = "Observed vs. Expected run length frequencies",
      x = "Run Length",
      y = "Frequency",
      fill = ""
    )
  
  # Save plot
  run_length_path <- file.path(output_dir, "run_length_distribution_plot.png")
  ggplot2::ggsave(run_length_path, p4, width = 10, height = 6)
  
  # 5. Poker test visualization (4-bit patterns)
  # Calculate frequencies of 4-bit patterns
  bits_per_value <- ncol(bit_matrix)
  k <- 4
  
  # Ensure k divides bits_per_value (handling remainders)
  blocks_per_value <- bits_per_value %/% k
  
  # Extract all k-bit blocks
  patterns <- list()
  for (i in 1:nrow(bit_matrix)) {
    for (j in 1:blocks_per_value) {
      start_idx <- (j - 1) * k + 1
      end_idx <- min(j * k, bits_per_value)
      block <- bit_matrix[i, start_idx:end_idx]
      
      # Convert block to decimal
      decimal <- sum(block * (2^(rev(seq_along(block) - 1))))
      patterns <- c(patterns, decimal)
    }
  }
  
  # Count pattern frequencies
  pattern_counts <- table(factor(unlist(patterns), levels = 0:(2^k - 1)))
  
  # Expected frequency for each pattern
  total_blocks <- length(unlist(patterns))
  expected <- rep(total_blocks / (2^k), 2^k)
  
  # Prepare data for visualization
  poker_data <- data.frame(
    Pattern = as.integer(names(pattern_counts)),
    Observed = as.numeric(pattern_counts),
    Expected = expected
  )
  
  # Convert to long format for grouped bar chart
  poker_data_long <- reshape2::melt(poker_data, id.vars = "Pattern", 
                                 variable.name = "Type", 
                                 value.name = "Count")
  
  p5 <- ggplot2::ggplot(poker_data_long, 
                     ggplot2::aes(x = factor(Pattern), y = Count, fill = Type)) +
    ggplot2::geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
    ggplot2::scale_fill_manual(values = c("steelblue", "orange")) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, size = 7)) +
    ggplot2::labs(
      title = "Poker Test (4-bit patterns)",
      subtitle = "Observed vs. Expected frequencies of 4-bit patterns",
      x = "Pattern (decimal)",
      y = "Frequency",
      fill = ""
    )
  
  # Save plot
  poker_path <- file.path(output_dir, "poker_test_plot.png")
  ggplot2::ggsave(poker_path, p5, width = 12, height = 6)
  
  # Store visualization paths in suite
  suite$visualizations$binary <- list(
    bit_frequency_plot = bit_freq_path,
    bit_pattern_plot = bit_pattern_path,
    bit_runs_plot = bit_runs_path,
    run_length_distribution_plot = run_length_path,
    poker_test_plot = poker_path
  )
  
  return(suite)
}
