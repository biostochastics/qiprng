# File: compression_tests_bootstrap.R
# ----------------------------------------------------------------------
#' Bootstrap-based compression tests for PRNG quality
#'
#' This module provides enhanced compression tests using bootstrap methods
#' for proper statistical p-value calculations instead of binary thresholds.
#'
#' The tests evaluate the compressibility of PRNG output, which is an
#' important measure of randomness. Random data should not compress well.

# Source bootstrap framework if not already loaded
if (!exists("bootstrap_p_value")) {
  bootstrap_framework_path <- system.file("R", "bootstrap_framework.R", package = "qiprng")
  if (file.exists(bootstrap_framework_path)) {
    source(bootstrap_framework_path)
  } else if (file.exists("R/bootstrap_framework.R")) {
    source("R/bootstrap_framework.R")
  }
}

#' Run bootstrap-based compression tests
#'
#' Executes compression tests using bootstrap methods for p-value calculation,
#' kernel density estimation for modeling compression ratio distributions,
#' and support for multiple compression algorithms.
#'
#' @param suite The test suite object containing configuration and PRNG function
#' @param use_legacy Logical; if TRUE, use legacy binary threshold logic (default: FALSE)
#' @return Updated test suite with compression test results
#' @export
run_compression_tests_bootstrap <- function(suite, use_legacy = FALSE) {
  # Fall back to legacy implementation if requested
  if (use_legacy) {
    return(run_compression_tests(suite))
  }

  # Generate random numbers
  n <- suite$config$compression_sample_size
  x <- suite$prng_func(n)

  # Initialize results
  suite$results$compression <- list()

  # Get compression algorithm configuration
  algorithms <- suite$config$compression_algorithms
  if (is.null(algorithms)) {
    # Default algorithms
    algorithms <- c("gzip", "bzip2")
    if (!requireNamespace("memCompress", quietly = TRUE)) {
      algorithms <- c("rle") # Fallback
    }
  }

  # 1. Bootstrap-based compression ratio test
  compression_ratio_test_bootstrap <- function(x, algorithm = "gzip") {
    # Convert to binary representation (8 bits per value)
    x_scaled <- as.integer(x * 255)
    x_raw <- as.raw(x_scaled)

    # Define test statistic: compression ratio
    compress_stat <- function(data_raw) {
      if (algorithm %in% c("gzip", "bzip2", "xz")) {
        if (exists("memCompress")) {
          compressed <- memCompress(data_raw, algorithm)
          ratio <- length(compressed) / length(data_raw)
        } else {
          stop(paste("memCompress not available for algorithm:", algorithm))
        }
      } else if (algorithm == "rle") {
        # RLE fallback - convert raw to integer if needed
        if (is.raw(data_raw)) {
          data_int <- as.integer(data_raw)
        } else {
          data_int <- as.integer(data_raw)
        }
        rle_result <- rle(data_int)
        rle_size <- length(rle_result$lengths) + length(rle_result$values)
        ratio <- rle_size / length(data_int)
      } else {
        stop(paste("Unsupported algorithm:", algorithm))
      }
      return(ratio)
    }

    # Calculate observed compression ratio
    observed_ratio <- compress_stat(x_raw)

    # Bootstrap to get null distribution of compression ratios
    # Under null hypothesis: data is truly random
    # For RLE, we need numeric data, not raw
    if (algorithm == "rle") {
      bootstrap_data <- x_scaled
    } else {
      bootstrap_data <- x_raw
    }

    # Adapt compress_stat for bootstrap
    compress_stat_bootstrap <- function(data) {
      if (algorithm == "rle") {
        # For RLE, data is already integer
        rle_result <- rle(as.integer(data))
        rle_size <- length(rle_result$lengths) + length(rle_result$values)
        ratio <- rle_size / length(data)
      } else {
        # For other algorithms, convert to raw
        if (!is.raw(data)) {
          data <- as.raw(as.integer(data))
        }
        ratio <- compress_stat(data)
      }
      return(ratio)
    }

    bootstrap_result <- bootstrap_p_value(
      data = bootstrap_data,
      test_statistic = compress_stat_bootstrap,
      observed_stat = observed_ratio,
      n_bootstraps = suite$config$bootstrap_samples %||% 5000,
      alternative = "less", # Random data should have high ratio (less compressible)
      progress = suite$config$show_progress %||% FALSE
    )

    # Add kernel density estimation for distribution modeling
    if (requireNamespace("stats", quietly = TRUE)) {
      kde <- density(bootstrap_result$bootstrap_stats,
        kernel = "gaussian",
        bw = "SJ"
      ) # Sheather-Jones bandwidth selection

      # Calculate p-value using KDE
      kde_p_value <- mean(kde$y[kde$x <= observed_ratio]) / mean(kde$y)
      kde_p_value <- max(0, min(1, kde_p_value)) # Ensure in [0,1]
    } else {
      kde <- NULL
      kde_p_value <- bootstrap_result$p.value
    }

    list(
      p.value = bootstrap_result$p.value,
      p.value_kde = kde_p_value,
      observed_ratio = observed_ratio,
      bootstrap_stats = bootstrap_result$bootstrap_stats,
      kde = kde,
      algorithm = algorithm
    )
  }

  # Run compression tests for each algorithm
  compression_results <- list()
  for (algo in algorithms) {
    tryCatch(
      {
        result <- compression_ratio_test_bootstrap(x, algorithm = algo)
        compression_results[[algo]] <- result

        # Store individual algorithm results
        suite$results$compression[[paste0("compression_ratio_", algo)]] <- list(
          description = paste("Bootstrap Compression Ratio Test -", toupper(algo)),
          result = if (is.na(result$p.value)) {
            "INCONCLUSIVE"
          } else if (result$p.value >= suite$config$significance_level) {
            "PASS"
          } else {
            "FAIL"
          },
          p_value = result$p.value,
          p_value_kde = result$p.value_kde,
          observed_ratio = result$observed_ratio,
          statistic = result$observed_ratio,
          details = paste(
            "Bootstrap-based test of compression ratio.",
            "Observed ratio:", round(result$observed_ratio, 4),
            "| Bootstrap p-value:", round(result$p.value, 4),
            "| KDE p-value:", round(result$p.value_kde, 4)
          )
        )
      },
      error = function(e) {
        warning(paste("Error testing algorithm", algo, ":", e$message))
      }
    )
  }

  # Combined p-value using Fisher's method
  if (length(compression_results) > 1) {
    p_values <- sapply(compression_results, function(r) r$p.value)
    # Fisher's method: -2 * sum(log(p_values)) ~ chi-square with 2k df
    fisher_stat <- -2 * sum(log(p_values))
    combined_p <- pchisq(fisher_stat, df = 2 * length(p_values), lower.tail = FALSE)

    suite$results$compression$compression_ratio_combined <- list(
      description = "Combined Compression Test (Fisher's Method)",
      result = if (is.na(combined_p)) {
        "INCONCLUSIVE"
      } else if (combined_p >= suite$config$significance_level) {
        "PASS"
      } else {
        "FAIL"
      },
      p_value = combined_p,
      statistic = fisher_stat,
      details = paste(
        "Combined test across", length(algorithms), "algorithms.",
        "Fisher statistic:", round(fisher_stat, 2),
        "Combined p-value:", round(combined_p, 4)
      )
    )
  }

  # 2. Bootstrap-based entropy test
  entropy_test_bootstrap <- function(x) {
    # Discretize to 8-bit values
    x_scaled <- as.integer(x * 255)

    # Define test statistic: deviation from maximum entropy
    entropy_stat <- function(data_int) {
      # Calculate entropy
      bin_counts <- table(factor(data_int, levels = 0:255))
      probs <- bin_counts / sum(bin_counts)
      probs <- probs[probs > 0] # Remove zero probabilities
      entropy <- -sum(probs * log2(probs))

      # Return deviation from maximum entropy
      max_entropy <- log2(256) # 8 bits
      deviation <- abs(entropy - max_entropy)
      return(deviation)
    }

    # Calculate observed deviation
    observed_deviation <- entropy_stat(x_scaled)

    # Bootstrap for null distribution
    bootstrap_result <- bootstrap_p_value(
      data = x_scaled,
      test_statistic = entropy_stat,
      observed_stat = observed_deviation,
      n_bootstraps = suite$config$bootstrap_samples %||% 5000,
      alternative = "greater", # Large deviation is bad
      progress = suite$config$show_progress %||% TRUE
    )

    # Calculate actual entropy for reporting
    bin_counts <- table(factor(x_scaled, levels = 0:255))
    probs <- bin_counts / sum(bin_counts)
    probs <- probs[probs > 0]
    actual_entropy <- -sum(probs * log2(probs))

    list(
      p.value = bootstrap_result$p.value,
      entropy = actual_entropy,
      max_entropy = log2(256),
      deviation = observed_deviation,
      bootstrap_stats = bootstrap_result$bootstrap_stats
    )
  }

  # Run entropy test
  entropy_result <- entropy_test_bootstrap(x)
  suite$results$compression$entropy_bootstrap <- list(
    description = "Bootstrap Entropy Test",
    result = if (is.na(entropy_result$p.value)) {
      "INCONCLUSIVE"
    } else if (entropy_result$p.value >= suite$config$significance_level) {
      "PASS"
    } else {
      "FAIL"
    },
    p_value = entropy_result$p.value,
    statistic = entropy_result$deviation,
    entropy = entropy_result$entropy,
    max_entropy = entropy_result$max_entropy,
    deviation = entropy_result$deviation,
    details = paste(
      "Bootstrap test of entropy deviation from maximum.",
      "Entropy:", round(entropy_result$entropy, 4),
      "/ Max:", round(entropy_result$max_entropy, 4),
      "| Deviation:", round(entropy_result$deviation, 6),
      "| p-value:", round(entropy_result$p.value, 4)
    )
  )

  # 3. Permutation test for byte frequency uniformity
  byte_frequency_permutation_test <- function(x) {
    # Discretize to 8-bit values
    x_scaled <- as.integer(x * 255)

    # Test statistic: chi-square for uniformity
    chi_square_stat <- function(data_int) {
      byte_counts <- table(factor(data_int, levels = 0:255))
      expected <- length(data_int) / 256
      chi_square <- sum((byte_counts - expected)^2 / expected)
      return(chi_square)
    }

    # For permutation test, we need to define what we're permuting
    # We'll use Monte Carlo approach instead
    observed_chi <- chi_square_stat(x_scaled)

    # Monte Carlo p-value under null hypothesis of uniformity
    null_simulator <- function(n) {
      # Generate uniform random bytes
      sample(0:255, size = n, replace = TRUE)
    }

    mc_result <- monte_carlo_p_value(
      observed_stat = observed_chi,
      null_simulator = null_simulator,
      test_statistic = chi_square_stat,
      n_simulations = suite$config$bootstrap_samples %||% 5000,
      alternative = "two.sided",
      progress = suite$config$show_progress %||% TRUE,
      n = length(x_scaled)
    )

    # Also calculate analytical p-value for comparison
    analytical_p <- pchisq(observed_chi, df = 255, lower.tail = FALSE)

    list(
      p.value = mc_result$p.value,
      p.value_analytical = analytical_p,
      statistic = observed_chi,
      simulated_stats = mc_result$simulated_stats
    )
  }

  # Run byte frequency test
  byte_freq_result <- byte_frequency_permutation_test(x)
  suite$results$compression$byte_frequency_monte_carlo <- list(
    description = "Monte Carlo Byte Frequency Test",
    result = if (is.na(byte_freq_result$p.value)) {
      "INCONCLUSIVE"
    } else if (byte_freq_result$p.value >= suite$config$significance_level) {
      "PASS"
    } else {
      "FAIL"
    },
    p_value = byte_freq_result$p.value,
    p_value_analytical = byte_freq_result$p.value_analytical,
    statistic = byte_freq_result$statistic,
    details = paste(
      "Monte Carlo test of byte frequency uniformity.",
      "Chi-square:", round(byte_freq_result$statistic, 2),
      "| MC p-value:", round(byte_freq_result$p.value, 4),
      "| Analytical p-value:", round(byte_freq_result$p.value_analytical, 4)
    )
  )

  # 4. Maurer's Universal Statistical Test
  maurers_universal_test <- function(x, L = 7, Q = 1280) {
    # Convert to bits
    x_scaled <- as.integer(x * 255)

    # Parameters:
    # L = block size (typically 7 or 8)
    # Q = initialization blocks (typically 10*2^L)

    n_blocks <- length(x_scaled)
    if (n_blocks < Q + 2) {
      warning("Not enough data for Maurer's test with given parameters")
      return(list(p.value = NA, statistic = NA))
    }

    # Create table to store positions
    table_size <- 2^L
    table <- rep(-1, table_size)

    # Initialize table with first Q blocks
    for (i in 1:Q) {
      if (i + L - 1 <= length(x_scaled)) {
        # Extract L-bit block
        block_start <- i
        block_end <- min(i + L - 1, length(x_scaled))
        block_bits <- x_scaled[block_start:block_end]

        # Convert to decimal
        block_value <- sum(block_bits %% 2^(1:length(block_bits)) * 2^(0:(length(block_bits) - 1)))
        block_index <- (block_value %% table_size) + 1

        table[block_index] <- i
      }
    }

    # Compute test statistic
    sum_log <- 0
    K <- 0

    for (i in (Q + 1):(n_blocks - L + 1)) {
      # Extract L-bit block
      block_start <- i
      block_end <- min(i + L - 1, length(x_scaled))
      block_bits <- x_scaled[block_start:block_end]

      # Convert to decimal
      block_value <- sum(block_bits %% 2^(1:length(block_bits)) * 2^(0:(length(block_bits) - 1)))
      block_index <- (block_value %% table_size) + 1

      # Calculate distance
      if (table[block_index] > 0) {
        distance <- i - table[block_index]
        sum_log <- sum_log + log2(distance)
        K <- K + 1
      }

      # Update table
      table[block_index] <- i
    }

    if (K == 0) {
      return(list(p.value = NA, statistic = NA))
    }

    # Calculate fn statistic
    fn <- sum_log / K

    # Expected value and variance for L=7 (from NIST SP 800-22)
    if (L == 7) {
      expected_value <- 6.1962507
      variance <- 3.125
    } else if (L == 8) {
      expected_value <- 7.1836656
      variance <- 3.238
    } else {
      # Approximate for other L
      expected_value <- L - 0.8
      variance <- 3.0
    }

    # Standardized statistic
    sigma <- sqrt(variance / K)
    z_score <- (fn - expected_value) / sigma

    # Two-tailed p-value
    p_value <- 2 * pnorm(-abs(z_score))

    list(
      p.value = p_value,
      statistic = fn,
      z_score = z_score,
      K = K,
      L = L
    )
  }

  # Run Maurer's test
  maurers_result <- maurers_universal_test(x)
  if (!is.na(maurers_result$p.value)) {
    suite$results$compression$maurers_universal <- list(
      description = "Maurer's Universal Statistical Test",
      result = if (is.na(maurers_result$p.value)) {
        "INCONCLUSIVE"
      } else if (maurers_result$p.value >= suite$config$significance_level) {
        "PASS"
      } else {
        "FAIL"
      },
      p_value = maurers_result$p.value,
      statistic = maurers_result$statistic,
      details = paste(
        "Tests for compressibility using pattern matching.",
        "fn statistic:", round(maurers_result$statistic, 4),
        "| z-score:", round(maurers_result$z_score, 3),
        "| p-value:", round(maurers_result$p.value, 4),
        "| L =", maurers_result$L, "| K =", maurers_result$K
      )
    )
  }

  # Store bootstrap distributions for potential visualization
  suite$bootstrap_distributions$compression <- list(
    compression_ratios = compression_results,
    entropy_deviations = entropy_result$bootstrap_stats,
    byte_chi_squares = byte_freq_result$simulated_stats
  )

  # Generate visualizations if requested
  if (require(ggplot2) && suite$config$save_visualizations) {
    suite <- visualize_compression_tests_bootstrap(suite, x, compression_results)
  }

  return(suite)
}

#' Visualize bootstrap compression test results
#'
#' @param suite The test suite object
#' @param x The generated random numbers
#' @param compression_results Results from compression ratio tests
#' @return Updated test suite with visualization paths
#' @keywords internal
visualize_compression_tests_bootstrap <- function(suite, x, compression_results) {
  # Set up output directory
  output_dir <- file.path(suite$config$output_dir, "visualizations", "compression_bootstrap")
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # 1. Bootstrap distribution plots for each compression algorithm
  for (algo in names(compression_results)) {
    result <- compression_results[[algo]]
    if (!is.null(result$bootstrap_stats)) {
      # Create histogram with KDE overlay
      df <- data.frame(ratio = result$bootstrap_stats)

      p <- ggplot2::ggplot(df, ggplot2::aes(x = ratio)) +
        ggplot2::geom_histogram(ggplot2::aes(y = ..density..),
          bins = 30, fill = "lightblue", alpha = 0.7
        ) +
        ggplot2::geom_vline(
          xintercept = result$observed_ratio,
          color = "red", linetype = "dashed", size = 1
        ) +
        ggplot2::theme_minimal() +
        ggplot2::labs(
          title = paste("Bootstrap Distribution -", toupper(algo), "Compression Ratio"),
          subtitle = paste(
            "Observed ratio (red):", round(result$observed_ratio, 4),
            "| p-value:", round(result$p.value, 4)
          ),
          x = "Compression Ratio",
          y = "Density"
        )

      # Add KDE if available
      if (!is.null(result$kde)) {
        kde_df <- data.frame(x = result$kde$x, y = result$kde$y)
        p <- p + ggplot2::geom_line(
          data = kde_df,
          ggplot2::aes(x = x, y = y),
          color = "blue", size = 1
        )
      }

      # Save plot
      plot_path <- file.path(output_dir, paste0("bootstrap_dist_", algo, ".png"))
      ggplot2::ggsave(plot_path, p, width = 8, height = 6)
    }
  }

  # 2. Entropy deviation bootstrap distribution
  if (!is.null(suite$bootstrap_distributions$compression$entropy_deviations)) {
    df <- data.frame(deviation = suite$bootstrap_distributions$compression$entropy_deviations)
    observed_dev <- suite$results$compression$entropy_bootstrap$statistic

    p <- ggplot2::ggplot(df, ggplot2::aes(x = deviation)) +
      ggplot2::geom_histogram(ggplot2::aes(y = ..density..),
        bins = 30, fill = "lightgreen", alpha = 0.7
      ) +
      ggplot2::geom_vline(
        xintercept = observed_dev,
        color = "red", linetype = "dashed", size = 1
      ) +
      ggplot2::theme_minimal() +
      ggplot2::labs(
        title = "Bootstrap Distribution - Entropy Deviation",
        subtitle = paste(
          "Observed deviation (red):", round(observed_dev, 6),
          "| p-value:", round(suite$results$compression$entropy_bootstrap$p_value, 4)
        ),
        x = "Deviation from Maximum Entropy",
        y = "Density"
      )

    plot_path <- file.path(output_dir, "bootstrap_dist_entropy.png")
    ggplot2::ggsave(plot_path, p, width = 8, height = 6)
  }

  # 3. Chi-square Monte Carlo distribution
  if (!is.null(suite$bootstrap_distributions$compression$byte_chi_squares)) {
    df <- data.frame(chi_square = suite$bootstrap_distributions$compression$byte_chi_squares)
    observed_chi <- suite$results$compression$byte_frequency_monte_carlo$statistic

    p <- ggplot2::ggplot(df, ggplot2::aes(x = chi_square)) +
      ggplot2::geom_histogram(ggplot2::aes(y = ..density..),
        bins = 30, fill = "lightyellow", alpha = 0.7
      ) +
      ggplot2::geom_vline(
        xintercept = observed_chi,
        color = "red", linetype = "dashed", size = 1
      ) +
      ggplot2::stat_function(
        fun = function(x) dchisq(x, df = 255),
        color = "blue", size = 1, linetype = "dotted"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::labs(
        title = "Monte Carlo Distribution - Byte Frequency Chi-Square",
        subtitle = paste(
          "Observed chi-square (red):", round(observed_chi, 2),
          "| MC p-value:", round(suite$results$compression$byte_frequency_monte_carlo$p_value, 4)
        ),
        x = "Chi-Square Statistic",
        y = "Density"
      )

    plot_path <- file.path(output_dir, "monte_carlo_chi_square.png")
    ggplot2::ggsave(plot_path, p, width = 8, height = 6)
  }

  # Store visualization paths
  suite$visualizations$compression_bootstrap <- list(
    output_directory = output_dir
  )

  return(suite)
}

# Maintain backward compatibility by updating the original function
# to use bootstrap methods by default
run_compression_tests <- function(suite) {
  # Check if bootstrap mode is disabled
  use_bootstrap <- suite$config$use_bootstrap_compression %||% TRUE

  if (use_bootstrap) {
    return(run_compression_tests_bootstrap(suite, use_legacy = FALSE))
  } else {
    # Call original implementation (would need to be renamed)
    return(run_compression_tests_legacy(suite))
  }
}
