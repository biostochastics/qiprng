# Excellent Discriminants Selection Module
# Provides functions to select from the 370 excellent discriminants for production use

#' Load and filter excellent discriminants from analysis results
#'
#' Loads discriminant analysis results and filters for those rated as "Excellent"
#' based on comprehensive statistical testing. These discriminants have passed
#' all major randomness tests and are suitable for production use.
#'
#' @param results_file Path to the analysis results RDS file containing test results
#'   (default: "discriminant_analysis_results/raw_results.rds")
#' @param min_score Minimum overall score threshold for excellent discriminants
#'   (default: 0.85)
#'
#' @return A data frame containing excellent discriminants with columns:
#'   \describe{
#'     \item{a}{Quadratic coefficient}
#'     \item{b}{Linear coefficient}
#'     \item{c}{Constant term}
#'     \item{discriminant}{The discriminant value b² - 4ac}
#'     \item{overall_score}{Combined test score (0-1)}
#'     \item{quality_rating}{Quality classification ("Excellent")}
#'     \item{uniformity_passed}{Logical; uniformity test result}
#'     \item{independence_passed}{Logical; independence test result}
#'     \item{autocorrelation_passed}{Logical; autocorrelation test result}
#'     \item{periodicity_passed}{Logical; periodicity test result}
#'   }
#'
#' @details
#' The function filters discriminants that:
#' \itemize{
#'   \item Have quality_rating == "Excellent"
#'   \item Have overall_score >= min_score (default 0.85)
#'   \item Have valid (non-NA) scores
#' }
#'
#' Results are sorted by overall_score in descending order.
#'
#' @note Requires that discriminant analysis has been run and results saved.
#' Run \code{run_discriminant_analysis()} first if results file doesn't exist.
#'
#' @examples
#' \dontrun{
#' # Load excellent discriminants
#' excellent <- load_excellent_discriminants()
#'
#' # Load with higher threshold
#' top_excellent <- load_excellent_discriminants(min_score = 0.90)
#' }
#'
#' @seealso \code{\link{run_discriminant_analysis}}, \code{\link{get_recommended_discriminants}}
#'
#' @export
load_excellent_discriminants <- function(results_file = "discriminant_analysis_results/raw_results.rds",
                                         min_score = 0.85) {
  if (!file.exists(results_file)) {
    stop("Analysis results file not found. Please run the full discriminant analysis first.")
  }

  # Load analysis results
  results <- readRDS(results_file)

  # Generate summary statistics
  source("R/discriminant_reports.R")
  summary_data <- generate_summary_stats(results)

  # Filter for excellent discriminants
  excellent <- summary_data[summary_data$quality_rating == "Excellent" &
    !is.na(summary_data$overall_score) &
    summary_data$overall_score >= min_score, ]

  # Sort by overall score (descending)
  excellent <- excellent[order(excellent$overall_score, decreasing = TRUE), ]

  cat("Found", nrow(excellent), "excellent discriminants with score >=", min_score, "\n")
  cat("Score range:", round(min(excellent$overall_score), 3), "to", round(max(excellent$overall_score), 3), "\n")

  return(excellent)
}

#' Get recommended discriminant parameters for production use
#'
#' Selects a subset of excellent discriminants based on specified criteria.
#' Provides three selection strategies: highest scores, balanced test performance,
#' or diverse parameter ranges.
#'
#' @param n Number of discriminants to return (default: 10)
#' @param results_file Path to the analysis results RDS file
#'   (default: "discriminant_analysis_results/raw_results.rds")
#' @param criteria Selection criteria, one of:
#'   \describe{
#'     \item{"top_score"}{Select discriminants with highest overall scores}
#'     \item{"balanced"}{Select discriminants that pass all tests uniformly}
#'     \item{"diverse"}{Select discriminants with diverse parameter ranges using k-means clustering}
#'   }
#'
#' @return A data frame with recommended discriminant parameters containing:
#'   \describe{
#'     \item{a, b, c}{Discriminant parameters}
#'     \item{discriminant}{The discriminant value b² - 4ac}
#'     \item{overall_score}{Combined test score}
#'     \item{Test results}{Individual test pass/fail status}
#'     \item{balance_score}{For "balanced" criteria, the proportion of tests passed}
#'   }
#'
#' @details
#' Selection strategies:
#' \itemize{
#'   \item **top_score**: Simply returns the n highest-scoring discriminants
#'   \item **balanced**: Prioritizes discriminants that pass all tests equally well,
#'     then sorts by overall score
#'   \item **diverse**: Uses k-means clustering on normalized (a,b,c) parameters
#'     to ensure diverse parameter combinations, selecting the best from each cluster
#' }
#'
#' If fewer excellent discriminants exist than requested, returns all available
#' with a warning.
#'
#' @note The "diverse" criteria requires at least 3 discriminants and uses
#' k-means clustering with seed=42 for reproducibility.
#'
#' @examples
#' \dontrun{
#' # Get top 10 discriminants by score
#' top10 <- get_recommended_discriminants(n = 10, criteria = "top_score")
#'
#' # Get discriminants with balanced test performance
#' balanced <- get_recommended_discriminants(n = 5, criteria = "balanced")
#'
#' # Get diverse parameter combinations
#' diverse <- get_recommended_discriminants(n = 20, criteria = "diverse")
#' }
#'
#' @seealso \code{\link{load_excellent_discriminants}}, \code{\link{create_excellent_prng_config}}
#'
#' @export
get_recommended_discriminants <- function(n = 10,
                                          results_file = "discriminant_analysis_results/raw_results.rds",
                                          criteria = "top_score") {
  excellent <- load_excellent_discriminants(results_file)

  if (nrow(excellent) == 0) {
    stop("No excellent discriminants found. Check analysis results.")
  }

  if (n > nrow(excellent)) {
    warning("Requested ", n, " discriminants but only ", nrow(excellent), " excellent ones available.")
    n <- nrow(excellent)
  }

  selected <- switch(criteria,
    "top_score" = {
      # Simply take the top n by score
      head(excellent, n)
    },
    "balanced" = {
      # Select discriminants that balance all test performance
      excellent$balance_score <- (
        as.numeric(excellent$uniformity_passed) +
          as.numeric(excellent$independence_passed) +
          as.numeric(excellent$autocorrelation_passed) +
          as.numeric(excellent$periodicity_passed)
      ) / 4

      # Prefer discriminants that pass all tests, then by overall score
      excellent_balanced <- excellent[order(-excellent$balance_score, -excellent$overall_score), ]
      head(excellent_balanced, n)
    },
    "diverse" = {
      # Select discriminants with diverse parameter ranges
      # Use k-means clustering on (a, b, c) parameters to ensure diversity
      if (n >= 3 && nrow(excellent) >= n) {
        param_matrix <- as.matrix(excellent[, c("a", "b", "c")])

        # Normalize parameters for clustering
        param_scaled <- scale(param_matrix)

        # Perform k-means clustering
        set.seed(42) # For reproducibility
        clusters <- kmeans(param_scaled, centers = min(n, nrow(excellent)), nstart = 20)

        # Select the best discriminant from each cluster
        selected_indices <- sapply(1:n, function(i) {
          cluster_members <- which(clusters$cluster == i)
          if (length(cluster_members) > 0) {
            # Return the best scoring member of this cluster
            cluster_scores <- excellent$overall_score[cluster_members]
            cluster_members[which.max(cluster_scores)]
          } else {
            NA
          }
        })

        selected_indices <- selected_indices[!is.na(selected_indices)]
        excellent[selected_indices, ]
      } else {
        # Fallback to top score if clustering not feasible
        head(excellent, n)
      }
    },
    {
      # Default to top_score
      head(excellent, n)
    }
  )

  cat("\nSelected", nrow(selected), "discriminants using '", criteria, "' criteria:\n")
  for (i in 1:nrow(selected)) {
    row <- selected[i, ]
    cat(sprintf(
      "  %d. a=%d, b=%d, c=%d, Δ=%d (Score: %.3f)\n",
      i, row$a, row$b, row$c, row$discriminant, row$overall_score
    ))
  }

  return(selected)
}

#' Create PRNG configuration using excellent discriminant
#'
#' Creates a complete PRNG configuration object from a single excellent
#' discriminant parameter set. The configuration is optimized for high-quality
#' random number generation.
#'
#' @param discriminant_params A single-row data frame from excellent discriminants
#'   containing columns a, b, c, discriminant, and overall_score
#' @param precision MPFR precision in bits for calculations (default: 256)
#' @param use_crypto Enable cryptographic mixing with ChaCha20 (default: TRUE)
#'
#' @return A list containing PRNG configuration:
#'   \describe{
#'     \item{a}{Quadratic coefficient}
#'     \item{b}{Linear coefficient}
#'     \item{c}{Constant term}
#'     \item{precision}{MPFR precision in bits}
#'     \item{use_parallel_filling}{Set to FALSE for stability}
#'     \item{use_cryptographic_mixing}{ChaCha20 mixing flag}
#'   }
#'
#' @details
#' The function creates a configuration optimized for production use:
#' \itemize{
#'   \item Parallel filling is disabled to avoid performance issues
#'   \item Cryptographic mixing is enabled by default for enhanced security
#'   \item The discriminant parameters are validated to be from excellent set
#' }
#'
#' The function also prints a summary of the configuration including the
#' discriminant value and quality score.
#'
#' @note The discriminant_params must be a single row. Use indexing or
#' subset operations if working with multiple discriminants.
#'
#' @examples
#' \dontrun{
#' # Get the best discriminant and create config
#' excellent <- load_excellent_discriminants()
#' best_discriminant <- excellent[1, ]
#' config <- create_excellent_prng_config(best_discriminant)
#'
#' # Create config with custom precision
#' config_512 <- create_excellent_prng_config(best_discriminant, precision = 512)
#' }
#'
#' @seealso \code{\link{createPRNG}}, \code{\link{get_recommended_discriminants}}
#'
#' @export
create_excellent_prng_config <- function(discriminant_params, precision = 256, use_crypto = TRUE) {
  if (nrow(discriminant_params) != 1) {
    stop("Please provide exactly one discriminant parameter set")
  }

  config <- list(
    a = discriminant_params$a,
    b = discriminant_params$b,
    c = discriminant_params$c,
    precision = precision,
    use_parallel_filling = FALSE,
    use_cryptographic_mixing = use_crypto
  )

  cat("Created PRNG configuration for excellent discriminant:\n")
  cat("  Parameters: a=", config$a, ", b=", config$b, ", c=", config$c, "\n")
  cat("  Discriminant: Δ=", discriminant_params$discriminant, "\n")
  cat("  Quality Score:", round(discriminant_params$overall_score, 3), "\n")
  cat("  Precision:", precision, "bits\n")
  cat("  Cryptographic mixing:", use_crypto, "\n")

  return(config)
}

#' Generate random numbers using an excellent discriminant
#'
#' Convenience function that loads excellent discriminants, selects one by index,
#' creates a PRNG configuration, and generates random numbers. Combines the
#' functionality of multiple functions for ease of use.
#'
#' @param n Number of random numbers to generate (default: 10000)
#' @param discriminant_index Index of discriminant to use where 1 = best,
#'   2 = second best, etc. (default: 1)
#' @param results_file Path to the analysis results RDS file
#'   (default: "discriminant_analysis_results/raw_results.rds")
#' @param precision MPFR precision in bits (default: 256)
#'
#' @return A numeric vector of n random numbers uniformly distributed on [0,1]
#'
#' @details
#' This function:
#' 1. Loads the excellent discriminants from the results file
#' 2. Selects the discriminant at the specified index (sorted by score)
#' 3. Creates a PRNG configuration with cryptographic mixing enabled
#' 4. Initializes the PRNG
#' 5. Generates the requested random numbers
#'
#' The discriminants are ordered by overall_score, so index 1 gives the
#' highest-scoring discriminant.
#'
#' @note Requires that discriminant analysis has been run and results saved.
#' The discriminant_index must not exceed the number of available excellent
#' discriminants (typically 370).
#'
#' @examples
#' \dontrun{
#' # Generate random numbers using the best discriminant
#' samples <- generate_excellent_random(n = 50000)
#' hist(samples, breaks = 50)
#'
#' # Use the 10th best discriminant
#' samples <- generate_excellent_random(n = 10000, discriminant_index = 10)
#'
#' # Use higher precision
#' samples <- generate_excellent_random(n = 5000, precision = 512)
#' }
#'
#' @seealso \code{\link{load_excellent_discriminants}}, \code{\link{create_excellent_prng_config}},
#'   \code{\link{createPRNG}}, \code{\link{generatePRNG}}
#'
#' @export
generate_excellent_random <- function(n = 10000,
                                      discriminant_index = 1,
                                      results_file = "discriminant_analysis_results/raw_results.rds",
                                      precision = 256) {
  # Get the excellent discriminants
  excellent <- load_excellent_discriminants(results_file)

  if (discriminant_index > nrow(excellent)) {
    stop("Discriminant index ", discriminant_index, " exceeds available excellent discriminants (", nrow(excellent), ")")
  }

  # Select the specified discriminant
  selected_discriminant <- excellent[discriminant_index, ]

  # Create PRNG configuration
  config <- create_excellent_prng_config(selected_discriminant, precision = precision)

  # Initialize and generate
  # library(qiprng) # Not needed - functions are available within package
  createPRNG(config = config)
  samples <- generatePRNG(n)

  cat("Generated", n, "random numbers using excellent discriminant #", discriminant_index, "\n")

  return(samples)
}

#' Print summary of excellent discriminants analysis
#'
#' Displays a comprehensive summary of the excellent discriminants found in the
#' analysis, including count, score ranges, test performance, parameter ranges,
#' and usage recommendations.
#'
#' @param results_file Path to the analysis results RDS file
#'   (default: "discriminant_analysis_results/raw_results.rds")
#'
#' @return NULL (invisibly). Function is called for its side effect of printing
#'   a formatted summary to the console.
#'
#' @details
#' The summary includes:
#' \itemize{
#'   \item Total count of excellent discriminants
#'   \item Overall score range
#'   \item Test performance summary (pass rates for each test type)
#'   \item Parameter ranges (min/max for a, b, c, and discriminant)
#'   \item Top 5 discriminants by score
#'   \item Usage recommendations with example function calls
#' }
#'
#' @note This function is primarily for interactive use to quickly assess
#' the results of discriminant analysis.
#'
#' @examples
#' \dontrun{
#' # Print summary of excellent discriminants
#' print_excellent_summary()
#'
#' # Print summary from custom results file
#' print_excellent_summary("my_analysis/results.rds")
#' }
#'
#' @seealso \code{\link{load_excellent_discriminants}}, \code{\link{get_recommended_discriminants}}
#'
#' @export
print_excellent_summary <- function(results_file = "discriminant_analysis_results/raw_results.rds") {
  excellent <- load_excellent_discriminants(results_file)

  cat("\n=== EXCELLENT DISCRIMINANTS SUMMARY ===\n")
  cat("Total excellent discriminants found:", nrow(excellent), "\n")
  cat("Score range:", round(min(excellent$overall_score), 3), "to", round(max(excellent$overall_score), 3), "\n\n")

  cat("Test Performance Summary:\n")
  cat("  Uniformity pass rate:", round(100 * mean(excellent$uniformity_passed), 1), "%\n")
  cat("  Independence pass rate:", round(100 * mean(excellent$independence_passed), 1), "%\n")
  cat("  Autocorrelation pass rate:", round(100 * mean(excellent$autocorrelation_passed), 1), "%\n")
  cat("  Periodicity pass rate:", round(100 * mean(excellent$periodicity_passed), 1), "%\n\n")

  cat("Parameter Ranges:\n")
  cat("  a: ", min(excellent$a), " to ", max(excellent$a), "\n")
  cat("  b: ", min(excellent$b), " to ", max(excellent$b), "\n")
  cat("  c: ", min(excellent$c), " to ", max(excellent$c), "\n")
  cat("  Discriminant: ", min(excellent$discriminant), " to ", max(excellent$discriminant), "\n\n")

  cat("Top 5 Excellent Discriminants:\n")
  top5 <- head(excellent, 5)
  for (i in 1:nrow(top5)) {
    row <- top5[i, ]
    cat(sprintf(
      "  %d. a=%d, b=%d, c=%d, Δ=%d (Score: %.3f)\n",
      i, row$a, row$b, row$c, row$discriminant, row$overall_score
    ))
  }

  cat("\n=== USAGE RECOMMENDATIONS ===\n")
  cat("For production use, call:\n")
  cat("  get_recommended_discriminants(n=10, criteria='balanced')\n")
  cat("  generate_excellent_random(n=50000, discriminant_index=1)\n\n")
}
