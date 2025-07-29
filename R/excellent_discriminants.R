# Excellent Discriminants Selection Module
# Provides functions to select from the 227 excellent discriminants for production use

#' Load and filter excellent discriminants from analysis results
#'
#' @param results_file Path to the analysis results RDS file
#' @param min_score Minimum score threshold for excellent discriminants (default 0.85)
#' @return Data frame with excellent discriminants only
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
#' @param n Number of discriminants to return (default 10)
#' @param results_file Path to the analysis results RDS file
#' @param criteria Selection criteria: "top_score", "balanced", or "diverse"
#' @return Data frame with recommended discriminant parameters
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
        set.seed(42)  # For reproducibility
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
    cat(sprintf("  %d. a=%d, b=%d, c=%d, Δ=%d (Score: %.3f)\n", 
                i, row$a, row$b, row$c, row$discriminant, row$overall_score))
  }
  
  return(selected)
}

#' Create PRNG configuration using excellent discriminant
#'
#' @param discriminant_params Single row from excellent discriminants data frame
#' @param precision MPFR precision (default 256)
#' @param use_crypto Enable cryptographic mixing (default TRUE)
#' @return PRNG configuration list
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
#' @param n Number of random numbers to generate
#' @param discriminant_index Index of discriminant to use (1 = best, 2 = second best, etc.)
#' @param results_file Path to the analysis results RDS file
#' @param precision MPFR precision (default 256)
#' @return Vector of random numbers
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
  library(qiprng)
  createPRNG(config = config)
  samples <- generatePRNG(n)
  
  cat("Generated", n, "random numbers using excellent discriminant #", discriminant_index, "\n")
  
  return(samples)
}

#' Print summary of excellent discriminants analysis
#'
#' @param results_file Path to the analysis results RDS file
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
    cat(sprintf("  %d. a=%d, b=%d, c=%d, Δ=%d (Score: %.3f)\n", 
                i, row$a, row$b, row$c, row$discriminant, row$overall_score))
  }
  
  cat("\n=== USAGE RECOMMENDATIONS ===\n")
  cat("For production use, call:\n")
  cat("  get_recommended_discriminants(n=10, criteria='balanced')\n")
  cat("  generate_excellent_random(n=50000, discriminant_index=1)\n\n")
}
