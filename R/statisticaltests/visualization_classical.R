# File: visualization_classical.R
# ----------------------------------------------------------------------
#' Visualization functions for classical tests
#'
#' This file contains functions for visualizing the results of
#' classical tests for PRNG quality evaluation.

#' Visualize classical tests
#'
#' @param suite The test suite object
#' @param x The generated random numbers
#' @param collection_times Coupon collection times
#' @param expected_time Expected coupon collection time
#' @return Updated test suite with visualization paths
#' @keywords internal
visualize_classical_tests <- function(suite, x, collection_times, expected_time) {
  # Set up output directory if it doesn't exist
  output_dir <- file.path(suite$config$output_dir, "visualizations", "classical")
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # 1. Coupon Collector Test
  if (!is.null(collection_times) && length(collection_times) > 0) {
    coupon_data <- data.frame(
      Trial = 1:length(collection_times),
      CollectionTime = collection_times
    )
    
    p1 <- ggplot2::ggplot(coupon_data, ggplot2::aes(x = CollectionTime)) +
      ggplot2::geom_histogram(bins = 30, fill = "steelblue", color = "white", alpha = 0.7) +
      ggplot2::geom_vline(xintercept = expected_time, color = "red", linetype = "dashed") +
      ggplot2::theme_minimal() +
      ggplot2::labs(
        title = "Coupon Collector Test",
        subtitle = "Distribution of collection times with expected value (red line)",
        x = "Collection Time",
        y = "Frequency"
      )
    
    # Save plot
    coupon_path <- file.path(output_dir, "coupon_collector_plot.png")
    ggplot2::ggsave(coupon_path, p1, width = 10, height = 6)
  } else {
    coupon_path <- NULL
  }
  
  # 2. Poker Hand Test Visualization
  # Discretize values into "ranks" (1-13) and "suits" (1-4)
  n <- min(10000, length(x))  # Use a reasonable number of values
  
  # Create poker hands (groups of 5)
  num_hands <- floor(n / 5)
  hands <- matrix(x[1:(num_hands * 5)], ncol = 5, byrow = TRUE)
  
  # Function to classify poker hand type
  classify_hand <- function(hand) {
    # Discretize values into "ranks" (1-13 for simplicity)
    ranks <- ceiling(hand * 13)
    rank_counts <- table(ranks)
    
    if (length(rank_counts) == 5) {
      return("all_different") # All different
    } else if (length(rank_counts) == 4) {
      return("one_pair") # One pair
    } else if (length(rank_counts) == 3) {
      if (max(rank_counts) == 2) {
        return("two_pair") # Two pair
      } else {
        return("three_of_a_kind") # Three of a kind
      }
    } else if (length(rank_counts) == 2) {
      if (max(rank_counts) == 3) {
        return("full_house") # Full house
      } else {
        return("four_of_a_kind") # Four of a kind
      }
    } else {
      return("five_of_a_kind") # Five of a kind
    }
  }
  
  # Classify each hand
  hand_types <- apply(hands, 1, classify_hand)
  
  # Count frequencies
  hand_counts <- table(factor(hand_types, levels = c("all_different", "one_pair", "two_pair", 
                                                   "three_of_a_kind", "full_house", 
                                                   "four_of_a_kind", "five_of_a_kind")))
  
  # Calculate probabilities for each hand type (based on a perfect PRNG)
  n_values <- 13 # Using 13 discretized values
  
  # Calculate expected frequencies
  # These are approximate probabilities for our simplified model
  p_all_diff <- 0.3  # Approximation
  p_one_pair <- 0.44  # Approximation
  p_two_pair <- 0.12  # Approximation
  p_three <- 0.1  # Approximation
  p_full_house <- 0.02  # Approximation
  p_four <- 0.01  # Approximation
  p_five <- 0.01  # Approximation
  
  expected_probs <- c(p_all_diff, p_one_pair, p_two_pair, p_three, 
                     p_full_house, p_four, p_five)
  expected_probs <- expected_probs / sum(expected_probs)
  expected_counts <- num_hands * expected_probs
  
  # Prepare data for visualization
  hand_data <- data.frame(
    HandType = names(hand_counts),
    Observed = as.numeric(hand_counts),
    Expected = expected_counts[1:length(hand_counts)]
  )
  
  # Convert to long format for grouped bar chart
  hand_data_long <- reshape2::melt(hand_data, id.vars = "HandType", 
                                variable.name = "Type", 
                                value.name = "Count")
  
  # Reorder levels for clearer visualization
  hand_data_long$HandType <- factor(hand_data_long$HandType, 
                                  levels = c("all_different", "one_pair", "two_pair", 
                                           "three_of_a_kind", "full_house", 
                                           "four_of_a_kind", "five_of_a_kind"))
  
  p2 <- ggplot2::ggplot(hand_data_long, 
                     ggplot2::aes(x = HandType, y = Count, fill = Type)) +
    ggplot2::geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
    ggplot2::scale_fill_manual(values = c("steelblue", "orange")) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::labs(
      title = "Poker Hand Test",
      subtitle = "Observed vs. Expected frequencies of poker hand types",
      x = "Hand Type",
      y = "Frequency",
      fill = ""
    )
  
  # Save plot
  poker_hand_path <- file.path(output_dir, "poker_hand_plot.png")
  ggplot2::ggsave(poker_hand_path, p2, width = 10, height = 6)
  
  # 3. Birthday Spacing Test Visualization
  # We'll simulate the test here just for visualization
  num_days <- 365
  num_people <- 100
  num_trials <- 100
  
  # Function to count collisions in a simulated birthday scenario
  count_birthday_collisions <- function() {
    birthdays <- ceiling(sample(x, num_people) * num_days)
    sum(table(birthdays) > 1)
  }
  
  # Run simulations
  collision_counts <- replicate(num_trials, count_birthday_collisions())
  
  # Expected distribution is approximately Poisson with lambda = m^2 / (2*d)
  lambda <- num_people^2 / (2 * num_days)
  
  # Create data for visualization
  max_collisions <- max(collision_counts, 20)  # Cap at 20 for better visualization
  collision_counts_table <- table(factor(collision_counts, levels = 0:max_collisions))
  
  collision_data <- data.frame(
    Collisions = as.integer(names(collision_counts_table)),
    Observed = as.numeric(collision_counts_table)
  )
  
  # Add expected Poisson probabilities
  expected_probs <- dpois(collision_data$Collisions, lambda)
  collision_data$Expected <- num_trials * expected_probs
  
  # Convert to long format
  collision_data_long <- reshape2::melt(collision_data, id.vars = "Collisions", 
                                     variable.name = "Type", 
                                     value.name = "Count")
  
  p3 <- ggplot2::ggplot(collision_data_long, 
                     ggplot2::aes(x = factor(Collisions), y = Count, fill = Type)) +
    ggplot2::geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
    ggplot2::scale_fill_manual(values = c("steelblue", "orange")) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "Birthday Spacing Test",
      subtitle = paste("Distribution of collisions with expected Poisson(λ =", 
                     round(lambda, 2), ")"),
      x = "Number of Collisions",
      y = "Frequency",
      fill = ""
    )
  
  # Save plot
  birthday_path <- file.path(output_dir, "birthday_spacing_plot.png")
  ggplot2::ggsave(birthday_path, p3, width = 10, height = 6)
  
  # Store visualization paths in suite
  suite$visualizations$classical <- list(
    coupon_collector_plot = coupon_path,
    poker_hand_plot = poker_hand_path,
    birthday_spacing_plot = birthday_path
  )
  
  return(suite)
}
