# File: classical_tests.R
# ----------------------------------------------------------------------
#' Classical tests for PRNG quality
#'
#' This module provides classical tests for PRNG quality evaluation,
#' including coupon collector, poker, and other traditional tests.

#' Run classical PRNG tests
#'
#' @param suite The test suite object
#' @return Updated test suite with results
#' @keywords internal
run_classical_tests <- function(suite) {
  # Generate random numbers
  n <- suite$config$classical_sample_size
  x <- suite$prng_func(n)
  
  # Initialize results
  suite$results$classical <- list()
  
  # 1. Coupon collector test
  # Tests how many samples are needed to get a complete set of "coupons"
  coupon_collector_test <- function(x, num_bins = 10) {
    # Bin values into discrete "coupons"
    coupons <- ceiling(x * num_bins)
    
    # Initialize counters
    trials <- numeric(0)
    collection_times <- numeric(0)
    
    # Calculate collection times for multiple trials
    max_trials <- min(1000, floor(length(x) / (num_bins * 2)))
    for (i in 1:max_trials) {
      start_idx <- (i - 1) * (num_bins * 5) + 1
      if (start_idx + num_bins * 5 > length(coupons)) break
      
      # Process this batch of values
      collected <- rep(FALSE, num_bins)
      count <- 0
      for (j in start_idx:(start_idx + num_bins * 5)) {
        if (j > length(coupons)) break
        count <- count + 1
        collected[coupons[j]] <- TRUE
        if (all(collected)) break
      }
      
      # If we collected all coupons, record the time
      if (all(collected)) {
        trials <- c(trials, i)
        collection_times <- c(collection_times, count)
      }
    }
    
    # Need sufficient successful trials
    if (length(collection_times) < 10) {
      return(list(
        p.value = NA,
        statistic = NA,
        details = "Insufficient data for coupon collector test"
      ))
    }
    
    # Expected time to collect all coupons: n*H_n where H_n is the nth harmonic number
    harmonic_n <- sum(1 / (1:num_bins))
    expected_time <- num_bins * harmonic_n
    
    # Variance (approximate)
    variance <- (pi^2 / 6) * num_bins^2  # Simplified approximation
    
    # Chi-square test on the collection times
    # We'll use a chi-square test with empirical bins
    observed <- table(cut(collection_times, breaks = 5))
    probs <- seq(0, 1, length.out = 6)
    q <- qnorm((probs[-1] + probs[-length(probs)]) / 2, mean = expected_time, sd = sqrt(variance))
    expected <- length(collection_times) * diff(probs)
    
    chi_result <- suppressWarnings(chisq.test(observed, p = expected / sum(expected)))
    
    list(
      p.value = chi_result$p.value,
      statistic = chi_result$statistic,
      mean_time = mean(collection_times),
      expected_time = expected_time,
      num_trials = length(collection_times),
      collection_times = collection_times
    )
  }
  
  # Run coupon collector test
  cc_result <- coupon_collector_test(x)
  suite$results$classical$coupon_collector <- list(
    description = "Coupon Collector Test",
    result = ifelse(!is.na(cc_result$p.value) && 
                   cc_result$p.value >= suite$config$significance_level, 
                   "PASS", ifelse(is.na(cc_result$p.value), "INCONCLUSIVE", "FAIL")),
    p_value = cc_result$p.value,
    statistic = cc_result$statistic,
    details = if(!is.na(cc_result$p.value)) {
      paste("Tests distribution of times to collect all coupons.",
            "Mean time:", round(cc_result$mean_time, 2),
            "/ Expected:", round(cc_result$expected_time, 2),
            "Trials:", cc_result$num_trials)
    } else {
      cc_result$details
    }
  )
  
  # 2. Poker Hand Test
  # Tests for distribution of "poker hands" in groups of values
  poker_hand_test <- function(x, hand_size = 5) {
    # Need sufficient values
    if (length(x) < hand_size * 100) {
      return(list(
        p.value = NA,
        statistic = NA,
        details = "Insufficient data for poker hand test"
      ))
    }
    
    # Process values in groups of hand_size
    hands <- matrix(x[1:(floor(length(x) / hand_size) * hand_size)], 
                   ncol = hand_size, byrow = TRUE)
    
    # Function to classify poker hand type
    # We'll simplify to: all different, one pair, two pair, three of a kind, full house, four of a kind, five of a kind
    classify_hand <- function(hand) {
      # Discretize values into "ranks" (1-13 for simplicity)
      ranks <- ceiling(hand * 13)
      rank_counts <- table(ranks)
      
      if (length(rank_counts) == hand_size) {
        return("all_different") # All different
      } else if (length(rank_counts) == hand_size - 1) {
        return("one_pair") # One pair
      } else if (length(rank_counts) == hand_size - 2) {
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
    observed <- table(factor(hand_types, levels = c("all_different", "one_pair", "two_pair", 
                                                  "three_of_a_kind", "full_house", 
                                                  "four_of_a_kind", "five_of_a_kind")))
    
    # Calculate probabilities for each hand type (based on a perfect PRNG)
    # These are approximate probabilities for a simplified poker model
    n_hands <- nrow(hands)
    n_values <- 13 # Using 13 discretized values
    
    # Calculate combinatorial probabilities
    # These probabilities assume equal likelihood of any "card" and replacement
    # They're approximations of the probabilities for our discretized model
    
    # Probability of all different
    p_all_diff <- prod(1:hand_size) / n_values^hand_size * choose(n_values, hand_size)
    
    # Probability of one pair
    p_one_pair <- choose(hand_size, 2) * n_values * (n_values - 1)^(hand_size - 2) / n_values^hand_size
    
    # Probability of two pair (approximation)
    p_two_pair <- choose(hand_size, 2) * choose(hand_size - 2, 2) * n_values * (n_values - 1) / 
      (2 * n_values^hand_size)
    
    # Probability of three of a kind
    p_three <- choose(hand_size, 3) * n_values * (n_values - 1)^(hand_size - 3) / n_values^hand_size
    
    # Probability of full house (for hand_size = 5)
    p_full_house <- if (hand_size == 5) {
      choose(n_values, 1) * choose(n_values - 1, 1) * choose(5, 3) / n_values^5
    } else {
      0.01 # Approximation for other hand sizes
    }
    
    # Probability of four of a kind
    p_four <- choose(hand_size, 4) * n_values * (n_values - 1)^(hand_size - 4) / n_values^hand_size
    
    # Probability of five of a kind
    p_five <- n_values / n_values^hand_size
    
    # Ensure probabilities sum to 1
    probs <- c(p_all_diff, p_one_pair, p_two_pair, p_three, p_full_house, p_four, p_five)
    probs <- probs / sum(probs)
    
    # Expected frequencies
    expected <- n_hands * probs
    
    # Combine categories with low expected counts
    min_expected <- 5
    if (any(expected < min_expected)) {
      # Keep only categories with sufficient expected count
      keep <- which(expected >= min_expected)
      if (length(keep) <= 1) {
        return(list(
          p.value = NA,
          statistic = NA,
          details = "Expected frequencies too small for valid test"
        ))
      }
      
      # Combine others into an "other" category
      observed_valid <- observed[keep]
      observed_other <- sum(observed[-keep])
      observed <- c(observed_valid, Other = observed_other)
      
      expected_valid <- expected[keep]
      expected_other <- sum(expected[-keep])
      expected <- c(expected_valid, expected_other)
    }
    
    # Chi-square test
    chi_result <- suppressWarnings(chisq.test(observed, p = expected / sum(expected)))
    
    list(
      p.value = chi_result$p.value,
      statistic = chi_result$statistic,
      observed = observed,
      expected = expected,
      df = length(observed) - 1
    )
  }
  
  # Run poker hand test
  poker_result <- poker_hand_test(x)
  suite$results$classical$poker_hand <- list(
    description = "Poker Hand Test",
    result = ifelse(!is.na(poker_result$p.value) && 
                   poker_result$p.value >= suite$config$significance_level, 
                   "PASS", ifelse(is.na(poker_result$p.value), "INCONCLUSIVE", "FAIL")),
    p_value = poker_result$p.value,
    statistic = poker_result$statistic,
    details = if(!is.na(poker_result$p.value)) {
      paste("Tests distribution of poker hand types.",
            "Chi-square:", round(poker_result$statistic, 2),
            "with", poker_result$df, "degrees of freedom")
    } else {
      poker_result$details
    }
  )
  
  # 3. Birthday Spacing Test
  # Tests the distribution of spacings between "birthdays" (same values)
  birthday_spacing_test <- function(x, num_days = 365, num_people = 100) {
    # Need sufficient values
    if (length(x) < num_people * 100) {
      return(list(
        p.value = NA,
        statistic = NA,
        details = "Insufficient data for birthday spacing test"
      ))
    }
    
    # Number of trials
    num_trials <- floor(length(x) / num_people)
    
    # Process in groups of num_people
    collision_counts <- numeric(num_trials)
    
    for (i in 1:num_trials) {
      start_idx <- (i - 1) * num_people + 1
      end_idx <- i * num_people
      if (end_idx > length(x)) break
      
      # Map values to "birthdays"
      birthdays <- ceiling(x[start_idx:end_idx] * num_days)
      
      # Count collisions (same "birthdays")
      birthday_table <- table(birthdays)
      collision_counts[i] <- sum(birthday_table > 1)
    }
    
    # Expected distribution is approximately Poisson with lambda = m^2 / (2*d)
    # where m is num_people and d is num_days
    lambda <- num_people^2 / (2 * num_days)
    
    # Observed frequencies of collision counts
    observed <- table(factor(collision_counts, levels = 0:max(collision_counts)))
    
    # Expected frequencies from Poisson distribution
    expected_probs <- dpois(as.integer(names(observed)), lambda)
    expected <- num_trials * expected_probs
    
    # Combine categories with low expected counts
    min_expected <- 5
    if (any(expected < min_expected)) {
      # Keep only categories with sufficient expected count
      keep <- which(expected >= min_expected)
      if (length(keep) <= 1) {
        return(list(
          p.value = NA,
          statistic = NA,
          details = "Expected frequencies too small for valid test"
        ))
      }
      
      # Combine others into an "other" category
      observed_valid <- observed[keep]
      observed_other <- sum(observed[-keep])
      observed <- c(observed_valid, Other = observed_other)
      
      expected_valid <- expected[keep]
      expected_other <- sum(expected[-keep])
      expected <- c(expected_valid, expected_other)
    }
    
    # Chi-square test
    chi_result <- suppressWarnings(chisq.test(observed, p = expected / sum(expected)))
    
    list(
      p.value = chi_result$p.value,
      statistic = chi_result$statistic,
      mean_collisions = mean(collision_counts),
      expected_collisions = lambda,
      df = length(observed) - 1
    )
  }
  
  # Run birthday spacing test
  birthday_result <- birthday_spacing_test(x)
  suite$results$classical$birthday_spacing <- list(
    description = "Birthday Spacing Test",
    result = ifelse(!is.na(birthday_result$p.value) && 
                   birthday_result$p.value >= suite$config$significance_level, 
                   "PASS", ifelse(is.na(birthday_result$p.value), "INCONCLUSIVE", "FAIL")),
    p_value = birthday_result$p.value,
    statistic = birthday_result$statistic,
    details = if(!is.na(birthday_result$p.value)) {
      paste("Tests distribution of 'birthday' collisions.",
            "Mean collisions:", round(birthday_result$mean_collisions, 2),
            "/ Expected:", round(birthday_result$expected_collisions, 2),
            "Chi-square:", round(birthday_result$statistic, 2),
            "with", birthday_result$df, "degrees of freedom")
    } else {
      birthday_result$details
    }
  )
  
  # Generate visualizations
  if (suite$config$save_visualizations && requireNamespace("ggplot2", quietly = TRUE)) {
    suite <- visualize_classical_tests(suite, x, 
                                     cc_result$collection_times,
                                     cc_result$expected_time)
  }
  
  return(suite)
}
