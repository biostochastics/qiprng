# Helper functions for PRNG testing

#' Setup default PRNG configuration for tests
setup_prng <- function(cfg = list()) {
  # Create PRNG with default or custom config
  qiprng::createPRNG(cfg)
}

#' Generate uniform numbers in a range
#' @param n Number of values to generate
#' @param min Lower bound
#' @param max Upper bound
generate_uniform_range <- function(n, min, max) {
  cfg <- list(
    distribution = "uniform_range",
    range_min = min,
    range_max = max
  )
  qiprng::updatePRNG(cfg)
  qiprng::generatePRNG(n)
}

#' Generate normal random numbers
#' @param n Number of values to generate
#' @param mean Mean of normal distribution
#' @param sd Standard deviation
generate_normal <- function(n, mean = 0, sd = 1) {
  cfg <- list(
    distribution = "normal",
    normal_mean = mean,
    normal_sd = sd
  )
  qiprng::updatePRNG(cfg)
  qiprng::generatePRNG(n)
}

#' Generate exponential random numbers
#' @param n Number of values to generate
#' @param lambda Rate parameter
generate_exponential <- function(n, lambda = 1) {
  cfg <- list(
    distribution = "exponential",
    exponential_lambda = lambda
  )
  qiprng::updatePRNG(cfg)
  qiprng::generatePRNG(n)
}

#' Run statistical tests on generated numbers
#' @param nums Vector of numbers to test
#' @param distribution Expected distribution
#' @param ... Additional parameters for specific distributions
test_distribution <- function(nums, distribution, ...) {
  args <- list(...)

  # Basic checks
  testthat::expect_true(is.numeric(nums))
  testthat::expect_false(any(is.na(nums)))
  testthat::expect_false(any(is.infinite(nums)))

  if (distribution == "uniform_01") {
    testthat::expect_true(all(nums >= 0 & nums <= 1))
    ks <- stats::ks.test(nums, "punif", 0, 1)
    testthat::expect_true(ks$p.value > 0.05)
  } else if (distribution == "uniform_range") {
    min <- args$min
    max <- args$max
    testthat::expect_true(all(nums >= min & nums <= max))
    ks <- stats::ks.test(nums, "punif", min, max)
    testthat::expect_true(ks$p.value > 0.05)
  } else if (distribution == "normal") {
    mean <- args$mean %||% 0
    sd <- args$sd %||% 1
    ks <- stats::ks.test(nums, "pnorm", mean, sd)
    testthat::expect_true(ks$p.value > 0.05)
  } else if (distribution == "exponential") {
    lambda <- args$lambda %||% 1
    testthat::expect_true(all(nums >= 0))
    ks <- stats::ks.test(nums, "pexp", lambda)
    testthat::expect_true(ks$p.value > 0.05)
  }
}

#' Helper function to run statistical tests
run_statistical_tests <- function(x, distribution = "uniform") {
  # Run KS test
  ks <- stats::ks.test(x, "punif")

  # Run runs test
  runs <- sum(diff(x) > 0)
  runs_z <- (runs - (length(x) - 1) / 2) / sqrt((length(x) - 1) / 4)

  # Run FFT test
  centered <- x - mean(x)
  fft_val <- stats::fft(centered)
  spec_sd <- stats::sd(Mod(fft_val))

  # Return test results
  list(
    ks_pvalue = ks$p.value,
    runs_z_score = runs_z,
    spectral_sd = spec_sd
  )
}

#' Helper function to clean up PRNG after tests
cleanup_prng <- function() {
  # Reset PRNG state
  qiprng::createPRNG()
}

#' Clean up PRNG state after tests
teardown_prng <- function() {
  # Reset to default configuration
  cfg <- list(
    distribution = "uniform_01",
    use_crypto_mixing = FALSE
  )
  qiprng::updatePRNG(cfg)
}

# Function to test discriminant selection thread safety
test_choose_discriminant <- function(thread_count = 4, iterations = 5) {
  cat(paste0("\nTesting with ", thread_count, " threads and ", iterations, " iterations\n"))

  # Create a cluster
  cl <- parallel::makeCluster(thread_count)

  # Export the qiprng package to all workers
  parallel::clusterEvalQ(cl, {
    library(qiprng)
  })

  # Function to select discriminants and return selections
  discriminant_thread_test <- function(thread_id, iterations) {
    # Results container
    selected <- list()

    for (i in 1:iterations) {
      # Create configuration and select discriminant
      tryCatch(
        {
          # This should be safe now with mutex protection
          disc <- qiprng:::pickMultiQiSet(TRUE)
          selected[[i]] <- list(thread_id = thread_id, iteration = i, discriminant = disc[1])
        },
        error = function(e) {
          selected[[i]] <- list(thread_id = thread_id, iteration = i, error = e$message)
        }
      )
      # Small sleep to allow other threads to access discriminant selection
      Sys.sleep(0.01)
    }

    return(selected)
  }

  # Run the test in parallel
  results <- parallel::parLapply(cl, 1:thread_count, discriminant_thread_test, iterations)

  # Stop the cluster
  parallel::stopCluster(cl)

  # Create a safer flattening approach that doesn't rely on nested lists
  flat_results <- list()

  # Directly create a list of results without nested indexing
  for (thread_result in results) {
    if (is.list(thread_result)) {
      for (item in thread_result) {
        if (!is.null(item)) {
          flat_results <- c(flat_results, list(item))
        }
      }
    }
  }

  return(flat_results)
}

# Function to check if all discriminants are unique
check_discriminants_unique <- function(results) {
  # Extract discriminants
  discs <- lapply(results, function(r) {
    if (!is.null(r$discriminant)) {
      return(r$discriminant)
    } else {
      return(NULL)
    }
  })

  # Remove NULL values
  discs <- discs[!sapply(discs, is.null)]

  # Check uniqueness
  unique_count <- length(unique(unlist(discs)))
  total_count <- length(unlist(discs))

  cat(paste0("Unique discriminants: ", unique_count, " out of ", total_count, "\n"))

  return(unique_count == total_count)
}

# Simplified thread safety test for normal test runner
test_thread_safety_simple <- function() {
  cat("\n===== Testing thread safety of discriminant selection =====\n\n")

  # Create PRNG with threading and parallel filling enabled
  cat("Creating PRNG with threading and parallel filling enabled...\n")
  tryCatch(
    {
      createPRNG(list(
        distribution = "normal",
        normal_method = "ziggurat",
        use_threading = TRUE,
        use_parallel_filling = TRUE,
        debug = TRUE
      ))

      # Generate 10,000 values
      cat("Generating 10,000 values...\n")
      samples <- generatePRNG(10000)

      # Check statistics
      cat(paste0("Mean: ", mean(samples), " (expected ~0)\n"))
      cat(paste0("Variance: ", var(samples), " (expected ~1)\n"))
      cat(paste0("Min: ", min(samples), "\n"))
      cat(paste0("Max: ", max(samples), "\n"))

      # Clean up
      cleanup_prng()

      return(TRUE)
    },
    error = function(e) {
      cat(paste0("ERROR: ", e$message, "\n"))
      return(FALSE)
    }
  )
}
