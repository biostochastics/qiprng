# File: external_wrappers.R
# ----------------------------------------------------------------------
#' Wrappers for External Randomness Test Packages
#'
#' This module provides unified interfaces to randomness tests from
#' external R packages (CryptRndTest, randtests) that complement
#' the existing qiprng test suite.
#'
#' Tests from CryptRndTest:
#' \itemize{
#'   \item adaptive.chi.square - Adaptive chi-square test
#'   \item birthday.spacings - Birthday spacings test
#'   \item book.stack - Book stack test
#'   \item GCD.test - Greatest common divisor test
#'   \item random.walk.tests - Three random walk tests
#'   \item topological.binary - Topological binary test
#' }
#'
#' Tests from randtests:
#' \itemize{
#'   \item bartels.rank.test - Bartels rank test
#'   \item cox.stuart.test - Cox-Stuart test
#'   \item difference.sign.test - Difference sign test
#'   \item turning.point.test - Turning point test
#' }

# Check if packages are available
has_cryptrndtest <- requireNamespace("CryptRndTest", quietly = TRUE)
has_randtests <- requireNamespace("randtests", quietly = TRUE)

#' Run CryptRndTest Suite
#'
#' Runs randomness tests from the CryptRndTest package on the provided data
#'
#' @param x Numeric vector of random numbers in [0,1)
#' @param test_type Character vector of test names to run.
#'        Default is "all" which runs all available tests.
#' @param alpha Significance level (default: 0.05)
#' @param ... Additional parameters passed to individual tests
#' @return List of test results with p-values and statistics
#' @export
run_cryptrndtest_suite <- function(x, test_type = "all", alpha = 0.05, ...) {
  if (!has_cryptrndtest) {
    stop("CryptRndTest package is not installed. Install with: install.packages('CryptRndTest')")
  }

  results <- list()
  available_tests <- c(
    "adaptive_chi_square", "birthday_spacings", "book_stack",
    "gcd", "random_walk", "topological_binary"
  )

  if ("all" %in% test_type) {
    test_type <- available_tests
  }

  # Adaptive Chi-Square Test
  if ("adaptive_chi_square" %in% test_type) {
    tryCatch(
      {
        # Convert to integers if needed
        if (max(x) <= 1) {
          x_int <- as.integer(x * 2^16) # Convert to 16-bit integers
        } else {
          x_int <- as.integer(x)
        }

        # Extract parameters or use defaults
        params <- list(...)
        B <- params$B
        S <- params$S
        if (is.null(B)) B <- 16 # Default block size
        if (is.null(S)) S <- length(x_int) # Default to full sample

        result <- CryptRndTest::adaptive.chi.square(x_int,
          B = B, S = S,
          alpha = alpha
        )
        results$adaptive_chi_square <- list(
          description = "Adaptive Chi-Square Test",
          p_value = result$p.value,
          statistic = result$statistic,
          result = if (is.na(result$p.value)) {
            "INCONCLUSIVE"
          } else if (result$p.value >= alpha) {
            "PASS"
          } else {
            "FAIL"
          },
          details = result$method
        )
      },
      error = function(e) {
        results$adaptive_chi_square <- list(
          description = "Adaptive Chi-Square Test",
          error = as.character(e),
          result = "ERROR"
        )
      }
    )
  }

  # Birthday Spacings Test
  if ("birthday_spacings" %in% test_type) {
    tryCatch(
      {
        # Birthday spacings needs specific format
        n <- length(x)
        m <- floor(n^(2 / 3)) # Recommended by package

        if (max(x) <= 1) {
          x_int <- as.integer(x * 2^16)
        } else {
          x_int <- as.integer(x)
        }

        # Extract parameters
        params <- list(...)
        n_param <- params$n
        if (is.null(n_param)) n_param <- 2^16

        # Calculate lambda based on theory
        lambda_param <- params$lambda
        if (is.null(lambda_param)) {
          lambda_param <- m^3 / (4 * n_param)
        }

        result <- CryptRndTest::birthday.spacings(x_int,
          m = m, n = n_param,
          lambda = lambda_param,
          alpha = alpha
        )
        results$birthday_spacings <- list(
          description = "Birthday Spacings Test",
          p_value = result$p.value,
          statistic = result$statistic,
          result = if (is.na(result$p.value)) {
            "INCONCLUSIVE"
          } else if (result$p.value >= alpha) {
            "PASS"
          } else {
            "FAIL"
          },
          details = result$method
        )
      },
      error = function(e) {
        results$birthday_spacings <- list(
          description = "Birthday Spacings Test",
          error = as.character(e),
          result = "ERROR"
        )
      }
    )
  }

  # Book Stack Test
  if ("book_stack" %in% test_type) {
    tryCatch(
      {
        # Book stack test
        if (max(x) <= 1) {
          x_int <- as.integer(x * 256) # Convert to byte values
        } else {
          x_int <- as.integer(x)
        }

        result <- CryptRndTest::book.stack(x_int, ...)
        results$book_stack <- list(
          description = "Book Stack Test",
          p_value = result$KS.p.value, # Uses KS test
          statistic = result$KS.statistic,
          result = if (is.na(result$KS.p.value)) {
            "INCONCLUSIVE"
          } else if (result$KS.p.value >= alpha) {
            "PASS"
          } else {
            "FAIL"
          },
          details = paste("KS test on", result$method)
        )
      },
      error = function(e) {
        results$book_stack <- list(
          description = "Book Stack Test",
          error = as.character(e),
          result = "ERROR"
        )
      }
    )
  }

  # GCD Test
  if ("gcd" %in% test_type) {
    tryCatch(
      {
        # GCD test needs pairs of integers
        if (max(x) <= 1) {
          x_int <- as.integer(x * 2^16)
        } else {
          x_int <- as.integer(x)
        }

        # Need even number of values
        if (length(x_int) %% 2 == 1) {
          x_int <- x_int[-length(x_int)]
        }

        # Convert to N×2 matrix as required by GCD.test
        x_matrix <- matrix(x_int, ncol = 2, byrow = TRUE)

        # Extract parameters or use defaults
        params <- list(...)
        B <- params$B
        if (is.null(B)) B <- 32

        # Calculate mu and sd based on uniform distribution
        mu_param <- params$mu
        sd_param <- params$sd
        if (is.null(mu_param) || is.null(sd_param)) {
          # Filter out zeros for log calculation
          x_positive <- x_int[x_int > 0]
          if (length(x_positive) > 0) {
            if (is.null(mu_param)) mu_param <- mean(log(x_positive))
            if (is.null(sd_param)) sd_param <- sd(log(x_positive))
          } else {
            # Fallback values
            mu_param <- 10
            sd_param <- 3
          }
        }

        result <- CryptRndTest::GCD.test(x_matrix,
          B = B, mu = mu_param,
          sd = sd_param, alpha = alpha
        )
        results$gcd <- list(
          description = "Greatest Common Divisor Test",
          p_value = result$p.value,
          statistic = result$statistic,
          result = if (is.na(result$p.value)) {
            "INCONCLUSIVE"
          } else if (result$p.value >= alpha) {
            "PASS"
          } else {
            "FAIL"
          },
          details = result$method
        )
      },
      error = function(e) {
        results$gcd <- list(
          description = "Greatest Common Divisor Test",
          error = as.character(e),
          result = "ERROR"
        )
      }
    )
  }

  # Random Walk Tests
  if ("random_walk" %in% test_type) {
    tryCatch(
      {
        # Random walk tests work on binary data
        # Convert to bits
        bits <- numeric()
        for (val in x) {
          # Convert each value to 8 bits
          byte_val <- as.integer(val * 255)
          for (i in 0:7) {
            bits <- c(bits, (byte_val %/% (2^i)) %% 2)
          }
        }

        # Extract B parameter if provided, otherwise use default
        B <- list(...)$B
        if (is.null(B)) B <- 64

        result <- CryptRndTest::random.walk.tests(bits,
          B = B,
          alpha = alpha
        )

        # Extract p-values from different walk tests
        results$random_walk <- list(
          description = "Random Walk Tests",
          p_value = min(
            result$Excursion$p.value,
            result$Expansion$p.value,
            result$Height$p.value
          ),
          excursion_p = result$Excursion$p.value,
          expansion_p = result$Expansion$p.value,
          height_p = result$Height$p.value,
          result = ifelse(min(
            result$Excursion$p.value,
            result$Expansion$p.value,
            result$Height$p.value
          ) >= alpha, "PASS", "FAIL"),
          details = "Three random walk tests: Excursion, Expansion, Height"
        )
      },
      error = function(e) {
        results$random_walk <- list(
          description = "Random Walk Tests",
          error = as.character(e),
          result = "ERROR"
        )
      }
    )
  }

  # Topological Binary Test
  if ("topological_binary" %in% test_type) {
    tryCatch(
      {
        # Convert to 8-bit integers
        if (max(x) <= 1) {
          x_int <- as.integer(x * 255)
        } else {
          x_int <- as.integer(x)
        }

        # Need at least 8 values
        if (length(x_int) >= 8) {
          # Extract parameters or use defaults
          params <- list(...)
          B <- params$B
          if (is.null(B)) B <- 8 # Default block size

          # Get critical value based on B and sample size
          critical_value <- params$critical.value
          if (is.null(critical_value)) {
            # Approximate critical values for common B values
            if (B == 8) {
              critical_value <- floor(length(x_int) * 0.95)
            } else if (B == 16) {
              critical_value <- 9245 # For k=10^4
            } else {
              # General approximation
              critical_value <- floor(length(x_int) * (1 - exp(-length(x_int) / 2^B)))
            }
          }

          result <- CryptRndTest::topological.binary(x_int,
            B = B,
            alpha = alpha,
            critical.value = critical_value
          )
          results$topological_binary <- list(
            description = "Topological Binary Test",
            p_value = result$p.value,
            statistic = result$statistic,
            result = if (is.na(result$p.value)) {
              "INCONCLUSIVE"
            } else if (result$p.value >= alpha) {
              "PASS"
            } else {
              "FAIL"
            },
            details = result$method
          )
        } else {
          results$topological_binary <- list(
            description = "Topological Binary Test",
            error = "Insufficient data (need at least 8 values)",
            result = "ERROR"
          )
        }
      },
      error = function(e) {
        results$topological_binary <- list(
          description = "Topological Binary Test",
          error = as.character(e),
          result = "ERROR"
        )
      }
    )
  }

  return(results)
}

#' Run randtests Suite
#'
#' Runs randomness tests from the randtests package on the provided data
#'
#' @param x Numeric vector of random numbers
#' @param test_type Character vector of test names to run
#' @param alpha Significance level (default: 0.05)
#' @param ... Additional parameters passed to individual tests
#' @return List of test results
#' @export
run_randtests_suite <- function(x, test_type = "all", alpha = 0.05, ...) {
  if (!has_randtests) {
    stop("randtests package is not installed. Install with: install.packages('randtests')")
  }

  results <- list()
  available_tests <- c(
    "runs", "bartels_rank", "cox_stuart",
    "difference_sign", "turning_point"
  )

  if ("all" %in% test_type) {
    test_type <- available_tests
  }

  # Runs Test (already in qiprng but this is a different implementation)
  if ("runs" %in% test_type) {
    tryCatch(
      {
        result <- randtests::runs.test(x, ...)
        results$runs <- list(
          description = "Runs Test (randtests)",
          p_value = result$p.value,
          statistic = result$statistic,
          result = if (is.na(result$p.value)) {
            "INCONCLUSIVE"
          } else if (result$p.value >= alpha) {
            "PASS"
          } else {
            "FAIL"
          },
          details = result$method
        )
      },
      error = function(e) {
        results$runs <- list(
          description = "Runs Test (randtests)",
          error = as.character(e),
          result = "ERROR"
        )
      }
    )
  }

  # Bartels Rank Test
  if ("bartels_rank" %in% test_type) {
    tryCatch(
      {
        result <- randtests::bartels.rank.test(x, ...)
        results$bartels_rank <- list(
          description = "Bartels Rank Test",
          p_value = result$p.value,
          statistic = result$statistic,
          result = if (is.na(result$p.value)) {
            "INCONCLUSIVE"
          } else if (result$p.value >= alpha) {
            "PASS"
          } else {
            "FAIL"
          },
          details = result$method
        )
      },
      error = function(e) {
        results$bartels_rank <- list(
          description = "Bartels Rank Test",
          error = as.character(e),
          result = "ERROR"
        )
      }
    )
  }

  # Cox-Stuart Test
  if ("cox_stuart" %in% test_type) {
    tryCatch(
      {
        result <- randtests::cox.stuart.test(x, ...)
        results$cox_stuart <- list(
          description = "Cox-Stuart Test",
          p_value = result$p.value,
          statistic = result$statistic,
          result = if (is.na(result$p.value)) {
            "INCONCLUSIVE"
          } else if (result$p.value >= alpha) {
            "PASS"
          } else {
            "FAIL"
          },
          details = result$method
        )
      },
      error = function(e) {
        results$cox_stuart <- list(
          description = "Cox-Stuart Test",
          error = as.character(e),
          result = "ERROR"
        )
      }
    )
  }

  # Difference Sign Test
  if ("difference_sign" %in% test_type) {
    tryCatch(
      {
        result <- randtests::difference.sign.test(x, ...)
        results$difference_sign <- list(
          description = "Difference Sign Test",
          p_value = result$p.value,
          statistic = result$statistic,
          result = if (is.na(result$p.value)) {
            "INCONCLUSIVE"
          } else if (result$p.value >= alpha) {
            "PASS"
          } else {
            "FAIL"
          },
          details = result$method
        )
      },
      error = function(e) {
        results$difference_sign <- list(
          description = "Difference Sign Test",
          error = as.character(e),
          result = "ERROR"
        )
      }
    )
  }

  # Turning Point Test
  if ("turning_point" %in% test_type) {
    tryCatch(
      {
        result <- randtests::turning.point.test(x, ...)
        results$turning_point <- list(
          description = "Turning Point Test",
          p_value = result$p.value,
          statistic = result$statistic,
          result = if (is.na(result$p.value)) {
            "INCONCLUSIVE"
          } else if (result$p.value >= alpha) {
            "PASS"
          } else {
            "FAIL"
          },
          details = result$method
        )
      },
      error = function(e) {
        results$turning_point <- list(
          description = "Turning Point Test",
          error = as.character(e),
          result = "ERROR"
        )
      }
    )
  }

  return(results)
}

#' Integrate External Tests into qiprng Suite
#'
#' Adds external package tests to the qiprng test suite results
#'
#' @param suite The qiprng test suite object
#' @param include_cryptrndtest Whether to include CryptRndTest tests
#' @param include_randtests Whether to include randtests tests
#' @return Updated suite with external test results
#' @export
run_external_wrapper_tests <- function(suite,
                                       include_cryptrndtest = TRUE,
                                       include_randtests = TRUE) {
  # Generate data if not already present
  n <- suite$config$external_sample_size %||% 1e5
  x <- suite$prng_func(n)

  # Initialize external results section
  if (is.null(suite$results$external_wrappers)) {
    suite$results$external_wrappers <- list()
  }

  # Run CryptRndTest suite
  if (include_cryptrndtest && has_cryptrndtest) {
    message("Running CryptRndTest suite...")
    crypto_results <- run_cryptrndtest_suite(x,
      alpha = suite$config$significance_level
    )
    suite$results$external_wrappers$cryptrndtest <- crypto_results
  }

  # Run randtests suite
  if (include_randtests && has_randtests) {
    message("Running randtests suite...")
    rand_results <- run_randtests_suite(x,
      alpha = suite$config$significance_level
    )
    suite$results$external_wrappers$randtests <- rand_results
  }

  # Summary statistics
  all_results <- list()

  # Collect CryptRndTest results
  if (include_cryptrndtest && has_cryptrndtest && !is.null(crypto_results)) {
    for (test_name in names(crypto_results)) {
      if (!is.null(crypto_results[[test_name]]$result)) {
        all_results[[paste0("crypto_", test_name)]] <- crypto_results[[test_name]]
      }
    }
  }

  # Collect randtests results
  if (include_randtests && has_randtests && !is.null(rand_results)) {
    for (test_name in names(rand_results)) {
      if (!is.null(rand_results[[test_name]]$result)) {
        all_results[[paste0("rand_", test_name)]] <- rand_results[[test_name]]
      }
    }
  }

  # Calculate statistics
  passed <- sum(sapply(all_results, function(r) {
    identical(r$result, "PASS")
  }))
  failed <- sum(sapply(all_results, function(r) {
    identical(r$result, "FAIL")
  }))
  errors <- sum(sapply(all_results, function(r) {
    identical(r$result, "ERROR")
  }))

  suite$results$external_wrappers$summary <- list(
    total_tests = length(all_results),
    passed = passed,
    failed = failed,
    errors = errors,
    pass_rate = passed / length(all_results)
  )

  return(suite)
}

# Additional wrapper functions for comprehensive comparison framework

#' Wrapper for CryptRndTest suite (comprehensive framework)
#' @export
run_cryptrndtest_wrapper <- function(x, test_params = list()) {
  # Check if package is available
  if (!has_cryptrndtest) {
    return(list(
      results = list(error = "CryptRndTest package not installed"),
      summary = data.frame(
        test = "cryptrndtest",
        p_value = NA,
        statistic = NA,
        passed = FALSE,
        details = "Package not available",
        stringsAsFactors = FALSE
      )
    ))
  }

  tryCatch(
    {
      # Run CryptRndTest tests directly
      results <- list()

      # Load the package
      library(CryptRndTest)

      # Adaptive chi-square test
      tryCatch(
        {
          # Convert to integers
          x_int <- as.integer(x * 2^16)
          B <- 16 # Default block size
          S <- length(x_int) # Full sample
          results$adaptive_chi_square <- adaptive.chi.square(x_int, B = B, S = S)
        },
        error = function(e) NULL
      )

      # Birthday spacings test
      tryCatch(
        {
          x_int <- as.integer(x * 2^16)
          m <- floor(length(x)^(2 / 3))
          n_param <- 2^16
          lambda_param <- m^3 / (4 * n_param)
          results$birthday_spacings <- birthday.spacings(x_int,
            m = m, n = n_param,
            lambda = lambda_param
          )
        },
        error = function(e) NULL
      )

      # Book stack test
      tryCatch(
        {
          results$book_stack <- book.stack(x)
        },
        error = function(e) NULL
      )

      # GCD test
      tryCatch(
        {
          x_int <- as.integer(x * 2^32)
          # Remove odd length
          if (length(x_int) %% 2 == 1) {
            x_int <- x_int[-length(x_int)]
          }
          # Convert to N×2 matrix as required by GCD.test
          x_matrix <- matrix(x_int, ncol = 2, byrow = TRUE)
          # Filter out zeros for log calculation
          x_positive <- x_int[x_int > 0]
          if (length(x_positive) > 0) {
            mu_param <- mean(log(x_positive))
            sd_param <- sd(log(x_positive))
          } else {
            # Fallback values
            mu_param <- 10
            sd_param <- 3
          }
          results$gcd <- GCD.test(x_matrix, B = 32, mu = mu_param, sd = sd_param)
        },
        error = function(e) NULL
      )

      # Random walk tests
      tryCatch(
        {
          # Convert to bits first
          bits <- numeric()
          bytes <- as.raw(x * 255)
          for (byte in bytes) {
            byte_val <- as.integer(byte)
            for (i in 0:7) {
              bits <- c(bits, (byte_val %/% (2^i)) %% 2)
            }
          }
          results$random_walk <- random.walk.tests(bits, B = 64)
        },
        error = function(e) NULL
      )

      # Topological binary test
      tryCatch(
        {
          # Convert to 8-bit integers
          x_int <- as.integer(x * 255)
          B <- 8 # Default block size
          critical_value <- floor(length(x_int) * 0.95)
          results$topological_binary <- topological.binary(x_int,
            B = B,
            critical.value = critical_value
          )
        },
        error = function(e) NULL
      )

      # Convert to standardized format
      if (is.null(results) || length(results) == 0) {
        return(list(
          results = list(error = "No CryptRndTest results"),
          summary = data.frame(
            test = "cryptrndtest",
            p_value = NA,
            statistic = NA,
            passed = FALSE,
            details = "No results returned",
            stringsAsFactors = FALSE
          )
        ))
      }

      # Extract summary data
      summary_rows <- lapply(names(results), function(test_name) {
        test_result <- results[[test_name]]

        # Extract p-value and statistic
        p_val <- extract_p_value(test_result)
        stat <- extract_statistic(test_result)

        data.frame(
          test = paste0("crt_", test_name),
          p_value = p_val,
          statistic = stat,
          passed = !is.na(p_val) && p_val > 0.01,
          details = extract_method(test_result),
          stringsAsFactors = FALSE
        )
      })

      summary_df <- do.call(rbind, summary_rows)

      return(list(
        results = results,
        summary = summary_df
      ))
    },
    error = function(e) {
      list(
        results = list(error = toString(e)),
        summary = data.frame(
          test = "cryptrndtest",
          p_value = NA,
          statistic = NA,
          passed = FALSE,
          details = paste("Error:", toString(e)),
          stringsAsFactors = FALSE
        )
      )
    }
  )
}

#' Wrapper for randtests suite (comprehensive framework)
#' @export
run_randtests_wrapper <- function(x, test_params = list()) {
  # Check if package is available
  if (!has_randtests) {
    return(list(
      results = list(error = "randtests package not installed"),
      summary = data.frame(
        test = "randtests",
        p_value = NA,
        statistic = NA,
        passed = FALSE,
        details = "Package not available",
        stringsAsFactors = FALSE
      )
    ))
  }

  tryCatch(
    {
      # Run randtests tests directly
      results <- list()

      # Load the package
      library(randtests)

      # Bartels test
      if (exists("bartels.rank.test")) {
        results$bartels <- bartels.rank.test(x)
      }

      # Cox-Stuart test
      if (exists("cox.stuart.test")) {
        results$cox_stuart <- cox.stuart.test(x)
      }

      # Runs test
      if (exists("runs.test")) {
        results$runs <- runs.test(x)
      }

      # Turning points test
      if (exists("turning.point.test")) {
        results$turning_points <- turning.point.test(x)
      }

      # Convert to standardized format
      if (is.null(results) || length(results) == 0) {
        return(list(
          results = list(error = "No randtests results"),
          summary = data.frame(
            test = "randtests",
            p_value = NA,
            statistic = NA,
            passed = FALSE,
            details = "No results returned",
            stringsAsFactors = FALSE
          )
        ))
      }

      # Extract summary data
      summary_rows <- lapply(names(results), function(test_name) {
        test_result <- results[[test_name]]

        p_val <- extract_p_value(test_result)
        stat <- extract_statistic(test_result)

        data.frame(
          test = paste0("randtests_", test_name),
          p_value = p_val,
          statistic = stat,
          passed = !is.na(p_val) && p_val > 0.01,
          details = extract_method(test_result),
          stringsAsFactors = FALSE
        )
      })

      summary_df <- do.call(rbind, summary_rows)

      return(list(
        results = results,
        summary = summary_df
      ))
    },
    error = function(e) {
      list(
        results = list(error = toString(e)),
        summary = data.frame(
          test = "randtests",
          p_value = NA,
          statistic = NA,
          passed = FALSE,
          details = paste("Error:", toString(e)),
          stringsAsFactors = FALSE
        )
      )
    }
  )
}

#' Wrapper for tseries tests (comprehensive framework)
#' @export
run_tseries_wrapper <- function(x, test_params = list()) {
  # Check if package is available
  if (!requireNamespace("tseries", quietly = TRUE)) {
    return(list(
      results = list(error = "tseries package not installed"),
      summary = data.frame(
        test = "tseries",
        p_value = NA,
        statistic = NA,
        passed = FALSE,
        details = "Package not available",
        stringsAsFactors = FALSE
      )
    ))
  }

  tryCatch(
    {
      results <- list()

      # Load the package
      library(tseries)

      # Jarque-Bera test
      results$jarque_bera <- jarque.bera.test(x)

      # Box test (for autocorrelation)
      results$box_ljung <- Box.test(x, type = "Ljung-Box")

      # ADF test (stationarity)
      # Suppress warnings about p-value computation
      results$adf <- suppressWarnings(adf.test(x))

      # Convert to standardized format
      summary_rows <- lapply(names(results), function(test_name) {
        test_result <- results[[test_name]]

        p_val <- extract_p_value(test_result)
        stat <- extract_statistic(test_result)

        data.frame(
          test = paste0("tseries_", test_name),
          p_value = p_val,
          statistic = stat,
          passed = !is.na(p_val) && p_val > 0.01,
          details = extract_method(test_result),
          stringsAsFactors = FALSE
        )
      })

      summary_df <- do.call(rbind, summary_rows)

      return(list(
        results = results,
        summary = summary_df
      ))
    },
    error = function(e) {
      list(
        results = list(error = toString(e)),
        summary = data.frame(
          test = "tseries",
          p_value = NA,
          statistic = NA,
          passed = FALSE,
          details = paste("Error:", toString(e)),
          stringsAsFactors = FALSE
        )
      )
    }
  )
}

#' Wrapper for multi-dimensional 2D tests (comprehensive framework)
#' @export
run_multidim_2d_wrapper <- function(x, test_params = list()) {
  # Source multidim tests if needed
  if (!exists("run_2d_tests")) {
    tryCatch(
      {
        source("R/statisticaltests/multidim_tests_fixes.R")
      },
      error = function(e) {
        return(list(
          results = list(error = "2D test functions not available"),
          summary = data.frame(
            test = "multidim_2d",
            p_value = NA,
            statistic = NA,
            passed = FALSE,
            details = "Functions not found",
            stringsAsFactors = FALSE
          )
        ))
      }
    )
  }

  tryCatch(
    {
      # Need pairs of values for 2D tests
      n <- length(x)
      if (n %% 2 != 0) {
        x <- x[1:(n - 1)] # Make even
      }

      # Create 2D points
      x_coords <- x[seq(1, length(x), 2)]
      y_coords <- x[seq(2, length(x), 2)]

      # Run 2D tests
      results <- run_2d_tests(x_coords, y_coords)

      # Convert to standardized format
      summary_rows <- lapply(names(results), function(test_name) {
        test_result <- results[[test_name]]

        p_val <- if (!is.null(test_result$p_value)) test_result$p_value else NA
        stat <- if (!is.null(test_result$statistic)) {
          test_result$statistic
        } else if (!is.null(test_result$test_statistic)) {
          test_result$test_statistic
        } else {
          NA
        }

        data.frame(
          test = paste0("2d_", test_name),
          p_value = p_val,
          statistic = stat,
          passed = !is.na(p_val) && p_val > 0.01,
          details = if (!is.null(test_result$method)) test_result$method else "2D uniformity test",
          stringsAsFactors = FALSE
        )
      })

      summary_df <- do.call(rbind, summary_rows)

      return(list(
        results = results,
        summary = summary_df
      ))
    },
    error = function(e) {
      list(
        results = list(error = toString(e)),
        summary = data.frame(
          test = "multidim_2d",
          p_value = NA,
          statistic = NA,
          passed = FALSE,
          details = paste("Error:", toString(e)),
          stringsAsFactors = FALSE
        )
      )
    }
  )
}

#' Wrapper for multi-dimensional 3D tests (comprehensive framework)
#' @export
run_multidim_3d_wrapper <- function(x, test_params = list()) {
  # Source multidim tests if needed
  if (!exists("run_3d_tests")) {
    tryCatch(
      {
        source("R/statisticaltests/multidim_tests_fixes.R")
      },
      error = function(e) {
        return(list(
          results = list(error = "3D test functions not available"),
          summary = data.frame(
            test = "multidim_3d",
            p_value = NA,
            statistic = NA,
            passed = FALSE,
            details = "Functions not found",
            stringsAsFactors = FALSE
          )
        ))
      }
    )
  }

  tryCatch(
    {
      # Need triplets of values for 3D tests
      n <- length(x)
      n <- n - (n %% 3) # Make divisible by 3

      if (n < 3) {
        return(list(
          results = list(error = "Insufficient data for 3D tests"),
          summary = data.frame(
            test = "multidim_3d",
            p_value = NA,
            statistic = NA,
            passed = FALSE,
            details = "Need at least 3 values",
            stringsAsFactors = FALSE
          )
        ))
      }

      x <- x[1:n]

      # Create 3D points
      x_coords <- x[seq(1, n, 3)]
      y_coords <- x[seq(2, n, 3)]
      z_coords <- x[seq(3, n, 3)]

      # Run 3D tests
      results <- run_3d_tests(x_coords, y_coords, z_coords)

      # Convert to standardized format
      summary_rows <- lapply(names(results), function(test_name) {
        test_result <- results[[test_name]]

        p_val <- if (!is.null(test_result$p_value)) test_result$p_value else NA
        stat <- if (!is.null(test_result$statistic)) {
          test_result$statistic
        } else if (!is.null(test_result$test_statistic)) {
          test_result$test_statistic
        } else {
          NA
        }

        data.frame(
          test = paste0("3d_", test_name),
          p_value = p_val,
          statistic = stat,
          passed = !is.na(p_val) && p_val > 0.01,
          details = if (!is.null(test_result$method)) test_result$method else "3D uniformity test",
          stringsAsFactors = FALSE
        )
      })

      summary_df <- do.call(rbind, summary_rows)

      return(list(
        results = results,
        summary = summary_df
      ))
    },
    error = function(e) {
      list(
        results = list(error = toString(e)),
        summary = data.frame(
          test = "multidim_3d",
          p_value = NA,
          statistic = NA,
          passed = FALSE,
          details = paste("Error:", toString(e)),
          stringsAsFactors = FALSE
        )
      )
    }
  )
}

# Helper functions
extract_p_value <- function(test_result) {
  if (is.list(test_result)) {
    if (!is.null(test_result$p.value)) {
      return(test_result$p.value)
    }
    if (!is.null(test_result$p_value)) {
      return(test_result$p_value)
    }
  }
  return(NA)
}

extract_statistic <- function(test_result) {
  if (is.list(test_result)) {
    if (!is.null(test_result$statistic)) {
      return(as.numeric(test_result$statistic))
    }
    if (!is.null(test_result$test_statistic)) {
      return(test_result$test_statistic)
    }
  }
  return(NA)
}

extract_method <- function(test_result) {
  if (is.list(test_result) && !is.null(test_result$method)) {
    return(test_result$method)
  }
  return("")
}

# Default export
if (!exists("default_export")) {
  default_export <- list(
    has_cryptrndtest = has_cryptrndtest,
    has_randtests = has_randtests,
    run_cryptrndtest_suite = run_cryptrndtest_suite,
    run_randtests_suite = run_randtests_suite,
    run_external_wrapper_tests = run_external_wrapper_tests
  )
}
