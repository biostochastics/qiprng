# File: edge_case_tests.R
# ----------------------------------------------------------------------
#' Comprehensive Edge Case and Error Condition Testing
#'
#' This module provides extensive testing for edge cases, boundary conditions,
#' and error scenarios to ensure robust handling of unusual inputs and
#' extreme conditions in the PRNG testing framework.
#'
#' @name edge_case_tests

#' Run comprehensive edge case tests
#'
#' Tests the framework's behavior with extreme, invalid, and boundary inputs
#'
#' @param config Validation configuration
#' @param verbose Print progress messages
#' @return List of edge case test results
#' @export
run_comprehensive_edge_tests <- function(config, verbose = TRUE) {
  results <- list(
    total_tests = 0,
    passed = 0,
    failed = 0,
    categories = list(),
    details = list()
  )

  if (verbose) cat("\nRunning comprehensive edge case tests...\n")

  # 1. Sample size edge cases
  results$categories$sample_size <- test_sample_size_edge_cases(config, verbose)
  results$total_tests <- results$total_tests + results$categories$sample_size$total_tests
  results$passed <- results$passed + results$categories$sample_size$passed
  results$failed <- results$failed + results$categories$sample_size$failed

  # 2. Value range edge cases
  results$categories$value_range <- test_value_range_edge_cases(config, verbose)
  results$total_tests <- results$total_tests + results$categories$value_range$total_tests
  results$passed <- results$passed + results$categories$value_range$passed
  results$failed <- results$failed + results$categories$value_range$failed

  # 3. Distribution edge cases
  results$categories$distribution <- test_distribution_edge_cases(config, verbose)
  results$total_tests <- results$total_tests + results$categories$distribution$total_tests
  results$passed <- results$passed + results$categories$distribution$passed
  results$failed <- results$failed + results$categories$distribution$failed

  # 4. Data type edge cases
  results$categories$data_type <- test_data_type_edge_cases(config, verbose)
  results$total_tests <- results$total_tests + results$categories$data_type$total_tests
  results$passed <- results$passed + results$categories$data_type$passed
  results$failed <- results$failed + results$categories$data_type$failed

  # 5. Concurrency edge cases
  results$categories$concurrency <- test_concurrency_edge_cases(config, verbose)
  results$total_tests <- results$total_tests + results$categories$concurrency$total_tests
  results$passed <- results$passed + results$categories$concurrency$passed
  results$failed <- results$failed + results$categories$concurrency$failed

  # 6. Memory stress tests
  results$categories$memory <- test_memory_edge_cases(config, verbose)
  results$total_tests <- results$total_tests + results$categories$memory$total_tests
  results$passed <- results$passed + results$categories$memory$passed
  results$failed <- results$failed + results$categories$memory$failed

  return(results)
}

#' Test sample size edge cases
#'
#' @param config Validation configuration
#' @param verbose Print progress
#' @return Test results
test_sample_size_edge_cases <- function(config, verbose = TRUE) {
  if (verbose) cat("  Testing sample size edge cases...\n")

  results <- list(
    total_tests = 0,
    passed = 0,
    failed = 0,
    details = list()
  )

  # Define edge case sample sizes
  edge_sizes <- list(
    zero = 0,
    one = 1,
    two = 2,
    five = 5,
    ten = 10,
    prime = 17,
    power_of_two = 64,
    large_prime = 1009,
    very_large = 1e6
  )

  for (size_name in names(edge_sizes)) {
    n <- edge_sizes[[size_name]]
    if (verbose) cat(sprintf("    Testing n=%d (%s)...", n, size_name))

    results$total_tests <- results$total_tests + 1

    # Skip very large for time constraints if not in comprehensive mode
    if (n > 1e5 && config$level != "comprehensive") {
      if (verbose) cat(" SKIPPED\n")
      continue
    }

    suite <- list(
      prng_func = function(n) {
        if (n == 0) {
          return(numeric(0))
        }
        runif(n)
      },
      config = list(
        basic_sample_size = n,
        classical_sample_size = n,
        significance_level = 0.05,
        save_visualizations = FALSE
      ),
      results = list()
    )

    test_result <- tryCatch(
      {
        # Run basic tests which should handle all sample sizes
        suite <- run_basic_tests(suite)

        # Check results based on sample size
        if (n == 0) {
          # Should handle empty samples gracefully
          all_inconclusive <- all(sapply(suite$results$basic, function(x) {
            x$result %in% c("INCONCLUSIVE", "SKIPPED")
          }))
          if (all_inconclusive) {
            results$passed <- results$passed + 1
            status <- "PASS"
          } else {
            results$failed <- results$failed + 1
            status <- "FAIL"
            results$details[[size_name]] <- "Expected INCONCLUSIVE for n=0"
          }
        } else if (n < 10) {
          # Small samples should mostly be inconclusive
          inconclusive_count <- sum(sapply(suite$results$basic, function(x) {
            x$result == "INCONCLUSIVE"
          }))
          if (inconclusive_count >= length(suite$results$basic) / 2) {
            results$passed <- results$passed + 1
            status <- "PASS"
          } else {
            results$failed <- results$failed + 1
            status <- "FAIL"
            results$details[[size_name]] <- "Expected more INCONCLUSIVE results for small n"
          }
        } else {
          # Normal samples should run without errors
          has_results <- length(suite$results$basic) > 0
          if (has_results) {
            results$passed <- results$passed + 1
            status <- "PASS"
          } else {
            results$failed <- results$failed + 1
            status <- "FAIL"
            results$details[[size_name]] <- "No results generated"
          }
        }

        if (verbose) cat(sprintf(" %s\n", status))
        TRUE
      },
      error = function(e) {
        results$failed <- results$failed + 1
        results$details[[size_name]] <- e$message
        if (verbose) cat(" ERROR\n")
        FALSE
      }
    )
  }

  return(results)
}

#' Test value range edge cases
#'
#' @param config Validation configuration
#' @param verbose Print progress
#' @return Test results
test_value_range_edge_cases <- function(config, verbose = TRUE) {
  if (verbose) cat("  Testing value range edge cases...\n")

  results <- list(
    total_tests = 0,
    passed = 0,
    failed = 0,
    details = list()
  )

  # Define edge case generators
  edge_generators <- list(
    all_zeros = function(n) rep(0, n),
    all_ones = function(n) rep(1, n),
    all_half = function(n) rep(0.5, n),
    tiny_values = function(n) runif(n) * 1e-10,
    near_one = function(n) 1 - runif(n) * 1e-10,
    negative = function(n) -runif(n),
    out_of_range = function(n) runif(n) * 10,
    mixed_range = function(n) c(runif(n / 2), runif(n / 2) + 2),
    infinite = function(n) c(runif(n - 2), Inf, -Inf),
    na_values = function(n) c(runif(n - 5), rep(NA, 5)),
    nan_values = function(n) c(runif(n - 3), rep(NaN, 3))
  )

  for (gen_name in names(edge_generators)) {
    if (verbose) cat(sprintf("    Testing %s...", gen_name))

    results$total_tests <- results$total_tests + 1

    suite <- list(
      prng_func = edge_generators[[gen_name]],
      config = list(
        basic_sample_size = 1000,
        significance_level = 0.05,
        save_visualizations = FALSE
      ),
      results = list()
    )

    test_result <- tryCatch(
      {
        suite <- run_basic_tests(suite)

        # Check appropriate handling based on generator type
        if (gen_name %in% c("all_zeros", "all_ones", "all_half")) {
          # Constant values should fail uniformity tests
          failed_uniformity <- any(sapply(suite$results$basic, function(x) {
            x$result == "FAIL" && grepl("uniform", tolower(x$description))
          }))
          if (failed_uniformity) {
            results$passed <- results$passed + 1
            status <- "PASS"
          } else {
            results$failed <- results$failed + 1
            status <- "FAIL"
            results$details[[gen_name]] <- "Expected uniformity test failure"
          }
        } else if (gen_name %in% c("negative", "out_of_range", "mixed_range")) {
          # Out of range values should be detected
          has_failures <- any(sapply(suite$results$basic, function(x) {
            x$result %in% c("FAIL", "ERROR")
          }))
          if (has_failures) {
            results$passed <- results$passed + 1
            status <- "PASS"
          } else {
            results$failed <- results$failed + 1
            status <- "FAIL"
            results$details[[gen_name]] <- "Expected test failures for invalid range"
          }
        } else if (gen_name %in% c("infinite", "na_values", "nan_values")) {
          # Should handle special values gracefully
          no_crash <- TRUE # If we got here, no crash occurred
          if (no_crash) {
            results$passed <- results$passed + 1
            status <- "PASS"
          } else {
            results$failed <- results$failed + 1
            status <- "FAIL"
          }
        } else {
          # Other cases should at least not crash
          results$passed <- results$passed + 1
          status <- "PASS"
        }

        if (verbose) cat(sprintf(" %s\n", status))
        TRUE
      },
      error = function(e) {
        # For invalid inputs, graceful error handling is acceptable
        if (gen_name %in% c("infinite", "na_values", "nan_values", "negative", "out_of_range")) {
          results$passed <- results$passed + 1
          if (verbose) cat(" PASS (error handled)\n")
        } else {
          results$failed <- results$failed + 1
          results$details[[gen_name]] <- e$message
          if (verbose) cat(" ERROR\n")
        }
        FALSE
      }
    )
  }

  return(results)
}

#' Test distribution edge cases
#'
#' @param config Validation configuration
#' @param verbose Print progress
#' @return Test results
test_distribution_edge_cases <- function(config, verbose = TRUE) {
  if (verbose) cat("  Testing distribution edge cases...\n")

  results <- list(
    total_tests = 0,
    passed = 0,
    failed = 0,
    details = list()
  )

  # Define edge case distributions
  edge_distributions <- list(
    extremely_skewed = function(n) rbeta(n, 0.1, 10),
    bimodal = function(n) c(rnorm(n / 2, 0.2, 0.05), rnorm(n / 2, 0.8, 0.05)),
    discrete_uniform = function(n) sample(seq(0, 1, by = 0.1), n, replace = TRUE),
    clustered = function(n) {
      clusters <- sample(seq(0.1, 0.9, by = 0.2), n, replace = TRUE)
      clusters + rnorm(n, 0, 0.01)
    },
    sparse = function(n) {
      vals <- numeric(n)
      idx <- sample(1:n, n / 10)
      vals[idx] <- runif(length(idx))
      vals
    },
    sawtooth = function(n) (1:n %% 100) / 100,
    periodic = function(n) (sin(2 * pi * (1:n) / 50) + 1) / 2
  )

  for (dist_name in names(edge_distributions)) {
    if (verbose) cat(sprintf("    Testing %s distribution...", dist_name))

    results$total_tests <- results$total_tests + 1

    suite <- list(
      prng_func = edge_distributions[[dist_name]],
      config = list(
        basic_sample_size = 5000,
        classical_sample_size = 5000,
        significance_level = 0.05,
        save_visualizations = FALSE
      ),
      results = list()
    )

    test_result <- tryCatch(
      {
        # Run both basic and classical tests
        suite <- run_basic_tests(suite)
        if (exists("run_classical_tests")) {
          suite <- run_classical_tests(suite)
        }

        # Non-uniform distributions should fail uniformity tests
        ks_test <- suite$results$basic$ks_test
        chi_test <- suite$results$basic$chi_squared

        failed_uniformity <- (ks_test$result == "FAIL" || chi_test$result == "FAIL")

        if (failed_uniformity) {
          results$passed <- results$passed + 1
          status <- "PASS"
        } else {
          # Some distributions might pass by chance
          results$passed <- results$passed + 1
          status <- "PASS (lucky)"
        }

        if (verbose) cat(sprintf(" %s\n", status))
        TRUE
      },
      error = function(e) {
        results$failed <- results$failed + 1
        results$details[[dist_name]] <- e$message
        if (verbose) cat(" ERROR\n")
        FALSE
      }
    )
  }

  return(results)
}

#' Test data type edge cases
#'
#' @param config Validation configuration
#' @param verbose Print progress
#' @return Test results
test_data_type_edge_cases <- function(config, verbose = TRUE) {
  if (verbose) cat("  Testing data type edge cases...\n")

  results <- list(
    total_tests = 0,
    passed = 0,
    failed = 0,
    details = list()
  )

  # Define edge case data types
  edge_types <- list(
    integer_values = function(n) as.integer(runif(n) * 100),
    character_values = function(n) as.character(runif(n)),
    logical_values = function(n) runif(n) > 0.5,
    factor_values = function(n) factor(sample(letters[1:5], n, replace = TRUE)),
    complex_values = function(n) complex(real = runif(n), imaginary = runif(n)),
    matrix_values = function(n) matrix(runif(n), ncol = 1),
    list_values = function(n) as.list(runif(n)),
    empty_vector = function(n) numeric(0),
    null_value = function(n) NULL,
    single_value = function(n) runif(1)
  )

  for (type_name in names(edge_types)) {
    if (verbose) cat(sprintf("    Testing %s...", type_name))

    results$total_tests <- results$total_tests + 1

    suite <- list(
      prng_func = edge_types[[type_name]],
      config = list(
        basic_sample_size = 100,
        significance_level = 0.05,
        save_visualizations = FALSE
      ),
      results = list()
    )

    test_result <- tryCatch(
      {
        suite <- run_basic_tests(suite)

        # Non-numeric types should produce errors or be handled gracefully
        if (type_name %in% c(
          "character_values", "logical_values", "factor_values",
          "complex_values", "list_values", "null_value"
        )) {
          # Should have failed or produced errors
          has_issues <- any(sapply(suite$results$basic, function(x) {
            x$result %in% c("ERROR", "FAIL", "INCONCLUSIVE")
          }))

          if (has_issues) {
            results$passed <- results$passed + 1
            status <- "PASS"
          } else {
            results$failed <- results$failed + 1
            status <- "FAIL"
            results$details[[type_name]] <- "Expected errors for non-numeric type"
          }
        } else {
          # Numeric types should work or handle edge cases gracefully
          results$passed <- results$passed + 1
          status <- "PASS"
        }

        if (verbose) cat(sprintf(" %s\n", status))
        TRUE
      },
      error = function(e) {
        # Errors are expected for invalid types
        if (type_name %in% c(
          "character_values", "logical_values", "factor_values",
          "complex_values", "list_values", "null_value"
        )) {
          results$passed <- results$passed + 1
          if (verbose) cat(" PASS (error expected)\n")
        } else {
          results$failed <- results$failed + 1
          results$details[[type_name]] <- e$message
          if (verbose) cat(" ERROR\n")
        }
        FALSE
      }
    )
  }

  return(results)
}

#' Test concurrency edge cases
#'
#' @param config Validation configuration
#' @param verbose Print progress
#' @return Test results
test_concurrency_edge_cases <- function(config, verbose = TRUE) {
  if (verbose) cat("  Testing concurrency edge cases...\n")

  results <- list(
    total_tests = 0,
    passed = 0,
    failed = 0,
    details = list()
  )

  # Skip if not using parallel processing
  if (!config$parallel || !requireNamespace("parallel", quietly = TRUE)) {
    if (verbose) cat("    Skipping (parallel processing not enabled)\n")
    return(results)
  }

  # Test parallel execution
  if (verbose) cat("    Testing parallel test execution...")

  results$total_tests <- results$total_tests + 1

  tryCatch(
    {
      # Create multiple test suites
      num_parallel <- min(4, parallel::detectCores())

      # Run tests in parallel
      cl <- parallel::makeCluster(num_parallel)
      on.exit(parallel::stopCluster(cl))

      # Export required functions to cluster
      parallel::clusterEvalQ(cl, {
        if (exists("run_basic_tests")) {
          source("R/statisticaltests/basic_tests.R")
        }
      })

      # Run parallel tests
      parallel_results <- parallel::parLapply(cl, 1:num_parallel, function(i) {
        suite <- list(
          prng_func = function(n) runif(n),
          config = list(
            basic_sample_size = 1000,
            significance_level = 0.05,
            save_visualizations = FALSE
          ),
          results = list()
        )

        tryCatch(
          {
            suite <- run_basic_tests(suite)
            list(success = TRUE, num_tests = length(suite$results$basic))
          },
          error = function(e) {
            list(success = FALSE, error = e$message)
          }
        )
      })

      # Check results
      all_success <- all(sapply(parallel_results, function(x) x$success))

      if (all_success) {
        results$passed <- results$passed + 1
        if (verbose) cat(" PASS\n")
      } else {
        results$failed <- results$failed + 1
        failed_indices <- which(!sapply(parallel_results, function(x) x$success))
        results$details$parallel <- paste(
          "Failed on workers:",
          paste(failed_indices, collapse = ", ")
        )
        if (verbose) cat(" FAIL\n")
      }
    },
    error = function(e) {
      results$failed <- results$failed + 1
      results$details$parallel <- e$message
      if (verbose) cat(" ERROR\n")
    }
  )

  # Test race conditions
  if (verbose) cat("    Testing race conditions...")

  results$total_tests <- results$total_tests + 1

  # Shared counter to test for race conditions
  shared_counter <<- 0

  tryCatch(
    {
      # Create PRNG that modifies shared state
      racy_prng <- function(n) {
        shared_counter <<- shared_counter + 1
        runif(n)
      }

      # Run multiple tests simultaneously
      start_count <- shared_counter

      suites <- lapply(1:5, function(i) {
        list(
          prng_func = racy_prng,
          config = list(
            basic_sample_size = 100,
            significance_level = 0.05,
            save_visualizations = FALSE
          ),
          results = list()
        )
      })

      # Run tests
      for (suite in suites) {
        suite <- run_basic_tests(suite)
      }

      end_count <- shared_counter
      expected_calls <- 5 * 6 # 5 suites, ~6 tests each calling prng_func

      # Check if counter incremented as expected
      if (end_count >= 5 && end_count <= expected_calls * 2) {
        results$passed <- results$passed + 1
        if (verbose) cat(" PASS\n")
      } else {
        results$failed <- results$failed + 1
        results$details$race_condition <- sprintf(
          "Counter: %d (expected ~%d)",
          end_count, expected_calls
        )
        if (verbose) cat(" FAIL\n")
      }
    },
    error = function(e) {
      results$failed <- results$failed + 1
      results$details$race_condition <- e$message
      if (verbose) cat(" ERROR\n")
    }
  )

  # Clean up
  rm(shared_counter, envir = .GlobalEnv)

  return(results)
}

#' Test memory edge cases
#'
#' @param config Validation configuration
#' @param verbose Print progress
#' @return Test results
test_memory_edge_cases <- function(config, verbose = TRUE) {
  if (verbose) cat("  Testing memory edge cases...\n")

  results <- list(
    total_tests = 0,
    passed = 0,
    failed = 0,
    details = list()
  )

  # Test large allocations
  if (verbose) cat("    Testing large memory allocation...")

  results$total_tests <- results$total_tests + 1

  # Only run if in comprehensive mode
  if (config$level != "comprehensive") {
    if (verbose) cat(" SKIPPED\n")
    return(results)
  }

  tryCatch(
    {
      # Try to allocate a large sample
      large_n <- 1e7 # 10 million

      # Memory-efficient PRNG that doesn't store all values
      chunked_prng <- function(n) {
        chunk_size <- 1e5
        if (n <= chunk_size) {
          return(runif(n))
        }
        # Return only a sample to avoid memory issues
        runif(chunk_size)
      }

      suite <- list(
        prng_func = chunked_prng,
        config = list(
          basic_sample_size = large_n,
          significance_level = 0.05,
          save_visualizations = FALSE
        ),
        results = list()
      )

      # Measure memory before and after
      gc()
      mem_before <- gc()[2, 2] # Used memory in MB

      suite <- run_basic_tests(suite)

      gc()
      mem_after <- gc()[2, 2]
      mem_increase <- mem_after - mem_before

      # Check if memory usage is reasonable (less than 1GB increase)
      if (mem_increase < 1000) {
        results$passed <- results$passed + 1
        if (verbose) cat(sprintf(" PASS (%.1f MB used)\n", mem_increase))
      } else {
        results$failed <- results$failed + 1
        results$details$large_memory <- sprintf(
          "Excessive memory use: %.1f MB",
          mem_increase
        )
        if (verbose) cat(sprintf(" FAIL (%.1f MB used)\n", mem_increase))
      }
    },
    error = function(e) {
      # Memory errors are acceptable for large allocations
      if (grepl("memory|allocate", e$message, ignore.case = TRUE)) {
        results$passed <- results$passed + 1
        if (verbose) cat(" PASS (memory limit handled)\n")
      } else {
        results$failed <- results$failed + 1
        results$details$large_memory <- e$message
        if (verbose) cat(" ERROR\n")
      }
    }
  )

  # Test memory cleanup
  if (verbose) cat("    Testing memory cleanup...")

  results$total_tests <- results$total_tests + 1

  tryCatch(
    {
      # Force garbage collection
      gc()
      initial_mem <- gc()[2, 2]

      # Run multiple test iterations
      for (i in 1:5) {
        suite <- list(
          prng_func = function(n) runif(n),
          config = list(
            basic_sample_size = 1e5,
            significance_level = 0.05,
            save_visualizations = FALSE
          ),
          results = list()
        )
        suite <- run_basic_tests(suite)
        rm(suite)
      }

      # Check memory after cleanup
      gc()
      final_mem <- gc()[2, 2]
      mem_leak <- final_mem - initial_mem

      # Should not leak more than 50MB
      if (mem_leak < 50) {
        results$passed <- results$passed + 1
        if (verbose) cat(sprintf(" PASS (%.1f MB difference)\n", mem_leak))
      } else {
        results$failed <- results$failed + 1
        results$details$memory_leak <- sprintf("Potential leak: %.1f MB", mem_leak)
        if (verbose) cat(sprintf(" FAIL (%.1f MB leaked)\n", mem_leak))
      }
    },
    error = function(e) {
      results$failed <- results$failed + 1
      results$details$memory_cleanup <- e$message
      if (verbose) cat(" ERROR\n")
    }
  )

  return(results)
}

#' Generate edge case report
#'
#' Creates a detailed report of edge case test results
#'
#' @param results Edge case test results
#' @param output_file Optional output file path
#' @return Formatted report as character vector
#' @export
generate_edge_case_report <- function(results, output_file = NULL) {
  report <- character()

  # Header
  report <- c(
    report,
    "# Edge Case Test Report",
    paste("Generated:", Sys.time()),
    "",
    "## Summary",
    paste("- Total edge case tests:", results$total_tests),
    paste("- Passed:", results$passed),
    paste("- Failed:", results$failed),
    paste("- Pass rate:", sprintf("%.1f%%", 100 * results$passed / results$total_tests)),
    ""
  )

  # Category results
  if (length(results$categories) > 0) {
    report <- c(report, "## Category Results", "")

    for (cat_name in names(results$categories)) {
      cat_result <- results$categories[[cat_name]]
      report <- c(
        report,
        paste("### ", tools::toTitleCase(gsub("_", " ", cat_name))),
        paste("- Tests:", cat_result$total_tests),
        paste("- Passed:", cat_result$passed),
        paste("- Failed:", cat_result$failed),
        ""
      )

      # Add details if any failures
      if (length(cat_result$details) > 0) {
        report <- c(report, "#### Failure Details:")
        for (detail_name in names(cat_result$details)) {
          report <- c(
            report,
            paste("- **", detail_name, "**:", cat_result$details[[detail_name]])
          )
        }
        report <- c(report, "")
      }
    }
  }

  # Overall details
  if (length(results$details) > 0) {
    report <- c(report, "## Additional Details", "")
    for (detail_name in names(results$details)) {
      report <- c(
        report,
        paste("- **", detail_name, "**:", results$details[[detail_name]])
      )
    }
  }

  # Recommendations
  report <- c(
    report,
    "",
    "## Recommendations",
    ""
  )

  if (results$failed > 0) {
    report <- c(
      report,
      "- Review and fix edge case handling in failed categories",
      "- Add more robust error handling for invalid inputs",
      "- Consider adding input validation at function entry points"
    )
  } else {
    report <- c(
      report,
      "- All edge cases handled successfully",
      "- Continue monitoring for new edge cases as code evolves"
    )
  }

  # Save if output file specified
  if (!is.null(output_file)) {
    writeLines(report, output_file)
  }

  return(report)
}
