# File: internal_test_wrappers.R
# ----------------------------------------------------------------------
#' Wrappers for internal test suites for generator comparison
#'
#' This module provides unified wrappers for all internal test suites
#' to work with the comprehensive generator comparison framework.

# Source internal test modules
tryCatch({
  # Try to find the correct path
  test_files <- c("basic_tests.R", "classical_tests.R", "binary_tests.R", 
                  "compression_tests.R", "correlation_tests.R", "runs_tests.R",
                  "simple_test_implementations.R")
  
  # Check different possible locations
  for (file in test_files) {
    if (file.exists(file.path("R/statisticaltests", file))) {
      source(file.path("R/statisticaltests", file))
    } else if (file.exists(file)) {
      source(file)
    }
  }
}, error = function(e) {
  message("Note: Some internal test modules may not be available: ", e$message)
})

#' Wrapper for basic tests
#' @export
run_basic_tests_wrapper <- function(x, test_params = list()) {
  # Use simple implementation if available
  if (exists("simple_basic_tests")) {
    tryCatch({
      results <- simple_basic_tests(x)
      
      # Convert to standardized format
      summary_df <- data.frame(
        test = names(results),
        p_value = sapply(results, function(r) r$p_value),
        statistic = sapply(results, function(r) as.numeric(r$statistic)),
        passed = sapply(results, function(r) r$result == "PASS"),
        details = sapply(results, function(r) r$description),
        stringsAsFactors = FALSE
      )
      
      return(list(results = results, summary = summary_df))
      
    }, error = function(e) {
      # Fall back to suite-based implementation if simple fails
      if (exists("run_basic_tests")) {
        return(run_basic_tests_with_suite(x, test_params))
      } else {
        return(list(
          results = list(error = toString(e)),
          summary = data.frame(
            test = "basic_tests",
            p_value = NA,
            statistic = NA,
            passed = FALSE,
            details = paste("Error:", toString(e)),
            stringsAsFactors = FALSE
          )
        ))
      }
    })
  } else if (exists("run_basic_tests")) {
    return(run_basic_tests_with_suite(x, test_params))
  } else {
    return(list(
      results = list(error = "Basic tests not available"),
      summary = data.frame(
        test = "basic_tests",
        p_value = NA,
        statistic = NA,
        passed = FALSE,
        details = "Module not found",
        stringsAsFactors = FALSE
      )
    ))
  }
}

# Helper function for suite-based tests
run_basic_tests_with_suite <- function(x, test_params) {
  tryCatch({
    # Create a minimal suite object that basic_tests expects
    suite <- list(
      prng_func = function(n) {
        if (n <= length(x)) {
          x[1:n]
        } else {
          rep(x, length.out = n)
        }
      },
      config = list(
        basic_sample_size = length(x),
        significance_level = 0.05,
        chi_squared_bins = min(20, max(5, floor(length(x)/5))),
        save_visualizations = FALSE
      ),
      results = list()
    )
    
    # Run the tests
    suite <- run_basic_tests(suite)
    results <- suite$results$basic
    
    # Convert to standardized format
    summary_df <- data.frame(
      test = names(results),
      p_value = sapply(results, function(r) {
        if (is.list(r) && "p_value" %in% names(r)) r$p_value
        else NA
      }),
      statistic = sapply(results, function(r) {
        if (is.list(r) && "statistic" %in% names(r)) as.numeric(r$statistic)
        else NA
      }),
      passed = sapply(results, function(r) {
        if (is.list(r) && "result" %in% names(r)) r$result == "PASS"
        else FALSE
      }),
      details = sapply(results, function(r) {
        if (is.list(r) && "description" %in% names(r)) r$description
        else ""
      }),
      stringsAsFactors = FALSE
    )
    
    return(list(results = results, summary = summary_df))
    
  }, error = function(e) {
    list(
      results = list(error = toString(e)),
      summary = data.frame(
        test = "basic_tests",
        p_value = NA,
        statistic = NA,
        passed = FALSE,
        details = paste("Error:", toString(e)),
        stringsAsFactors = FALSE
      )
    )
  })
}

#' Wrapper for classical tests
#' @export
run_classical_tests_wrapper <- function(x, test_params = list()) {
  if (!exists("run_classical_tests")) {
    return(list(
      results = list(error = "Classical tests not available"),
      summary = data.frame(
        test = "classical_tests",
        p_value = NA,
        statistic = NA,
        passed = FALSE,
        details = "Module not found"
      )
    ))
  }
  
  # Set appropriate parameters for sample size
  n <- length(x)
  params <- list(
    num_categories = test_params$num_categories %||% min(20, max(5, floor(sqrt(n)))),
    num_people = test_params$num_people %||% min(100, floor(n/100)),
    hand_size = test_params$hand_size %||% 5,
    num_hands = test_params$num_hands %||% min(floor(n/5), 10000)
  )
  
  tryCatch({
    # Create suite object for classical tests
    suite <- list(
      prng_func = function(n_req) {
        if (n_req <= length(x)) {
          x[1:n_req]
        } else {
          rep(x, length.out = n_req)
        }
      },
      config = list(
        classical_sample_size = length(x),
        coupon_collector_categories = params$num_categories,
        poker_hand_size = params$hand_size,
        poker_hands = params$num_hands,
        birthday_spacing_people = params$num_people,
        significance_level = 0.05
      ),
      results = list()
    )
    
    # Run the tests
    suite <- run_classical_tests(suite)
    results <- suite$results$classical
    
    # Convert to standardized format
    test_names <- names(results)
    summary_df <- data.frame(
      test = test_names,
      p_value = sapply(results, function(r) {
        if (is.list(r) && "p_value" %in% names(r)) r$p_value
        else NA
      }),
      statistic = sapply(results, function(r) {
        if (is.list(r) && "statistic" %in% names(r)) as.numeric(r$statistic)
        else NA
      }),
      passed = sapply(results, function(r) {
        if (is.list(r) && "result" %in% names(r)) r$result == "PASS"
        else FALSE
      }),
      details = sapply(results, function(r) {
        if (is.list(r) && "description" %in% names(r)) r$description
        else if (is.list(r) && "details" %in% names(r)) r$details
        else ""
      }),
      stringsAsFactors = FALSE
    )
    
    return(list(results = results, summary = summary_df))
    
  }, error = function(e) {
    list(
      results = list(error = toString(e)),
      summary = data.frame(
        test = "classical_tests",
        p_value = NA,
        statistic = NA,
        passed = FALSE,
        details = paste("Error:", toString(e)),
        stringsAsFactors = FALSE
      )
    )
  })
}

#' Wrapper for binary tests
#' @export
run_binary_tests_wrapper <- function(x, test_params = list()) {
  # Use simple implementation if available
  if (exists("simple_binary_tests")) {
    tryCatch({
      results <- simple_binary_tests(x)
      
      # Convert to standardized format
      summary_df <- data.frame(
        test = names(results),
        p_value = sapply(results, function(r) r$p_value),
        statistic = sapply(results, function(r) as.numeric(r$statistic)),
        passed = sapply(results, function(r) r$result == "PASS"),
        details = sapply(results, function(r) r$description),
        stringsAsFactors = FALSE
      )
      
      return(list(results = results, summary = summary_df))
      
    }, error = function(e) {
      return(list(
        results = list(error = toString(e)),
        summary = data.frame(
          test = "binary_tests",
          p_value = NA,
          statistic = NA,
          passed = FALSE,
          details = paste("Error:", toString(e)),
          stringsAsFactors = FALSE
        )
      ))
    })
  } else if (exists("run_binary_tests")) {
    # Fall back to original implementation
    tryCatch({
      # Convert to binary if needed
      if (all(x >= 0 & x <= 1)) {
        binary_x <- as.integer(x > 0.5)
      } else {
        # Assume integers, convert to binary
        binary_x <- as.integer(x %% 2)
      }
      
      results <- run_binary_tests(binary_x)
      
      # Extract summary info
      summary_df <- data.frame(
        test = names(results),
        p_value = sapply(results, function(r) {
          if (is.list(r) && "p_value" %in% names(r)) r$p_value
          else if (is.list(r) && "p.value" %in% names(r)) r$p.value
          else NA
        }),
        statistic = sapply(results, function(r) {
          if (is.list(r) && "statistic" %in% names(r)) r$statistic
          else if (is.list(r) && "test_statistic" %in% names(r)) r$test_statistic
          else NA
        }),
        passed = sapply(results, function(r) {
          p <- if (is.list(r) && "p_value" %in% names(r)) r$p_value
               else if (is.list(r) && "p.value" %in% names(r)) r$p.value
               else NA
          !is.na(p) && p > 0.01
        }),
        details = "Binary sequence test",
        stringsAsFactors = FALSE
      )
      
      return(list(results = results, summary = summary_df))
      
    }, error = function(e) {
      list(
        results = list(error = toString(e)),
        summary = data.frame(
          test = "binary_tests",
          p_value = NA,
          statistic = NA,
          passed = FALSE,
          details = paste("Error:", toString(e)),
          stringsAsFactors = FALSE
        )
      )
    })
  } else {
    return(list(
      results = list(error = "Binary tests not available"),
      summary = data.frame(
        test = "binary_tests",
        p_value = NA,
        statistic = NA,
        passed = FALSE,
        details = "Module not found",
        stringsAsFactors = FALSE
      )
    ))
  }
}

#' Wrapper for compression tests
#' @export
run_compression_tests_wrapper <- function(x, test_params = list()) {
  # Check for bootstrap version first
  if (exists("run_compression_tests_bootstrap")) {
    return(run_compression_bootstrap_wrapper(x, test_params))
  }
  
  if (!exists("run_compression_tests")) {
    return(list(
      results = list(error = "Compression tests not available"),
      summary = data.frame(
        test = "compression_tests",
        p_value = NA,
        statistic = NA,
        passed = FALSE,
        details = "Module not found"
      )
    ))
  }
  
  tryCatch({
    results <- run_compression_tests(x)
    
    # Extract summary
    summary_df <- data.frame(
      test = "compression_ratio",
      p_value = if (!is.null(results$p_value)) results$p_value else NA,
      statistic = if (!is.null(results$compression_ratio)) results$compression_ratio else NA,
      passed = if (!is.null(results$passed)) results$passed else FALSE,
      details = if (!is.null(results$interpretation)) results$interpretation else ""
    )
    
    return(list(results = results, summary = summary_df))
    
  }, error = function(e) {
    list(
      results = list(error = toString(e)),
      summary = data.frame(
        test = "compression_tests",
        p_value = NA,
        statistic = NA,
        passed = FALSE,
        details = paste("Error:", toString(e))
      )
    )
  })
}

#' Wrapper for compression bootstrap tests
#' @export
run_compression_bootstrap_wrapper <- function(x, test_params = list()) {
  if (!exists("run_compression_tests_bootstrap")) {
    return(list(
      results = list(error = "Compression bootstrap tests not available"),
      summary = data.frame(
        test = "compression_bootstrap",
        p_value = NA,
        statistic = NA,
        passed = FALSE,
        details = "Module not found"
      )
    ))
  }
  
  # Configure parameters
  config <- list(
    n_bootstrap = test_params$n_bootstrap %||% 100,
    block_size = test_params$block_size %||% 1000,
    methods = test_params$methods %||% c("gzip", "bzip2", "xz")
  )
  
  tryCatch({
    results <- run_compression_tests_bootstrap(
      x,
      n_bootstrap = config$n_bootstrap,
      sample_config = list(block_size = config$block_size),
      methods = config$methods
    )
    
    # Extract summary for each method
    summary_rows <- lapply(names(results), function(method) {
      res <- results[[method]]
      data.frame(
        test = paste("compression", method, sep = "_"),
        p_value = if (!is.null(res$p_value)) res$p_value else NA,
        statistic = if (!is.null(res$observed_ratio)) res$observed_ratio else NA,
        passed = if (!is.null(res$passed)) res$passed else FALSE,
        details = paste("CI:", 
          if (!is.null(res$confidence_interval)) 
            paste0("[", round(res$confidence_interval[1], 4), ", ", 
                   round(res$confidence_interval[2], 4), "]")
          else "NA"
        )
      )
    })
    
    summary_df <- do.call(rbind, summary_rows)
    return(list(results = results, summary = summary_df))
    
  }, error = function(e) {
    list(
      results = list(error = toString(e)),
      summary = data.frame(
        test = "compression_bootstrap",
        p_value = NA,
        statistic = NA,
        passed = FALSE,
        details = paste("Error:", toString(e))
      )
    )
  })
}

#' Wrapper for correlation tests
#' @export
run_correlation_tests_wrapper <- function(x, test_params = list()) {
  # Set lag parameters
  max_lag <- test_params$max_lag %||% min(10, floor(length(x)/10))
  
  # Use simple implementation if available
  if (exists("simple_correlation_tests")) {
    tryCatch({
      results <- simple_correlation_tests(x, max_lag = max_lag)
      
      # Convert to standardized format
      summary_df <- data.frame(
        test = names(results),
        p_value = sapply(results, function(r) r$p_value),
        statistic = sapply(results, function(r) as.numeric(r$statistic)),
        passed = sapply(results, function(r) r$result == "PASS"),
        details = sapply(results, function(r) r$description),
        stringsAsFactors = FALSE
      )
      
      return(list(results = results, summary = summary_df))
      
    }, error = function(e) {
      return(list(
        results = list(error = toString(e)),
        summary = data.frame(
          test = "correlation_tests",
          p_value = NA,
          statistic = NA,
          passed = FALSE,
          details = paste("Error:", toString(e)),
          stringsAsFactors = FALSE
        )
      ))
    })
  } else if (exists("run_correlation_tests")) {
    tryCatch({
      results <- run_correlation_tests(x, max_lag = max_lag)
      
      # Extract key metrics
      summary_df <- data.frame(
        test = c("serial_correlation", "ljung_box", "max_correlation"),
        p_value = c(
          if (!is.null(results$serial_test$p_value)) results$serial_test$p_value else NA,
          if (!is.null(results$ljung_box$p.value)) results$ljung_box$p.value else NA,
          NA  # Max correlation doesn't have p-value
        ),
        statistic = c(
          if (!is.null(results$serial_test$correlation)) results$serial_test$correlation else NA,
          if (!is.null(results$ljung_box$statistic)) results$ljung_box$statistic else NA,
          if (!is.null(results$max_abs_correlation)) results$max_abs_correlation else NA
        ),
        passed = c(
          !is.null(results$serial_test$p_value) && results$serial_test$p_value > 0.01,
          !is.null(results$ljung_box$p.value) && results$ljung_box$p.value > 0.01,
          !is.null(results$max_abs_correlation) && results$max_abs_correlation < 0.1
        ),
        details = c(
          "Lag-1 serial correlation",
          paste("Ljung-Box test, max lag:", max_lag),
          paste("Max absolute correlation across all lags")
        ),
        stringsAsFactors = FALSE
      )
      
      return(list(results = results, summary = summary_df))
      
    }, error = function(e) {
      list(
        results = list(error = toString(e)),
        summary = data.frame(
          test = "correlation_tests",
          p_value = NA,
          statistic = NA,
          passed = FALSE,
          details = paste("Error:", toString(e)),
          stringsAsFactors = FALSE
        )
      )
    })
  } else {
    return(list(
      results = list(error = "Correlation tests not available"),
      summary = data.frame(
        test = "correlation_tests",
        p_value = NA,
        statistic = NA,
        passed = FALSE,
        details = "Module not found",
        stringsAsFactors = FALSE
      )
    ))
  }
}

#' Wrapper for runs tests
#' @export
run_runs_tests_wrapper <- function(x, test_params = list()) {
  # Use simple implementation if available
  if (exists("simple_runs_test")) {
    tryCatch({
      results <- simple_runs_test(x)
      
      # Convert to standardized format
      summary_df <- data.frame(
        test = names(results),
        p_value = sapply(results, function(r) r$p_value),
        statistic = sapply(results, function(r) as.numeric(r$statistic)),
        passed = sapply(results, function(r) r$result == "PASS"),
        details = sapply(results, function(r) r$description),
        stringsAsFactors = FALSE
      )
      
      return(list(results = results, summary = summary_df))
      
    }, error = function(e) {
      return(list(
        results = list(error = toString(e)),
        summary = data.frame(
          test = "runs_tests",
          p_value = NA,
          statistic = NA,
          passed = FALSE,
          details = paste("Error:", toString(e)),
          stringsAsFactors = FALSE
        )
      ))
    })
  } else if (exists("run_runs_tests")) {
    tryCatch({
      results <- run_runs_tests(x)
      
      # Extract summary
      summary_df <- data.frame(
        test = names(results),
        p_value = sapply(results, function(r) {
          if (is.list(r) && "p.value" %in% names(r)) r$p.value
          else if (is.list(r) && "p_value" %in% names(r)) r$p_value
          else NA
        }),
        statistic = sapply(results, function(r) {
          if (is.list(r) && "statistic" %in% names(r)) r$statistic
          else if (is.list(r) && "test_statistic" %in% names(r)) r$test_statistic
          else NA
        }),
        passed = sapply(results, function(r) {
          p <- if (is.list(r) && "p.value" %in% names(r)) r$p.value
               else if (is.list(r) && "p_value" %in% names(r)) r$p_value
               else NA
          !is.na(p) && p > 0.01
        }),
        details = "Runs-based randomness test",
        stringsAsFactors = FALSE
      )
      
      return(list(results = results, summary = summary_df))
      
    }, error = function(e) {
      list(
        results = list(error = toString(e)),
        summary = data.frame(
          test = "runs_tests",
          p_value = NA,
          statistic = NA,
          passed = FALSE,
          details = paste("Error:", toString(e)),
          stringsAsFactors = FALSE
        )
      )
    })
  } else {
    return(list(
      results = list(error = "Runs tests not available"),
      summary = data.frame(
        test = "runs_tests",
        p_value = NA,
        statistic = NA,
        passed = FALSE,
        details = "Module not found",
        stringsAsFactors = FALSE
      )
    ))
  }
}

# Helper function
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}