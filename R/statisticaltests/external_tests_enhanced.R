# File: external_tests_enhanced.R
# ----------------------------------------------------------------------
#' Enhanced External tests for PRNG quality with Configuration Management
#'
#' This module provides enhanced wrappers for external test tools
#' with flexible configuration management support.

# Source configuration manager
config_manager_path <- system.file("R/statisticaltests/config_manager.R", package = "qiprng")
if (config_manager_path == "") {
  if (file.exists("config_manager.R")) {
    source("config_manager.R")
  } else if (file.exists("R/statisticaltests/config_manager.R")) {
    source("R/statisticaltests/config_manager.R")
  }
} else if (file.exists(config_manager_path)) {
  source(config_manager_path)
}

#' Run enhanced external tests with configuration management
#'
#' @param suite The test suite object
#' @param config_file Optional path to configuration file
#' @param tools Character vector of tools to run (NULL = all enabled)
#' @return Updated test suite with results
#' @export
run_external_tests_enhanced <- function(suite, config_file = NULL, tools = NULL) {
  # Load configuration
  config <- load_config(config_file)
  
  # Generate random numbers
  n <- suite$config$external_sample_size
  x <- suite$prng_func(n)
  
  # Initialize results
  suite$results$external <- list()
  suite$results$external_enhanced <- list()
  
  # Determine which tools to run
  if (is.null(tools)) {
    tools <- names(config)[sapply(config, function(tc) isTRUE(tc$enabled))]
  }
  
  # Create temporary directory for test files
  temp_dir <- tempfile("qiprng_external_")
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
  
  # Run each enabled tool
  for (tool in tools) {
    if (!tool %in% names(config)) {
      warning(paste("Tool", tool, "not found in configuration"))
      next
    }
    
    if (!validate_tool_config(tool, config)) {
      suite$results$external_enhanced[[tool]] <- list(
        description = paste(toupper(tool), "Test Suite"),
        result = "SKIPPED",
        details = "Tool not available or not enabled"
      )
      next
    }
    
    # Run tool-specific tests
    suite <- switch(tool,
      dieharder = run_dieharder_enhanced(suite, x, config, temp_dir),
      ent = run_ent_enhanced(suite, x, config, temp_dir),
      nist_sts = run_nist_enhanced(suite, x, config, temp_dir),
      testu01 = run_testu01_enhanced(suite, x, config, temp_dir),
      practrand = run_practrand_enhanced(suite, x, config, temp_dir),
      {
        warning(paste("Unknown tool:", tool))
        suite
      }
    )
  }
  
  # Run wrapper-based tests (backward compatibility)
  suite <- run_external_wrapper_tests(suite, 
                                      include_cryptrndtest = TRUE,
                                      include_randtests = TRUE)
  
  # Merge results
  if (!is.null(suite$results$external_wrappers)) {
    merge_wrapper_results(suite)
  }
  
  # Generate enhanced visualizations if requested
  if (suite$config$save_visualizations) {
    suite <- visualize_external_tests_enhanced(suite, x)
  }
  
  return(suite)
}

#' Run Dieharder tests with configuration
#' @keywords internal
run_dieharder_enhanced <- function(suite, x, config, temp_dir) {
  tool_config <- config$dieharder
  
  # Write data to file
  data_file <- file.path(temp_dir, "dieharder_input.dat")
  write_binary_data(x, data_file)
  
  # Build command
  cmd <- build_tool_command("dieharder", data_file, config = config)
  
  # Run tests
  if (suite$config$verbose) {
    message("Running Dieharder tests...")
  }
  
  tryCatch({
    output <- system(cmd, intern = TRUE)
    results <- parse_dieharder_output(output)
    
    suite$results$external_enhanced$dieharder <- list(
      description = "Dieharder Test Suite",
      result = calculate_overall_result(results),
      total_tests = length(results),
      passed = sum(sapply(results, function(r) r$result == "PASS")),
      failed = sum(sapply(results, function(r) r$result == "FAIL")),
      weak = sum(sapply(results, function(r) r$result == "WEAK")),
      details = results,
      command = cmd
    )
  }, error = function(e) {
    suite$results$external_enhanced$dieharder <- list(
      description = "Dieharder Test Suite",
      result = "ERROR",
      details = paste("Error running Dieharder:", e$message),
      command = cmd
    )
  })
  
  return(suite)
}

#' Run ENT tests with configuration
#' @keywords internal
run_ent_enhanced <- function(suite, x, config, temp_dir) {
  tool_config <- config$ent
  
  # Write data to file
  data_file <- file.path(temp_dir, "ent_input.dat")
  if (isTRUE(tool_config$parameters$binary_mode)) {
    write_binary_data(x, data_file)
  } else {
    write_text_data(x, data_file)
  }
  
  # Build command
  cmd <- build_tool_command("ent", data_file, config = config)
  
  # Run tests
  if (suite$config$verbose) {
    message("Running ENT tests...")
  }
  
  tryCatch({
    output <- system(cmd, intern = TRUE)
    results <- parse_ent_output(output)
    
    suite$results$external_enhanced$ent <- list(
      description = "ENT - Entropy Test",
      result = evaluate_ent_results(results),
      entropy = results$entropy,
      chi_square = results$chi_square,
      arithmetic_mean = results$arithmetic_mean,
      monte_carlo_pi = results$monte_carlo_pi,
      serial_correlation = results$serial_correlation,
      details = results,
      command = cmd
    )
  }, error = function(e) {
    suite$results$external_enhanced$ent <- list(
      description = "ENT - Entropy Test",
      result = "ERROR",
      details = paste("Error running ENT:", e$message),
      command = cmd
    )
  })
  
  return(suite)
}

#' Write binary data to file
#' @keywords internal
write_binary_data <- function(x, file_path) {
  # Convert [0,1) to bytes
  bytes <- as.integer(x * 256) %% 256
  writeBin(as.raw(bytes), file_path)
}

#' Write text data to file
#' @keywords internal
write_text_data <- function(x, file_path) {
  write.table(x, file_path, row.names = FALSE, col.names = FALSE)
}

#' Parse Dieharder output
#' @keywords internal
parse_dieharder_output <- function(output) {
  results <- list()
  
  # Look for test result lines
  result_pattern <- "^\\s*([^|]+)\\|\\s*(\\d+)\\|\\s*([\\d.]+)\\|\\s*([\\d.]+)\\|\\s*(PASSED|WEAK|FAILED)"
  
  for (line in output) {
    if (grepl(result_pattern, line)) {
      matches <- regmatches(line, regexec(result_pattern, line))[[1]]
      if (length(matches) > 5) {
        test_name <- trimws(matches[2])
        results[[test_name]] <- list(
          name = test_name,
          ntup = as.numeric(matches[3]),
          tsamples = as.numeric(matches[4]),
          pvalue = as.numeric(matches[5]),
          result = matches[6]
        )
      }
    }
  }
  
  return(results)
}

#' Parse ENT output
#' @keywords internal
parse_ent_output <- function(output) {
  results <- list()
  
  for (line in output) {
    if (grepl("Entropy =", line)) {
      results$entropy <- as.numeric(gsub(".*Entropy = ([\\d.]+).*", "\\1", line))
    } else if (grepl("Chi square:", line)) {
      results$chi_square <- list(
        value = as.numeric(gsub(".*Chi square: ([\\d.]+).*", "\\1", line)),
        percent = as.numeric(gsub(".*\\(([\\d.]+)%.*", "\\1", line))
      )
    } else if (grepl("Arithmetic mean value", line)) {
      results$arithmetic_mean <- as.numeric(gsub(".*value of data bytes is ([\\d.]+).*", "\\1", line))
    } else if (grepl("Monte Carlo value for Pi", line)) {
      results$monte_carlo_pi <- list(
        value = as.numeric(gsub(".*Pi is ([\\d.]+).*", "\\1", line)),
        error = as.numeric(gsub(".*error ([\\d.]+).*", "\\1", line))
      )
    } else if (grepl("Serial correlation coefficient", line)) {
      results$serial_correlation <- as.numeric(gsub(".*coefficient is ([\\d.-]+).*", "\\1", line))
    }
  }
  
  return(results)
}

#' Calculate overall result from individual test results
#' @keywords internal
calculate_overall_result <- function(results) {
  if (length(results) == 0) return("ERROR")
  
  failed <- sum(sapply(results, function(r) r$result == "FAIL"))
  weak <- sum(sapply(results, function(r) r$result == "WEAK"))
  
  if (failed > 0) return("FAIL")
  if (weak > 2) return("WEAK")  # Allow up to 2 weak results
  return("PASS")
}

#' Evaluate ENT results
#' @keywords internal
evaluate_ent_results <- function(results) {
  # Ideal values for random data
  ideal_entropy <- 8.0
  ideal_mean <- 127.5
  ideal_chi_square_percent <- 50.0
  ideal_serial_correlation <- 0.0
  
  # Calculate deviations
  entropy_ok <- !is.null(results$entropy) && results$entropy > 7.9
  mean_ok <- !is.null(results$arithmetic_mean) && 
             abs(results$arithmetic_mean - ideal_mean) < 10
  chi_square_ok <- !is.null(results$chi_square) && 
                   results$chi_square$percent > 10 && 
                   results$chi_square$percent < 90
  correlation_ok <- !is.null(results$serial_correlation) && 
                    abs(results$serial_correlation) < 0.1
  
  if (all(c(entropy_ok, mean_ok, chi_square_ok, correlation_ok))) {
    return("PASS")
  } else if (sum(c(entropy_ok, mean_ok, chi_square_ok, correlation_ok)) >= 3) {
    return("WEAK")
  } else {
    return("FAIL")
  }
}

#' Merge wrapper results into external results
#' @keywords internal
merge_wrapper_results <- function(suite) {
  wrapper_results <- suite$results$external_wrappers
  
  # Add CryptRndTest results
  if (!is.null(wrapper_results$cryptrndtest)) {
    for (test_name in names(wrapper_results$cryptrndtest)) {
      suite$results$external[[test_name]] <- wrapper_results$cryptrndtest[[test_name]]
    }
  }
  
  # Add randtests results
  if (!is.null(wrapper_results$randtests)) {
    for (test_name in names(wrapper_results$randtests)) {
      suite$results$external[[test_name]] <- wrapper_results$randtests[[test_name]]
    }
  }
}

#' Enhanced visualization of external test results
#' @keywords internal
visualize_external_tests_enhanced <- function(suite, x) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    return(suite)
  }
  
  # Set up output directory
  output_dir <- file.path(suite$config$output_dir, "visualizations", "external_enhanced")
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Create summary dashboard
  create_external_test_dashboard(suite, output_dir)
  
  # Store visualization paths
  suite$visualizations$external_enhanced <- list(
    dashboard = file.path(output_dir, "external_test_dashboard.png")
  )
  
  return(suite)
}

#' Create external test dashboard
#' @keywords internal
create_external_test_dashboard <- function(suite, output_dir) {
  # Collect all test results
  test_summary <- data.frame(
    Tool = character(),
    Test = character(),
    Result = character(),
    stringsAsFactors = FALSE
  )
  
  # Enhanced results
  for (tool in names(suite$results$external_enhanced)) {
    tool_results <- suite$results$external_enhanced[[tool]]
    test_summary <- rbind(test_summary, data.frame(
      Tool = tool,
      Test = tool_results$description,
      Result = tool_results$result,
      stringsAsFactors = FALSE
    ))
  }
  
  # Regular external results
  for (test_name in names(suite$results$external)) {
    test <- suite$results$external[[test_name]]
    test_summary <- rbind(test_summary, data.frame(
      Tool = "Built-in",
      Test = test$description,
      Result = test$result,
      stringsAsFactors = FALSE
    ))
  }
  
  if (nrow(test_summary) > 0) {
    # Create summary plot
    p <- ggplot2::ggplot(test_summary, ggplot2::aes(x = Test, y = Tool, fill = Result)) +
      ggplot2::geom_tile(color = "white", size = 1) +
      ggplot2::scale_fill_manual(
        values = c("PASS" = "green", "FAIL" = "red", "WEAK" = "yellow", 
                   "ERROR" = "gray", "SKIPPED" = "lightgray")
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
        plot.title = ggplot2::element_text(size = 16, face = "bold")
      ) +
      ggplot2::labs(
        title = "External Test Results Dashboard",
        x = "Test",
        y = "Tool",
        fill = "Result"
      )
    
    # Save plot
    ggplot2::ggsave(
      file.path(output_dir, "external_test_dashboard.png"),
      p, width = 12, height = 8
    )
  }
}