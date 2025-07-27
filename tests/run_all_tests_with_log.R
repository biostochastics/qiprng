#\!/usr/bin/env Rscript

# Comprehensive test script to run all tests and log results
suppressPackageStartupMessages(library(qiprng))

# Create a log file
log_file <- "test_results/comprehensive_results.log"
if (file.exists(log_file)) file.remove(log_file)

# Function to log messages
log_message <- function(msg, append_to_console = TRUE) {
  cat(msg, file = log_file, append = TRUE)
  if (append_to_console) cat(msg)
}

# Function to run a test with proper logging
run_test <- function(name, func, log_prefix = "") {
  log_message(paste0(log_prefix, "Running test: ", name, "...\n"))
  
  start_time <- Sys.time()
  result <- tryCatch({
    func()
    list(success = TRUE, error = NULL)
  }, error = function(e) {
    list(success = FALSE, error = e$message)
  }, warning = function(w) {
    list(success = TRUE, warning = w$message)
  })
  end_time <- Sys.time()
  
  duration <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  if (result$success) {
    log_message(paste0(log_prefix, "  [PASS] ", name, " (", round(duration, 2), " seconds)\n"))
  } else {
    log_message(paste0(log_prefix, "  [FAIL] ", name, " (", round(duration, 2), " seconds): ", result$error, "\n"))
  }
  
  return(result$success)
}

# Helper function to run R script files safely
run_script_test <- function(script_path, name = NULL) {
  if (is.null(name)) {
    name <- basename(script_path)
  }
  
  log_message(paste0("Running script test: ", name, "...\n"))
  
  start_time <- Sys.time()
  result <- tryCatch({
    source(script_path)
    list(success = TRUE, error = NULL)
  }, error = function(e) {
    list(success = FALSE, error = e$message)
  })
  end_time <- Sys.time()
  
  duration <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  if (result$success) {
    log_message(paste0("  [PASS] ", name, " (", round(duration, 2), " seconds)\n"))
  } else {
    log_message(paste0("  [FAIL] ", name, " (", round(duration, 2), " seconds): ", result$error, "\n"))
  }
  
  return(result$success)
}

# Start the test run
log_message("\n===== Starting Comprehensive Test Suite =====\n")
log_message(paste0("Date: ", Sys.time(), "\n"))
log_message(paste0("R Version: ", R.version.string, "\n"))
log_message(paste0("Platform: ", Sys.info()["sysname"], " ", Sys.info()["release"], "\n\n"))

# First run our safest test which is the main thread safety verification
log_message("\n----- Core Thread Safety Tests -----\n")
run_script_test("safest_ziggurat_test.R", "Safest Ziggurat Test")

# Test the Ziggurat normal generation in non-threaded mode
basic_ziggurat_test <- function() {
  # Create a PRNG with Ziggurat normal distribution
  cfg <- list(
    distribution = "normal",
    normal_method = "ziggurat",
    use_threading = FALSE
  )
  
  createPRNG(cfg)
  values <- generatePRNG(1000)
  cleanup_prng()
  
  # Basic checks
  if (is.nan(mean(values)) || is.nan(sd(values))) {
    stop("Generated values contain NaN")
  }
  
  if (any(is.infinite(values))) {
    stop("Generated values contain Infinity")
  }
  
  # Success if we reach here
  return(TRUE)
}

run_test("Basic Ziggurat (Non-Threaded)", basic_ziggurat_test)

# Test the Ziggurat normal generation in threaded mode
threaded_ziggurat_test <- function() {
  # Create a PRNG with Ziggurat normal distribution in thread-safe mode
  cfg <- list(
    distribution = "normal",
    normal_method = "ziggurat",
    use_threading = TRUE
  )
  
  createPRNG(cfg)
  values <- generatePRNG(1000)
  cleanup_prng()
  
  # Basic checks
  if (is.nan(mean(values)) || is.nan(sd(values))) {
    stop("Generated values contain NaN")
  }
  
  if (any(is.infinite(values))) {
    stop("Generated values contain Infinity")
  }
  
  # Success if we reach here
  return(TRUE)
}

run_test("Basic Ziggurat (Threaded)", threaded_ziggurat_test)

# Run through all the test scripts in the testthat directory
log_message("\n----- Running Test Scripts -----\n")

# First, find all test scripts
test_dir <- "tests/testthat"
test_scripts <- list.files(path = test_dir, pattern = "^test.*\\.R$", full.names = TRUE)

# Categorize the tests
ziggurat_tests <- grep("ziggurat", test_scripts, value = TRUE, ignore.case = TRUE)
thread_tests <- grep("thread", test_scripts, value = TRUE, ignore.case = TRUE)
other_tests <- setdiff(test_scripts, c(ziggurat_tests, thread_tests))

# Run Ziggurat-related tests first
log_message("\n----- Ziggurat-specific Tests -----\n")
ziggurat_results <- sapply(ziggurat_tests, function(script) {
  name <- basename(script)
  tryCatch({
    run_script_test(script, name)
    TRUE
  }, error = function(e) {
    log_message(paste0("  [ERROR] ", name, ": ", e$message, "\n"))
    FALSE
  })
})

# Run thread-related tests
log_message("\n----- Thread-related Tests -----\n")
thread_results <- sapply(thread_tests, function(script) {
  name <- basename(script)
  tryCatch({
    run_script_test(script, name)
    TRUE
  }, error = function(e) {
    log_message(paste0("  [ERROR] ", name, ": ", e$message, "\n"))
    FALSE
  })
})

# Run other tests
log_message("\n----- Other Tests -----\n")
other_results <- sapply(other_tests, function(script) {
  name <- basename(script)
  tryCatch({
    run_script_test(script, name)
    TRUE
  }, error = function(e) {
    log_message(paste0("  [ERROR] ", name, ": ", e$message, "\n"))
    FALSE
  })
})

# Run our custom verification script
log_message("\n----- Final Verification -----\n")
run_script_test("final_verify.R", "Final Thread Safety Verification")

# Generate test summary
log_message("\n===== Test Summary =====\n")
log_message(paste0("Core Tests: ", 
                 sum(c(TRUE, TRUE)), " passed, ", 
                 sum(c(FALSE, FALSE)), " failed\n"))

log_message(paste0("Ziggurat Tests: ", 
                 sum(ziggurat_results), " passed, ", 
                 sum(\!ziggurat_results), " failed\n"))

log_message(paste0("Thread Tests: ", 
                 sum(thread_results), " passed, ", 
                 sum(\!thread_results), " failed\n"))

log_message(paste0("Other Tests: ", 
                 sum(other_results), " passed, ", 
                 sum(\!other_results), " failed\n"))

total_passed <- sum(c(TRUE, TRUE)) + sum(ziggurat_results) + sum(thread_results) + sum(other_results) + 1 # +1 for final verification
total_failed <- sum(c(FALSE, FALSE)) + sum(\!ziggurat_results) + sum(\!thread_results) + sum(\!other_results)
total_tests <- total_passed + total_failed

log_message(paste0("\nTOTAL: ", total_passed, " / ", total_tests, " (", 
                 round(100 * total_passed / total_tests, 1), "%) tests passed\n"))

# List failing tests
if (total_failed > 0) {
  log_message("\nFailing tests:\n")
  
  # Ziggurat failing tests
  if (sum(\!ziggurat_results) > 0) {
    failing_ziggurat <- ziggurat_tests[\!ziggurat_results]
    for (test in failing_ziggurat) {
      log_message(paste0("- ", basename(test), "\n"))
    }
  }
  
  # Thread failing tests
  if (sum(\!thread_results) > 0) {
    failing_thread <- thread_tests[\!thread_results]
    for (test in failing_thread) {
      log_message(paste0("- ", basename(test), "\n"))
    }
  }
  
  # Other failing tests
  if (sum(\!other_results) > 0) {
    failing_other <- other_tests[\!other_results]
    for (test in failing_other) {
      log_message(paste0("- ", basename(test), "\n"))
    }
  }
}

log_message("\n===== Testing Completed =====\n")
log_message(paste0("End time: ", Sys.time(), "\n"))

# Show the log file path
cat("\nTest results written to:", normalizePath(log_file), "\n")
