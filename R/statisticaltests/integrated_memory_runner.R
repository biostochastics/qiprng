# File: integrated_memory_runner.R
# ----------------------------------------------------------------------
#' Integrated Test Runner with Memory Management
#'
#' This module extends the integrated test runner to include
#' memory management capabilities.
#'
#' @name integrated-memory-runner
NULL

# Load dependencies
source_if_exists <- function(path) {
  if (file.exists(path)) {
    source(path)
  } else {
    warning(paste("Module not found:", path))
  }
}

source_if_exists("R/statisticaltests/config_manager.R")
source_if_exists("R/statisticaltests/progress_monitor.R")
source_if_exists("R/statisticaltests/result_aggregator.R")
source_if_exists("R/statisticaltests/memory_manager.R")

#' Run memory-aware test suite
#'
#' @param suite Test suite object
#' @param config_file Configuration file path
#' @param enable_memory_mgmt Whether to enable memory management
#' @return Enhanced test suite with results
#' @export
run_memory_aware_test_suite <- function(suite, 
                                      config_file = NULL,
                                      enable_memory_mgmt = TRUE) {
  
  # Load configuration
  config <- load_config(config_file)
  
  # Create memory manager if enabled
  memory_manager <- NULL
  if (enable_memory_mgmt) {
    memory_manager <- MemoryManager$new()
    memory_manager$start_monitoring()
    
    message("Memory management enabled")
    message(sprintf("Memory limit: %.0f MB", memory_manager$config$memory_limit_mb))
    message(sprintf("GC threshold: %.0f MB", memory_manager$config$gc_threshold_mb))
  }
  
  # Create progress monitor with memory callback
  progress_monitor <- ProgressMonitor$new("console")
  
  if (!is.null(memory_manager)) {
    # Add memory monitoring callback
    progress_monitor$add_callback(function(event, data, monitor) {
      if (event == "test" && data$status == "completed") {
        # Check memory after each test
        mem_info <- memory_manager$check_memory()
        
        # Add memory info to progress
        if (memory_manager$config$verbose) {
          cat(sprintf(" [Memory: %.1f MB]", mem_info$memory_info$used_mb))
        }
      }
    }, events = "test")
  }
  
  # Create result aggregator
  result_aggregator <- ResultAggregator$new()
  
  # Enhanced PRNG function with memory management
  if (!is.null(memory_manager)) {
    original_prng <- suite$prng_func
    suite$prng_func <- function(n) {
      # Use chunked generation for large samples
      if (n > 1e6) {
        return(generate_random_chunked(n, original_prng, 
                                     chunk_size = 1e6,
                                     memory_manager = memory_manager))
      } else {
        return(original_prng(n))
      }
    }
  }
  
  # Count tests
  total_tests <- count_total_tests_memory_aware(suite, config, memory_manager)
  categories <- get_test_categories(suite, config)
  
  # Start progress
  progress_monitor$start(total_tests, categories)
  
  # Run tests with memory monitoring
  tryCatch({
    for (category in categories) {
      # Check memory before category
      if (!is.null(memory_manager)) {
        memory_manager$check_memory()
      }
      
      # Run category
      suite <- run_category_memory_aware(suite, category, config, 
                                       progress_monitor, result_aggregator,
                                       memory_manager)
      
      # GC after each category
      if (!is.null(memory_manager)) {
        memory_manager$collect_garbage(verbose = FALSE)
      }
    }
    
    # Complete
    progress_monitor$complete()
    
  }, interrupt = function(e) {
    progress_monitor$cancel()
    if (!is.null(memory_manager)) {
      memory_manager$stop_monitoring()
    }
    stop("Test suite interrupted")
  })
  
  # Final statistics
  result_aggregator$compute_statistics()
  
  # Add memory summary to results
  if (!is.null(memory_manager)) {
    memory_summary <- memory_manager$get_summary()
    suite$memory_summary <- memory_summary
    
    # Stop monitoring
    memory_manager$stop_monitoring()
    
    # Print memory summary
    cat("\n", memory_manager$get_summary("text"), "\n")
  }
  
  # Add results
  suite$integrated_results <- list(
    aggregator = result_aggregator,
    summary = result_aggregator$get_summary(),
    memory_summary = if (!is.null(memory_manager)) memory_summary else NULL
  )
  
  return(suite)
}

#' Count tests with memory estimation
#' @keywords internal
count_total_tests_memory_aware <- function(suite, config, memory_manager) {
  count <- 0
  estimated_memory_mb <- 0
  
  # Get sample size
  sample_size <- get_test_param("global.default_sample_size", config, 10000)
  
  # Estimate memory per test category
  if (isTRUE(suite$config$run_basic_tests)) {
    count <- count + 5
    estimated_memory_mb <- estimated_memory_mb + (sample_size * 8 * 5 / 1e6)
  }
  
  if (isTRUE(suite$config$run_classical_tests)) {
    count <- count + 6
    estimated_memory_mb <- estimated_memory_mb + (sample_size * 8 * 6 / 1e6)
  }
  
  if (isTRUE(suite$config$run_binary_tests)) {
    count <- count + 8
    # Binary tests need more memory for bit conversion
    estimated_memory_mb <- estimated_memory_mb + (sample_size * 32 * 8 / 1e6)
  }
  
  if (!is.null(memory_manager) && memory_manager$config$verbose) {
    message(sprintf("Estimated memory requirement: %.1f MB", estimated_memory_mb))
  }
  
  return(count)
}

#' Run category with memory awareness
#' @keywords internal  
run_category_memory_aware <- function(suite, category, config, 
                                    progress_monitor, result_aggregator,
                                    memory_manager) {
  
  # Track category data if memory manager available
  if (!is.null(memory_manager)) {
    # Get sample size for this category
    sample_size <- get_test_param("global.default_sample_size", config, 10000)
    
    # Check if we have enough memory
    mem_info <- memory_manager$check_memory()
    required_mb <- sample_size * 8 / 1e6  # Rough estimate
    
    if (mem_info$memory_info$available_mb < required_mb * 2) {
      warning(sprintf("Low memory for %s tests. Available: %.1f MB, Required: ~%.1f MB",
                     category, mem_info$memory_info$available_mb, required_mb))
      
      # Force GC
      memory_manager$collect_garbage()
    }
  }
  
  # Original category runner
  progress_monitor$update_category(category, status = "running")
  
  # Run tests
  # (This would call the actual test functions)
  
  progress_monitor$update_category(category, status = "completed")
  
  return(suite)
}

#' Create memory-optimized test wrapper
#' 
#' @param test_func Original test function
#' @param memory_manager Memory manager instance
#' @param test_name Test name for tracking
#' @export
create_memory_optimized_test <- function(test_func, memory_manager, test_name) {
  function(data, ...) {
    # Track input data
    memory_manager$track_object_size(paste0(test_name, "_input"), data, "test_data")
    
    # Run test with memory wrapping
    wrapped_test <- memory_manager$wrap_function(
      test_func,
      check_before = TRUE,
      check_after = TRUE,
      gc_after = TRUE
    )
    
    result <- wrapped_test(data, ...)
    
    # Track result size
    memory_manager$track_object_size(paste0(test_name, "_result"), result, "test_results")
    
    return(result)
  }
}

#' Demo memory-aware test suite
#' @export
demo_memory_aware_suite <- function() {
  # Create test suite
  suite <- list(
    config = list(
      run_basic_tests = TRUE,
      run_classical_tests = TRUE,
      run_binary_tests = TRUE,
      sample_size = 1e6
    ),
    prng_func = function(n) runif(n)
  )
  
  # Create temporary config
  config <- get_default_config()
  config$internal_tests$performance$memory_limit_mb <- 2048
  config$internal_tests$performance$gc_threshold_mb <- 512
  config$internal_tests$performance$gc_frequency <- 10
  config$internal_tests$global$default_sample_size <- 1e6
  
  # Save temp config
  temp_config <- tempfile(fileext = ".json")
  save_config(config, temp_config)
  
  # Run with memory management
  cat("Running memory-aware test suite demo...\n")
  suite <- run_memory_aware_test_suite(
    suite,
    config_file = temp_config,
    enable_memory_mgmt = TRUE
  )
  
  # Clean up
  unlink(temp_config)
  
  return(suite)
}