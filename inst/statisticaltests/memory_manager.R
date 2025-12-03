# File: memory_manager.R
# ----------------------------------------------------------------------
#' Memory Management Framework for PRNG Test Suite
#'
#' This module provides comprehensive memory management capabilities
#' including monitoring, garbage collection, and optimization strategies
#' for handling large-scale random number generation and testing.
#'
#' Features:
#' \itemize{
#'   \item Real-time memory usage monitoring
#'   \item Strategic garbage collection
#'   \item Object size tracking
#'   \item Memory-efficient data chunking
#'   \item Memory limit enforcement
#'   \item Integration with configuration system
#' }
#'
#' @name memory-manager
#' @import utils
NULL

#' Memory manager class
#'
#' @export
MemoryManager <- R6::R6Class("MemoryManager",
  public = list(
    #' @field config Memory management configuration
    config = NULL,

    #' @field stats Memory usage statistics
    stats = NULL,

    #' @field thresholds Memory thresholds
    thresholds = NULL,

    #' @field monitoring Whether monitoring is active
    monitoring = FALSE,

    #' @field gc_count Number of garbage collections performed
    gc_count = 0,

    #' Initialize memory manager
    #'
    #' @param config Configuration options (uses system config if NULL)
    initialize = function(config = NULL) {
      if (is.null(config)) {
        # Load from system configuration
        if (exists("load_config") && exists("get_test_param")) {
          sys_config <- load_config()
          self$config <- list(
            memory_limit_mb = get_test_param("performance.memory_limit_mb", sys_config, 4096),
            gc_frequency = get_test_param("performance.gc_frequency", sys_config, 100),
            gc_threshold_mb = get_test_param("performance.gc_threshold_mb", sys_config, 1024),
            chunk_size = get_test_param("performance.chunk_size", sys_config, 1000),
            monitor_interval = get_test_param("performance.monitor_interval", sys_config, 10),
            verbose = get_test_param("global.verbose", sys_config, FALSE)
          )
        } else {
          self$config <- private$default_config()
        }
      } else {
        self$config <- config
      }

      # Initialize statistics
      self$stats <- list(
        initial = private$get_memory_info(),
        current = NULL,
        peak = 0,
        gc_history = list(),
        object_sizes = list()
      )

      # Set thresholds
      self$thresholds <- list(
        warning = self$config$memory_limit_mb * 0.75,
        critical = self$config$memory_limit_mb * 0.90,
        gc_trigger = self$config$gc_threshold_mb
      )

      # Set memory limit if specified
      if (!is.null(self$config$memory_limit_mb) && self$config$memory_limit_mb > 0) {
        private$set_memory_limit(self$config$memory_limit_mb)
      }
    },

    #' Start memory monitoring
    start_monitoring = function() {
      self$monitoring <- TRUE
      self$stats$start_time <- Sys.time()
      private$update_stats()

      if (self$config$verbose) {
        cat("Memory monitoring started\n")
        cat(sprintf("Initial memory usage: %.1f MB\n", self$stats$initial$used_mb))
      }
    },

    #' Stop memory monitoring
    stop_monitoring = function() {
      self$monitoring <- FALSE
      private$update_stats()

      if (self$config$verbose) {
        cat("Memory monitoring stopped\n")
        cat(sprintf("Peak memory usage: %.1f MB\n", self$stats$peak))
      }
    },

    #' Check memory usage and trigger GC if needed
    #'
    #' @param force Force garbage collection regardless of threshold
    #' @return List with memory info and gc_triggered flag
    check_memory = function(force = FALSE) {
      private$update_stats()

      mem_info <- self$stats$current
      gc_triggered <- FALSE

      # Check if GC is needed
      if (force || mem_info$used_mb > self$thresholds$gc_trigger) {
        gc_triggered <- self$collect_garbage()
      }

      # Check warning thresholds
      if (mem_info$used_mb > self$thresholds$critical) {
        warning(sprintf(
          "CRITICAL: Memory usage (%.1f MB) exceeds critical threshold (%.1f MB)",
          mem_info$used_mb, self$thresholds$critical
        ))
      } else if (mem_info$used_mb > self$thresholds$warning) {
        if (self$config$verbose) {
          message(sprintf(
            "Warning: Memory usage (%.1f MB) exceeds warning threshold (%.1f MB)",
            mem_info$used_mb, self$thresholds$warning
          ))
        }
      }

      return(list(
        memory_info = mem_info,
        gc_triggered = gc_triggered
      ))
    },

    #' Perform garbage collection
    #'
    #' @param verbose Whether to print GC info
    #' @return Memory info after GC
    collect_garbage = function(verbose = NULL) {
      if (is.null(verbose)) {
        verbose <- self$config$verbose
      }

      # Get memory before GC
      before <- private$get_memory_info()

      # Perform garbage collection
      gc_result <- gc(verbose = FALSE, full = TRUE)
      self$gc_count <- self$gc_count + 1

      # Get memory after GC
      after <- private$get_memory_info()

      # Calculate freed memory
      freed_mb <- before$used_mb - after$used_mb

      # Record GC event
      gc_event <- list(
        timestamp = Sys.time(),
        before_mb = before$used_mb,
        after_mb = after$used_mb,
        freed_mb = freed_mb,
        gc_result = gc_result
      )

      self$stats$gc_history[[length(self$stats$gc_history) + 1]] <- gc_event

      if (verbose) {
        cat(sprintf(
          "Garbage collection #%d: freed %.1f MB (%.1f -> %.1f MB)\n",
          self$gc_count, freed_mb, before$used_mb, after$used_mb
        ))
      }

      # Update current stats
      private$update_stats()

      return(after)
    },

    #' Track object size
    #'
    #' @param object_name Name of the object
    #' @param object The object to track
    #' @param category Optional category for grouping
    track_object_size = function(object_name, object, category = NULL) {
      size_bytes <- object.size(object)
      size_mb <- as.numeric(size_bytes) / (1024 * 1024)

      # Store size info
      size_info <- list(
        name = object_name,
        category = category,
        size_bytes = size_bytes,
        size_mb = size_mb,
        timestamp = Sys.time(),
        class = class(object)[1],
        dimensions = if (is.matrix(object) || is.data.frame(object)) {
          dim(object)
        } else {
          length(object)
        }
      )

      # Add to tracking
      key <- if (!is.null(category)) paste(category, object_name, sep = "_") else object_name
      self$stats$object_sizes[[key]] <- size_info

      # Check if this object is unusually large
      if (size_mb > self$config$memory_limit_mb * 0.1) { # More than 10% of limit
        if (self$config$verbose) {
          warning(sprintf(
            "Large object detected: %s (%.1f MB, %.1f%% of memory limit)",
            object_name, size_mb, (size_mb / self$config$memory_limit_mb) * 100
          ))
        }
      }

      return(size_info)
    },

    #' Process data in memory-efficient chunks
    #'
    #' @param data Data to process
    #' @param process_func Function to apply to each chunk
    #' @param chunk_size Size of each chunk (NULL uses config default)
    #' @param gc_between_chunks Whether to GC between chunks
    #' @param ... Additional arguments to process_func
    #' @return Processed results
    process_in_chunks = function(data, process_func, chunk_size = NULL,
                                 gc_between_chunks = TRUE, ...) {
      if (is.null(chunk_size)) {
        chunk_size <- self$config$chunk_size
      }

      n <- if (is.matrix(data) || is.data.frame(data)) nrow(data) else length(data)
      n_chunks <- ceiling(n / chunk_size)

      if (self$config$verbose && n_chunks > 1) {
        cat(sprintf(
          "Processing %d items in %d chunks of size %d\n",
          n, n_chunks, chunk_size
        ))
      }

      results <- vector("list", n_chunks)

      for (i in seq_len(n_chunks)) {
        # Calculate chunk indices
        start_idx <- (i - 1) * chunk_size + 1
        end_idx <- min(i * chunk_size, n)

        # Extract chunk
        if (is.matrix(data) || is.data.frame(data)) {
          chunk <- data[start_idx:end_idx, , drop = FALSE]
        } else {
          chunk <- data[start_idx:end_idx]
        }

        # Process chunk
        results[[i]] <- process_func(chunk, ...)

        # Check memory and GC if needed
        if (gc_between_chunks && i < n_chunks) {
          mem_check <- self$check_memory()
          if (mem_check$memory_info$used_mb > self$thresholds$gc_trigger * 0.8) {
            self$collect_garbage(verbose = FALSE)
          }
        }

        # Progress update
        if (self$config$verbose && n_chunks > 10 && i %% 10 == 0) {
          cat(sprintf(
            "Processed %d/%d chunks (%.1f%%)\n",
            i, n_chunks, (i / n_chunks) * 100
          ))
        }
      }

      # Combine results
      if (all(sapply(results, is.atomic))) {
        combined <- unlist(results)
      } else {
        combined <- do.call(c, results)
      }

      return(combined)
    },

    #' Get memory usage summary
    #'
    #' @param format Output format: "list", "text", "data.frame"
    #' @return Memory usage summary
    get_summary = function(format = "list") {
      private$update_stats()

      summary_data <- list(
        current_usage_mb = self$stats$current$used_mb,
        peak_usage_mb = self$stats$peak,
        initial_usage_mb = self$stats$initial$used_mb,
        memory_limit_mb = self$config$memory_limit_mb,
        gc_count = self$gc_count,
        gc_threshold_mb = self$config$gc_threshold_mb,
        largest_objects = private$get_largest_objects(5),
        usage_percentage = (self$stats$current$used_mb / self$config$memory_limit_mb) * 100
      )

      if (format == "text") {
        return(private$format_text_summary(summary_data))
      } else if (format == "data.frame") {
        return(private$format_df_summary(summary_data))
      } else {
        return(summary_data)
      }
    },

    #' Clean up tracked objects
    #'
    #' @param pattern Pattern to match object names (NULL removes all)
    #' @param older_than Remove objects older than this many seconds
    clean_tracked_objects = function(pattern = NULL, older_than = NULL) {
      if (!is.null(pattern)) {
        self$stats$object_sizes <- self$stats$object_sizes[
          !grepl(pattern, names(self$stats$object_sizes))
        ]
      }

      if (!is.null(older_than)) {
        current_time <- Sys.time()
        self$stats$object_sizes <- self$stats$object_sizes[
          sapply(self$stats$object_sizes, function(x) {
            as.numeric(difftime(current_time, x$timestamp, units = "secs")) < older_than
          })
        ]
      }

      if (is.null(pattern) && is.null(older_than)) {
        self$stats$object_sizes <- list()
      }
    },

    #' Create memory-aware wrapper for functions
    #'
    #' @param func Function to wrap
    #' @param check_before Whether to check memory before execution
    #' @param check_after Whether to check memory after execution
    #' @param gc_after Whether to GC after execution
    #' @return Wrapped function
    wrap_function = function(func, check_before = TRUE, check_after = TRUE, gc_after = FALSE) {
      force(func)
      force(check_before)
      force(check_after)
      force(gc_after)

      manager <- self

      function(...) {
        # Check memory before
        if (check_before) {
          before_mem <- manager$check_memory()
          if (before_mem$memory_info$used_mb > manager$thresholds$warning) {
            warning("High memory usage before function execution")
          }
        }

        # Execute function
        result <- func(...)

        # Check memory after
        if (check_after) {
          after_mem <- manager$check_memory()
          mem_increase <- after_mem$memory_info$used_mb -
            (if (check_before) before_mem$memory_info$used_mb else 0)

          if (manager$config$verbose && mem_increase > 100) {
            message(sprintf("Function increased memory usage by %.1f MB", mem_increase))
          }
        }

        # GC if requested
        if (gc_after) {
          manager$collect_garbage(verbose = FALSE)
        }

        return(result)
      }
    }
  ),
  private = list(
    # Get current memory information
    get_memory_info = function() {
      # Get memory stats
      mem_used <- sum(gc()[, "used"]) / 1024 # Convert to MB
      mem_limit <- as.numeric(memory.limit())

      # For Unix systems, try to get more detailed info
      if (.Platform$OS.type == "unix") {
        # Try to get RSS (Resident Set Size) if possible
        pid <- Sys.getpid()
        try(
          {
            if (Sys.info()["sysname"] == "Darwin") {
              # macOS
              ps_output <- system(paste("ps -o rss= -p", pid), intern = TRUE)
            } else {
              # Linux
              ps_output <- system(paste("ps -o rss= -p", pid), intern = TRUE)
            }
            if (length(ps_output) > 0) {
              rss_kb <- as.numeric(trimws(ps_output[1]))
              mem_used <- rss_kb / 1024 # Convert to MB
            }
          },
          silent = TRUE
        )
      }

      list(
        used_mb = mem_used,
        limit_mb = mem_limit,
        available_mb = mem_limit - mem_used,
        percentage = (mem_used / mem_limit) * 100,
        timestamp = Sys.time()
      )
    },

    # Update statistics
    update_stats = function() {
      self$stats$current <- private$get_memory_info()

      # Update peak
      if (self$stats$current$used_mb > self$stats$peak) {
        self$stats$peak <- self$stats$current$used_mb
      }
    },

    # Set memory limit
    set_memory_limit = function(limit_mb) {
      if (.Platform$OS.type == "windows") {
        memory.limit(size = limit_mb)
      } else {
        # Unix systems don't support memory.limit()
        # Could potentially use ulimit via system() but that's complex
        if (self$config$verbose) {
          message("Memory limit setting is only supported on Windows. Using monitoring only.")
        }
      }
    },

    # Get largest tracked objects
    get_largest_objects = function(n = 5) {
      if (length(self$stats$object_sizes) == 0) {
        return(NULL)
      }

      # Sort by size
      sizes <- sapply(self$stats$object_sizes, function(x) x$size_mb)
      top_indices <- order(sizes, decreasing = TRUE)[1:min(n, length(sizes))]

      self$stats$object_sizes[top_indices]
    },

    # Format text summary
    format_text_summary = function(summary_data) {
      lines <- character()

      lines <- c(lines, "Memory Usage Summary")
      lines <- c(lines, paste(rep("=", 40), collapse = ""))
      lines <- c(lines, sprintf(
        "Current usage: %.1f MB (%.1f%%)",
        summary_data$current_usage_mb,
        summary_data$usage_percentage
      ))
      lines <- c(lines, sprintf("Peak usage: %.1f MB", summary_data$peak_usage_mb))
      lines <- c(lines, sprintf("Memory limit: %.1f MB", summary_data$memory_limit_mb))
      lines <- c(lines, sprintf("GC performed: %d times", summary_data$gc_count))

      if (!is.null(summary_data$largest_objects)) {
        lines <- c(lines, "\nLargest tracked objects:")
        for (obj in summary_data$largest_objects) {
          lines <- c(lines, sprintf(
            "  %s: %.1f MB (%s)",
            obj$name, obj$size_mb, obj$class
          ))
        }
      }

      paste(lines, collapse = "\n")
    },

    # Format data frame summary
    format_df_summary = function(summary_data) {
      data.frame(
        metric = c(
          "current_usage_mb", "peak_usage_mb", "memory_limit_mb",
          "usage_percentage", "gc_count"
        ),
        value = c(
          summary_data$current_usage_mb, summary_data$peak_usage_mb,
          summary_data$memory_limit_mb, summary_data$usage_percentage,
          summary_data$gc_count
        ),
        stringsAsFactors = FALSE
      )
    },

    # Default configuration
    default_config = function() {
      list(
        memory_limit_mb = 4096,
        gc_frequency = 100,
        gc_threshold_mb = 1024,
        chunk_size = 1000,
        monitor_interval = 10,
        verbose = FALSE
      )
    }
  )
)

#' Create memory manager for test suite
#'
#' @param suite Test suite object
#' @param config Configuration options
#' @export
create_test_memory_manager <- function(suite = NULL, config = NULL) {
  # Create config from suite if available
  if (!is.null(suite) && is.null(config)) {
    config <- list(
      memory_limit_mb = suite$config$memory_limit_mb,
      gc_frequency = suite$config$gc_frequency,
      verbose = suite$config$verbose
    )
  }

  manager <- MemoryManager$new(config)

  # Add to suite if provided
  if (!is.null(suite)) {
    suite$memory_manager <- manager
  }

  return(manager)
}

#' Memory-efficient random number generation
#'
#' @param n Number of random numbers to generate
#' @param generator Generator function (default: runif)
#' @param chunk_size Chunk size for generation
#' @param memory_manager Memory manager instance
#' @param ... Additional arguments to generator
#' @export
generate_random_chunked <- function(n, generator = runif, chunk_size = 1e6,
                                    memory_manager = NULL, ...) {
  if (is.null(memory_manager)) {
    memory_manager <- MemoryManager$new()
  }

  # If n is small, generate directly
  if (n <= chunk_size) {
    return(generator(n, ...))
  }

  # Generate in chunks
  n_chunks <- ceiling(n / chunk_size)
  results <- vector("list", n_chunks)

  for (i in seq_len(n_chunks)) {
    chunk_n <- if (i < n_chunks) chunk_size else (n - (i - 1) * chunk_size)
    results[[i]] <- generator(chunk_n, ...)

    # Check memory periodically
    if (i %% 10 == 0) {
      memory_manager$check_memory()
    }
  }

  # Combine results
  combined <- unlist(results)

  # Final GC
  memory_manager$collect_garbage(verbose = FALSE)

  return(combined)
}
