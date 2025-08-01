# File: progress_monitor.R
# ----------------------------------------------------------------------
#' Progress Monitoring Framework for PRNG Test Suite
#'
#' This module provides comprehensive progress monitoring capabilities
#' for the PRNG test suite, supporting real-time progress tracking,
#' callbacks, and multiple output backends.
#'
#' Features:
#' \itemize{
#'   \item Real-time progress tracking
#'   \item Multiple progress backends (console, file, callback)
#'   \item Nested progress support (suite -> category -> test)
#'   \item Cancellation and pause/resume support
#'   \item Time estimation and ETA calculation
#' }
#'
#' @name progress-monitor
#' @import utils
#' @import cli
NULL

#' Progress monitor class
#' 
#' @export
ProgressMonitor <- R6::R6Class("ProgressMonitor",
  public = list(
    #' @field backend The progress backend to use
    backend = NULL,
    
    #' @field config Progress monitoring configuration
    config = NULL,
    
    #' @field state Current progress state
    state = NULL,
    
    #' @field callbacks List of progress callbacks
    callbacks = NULL,
    
    #' @field start_time Start time of monitoring
    start_time = NULL,
    
    #' Initialize progress monitor
    #' 
    #' @param backend Backend type: "console", "file", "callback", "none"
    #' @param config Configuration options
    initialize = function(backend = "console", config = list()) {
      self$backend <- backend
      self$config <- merge_progress_config(config)
      self$callbacks <- list()
      self$state <- list(
        total_tests = 0,
        completed_tests = 0,
        current_category = NULL,
        current_test = NULL,
        categories = list(),
        cancelled = FALSE,
        paused = FALSE
      )
      
      # Initialize backend
      private$init_backend()
    },
    
    #' Start progress monitoring
    #' 
    #' @param total_tests Total number of tests to run
    #' @param categories List of test categories
    start = function(total_tests, categories = NULL) {
      self$start_time <- Sys.time()
      self$state$total_tests <- total_tests
      self$state$completed_tests <- 0
      
      if (!is.null(categories)) {
        self$state$categories <- lapply(categories, function(cat) {
          list(
            name = cat,
            total = 0,
            completed = 0,
            status = "pending"
          )
        })
        names(self$state$categories) <- categories
      }
      
      # Notify callbacks
      private$notify_callbacks("start", self$state)
      
      # Update backend
      private$update_backend("start")
    },
    
    #' Update category progress
    #' 
    #' @param category Category name
    #' @param total Total tests in category
    #' @param status Category status
    update_category = function(category, total = NULL, status = "running") {
      if (!is.null(self$state$categories[[category]])) {
        self$state$categories[[category]]$status <- status
        if (!is.null(total)) {
          self$state$categories[[category]]$total <- total
        }
      }
      
      self$state$current_category <- category
      
      # Notify callbacks
      private$notify_callbacks("category", list(
        category = category,
        status = status,
        total = total
      ))
      
      # Update backend
      private$update_backend("category")
    },
    
    #' Update test progress
    #' 
    #' @param test_name Test name
    #' @param status Test status: "running", "completed", "failed", "skipped"
    #' @param details Additional details
    update_test = function(test_name, status = "running", details = NULL) {
      self$state$current_test <- test_name
      
      if (status == "completed" || status == "failed" || status == "skipped") {
        self$state$completed_tests <- self$state$completed_tests + 1
        
        # Update category progress
        if (!is.null(self$state$current_category) && 
            !is.null(self$state$categories[[self$state$current_category]])) {
          self$state$categories[[self$state$current_category]]$completed <- 
            self$state$categories[[self$state$current_category]]$completed + 1
        }
      }
      
      # Calculate ETA
      eta <- private$calculate_eta()
      
      # Notify callbacks
      private$notify_callbacks("test", list(
        test = test_name,
        status = status,
        details = details,
        progress = self$state$completed_tests / self$state$total_tests,
        eta = eta
      ))
      
      # Update backend
      private$update_backend("test", list(
        test = test_name,
        status = status,
        progress = self$state$completed_tests / self$state$total_tests,
        eta = eta
      ))
    },
    
    #' Complete progress monitoring
    #' 
    #' @param summary Summary information
    complete = function(summary = NULL) {
      elapsed <- as.numeric(difftime(Sys.time(), self$start_time, units = "secs"))
      
      # Notify callbacks
      private$notify_callbacks("complete", list(
        total_tests = self$state$total_tests,
        completed_tests = self$state$completed_tests,
        elapsed_time = elapsed,
        summary = summary
      ))
      
      # Update backend
      private$update_backend("complete")
      
      # Clean up
      private$cleanup_backend()
    },
    
    #' Add progress callback
    #' 
    #' @param callback Function to call on progress updates
    #' @param events Events to trigger callback: "all", "start", "test", "category", "complete"
    add_callback = function(callback, events = "all") {
      if (!is.function(callback)) {
        stop("Callback must be a function")
      }
      
      self$callbacks[[length(self$callbacks) + 1]] <- list(
        func = callback,
        events = events
      )
    },
    
    #' Cancel monitoring
    cancel = function() {
      self$state$cancelled <- TRUE
      private$notify_callbacks("cancel", self$state)
      private$cleanup_backend()
    },
    
    #' Pause monitoring
    pause = function() {
      self$state$paused <- TRUE
      self$state$pause_time <- Sys.time()
      private$notify_callbacks("pause", self$state)
    },
    
    #' Resume monitoring
    resume = function() {
      if (self$state$paused) {
        pause_duration <- as.numeric(difftime(Sys.time(), self$state$pause_time, units = "secs"))
        self$start_time <- self$start_time + pause_duration
        self$state$paused <- FALSE
        private$notify_callbacks("resume", self$state)
      }
    },
    
    #' Get current progress
    #' 
    #' @return Current progress state
    get_progress = function() {
      list(
        total = self$state$total_tests,
        completed = self$state$completed_tests,
        percentage = if (self$state$total_tests > 0) 
          round(self$state$completed_tests / self$state$total_tests * 100, 1) else 0,
        current_category = self$state$current_category,
        current_test = self$state$current_test,
        elapsed = as.numeric(difftime(Sys.time(), self$start_time, units = "secs")),
        eta = private$calculate_eta(),
        cancelled = self$state$cancelled,
        paused = self$state$paused
      )
    }
  ),
  
  private = list(
    # Backend-specific objects
    progress_bar = NULL,
    file_handle = NULL,
    
    # Initialize backend
    init_backend = function() {
      if (self$backend == "console") {
        if (self$config$use_cli && requireNamespace("cli", quietly = TRUE)) {
          # Use cli for better console output
          private$init_cli_backend()
        } else {
          # Use base R progress bar
          private$init_base_backend()
        }
      } else if (self$backend == "file") {
        private$init_file_backend()
      }
    },
    
    # Initialize CLI backend
    init_cli_backend = function() {
      # CLI will be initialized on start
    },
    
    # Initialize base R backend
    init_base_backend = function() {
      # txtProgressBar will be created on start
    },
    
    # Initialize file backend
    init_file_backend = function() {
      if (!is.null(self$config$log_file)) {
        private$file_handle <- file(self$config$log_file, open = "w")
        writeLines(paste("Progress log started at", Sys.time()), private$file_handle)
      }
    },
    
    # Update backend
    update_backend = function(event, data = NULL) {
      if (self$backend == "console") {
        if (self$config$use_cli && requireNamespace("cli", quietly = TRUE)) {
          private$update_cli_backend(event, data)
        } else {
          private$update_base_backend(event, data)
        }
      } else if (self$backend == "file") {
        private$update_file_backend(event, data)
      }
    },
    
    # Update CLI backend
    update_cli_backend = function(event, data) {
      if (event == "start") {
        cli::cli_h1("Running PRNG Test Suite")
        cli::cli_alert_info("Total tests: {self$state$total_tests}")
        private$progress_bar <- cli::cli_progress_bar(
          total = self$state$total_tests,
          format = "{cli::pb_spin} {.strong [{cli::pb_current}/{cli::pb_total}]} {cli::pb_percent} | {cli::pb_eta_str}"
        )
      } else if (event == "test" && !is.null(data)) {
        cli::cli_progress_update(id = private$progress_bar)
        if (data$status == "failed") {
          cli::cli_alert_danger("Test failed: {data$test}")
        }
      } else if (event == "category") {
        cli::cli_alert_info("Testing category: {self$state$current_category}")
      } else if (event == "complete") {
        cli::cli_progress_done(id = private$progress_bar)
        cli::cli_alert_success("Test suite completed!")
      }
    },
    
    # Update base R backend
    update_base_backend = function(event, data) {
      if (event == "start") {
        private$progress_bar <- utils::txtProgressBar(
          min = 0, max = self$state$total_tests, 
          style = 3, width = 50
        )
      } else if (event == "test" && !is.null(data)) {
        utils::setTxtProgressBar(private$progress_bar, self$state$completed_tests)
      } else if (event == "complete") {
        close(private$progress_bar)
        cat("\nTest suite completed!\n")
      }
    },
    
    # Update file backend
    update_file_backend = function(event, data) {
      if (!is.null(private$file_handle)) {
        timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        
        if (event == "start") {
          writeLines(paste(timestamp, "- Started test suite with", self$state$total_tests, "tests"), 
                     private$file_handle)
        } else if (event == "test" && !is.null(data)) {
          writeLines(paste(timestamp, "- Test:", data$test, "- Status:", data$status, 
                          "- Progress:", round(data$progress * 100, 1), "%"),
                     private$file_handle)
        } else if (event == "category") {
          writeLines(paste(timestamp, "- Starting category:", self$state$current_category),
                     private$file_handle)
        } else if (event == "complete") {
          writeLines(paste(timestamp, "- Test suite completed"), private$file_handle)
        }
        
        flush(private$file_handle)
      }
    },
    
    # Notify callbacks
    notify_callbacks = function(event, data) {
      for (cb in self$callbacks) {
        if (cb$events == "all" || event %in% cb$events) {
          tryCatch({
            cb$func(event, data, self)
          }, error = function(e) {
            warning(paste("Progress callback error:", e$message))
          })
        }
      }
    },
    
    # Calculate ETA
    calculate_eta = function() {
      if (self$state$completed_tests == 0 || self$state$paused) {
        return(NULL)
      }
      
      elapsed <- as.numeric(difftime(Sys.time(), self$start_time, units = "secs"))
      rate <- self$state$completed_tests / elapsed
      remaining <- self$state$total_tests - self$state$completed_tests
      eta_seconds <- remaining / rate
      
      return(list(
        seconds = eta_seconds,
        formatted = format_time(eta_seconds)
      ))
    },
    
    # Clean up backend
    cleanup_backend = function() {
      if (self$backend == "console" && !is.null(private$progress_bar)) {
        if (self$config$use_cli && requireNamespace("cli", quietly = TRUE)) {
          cli::cli_progress_done(id = private$progress_bar)
        } else if (inherits(private$progress_bar, "txtProgressBar")) {
          close(private$progress_bar)
        }
      } else if (self$backend == "file" && !is.null(private$file_handle)) {
        close(private$file_handle)
      }
    }
  )
)

#' Default progress monitoring configuration
#' @keywords internal
default_progress_config <- list(
  use_cli = TRUE,
  log_file = NULL,
  update_interval = 0.5,  # seconds
  show_eta = TRUE,
  show_category = TRUE,
  show_test_details = FALSE,
  quiet = FALSE
)

#' Merge progress configuration
#' @keywords internal
merge_progress_config <- function(config) {
  merged <- default_progress_config
  
  for (name in names(config)) {
    if (name %in% names(merged)) {
      merged[[name]] <- config[[name]]
    }
  }
  
  return(merged)
}

#' Format time duration
#' @keywords internal
format_time <- function(seconds) {
  if (is.null(seconds) || is.na(seconds) || seconds < 0) {
    return("--:--")
  }
  
  hours <- floor(seconds / 3600)
  minutes <- floor((seconds %% 3600) / 60)
  secs <- round(seconds %% 60)
  
  if (hours > 0) {
    sprintf("%d:%02d:%02d", hours, minutes, secs)
  } else {
    sprintf("%d:%02d", minutes, secs)
  }
}

#' Create a simple progress callback
#' 
#' @param prefix Prefix for progress messages
#' @export
simple_progress_callback <- function(prefix = "[Progress]") {
  function(event, data, monitor) {
    if (event == "test" && !is.null(data$progress)) {
      cat(sprintf("\r%s %.1f%% complete", prefix, data$progress * 100))
      if (!is.null(data$eta)) {
        cat(sprintf(" - ETA: %s", data$eta$formatted))
      }
      flush.console()
    } else if (event == "complete") {
      cat("\n")
    }
  }
}

#' Create a detailed progress callback
#' 
#' @param log_file Optional file to log progress
#' @export
detailed_progress_callback <- function(log_file = NULL) {
  log_handle <- NULL
  
  if (!is.null(log_file)) {
    log_handle <- file(log_file, open = "w")
  }
  
  function(event, data, monitor) {
    timestamp <- format(Sys.time(), "%H:%M:%S")
    
    msg <- switch(event,
      start = sprintf("[%s] Starting test suite with %d tests", timestamp, data$total_tests),
      category = sprintf("[%s] Category: %s", timestamp, data$category),
      test = sprintf("[%s] Test: %s - %s (%.1f%%)", timestamp, data$test, data$status, 
                     data$progress * 100),
      complete = sprintf("[%s] Completed %d/%d tests in %.1f seconds", timestamp,
                        data$completed_tests, data$total_tests, data$elapsed_time),
      cancel = sprintf("[%s] Test suite cancelled", timestamp),
      paste("[", timestamp, "] Event: ", event, sep = "")
    )
    
    cat(msg, "\n")
    
    if (!is.null(log_handle)) {
      writeLines(msg, log_handle)
      flush(log_handle)
    }
  }
}

#' Create progress monitor for test suite
#' 
#' @param suite Test suite object
#' @param backend Backend type
#' @param config Configuration options
#' @export
create_test_progress_monitor <- function(suite, backend = NULL, config = NULL) {
  # Determine backend from suite config
  if (is.null(backend)) {
    if (isTRUE(suite$config$quiet)) {
      backend <- "none"
    } else if (!is.null(suite$config$progress_log_file)) {
      backend <- "file"
    } else {
      backend <- "console"
    }
  }
  
  # Merge configurations
  monitor_config <- config
  if (is.null(monitor_config)) {
    monitor_config <- list()
  }
  
  if (!is.null(suite$config$progress_log_file)) {
    monitor_config$log_file <- suite$config$progress_log_file
  }
  
  # Create monitor
  monitor <- ProgressMonitor$new(backend, monitor_config)
  
  # Add default callbacks if requested
  if (isTRUE(suite$config$progress_callbacks)) {
    if (!is.null(suite$config$progress_callback_func)) {
      monitor$add_callback(suite$config$progress_callback_func)
    }
  }
  
  return(monitor)
}