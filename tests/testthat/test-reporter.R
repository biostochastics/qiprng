# Custom test reporter that writes detailed output to a file
DetailedFileReporter <- R6::R6Class("DetailedFileReporter",
  inherit = testthat::Reporter,
  public = list(
    output_file = NULL,
    
    initialize = function(file = "test_log.txt") {
      super$initialize()
      self$output_file <- file
      # Clear the file at start
      cat("", file = self$output_file)
    },
    
    start_test = function(context, test) {
      cat(sprintf("\n[TEST] %s: %s\n", context, test), 
          file = self$output_file, append = TRUE)
    },
    
    add_result = function(context, test, result) {
      status <- if (inherits(result, "expectation_success")) "PASS" else "FAIL"
      message <- if (!is.null(result$message)) result$message else ""
      
      output <- sprintf("[%s] %s\n%s\n", 
                       status,
                       test,
                       message)
      
      if (inherits(result, "expectation_failure")) {
        if (!is.null(result$call)) {
          output <- paste0(output, "Call: ", deparse(result$call), "\n")
        }
        if (!is.null(result$trace)) {
          output <- paste0(output, "Trace:\n", paste(result$trace, collapse = "\n"), "\n")
        }
      }
      
      cat(output, file = self$output_file, append = TRUE)
    },
    
    end_reporter = function() {
      summary <- self$get_summary()
      cat(sprintf("\nTest Summary:\n============\n"), 
          file = self$output_file, append = TRUE)
      cat(sprintf("Total Tests: %d\n", sum(summary)),
          sprintf("Passed: %d\n", summary["success"]),
          sprintf("Failed: %d\n", summary["failure"]),
          sprintf("Skipped: %d\n", summary["skip"]),
          sprintf("Warnings: %d\n", summary["warning"]),
          file = self$output_file, append = TRUE)
    }
  )
)
