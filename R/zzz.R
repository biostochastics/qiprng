# File: zzz.R
# ----------------------------------------------------------------------
#' Package startup and shutdown functions
#'
#' Internal functions for managing the QIPRNG package lifecycle.
#' These functions handle initialization and cleanup of package resources.
#'
#' @name qiprng-internal
#' @keywords internal

#' Package environment
#'
#' Internal environment containing global state for the QIPRNG package.
#' This environment is isolated (has emptyenv as parent) to avoid namespace conflicts.
#'
#' @keywords internal
.pkgenv <- new.env(parent = emptyenv())

#' Package initialization function
#'
#' Called when the package is loaded. Initializes the package environment,
#' sets up the cryptographic backend (libsodium), and registers cleanup handlers.
#'
#' @param libname The library path where the package is installed
#' @param pkgname The name of the package ("qiprng")
#' @keywords internal
.onLoad <- function(libname, pkgname) {
  # Initialize package environment
  # We do NOT define .default_config here anymore to avoid conflicts.
  # The canonical default_config is in R/prng_interface.R

  assign("current_config", NULL, envir = .pkgenv)
  assign("g_prng", NULL, envir = .pkgenv)

  # Initialize libsodium early
  suppressWarnings(try(.initialize_libsodium_(), silent = TRUE))

  # Register finalizer to clean up PRNG resources
  reg.finalizer(.pkgenv, function(e) {
    if (!is.null(e$g_prng)) {
      # Clean up PRNG resources
      rm("g_prng", envir = e)
    }
  }, onexit = TRUE)
}

#' Package unload function
#'
#' Called when the package is unloaded. Cleans up package resources including
#' the PRNG instance and thread pool to prevent memory leaks and ensure proper shutdown.
#'
#' @param libpath The library path where the package is installed
#' @keywords internal
.onUnload <- function(libpath) {
  # Clean up R-level resources
  if (exists("g_prng", envir = .pkgenv)) {
    rm("g_prng", envir = .pkgenv)
  }

  # CRITICAL: Call comprehensive C++ cleanup BEFORE unloading the library
  # This ensures all thread-local storage, MPFR pools, and static objects
  # are cleaned up in the correct order to prevent segfaults.

  # Track cleanup success to conditionally unload library
  cleanup_success <- FALSE
  cleanup_error <- NULL

  tryCatch(
    {
      .prepare_for_unload_()
      cleanup_success <- TRUE
    },
    error = function(e) {
      cleanup_error <<- e$message
      # Log warning about cleanup failure
      warning(
        sprintf("qiprng: C++ cleanup failed during unload: %s. ",
                cleanup_error),
        "Library unload will proceed but may cause instability.",
        call. = FALSE
      )
    }
  )

  # Unload the shared library
  # At this point, all C++ resources should be cleaned up (or we tried our best)
  tryCatch(
    {
      library.dynam.unload("qiprng", libpath)
    },
    error = function(e) {
      # If unload fails after cleanup failure, this is serious
      if (!cleanup_success) {
        warning(
          sprintf("qiprng: Library unload failed after cleanup error: %s",
                  e$message),
          call. = FALSE
        )
      }
    }
  )
}
