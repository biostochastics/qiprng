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
#' the PRNG instance to prevent memory leaks and ensure proper shutdown.
#'
#' @param libpath The library path where the package is installed
#' @keywords internal
.onUnload <- function(libpath) {
  # Clean up PRNG resources
  if (exists("g_prng", envir = .pkgenv)) {
    rm("g_prng", envir = .pkgenv)
  }
}
