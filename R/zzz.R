# File: zzz.R
# ----------------------------------------------------------------------
# Package environment setup

.pkgenv <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  # Initialize package environment
  # We do NOT define .default_config here anymore to avoid conflicts.
  # The canonical default_config is in R/prng_interface.R
  
  assign("current_config", NULL, envir = .pkgenv)
  assign("g_prng", NULL, envir = .pkgenv)

  # Register finalizer to clean up PRNG resources
  reg.finalizer(.pkgenv, function(e) {
    if (!is.null(e$g_prng)) {
      # Clean up PRNG resources
      rm("g_prng", envir = e)
    }
  }, onexit = TRUE)
}

.onUnload <- function(libpath) {
  # Clean up PRNG resources
  if (exists("g_prng", envir = .pkgenv)) {
    rm("g_prng", envir = .pkgenv)
  }
}