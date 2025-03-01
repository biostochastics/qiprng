# File: prng_interface.R
# ----------------------------------------------------------------------
#' Enhanced Quadratic Irrational PRNG
#'
#' A package implementing a high-precision quadratic irrational PRNG
#' with optional cryptographic mixing, hardware RNG, automatic reseeding,
#' multiple distributions, etc.
#'
#' @docType package
#' @name qiprng
#' @useDynLib qiprng, .registration = TRUE
#' @importFrom Rcpp sourceCpp
NULL

# ----------------------------------------------------------------------
# The SINGLE authoritative default_config aligned with C++ PRNGDefaults
default_config <- list(
  # Core parameters
  a = 2L,  # PRNGDefaults::a
  b = 5L,  # PRNGDefaults::b
  c = -2L, # PRNGDefaults::c
  mpfr_precision = 53L,   # PRNGDefaults::mpfr_precision
  buffer_size = 1024L,    # PRNGDefaults::buffer_size
  
  # Distribution parameters
  distribution = "uniform_01",
  range_min = 0.0,
  range_max = 1.0,
  normal_mean = 0.0,
  normal_sd = 1.0,
  exponential_lambda = 1.0,
  
  # Security parameters
  use_crypto_mixing = TRUE,
  use_hardware_rng = FALSE,
  reseed_interval = 1000000L
)

# ----------------------------------------------------------------------
#' Validate PRNG configuration
#' 
#' @param config List of configuration parameters
#' @return TRUE if valid, throws error otherwise
#' @keywords internal
validate_config <- function(config) {
    # Only perform basic type checking and user-friendly validations here
    # Core parameter validation is handled by C++ code
    
    if (!is.list(config)) {
        stop("Configuration must be a list")
    }
    
    # Required parameters
    required <- c("a", "b", "c", "mpfr_precision", "buffer_size")
    missing <- setdiff(required, names(config))
    if (length(missing) > 0) {
        stop("Missing required parameters: ", paste(missing, collapse=", "))
    }
    
    # Type checking
    if (!is.numeric(config$a)) stop("Parameter 'a' must be numeric")
    if (!is.numeric(config$b)) stop("Parameter 'b' must be numeric")
    if (!is.numeric(config$c)) stop("Parameter 'c' must be numeric")
    if (!is.numeric(config$mpfr_precision)) stop("Parameter 'mpfr_precision' must be numeric")
    if (!is.numeric(config$buffer_size)) stop("Parameter 'buffer_size' must be numeric")
    
    # Distribution validation
    if (!is.null(config$distribution)) {
        valid_dist <- c("uniform_01", "uniform_range", "normal", "exponential")
        if (!config$distribution %in% valid_dist) {
            stop("Invalid distribution. Must be one of: ", paste(valid_dist, collapse=", "))
        }
    }
    
    # Note: Detailed numeric range validation is handled by C++ code
    TRUE
}

#' Create a new PRNG instance
#' 
#' Creates a new Quadratic Irrational PRNG instance with the specified configuration.
#' Note: This package currently uses a global PRNG instance that is protected by a mutex.
#' While basic thread safety is provided, for heavy parallel workloads it is recommended
#' to create separate PRNG instances in each thread.
#' 
#' @param config List of configuration parameters. See default_config for available options.
#' @export
createPRNG <- function(config = default_config) {
    # Merge with defaults
    config <- modifyList(default_config, config)
    
    # Basic validation
    validate_config(config)
    
    # Create PRNG
    .createPRNG_(config)
    
    invisible(NULL)
}

#' Update PRNG configuration
#' 
#' Updates the configuration of the current PRNG instance.
#' Thread-safe: Will block until mutex is acquired.
#' 
#' @param config List of new configuration parameters
#' @export
updatePRNG <- function(config) {
    if (missing(config)) {
        stop("Configuration parameter is required")
    }
    
    # Merge with current config (stored in C++)
    config <- modifyList(default_config, config)
    
    # Basic validation
    validate_config(config)
    
    # Update PRNG
    .updatePRNG_(config)
    
    invisible(NULL)
}

#' Generate random numbers
#' 
#' Generates random numbers using the current PRNG configuration.
#' Thread-safe: Will block until mutex is acquired.
#' 
#' @param n Number of random numbers to generate
#' @return Numeric vector of length n
#' @export
generatePRNG <- function(n) {
    if (!is.numeric(n) || n <= 0) {
        stop("n must be a positive number")
    }
    
    .generatePRNG_(as.integer(n))
}

#' Reseed the PRNG
#' 
#' @param prng PRNG instance
#' @export
reseedPRNG <- function(prng) {
    if (!inherits(prng, "qiprng")) {
        stop("Invalid PRNG instance")
    }
    .reseedPRNG_(prng)
    invisible(NULL)
}

#' Clean up PRNG resources
#' 
#' @param prng PRNG instance
#' @export
cleanup_prng <- function(prng) {
    if (inherits(prng, "qiprng")) {
        .cleanup_prng_(prng)
    }
    invisible(NULL)
}

# ----------------------------------------------------------------------
# We store the PRNG instance & lock in a special environment
.prng_env <- new.env(parent = emptyenv())
.prng_env$prng <- NULL