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
#' Default configuration for PRNG
#' 
#' @export
default_config <- list(
    # Core parameters - ensure b^2 - 4ac > 0
    # With a=2, b=5, c=-2: b^2 - 4ac = 25 - 4(2)(-2) = 25 + 16 = 41 > 0
    a = 2L,
    b = 5L,
    c = -2L,  
    mpfr_precision = 53L,  # Default double precision
    buffer_size = 1024L,
    distribution = "uniform_01",
    range_min = 0,
    range_max = 1,
    normal_mean = 0,
    normal_sd = 1,
    exponential_lambda = 1,
    use_crypto_mixing = FALSE,
    reseed_interval = 1000L,  # Default to 1000 iterations between reseeds
    use_threading = FALSE,    # Enable thread-local PRNG instances
    debug = FALSE
)

# ----------------------------------------------------------------------
#' Validate PRNG configuration
#' 
#' @param config List of configuration parameters
#' @return TRUE if valid, throws error otherwise
#' @keywords internal
validate_config <- function(config) {
    # Check quadratic parameters
    a <- as.integer(config$a)
    b <- as.integer(config$b)
    c <- as.integer(config$c)
    
    # Check discriminant
    D <- b * b - 4 * a * c
    if (D <= 0) {
        stop("Invalid quadratic parameters: discriminant must be positive")
    }
    
    # Check MPFR precision (53-bit is standard double precision)
    # MPFR requires precision to be between 2 and (2^31 - 1 - 256)
    mpfr_precision <- as.integer(config$mpfr_precision)
    if (mpfr_precision < 2 || mpfr_precision > 2147483391L) {  # 2^31 - 1 - 256
        stop("Invalid MPFR precision: must be between 2 and 2147483391")
    }
    
    # Check distribution parameters
    if (!is.null(config$distribution)) {
        dist <- config$distribution
        if (!dist %in% c("uniform_01", "uniform_range", "normal", "exponential")) {
            stop("Invalid distribution: must be one of 'uniform_01', 'uniform_range', 'normal', 'exponential'")
        }
        
        if (dist == "uniform_range") {
            if (config$range_max <= config$range_min) {
                stop("Invalid uniform range: max must be greater than min")
            }
        } else if (dist == "normal") {
            if (config$normal_sd <= 0) {
                stop("Invalid normal distribution: standard deviation must be positive")
            }
        } else if (dist == "exponential") {
            if (config$exponential_lambda <= 0) {
                stop("Invalid exponential distribution: lambda must be positive")
            }
        }
    }
    
    # Check buffer size
    buffer_size <- as.integer(config$buffer_size)
    if (buffer_size < 1) {
        stop("Invalid buffer size: must be positive")
    }
    
    # Check reseed interval
    reseed_interval <- as.integer(config$reseed_interval)
    if (reseed_interval < 1) {
        stop("Invalid reseed interval: must be positive")
    }
    
    TRUE
}

#' Create a new PRNG instance
#' 
#' Creates a new global PRNG instance with the specified configuration.
#' Note: This package uses a global PRNG instance that is protected by a mutex.
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
#' Updates the configuration of the global PRNG instance.
#' Thread-safe: Will block until mutex is acquired.
#' 
#' @param config List of new configuration parameters
#' @export
updatePRNG <- function(config) {
    if (missing(config)) {
        stop("Configuration parameter is required")
    }
    
    # Get current config from C++
    current_config <- .getPRNGConfig_()
    
    # Merge with current config
    config <- modifyList(current_config, config)
    
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
#' Forces a reseed of the global PRNG instance.
#' Thread-safe: Will block until mutex is acquired.
#' 
#' @export
reseedPRNG <- function() {
    .reseedPRNG_()
    invisible(NULL)
}

#' Clean up PRNG resources
#' 
#' Cleans up the global PRNG instance.
#' Thread-safe: Will block until mutex is acquired.
#' 
#' @export
cleanupPRNG <- function() {
    .cleanup_prng_()
    invisible(NULL)
}

#' @export
cleanup_prng <- function() {
    cleanupPRNG()
}

#' Jump ahead in the PRNG sequence
#' 
#' Advances the PRNG state by skipping ahead n numbers.
#' This is more efficient than generating and discarding n numbers.
#' Thread-safe: Will block until mutex is acquired.
#' 
#' @param n Number of steps to jump ahead
#' @export
jumpAheadPRNG <- function(n) {
    if (!is.numeric(n) || n <= 0) {
        stop("n must be a positive number")
    }
    
    .jumpAheadPRNG_(as.numeric(n))
    invisible(NULL)
}

# ----------------------------------------------------------------------
# We store the PRNG instance & lock in a special environment
.prng_env <- new.env(parent = emptyenv())
.prng_env$prng <- NULL