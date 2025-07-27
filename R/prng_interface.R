# File: prng_interface.R
# ----------------------------------------------------------------------
#' @useDynLib qiprng, .registration = TRUE
#' @importFrom Rcpp sourceCpp
NULL

# ----------------------------------------------------------------------
#' Default configuration for PRNG
#' 
#' A list containing the default configuration parameters for the QIPRNG generator.
#' This provides a baseline configuration that can be modified when creating a new
#' PRNG instance.
#'
#' @format A list with the following components:
#'   \describe{
#'     \item{a, b, c}{Integer parameters for the quadratic equation (ax² + bx + c)}
#'     \item{mpfr_precision}{Integer precision in bits for MPFR computations}
#'     \item{buffer_size}{Integer size of the buffer for random numbers}
#'     \item{distribution}{String specifying the default distribution}
#'     \item{range_min, range_max}{Numeric bounds for uniform_range distribution}
#'     \item{normal_mean, normal_sd}{Parameters for normal distribution}
#'     \item{exponential_lambda}{Rate parameter for exponential distribution}
#'     \item{poisson_lambda}{Rate parameter for Poisson distribution}
#'     \item{gamma_shape, gamma_scale}{Parameters for gamma distribution}
#'     \item{beta_alpha, beta_beta}{Parameters for beta distribution}
#'     \item{use_crypto_mixing}{Logical: whether to apply cryptographic mixing}
#'     \item{reseed_interval}{Integer: number of iterations between automatic reseeds}
#'     \item{use_threading}{Logical: whether to enable thread-local PRNG instances}
#'     \item{use_csv_discriminants}{Logical: whether to use custom discriminants from file}
#'     \item{use_parallel_filling}{Logical: whether to use parallel buffer filling}
#'     \item{debug}{Logical: whether to enable debugging output}
#'   }
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
    poisson_lambda = 1,
    gamma_shape = 1,
    gamma_scale = 1,
    beta_alpha = 1,
    beta_beta = 1,
    use_crypto_mixing = TRUE,
    reseed_interval = 1000L,  # Default to 1000 iterations between reseeds
    use_threading = FALSE,    # Enable thread-local PRNG instances
    use_csv_discriminants = TRUE,  # Use custom discriminants from discriminants.csv
    use_parallel_filling = FALSE,
    debug = FALSE
)

# ----------------------------------------------------------------------
#' Validate PRNG configuration
#' 
#' Validates the configuration parameters for the PRNG to ensure they meet
#' requirements for mathematical correctness and numerical stability.
#' 
#' @param config List of configuration parameters
#' @return TRUE if valid, throws error otherwise with detailed message
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
    # MPFR implementation restricts precision to 24..10000 bits for stability
    mpfr_precision <- as.integer(config$mpfr_precision)
    if (mpfr_precision < 24 || mpfr_precision > 10000) {
        stop("Invalid MPFR precision: must be 24..10000 bits")
    }
    
    # Check distribution parameters
    if (!is.null(config$distribution)) {
        dist <- config$distribution
        if (!dist %in% c("uniform_01", "uniform_range", "normal", "exponential", "poisson", "gamma", "beta")) {
            stop("Invalid distribution: must be one of 'uniform_01', 'uniform_range', 'normal', 'exponential', 'poisson', 'gamma', 'beta'")
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
        } else if (dist == "poisson") {
            if (config$poisson_lambda <= 0) {
                stop("Invalid poisson distribution: lambda must be positive")
            }
        } else if (dist == "gamma") {
            if (config$gamma_shape <= 0) {
                stop("Invalid gamma distribution: shape must be positive")
            }
            if (config$gamma_scale <= 0) {
                stop("Invalid gamma distribution: scale must be positive")
            }
        } else if (dist == "beta") {
            if (config$beta_alpha <= 0) {
                stop("Invalid beta distribution: alpha must be positive")
            }
            if (config$beta_beta <= 0) {
                stop("Invalid beta distribution: beta must be positive")
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
#' Creates a new global PRNG instance with the specified configuration. This function
#' initializes a new pseudo-random number generator based on quadratic irrationals with 
#' configurable parameters.
#'
#' Thread safety information: This package uses a global PRNG instance that is protected by a mutex.
#' While comprehensive thread safety is provided through mutex protection and thread-local storage,
#' for heavy parallel workloads it is recommended to set `use_threading=TRUE` in the configuration.
#' 
#' @param config List of configuration parameters. Default values are provided by `default_config`.
#'   Possible parameters include:
#'   \describe{
#'     \item{a, b, c}{Integer parameters for the quadratic equation (ax² + bx + c). Must satisfy b² - 4ac > 0}
#'     \item{mpfr_precision}{Integer precision in bits for MPFR computations (24-10000)}
#'     \item{buffer_size}{Integer size of the buffer for random numbers}
#'     \item{distribution}{String specifying the distribution: "uniform_01", "uniform_range", "normal", "exponential", "poisson", "gamma", or "beta"}
#'     \item{range_min, range_max}{Numeric bounds for uniform_range distribution}
#'     \item{normal_mean, normal_sd}{Parameters for normal distribution}
#'     \item{exponential_lambda}{Rate parameter for exponential distribution}
#'     \item{poisson_lambda}{Rate parameter for Poisson distribution}
#'     \item{gamma_shape, gamma_scale}{Parameters for gamma distribution}
#'     \item{beta_alpha, beta_beta}{Parameters for beta distribution}
#'     \item{use_crypto_mixing}{Logical: whether to apply cryptographic mixing}
#'     \item{reseed_interval}{Integer: number of iterations between automatic reseeds}
#'     \item{use_threading}{Logical: whether to enable thread-local PRNG instances}
#'     \item{use_csv_discriminants}{Logical: whether to use custom discriminants from discriminants.csv}
#'     \item{use_parallel_filling}{Logical: whether to use parallel buffer filling}
#'     \item{debug}{Logical: whether to enable debugging output}
#'   }
#' @return Invisibly returns NULL
#' @examples
#' # Create PRNG with default settings
#' createPRNG()
#' 
#' # Create PRNG with custom configuration
#' createPRNG(list(
#'   a = 3L,
#'   b = 7L, 
#'   c = -5L,
#'   distribution = "normal",
#'   normal_mean = 10,
#'   normal_sd = 2,
#'   use_threading = TRUE
#' ))
#' @export
createPRNG <- function(config = default_config) {
    # Initialize libsodium first
    .initialize_libsodium_()
    
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
#' Updates the configuration of the global PRNG instance with new parameters.
#' This allows changing the distribution type, parameters, or other settings
#' of an existing PRNG without recreating it.
#' 
#' Thread safety: This function is fully thread-safe and will block until mutex
#' is acquired. When switching between distributions (e.g., uniform to normal),
#' the implementation ensures a clean transition with proper buffer management
#' and reseeding.
#' 
#' @param config List of new configuration parameters to update. Only specified
#'   parameters will be changed; others retain their current values.
#' @return Invisibly returns NULL
#' @examples
#' # Create default PRNG
#' createPRNG()
#' 
#' # Generate some uniform numbers
#' uniform_values <- generatePRNG(10)
#' 
#' # Switch to normal distribution
#' updatePRNG(list(
#'   distribution = "normal",
#'   normal_mean = 5,
#'   normal_sd = 2
#' ))
#' 
#' # Generate normal values
#' normal_values <- generatePRNG(10)
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
#' Generates random numbers using the current PRNG configuration. The distribution
#' type and parameters are determined by the current configuration set with
#' `createPRNG()` or `updatePRNG()`.
#' 
#' Thread safety: This function is fully thread-safe and will block until mutex
#' is acquired. When `use_threading=TRUE` is set in the configuration, it uses 
#' thread-local storage for improved performance in multi-threaded environments.
#' 
#' @param n Number of random numbers to generate
#' @return Numeric vector of length n with values from the configured distribution
#' @examples
#' # Create default PRNG (uniform 0-1)
#' createPRNG()
#' 
#' # Generate 5 uniform random numbers
#' generatePRNG(5)
#' 
#' # Switch to normal distribution
#' updatePRNG(list(distribution = "normal", normal_mean = 0, normal_sd = 1))
#' 
#' # Generate 5 normally distributed random numbers
#' generatePRNG(5)
#' @export
generatePRNG <- function(n) {
    if (!is.numeric(n) || n <= 0) {
        stop("n must be a positive number")
    }
    
    .generatePRNG_(as.integer(n))
}

#' Reseed the PRNG
#' 
#' Forces a manual reseed of the global PRNG instance. This operation introduces
#' additional entropy into the generator, which can be useful for security-sensitive
#' applications or to ensure sequence independence between different parts of
#' a simulation.
#' 
#' Thread safety: This function is fully thread-safe and will block until mutex
#' is acquired. The implementation ensures that all thread-local resources are
#' properly handled during reseeding.
#' 
#' @return Invisibly returns NULL
#' @examples
#' # Create default PRNG
#' createPRNG()
#' 
#' # Generate some values
#' generatePRNG(10)
#' 
#' # Force a reseed
#' reseedPRNG()
#' 
#' # Generate more values (new sequence after reseed)
#' generatePRNG(10)
#' @export
reseedPRNG <- function() {
    .reseedPRNG_()
    invisible(NULL)
}

#' Clean up PRNG resources
#' 
#' Cleans up the global PRNG instance in a thread-safe manner. This function safely
#' releases all PRNG resources, including thread-local storage, memory buffers,
#' and cryptographic state.
#' 
#' Thread safety: This function implements a two-phase cleanup process that first
#' disables threading to prevent new generations during cleanup, then releases all
#' resources in a controlled manner. This approach ensures memory safety even in
#' multi-threaded environments.
#' 
#' @return Invisibly returns NULL
#' @examples
#' # Create PRNG
#' createPRNG()
#' 
#' # Use the PRNG
#' generatePRNG(10)
#' 
#' # Clean up when done
#' cleanupPRNG()
#' @export
cleanupPRNG <- function() {
    # First disable thread-safe mode to prevent new generations during cleanup
    result <- try({
        # Update configuration to disable thread-safe mode
        current_config <- .getPRNGConfig_()
        if (current_config$use_threading) {
            safe_config <- current_config
            safe_config$use_threading <- FALSE
            .updatePRNG_(safe_config)
        }
        
        # Call thread-safe cleanup which will properly release resources
        .cleanup_prng_()
    }, silent = TRUE)
    
    # If cleanup failed, log error but don't crash
    if (inherits(result, "try-error")) {
        if (getOption("qiprng.debug", FALSE)) {
            warning("PRNG cleanup encountered an error but continued safely: ", result)
        }
    }
    
    # No additional cleanup needed with our improved implementation
    
    invisible(NULL)
}

#' Clean up PRNG resources (alias)
#' 
#' An alias for `cleanupPRNG()` for backward compatibility.
#' 
#' @return Invisibly returns NULL
#' @export
cleanup_prng <- function() {
    cleanupPRNG()
}

#' Control MPFR warning messages
#'
#' Enables or disables warnings about inexact results in MPFR operations.
#' Some inexact results are normal and expected in MPFR operations, especially
#' for irrational numbers and square roots. This function lets you control
#' whether these warnings are shown.
#'
#' @param suppress Logical: TRUE to suppress warnings, FALSE to show them
#' @return The previous setting (invisibly)
#' @examples
#' # Suppress MPFR warnings
#' suppressMPFRWarnings(TRUE)
#' 
#' # Create PRNG with high precision
#' createPRNG(list(mpfr_precision = 256L))
#' 
#' # Generate values without warnings
#' values <- generatePRNG(10)
#' 
#' # Re-enable warnings if needed
#' suppressMPFRWarnings(FALSE)
#' @export
suppressMPFRWarnings <- function(suppress = TRUE) {
    if (!is.logical(suppress)) {
        stop("suppress must be a logical value (TRUE or FALSE)")
    }
    previous <- .suppress_mpfr_warnings_()
    .set_mpfr_warnings_(!suppress)
    invisible(previous)
}

#' Jump ahead in the PRNG sequence
#' 
#' Advances the PRNG state by skipping ahead n numbers. This is significantly more
#' efficient than generating and discarding n numbers, as it uses mathematical properties
#' of quadratic irrationals to compute the state directly.
#' 
#' Thread safety: This function is fully thread-safe and will block until mutex
#' is acquired. The implementation ensures proper synchronization of all thread-local
#' storage during jump-ahead operations.
#' 
#' @param n Number of steps to jump ahead (positive integer or numeric value)
#' @return Invisibly returns NULL
#' @examples
#' # Create default PRNG
#' createPRNG()
#' 
#' # Generate some initial values
#' initial_values <- generatePRNG(5)
#' 
#' # Jump ahead 1000 values
#' jumpAheadPRNG(1000)
#' 
#' # Generate values after the jump
#' after_jump_values <- generatePRNG(5)
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