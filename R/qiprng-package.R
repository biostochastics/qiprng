#' Quadratic Irrational PRNG Package
#'
#' Provides a high-quality, thread-safe pseudo-random number generator based on
#' quadratic irrationals with optional cryptographic mixing.
#'
#' @details
#' The QIPRNG package implements a high-precision pseudo-random number generator
#' based on mathematical properties of quadratic irrational numbers. It features:
#' 
#' \itemize{
#'   \item Multiple probability distributions (uniform, normal, exponential, etc.)
#'   \item Optional cryptographic mixing for enhanced security
#'   \item Thread-safe implementation with mutex protection and thread-local storage
#'   \item Configurable precision using MPFR
#'   \item Jump-ahead capability for efficient sequence advancement
#'   \item Automatic reseeding for enhanced statistical quality
#'   \item Buffer management for performance optimization
#' }
#' 
#' Key functions:
#' \itemize{
#'   \item \code{\link{createPRNG}}: Initialize a new PRNG with specified configuration
#'   \item \code{\link{generatePRNG}}: Generate random numbers from current configuration
#'   \item \code{\link{updatePRNG}}: Update PRNG configuration (change distribution, etc.)
#'   \item \code{\link{reseedPRNG}}: Force a manual reseed of the PRNG
#'   \item \code{\link{jumpAheadPRNG}}: Advance the PRNG sequence efficiently
#'   \item \code{\link{cleanupPRNG}}: Clean up PRNG resources when finished
#' }
#'
#' @docType package
#' @name qiprng
#' @author Sergey Kornilov
"_PACKAGE"