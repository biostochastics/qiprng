\name{createPRNG}
\alias{createPRNG}
\title{Create a New PRNG Instance}
\description{
Creates a new global PRNG instance with the specified configuration.
The PRNG uses quadratic irrational numbers with optional cryptographic mixing
and supports multiple probability distributions.
}
\usage{
createPRNG(config = default_config)
}
\arguments{
\item{config}{A list of configuration parameters:
  \itemize{
    \item \code{a}, \code{b}, \code{c}: Integer coefficients for quadratic equation (ax^2 + bx + c). Must satisfy b^2 - 4ac > 0
    \item \code{mpfr_precision}: Integer, MPFR precision in bits (2 to 2147483391)
    \item \code{buffer_size}: Integer > 0, size of internal buffer for random numbers
    \item \code{distribution}: String, one of "uniform_01", "uniform_range", "normal", "exponential"
    \item \code{range_min}, \code{range_max}: Numeric, range for uniform distribution
    \item \code{normal_mean}, \code{normal_sd}: Numeric, parameters for normal distribution (sd > 0)
    \item \code{exponential_lambda}: Numeric > 0, rate parameter for exponential distribution
    \item \code{use_crypto_mixing}: Logical, whether to apply cryptographic mixing
    \item \code{reseed_interval}: Integer > 0, number of samples before automatic reseed
    \item \code{debug}: Logical, enable debug output
  }
}
}
\value{
NULL invisibly. Creates a global PRNG instance protected by a mutex.
}
\details{
The PRNG uses high-precision arithmetic via MPFR and can optionally apply
cryptographic mixing for enhanced security. The implementation is thread-safe,
using mutex locks for all operations.

For parallel workloads, it's recommended to create separate PRNG instances
in each thread rather than sharing a single instance.

The quadratic parameters (a, b, c) must satisfy b^2 - 4ac > 0 to ensure
the quadratic irrational exists and has the desired properties for random
number generation.
}
\examples{
# Create PRNG with default settings
createPRNG()

# Create PRNG for normal distribution
cfg <- list(
  distribution = "normal",
  normal_mean = 0,
  normal_sd = 1,
  use_crypto_mixing = TRUE
)
createPRNG(cfg)

# Create PRNG for exponential distribution
cfg <- list(
  distribution = "exponential",
  exponential_lambda = 0.5,
  reseed_interval = 5000
)
createPRNG(cfg)

# Create PRNG for custom uniform range
cfg <- list(
  distribution = "uniform_range",
  range_min = -10,
  range_max = 10,
  buffer_size = 2048
)
createPRNG(cfg)
}
\seealso{
\code{\link{updatePRNG}}, \code{\link{generatePRNG}}, \code{\link{reseedPRNG}}
}
