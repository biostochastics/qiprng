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
    \item \code{normal_method}: String, one of "ziggurat" (default, faster) or "box_muller" (traditional)
    \item \code{exponential_lambda}: Numeric > 0, rate parameter for exponential distribution
    \item \code{use_crypto_mixing}: Logical, whether to apply cryptographic mixing
    \item \code{use_parallel_filling}: Logical, whether to use parallel buffer filling
    \item \code{use_csv_discriminants}: Logical, whether to use discriminants from "discriminants.csv"
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

For normal distribution generation, two methods are available:
\itemize{
  \item \bold{Ziggurat algorithm} (default): An efficient algorithm developed by Marsaglia 
  and Tsang that uses precomputed tables to generate normally distributed random numbers. 
  It's typically 2-3x faster than the Box-Muller transform and has excellent statistical 
  properties.
  \item \bold{Box-Muller transform}: The traditional method that uses trigonometric 
  functions to convert uniform random numbers to a normal distribution.
}

Custom discriminants can be provided by creating a file named "discriminants.csv" 
in the working directory and setting \code{use_csv_discriminants=TRUE}. Each line 
of the CSV file should contain a single positive integer representing a valid 
discriminant (D = b^2 - 4ac > 0) for generating quadratic irrationals. This 
allows more control over the specific quadratic irrationals used in the sequence.

Parallel buffer filling is enabled by default and uses multiple CPU threads to 
fill the random number buffer more efficiently, while maintaining sequence 
consistency through the jump-ahead functionality.

For parallel workloads, it's recommended to create separate PRNG instances
in each thread rather than sharing a single instance.

The quadratic parameters (a, b, c) must satisfy b^2 - 4ac > 0 to ensure
the quadratic irrational exists and has the desired properties for random
number generation.
}
\examples{
# Create PRNG with default settings
createPRNG()

# Create PRNG for normal distribution with Ziggurat algorithm (default)
cfg <- list(
  distribution = "normal",
  normal_mean = 0,
  normal_sd = 1,
  normal_method = "ziggurat",
  use_crypto_mixing = TRUE
)
createPRNG(cfg)

# Create PRNG for normal distribution with Box-Muller transform
cfg <- list(
  distribution = "normal",
  normal_mean = 0,
  normal_sd = 1,
  normal_method = "box_muller"
)
createPRNG(cfg)

# Create PRNG with custom discriminants from CSV
# Assuming you have a "discriminants.csv" file in the working directory
cfg <- list(
  use_csv_discriminants = TRUE,
  buffer_size = 2048
)
createPRNG(cfg)

# Create PRNG for exponential distribution
cfg <- list(
  distribution = "exponential",
  exponential_lambda = 0.5,
  reseed_interval = 5000
)
createPRNG(cfg)

# Create PRNG with parallel buffer filling disabled
cfg <- list(
  distribution = "uniform_range",
  range_min = -10,
  range_max = 10,
  buffer_size = 2048,
  use_parallel_filling = FALSE
)
createPRNG(cfg)
}
\seealso{
\code{\link{updatePRNG}}, \code{\link{generatePRNG}}, \code{\link{reseedPRNG}}
}
