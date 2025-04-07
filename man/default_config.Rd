\name{default_config}
\alias{default_config}
\docType{data}
\title{Default PRNG Configuration}
\description{
Default configuration parameters for the QIPRNG package. These values are used
when creating a new PRNG instance if no custom configuration is provided.
}
\usage{
default_config
}
\format{
A list containing the following parameters:
\describe{
  \item{a}{Integer (2). Quadratic coefficient for ax^2 + bx + c}
  \item{b}{Integer (5). Linear coefficient for ax^2 + bx + c}
  \item{c}{Integer (-2). Constant term for ax^2 + bx + c}
  \item{mpfr_precision}{Integer (53). MPFR precision in bits, standard double precision}
  \item{buffer_size}{Integer (1024). Size of internal buffer for random numbers}
  \item{distribution}{Character ("uniform_01"). Default distribution type, one of "uniform_01", "uniform_range", "normal", "exponential", "poisson", "gamma", "beta"}
  \item{range_min}{Numeric (0). Lower bound for uniform range distribution}
  \item{range_max}{Numeric (1). Upper bound for uniform range distribution}
  \item{normal_mean}{Numeric (0). Mean parameter for normal distribution}
  \item{normal_sd}{Numeric (1). Standard deviation for normal distribution}
  \item{normal_method}{Character ("ziggurat"). Method for generating normal distribution ("ziggurat" or "box_muller")}
  \item{exponential_lambda}{Numeric (1). Rate parameter for exponential distribution}
  \item{poisson_lambda}{Numeric (1). Mean parameter for Poisson distribution}
  \item{gamma_shape}{Numeric (1). Shape parameter for Gamma distribution}
  \item{gamma_scale}{Numeric (1). Scale parameter for Gamma distribution}
  \item{beta_alpha}{Numeric (1). First shape parameter (alpha) for Beta distribution}
  \item{beta_beta}{Numeric (1). Second shape parameter (beta) for Beta distribution}
  \item{use_crypto_mixing}{Logical (FALSE). Whether to apply cryptographic mixing}
  \item{use_parallel_filling}{Logical (TRUE). Whether to use parallel buffer filling}
  \item{use_csv_discriminants}{Logical (FALSE). Whether to use discriminants from a CSV file}
  \item{reseed_interval}{Integer (1000). Number of samples before automatic reseed}
  \item{debug}{Logical (FALSE). Enable debug output}
}
}
\details{
The default configuration provides a balance between performance and quality
of random numbers. The quadratic parameters (a=2, b=5, c=-2) are chosen to
ensure a positive discriminant (b^2 - 4ac = 41 > 0), which is required for
the quadratic irrational to exist.

For normal distribution generation, two methods are available:
\itemize{
  \item \code{ziggurat} (default): An efficient algorithm developed by Marsaglia and Tsang that uses
  precomputed tables to generate normally distributed random numbers very quickly.
  This method is typically 2-3x faster than the Box-Muller transform.
  \item \code{box_muller}: The traditional Box-Muller transform which uses trigonometric
  functions to convert uniform random numbers to normal distribution.
}

Custom discriminants can be provided via a CSV file by setting
\code{use_csv_discriminants=TRUE}. The CSV file should be named "discriminants.csv" 
and placed in the working directory. Each line should contain a single positive integer 
representing a valid discriminant for generating quadratic irrationals.

Parallel buffer filling (\code{use_parallel_filling=TRUE}) utilizes multiple threads to
fill the internal buffer more efficiently while maintaining sequence consistency through 
the jump-ahead functionality.

The default MPFR precision of 53 bits matches IEEE double precision, which
is sufficient for most applications. For higher precision needs, this can
be increased up to 2147483391 bits.

The default buffer size of 1024 provides good performance by reducing the
number of quadratic irrational calculations needed, while keeping memory
usage reasonable.
}
\examples{
# View default configuration
str(default_config)

# Create PRNG with mostly defaults but change distribution
cfg <- modifyList(default_config, list(
  distribution = "normal",
  normal_sd = 2
))
createPRNG(cfg)
}
\seealso{
\code{\link{createPRNG}}, \code{\link{updatePRNG}}
}
