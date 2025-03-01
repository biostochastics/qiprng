\name{createPRNG}
\alias{createPRNG}
\title{Create and Initialize the Enhanced PRNG}
\description{
This function creates and initializes the PRNG with the given configuration.
}
\usage{
createPRNG(cfg = list())
}
\arguments{
\item{cfg}{A named list of configuration parameters:
\itemize{
  \item a: Quadratic coefficient (default: 2)
  \item b: Linear coefficient (default: 1)
  \item c: Constant term (default: -1)
  \item mpfr_precision: MPFR precision in bits (default: 53)
  \item use_crypto_mixing: Whether to use AES-CTR mixing (default: TRUE)
  \item buffer_size: Number of random values to buffer (default: 1024)
  \item distribution: Distribution type (default: "uniform_01")
  \item range_min: Lower bound for uniform_range distribution (default: 0)
  \item range_max: Upper bound for uniform_range distribution (default: 1)
  \item normal_mean: Mean for normal distribution (default: 0)
  \item normal_sd: Standard deviation for normal distribution (default: 1)
  \item exponential_lambda: Rate for exponential distribution (default: 1)
}}
}
\value{
NULL invisibly
}
\examples{
# Create a PRNG with default settings
cfg <- list(
  a = 1L,
  b = 5L,
  c = -1L,
  mpfr_precision = 53L,
  use_crypto_mixing = TRUE,
  buffer_size = 1024L
)
createPRNG(cfg)
}
