\name{updatePRNG}
\alias{updatePRNG}
\title{Update PRNG Configuration}
\description{
This function updates the configuration of an existing PRNG instance.
}
\usage{
updatePRNG(cfg)
}
\arguments{
\item{cfg}{Configuration parameters (see createPRNG for details)}
}
\value{
NULL invisibly
}
\examples{
# Create PRNG first
cfg <- list(
  a = 1L,
  b = 5L,
  c = -1L,
  mpfr_precision = 53L,
  use_crypto_mixing = TRUE,
  buffer_size = 1024L
)
createPRNG(cfg)

# Update PRNG to use normal distribution
updatePRNG(list(
  a = 1L,
  b = 5L,
  c = -1L,
  distribution = "normal",
  normal_mean = 0,
  normal_sd = 1
))
}
