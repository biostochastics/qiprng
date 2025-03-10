\name{updatePRNG}
\alias{updatePRNG}
\title{Update PRNG Configuration}
\description{
Updates the configuration of the global PRNG instance. This function allows changing
distribution types, parameters, and other settings while preserving the current
PRNG state.
}
\usage{
updatePRNG(config)
}
\arguments{
\item{config}{A list of configuration parameters to update. See \code{\link{createPRNG}}
for the full list of available parameters. Only specified parameters will be updated;
others retain their current values.}
}
\value{
NULL invisibly
}
\details{
The function is thread-safe and will block until the PRNG mutex is acquired.
All parameter validation is performed before any changes are applied.

When changing distribution types, appropriate parameters for the new distribution
must be provided. For example, when switching to normal distribution, both
\code{normal_mean} and \code{normal_sd} should be specified.
}
\examples{
# Switch to normal distribution
updatePRNG(list(
  distribution = "normal",
  normal_mean = 0,
  normal_sd = 1
))

# Update uniform range and increase buffer size
updatePRNG(list(
  distribution = "uniform_range",
  range_min = -5,
  range_max = 5,
  buffer_size = 2048
))

# Enable crypto mixing and set reseed interval
updatePRNG(list(
  use_crypto_mixing = TRUE,
  reseed_interval = 5000
))

# Switch to exponential distribution
updatePRNG(list(
  distribution = "exponential",
  exponential_lambda = 0.5
))
}
\seealso{
\code{\link{createPRNG}}, \code{\link{generatePRNG}}, \code{\link{reseedPRNG}}
}
