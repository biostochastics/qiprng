\name{generatePRNG}
\alias{generatePRNG}
\title{Generate Random Numbers}
\description{
Generates random numbers using the current PRNG configuration. The distribution
type and parameters are determined by the current configuration set via
\code{\link{createPRNG}} or \code{\link{updatePRNG}}.
}
\usage{
generatePRNG(n)
}
\arguments{
\item{n}{A positive integer specifying the number of random numbers to generate}
}
\value{
A numeric vector of length \code{n} containing random numbers from the configured distribution:
\itemize{
  \item For \code{uniform_01}: Numbers in [0,1]
  \item For \code{uniform_range}: Numbers in [range_min, range_max]
  \item For \code{normal}: Normal distribution with specified mean and standard deviation
  \item For \code{exponential}: Exponential distribution with specified rate lambda
}
}
\details{
The function is thread-safe and will block until the PRNG mutex is acquired.
Numbers are generated efficiently using an internal buffer, with automatic
reseeding based on the configured \code{reseed_interval}.

When cryptographic mixing is enabled, each buffer of random numbers is mixed
using a secure cryptographic function to enhance the statistical properties
and security of the output.
}
\examples{
# Generate 1000 uniform numbers in [0,1]
u <- generatePRNG(1000)

# Switch to normal and generate samples
updatePRNG(list(distribution = "normal"))
x <- generatePRNG(1000)
hist(x)  # Should show normal distribution

# Generate exponential samples
updatePRNG(list(
  distribution = "exponential",
  exponential_lambda = 2
))
y <- generatePRNG(500)
hist(y)  # Should show exponential distribution
}
\seealso{
\code{\link{createPRNG}}, \code{\link{updatePRNG}}, \code{\link{reseedPRNG}}
}
