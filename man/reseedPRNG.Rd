\name{reseedPRNG}
\alias{reseedPRNG}
\title{Force PRNG Reseed}
\description{
Forces an immediate reseed of the PRNG using hardware-based random number generation.
This is useful when you want to ensure maximum randomness or reset the PRNG state.
}
\usage{
reseedPRNG()
}
\value{
NULL invisibly
}
\details{
The function is thread-safe and will block until the PRNG mutex is acquired.
The reseeding process uses hardware-based random number generation when available,
falling back to system entropy sources if necessary.

While the PRNG automatically reseeds based on the \code{reseed_interval} configuration,
this function allows manual reseeding when desired. Common use cases include:
\itemize{
  \item Before generating cryptographically sensitive random numbers
  \item After detecting potential statistical anomalies
  \item When maximum unpredictability is required
}
}
\examples{
# Force immediate reseed
reseedPRNG()

# Generate numbers after reseeding
x <- generatePRNG(1000)

# Example of periodic manual reseeding
for (i in 1:5) {
  reseedPRNG()
  x <- generatePRNG(100)
  # Process x...
}
}
\seealso{
\code{\link{createPRNG}}, \code{\link{updatePRNG}}, \code{\link{generatePRNG}}
}
