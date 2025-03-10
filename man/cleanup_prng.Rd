\name{cleanup_prng}
\alias{cleanup_prng}
\title{Clean Up PRNG Resources}
\description{
Safely releases all resources associated with the global PRNG instance.
This includes freeing memory buffers, releasing the mutex lock, and
cleaning up any cryptographic state.
}
\usage{
cleanup_prng()
}
\value{
NULL invisibly
}
\details{
The function is thread-safe and will block until the PRNG mutex is acquired.
It should be called when you are completely finished using the PRNG to ensure
proper cleanup of system resources.

Key cleanup operations include:
\itemize{
  \item Securely wiping and freeing the random number buffer
  \item Cleaning up cryptographic state if crypto mixing was enabled
  \item Releasing the mutex lock
  \item Resetting the PRNG configuration to defaults
}

After calling this function, you will need to call \code{\link{createPRNG}}
again before generating more random numbers.
}
\examples{
# Create and use PRNG
createPRNG()
x <- generatePRNG(1000)

# Clean up when finished
cleanup_prng()

# Must recreate before using again
createPRNG()
}
\seealso{
\code{\link{createPRNG}}, \code{\link{updatePRNG}}, \code{\link{generatePRNG}}
}
