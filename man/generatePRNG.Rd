\name{generatePRNG}
\alias{generatePRNG}
\title{Generate Random Numbers from the PRNG}
\description{
This function generates random numbers using the initialized PRNG.
}
\usage{
generatePRNG(n, cfg = NULL)
}
\arguments{
\item{n}{Number of random values to generate}

\item{cfg}{Optional configuration updates to apply before generation (default: NULL)}
}
\value{
Numeric vector of length n containing random numbers according to the configured distribution
}
\examples{
# Generate 1000 random numbers
nums <- generatePRNG(1000)
hist(nums, breaks = 20)
}
