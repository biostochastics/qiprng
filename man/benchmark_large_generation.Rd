\name{benchmark_large_generation}
\alias{benchmark_large_generation}
\title{Benchmark large-scale random number generation}
\usage{
benchmark_large_generation(
  n_values = c(1e6, 5e6, 1e7, 5e7),
  configs = list(
    "default" = list(),
    "optimized" = list(buffer_size = 1e6)
  ),
  generators = list(
    "qiprng" = function(n) generatePRNG(n),
    "base_r" = function(n) runif(n)
  ),
  repetitions = 3
)
}
\arguments{
\item{n_values}{Vector of large sample sizes to benchmark}

\item{configs}{List of QIPRNG configurations to test}

\item{generators}{List of generator functions to compare}

\item{repetitions}{Number of repetitions for each benchmark}
}
\value{
A list containing:
  \itemize{
    \item results: Raw benchmark data
    \item summary: Summary statistics for each generator and configuration
    \item plots: A list of ggplot objects visualizing the results
  }
}
\description{
This function focuses on benchmarking the performance of random number generators
when generating large quantities of random numbers. It is particularly useful for
evaluating the scaling behavior and memory efficiency of different generators.
}
\examples{
\dontrun{
# Compare QIPRNG to base R for large sample sizes
results <- benchmark_large_generation(
  n_values = c(1e6, 5e6),
  configs = list(
    "default" = list(),
    "large_buffer" = list(buffer_size = 100000)
  ),
  repetitions = 2
)

# View the results
print(results$summary)
print(results$plots$time_plot)
}
}
\seealso{
\code{\link{benchmark_qiprng}}, \code{\link{profile_qiprng_config}}
}
