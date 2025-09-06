\name{benchmark_qiprng}
\alias{benchmark_qiprng}
\title{Benchmark QIPRNG against other random number generators}
\usage{
benchmark_qiprng(
  n_values = c(10, 100, 1000, 10000, 100000, 1000000),
  generators = list(
    "qiprng" = function(n) generatePRNG(n),
    "base_r" = function(n) runif(n),
    "dqrng" = function(n) dqrunif(n)
  ),
  distributions = c("uniform", "normal", "exponential"),
  repetitions = 10,
  configs = list(
    "default" = list(),
    "crypto" = list(use_crypto_mixing = TRUE),
    "high_precision" = list(mpfr_precision = 128)
  ),
  export_data = FALSE,
  file = "qiprng_benchmark.rds"
)
}
\arguments{
\item{n_values}{Vector of sample sizes to benchmark}

\item{generators}{List of generator functions to compare; each function should take n as input and return n random numbers}

\item{distributions}{Character vector of distributions to test ("uniform", "normal", "exponential")}

\item{repetitions}{Number of repetitions for each benchmark}

\item{configs}{List of QIPRNG configurations to test}

\item{export_data}{Whether to export raw benchmark data}

\item{file}{Path to save results if export_data is TRUE}
}
\value{
A list containing benchmark results and plots:
  \itemize{
    \item results: Raw benchmark data from microbenchmark
    \item plots: A list of ggplot objects visualizing the results
  }
}
\description{
This function benchmarks the QIPRNG package against other random number generators
in R. It provides detailed performance comparisons across different sample sizes,
configurations, and distributions.
}
\examples{
\dontrun{
# Basic benchmark against R's built-in generator
results <- benchmark_qiprng(
  n_values = c(1000, 10000),
  generators = list(
    "qiprng" = function(n) generatePRNG(n),
    "base_r" = function(n) runif(n)
  ),
  repetitions = 5
)

# Visualize the results
print(results$plots$size_comparison)
}
}
\seealso{
\code{\link{profile_qiprng_config}}, \code{\link{benchmark_large_generation}}, \code{\link{compare_rng_quality}}
}
