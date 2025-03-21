\name{compare_rng_quality}
\alias{compare_rng_quality}
\title{Compare the quality of random numbers from different generators}
\usage{
compare_rng_quality(
  n = 50000,
  generators = list(
    "qiprng" = function(n) generatePRNG(n),
    "base_r" = function(n) runif(n)
  ),
  save_plots = TRUE,
  output_dir = "quality_comparison"
)
}
\arguments{
\item{n}{Sample size to use for quality assessment}

\item{generators}{List of generator functions to compare}

\item{save_plots}{Whether to save quality comparison plots to disk}

\item{output_dir}{Directory to save plots if save_plots is TRUE}
}
\value{
A list containing:
  \itemize{
    \item metrics: Data frame with statistical quality metrics for each generator
    \item plots: List of ggplot objects visualizing the quality assessments
    \item raw_data: Raw random number samples used in the assessment
  }
}
\description{
This function performs a comprehensive quality assessment of different random number 
generators, applying various statistical tests and visualizations to evaluate 
randomness, uniformity, and independence properties.
}
\examples{
\dontrun{
# Compare quality between QIPRNG and R's built-in generator
quality <- compare_rng_quality(
  n = 20000,
  generators = list(
    "qiprng" = function(n) generatePRNG(n),
    "base_r" = function(n) runif(n)
  ),
  save_plots = FALSE
)

# View quality metrics
print(quality$metrics)

# Display one of the comparison plots
print(quality$plots$qq_plot)
}
}
\seealso{
\code{\link{benchmark_qiprng}}, \code{\link{run_prng_test_suite}}
}
