\name{run_prng_test_suite}
\alias{run_prng_test_suite}
\title{Run comprehensive statistical tests on PRNG output}
\usage{
run_prng_test_suite(
  prng_func,
  config = list(
    basic_sample_size = 10000,
    runs_sample_size = 10000,
    correlation_sample_size = 10000,
    binary_sample_size = 100000,
    classical_sample_size = 10000,
    compression_sample_size = 10000,
    external_sample_size = 10000,
    significance_level = 0.01,
    output_dir = "prng_test_results"
  )
)
}
\arguments{
\item{prng_func}{Function that generates random numbers, taking n as parameter and returning n uniform random numbers in [0,1]}

\item{config}{List of configuration options for the test suite, including:
  \itemize{
    \item basic_sample_size: Sample size for basic distribution tests
    \item runs_sample_size: Sample size for runs and independence tests
    \item correlation_sample_size: Sample size for correlation tests
    \item binary_sample_size: Sample size for binary and bitwise tests
    \item classical_sample_size: Sample size for classical tests
    \item compression_sample_size: Sample size for compression tests
    \item external_sample_size: Sample size for external package tests
    \item significance_level: Alpha level for statistical tests
    \item output_dir: Directory for saving test results and visualizations
  }
}
}
\value{
A test suite object containing:
  \itemize{
    \item config: The configuration used for testing
    \item prng_func: The generator function being tested
    \item results: Nested list of test results by category
    \item visualizations: Paths to generated visualization files
    \item report: Path to the generated HTML report if created
    \item summary: Overall summary of test results
  }
}
\description{
This function runs a comprehensive battery of statistical tests to evaluate 
the quality of a pseudo-random number generator (PRNG). It covers tests for 
uniformity, independence, correlation, bit-level properties, and more.
}
\details{
The test suite is organized into several categories:

\strong{Basic Distribution Tests}:
  \itemize{
    \item Kolmogorov-Smirnov test for uniformity
    \item Chi-square goodness-of-fit test
    \item Distribution moments test (mean, variance, skewness, kurtosis)
    \item Min/max tests
  }

\strong{Runs and Independence Tests}:
  \itemize{
    \item Runs test (above/below median)
    \item Gap test
    \item Turning points test
    \item Rank test
    \item Serial independence tests
  }

\strong{Correlation Tests}:
  \itemize{
    \item Autocorrelation function (ACF) test
    \item Partial autocorrelation function (PACF) test
    \item Ljung-Box test
    \item Serial correlation test
  }

\strong{Binary and Bitwise Tests}:
  \itemize{
    \item Monobit (frequency) test
    \item Bit frequency by position test
    \item Bit runs test
    \item Serial test (bit patterns)
  }

\strong{Classical Tests}:
  \itemize{
    \item Poker test
    \item Coupon collector test
    \item Birthday spacings test
  }

\strong{Compression Tests}:
  \itemize{
    \item Compression ratio test
    \item Entropy estimation
  }

\strong{External Package Tests}:
  \itemize{
    \item Additional tests from randtests package
    \item Tests from nortest package
  }

The function also creates visualizations to help interpret the test results, including histograms, 
QQ plots, autocorrelation plots, and more. If rmarkdown is available, it will generate a 
comprehensive HTML report of all test results.
}
\examples{
\dontrun{
# Test R's built-in PRNG
results <- run_prng_test_suite(
  prng_func = function(n) runif(n),
  config = list(
    basic_sample_size = 10000,
    significance_level = 0.01,
    output_dir = "r_prng_results"
  )
)

# Test QIPRNG
results <- run_prng_test_suite(
  prng_func = function(n) generatePRNG(n),
  config = list(
    output_dir = "qiprng_results"
  )
)

# View summary of test results
print(results$summary)
}
}
\seealso{
\code{\link{test_qiprng}}, \code{\link{compare_rng_quality}}
}
