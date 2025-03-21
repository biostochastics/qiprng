\name{visualize_prng_tests}
\alias{visualize_prng_tests}
\alias{visualize_basic_tests}
\alias{visualize_runs_tests}
\alias{visualize_correlation_tests}
\alias{visualize_binary_tests}
\alias{visualize_classical_tests}
\alias{visualize_compression_tests}
\alias{visualize_external_tests}
\title{Visualize PRNG test results}
\usage{
visualize_prng_tests(suite, create_report = TRUE)
}
\arguments{
\item{suite}{Test suite object returned by run_prng_test_suite}

\item{create_report}{Logical, whether to create an HTML report (default: TRUE)}
}
\value{
The updated test suite object with visualization paths and report path (if generated)
}
\description{
This function generates comprehensive visualizations for statistical test results 
from the PRNG test suite. It creates plots for each test category and can optionally 
generate an HTML report summarizing all results.
}
\details{
The visualizations include:

\strong{Basic Distribution Tests}:
  \itemize{
    \item Histogram of generated random numbers
    \item QQ plot against theoretical uniform distribution
    \item Empirical vs. theoretical CDF plot
    \item Chi-square residuals plot
  }

\strong{Runs and Independence Tests}:
  \itemize{
    \item Sequence plot showing values over the sequence
    \item Lagged scatter plot (x[i] vs x[i+1])
    \item Runs length distribution
    \item Gap distribution plots
  }

\strong{Correlation Tests}:
  \itemize{
    \item Autocorrelation function (ACF) plot
    \item Partial autocorrelation function (PACF) plot
    \item Spectrum analysis plot
  }

\strong{Binary and Bitwise Tests}:
  \itemize{
    \item Bit frequency by position bar chart
    \item Runs of bits distribution
    \item Bit pattern frequency heatmap
  }

\strong{Classical Tests}:
  \itemize{
    \item Poker hand frequency distribution
    \item Coupon collection time distribution
    \item Birthday spacing distribution
  }

\strong{Compression Tests}:
  \itemize{
    \item Compression ratio comparison
    \item Entropy estimation plot
  }

\strong{External Tests}:
  \itemize{
    \item P-value summary for external package tests
    \item QQ plot of transformed data
  }

All visualizations are saved in the output directory specified in the test suite configuration,
organized in subdirectories by test category.
}
\examples{
\dontrun{
# Run the test suite
results <- run_prng_test_suite(
  prng_func = function(n) generatePRNG(n),
  config = list(
    output_dir = "qiprng_test_results"
  )
)

# Generate visualizations
results <- visualize_prng_tests(results, create_report = TRUE)

# Access the report
if (!is.null(results$report)) {
  browseURL(results$report)
}
}
}
\seealso{
\code{\link{run_prng_test_suite}}, \code{\link{test_qiprng}}
}
