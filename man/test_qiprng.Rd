\name{test_qiprng}
\alias{test_qiprng}
\title{Run statistical tests on the PRNG}
\usage{
test_qiprng(n = 10000, cfg = NULL)
}
\arguments{
\item{n}{Number of samples to use in tests (default: 10000)}

\item{cfg}{Optional configuration to use for testing (default: NULL)}
}
\value{
List containing test results:
  \itemize{
    \item ks_pvalue: p-value from Kolmogorov-Smirnov test for uniformity
    \item runs_z_score: Z-score from runs test for independence
    \item spectral_sd: Standard deviation of FFT components
  }
}
\description{
This function performs statistical tests on the PRNG output to verify
its quality.
}
\examples{
# Run tests with default configuration
results <- test_qiprng()

# Run tests with custom configuration
cfg <- list(
  a = 2L,
  b = 0L,
  c = -1L,
  mpfr_precision = 53L,
  use_crypto_mixing = TRUE,
  buffer_size = 1024L
)
results <- test_qiprng(10000, cfg)
}
