\name{profile_qiprng_config}
\alias{profile_qiprng_config}
\title{Profile different QIPRNG configurations}
\usage{
profile_qiprng_config(
  config_list = list(
    "default" = list(),
    "crypto" = list(use_crypto_mixing = TRUE),
    "high_precision" = list(mpfr_precision = 128),
    "normal" = list(distribution = "normal")
  ),
  n = 10000,
  repetitions = 10
)
}
\arguments{
\item{config_list}{List of named configurations to test}

\item{n}{Number of random numbers to generate for each test}

\item{repetitions}{Number of benchmark repetitions}
}
\value{
A data frame with benchmark results for each configuration, including:
  \itemize{
    \item config: Configuration name
    \item mean_time: Mean execution time in milliseconds
    \item median_time: Median execution time in milliseconds
    \item relative_speed: Performance relative to the default configuration
  }
}
\description{
This function profiles the performance of different QIPRNG configurations
to help users identify the optimal settings for their specific use case.
}
\examples{
\dontrun{
# Profile several QIPRNG configurations
profiles <- profile_qiprng_config(
  config_list = list(
    "default" = list(),
    "crypto" = list(use_crypto_mixing = TRUE),
    "high_precision" = list(mpfr_precision = 128),
    "large_buffer" = list(buffer_size = 10000)
  ),
  n = 5000,
  repetitions = 3
)

# View the results
print(profiles)
}
}
\seealso{
\code{\link{benchmark_qiprng}}, \code{\link{benchmark_large_generation}}
}
