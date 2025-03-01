# Helper functions for PRNG testing

#' Setup default PRNG configuration for tests
setup_prng <- function(cfg = list()) {
    # Create PRNG with default or custom config
    qiprng::createPRNG(cfg)
}

#' Generate uniform numbers in a range
#' @param n Number of values to generate
#' @param min Lower bound
#' @param max Upper bound
generate_uniform_range <- function(n, min, max) {
    cfg <- list(
        distribution = "uniform_range",
        range_min = min,
        range_max = max
    )
    qiprng::updatePRNG(cfg)
    qiprng::generatePRNG(n)
}

#' Generate normal random numbers
#' @param n Number of values to generate
#' @param mean Mean of normal distribution
#' @param sd Standard deviation
generate_normal <- function(n, mean = 0, sd = 1) {
    cfg <- list(
        distribution = "normal",
        normal_mean = mean,
        normal_sd = sd
    )
    qiprng::updatePRNG(cfg)
    qiprng::generatePRNG(n)
}

#' Generate exponential random numbers
#' @param n Number of values to generate
#' @param lambda Rate parameter
generate_exponential <- function(n, lambda = 1) {
    cfg <- list(
        distribution = "exponential",
        exponential_lambda = lambda
    )
    qiprng::updatePRNG(cfg)
    qiprng::generatePRNG(n)
}

#' Run statistical tests on generated numbers
#' @param nums Vector of numbers to test
#' @param distribution Expected distribution
#' @param ... Additional parameters for specific distributions
test_distribution <- function(nums, distribution, ...) {
    args <- list(...)
    
    # Basic checks
    testthat::expect_true(is.numeric(nums))
    testthat::expect_false(any(is.na(nums)))
    testthat::expect_false(any(is.infinite(nums)))
    
    if (distribution == "uniform_01") {
        testthat::expect_true(all(nums >= 0 & nums <= 1))
        ks <- stats::ks.test(nums, "punif", 0, 1)
        testthat::expect_true(ks$p.value > 0.05)
    }
    else if (distribution == "uniform_range") {
        min <- args$min
        max <- args$max
        testthat::expect_true(all(nums >= min & nums <= max))
        ks <- stats::ks.test(nums, "punif", min, max)
        testthat::expect_true(ks$p.value > 0.05)
    }
    else if (distribution == "normal") {
        mean <- args$mean %||% 0
        sd <- args$sd %||% 1
        ks <- stats::ks.test(nums, "pnorm", mean, sd)
        testthat::expect_true(ks$p.value > 0.05)
    }
    else if (distribution == "exponential") {
        lambda <- args$lambda %||% 1
        testthat::expect_true(all(nums >= 0))
        ks <- stats::ks.test(nums, "pexp", lambda)
        testthat::expect_true(ks$p.value > 0.05)
    }
}

#' Helper function to run statistical tests
run_statistical_tests <- function(x, distribution = "uniform") {
    # Run KS test
    ks <- stats::ks.test(x, "punif")
    
    # Run runs test
    runs <- sum(diff(x) > 0)
    runs_z <- (runs - (length(x) - 1)/2) / sqrt((length(x) - 1)/4)
    
    # Run FFT test
    centered <- x - mean(x)
    fft_val <- stats::fft(centered)
    spec_sd <- stats::sd(Mod(fft_val))
    
    # Return test results
    list(
        ks_pvalue = ks$p.value,
        runs_z_score = runs_z,
        spectral_sd = spec_sd
    )
}

#' Helper function to clean up PRNG after tests
cleanup_prng <- function() {
    # Reset PRNG state
    qiprng::createPRNG()
}

#' Clean up PRNG state after tests
teardown_prng <- function() {
    # Reset to default configuration
    cfg <- list(
        distribution = "uniform_01",
        use_crypto_mixing = FALSE
    )
    qiprng::updatePRNG(cfg)
}
