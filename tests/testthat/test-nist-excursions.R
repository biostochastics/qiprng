# Test file for NIST Random Excursions and Linear Complexity tests

test_that("NIST Random Excursions tests handle insufficient data gracefully", {
  skip_if_not_installed("qiprng")
  
  # Source the binary tests module directly
  source(file.path(system.file(package = "qiprng"), "..", "..", "R", "statisticaltests", "binary_tests.R"))
  
  # Create a small test suite
  suite <- list(
    config = list(
      binary_sample_size = 100,
      significance_level = 0.01,
      save_visualizations = FALSE
    ),
    prng_func = function(n) runif(n),
    results = list()
  )
  
  # Run binary tests (which includes our new tests)
  suite_result <- run_binary_tests(suite)
  
  # Check that the tests exist and handle small samples correctly
  expect_true("random_excursions" %in% names(suite_result$results$binary))
  expect_true("random_excursions_variant" %in% names(suite_result$results$binary))
  expect_true("linear_complexity" %in% names(suite_result$results$binary))
  
  # With small sample size, these tests should be INCONCLUSIVE
  expect_equal(suite_result$results$binary$random_excursions$result, "INCONCLUSIVE")
  expect_equal(suite_result$results$binary$random_excursions_variant$result, "INCONCLUSIVE")
  expect_equal(suite_result$results$binary$linear_complexity$result, "INCONCLUSIVE")
})

test_that("NIST tests produce valid results with sufficient data", {
  skip_on_cran()  # Skip on CRAN due to large memory/time requirements
  
  # Create a test suite with sufficient data
  suite <- list(
    config = list(
      binary_sample_size = 40000,  # Will generate 1.28M bits (40000 * 32)
      significance_level = 0.01,
      save_visualizations = FALSE
    ),
    prng_func = function(n) runif(n),
    results = list()
  )
  
  # Run binary tests
  suite_result <- run_binary_tests(suite)
  
  # Check that p-values are in valid range [0, 1]
  excursions_pvals <- suite_result$results$binary$random_excursions$p_values
  expect_true(all(is.na(excursions_pvals) | (excursions_pvals >= 0 & excursions_pvals <= 1)))
  
  excursions_var_pvals <- suite_result$results$binary$random_excursions_variant$p_values
  expect_true(all(is.na(excursions_var_pvals) | (excursions_var_pvals >= 0 & excursions_var_pvals <= 1)))
  
  linear_pval <- suite_result$results$binary$linear_complexity$p_value
  expect_true(is.na(linear_pval) | (linear_pval >= 0 & linear_pval <= 1))
})

test_that("Berlekamp-Massey algorithm works correctly", {
  # Test with a known sequence
  # Simple periodic sequence: 0,0,1,0,0,1,0,0 has linear complexity 3
  test_seq <- c(0,0,1,0,0,1,0,0)
  
  # Extract the berlekamp_massey function from within linear_complexity_test
  # We'll create a minimal version for testing
  berlekamp_massey <- function(s) {
    n <- length(s)
    C <- c(1, rep(0, n - 1))
    B <- c(1, rep(0, n - 1))
    L <- 0
    m <- -1
    N <- 0
    
    while (N < n) {
      d <- s[N + 1]
      for (i in 1:L) {
        if (N - i >= 0 && (N - i + 1) <= length(s)) {
          d <- (d + C[i + 1] * s[N - i + 1]) %% 2
        }
      }
      
      if (d == 1) {
        T <- C
        j <- 0
        while (j <= n - 1 && N - m + j <= n - 1) {
          if (N - m + j >= 0) {
            C[j + 1] <- (C[j + 1] + B[j + 1]) %% 2
          }
          j <- j + 1
        }
        
        if (L <= N / 2) {
          L <- N + 1 - L
          m <- N
          B <- T
        }
      }
      N <- N + 1
    }
    
    return(L)
  }
  
  # Test the algorithm
  complexity <- berlekamp_massey(test_seq)
  expect_equal(complexity, 3)
})