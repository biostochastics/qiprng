# Comprehensive Test Suite for Compression Tests
# ----------------------------------------------------------------------
# This test suite validates the compression-based randomness tests by
# evaluating their ability to distinguish between truly random sequences
# and various non-random patterns including cryptographic RNGs, defective
# generators, and edge cases.

# Source required files
source_test_helpers <- function() {
  # Try to source bootstrap framework
  if (!exists("bootstrap_p_value")) {
    if (file.exists("../../R/bootstrap_framework.R")) {
      source("../../R/bootstrap_framework.R")
    }
  }
  
  # Try to source compression tests
  if (!exists("run_compression_tests_bootstrap")) {
    if (file.exists("../../R/statisticaltests/compression_tests_bootstrap.R")) {
      source("../../R/statisticaltests/compression_tests_bootstrap.R")
    }
  }
}

# ----------------------------------------------------------------------
# Test Data Generators
# ----------------------------------------------------------------------

#' Cryptographic hash-based RNG (simulating AES-CTR)
#' Uses digest package to create cryptographically secure sequences
crypto_rng_aes <- function(n, seed = 12345) {
  if (!requireNamespace("digest", quietly = TRUE)) {
    skip("digest package required for cryptographic RNG tests")
  }
  
  # Initialize counter and key
  key <- as.character(seed)
  counter <- 0
  values <- numeric(n)
  
  # Generate values using hash-based approach
  bytes_per_hash <- 32  # SHA256 produces 32 bytes
  values_per_hash <- bytes_per_hash / 4  # 4 bytes per float
  
  i <- 1
  while (i <= n) {
    # Create input for hash
    input <- paste0(key, ":", counter)
    hash <- digest::digest(input, algo = "sha256", serialize = FALSE, raw = TRUE)
    
    # Convert hash bytes to floats in [0,1)
    for (j in seq_len(min(values_per_hash, n - i + 1))) {
      # Extract 4 bytes and convert to unsigned integer
      bytes <- hash[((j-1)*4 + 1):(j*4)]
      uint <- sum(as.integer(bytes) * 256^(0:3))
      # Convert to [0,1)
      values[i] <- uint / (2^32)
      i <- i + 1
      if (i > n) break
    }
    counter <- counter + 1
  }
  
  return(values)
}

#' Cryptographic RNG simulating ChaCha20
#' Uses a different hash algorithm for variety
crypto_rng_chacha <- function(n, seed = 54321) {
  if (!requireNamespace("digest", quietly = TRUE)) {
    skip("digest package required for cryptographic RNG tests")
  }
  
  # Use SHA512 for ChaCha20 simulation
  key <- as.character(seed)
  counter <- 0
  values <- numeric(n)
  
  bytes_per_hash <- 64  # SHA512 produces 64 bytes
  values_per_hash <- bytes_per_hash / 4
  
  i <- 1
  while (i <= n) {
    input <- paste0("chacha20:", key, ":", counter)
    hash <- digest::digest(input, algo = "sha512", serialize = FALSE, raw = TRUE)
    
    for (j in seq_len(min(values_per_hash, n - i + 1))) {
      bytes <- hash[((j-1)*4 + 1):(j*4)]
      uint <- sum(as.integer(bytes) * 256^(0:3))
      values[i] <- uint / (2^32)
      i <- i + 1
      if (i > n) break
    }
    counter <- counter + 1
  }
  
  return(values)
}

#' Defective Linear Congruential Generator (LCG)
#' RANDU - infamous bad LCG with poor randomness properties
defective_lcg_randu <- function(n, seed = 1) {
  # RANDU parameters: X_{n+1} = (65539 * X_n) mod 2^31
  a <- 65539
  m <- 2^31
  
  x <- seed
  values <- numeric(n)
  
  for (i in seq_len(n)) {
    x <- (a * x) %% m
    values[i] <- x / m  # Normalize to [0,1)
  }
  
  return(values)
}

#' Another defective LCG with poor parameters
#' Small multiplier leads to obvious patterns
defective_lcg_small <- function(n, seed = 1) {
  # Poor parameters that create obvious patterns
  a <- 11  # Small multiplier
  c <- 0   # No increment
  m <- 256 # Small modulus
  
  x <- seed
  values <- numeric(n)
  
  for (i in seq_len(n)) {
    x <- (a * x + c) %% m
    values[i] <- x / m
  }
  
  return(values)
}

#' Edge case: All zeros
edge_case_zeros <- function(n) {
  rep(0, n)
}

#' Edge case: All ones
edge_case_ones <- function(n) {
  rep(0.999999, n)  # Just below 1 to stay in [0,1)
}

#' Edge case: Alternating pattern
edge_case_alternating <- function(n) {
  rep(c(0, 0.999999), length.out = n)
}

#' Edge case: Sequential pattern
edge_case_sequential <- function(n) {
  seq(0, 0.999999, length.out = n)
}

#' Edge case: Repeating small block
edge_case_block_repeat <- function(n, block_size = 10) {
  block <- runif(block_size)
  rep(block, length.out = n)
}

#' NIST-like reference sequence
#' Generates a sequence with known statistical properties
nist_reference_sequence <- function(n, type = "e") {
  if (type == "e") {
    # Use digits of e as a reference sequence
    # First, generate enough digits
    e_str <- "2.71828182845904523536028747135266249775724709369995957496696762772407663035354759457138217852516642742746639193200305992181741359662904357290033429526059563073813232862794349076323382988075319525101901157383418793070215408914993488416750924476146066808226480016847741185374234544243710753907774499206955170276183860626133138458300075204493382656029760673711320070932870912744374704723069697720931014169283681902551510865746377211125238978442505695369677078544996996794686445490598793163688923009879312773617821542499922957635148220826989519366803318252886939849646510582093923982948879332036250944311730123819706841614039701983767932068328237646480429531180232878250981945581530175671736133206981125099618188159304169035159888851934580727386673858942287922849506900258878053320838142061717766914730359825349042875546873115956286388235378759375195778185778053217122680661300192787661119590921642019"
    
    # Convert to numeric values in [0,1)
    digits <- as.numeric(strsplit(gsub("[^0-9]", "", e_str), "")[[1]])
    
    # Create values by taking pairs of digits
    values <- numeric(n)
    for (i in seq_len(n)) {
      idx <- ((i-1) * 2) %% (length(digits) - 1) + 1
      values[i] <- (digits[idx] * 10 + digits[idx + 1]) / 100
    }
    return(values)
  } else if (type == "pi") {
    # Use digits of pi
    pi_str <- "3.14159265358979323846264338327950288419716939937510582097494459230781640628620899862803482534211706798214808651328230664709384460955058223172535940812848111745028410270193852110555964462294895493038196442881097566593344612847564823378678316527120190914564856692346034861045432664821339360726024914127372458700660631558817488152092096282925409171536436789259036001133053054882046652138414695194151160943305727036575959195309218611738193261179310511854807446237996274956735188575272489122793818301194912983367336244065664308602139494639522473719070217986094370277053921717629317675238467481846766940513200056812714526356082778577134275778960917363717872146844090122495343014654958537105079227968925892354201995611212902196086403441815981362977477130996051870721134999999"
    
    digits <- as.numeric(strsplit(gsub("[^0-9]", "", pi_str), "")[[1]])
    values <- numeric(n)
    for (i in seq_len(n)) {
      idx <- ((i-1) * 2) %% (length(digits) - 1) + 1
      values[i] <- (digits[idx] * 10 + digits[idx + 1]) / 100
    }
    return(values)
  }
}

# ----------------------------------------------------------------------
# Helper function to create test suite
# ----------------------------------------------------------------------
create_test_suite <- function(n = 10000, prng_func = NULL, 
                              algorithms = c("gzip", "bzip2", "rle")) {
  if (is.null(prng_func)) {
    prng_func <- function(n) runif(n)
  }
  
  list(
    config = list(
      compression_sample_size = n,
      significance_level = 0.05,
      bootstrap_samples = 1000,  # Reduced for faster tests
      show_progress = FALSE,
      save_visualizations = FALSE,
      compression_algorithms = algorithms,
      use_bootstrap_compression = TRUE
    ),
    prng_func = prng_func,
    results = list()
  )
}

# ----------------------------------------------------------------------
# Main Test Cases
# ----------------------------------------------------------------------

test_that("compression tests correctly identify truly random sequences", {
  skip_if_not(exists("bootstrap_p_value"), "Bootstrap framework not available")
  source_test_helpers()
  
  set.seed(42)
  
  # Test with R's built-in RNG (should be random)
  suite <- create_test_suite(n = 5000)
  suite_result <- run_compression_tests_bootstrap(suite)
  
  # Check that all main tests pass
  compression_results <- suite_result$results$compression
  
  # Compression ratio tests should pass
  for (algo in c("gzip", "bzip2", "rle")) {
    test_name <- paste0("compression_ratio_", algo)
    if (test_name %in% names(compression_results)) {
      result <- compression_results[[test_name]]
      expect_true(result$p_value > 0.01, 
                  info = paste(algo, "compression test should pass for random data"))
    }
  }
  
  # Entropy test should pass
  if ("entropy_bootstrap" %in% names(compression_results)) {
    expect_true(compression_results$entropy_bootstrap$p_value > 0.01,
                info = "Entropy test should pass for random data")
  }
  
  # Byte frequency test should pass
  if ("byte_frequency_monte_carlo" %in% names(compression_results)) {
    expect_true(compression_results$byte_frequency_monte_carlo$p_value > 0.01,
                info = "Byte frequency test should pass for random data")
  }
})

test_that("compression tests detect cryptographic RNGs as random", {
  skip_if_not(exists("bootstrap_p_value"), "Bootstrap framework not available")
  skip_if_not(requireNamespace("digest", quietly = TRUE), "digest package required")
  source_test_helpers()
  
  # Test AES-like crypto RNG
  suite_aes <- create_test_suite(n = 5000, prng_func = crypto_rng_aes)
  result_aes <- run_compression_tests_bootstrap(suite_aes)
  
  # Crypto RNG should pass randomness tests
  if ("compression_ratio_gzip" %in% names(result_aes$results$compression)) {
    expect_true(result_aes$results$compression$compression_ratio_gzip$p_value > 0.01,
                info = "AES-like RNG should pass compression test")
  }
  
  # Test ChaCha-like crypto RNG
  suite_chacha <- create_test_suite(n = 5000, prng_func = crypto_rng_chacha)
  result_chacha <- run_compression_tests_bootstrap(suite_chacha)
  
  if ("compression_ratio_gzip" %in% names(result_chacha$results$compression)) {
    expect_true(result_chacha$results$compression$compression_ratio_gzip$p_value > 0.01,
                info = "ChaCha-like RNG should pass compression test")
  }
})

test_that("compression tests detect defective LCGs as non-random", {
  skip_if_not(exists("bootstrap_p_value"), "Bootstrap framework not available")
  source_test_helpers()
  
  # Test RANDU
  suite_randu <- create_test_suite(n = 5000, prng_func = defective_lcg_randu)
  result_randu <- run_compression_tests_bootstrap(suite_randu)
  
  # RANDU should fail at least one test
  compression_results <- result_randu$results$compression
  any_test_failed <- FALSE
  
  for (test_name in names(compression_results)) {
    if ("p_value" %in% names(compression_results[[test_name]])) {
      if (compression_results[[test_name]]$p_value < 0.05) {
        any_test_failed <- TRUE
        break
      }
    }
  }
  
  expect_true(any_test_failed, 
              info = "RANDU should fail at least one compression test")
  
  # Test small LCG
  suite_small <- create_test_suite(n = 5000, prng_func = defective_lcg_small)
  result_small <- run_compression_tests_bootstrap(suite_small)
  
  # Small LCG should definitely fail compression tests
  if ("compression_ratio_gzip" %in% names(result_small$results$compression)) {
    expect_true(result_small$results$compression$compression_ratio_gzip$p_value < 0.05,
                info = "Small LCG should fail compression test")
  }
})

test_that("compression tests detect edge cases as non-random", {
  skip_if_not(exists("bootstrap_p_value"), "Bootstrap framework not available")
  source_test_helpers()
  
  # Test all zeros
  suite_zeros <- create_test_suite(n = 5000, prng_func = edge_case_zeros)
  result_zeros <- run_compression_tests_bootstrap(suite_zeros)
  
  # All zeros should compress extremely well
  if ("compression_ratio_gzip" %in% names(result_zeros$results$compression)) {
    expect_true(result_zeros$results$compression$compression_ratio_gzip$observed_ratio < 0.1,
                info = "All zeros should compress to very small size")
    expect_true(result_zeros$results$compression$compression_ratio_gzip$p_value < 0.01,
                info = "All zeros should fail compression test")
  }
  
  # Test alternating pattern
  suite_alt <- create_test_suite(n = 5000, prng_func = edge_case_alternating)
  result_alt <- run_compression_tests_bootstrap(suite_alt)
  
  # Alternating pattern should compress well
  if ("compression_ratio_gzip" %in% names(result_alt$results$compression)) {
    expect_true(result_alt$results$compression$compression_ratio_gzip$p_value < 0.05,
                info = "Alternating pattern should fail compression test")
  }
  
  # Test block repeat
  suite_block <- create_test_suite(n = 5000, 
                                   prng_func = function(n) edge_case_block_repeat(n, 20))
  result_block <- run_compression_tests_bootstrap(suite_block)
  
  # Repeating blocks should compress well
  if ("compression_ratio_gzip" %in% names(result_block$results$compression)) {
    expect_true(result_block$results$compression$compression_ratio_gzip$p_value < 0.05,
                info = "Repeating blocks should fail compression test")
  }
})

test_that("NIST reference sequences behave as expected", {
  skip_if_not(exists("bootstrap_p_value"), "Bootstrap framework not available")
  source_test_helpers()
  
  # Test e-based sequence
  suite_e <- create_test_suite(n = 5000, 
                               prng_func = function(n) nist_reference_sequence(n, "e"))
  result_e <- run_compression_tests_bootstrap(suite_e)
  
  # Mathematical constants should appear random-ish but may have subtle patterns
  # We expect them to mostly pass but possibly fail some tests
  e_results <- result_e$results$compression
  
  # Count how many tests pass
  pass_count <- 0
  total_count <- 0
  for (test_name in names(e_results)) {
    if ("p_value" %in% names(e_results[[test_name]])) {
      total_count <- total_count + 1
      if (e_results[[test_name]]$p_value > 0.05) {
        pass_count <- pass_count + 1
      }
    }
  }
  
  # Should pass most tests
  expect_true(pass_count / total_count > 0.5,
              info = "e-based sequence should pass majority of tests")
})

# ----------------------------------------------------------------------
# Kolmogorov-Smirnov Test for P-value Distribution
# ----------------------------------------------------------------------

test_that("p-value distribution follows uniform distribution (KS test)", {
  skip_if_not(exists("bootstrap_p_value"), "Bootstrap framework not available")
  source_test_helpers()
  
  set.seed(123)
  n_iterations <- 100  # Number of test iterations
  p_values <- numeric(n_iterations)
  
  # Run compression test multiple times and collect p-values
  for (i in seq_len(n_iterations)) {
    # Generate new random data each time
    suite <- create_test_suite(n = 2000)  # Smaller n for speed
    suite$config$bootstrap_samples <- 500  # Fewer bootstraps for speed
    
    result <- run_compression_tests_bootstrap(suite)
    
    # Collect p-value from gzip compression test
    if ("compression_ratio_gzip" %in% names(result$results$compression)) {
      p_values[i] <- result$results$compression$compression_ratio_gzip$p_value
    } else if ("compression_ratio_rle" %in% names(result$results$compression)) {
      p_values[i] <- result$results$compression$compression_ratio_rle$p_value
    }
  }
  
  # Remove any NA values
  p_values <- p_values[!is.na(p_values)]
  
  # Kolmogorov-Smirnov test for uniformity
  ks_result <- ks.test(p_values, punif)
  
  # P-values should be uniformly distributed
  expect_true(ks_result$p.value > 0.01,
              info = paste("KS test p-value:", round(ks_result$p.value, 4),
                           "- P-values should be uniformly distributed"))
  
  # Additional check: mean should be close to 0.5
  expect_true(abs(mean(p_values) - 0.5) < 0.1,
              info = paste("Mean p-value:", round(mean(p_values), 3),
                           "should be close to 0.5"))
})

# ----------------------------------------------------------------------
# Performance Benchmarks
# ----------------------------------------------------------------------

test_that("compression tests complete within reasonable time", {
  skip_if_not(exists("bootstrap_p_value"), "Bootstrap framework not available")
  skip_if_not(requireNamespace("microbenchmark", quietly = TRUE), 
              "microbenchmark package required for performance tests")
  source_test_helpers()
  
  # Benchmark different sequence lengths
  sequence_lengths <- c(1000, 5000, 10000)
  
  for (n in sequence_lengths) {
    suite <- create_test_suite(n = n)
    suite$config$bootstrap_samples <- 500
    
    # Time the compression test
    time_taken <- system.time({
      result <- run_compression_tests_bootstrap(suite)
    })["elapsed"]
    
    # Set reasonable time limits based on sequence length
    max_time <- n / 1000 * 5  # 5 seconds per 1000 values
    
    expect_true(time_taken < max_time,
                info = paste("Compression test for n =", n, 
                             "took", round(time_taken, 2), "seconds"))
  }
  
  # Detailed benchmark for standard test size
  if (requireNamespace("microbenchmark", quietly = TRUE)) {
    suite <- create_test_suite(n = 2000)
    suite$config$bootstrap_samples <- 100  # Very few for benchmarking
    
    benchmark_result <- microbenchmark::microbenchmark(
      compression_test = run_compression_tests_bootstrap(suite),
      times = 5
    )
    
    # Print benchmark summary
    print(summary(benchmark_result))
  }
})

# ----------------------------------------------------------------------
# Test Coverage Validation
# ----------------------------------------------------------------------

test_that("all compression test components are exercised", {
  skip_if_not(exists("bootstrap_p_value"), "Bootstrap framework not available")
  source_test_helpers()
  
  suite <- create_test_suite(n = 5000)
  result <- run_compression_tests_bootstrap(suite)
  
  # Check that all expected test components are present
  expected_tests <- c(
    "compression_ratio_gzip",
    "compression_ratio_bzip2", 
    "compression_ratio_rle",
    "compression_ratio_combined",
    "entropy_bootstrap",
    "byte_frequency_monte_carlo",
    "maurers_universal"
  )
  
  compression_results <- result$results$compression
  
  # Count how many expected tests are present
  present_tests <- intersect(expected_tests, names(compression_results))
  coverage <- length(present_tests) / length(expected_tests)
  
  # Should have at least 80% of expected tests
  # (Some may be skipped due to missing dependencies)
  expect_true(coverage > 0.8,
              info = paste("Test coverage:", round(coverage * 100, 1), "%"))
  
  # Verify bootstrap distributions are stored
  expect_true("bootstrap_distributions" %in% names(result),
              info = "Bootstrap distributions should be stored")
  
  # Verify each test has required fields
  for (test_name in names(compression_results)) {
    test_result <- compression_results[[test_name]]
    expect_true("p_value" %in% names(test_result),
                info = paste(test_name, "should have p_value"))
    expect_true("result" %in% names(test_result),
                info = paste(test_name, "should have result (PASS/FAIL)"))
    expect_true("description" %in% names(test_result),
                info = paste(test_name, "should have description"))
  }
})

# ----------------------------------------------------------------------
# False Positive/Negative Rate Analysis
# ----------------------------------------------------------------------

test_that("false positive rate is controlled at significance level", {
  skip_if_not(exists("bootstrap_p_value"), "Bootstrap framework not available")
  source_test_helpers()
  
  set.seed(456)
  n_iterations <- 50  # Number of test iterations
  false_positives <- 0
  significance_level <- 0.05
  
  for (i in seq_len(n_iterations)) {
    # Test with truly random data
    suite <- create_test_suite(n = 2000)
    suite$config$bootstrap_samples <- 500
    suite$config$significance_level <- significance_level
    
    result <- run_compression_tests_bootstrap(suite)
    
    # Check if any test incorrectly rejected randomness
    if ("compression_ratio_gzip" %in% names(result$results$compression)) {
      if (result$results$compression$compression_ratio_gzip$result == "FAIL") {
        false_positives <- false_positives + 1
      }
    }
  }
  
  false_positive_rate <- false_positives / n_iterations
  
  # False positive rate should be close to significance level
  # Allow some margin due to finite sample size
  expect_true(false_positive_rate < significance_level * 2,
              info = paste("False positive rate:", round(false_positive_rate, 3),
                           "should be close to", significance_level))
})

test_that("compression tests have high power against non-random sequences", {
  skip_if_not(exists("bootstrap_p_value"), "Bootstrap framework not available")
  source_test_helpers()
  
  set.seed(789)
  n_iterations <- 20
  true_positives <- 0
  
  for (i in seq_len(n_iterations)) {
    # Test with obviously non-random data (alternating pattern)
    suite <- create_test_suite(n = 2000, prng_func = edge_case_alternating)
    suite$config$bootstrap_samples <- 500
    
    result <- run_compression_tests_bootstrap(suite)
    
    # Check if test correctly detected non-randomness
    if ("compression_ratio_gzip" %in% names(result$results$compression)) {
      if (result$results$compression$compression_ratio_gzip$result == "FAIL") {
        true_positives <- true_positives + 1
      }
    }
  }
  
  power <- true_positives / n_iterations
  
  # Power should be very high for obvious patterns
  expect_true(power > 0.95,
              info = paste("Test power:", round(power, 3),
                           "should be very high for obvious patterns"))
})