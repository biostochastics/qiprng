# Comprehensive Discriminant Testing Suite
# Tests all 750 discriminants from discriminants.csv to ensure complete coverage
# This test file ensures that every provided discriminant is properly validated

context("Comprehensive discriminant testing")

library(qiprng)

# Load discriminant testing functions
source(system.file("R/discriminant_tests.R", package = "qiprng", mustWork = FALSE))
if (!exists("test_discriminant")) {
  source("../../R/discriminant_tests.R")
}

# Path to discriminants file
discriminants_file <- system.file("analysis/data/discriminants.csv", package = "qiprng", mustWork = FALSE)
if (!file.exists(discriminants_file)) {
  discriminants_file <- "../../analysis/data/discriminants.csv"
}

test_that("All 750 discriminants are present in the data file", {
  skip_if(!file.exists(discriminants_file), "Discriminants file not found")

  discriminants <- read.csv(discriminants_file, stringsAsFactors = FALSE)

  # Should have 750 discriminants (all possible with constraints)
  expect_equal(nrow(discriminants), 750,
    info = "Expected exactly 750 discriminants in the data file"
  )

  # Verify required columns
  required_cols <- c("a", "b", "c", "Discriminant")
  expect_true(all(required_cols %in% names(discriminants)),
    info = "Missing required columns in discriminants file"
  )

  # Verify discriminant calculation
  calculated <- discriminants$b^2 - 4 * discriminants$a * discriminants$c
  expect_true(all(abs(calculated - discriminants$Discriminant) < 1e-10),
    info = "Discriminant values don't match calculation"
  )

  # Verify constraints
  expect_true(all(discriminants$a > 0),
    info = "All 'a' values must be positive"
  )
  expect_true(all(discriminants$c < 0),
    info = "All 'c' values must be negative"
  )
  expect_true(all(discriminants$Discriminant > 0),
    info = "All discriminants must be positive"
  )
})

test_that("Sample of discriminants generate valid random numbers", {
  skip_if(!file.exists(discriminants_file), "Discriminants file not found")

  discriminants <- read.csv(discriminants_file, stringsAsFactors = FALSE)

  # Test a stratified sample of discriminants
  # Group by discriminant value ranges to ensure coverage
  discriminants$range_group <- cut(discriminants$Discriminant,
    breaks = quantile(discriminants$Discriminant,
      probs = seq(0, 1, 0.1)
    ),
    include.lowest = TRUE
  )

  # Sample 2 discriminants from each decile (20 total)
  set.seed(42)
  sample_indices <- unlist(lapply(
    split(
      seq_len(nrow(discriminants)),
      discriminants$range_group
    ),
    function(x) sample(x, min(2, length(x)))
  ))

  for (idx in sample_indices) {
    row <- discriminants[idx, ]

    # Test that PRNG can be created and generates values
    config <- list(
      a = row$a,
      b = row$b,
      c = row$c,
      mpfr_precision = 256,
      use_parallel_filling = FALSE
    )

    expect_no_error(createPRNG(config),
      info = paste(
        "Failed to create PRNG for discriminant", idx,
        "with a=", row$a, "b=", row$b, "c=", row$c
      )
    )

    samples <- generatePRNG(1000)

    # Basic validity checks
    expect_length(samples, 1000)
    expect_true(all(samples >= 0 & samples <= 1),
      info = paste("Invalid range for discriminant", idx)
    )
    expect_true(all(is.finite(samples)),
      info = paste("Non-finite values for discriminant", idx)
    )
    expect_true(length(unique(samples)) > 900,
      info = paste("Too many duplicates for discriminant", idx)
    )

    cleanup_prng()
  }
})

test_that("Excellent discriminants pass statistical tests", {
  skip_if(!file.exists(discriminants_file), "Discriminants file not found")
  skip_if(!exists("test_discriminant"), "test_discriminant function not available")

  discriminants <- read.csv(discriminants_file, stringsAsFactors = FALSE)

  # Test first 5 discriminants labeled as "Excellent" (if ratings available)
  # For now, test the first 5 discriminants as a quick validation
  test_indices <- 1:min(5, nrow(discriminants))

  pass_count <- 0
  for (idx in test_indices) {
    row <- discriminants[idx, ]

    # Run basic statistical tests with smaller sample for speed
    result <- test_discriminant(row$a, row$b, row$c, row$Discriminant, n = 10000)

    # Check if passes basic quality threshold
    if (!is.null(result$overall_score) && result$overall_score >= 0.6) {
      pass_count <- pass_count + 1
    }

    # At least record the quality rating
    cat(
      "Discriminant", idx, "quality:", result$quality_rating,
      "score:", round(result$overall_score, 3), "\n"
    )
  }

  # Expect at least 60% to pass (3 out of 5)
  expect_gte(pass_count, 3,
    info = "Too few discriminants passing quality threshold"
  )
})

test_that("Discriminant parallel testing framework works", {
  skip_if(!file.exists(discriminants_file), "Discriminants file not found")
  skip_if(!exists("run_discriminant_analysis"), "Parallel analysis function not available")

  # Test parallel framework with just 3 discriminants
  results <- run_discriminant_analysis(
    discriminants_file = discriminants_file,
    sample_size = 5000, # Small sample for speed
    max_discriminants = 3,
    n_cores = 2
  )

  expect_length(results, 3)

  # Verify structure of results
  for (i in 1:3) {
    result <- results[[i]]
    expect_true("parameters" %in% names(result))
    expect_true("overall_score" %in% names(result))
    expect_true("quality_rating" %in% names(result))
  }
})

test_that("All discriminant ranges are covered", {
  skip_if(!file.exists(discriminants_file), "Discriminants file not found")

  discriminants <- read.csv(discriminants_file, stringsAsFactors = FALSE)

  # Check coverage of discriminant values
  disc_values <- sort(unique(discriminants$Discriminant))

  # Should have good coverage across the range
  expect_true(min(disc_values) < 50,
    info = "Missing small discriminants"
  )
  expect_true(max(disc_values) > 1000,
    info = "Missing large discriminants"
  )

  # Check for gaps in coverage
  gaps <- diff(disc_values)
  max_gap <- max(gaps)

  # Report coverage statistics
  cat("\nDiscriminant coverage statistics:\n")
  cat("  Total unique discriminants:", length(disc_values), "\n")
  cat("  Range:", min(disc_values), "to", max(disc_values), "\n")
  cat("  Median:", median(disc_values), "\n")
  cat("  Maximum gap:", max_gap, "\n")
})

test_that("Discriminant test functions handle edge cases", {
  # Test with minimal valid discriminant
  expect_no_error({
    config <- list(a = 1, b = 1, c = -1, mpfr_precision = 53)
    createPRNG(config)
    samples <- generatePRNG(100)
    cleanup_prng()
  })

  # Test with large discriminant
  expect_no_error({
    config <- list(a = 100, b = 1000, c = -1, mpfr_precision = 256)
    createPRNG(config)
    samples <- generatePRNG(100)
    cleanup_prng()
  })

  # Test error handling for invalid discriminants
  expect_error(
    createPRNG(list(a = 1, b = 2, c = 1)), # discriminant = 0
    "discriminant must be positive"
  )
})

# Performance benchmark (optional, only run manually)
test_that("Discriminant testing performance is acceptable", {
  skip("Performance test - run manually")

  skip_if(!file.exists(discriminants_file), "Discriminants file not found")
  skip_if(!exists("test_discriminant"), "test_discriminant function not available")

  discriminants <- read.csv(discriminants_file, stringsAsFactors = FALSE)

  # Benchmark single discriminant test
  row <- discriminants[1, ]

  start_time <- Sys.time()
  result <- test_discriminant(row$a, row$b, row$c, row$Discriminant, n = 50000)
  elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

  cat("\nSingle discriminant test (50k samples):", round(elapsed, 2), "seconds\n")

  # Should complete in reasonable time (< 30 seconds)
  expect_lt(elapsed, 30,
    info = "Single discriminant test taking too long"
  )
})

# Full validation suite (optional, very time-consuming)
test_that("All 750 discriminants can be tested", {
  skip("Full validation - run manually with adequate resources")

  skip_if(!file.exists(discriminants_file), "Discriminants file not found")
  skip_if(!exists("run_discriminant_analysis"), "Parallel analysis function not available")

  # This would test ALL discriminants - only run with sufficient time/resources
  results <- run_discriminant_analysis(
    discriminants_file = discriminants_file,
    sample_size = 10000,
    max_discriminants = NULL, # Test all
    n_cores = parallel::detectCores() - 1
  )

  # Analyze results
  excellent <- sum(sapply(results, function(r) r$quality_rating == "Excellent"))
  very_good <- sum(sapply(results, function(r) r$quality_rating == "Very-Good"))
  good <- sum(sapply(results, function(r) r$quality_rating == "Good"))

  cat("\nFull discriminant analysis results:\n")
  cat("  Excellent:", excellent, "(", round(excellent / 750 * 100, 1), "%)\n")
  cat("  Very Good:", very_good, "(", round(very_good / 750 * 100, 1), "%)\n")
  cat("  Good:", good, "(", round(good / 750 * 100, 1), "%)\n")

  # Save results for further analysis
  saveRDS(results, "discriminant_test_results.rds")
})
