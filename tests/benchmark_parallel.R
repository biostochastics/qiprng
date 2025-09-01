#!/usr/bin/env Rscript

# Performance Benchmarking for qiprng v0.5.0
# Tests OpenMP parallelization and work-stealing queue effectiveness

library(qiprng)
library(microbenchmark)
library(ggplot2)

cat("===============================================\n")
cat("qiprng v0.5.0 Parallel Performance Benchmarks\n")
cat("===============================================\n\n")

# Function to benchmark generation with different thread counts
benchmark_parallel <- function(n, thread_counts = c(1, 2, 4, 8)) {
  results <- list()

  for (threads in thread_counts) {
    # Set OpenMP threads
    Sys.setenv(OMP_NUM_THREADS = threads)

    # Initialize PRNG with parallel configuration
    cfg <- list(
      a = 5,
      b = 7,
      c = -3,
      mpfr_precision = 256,
      buffer_size = 10000,
      distribution = "uniform_01",
      use_parallel_filling = TRUE,
      use_threading = threads > 1
    )

    createPRNG(cfg)

    # Benchmark generation
    timing <- microbenchmark(
      generatePRNG(n),
      times = 10
    )

    results[[paste0("threads_", threads)]] <- list(
      threads = threads,
      mean_time = mean(timing$time) / 1e9, # Convert to seconds
      median_time = median(timing$time) / 1e9,
      sd_time = sd(timing$time) / 1e9,
      samples_per_sec = n / (mean(timing$time) / 1e9)
    )

    cleanupPRNG()
  }

  return(results)
}

# Test 1: Scaling with different sample sizes
cat("Test 1: OpenMP Scaling Analysis\n")
cat("--------------------------------\n")

sample_sizes <- c(10000, 100000, 1000000, 10000000)
thread_counts <- c(1, 2, 4, 8)

scaling_data <- data.frame()

for (n in sample_sizes) {
  cat(sprintf("Testing n = %d samples...\n", n))
  results <- benchmark_parallel(n, thread_counts)

  for (res in results) {
    scaling_data <- rbind(scaling_data, data.frame(
      samples = n,
      threads = res$threads,
      time = res$mean_time,
      speedup = results$threads_1$mean_time / res$mean_time,
      efficiency = (results$threads_1$mean_time / res$mean_time) / res$threads * 100,
      samples_per_sec = res$samples_per_sec
    ))
  }
}

# Print scaling results
cat("\nScaling Results:\n")
print(scaling_data)

# Calculate and display speedup summary
cat("\n\nSpeedup Summary (relative to single-threaded):\n")
cat("------------------------------------------------\n")
for (n in sample_sizes) {
  subset_data <- scaling_data[scaling_data$samples == n, ]
  cat(sprintf("n = %d:\n", n))
  for (i in 1:nrow(subset_data)) {
    cat(sprintf(
      "  %d threads: %.2fx speedup (%.1f%% efficiency)\n",
      subset_data$threads[i],
      subset_data$speedup[i],
      subset_data$efficiency[i]
    ))
  }
}

# Test 2: Work-Stealing Effectiveness (uneven workloads)
cat("\n\nTest 2: Work-Stealing Queue Effectiveness\n")
cat("------------------------------------------\n")

# Create uneven workload by generating different distributions
test_workload <- function(use_work_stealing = TRUE) {
  cfg <- list(
    a = 5,
    b = 7,
    c = -3,
    mpfr_precision = 256,
    buffer_size = 10000,
    use_parallel_filling = TRUE,
    use_threading = TRUE,
    mixing_strategy = ifelse(use_work_stealing, "CASCADE_MIX", "ROUND_ROBIN")
  )

  createPRNG(cfg)

  # Generate samples with varying complexity
  timings <- numeric(10)
  for (i in 1:10) {
    # Vary the generation size to create imbalanced load
    n <- ifelse(i %% 3 == 0, 100000, 10000)

    timing <- system.time({
      generatePRNG(n)
    })
    timings[i] <- timing["elapsed"]
  }

  cleanupPRNG()
  return(timings)
}

cat("Testing with work-stealing...\n")
ws_times <- test_workload(TRUE)
cat(sprintf("  Mean time: %.3f sec, SD: %.3f sec\n", mean(ws_times), sd(ws_times)))

cat("Testing without work-stealing...\n")
no_ws_times <- test_workload(FALSE)
cat(sprintf("  Mean time: %.3f sec, SD: %.3f sec\n", mean(no_ws_times), sd(no_ws_times)))

improvement <- (mean(no_ws_times) - mean(ws_times)) / mean(no_ws_times) * 100
cat(sprintf("\nWork-stealing improvement: %.1f%%\n", improvement))

# Test 3: Extended Distributions Performance
cat("\n\nTest 3: Extended Distributions Performance\n")
cat("-------------------------------------------\n")

test_distributions <- function(n = 100000) {
  # Initialize PRNG
  cfg <- list(
    a = 5,
    b = 7,
    c = -3,
    mpfr_precision = 256,
    buffer_size = 10000,
    distribution = "uniform_01"
  )
  createPRNG(cfg)

  dist_times <- list()

  # Test Levy stable
  timing <- microbenchmark(
    generate_levy_stable(n, alpha = 1.5, beta = 0.5),
    times = 5
  )
  dist_times$levy <- mean(timing$time) / 1e9

  # Test Pareto
  timing <- microbenchmark(
    generate_pareto(n, xm = 1, alpha = 2.5),
    times = 5
  )
  dist_times$pareto <- mean(timing$time) / 1e9

  # Test Cauchy
  timing <- microbenchmark(
    generate_cauchy(n, location = 0, scale = 1),
    times = 5
  )
  dist_times$cauchy <- mean(timing$time) / 1e9

  # Test Multivariate Normal (if Eigen available)
  tryCatch(
    {
      mean_vec <- c(0, 0, 0)
      cov_mat <- diag(3)
      timing <- microbenchmark(
        generate_multivariate_normal(n / 3, mean_vec, cov_mat),
        times = 5
      )
      dist_times$mvn <- mean(timing$time) / 1e9
    },
    error = function(e) {
      dist_times$mvn <- NA
    }
  )

  cleanupPRNG()
  return(dist_times)
}

dist_perf <- test_distributions(100000)

cat("Distribution generation times (100k samples):\n")
for (dist in names(dist_perf)) {
  if (!is.na(dist_perf[[dist]])) {
    rate <- 100000 / dist_perf[[dist]]
    cat(sprintf(
      "  %s: %.3f sec (%.0f samples/sec)\n",
      dist, dist_perf[[dist]], rate
    ))
  }
}

# Plot scaling results
if (require(ggplot2, quietly = TRUE)) {
  p1 <- ggplot(scaling_data, aes(x = threads, y = speedup, color = factor(samples))) +
    geom_line(size = 1.2) +
    geom_point(size = 3) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", alpha = 0.5) +
    scale_x_continuous(breaks = thread_counts) +
    labs(
      title = "qiprng v0.5.0 OpenMP Scaling",
      x = "Number of Threads",
      y = "Speedup",
      color = "Sample Size"
    ) +
    theme_minimal()

  p2 <- ggplot(scaling_data, aes(x = threads, y = efficiency, color = factor(samples))) +
    geom_line(size = 1.2) +
    geom_point(size = 3) +
    geom_hline(yintercept = 100, linetype = "dashed", alpha = 0.5) +
    scale_x_continuous(breaks = thread_counts) +
    scale_y_continuous(limits = c(0, 110)) +
    labs(
      title = "Parallel Efficiency",
      x = "Number of Threads",
      y = "Efficiency (%)",
      color = "Sample Size"
    ) +
    theme_minimal()

  # Save plots
  ggsave("benchmark_scaling.png", p1, width = 10, height = 6)
  ggsave("benchmark_efficiency.png", p2, width = 10, height = 6)
  cat("\nPlots saved as benchmark_scaling.png and benchmark_efficiency.png\n")
}

cat("\n===============================================\n")
cat("Benchmark Complete!\n")
cat("===============================================\n")
