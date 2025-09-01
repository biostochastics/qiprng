context("Threading tests")

test_that("Thread safety works", {
  # skip_on_cran()

  # Create PRNG with threading enabled
  createPRNG(list(use_threading = TRUE))

  # Use more cores to increase chance of detecting issues
  cores <- min(4, parallel::detectCores())
  cl <- parallel::makeCluster(cores)
  on.exit(parallel::stopCluster(cl))

  # Export the qiprng package to the cluster and initialize with unique seeds
  parallel::clusterEvalQ(cl, {
    library(qiprng)
    # Initialize PRNG in each thread with threading enabled and a unique seed
    # based on the process ID to ensure different sequences
    pid <- Sys.getpid()
    createPRNG(list(
      use_threading = TRUE,
      a = 2L + (pid %% 5), # Vary the parameters slightly based on PID
      b = 5L + (pid %% 7),
      c = -2L - (pid %% 3)
    ))
    NULL
  })

  # Generate random numbers in each worker
  results <- parallel::parLapply(cl, 1:cores, function(i) {
    # Generate a sequence of random numbers
    qiprng::generatePRNG(1000)
  })

  # Verify that each worker produced different random numbers
  for (i in 1:(cores - 1)) {
    for (j in (i + 1):cores) {
      expect_false(identical(results[[i]], results[[j]]))
    }
  }
})
