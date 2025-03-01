context("Threading tests")

test_that("Thread safety works", {
  skip_on_cran()
  
  createPRNG(list(use_threading = TRUE))
  
  cores <- min(2, parallel::detectCores())
  cl <- parallel::makeCluster(cores)
  on.exit(parallel::stopCluster(cl))
  
  results <- parallel::parLapply(cl, 1:cores, function(i) {
    qiprng::generatePRNG(1000)
  })
  
  expect_false(identical(results[[1]], results[[2]]))
}) 