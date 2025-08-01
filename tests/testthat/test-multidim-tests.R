# test-multidim-tests.R
# Unit tests for multidimensional statistical tests

context("Multidimensional Statistical Tests")

# Load required functions
source("../../R/statisticaltests/multidim_tests.R")
source("../../R/statisticaltests/multidim_specific_tests.R")
source("../../R/statisticaltests/multidim_tests_fixes.R")
source("../../R/statisticaltests/multidim_wrapper.R")
source("../../R/statisticaltests/multidim_reftools_compat.R")

# Helper function to create test suite
create_test_suite <- function(prng_func = runif) {
  list(
    prng_func = prng_func,
    config = list(
      multidim_sample_size = 1000,
      significance_level = 0.05,
      visualize = FALSE
    ),
    results = list()
  )
}

# Test multidim_config creation
test_that("multidim_config creates proper configuration object", {
  config <- multidim_config(dimensions = 3, sample_size = 500)
  
  expect_s3_class(config, "multidim_config")
  expect_equal(config$dimensions, 3)
  expect_equal(config$sample_size, 500)
  expect_equal(config$distance_metric, "euclidean")
  expect_true(is.numeric(config$grid_resolution))
  expect_true(config$grid_resolution > 0)
})

# Test point generation
test_that("generate_multidim_points generates correct dimensions", {
  # Test 2D
  points_2d <- generate_multidim_points(100, 2, runif)
  expect_equal(dim(points_2d), c(100, 2))
  expect_true(all(points_2d >= 0 & points_2d <= 1))
  
  # Test 3D
  points_3d <- generate_multidim_points(50, 3, runif)
  expect_equal(dim(points_3d), c(50, 3))
  expect_true(all(points_3d >= 0 & points_3d <= 1))
  
  # Test higher dimensions
  points_5d <- generate_multidim_points(30, 5, runif)
  expect_equal(dim(points_5d), c(30, 5))
})

# Test distance calculations
test_that("calculate_distances computes correct distances", {
  # Simple 2D test case
  points <- matrix(c(0, 0, 1, 0, 0, 1, 1, 1), ncol = 2, byrow = TRUE)
  
  # Test Euclidean distance
  result <- calculate_distances(points, metric = "euclidean", 
                               use_kdtree = FALSE, k_nearest = 1)
  
  expect_type(result, "list")
  expect_equal(dim(result$distances), c(4, 1))
  expect_equal(dim(result$indices), c(4, 1))
  
  # Check specific distances
  # Point 1 (0,0) nearest to point 2 (1,0) or point 3 (0,1) - distance 1
  expect_equal(result$distances[1, 1], 1)
  # Point 4 (1,1) nearest to point 2 (1,0) or point 3 (0,1) - distance 1
  expect_equal(result$distances[4, 1], 1)
  
  # Test Manhattan distance
  result_man <- calculate_distances(points, metric = "manhattan", 
                                   k_nearest = 1)
  expect_equal(result_man$distances[1, 1], 1)
  
  # Test k>1 nearest neighbors
  result_k3 <- calculate_distances(points, k_nearest = 3)
  expect_equal(dim(result_k3$distances), c(4, 3))
})

# Test grid uniformity test
test_that("grid_uniformity_test performs chi-square test correctly", {
  # Generate uniform points
  set.seed(123)
  points <- generate_multidim_points(1000, 2, runif)
  
  result <- grid_uniformity_test(points, grid_resolution = 5)
  
  expect_type(result, "list")
  expect_true("statistic" %in% names(result))
  expect_true("p_value" %in% names(result))
  expect_true("result" %in% names(result))
  expect_true(result$p_value >= 0 && result$p_value <= 1)
  expect_equal(result$total_cells, 25)  # 5^2
  expect_true(result$result %in% c("PASS", "FAIL"))
  
  # Test with different grid resolution
  result2 <- grid_uniformity_test(points, grid_resolution = 10)
  expect_equal(result2$total_cells, 100)  # 10^2
})

# Test nearest neighbor test
test_that("nearest_neighbor_test analyzes NN distances correctly", {
  set.seed(123)
  points <- generate_multidim_points(500, 2, runif)
  
  result <- nearest_neighbor_test(points)
  
  expect_type(result, "list")
  expect_true("statistic" %in% names(result))
  expect_true("p_value" %in% names(result))
  expect_true("observed_mean" %in% names(result))
  expect_true("expected_mean" %in% names(result))
  expect_true(result$observed_mean > 0)
  expect_true(result$expected_mean > 0)
  
  # Test with 3D points
  points_3d <- generate_multidim_points(500, 3, runif)
  result_3d <- nearest_neighbor_test(points_3d)
  expect_true(result_3d$observed_mean > 0)
})

# Test Ripley's K function
test_that("ripleys_k_test computes K function correctly", {
  set.seed(123)
  points <- generate_multidim_points(200, 2, runif)
  
  result <- ripleys_k_test(points, radii = seq(0.05, 0.2, 0.05))
  
  expect_type(result, "list")
  expect_true("observed_k" %in% names(result))
  expect_true("expected_k" %in% names(result))
  expect_true("radii" %in% names(result))
  expect_equal(length(result$radii), 4)
  expect_equal(length(result$observed_k), 4)
  expect_equal(length(result$expected_k), 4)
  
  # K values should increase with radius
  expect_true(all(diff(result$observed_k) >= 0))
})

# Test 2D uniformity test
test_that("uniformity_test_2d performs 2D specific tests", {
  set.seed(123)
  points <- generate_multidim_points(400, 2, runif)
  
  result <- uniformity_test_2d(points, grid_size = 8)
  
  expect_type(result, "list")
  expect_equal(result$grid_size, 8)
  expect_true("empty_ratio" %in% names(result))
  expect_true("cv" %in% names(result))
  expect_true(result$empty_ratio >= 0 && result$empty_ratio <= 1)
  
  # Test error for non-2D input
  points_3d <- generate_multidim_points(100, 3, runif)
  expect_error(uniformity_test_2d(points_3d), "exactly 2 dimensions")
})

# Test 3D scatter test
test_that("scatter_test_3d analyzes 3D patterns correctly", {
  set.seed(123)
  points <- generate_multidim_points(300, 3, runif)
  
  # Test triplet method
  result <- scatter_test_3d(points, method = "triplet")
  
  expect_type(result, "list")
  expect_true("observed_mean" %in% names(result))
  expect_true("p_value" %in% names(result))
  expect_true(result$observed_mean > 0)
  
  # Test octant method
  result_oct <- scatter_test_3d(points, method = "octant")
  
  expect_type(result_oct, "list")
  expect_equal(length(result_oct$octant_counts), 8)
  expect_equal(sum(result_oct$octant_counts), nrow(points))
  
  # Test error for non-3D input
  points_2d <- generate_multidim_points(100, 2, runif)
  expect_error(scatter_test_3d(points_2d), "exactly 3 dimensions")
})

# Test minimum distance test
test_that("minimum_distance_test analyzes minimum distances", {
  set.seed(123)
  points <- generate_multidim_points(200, 2, runif)
  
  # Test with Poisson theoretical
  result <- minimum_distance_test(points, theoretical_dist = "poisson")
  
  expect_type(result, "list")
  expect_true("statistic" %in% names(result))
  expect_true("p_value" %in% names(result))
  expect_true("mean_distance" %in% names(result))
  expect_true(result$mean_distance > 0)
  
  # Test with empirical bootstrap
  result_emp <- minimum_distance_test(points, theoretical_dist = "empirical")
  
  expect_type(result_emp, "list")
  expect_true("bootstrap_mean" %in% names(result_emp))
  expect_true("bootstrap_sd" %in% names(result_emp))
})

# Test convex hull volume test
test_that("convex_hull_volume_test computes hull correctly", {
  skip_if_not_installed("geometry")
  
  set.seed(123)
  points <- generate_multidim_points(100, 2, runif)
  
  result <- convex_hull_volume_test(points, n_simulations = 100)
  
  expect_type(result, "list")
  expect_true("observed_volume" %in% names(result))
  expect_true("expected_volume" %in% names(result))
  expect_true("n_vertices" %in% names(result))
  expect_true(result$observed_volume > 0 && result$observed_volume <= 1)
  expect_true(result$n_vertices >= 3)  # Minimum for 2D hull
})

# Test serial correlation test
test_that("serial_test_multidim tests serial correlation", {
  set.seed(123)
  points <- generate_multidim_points(200, 3, runif)
  
  result <- serial_test_multidim(points, lag = 1, max_dim = 2)
  
  expect_type(result, "list")
  expect_true("dim1" %in% names(result))
  expect_true("dim2" %in% names(result))
  expect_true("combined" %in% names(result))
  
  # Check individual dimension results
  expect_true("correlation" %in% names(result$dim1))
  expect_true("p_value" %in% names(result$dim1))
  expect_true(abs(result$dim1$correlation) <= 1)
})

# Test comprehensive test suite
test_that("run_multidim_tests executes full test suite", {
  set.seed(123)
  suite <- create_test_suite()
  config <- multidim_config(dimensions = 2, sample_size = 300)
  
  suite <- run_multidim_tests(suite, config)
  
  expect_type(suite$results$multidimensional, "list")
  expect_true("grid_uniformity" %in% names(suite$results$multidimensional))
  expect_true("nearest_neighbor" %in% names(suite$results$multidimensional))
  expect_true("ripleys_k" %in% names(suite$results$multidimensional))
  
  # Check that points were stored
  expect_equal(dim(suite$multidim_data), c(300, 2))
})

# Test specific multidim tests
test_that("run_specific_multidim_tests adds specific tests", {
  set.seed(123)
  suite <- create_test_suite()
  
  # Run 2D specific tests
  suite <- run_specific_multidim_tests(suite, dimensions = 2)
  
  expect_true("multidim_specific" %in% names(suite$results))
  expect_true("uniformity_2d" %in% names(suite$results$multidim_specific))
  expect_true("minimum_distance" %in% names(suite$results$multidim_specific))
  
  # Run 3D specific tests
  suite <- create_test_suite()
  suite <- run_specific_multidim_tests(suite, dimensions = 3)
  
  expect_true("scatter_3d" %in% names(suite$results$multidim_specific))
})

# Test wrapper functions
test_that("run_multidimensional_tests wrapper works correctly", {
  set.seed(123)
  suite <- create_test_suite()
  
  suite <- run_multidimensional_tests(suite, dimensions = 2, 
                                     apply_fixes = FALSE,
                                     run_specific = TRUE)
  
  expect_true("multidimensional" %in% names(suite$results))
  expect_true("multidim_specific" %in% names(suite$results))
})

# Test bootstrap functionality
test_that("run_multidim_tests_bootstrap performs bootstrap correctly", {
  set.seed(123)
  suite <- create_test_suite()
  
  suite <- run_multidim_tests_bootstrap(suite, dimensions = 2, 
                                       n_bootstrap = 50,
                                       tests = c("grid_uniformity", 
                                               "nearest_neighbor"))
  
  expect_true("multidim_bootstrap" %in% names(suite))
  expect_true("grid_uniformity" %in% names(suite$multidim_bootstrap))
  
  boot_result <- suite$multidim_bootstrap$grid_uniformity
  expect_true("bootstrap_mean" %in% names(boot_result))
  expect_true("ci_95" %in% names(boot_result))
  expect_equal(length(boot_result$ci_95), 2)
})

# Test comparison functionality
test_that("compare_multidim_results compares generators correctly", {
  # Create mock results for two generators
  results_list <- list(
    gen1 = list(results = list(multidimensional = list(
      test1 = list(result = "PASS", p_value = 0.1),
      test2 = list(result = "FAIL", p_value = 0.01)
    ))),
    gen2 = list(results = list(multidimensional = list(
      test1 = list(result = "PASS", p_value = 0.2),
      test2 = list(result = "PASS", p_value = 0.06)
    )))
  )
  
  comparison <- compare_multidim_results(results_list)
  
  expect_type(comparison, "list")
  expect_true("comparison" %in% names(comparison))
  expect_true("pass_rates" %in% names(comparison))
  expect_true("summary" %in% names(comparison))
  
  expect_equal(dim(comparison$comparison), c(2, 2))
  expect_equal(comparison$pass_rates["test1"], c(test1 = 1.0))
})

# Test quick check function
test_that("quick_multidim_check provides quick assessment", {
  set.seed(123)
  
  # Capture output
  result <- capture.output(
    invisible(quick_multidim_check(runif, n = 100, dimensions = 2))
  )
  
  expect_true(any(grepl("Quick Multidimensional Check Results", result)))
  expect_true(any(grepl("Grid uniformity:", result)))
  expect_true(any(grepl("Nearest neighbor:", result)))
})

# Test fixes application
test_that("multidim test fixes can be applied and restored", {
  # Apply fixes
  apply_multidim_test_fixes()
  
  # Check that functions exist
  expect_true(exists("ripleys_k_test"))
  expect_true(exists("scatter_test_3d"))
  
  # Restore originals
  restore_original_multidim_tests()
  
  # Functions should still exist
  expect_true(exists("ripleys_k_test"))
})

# Test RefTools compatibility layer
test_that("RefTools compatibility functions work without RefTools", {
  # Test enhanced distance calculation fallback
  set.seed(123)
  points <- generate_multidim_points(50, 2, runif)
  
  result <- calculate_distances_enhanced(points, use_reftools = FALSE)
  
  expect_type(result, "list")
  expect_true(result$method %in% c("base", "kdtree"))
  
  # Test enhanced grid uniformity fallback
  result_grid <- grid_uniformity_test_enhanced(points, use_reftools = FALSE)
  
  expect_type(result_grid, "list")
  expect_true("statistic" %in% names(result_grid))
})

# Test edge cases
test_that("functions handle edge cases gracefully", {
  # Very small sample
  points_small <- generate_multidim_points(10, 2, runif)
  
  result <- grid_uniformity_test(points_small)
  expect_type(result, "list")
  
  # Single dimension
  points_1d <- matrix(runif(100), ncol = 1)
  result_1d <- nearest_neighbor_test(points_1d)
  expect_type(result_1d, "list")
  
  # High dimensions
  points_10d <- generate_multidim_points(50, 10, runif)
  result_10d <- grid_uniformity_test(points_10d)
  expect_type(result_10d, "list")
})