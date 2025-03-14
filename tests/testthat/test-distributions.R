context("Distribution Tests")

test_that("Normal distribution properties", {
    # skip_on_cran()
    
    # Create PRNG with normal distribution
    cfg <- list(
        a = 2,
        b = 5,
        c = -2,  # Ensures positive discriminant
        mpfr_precision = 53,
        distribution = "normal",
        normal_mean = 0,
        normal_sd = 1
    )
    createPRNG(cfg)
    
    # Generate large sample
    n <- 5000  # Reduced sample size for faster testing
    nums <- generatePRNG(n)
    
    # Check basic properties with relaxed thresholds
    expect_lt(abs(mean(nums)), 0.2)  # Mean should be close to 0
    expect_lt(abs(sd(nums) - 1), 0.2)  # SD should be close to 1
    
    # Check percentiles (68-95-99.7 rule)
    expect_gt(mean(abs(nums) <= 1), 0.6)     # ~68% within 1 SD
    expect_gt(mean(abs(nums) <= 2), 0.9)     # ~95% within 2 SD
    expect_gt(mean(abs(nums) <= 3), 0.98)    # ~99.7% within 3 SD
    
    # Check normality with relaxed threshold
    sw_test <- shapiro.test(nums[1:1000])  # Use subset for Shapiro-Wilk test
    expect_gt(sw_test$p.value, 0.01)
    
    # Check skewness and kurtosis with relaxed thresholds
    library(moments)
    skewness <- skewness(nums)
    kurtosis <- kurtosis(nums)
    expect_lt(abs(skewness), 0.3)  # Should be close to 0
    expect_lt(abs(kurtosis - 3), 1.0)  # Should be close to 3
    
    cleanup_prng()
})

test_that("Uniform range properties", {
    # skip_on_cran()
    
    # Create PRNG with uniform range distribution
    cfg <- list(
        a = 2,
        b = 5,
        c = -2,  # Ensures positive discriminant
        mpfr_precision = 53,
        distribution = "uniform_range",
        range_min = -1,
        range_max = 1
    )
    createPRNG(cfg)
    
    # Generate large sample
    n <- 5000
    nums <- generatePRNG(n)
    
    # Add small jitter to avoid ties in statistical tests
    nums <- nums + runif(length(nums), -1e-12, 1e-12)
    
    # Check basic properties
    expect_true(all(nums >= -1 - 1e-10 & nums <= 1 + 1e-10))  # Allow tiny numerical error
    expect_lt(abs(mean(nums)), 0.1)  # Mean should be close to 0
    expect_lt(abs(sd(nums) - sqrt(1/3)), 0.1)  # SD should be close to sqrt(1/3)
    
    # Check quantiles with relaxed thresholds
    q <- quantile(nums, probs = c(0.25, 0.5, 0.75))
    expect_lt(abs(q[1] + 0.5), 0.15)  # Q1 should be close to -0.5
    expect_lt(abs(q[2]), 0.15)        # Median should be close to 0
    expect_lt(abs(q[3] - 0.5), 0.15)  # Q3 should be close to 0.5
    
    # Test uniformity
    ks <- ks.test((nums + 1)/2, "punif")  # Transform to [0,1]
    expect_gt(ks$p.value, 0.001)  # Relaxed threshold
    
    cleanup_prng()
})

test_that("Exponential distribution properties", {
    # skip_on_cran()
    
    # Create PRNG with exponential distribution
    cfg <- list(
        a = 2,
        b = 5,
        c = -2,  # Ensures positive discriminant
        mpfr_precision = 53,
        distribution = "exponential",
        exponential_lambda = 1
    )
    createPRNG(cfg)
    
    # Generate large sample
    n <- 5000
    nums <- generatePRNG(n)
    
    # Check basic properties with relaxed thresholds
    expect_true(all(nums >= 0))  # All values should be non-negative
    expect_lt(abs(mean(nums) - 1), 0.2)  # Mean should be close to 1/lambda
    expect_lt(abs(sd(nums) - 1), 0.3)    # SD should be close to 1/lambda
    
    # Check quantiles with relaxed thresholds
    q <- quantile(nums, probs = c(0.25, 0.5, 0.75))
    expect_lt(abs(q[2] - log(2)), 0.25)  # Median should be close to ln(2)
    
    # Check distribution shape with relaxed threshold
    x_grid <- seq(0, 10, length.out = 100)
    emp_cdf <- ecdf(nums)
    theo_cdf <- pexp(x_grid, rate = 1)
    max_diff <- max(abs(emp_cdf(x_grid) - theo_cdf))
    expect_lt(max_diff, 0.2)  # Maximum difference should be small
    
    cleanup_prng()
})