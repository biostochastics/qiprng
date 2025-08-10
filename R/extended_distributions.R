#' Generate Levy Stable Distribution
#'
#' Generates random numbers from a Levy stable distribution using the 
#' Chambers-Mallows-Stuck method.
#'
#' @param n Number of random values to generate
#' @param alpha Stability parameter (0,2]. Alpha=2 gives normal distribution, 
#'              alpha=1 gives Cauchy distribution
#' @param beta Skewness parameter [-1,1]. Beta=0 gives symmetric distribution
#' @param mu Location parameter
#' @param sigma Scale parameter (must be positive)
#'
#' @return Numeric vector of n random values from the Levy stable distribution
#'
#' @details The Levy stable distribution is a family of continuous probability
#' distributions parameterized by stability (alpha) and skewness (beta). It
#' includes normal (alpha=2), Cauchy (alpha=1), and Levy (alpha=0.5, beta=1)
#' distributions as special cases.
#'
#' @examples
#' \dontrun{
#' # Generate from symmetric stable distribution
#' x <- generate_levy_stable(1000, alpha=1.5, beta=0)
#' 
#' # Generate from skewed stable distribution
#' y <- generate_levy_stable(1000, alpha=1.8, beta=0.5, mu=2, sigma=1)
#' }
#'
#' @export
generate_levy_stable <- function(n, alpha, beta = 0, mu = 0, sigma = 1) {
  if (!is.numeric(n) || length(n) != 1 || n <= 0 || n != as.integer(n)) {
    stop("n must be a positive integer")
  }
  
  if (alpha <= 0 || alpha > 2) {
    stop("alpha must be in (0,2]")
  }
  
  if (beta < -1 || beta > 1) {
    stop("beta must be in [-1,1]")
  }
  
  if (sigma <= 0) {
    stop("sigma must be positive")
  }
  
  cpp_levy_stable(as.integer(n), alpha, beta, mu, sigma)
}

#' Generate Pareto Distribution
#'
#' Generates random numbers from a Pareto distribution.
#'
#' @param n Number of random values to generate
#' @param xm Scale parameter (minimum value, must be positive)
#' @param alpha Shape parameter (must be positive)
#'
#' @return Numeric vector of n random values from the Pareto distribution
#'
#' @details The Pareto distribution is a heavy-tailed distribution often used
#' in economics and actuarial science. It follows the 80-20 rule and is useful
#' for modeling phenomena with power-law tails.
#'
#' @examples
#' \dontrun{
#' # Generate from Pareto distribution
#' x <- generate_pareto(1000, xm=1, alpha=2)
#' 
#' # Heavy-tailed distribution (smaller alpha = heavier tail)
#' y <- generate_pareto(1000, xm=10, alpha=1.5)
#' }
#'
#' @export
generate_pareto <- function(n, xm, alpha) {
  if (!is.numeric(n) || length(n) != 1 || n <= 0 || n != as.integer(n)) {
    stop("n must be a positive integer")
  }
  
  if (xm <= 0) {
    stop("xm (scale parameter) must be positive")
  }
  
  if (alpha <= 0) {
    stop("alpha (shape parameter) must be positive")
  }
  
  cpp_pareto(as.integer(n), xm, alpha)
}

#' Generate Cauchy Distribution
#'
#' Generates random numbers from a Cauchy distribution.
#'
#' @param n Number of random values to generate
#' @param location Location parameter (median of the distribution)
#' @param scale Scale parameter (must be positive)
#'
#' @return Numeric vector of n random values from the Cauchy distribution
#'
#' @details The Cauchy distribution is a heavy-tailed distribution with no
#' defined mean or variance. It is the distribution of the ratio of two
#' independent normal random variables.
#'
#' @examples
#' \dontrun{
#' # Standard Cauchy distribution
#' x <- generate_cauchy(1000)
#' 
#' # Cauchy with different location and scale
#' y <- generate_cauchy(1000, location=5, scale=2)
#' }
#'
#' @export
generate_cauchy <- function(n, location = 0, scale = 1) {
  if (!is.numeric(n) || length(n) != 1 || n <= 0 || n != as.integer(n)) {
    stop("n must be a positive integer")
  }
  
  if (scale <= 0) {
    stop("scale must be positive")
  }
  
  cpp_cauchy(as.integer(n), location, scale)
}

#' Generate Multivariate Normal Distribution
#'
#' Generates random vectors from a multivariate normal distribution using
#' Cholesky decomposition.
#'
#' @param n Number of random vectors to generate
#' @param mean Mean vector of length d
#' @param covariance Covariance matrix (d x d, must be positive definite)
#'
#' @return Matrix of n rows and d columns, where each row is a random vector
#'
#' @details Uses the Cholesky decomposition of the covariance matrix to
#' transform independent standard normal variables into correlated
#' multivariate normal variables. Requires Eigen library.
#'
#' @examples
#' \dontrun{
#' # 2D multivariate normal
#' mean <- c(0, 0)
#' cov <- matrix(c(1, 0.5, 0.5, 1), 2, 2)
#' x <- generate_multivariate_normal(1000, mean, cov)
#' 
#' # 3D multivariate normal with correlation
#' mean3 <- c(1, 2, 3)
#' cov3 <- matrix(c(1, 0.3, 0.2,
#'                  0.3, 1, 0.4,
#'                  0.2, 0.4, 1), 3, 3)
#' y <- generate_multivariate_normal(1000, mean3, cov3)
#' }
#'
#' @export
generate_multivariate_normal <- function(n, mean, covariance) {
  if (!is.numeric(n) || length(n) != 1 || n <= 0 || n != as.integer(n)) {
    stop("n must be a positive integer")
  }
  
  if (!is.numeric(mean) || !is.vector(mean)) {
    stop("mean must be a numeric vector")
  }
  
  if (!is.matrix(covariance) || !is.numeric(covariance)) {
    stop("covariance must be a numeric matrix")
  }
  
  d <- length(mean)
  if (nrow(covariance) != d || ncol(covariance) != d) {
    stop("covariance matrix dimensions must match mean vector length")
  }
  
  # Check symmetry
  if (!isSymmetric(covariance)) {
    stop("covariance matrix must be symmetric")
  }
  
  # Check positive definiteness (all eigenvalues > 0)
  eigenvals <- eigen(covariance, only.values = TRUE)$values
  if (any(eigenvals <= 0)) {
    stop("covariance matrix must be positive definite")
  }
  
  cpp_multivariate_normal(as.integer(n), as.numeric(mean), as.matrix(covariance))
}

#' Generate Samples with Gaussian Copula
#'
#' Generates random vectors from specified marginal distributions with
#' dependence structure defined by a Gaussian copula.
#'
#' @param n Number of random vectors to generate
#' @param correlation Correlation matrix defining the copula (must be positive definite)
#' @param marginals List of marginal distribution specifications. Each element
#'                  should be a list with 'type' and distribution-specific parameters
#'
#' @return Matrix of n rows and d columns, where d is the number of marginals
#'
#' @details The Gaussian copula allows combining arbitrary marginal distributions
#' with a correlation structure. This is useful for modeling multivariate
#' distributions where marginals are not normal but dependencies exist.
#'
#' @examples
#' \dontrun{
#' # Combine Cauchy and Pareto marginals with correlation
#' correlation <- matrix(c(1, 0.6, 0.6, 1), 2, 2)
#' marginals <- list(
#'   list(type = "cauchy", location = 0, scale = 1),
#'   list(type = "pareto", xm = 1, alpha = 2)
#' )
#' x <- generate_with_copula(1000, correlation, marginals)
#' 
#' # Three different marginals with correlation
#' corr3 <- matrix(c(1, 0.3, 0.2,
#'                   0.3, 1, 0.4,
#'                   0.2, 0.4, 1), 3, 3)
#' marg3 <- list(
#'   list(type = "levy", alpha = 1.5, beta = 0, mu = 0, sigma = 1),
#'   list(type = "cauchy", location = 2, scale = 1),
#'   list(type = "pareto", xm = 1, alpha = 3)
#' )
#' y <- generate_with_copula(1000, corr3, marg3)
#' }
#'
#' @export
generate_with_copula <- function(n, correlation, marginals) {
  if (!is.numeric(n) || length(n) != 1 || n <= 0 || n != as.integer(n)) {
    stop("n must be a positive integer")
  }
  
  if (!is.matrix(correlation) || !is.numeric(correlation)) {
    stop("correlation must be a numeric matrix")
  }
  
  d <- nrow(correlation)
  if (ncol(correlation) != d) {
    stop("correlation matrix must be square")
  }
  
  if (!is.list(marginals) || length(marginals) != d) {
    stop("marginals must be a list with length equal to correlation matrix dimension")
  }
  
  # Check correlation matrix properties
  if (!isSymmetric(correlation)) {
    stop("correlation matrix must be symmetric")
  }
  
  # Check diagonal is all 1s
  if (any(abs(diag(correlation) - 1) > 1e-10)) {
    stop("correlation matrix diagonal must be all 1s")
  }
  
  # Check positive definiteness
  eigenvals <- eigen(correlation, only.values = TRUE)$values
  if (any(eigenvals <= 0)) {
    stop("correlation matrix must be positive definite")
  }
  
  # Validate marginal specifications
  for (i in seq_along(marginals)) {
    marg <- marginals[[i]]
    if (!is.list(marg) || is.null(marg$type)) {
      stop(paste("Marginal", i, "must be a list with 'type' field"))
    }
    
    type <- marg$type
    if (!(type %in% c("cauchy", "pareto", "levy"))) {
      stop(paste("Unknown marginal type:", type, 
                 "- supported types are 'cauchy', 'pareto', 'levy'"))
    }
    
    # Validate parameters for each type
    if (type == "cauchy") {
      if (is.null(marg$location)) marg$location <- 0
      if (is.null(marg$scale)) marg$scale <- 1
      if (marg$scale <= 0) stop("Cauchy scale must be positive")
      marginals[[i]] <- marg
    } else if (type == "pareto") {
      if (is.null(marg$xm) || is.null(marg$alpha)) {
        stop("Pareto marginal requires 'xm' and 'alpha' parameters")
      }
      if (marg$xm <= 0) stop("Pareto xm must be positive")
      if (marg$alpha <= 0) stop("Pareto alpha must be positive")
    } else if (type == "levy") {
      if (is.null(marg$alpha)) stop("Levy marginal requires 'alpha' parameter")
      if (is.null(marg$beta)) marg$beta <- 0
      if (is.null(marg$mu)) marg$mu <- 0
      if (is.null(marg$sigma)) marg$sigma <- 1
      if (marg$alpha <= 0 || marg$alpha > 2) stop("Levy alpha must be in (0,2]")
      if (marg$beta < -1 || marg$beta > 1) stop("Levy beta must be in [-1,1]")
      if (marg$sigma <= 0) stop("Levy sigma must be positive")
      marginals[[i]] <- marg
    }
  }
  
  cpp_gaussian_copula(as.integer(n), as.matrix(correlation), marginals)
}