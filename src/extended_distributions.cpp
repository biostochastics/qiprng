#include "extended_distributions.hpp"

#include <Rcpp.h>

#include <memory>
#include <vector>

#include "enhanced_prng.hpp"
#include "prng_utils.hpp"

// Conditional compilation for Eigen-dependent distributions
#ifdef HAVE_EIGEN
#    define EIGEN_AVAILABLE 1
#else
#    define EIGEN_AVAILABLE 0
#    warning "Eigen not found - Multivariate Normal and Gaussian Copula will be disabled"
#endif

namespace qiprng {
namespace distributions {

// Helper function to get uniform random from current PRNG state
class PRNGUniformGenerator {
   private:
    EnhancedPRNG* prng_;

   public:
    PRNGUniformGenerator(EnhancedPRNG* prng) : prng_(prng) {}

    double operator()() {
        // Use next() to get uniform [0,1) value
        return prng_->next();
    }
};

}  // namespace distributions
}  // namespace qiprng

// Rcpp exports for extended distributions

// [[Rcpp::export]]
Rcpp::NumericVector cpp_levy_stable(int n, double alpha, double beta = 0, double mu = 0,
                                    double sigma = 1) {
    if (alpha <= 0 || alpha > 2) {
        Rcpp::stop("Alpha must be in (0,2]");
    }
    if (beta < -1 || beta > 1) {
        Rcpp::stop("Beta must be in [-1,1]");
    }
    if (sigma <= 0) {
        Rcpp::stop("Sigma must be positive");
    }

    // Get current PRNG instance (thread-local or global based on threading mode)
    qiprng::EnhancedPRNG* prng = nullptr;
    if (qiprng::g_use_threading) {
        if (!qiprng::t_prng) {
            Rcpp::stop("PRNG not initialized in current thread. Call createPRNG() first.");
        }
        prng = qiprng::t_prng.get();
    } else {
        std::lock_guard<std::mutex> lock(qiprng::g_prng_mutex);
        if (!qiprng::g_prng) {
            Rcpp::stop("PRNG not initialized. Call createPRNG() first.");
        }
        prng = qiprng::g_prng.get();
    }

    qiprng::distributions::PRNGUniformGenerator gen(prng);
    qiprng::distributions::LevyStable dist(alpha, beta, mu, sigma);

    Rcpp::NumericVector result(n);
    for (int i = 0; i < n; ++i) {
        result[i] = dist.sample(gen);
    }

    return result;
}

// [[Rcpp::export]]
Rcpp::NumericVector cpp_pareto(int n, double xm, double alpha) {
    if (xm <= 0) {
        Rcpp::stop("Scale parameter (xm) must be positive");
    }
    if (alpha <= 0) {
        Rcpp::stop("Shape parameter (alpha) must be positive");
    }

    // Get current PRNG instance (thread-local or global based on threading mode)
    qiprng::EnhancedPRNG* prng = nullptr;
    if (qiprng::g_use_threading) {
        if (!qiprng::t_prng) {
            Rcpp::stop("PRNG not initialized in current thread. Call createPRNG() first.");
        }
        prng = qiprng::t_prng.get();
    } else {
        std::lock_guard<std::mutex> lock(qiprng::g_prng_mutex);
        if (!qiprng::g_prng) {
            Rcpp::stop("PRNG not initialized. Call createPRNG() first.");
        }
        prng = qiprng::g_prng.get();
    }

    qiprng::distributions::PRNGUniformGenerator gen(prng);
    qiprng::distributions::Pareto dist(xm, alpha);

    Rcpp::NumericVector result(n);
    for (int i = 0; i < n; ++i) {
        result[i] = dist.sample(gen);
    }

    return result;
}

// [[Rcpp::export]]
Rcpp::NumericVector cpp_cauchy(int n, double location = 0, double scale = 1) {
    if (scale <= 0) {
        Rcpp::stop("Scale must be positive");
    }

    // Get current PRNG instance (thread-local or global based on threading mode)
    qiprng::EnhancedPRNG* prng = nullptr;
    if (qiprng::g_use_threading) {
        if (!qiprng::t_prng) {
            Rcpp::stop("PRNG not initialized in current thread. Call createPRNG() first.");
        }
        prng = qiprng::t_prng.get();
    } else {
        std::lock_guard<std::mutex> lock(qiprng::g_prng_mutex);
        if (!qiprng::g_prng) {
            Rcpp::stop("PRNG not initialized. Call createPRNG() first.");
        }
        prng = qiprng::g_prng.get();
    }

    qiprng::distributions::PRNGUniformGenerator gen(prng);
    qiprng::distributions::Cauchy dist(location, scale);

    Rcpp::NumericVector result(n);
    for (int i = 0; i < n; ++i) {
        result[i] = dist.sample(gen);
    }

    return result;
}

// Note: Student's t is already implemented elsewhere according to user

// [[Rcpp::export]]
Rcpp::NumericMatrix cpp_multivariate_normal(int n, Rcpp::NumericVector mean,
                                            Rcpp::NumericMatrix covariance) {
#if EIGEN_AVAILABLE
    int dim = mean.size();
    if (covariance.nrow() != dim || covariance.ncol() != dim) {
        Rcpp::stop("Covariance matrix dimensions must match mean vector");
    }

    // Get current PRNG instance (thread-local or global based on threading mode)
    qiprng::EnhancedPRNG* prng = nullptr;
    if (qiprng::g_use_threading) {
        if (!qiprng::t_prng) {
            Rcpp::stop("PRNG not initialized in current thread. Call createPRNG() first.");
        }
        prng = qiprng::t_prng.get();
    } else {
        std::lock_guard<std::mutex> lock(qiprng::g_prng_mutex);
        if (!qiprng::g_prng) {
            Rcpp::stop("PRNG not initialized. Call createPRNG() first.");
        }
        prng = qiprng::g_prng.get();
    }

    // Convert R types to std::vector
    std::vector<double> mean_vec(mean.begin(), mean.end());
    std::vector<std::vector<double>> cov_mat(dim, std::vector<double>(dim));
    for (int i = 0; i < dim; ++i) {
        for (int j = 0; j < dim; ++j) {
            cov_mat[i][j] = covariance(i, j);
        }
    }

    qiprng::distributions::PRNGUniformGenerator gen(prng);
    qiprng::distributions::MultivariateNormal dist(mean_vec, cov_mat);

    Rcpp::NumericMatrix result(n, dim);
    for (int i = 0; i < n; ++i) {
        auto sample = dist.sample(gen);
        for (int j = 0; j < dim; ++j) {
            result(i, j) = sample[j];
        }
    }

    return result;
#else
    Rcpp::stop("Multivariate normal distribution requires Eigen library. "
               "Please install Eigen and rebuild the package.");
#endif
}

// [[Rcpp::export]]
Rcpp::NumericMatrix cpp_gaussian_copula(int n, Rcpp::NumericMatrix correlation,
                                        Rcpp::List marginal_params) {
#if EIGEN_AVAILABLE
    int dim = correlation.nrow();
    if (correlation.ncol() != dim) {
        Rcpp::stop("Correlation matrix must be square");
    }
    if (marginal_params.size() != dim) {
        Rcpp::stop("Number of marginal distributions must match correlation dimension");
    }

    // Get current PRNG instance (thread-local or global based on threading mode)
    qiprng::EnhancedPRNG* prng = nullptr;
    if (qiprng::g_use_threading) {
        if (!qiprng::t_prng) {
            Rcpp::stop("PRNG not initialized in current thread. Call createPRNG() first.");
        }
        prng = qiprng::t_prng.get();
    } else {
        std::lock_guard<std::mutex> lock(qiprng::g_prng_mutex);
        if (!qiprng::g_prng) {
            Rcpp::stop("PRNG not initialized. Call createPRNG() first.");
        }
        prng = qiprng::g_prng.get();
    }

    // Convert correlation matrix
    std::vector<std::vector<double>> corr_mat(dim, std::vector<double>(dim));
    for (int i = 0; i < dim; ++i) {
        for (int j = 0; j < dim; ++j) {
            corr_mat[i][j] = correlation(i, j);
        }
    }

    // Create marginal distributions based on parameters
    std::vector<std::unique_ptr<qiprng::distributions::Distribution>> marginals;
    for (int i = 0; i < dim; ++i) {
        Rcpp::List params = marginal_params[i];
        std::string type = Rcpp::as<std::string>(params["type"]);

        if (type == "cauchy") {
            double loc = Rcpp::as<double>(params["location"]);
            double scale = Rcpp::as<double>(params["scale"]);
            marginals.push_back(std::make_unique<qiprng::distributions::Cauchy>(loc, scale));
        } else if (type == "pareto") {
            double xm = Rcpp::as<double>(params["xm"]);
            double alpha = Rcpp::as<double>(params["alpha"]);
            marginals.push_back(std::make_unique<qiprng::distributions::Pareto>(xm, alpha));
        } else if (type == "levy") {
            double alpha = Rcpp::as<double>(params["alpha"]);
            double beta = Rcpp::as<double>(params["beta"]);
            double mu = Rcpp::as<double>(params["mu"]);
            double sigma = Rcpp::as<double>(params["sigma"]);
            marginals.push_back(
                std::make_unique<qiprng::distributions::LevyStable>(alpha, beta, mu, sigma));
        } else {
            Rcpp::stop("Unknown marginal distribution type: " + type);
        }
    }

    qiprng::distributions::PRNGUniformGenerator gen(prng);
    qiprng::distributions::GaussianCopula copula(corr_mat, std::move(marginals));

    Rcpp::NumericMatrix result(n, dim);
    for (int i = 0; i < n; ++i) {
        auto sample = copula.sample(gen);
        for (int j = 0; j < dim; ++j) {
            result(i, j) = sample[j];
        }
    }

    return result;
#else
    Rcpp::stop("Gaussian copula requires Eigen library. "
               "Please install Eigen and rebuild the package.");
#endif
}