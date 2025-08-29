#ifndef EXTENDED_DISTRIBUTIONS_HPP
#define EXTENDED_DISTRIBUTIONS_HPP

#include <cmath>
#include <vector>
#include <stdexcept>
#include <algorithm>
#include <complex>
#include <functional>
#include <Eigen/Dense>  // For multivariate normal
#include "precision_utils.hpp"  // For high-precision constants and safe conversions

namespace qiprng {
namespace distributions {

// Base class for distributions
class Distribution {
public:
    virtual ~Distribution() = default;
    virtual double sample(std::function<double()> uniform_gen) = 0;
    virtual std::vector<double> sample_batch(std::function<double()> uniform_gen, size_t n) {
        std::vector<double> samples(n);
        for (size_t i = 0; i < n; ++i) {
            samples[i] = sample(uniform_gen);
        }
        return samples;
    }
};

// Levy stable distribution using Chambers-Mallows-Stuck method
class LevyStable : public Distribution {
private:
    double alpha_;  // Stability parameter (0,2]
    double beta_;   // Skewness parameter [-1,1]
    double mu_;     // Location parameter
    double sigma_;  // Scale parameter
    
public:
    LevyStable(double alpha, double beta = 0, double mu = 0, double sigma = 1)
        : alpha_(alpha), beta_(beta), mu_(mu), sigma_(sigma) {
        if (alpha <= 0 || alpha > 2) {
            throw std::invalid_argument("Alpha must be in (0,2]");
        }
        if (beta < -1 || beta > 1) {
            throw std::invalid_argument("Beta must be in [-1,1]");
        }
        if (sigma <= 0) {
            throw std::invalid_argument("Sigma must be positive");
        }
    }
    
    double sample(std::function<double()> uniform_gen) override {
        // Chambers-Mallows-Stuck algorithm
        double u = M_PI * (uniform_gen() - 0.5);
        double w = -std::log(uniform_gen());
        
        if (alpha_ == 1.0) {
            // Cauchy case
            double x = ((2.0 / M_PI) * ((M_PI / 2.0 + beta_ * u) * std::tan(u) 
                       - beta_ * std::log((M_PI * w * std::cos(u)) / (M_PI / 2.0 + beta_ * u))));
            return sigma_ * x + mu_;
        }
        
        double zeta = -beta_ * std::tan(M_PI * alpha_ / 2.0);
        double xi = (1.0 / alpha_) * std::atan(-zeta);
        
        double v = xi + u;
        double x = std::pow(1.0 + zeta * zeta, 1.0 / (2.0 * alpha_)) *
                  (std::sin(alpha_ * v) / std::pow(std::cos(u), 1.0 / alpha_)) *
                  std::pow(std::cos(u - alpha_ * v) / w, (1.0 - alpha_) / alpha_);
        
        return sigma_ * x + mu_;
    }
};

// Pareto distribution (heavy-tailed)
class Pareto : public Distribution {
private:
    double xm_;     // Scale parameter (minimum value)
    double alpha_;  // Shape parameter
    
public:
    Pareto(double xm, double alpha) : xm_(xm), alpha_(alpha) {
        if (xm <= 0) {
            throw std::invalid_argument("Scale parameter must be positive");
        }
        if (alpha <= 0) {
            throw std::invalid_argument("Shape parameter must be positive");
        }
    }
    
    double sample(std::function<double()> uniform_gen) override {
        double u = uniform_gen();
        return xm_ / std::pow(1.0 - u, 1.0 / alpha_);
    }
};

// Cauchy distribution (heavy-tailed)
class Cauchy : public Distribution {
private:
    double location_;
    double scale_;
    
public:
    Cauchy(double location = 0, double scale = 1) 
        : location_(location), scale_(scale) {
        if (scale <= 0) {
            throw std::invalid_argument("Scale must be positive");
        }
    }
    
    double sample(std::function<double()> uniform_gen) override {
        double u = uniform_gen();
        return location_ + scale_ * std::tan(M_PI * (u - 0.5));
    }
};

// Multivariate Normal distribution
class MultivariateNormal {
private:
    Eigen::VectorXd mean_;
    Eigen::MatrixXd L_;  // Cholesky decomposition of covariance
    size_t dim_;
    
public:
    MultivariateNormal(const std::vector<double>& mean,
                       const std::vector<std::vector<double>>& covariance) {
        dim_ = mean.size();
        if (covariance.size() != dim_ || covariance[0].size() != dim_) {
            throw std::invalid_argument("Covariance matrix dimensions must match mean vector");
        }
        
        // Convert to Eigen types
        mean_ = Eigen::VectorXd(dim_);
        for (size_t i = 0; i < dim_; ++i) {
            mean_(i) = mean[i];
        }
        
        Eigen::MatrixXd cov(dim_, dim_);
        for (size_t i = 0; i < dim_; ++i) {
            for (size_t j = 0; j < dim_; ++j) {
                cov(i, j) = covariance[i][j];
            }
        }
        
        // Compute Cholesky decomposition
        Eigen::LLT<Eigen::MatrixXd> llt(cov);
        if (llt.info() != Eigen::Success) {
            throw std::invalid_argument("Covariance matrix must be positive definite");
        }
        L_ = llt.matrixL();
    }
    
    std::vector<double> sample(std::function<double()> uniform_gen) {
        // Generate standard normal variates using Box-Muller
        Eigen::VectorXd z(dim_);
        for (size_t i = 0; i < dim_; i += 2) {
            double u1 = uniform_gen();
            double u2 = uniform_gen();
            double r = std::sqrt(-2.0 * std::log(u1));
            double theta = 2.0 * M_PI * u2;
            
            z(i) = r * std::cos(theta);
            if (i + 1 < dim_) {
                z(i + 1) = r * std::sin(theta);
            }
        }
        
        // Transform to multivariate normal
        Eigen::VectorXd x = mean_ + L_ * z;
        
        // Convert back to std::vector
        std::vector<double> result(dim_);
        for (size_t i = 0; i < dim_; ++i) {
            result[i] = x(i);
        }
        return result;
    }
};

// Gaussian Copula for dependency structures
class GaussianCopula {
private:
    Eigen::MatrixXd L_;  // Cholesky decomposition of correlation matrix
    size_t dim_;
    std::vector<std::unique_ptr<Distribution>> marginals_;
    
    // Inverse CDF of standard normal (using rational approximation)
    double norm_inv(double p) const {
        if (p <= 0 || p >= 1) {
            throw std::invalid_argument("Probability must be in (0,1)");
        }
        
        // Rational approximation coefficients
        const double a[] = {-3.969683028665376e+01, 2.209460984245205e+02,
                           -2.759285104469687e+02, 1.383577518672690e+02,
                           -3.066479806614716e+01, 2.506628277459239e+00};
        const double b[] = {-5.447609879822406e+01, 1.615858368580409e+02,
                           -1.556989798598866e+02, 6.680131188771972e+01,
                           -1.328068155288572e+01};
        const double c[] = {-7.784894002430293e-03, -3.223964580411365e-01,
                           -2.400758277161838e+00, -2.549732539343734e+00,
                           4.374664141464968e+00, 2.938163982698783e+00};
        const double d[] = {7.784695709041462e-03, 3.224671290700398e-01,
                           2.445134137142996e+00, 3.754408661907416e+00};
        
        double q = p < 0.5 ? p : 1 - p;
        double r;
        
        if (q > 0.02425) {
            // Central region
            double x = q - 0.5;
            double num = (((((a[0] * x + a[1]) * x + a[2]) * x + a[3]) * x + a[4]) * x + a[5]);
            double den = ((((b[0] * x + b[1]) * x + b[2]) * x + b[3]) * x + b[4]) * x + 1;
            r = x * num / den;
        } else {
            // Tail region
            double x = std::sqrt(-2.0 * std::log(q));
            double num = ((((c[0] * x + c[1]) * x + c[2]) * x + c[3]) * x + c[4]) * x + c[5];
            double den = (((d[0] * x + d[1]) * x + d[2]) * x + d[3]) * x + 1;
            r = num / den;
            if (p < 0.5) r = -r;
        }
        
        return p < 0.5 ? -r : r;
    }
    
public:
    GaussianCopula(const std::vector<std::vector<double>>& correlation,
                   std::vector<std::unique_ptr<Distribution>> marginals)
        : dim_(marginals.size()), marginals_(std::move(marginals)) {
        
        if (correlation.size() != dim_ || correlation[0].size() != dim_) {
            throw std::invalid_argument("Correlation matrix dimensions must match number of marginals");
        }
        
        // Convert to Eigen and compute Cholesky
        Eigen::MatrixXd corr(dim_, dim_);
        for (size_t i = 0; i < dim_; ++i) {
            for (size_t j = 0; j < dim_; ++j) {
                corr(i, j) = correlation[i][j];
            }
        }
        
        Eigen::LLT<Eigen::MatrixXd> llt(corr);
        if (llt.info() != Eigen::Success) {
            throw std::invalid_argument("Correlation matrix must be positive definite");
        }
        L_ = llt.matrixL();
    }
    
    std::vector<double> sample(std::function<double()> uniform_gen) {
        // Generate correlated normal variates
        Eigen::VectorXd z(dim_);
        for (size_t i = 0; i < dim_; ++i) {
            double u1 = uniform_gen();
            double u2 = uniform_gen();
            double r = std::sqrt(-2.0 * std::log(u1));
            double theta = 2.0 * M_PI * u2;
            z(i) = r * std::cos(theta);
        }
        
        Eigen::VectorXd y = L_ * z;
        
        // Transform to uniform using normal CDF
        std::vector<double> u(dim_);
        for (size_t i = 0; i < dim_; ++i) {
            // Normal CDF approximation
            double t = 1.0 / (1.0 + 0.2316419 * std::abs(y(i)));
            double d = 0.3989423 * std::exp(-y(i) * y(i) / 2.0);
            double prob = 1.0 - d * t * (0.319381530 + t * (-0.356563782 + 
                         t * (1.781477937 + t * (-1.821255978 + t * 1.330274429))));
            u[i] = y(i) < 0 ? 1.0 - prob : prob;
        }
        
        // Apply marginal distributions
        std::vector<double> result(dim_);
        for (size_t i = 0; i < dim_; ++i) {
            // Use inverse transform with the marginal
            auto gen = [u, i]() { return u[i]; };
            result[i] = marginals_[i]->sample(gen);
        }
        
        return result;
    }
};

// Student's t distribution (heavy-tailed)
class StudentT : public Distribution {
private:
    double df_;  // Degrees of freedom
    double mu_;  // Location
    double sigma_;  // Scale
    
public:
    StudentT(double df, double mu = 0, double sigma = 1) 
        : df_(df), mu_(mu), sigma_(sigma) {
        if (df <= 0) {
            throw std::invalid_argument("Degrees of freedom must be positive");
        }
        if (sigma <= 0) {
            throw std::invalid_argument("Scale must be positive");
        }
    }
    
    double sample(std::function<double()> uniform_gen) override {
        // Generate using ratio of normal to chi-square
        // First generate standard normal using Box-Muller
        double u1 = uniform_gen();
        double u2 = uniform_gen();
        double z = std::sqrt(-2.0 * std::log(u1)) * std::cos(2.0 * M_PI * u2);
        
        // Generate chi-square with df degrees of freedom
        double chi_sq = 0;
        for (int i = 0; i < df_; ++i) {
            double u3 = uniform_gen();
            double u4 = uniform_gen();
            double n = std::sqrt(-2.0 * std::log(u3)) * std::cos(2.0 * M_PI * u4);
            chi_sq += n * n;
        }
        
        // Student's t = normal / sqrt(chi-square/df)
        double t = z / std::sqrt(chi_sq / df_);
        return mu_ + sigma_ * t;
    }
};

} // namespace distributions
} // namespace qiprng

#endif // EXTENDED_DISTRIBUTIONS_HPP