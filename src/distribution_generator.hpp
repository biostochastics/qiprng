// File: distribution_generator.hpp
// --------------------------------------------------------------
// Modular distribution generation interface
// Part of Task #21: Refactor EnhancedPRNG for Better Modularity
#ifndef QIPRNG_DISTRIBUTION_GENERATOR_HPP
#define QIPRNG_DISTRIBUTION_GENERATOR_HPP

#include <Rcpp.h>

#include <cmath>
#include <memory>

#include "prng_config.hpp"
#include "ziggurat_normal.hpp"

namespace qiprng {

// Forward declaration
class UniformSource;

// Abstract base class for distribution generators
class DistributionGenerator {
   public:
    virtual ~DistributionGenerator() = default;
    virtual double generate(UniformSource& source) = 0;
    virtual void reset() {}  // Optional reset for stateful generators
};

// Interface for uniform random number sources
class UniformSource {
   public:
    virtual ~UniformSource() = default;
    virtual double next() = 0;  // Returns uniform [0,1)
};

// Uniform distribution generator
class UniformGenerator : public DistributionGenerator {
   private:
    double min_;
    double max_;

   public:
    UniformGenerator(double min = 0.0, double max = 1.0) : min_(min), max_(max) {}

    double generate(UniformSource& source) override {
        double u = source.next();
        if (min_ == 0.0 && max_ == 1.0) {
            return u;
        }
        return min_ + u * (max_ - min_);
    }
};

// Normal distribution generator with multiple methods
class NormalGenerator : public DistributionGenerator {
   private:
    double mean_;
    double sd_;
    std::string method_;
    std::unique_ptr<ZigguratNormal> ziggurat_;
    bool has_spare_;
    double spare_;

    std::pair<double, double> box_muller_pair(double u1, double u2) {
        double radius = std::sqrt(-2.0 * std::log(u1));
        double angle = 2.0 * M_PI * u2;
        return std::make_pair(radius * std::cos(angle), radius * std::sin(angle));
    }

   public:
    NormalGenerator(double mean, double sd, const std::string& method)
        : mean_(mean), sd_(sd), method_(method), has_spare_(false), spare_(0.0) {
        if (method_ == "ziggurat") {
            ziggurat_ = std::make_unique<ZigguratNormal>();
        }
    }

    double generate(UniformSource& source) override {
        double z;

        if (method_ == "ziggurat" && ziggurat_) {
            z = ziggurat_->next(source);
        } else {  // box_muller
            if (has_spare_) {
                z = spare_;
                has_spare_ = false;
            } else {
                double u1 = source.next();
                double u2 = source.next();
                auto [z1, z2] = box_muller_pair(u1, u2);
                z = z1;
                spare_ = z2;
                has_spare_ = true;
            }
        }

        return mean_ + sd_ * z;
    }

    void reset() override {
        has_spare_ = false;
        spare_ = 0.0;
    }
};

// Exponential distribution generator
class ExponentialGenerator : public DistributionGenerator {
   private:
    double lambda_;

   public:
    ExponentialGenerator(double lambda) : lambda_(lambda) {}

    double generate(UniformSource& source) override {
        double u = source.next();
        return -std::log(1.0 - u) / lambda_;
    }
};

// Poisson distribution generator
class PoissonGenerator : public DistributionGenerator {
   private:
    double lambda_;

    double generate_knuth(UniformSource& source) {
        double L = std::exp(-lambda_);
        double k = 0;
        double p = 1.0;

        do {
            k++;
            p *= source.next();
        } while (p > L);

        return k - 1;
    }

    double generate_normal_approx(UniformSource& source) {
        // Use normal approximation for large lambda
        NormalGenerator normal(lambda_, std::sqrt(lambda_), "box_muller");
        double x = normal.generate(source);
        return std::max(0.0, std::round(x));
    }

   public:
    PoissonGenerator(double lambda) : lambda_(lambda) {}

    double generate(UniformSource& source) override {
        if (lambda_ < 30) {
            return generate_knuth(source);
        } else {
            return generate_normal_approx(source);
        }
    }
};

// Factory class for creating distribution generators
class DistributionFactory {
   public:
    static std::unique_ptr<DistributionGenerator> create(const PRNGConfig& config) {
        const std::string& dist = config.distribution;

        if (dist == "uniform") {
            return std::make_unique<UniformGenerator>(config.uniform_min, config.uniform_max);
        } else if (dist == "normal") {
            return std::make_unique<NormalGenerator>(config.normal_mean, config.normal_sd,
                                                     config.normal_method);
        } else if (dist == "exponential") {
            return std::make_unique<ExponentialGenerator>(config.exponential_lambda);
        } else if (dist == "poisson") {
            return std::make_unique<PoissonGenerator>(config.poisson_lambda);
        } else {
            throw std::runtime_error("Unsupported distribution: " + dist);
        }
    }
};

}  // namespace qiprng

#endif  // QIPRNG_DISTRIBUTION_GENERATOR_HPP
