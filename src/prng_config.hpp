// File: prng_config.hpp
// --------------------------------------------------------------
#ifndef QIPRNG_CONFIG_HPP
#define QIPRNG_CONFIG_HPP

#include "prng_common.hpp" // For PRNGDefaults

namespace qiprng {

// PRNGConfig structure
struct PRNGConfig {
    enum Distribution {
        UNIFORM_01,
        UNIFORM_RANGE,
        NORMAL,
        EXPONENTIAL,
        POISSON,
        GAMMA,
        BETA,
        BERNOULLI,
        BINOMIAL,
        LOGNORMAL,
        WEIBULL,
        CHISQUARED,
        STUDENT_T,
        NEGATIVE_BINOMIAL
    };

    enum NormalMethod {
        BOX_MULLER = PRNGDefaults::BOX_MULLER,
        ZIGGURAT = PRNGDefaults::ZIGGURAT
    };
    NormalMethod normal_method = (NormalMethod)PRNGDefaults::normal_method;

    // Core parameters
    long a = PRNGDefaults::aa;
    long b = PRNGDefaults::b;
    long c = PRNGDefaults::c;
    unsigned int mpfr_precision = PRNGDefaults::mpfr_precision;
    size_t buffer_size = PRNGDefaults::buffer_size;

    // Distribution parameters
    Distribution distribution = UNIFORM_01;
    double range_min = PRNGDefaults::range_min;
    double range_max = PRNGDefaults::range_max;
    double normal_mean = PRNGDefaults::normal_mean;
    double normal_sd = PRNGDefaults::normal_sd;
    double exponential_lambda = PRNGDefaults::exponential_lambda;

    // New distribution parameters
    double poisson_lambda = 1.0;
    double gamma_shape = 1.0;
    double gamma_scale = 1.0;
    double beta_alpha = 1.0;
    double beta_beta = 1.0;
    
    // Additional distribution parameters
    double bernoulli_p = 0.5;
    int binomial_n = 10;
    double binomial_p = 0.5;
    double lognormal_mu = 0.0;
    double lognormal_sigma = 1.0;
    double weibull_shape = 1.0;
    double weibull_scale = 1.0;
    double chisquared_df = 1.0;
    double student_t_df = 1.0;
    double negative_binomial_r = 1.0;
    double negative_binomial_p = 0.5;

    // Crypto & advanced
    bool use_crypto_mixing = false;
    bool adhoc_corrections  = false;
    bool use_tie_breaking = true;
    unsigned long reseed_interval = 1000;

    // Discriminant options
    bool use_csv_discriminants = PRNGDefaults::use_csv_discriminants;

    // Performance and threading options
    bool use_parallel_filling = false; // Default to false for stability
    bool use_threading = true;         // Default to true for thread safety

    // Additional offset
    size_t offset = 0;

    // Debug
    bool debug = false;

    // Deterministic mode fields
    uint64_t seed = 0;          // Master seed for all randomness
    bool has_seed = false;      // Flag indicating if seed was explicitly set
    bool deterministic = false; // Force deterministic mode even without seed
};

} // namespace qiprng

#endif // QIPRNG_CONFIG_HPP