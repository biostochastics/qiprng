// File: prng_config.hpp
// --------------------------------------------------------------
#ifndef QIPRNG_CONFIG_HPP
#define QIPRNG_CONFIG_HPP

#include "prng_common.hpp"  // For PRNGDefaults

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
        NEGATIVE_BINOMIAL,
        // v0.5.0: Extended distributions
        LEVY_STABLE,
        PARETO,
        CAUCHY,
        MULTIVARIATE_NORMAL
    };

    enum NormalMethod { BOX_MULLER = PRNGDefaults::BOX_MULLER, ZIGGURAT = PRNGDefaults::ZIGGURAT };
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
    // NOTE: Defaults aligned with R interface (R/prng_interface.R default_config)
    bool use_crypto_mixing = true;  // Default to true for cryptographic security
    bool adhoc_corrections = false;
    bool use_tie_breaking = true;
    unsigned long reseed_interval = 1000;

    // Discriminant options
    bool use_csv_discriminants = true;  // Default to true for higher quality discriminants

    // Performance and threading options
    bool use_parallel_filling = false;  // Default to false for stability
    bool use_threading = false;         // Default to false for simplicity (matches R)

    // Additional offset
    size_t offset = 0;

    // Debug
    bool debug = false;

    // Deterministic mode fields
    uint64_t seed = 0;           // Master seed for all randomness
    bool has_seed = false;       // Flag indicating if seed was explicitly set
    bool deterministic = false;  // Force deterministic mode even without seed

    // v0.5.0: MultiQI mixing strategy
    int mixing_strategy = 0;  // 0=ROUND_ROBIN, 1=XOR_MIX, 2=AVERAGING, 3=MODULAR_ADD, 4=CASCADE_MIX

    // v0.5.0: Extended distribution parameters
    double levy_alpha = 1.5;  // Stability parameter (0,2]
    double levy_beta = 0.0;   // Skewness parameter [-1,1]
    double levy_mu = 0.0;     // Location parameter
    double levy_sigma = 1.0;  // Scale parameter

    double pareto_xm = 1.0;     // Scale parameter (minimum value)
    double pareto_alpha = 1.0;  // Shape parameter

    double cauchy_location = 0.0;  // Location parameter
    double cauchy_scale = 1.0;     // Scale parameter

    // Multivariate normal uses separate interface

    // Multi-QI manual configuration support
    std::vector<long> a_vec;           // Vector of a values for manual Multi-QI
    std::vector<long> b_vec;           // Vector of b values for manual Multi-QI
    std::vector<long> c_vec;           // Vector of c values for manual Multi-QI
    bool has_manual_multi_qi = false;  // Flag indicating manual Multi-QI configuration
};

}  // namespace qiprng

#endif  // QIPRNG_CONFIG_HPP
