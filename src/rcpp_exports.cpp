// File: rcpp_exports.cpp
// --------------------------------------------------------------
#include <Rcpp.h>

#include "enhanced_prng.hpp"
#include "multi_qi.hpp"            // For ThreadLocalPRNG and thread-local variables
#include "multi_qi_optimized.hpp"  // For MultiQIOptimized
#include "prng_utils.hpp"
#include "thread_manager.hpp"
#include "thread_pool.hpp"
#include "ziggurat_normal.hpp"

using namespace Rcpp;
using namespace qiprng;

// [[Rcpp::export(".initialize_libsodium_")]]
void initialize_libsodium_() {
    static std::atomic<bool> already_printed(false);

    try {
        qiprng::ensure_libsodium_initialized();

        // Only print the success message once
        bool expected = false;
        if (already_printed.compare_exchange_strong(expected, true)) {
            Rcpp::Rcout << "Libsodium initialized successfully." << std::endl;
        }
    } catch (const std::exception& e) {
        // Re-throw with R-friendly error message
        throw std::runtime_error(std::string("Failed to initialize libsodium: ") + e.what());
    }
}

// Helper function to validate PRNGConfig parameters
void validatePRNGConfig(const PRNGConfig& cfg) {
    // Validate discriminant (b^2 - 4ac > 0) using safe calculation
    long long disc;
    std::string error_msg;

    if (!safe_calculate_discriminant(cfg.a, cfg.b, cfg.c, disc, error_msg)) {
        throw std::invalid_argument("Invalid config: " + error_msg);
    }

    if (disc <= 0) {
        throw std::invalid_argument(
            "Invalid config: discriminant must be positive (b^2 - 4ac > 0)");
    }

    // Validate a is not zero
    if (cfg.a == 0) {
        throw std::invalid_argument("Invalid config: parameter 'a' cannot be zero");
    }

    // Validate buffer size with bounds
    if (cfg.buffer_size == 0 || cfg.buffer_size > 100000000) {  // Max 100MB of doubles
        throw std::invalid_argument("Invalid config: buffer_size must be between 1 and 100000000");
    }

    // Validate range parameters with bounds
    if (!std::isfinite(cfg.range_min) || !std::isfinite(cfg.range_max)) {
        throw std::invalid_argument("Invalid config: range values must be finite");
    }
    if (cfg.range_min >= cfg.range_max) {
        throw std::invalid_argument("Invalid config: range_min must be less than range_max");
    }
    if (std::abs(cfg.range_max - cfg.range_min) > 1e15) {  // Prevent excessive ranges
        throw std::invalid_argument("Invalid config: range too large");
    }

    // Validate distribution-specific parameters with bounds
    if (cfg.distribution == PRNGConfig::NORMAL) {
        if (!std::isfinite(cfg.normal_mean) || !std::isfinite(cfg.normal_sd)) {
            throw std::invalid_argument("Invalid config: normal parameters must be finite");
        }
        if (cfg.normal_sd <= 0 || cfg.normal_sd > 1e10) {
            throw std::invalid_argument(
                "Invalid config: normal_sd must be positive and reasonable");
        }
    }

    if (cfg.distribution == PRNGConfig::EXPONENTIAL) {
        if (!std::isfinite(cfg.exponential_lambda) || cfg.exponential_lambda <= 0 ||
            cfg.exponential_lambda > 1e10) {
            throw std::invalid_argument(
                "Invalid config: exponential_lambda must be positive and finite");
        }
    }

    if (cfg.distribution == PRNGConfig::POISSON) {
        if (!std::isfinite(cfg.poisson_lambda) || cfg.poisson_lambda <= 0 ||
            cfg.poisson_lambda > 1e10) {
            throw std::invalid_argument(
                "Invalid config: poisson_lambda must be positive and finite");
        }
    }

    if (cfg.distribution == PRNGConfig::GAMMA) {
        if (!std::isfinite(cfg.gamma_shape) || cfg.gamma_shape <= 0 || cfg.gamma_shape > 1e10) {
            throw std::invalid_argument("Invalid config: gamma_shape must be positive and finite");
        }
        if (!std::isfinite(cfg.gamma_scale) || cfg.gamma_scale <= 0 || cfg.gamma_scale > 1e10) {
            throw std::invalid_argument("Invalid config: gamma_scale must be positive and finite");
        }
    }

    if (cfg.distribution == PRNGConfig::BETA) {
        if (!std::isfinite(cfg.beta_alpha) || cfg.beta_alpha <= 0 || cfg.beta_alpha > 1e10) {
            throw std::invalid_argument("Invalid config: beta_alpha must be positive and finite");
        }
        if (!std::isfinite(cfg.beta_beta) || cfg.beta_beta <= 0 || cfg.beta_beta > 1e10) {
            throw std::invalid_argument("Invalid config: beta_beta must be positive and finite");
        }
    }

    // Prevent deterministic seed with ChaCha20 mixing (incompatible modes)
    if (cfg.deterministic && cfg.has_seed && cfg.use_crypto_mixing) {
        throw std::runtime_error(
            "Configuration error: Using deterministic seed with ChaCha20 mixing is not "
            "supported. Either disable crypto mixing (use_crypto_mixing=FALSE) or remove "
            "the seed for non-deterministic operation.");
    }

    // Validate MPFR precision
    // Note: Use practical maximum of 10000 bits instead of MPFR_PREC_MAX
    // since MPFR_PREC_MAX doesn't fit in unsigned int on most platforms
    constexpr unsigned int PRACTICAL_PREC_MAX = 10000;
    if (cfg.mpfr_precision < MPFR_PREC_MIN || cfg.mpfr_precision > PRACTICAL_PREC_MAX) {
        throw std::invalid_argument("Invalid config: mpfr_precision out of valid range (2-10000)");
    }
}

// Helper to parse Rcpp::List to PRNGConfig
PRNGConfig parsePRNGConfig(Rcpp::List rcfg) {
    PRNGConfig cfg;

    // Parse configuration from R list with type checking and defaults
    // Check for multi-QI vector configuration
    bool has_vector_params = false;

    if (rcfg.containsElementNamed("a")) {
        SEXP a_sexp = rcfg["a"];
        if (TYPEOF(a_sexp) == INTSXP || TYPEOF(a_sexp) == REALSXP) {
            if (Rf_length(a_sexp) > 1) {
                // Vector parameter for Multi-QI
                cfg.a_vec = Rcpp::as<std::vector<long>>(a_sexp);
                has_vector_params = true;
            } else {
                // Scalar parameter
                cfg.a = Rcpp::as<long>(a_sexp);
            }
        }
    }

    if (rcfg.containsElementNamed("b")) {
        SEXP b_sexp = rcfg["b"];
        if (TYPEOF(b_sexp) == INTSXP || TYPEOF(b_sexp) == REALSXP) {
            if (Rf_length(b_sexp) > 1) {
                // Vector parameter for Multi-QI
                cfg.b_vec = Rcpp::as<std::vector<long>>(b_sexp);
                has_vector_params = true;
            } else {
                // Scalar parameter
                cfg.b = Rcpp::as<long>(b_sexp);
            }
        }
    }

    if (rcfg.containsElementNamed("c")) {
        SEXP c_sexp = rcfg["c"];
        if (TYPEOF(c_sexp) == INTSXP || TYPEOF(c_sexp) == REALSXP) {
            if (Rf_length(c_sexp) > 1) {
                // Vector parameter for Multi-QI
                cfg.c_vec = Rcpp::as<std::vector<long>>(c_sexp);
                has_vector_params = true;
            } else {
                // Scalar parameter
                cfg.c = Rcpp::as<long>(c_sexp);
            }
        }
    }

    // If any vector parameters were found, set the flag
    if (has_vector_params) {
        cfg.has_manual_multi_qi = true;

        // Validate that all vectors have the same length
        size_t vec_size = 0;
        if (!cfg.a_vec.empty())
            vec_size = cfg.a_vec.size();
        else if (!cfg.b_vec.empty())
            vec_size = cfg.b_vec.size();
        else if (!cfg.c_vec.empty())
            vec_size = cfg.c_vec.size();

        if ((cfg.a_vec.size() != vec_size && !cfg.a_vec.empty()) ||
            (cfg.b_vec.size() != vec_size && !cfg.b_vec.empty()) ||
            (cfg.c_vec.size() != vec_size && !cfg.c_vec.empty())) {
            throw std::invalid_argument("Multi-QI vectors a, b, c must all have the same length");
        }

        // Set scalar values to first element for compatibility
        if (!cfg.a_vec.empty())
            cfg.a = cfg.a_vec[0];
        if (!cfg.b_vec.empty())
            cfg.b = cfg.b_vec[0];
        if (!cfg.c_vec.empty())
            cfg.c = cfg.c_vec[0];
    }

    if (rcfg.containsElementNamed("mpfr_precision")) {
        SEXP prec_sexp = rcfg["mpfr_precision"];
        if (TYPEOF(prec_sexp) == INTSXP || TYPEOF(prec_sexp) == REALSXP) {
            unsigned int prec = Rcpp::as<unsigned int>(prec_sexp);
            // SECURITY FIX: Validate MPFR precision bounds
            // Note: Use practical maximum of 10000 bits (matches validatePRNGConfig)
            constexpr unsigned int PRACTICAL_PREC_MAX = 10000;
            if (prec < MPFR_PREC_MIN || prec > PRACTICAL_PREC_MAX) {
                throw std::invalid_argument("Invalid mpfr_precision: must be between " +
                                            std::to_string(MPFR_PREC_MIN) + " and 10000");
            }
            cfg.mpfr_precision = prec;
        }
    }

    if (rcfg.containsElementNamed("buffer_size")) {
        SEXP buf_sexp = rcfg["buffer_size"];
        if (TYPEOF(buf_sexp) == INTSXP || TYPEOF(buf_sexp) == REALSXP) {
            double buf_val = Rcpp::as<double>(buf_sexp);
            // SECURITY FIX: Validate buffer size bounds
            if (buf_val <= 0 || buf_val > 100000000) {  // Max 100MB of doubles
                throw std::invalid_argument("Invalid buffer_size: must be between 1 and 100000000");
            }
            cfg.buffer_size = static_cast<size_t>(buf_val);
        }
    }

    if (rcfg.containsElementNamed("distribution")) {
        SEXP dist_sexp = rcfg["distribution"];
        if (TYPEOF(dist_sexp) == INTSXP) {
            int dist_int = Rcpp::as<int>(dist_sexp);
            if (dist_int >= 0 &&
                dist_int <= 13) {  // Check valid distribution enum range (14 distributions)
                cfg.distribution = static_cast<PRNGConfig::Distribution>(dist_int);
            }
        } else if (TYPEOF(dist_sexp) == STRSXP) {
            std::string dist_str = Rcpp::as<std::string>(dist_sexp);
            if (dist_str == "uniform_01")
                cfg.distribution = PRNGConfig::UNIFORM_01;
            else if (dist_str == "uniform_range")
                cfg.distribution = PRNGConfig::UNIFORM_RANGE;
            else if (dist_str == "normal")
                cfg.distribution = PRNGConfig::NORMAL;
            else if (dist_str == "exponential")
                cfg.distribution = PRNGConfig::EXPONENTIAL;
            else if (dist_str == "poisson")
                cfg.distribution = PRNGConfig::POISSON;
            else if (dist_str == "gamma")
                cfg.distribution = PRNGConfig::GAMMA;
            else if (dist_str == "beta")
                cfg.distribution = PRNGConfig::BETA;
            else if (dist_str == "bernoulli")
                cfg.distribution = PRNGConfig::BERNOULLI;
            else if (dist_str == "binomial")
                cfg.distribution = PRNGConfig::BINOMIAL;
            else if (dist_str == "lognormal")
                cfg.distribution = PRNGConfig::LOGNORMAL;
            else if (dist_str == "weibull")
                cfg.distribution = PRNGConfig::WEIBULL;
            else if (dist_str == "chisquared")
                cfg.distribution = PRNGConfig::CHISQUARED;
            else if (dist_str == "student_t")
                cfg.distribution = PRNGConfig::STUDENT_T;
            else if (dist_str == "negative_binomial")
                cfg.distribution = PRNGConfig::NEGATIVE_BINOMIAL;
        }
    }

    if (rcfg.containsElementNamed("normal_method")) {
        SEXP method_sexp = rcfg["normal_method"];
        if (TYPEOF(method_sexp) == INTSXP) {
            int method_int = Rcpp::as<int>(method_sexp);
            if (method_int >= 0 && method_int <= 1) {  // Check valid method enum range
                cfg.normal_method = static_cast<PRNGConfig::NormalMethod>(method_int);
            }
        } else if (TYPEOF(method_sexp) == STRSXP) {
            std::string method_str = Rcpp::as<std::string>(method_sexp);
            if (method_str == "box_muller")
                cfg.normal_method = PRNGConfig::BOX_MULLER;
            else if (method_str == "ziggurat")
                cfg.normal_method = PRNGConfig::ZIGGURAT;
        }
    }

    if (rcfg.containsElementNamed("range_min")) {
        SEXP min_sexp = rcfg["range_min"];
        if (TYPEOF(min_sexp) == REALSXP) {
            cfg.range_min = Rcpp::as<double>(min_sexp);
        }
    }

    if (rcfg.containsElementNamed("range_max")) {
        SEXP max_sexp = rcfg["range_max"];
        if (TYPEOF(max_sexp) == REALSXP) {
            cfg.range_max = Rcpp::as<double>(max_sexp);
        }
    }

    if (rcfg.containsElementNamed("normal_mean")) {
        SEXP mean_sexp = rcfg["normal_mean"];
        if (TYPEOF(mean_sexp) == REALSXP) {
            cfg.normal_mean = Rcpp::as<double>(mean_sexp);
        }
    }

    if (rcfg.containsElementNamed("normal_sd")) {
        SEXP sd_sexp = rcfg["normal_sd"];
        if (TYPEOF(sd_sexp) == REALSXP) {
            cfg.normal_sd = Rcpp::as<double>(sd_sexp);
        }
    }

    if (rcfg.containsElementNamed("exponential_lambda")) {
        SEXP lambda_sexp = rcfg["exponential_lambda"];
        if (TYPEOF(lambda_sexp) == REALSXP) {
            cfg.exponential_lambda = Rcpp::as<double>(lambda_sexp);
        }
    }

    // New distribution parameters
    if (rcfg.containsElementNamed("poisson_lambda")) {
        SEXP lambda_sexp = rcfg["poisson_lambda"];
        if (TYPEOF(lambda_sexp) == REALSXP) {
            cfg.poisson_lambda = Rcpp::as<double>(lambda_sexp);
        }
    }

    if (rcfg.containsElementNamed("gamma_shape")) {
        SEXP shape_sexp = rcfg["gamma_shape"];
        if (TYPEOF(shape_sexp) == REALSXP) {
            cfg.gamma_shape = Rcpp::as<double>(shape_sexp);
        }
    }

    if (rcfg.containsElementNamed("gamma_scale")) {
        SEXP scale_sexp = rcfg["gamma_scale"];
        if (TYPEOF(scale_sexp) == REALSXP) {
            cfg.gamma_scale = Rcpp::as<double>(scale_sexp);
        }
    }

    if (rcfg.containsElementNamed("beta_alpha")) {
        SEXP alpha_sexp = rcfg["beta_alpha"];
        if (TYPEOF(alpha_sexp) == REALSXP) {
            cfg.beta_alpha = Rcpp::as<double>(alpha_sexp);
        }
    }

    if (rcfg.containsElementNamed("beta_beta")) {
        SEXP beta_sexp = rcfg["beta_beta"];
        if (TYPEOF(beta_sexp) == REALSXP) {
            cfg.beta_beta = Rcpp::as<double>(beta_sexp);
        }
    }

    // New distribution parameters
    if (rcfg.containsElementNamed("bernoulli_p")) {
        SEXP p_sexp = rcfg["bernoulli_p"];
        if (TYPEOF(p_sexp) == REALSXP) {
            cfg.bernoulli_p = Rcpp::as<double>(p_sexp);
        }
    }

    if (rcfg.containsElementNamed("binomial_n")) {
        SEXP n_sexp = rcfg["binomial_n"];
        if (TYPEOF(n_sexp) == INTSXP || TYPEOF(n_sexp) == REALSXP) {
            cfg.binomial_n = Rcpp::as<int>(n_sexp);
        }
    }

    if (rcfg.containsElementNamed("binomial_p")) {
        SEXP p_sexp = rcfg["binomial_p"];
        if (TYPEOF(p_sexp) == REALSXP) {
            cfg.binomial_p = Rcpp::as<double>(p_sexp);
        }
    }

    if (rcfg.containsElementNamed("lognormal_mu")) {
        SEXP mu_sexp = rcfg["lognormal_mu"];
        if (TYPEOF(mu_sexp) == REALSXP) {
            cfg.lognormal_mu = Rcpp::as<double>(mu_sexp);
        }
    }

    if (rcfg.containsElementNamed("lognormal_sigma")) {
        SEXP sigma_sexp = rcfg["lognormal_sigma"];
        if (TYPEOF(sigma_sexp) == REALSXP) {
            cfg.lognormal_sigma = Rcpp::as<double>(sigma_sexp);
        }
    }

    if (rcfg.containsElementNamed("weibull_shape")) {
        SEXP shape_sexp = rcfg["weibull_shape"];
        if (TYPEOF(shape_sexp) == REALSXP) {
            cfg.weibull_shape = Rcpp::as<double>(shape_sexp);
        }
    }

    if (rcfg.containsElementNamed("weibull_scale")) {
        SEXP scale_sexp = rcfg["weibull_scale"];
        if (TYPEOF(scale_sexp) == REALSXP) {
            cfg.weibull_scale = Rcpp::as<double>(scale_sexp);
        }
    }

    if (rcfg.containsElementNamed("chisquared_df")) {
        SEXP df_sexp = rcfg["chisquared_df"];
        if (TYPEOF(df_sexp) == INTSXP || TYPEOF(df_sexp) == REALSXP) {
            cfg.chisquared_df = Rcpp::as<int>(df_sexp);
        }
    }

    if (rcfg.containsElementNamed("student_t_df")) {
        SEXP df_sexp = rcfg["student_t_df"];
        if (TYPEOF(df_sexp) == INTSXP || TYPEOF(df_sexp) == REALSXP) {
            cfg.student_t_df = Rcpp::as<int>(df_sexp);
        }
    }

    if (rcfg.containsElementNamed("negative_binomial_r")) {
        SEXP r_sexp = rcfg["negative_binomial_r"];
        if (TYPEOF(r_sexp) == INTSXP || TYPEOF(r_sexp) == REALSXP) {
            cfg.negative_binomial_r = Rcpp::as<int>(r_sexp);
        }
    }

    if (rcfg.containsElementNamed("negative_binomial_p")) {
        SEXP p_sexp = rcfg["negative_binomial_p"];
        if (TYPEOF(p_sexp) == REALSXP) {
            cfg.negative_binomial_p = Rcpp::as<double>(p_sexp);
        }
    }

    // Advanced options
    if (rcfg.containsElementNamed("use_crypto_mixing")) {
        SEXP crypto_sexp = rcfg["use_crypto_mixing"];
        if (TYPEOF(crypto_sexp) == LGLSXP) {
            cfg.use_crypto_mixing = Rcpp::as<bool>(crypto_sexp);
        }
    }

    if (rcfg.containsElementNamed("adhoc_corrections")) {
        SEXP adhoc_sexp = rcfg["adhoc_corrections"];
        if (TYPEOF(adhoc_sexp) == LGLSXP) {
            cfg.adhoc_corrections = Rcpp::as<bool>(adhoc_sexp);
        }
    }

    if (rcfg.containsElementNamed("use_tie_breaking")) {
        SEXP tie_sexp = rcfg["use_tie_breaking"];
        if (TYPEOF(tie_sexp) == LGLSXP) {
            cfg.use_tie_breaking = Rcpp::as<bool>(tie_sexp);
        }
    }

    if (rcfg.containsElementNamed("reseed_interval")) {
        SEXP interval_sexp = rcfg["reseed_interval"];
        if (TYPEOF(interval_sexp) == INTSXP || TYPEOF(interval_sexp) == REALSXP) {
            cfg.reseed_interval = Rcpp::as<unsigned long>(interval_sexp);
        }
    }

    if (rcfg.containsElementNamed("use_csv_discriminants")) {
        SEXP csv_sexp = rcfg["use_csv_discriminants"];
        if (TYPEOF(csv_sexp) == LGLSXP) {
            cfg.use_csv_discriminants = Rcpp::as<bool>(csv_sexp);
        }
    }

    if (rcfg.containsElementNamed("use_parallel_filling")) {
        SEXP parallel_sexp = rcfg["use_parallel_filling"];
        if (TYPEOF(parallel_sexp) == LGLSXP) {
            cfg.use_parallel_filling = Rcpp::as<bool>(parallel_sexp);
        }
    }

    if (rcfg.containsElementNamed("use_threading")) {
        SEXP threading_sexp = rcfg["use_threading"];
        if (TYPEOF(threading_sexp) == LGLSXP) {
            cfg.use_threading = Rcpp::as<bool>(threading_sexp);
        }
    }

    if (rcfg.containsElementNamed("offset")) {
        SEXP offset_sexp = rcfg["offset"];
        if (TYPEOF(offset_sexp) == INTSXP || TYPEOF(offset_sexp) == REALSXP) {
            cfg.offset = Rcpp::as<size_t>(offset_sexp);
        }
    }

    if (rcfg.containsElementNamed("debug")) {
        SEXP debug_sexp = rcfg["debug"];
        if (TYPEOF(debug_sexp) == LGLSXP) {
            cfg.debug = Rcpp::as<bool>(debug_sexp);
        }
    }

    if (rcfg.containsElementNamed("seed")) {
        SEXP seed_sexp = rcfg["seed"];
        if (TYPEOF(seed_sexp) == INTSXP || TYPEOF(seed_sexp) == REALSXP) {
            cfg.seed = Rcpp::as<uint64_t>(seed_sexp);
        }
    }

    if (rcfg.containsElementNamed("has_seed")) {
        SEXP has_seed_sexp = rcfg["has_seed"];
        if (TYPEOF(has_seed_sexp) == LGLSXP) {
            cfg.has_seed = Rcpp::as<bool>(has_seed_sexp);
        }
    }

    if (rcfg.containsElementNamed("deterministic")) {
        SEXP deterministic_sexp = rcfg["deterministic"];
        if (TYPEOF(deterministic_sexp) == LGLSXP) {
            cfg.deterministic = Rcpp::as<bool>(deterministic_sexp);
        }
    }

    // v0.5.0: Parse mixing strategy
    if (rcfg.containsElementNamed("mixing_strategy")) {
        SEXP strategy_sexp = rcfg["mixing_strategy"];
        if (TYPEOF(strategy_sexp) == STRSXP) {
            std::string strategy_str = Rcpp::as<std::string>(strategy_sexp);
            if (strategy_str == "round_robin")
                cfg.mixing_strategy = 0;
            else if (strategy_str == "xor_mix")
                cfg.mixing_strategy = 1;
            else if (strategy_str == "averaging")
                cfg.mixing_strategy = 2;
            else if (strategy_str == "modular_add")
                cfg.mixing_strategy = 3;
            else if (strategy_str == "cascade_mix")
                cfg.mixing_strategy = 4;
            // Default is already set to ROUND_ROBIN (0) in PRNGConfig
        }
    }

    // Validate the configuration before returning
    validatePRNGConfig(cfg);

    return cfg;
}

// Helper to convert PRNGConfig to Rcpp::List
Rcpp::List PRNGConfigToList(const PRNGConfig& cfg) {
    Rcpp::List result;

    result["a"] = cfg.a;
    result["b"] = cfg.b;
    result["c"] = cfg.c;
    result["mpfr_precision"] = cfg.mpfr_precision;
    result["buffer_size"] = static_cast<int>(cfg.buffer_size);

    // Convert distribution enum to string for R
    std::string dist_str;
    switch (cfg.distribution) {
        case PRNGConfig::UNIFORM_01:
            dist_str = "uniform_01";
            break;
        case PRNGConfig::UNIFORM_RANGE:
            dist_str = "uniform_range";
            break;
        case PRNGConfig::NORMAL:
            dist_str = "normal";
            break;
        case PRNGConfig::EXPONENTIAL:
            dist_str = "exponential";
            break;
        case PRNGConfig::POISSON:
            dist_str = "poisson";
            break;
        case PRNGConfig::GAMMA:
            dist_str = "gamma";
            break;
        case PRNGConfig::BETA:
            dist_str = "beta";
            break;
        case PRNGConfig::BERNOULLI:
            dist_str = "bernoulli";
            break;
        case PRNGConfig::BINOMIAL:
            dist_str = "binomial";
            break;
        case PRNGConfig::LOGNORMAL:
            dist_str = "lognormal";
            break;
        case PRNGConfig::WEIBULL:
            dist_str = "weibull";
            break;
        case PRNGConfig::CHISQUARED:
            dist_str = "chisquared";
            break;
        case PRNGConfig::STUDENT_T:
            dist_str = "student_t";
            break;
        case PRNGConfig::NEGATIVE_BINOMIAL:
            dist_str = "negative_binomial";
            break;
        default:
            dist_str = "unknown";
            break;
    }
    result["distribution"] = dist_str;

    // Convert normal method enum to string for R
    std::string method_str;
    switch (cfg.normal_method) {
        case PRNGConfig::BOX_MULLER:
            method_str = "box_muller";
            break;
        case PRNGConfig::ZIGGURAT:
            method_str = "ziggurat";
            break;
        default:
            method_str = "unknown";
            break;
    }
    result["normal_method"] = method_str;

    result["range_min"] = cfg.range_min;
    result["range_max"] = cfg.range_max;
    result["normal_mean"] = cfg.normal_mean;
    result["normal_sd"] = cfg.normal_sd;
    result["exponential_lambda"] = cfg.exponential_lambda;

    // New distribution parameters
    result["poisson_lambda"] = cfg.poisson_lambda;
    result["gamma_shape"] = cfg.gamma_shape;
    result["gamma_scale"] = cfg.gamma_scale;
    result["beta_alpha"] = cfg.beta_alpha;
    result["beta_beta"] = cfg.beta_beta;

    // Additional new distribution parameters
    result["bernoulli_p"] = cfg.bernoulli_p;
    result["binomial_n"] = cfg.binomial_n;
    result["binomial_p"] = cfg.binomial_p;
    result["lognormal_mu"] = cfg.lognormal_mu;
    result["lognormal_sigma"] = cfg.lognormal_sigma;
    result["weibull_shape"] = cfg.weibull_shape;
    result["weibull_scale"] = cfg.weibull_scale;
    result["chisquared_df"] = cfg.chisquared_df;
    result["student_t_df"] = cfg.student_t_df;
    result["negative_binomial_r"] = cfg.negative_binomial_r;
    result["negative_binomial_p"] = cfg.negative_binomial_p;

    // Advanced settings
    result["use_crypto_mixing"] = cfg.use_crypto_mixing;
    result["adhoc_corrections"] = cfg.adhoc_corrections;
    result["use_tie_breaking"] = cfg.use_tie_breaking;
    result["reseed_interval"] = static_cast<int>(cfg.reseed_interval);
    result["use_csv_discriminants"] = cfg.use_csv_discriminants;
    result["use_parallel_filling"] = cfg.use_parallel_filling;
    result["use_threading"] = cfg.use_threading;
    result["offset"] = static_cast<int>(cfg.offset);
    result["debug"] = cfg.debug;
    result["seed"] = static_cast<double>(cfg.seed);
    result["has_seed"] = cfg.has_seed;
    result["deterministic"] = cfg.deterministic;

    // v0.5.0: Convert mixing strategy to string
    std::string mix_str;
    switch (cfg.mixing_strategy) {
        case 0:
            mix_str = "round_robin";
            break;
        case 1:
            mix_str = "xor_mix";
            break;
        case 2:
            mix_str = "averaging";
            break;
        case 3:
            mix_str = "modular_add";
            break;
        case 4:
            mix_str = "cascade_mix";
            break;
        default:
            mix_str = "round_robin";
            break;
    }
    result["mixing_strategy"] = mix_str;

    return result;
}

// [[Rcpp::export(".createPRNG_")]]
void createPRNG_(Rcpp::List rcfg) {
    try {
        // Ensure libsodium is initialized (idempotent - safe to call multiple times)
        qiprng::ensure_libsodium_initialized();

        PRNGConfig cfg = parsePRNGConfig(rcfg);

        // Validate critical parameters (only for non-vector mode)
        if (!cfg.has_manual_multi_qi) {
            if (cfg.a == 0) {
                throw std::invalid_argument(
                    "Parameter 'a' cannot be 0 in quadratic irrational generator");
            }

            // Use safe discriminant calculation
            long long discriminant;
            std::string error_msg;

            if (!safe_calculate_discriminant(cfg.a, cfg.b, cfg.c, discriminant, error_msg)) {
                throw std::invalid_argument(error_msg);
            }

            if (discriminant <= 0) {
                throw std::invalid_argument("Discriminant (b^2 - 4ac) must be positive");
            }
        }

        // Make sure libsodium is initialized
        if (!qiprng::sodium_initialized) {
            Rcpp::stop("Libsodium not initialized before PRNG creation - this should not happen");
        }

        // Update global threading flag based on config
        g_use_threading = cfg.use_threading;

        // Clear used discriminants when creating a new PRNG with a seed
        // This ensures deterministic discriminant selection
        if (cfg.has_seed) {
            std::lock_guard<std::mutex> disc_lock(g_disc_mutex);
            g_used_discriminants.clear();
            // Reset the deterministic engine to ensure it starts fresh
            qiprng::resetDeterministicEngine();
            // Reset MultiQIOptimized thread counter for deterministic behavior
            MultiQIOptimized::reset_thread_counter();
        }

        // Clear thread-local caches from multi_qi.cpp to ensure deterministic behavior
        qiprng::tl_cache.clear();
        qiprng::tl_cache_pos = 0;

        // Reset the fallback generator to ensure determinism
        if (cfg.has_seed) {
            qiprng::tl_fallback.set_deterministic(true, cfg.seed);
        } else {
            qiprng::tl_fallback.set_deterministic(false, 0);
        }

        if (g_use_threading) {
            // Clean up any existing thread-local PRNG
            t_prng.reset();

            // For thread safety, we choose a slightly different a,b,c for each thread
            long long disc = chooseUniqueDiscriminant();

            // Use std::tie instead of C++17 structured bindings
            std::tuple<long, long, long> abc_vals = makeABCfromDelta(disc);
            long a_val = std::get<0>(abc_vals);
            long b_val = std::get<1>(abc_vals);
            long c_val = std::get<2>(abc_vals);

            // Override with thread-specific a,b,c unless user explicitly provided them
            if (!rcfg.containsElementNamed("a") && !rcfg.containsElementNamed("b") &&
                !rcfg.containsElementNamed("c")) {
                cfg.a = a_val;
                cfg.b = b_val;
                cfg.c = c_val;
            }

            // Select quadratic irrationals or use manual Multi-QI configuration
            std::vector<std::tuple<long, long, long>> abc_list;

            if (cfg.has_manual_multi_qi) {
                // Use manually specified Multi-QI vectors
                size_t num_qis = cfg.a_vec.size();
                for (size_t i = 0; i < num_qis; ++i) {
                    long a_val = cfg.a_vec[i];
                    long b_val = cfg.b_vec[i];
                    long c_val = cfg.c_vec[i];

                    // Validate discriminant for each QI
                    long long disc;
                    std::string err_msg;
                    if (!safe_calculate_discriminant(a_val, b_val, c_val, disc, err_msg)) {
                        throw std::invalid_argument("Multi-QI validation error for QI " +
                                                    std::to_string(i + 1) + ": " + err_msg);
                    }
                    if (disc <= 0) {
                        throw std::invalid_argument(
                            "Multi-QI discriminant must be positive for QI " +
                            std::to_string(i + 1));
                    }

                    abc_list.push_back(std::make_tuple(a_val, b_val, c_val));
                }
            } else {
                // Use automatic Multi-QI selection
                int num_qis = cfg.mpfr_precision < 64 ? 2 : (cfg.mpfr_precision < 128 ? 3 : 5);
                abc_list = pickMultiQiSet(cfg.mpfr_precision, num_qis, cfg.seed, cfg.has_seed);
            }

            t_prng = std::make_unique<EnhancedPRNG>(cfg, abc_list);

            if (cfg.debug) {
                t_prng->dumpConfig();
            }
        } else {
            std::lock_guard<std::mutex> lock(g_prng_mutex);
            // Clean up any existing global PRNG
            g_prng.reset();

            // Select quadratic irrationals or use manual Multi-QI configuration
            std::vector<std::tuple<long, long, long>> abc_list;

            if (cfg.has_manual_multi_qi) {
                // Use manually specified Multi-QI vectors
                size_t num_qis = cfg.a_vec.size();
                for (size_t i = 0; i < num_qis; ++i) {
                    long a_val = cfg.a_vec[i];
                    long b_val = cfg.b_vec[i];
                    long c_val = cfg.c_vec[i];

                    // Validate discriminant for each QI
                    long long disc;
                    std::string err_msg;
                    if (!safe_calculate_discriminant(a_val, b_val, c_val, disc, err_msg)) {
                        throw std::invalid_argument("Multi-QI validation error for QI " +
                                                    std::to_string(i + 1) + ": " + err_msg);
                    }
                    if (disc <= 0) {
                        throw std::invalid_argument(
                            "Multi-QI discriminant must be positive for QI " +
                            std::to_string(i + 1));
                    }

                    abc_list.push_back(std::make_tuple(a_val, b_val, c_val));
                }
            } else {
                // Use automatic Multi-QI selection
                int num_qis = cfg.mpfr_precision < 64 ? 2 : (cfg.mpfr_precision < 128 ? 3 : 5);
                abc_list = pickMultiQiSet(cfg.mpfr_precision, num_qis, cfg.seed, cfg.has_seed);
            }

            g_prng = std::make_unique<EnhancedPRNG>(cfg, abc_list);

            if (cfg.debug) {
                g_prng->dumpConfig();
            }
        }

    } catch (const std::exception& e) {
        // Just rethrow for R error handling
        throw;
    } catch (...) {
        // Just rethrow any unknown exceptions
        throw;
    }
}

// [[Rcpp::export(".updatePRNG_")]]
void updatePRNG_(Rcpp::List rcfg) {
    PRNGConfig cfg = parsePRNGConfig(rcfg);

    // Check if threading mode has changed
    bool was_threading = g_use_threading;
    g_use_threading = cfg.use_threading;

    // If threading mode changed, handle appropriately
    if (was_threading != g_use_threading) {
        // If switching from non-threading to threading
        if (g_use_threading) {
            // Create a new thread-local PRNG based on the global one if possible
            if (g_prng) {
                std::lock_guard<std::mutex> lock(g_prng_mutex);
                // Copy the global PRNG config and update with our specific changes
                PRNGConfig new_cfg = g_prng->getConfig();
                // Apply changes from the provided config
                if (rcfg.containsElementNamed("a"))
                    new_cfg.a = cfg.a;
                if (rcfg.containsElementNamed("b"))
                    new_cfg.b = cfg.b;
                if (rcfg.containsElementNamed("c"))
                    new_cfg.c = cfg.c;
                // Ensure threading is enabled
                new_cfg.use_threading = true;

                // Get the MultiQI set for our PRNG
                int num_qis =
                    new_cfg.mpfr_precision < 64 ? 2 : (new_cfg.mpfr_precision < 128 ? 3 : 5);
                std::vector<std::tuple<long, long, long>> abc_list =
                    pickMultiQiSet(new_cfg.mpfr_precision, num_qis, new_cfg.seed, new_cfg.has_seed);

                // Create the thread-local PRNG
                t_prng = std::make_unique<EnhancedPRNG>(new_cfg, abc_list);
            }
            // Otherwise the thread-local PRNG will be created on the next call to createPRNG_
        }
        // If switching from threading to non-threading
        else {
            // Create a new global PRNG based on the thread-local one if possible
            if (t_prng) {
                // Copy the thread-local PRNG config and update with our specific changes
                PRNGConfig new_cfg = t_prng->getConfig();
                // Apply changes from the provided config
                if (rcfg.containsElementNamed("a"))
                    new_cfg.a = cfg.a;
                if (rcfg.containsElementNamed("b"))
                    new_cfg.b = cfg.b;
                if (rcfg.containsElementNamed("c"))
                    new_cfg.c = cfg.c;
                // Ensure threading is disabled
                new_cfg.use_threading = false;

                // Get the MultiQI set for our PRNG
                int num_qis =
                    new_cfg.mpfr_precision < 64 ? 2 : (new_cfg.mpfr_precision < 128 ? 3 : 5);
                std::vector<std::tuple<long, long, long>> abc_list =
                    pickMultiQiSet(new_cfg.mpfr_precision, num_qis, new_cfg.seed, new_cfg.has_seed);

                // Create the global PRNG
                std::lock_guard<std::mutex> lock(g_prng_mutex);
                g_prng = std::make_unique<EnhancedPRNG>(new_cfg, abc_list);
            }
            // Otherwise the global PRNG will be created on the next call to createPRNG_
        }
    }

    // Update the appropriate PRNG with the new config
    if (g_use_threading) {
        if (!t_prng) {
            throw std::runtime_error("Cannot update PRNG: No active PRNG in current thread");
        }
        t_prng->updateConfig(cfg);
    } else {
        std::lock_guard<std::mutex> lock(g_prng_mutex);
        if (!g_prng) {
            throw std::runtime_error("Cannot update PRNG: No active global PRNG");
        }
        g_prng->updateConfig(cfg);
    }
}

// [[Rcpp::export(".getPRNGConfig_")]]
Rcpp::List getPRNGConfig_() {
    if (g_use_threading) {
        if (!t_prng) {
            throw std::runtime_error("Cannot get config: No active PRNG in current thread");
        }
        const PRNGConfig& cfg = t_prng->getConfig();
        return PRNGConfigToList(cfg);
    } else {
        std::lock_guard<std::mutex> lock(g_prng_mutex);
        if (!g_prng) {
            throw std::runtime_error("Cannot get config: No active global PRNG");
        }
        const PRNGConfig& cfg = g_prng->getConfig();
        return PRNGConfigToList(cfg);
    }
}

// [[Rcpp::export(".dumpPRNGConfig_")]]
void dumpPRNGConfig_() {
    if (g_use_threading) {
        if (!t_prng) {
            throw std::runtime_error("Cannot dump config: No active PRNG in current thread");
        }
        t_prng->dumpConfig();
    } else {
        std::lock_guard<std::mutex> lock(g_prng_mutex);
        if (!g_prng) {
            throw std::runtime_error("Cannot dump config: No active global PRNG");
        }
        g_prng->dumpConfig();
    }
}

// [[Rcpp::export(".generatePRNG_")]]
Rcpp::NumericVector generatePRNG_(int n) {
    if (n <= 0) {
        return Rcpp::NumericVector(0);
    }

    Rcpp::NumericVector result(n);

    if (g_use_threading) {
        if (!t_prng) {
            throw std::runtime_error("Cannot generate: No active PRNG in current thread");
        }
        t_prng->generate_n(result);
    } else {
        std::lock_guard<std::mutex> lock(g_prng_mutex);
        if (!g_prng) {
            throw std::runtime_error("Cannot generate: No active global PRNG");
        }
        g_prng->generate_n(result);
    }

    return result;
}

// [[Rcpp::export(".reseedPRNG_")]]
void reseedPRNG_() {
    if (g_use_threading) {
        if (!t_prng) {
            throw std::runtime_error("Cannot reseed: No active PRNG in current thread");
        }
        t_prng->reseed();
    } else {
        std::lock_guard<std::mutex> lock(g_prng_mutex);
        if (!g_prng) {
            throw std::runtime_error("Cannot reseed: No active global PRNG");
        }
        g_prng->reseed();
    }
}

// Flag to prevent multiple cleanups
static std::atomic<bool> cleanup_in_progress(false);

// [[Rcpp::export(".cleanup_prng_")]]
void cleanup_prng_() {
    // Safeguard against recursive or reentrant calls using compare_exchange
    // This is more robust than a simple exchange
    bool expected = false;
    if (!cleanup_in_progress.compare_exchange_strong(expected, true, std::memory_order_acq_rel)) {
        // Cleanup already in progress, just return
        Rcpp::Rcout << "Cleanup already in progress, skipping duplicate call" << std::endl;
        return;
    }

    try {
        // Mark any thread-local ZigguratTLSManager instances as exiting
        try {
            ZigguratTLSManager::mark_thread_exiting();
        } catch (...) {
            // Ignore any errors - this is just a best effort
        }

        // First, prepare ZigguratNormal for shutdown (before we destroy any PRNGs)
        // This prevents segfaults from trying to access thread-local resources
        ZigguratNormal::prepare_for_shutdown();

        // Call the EnhancedPRNG cleanup method to handle any thread-local resources
        EnhancedPRNG::cleanupAllThreadResources();

        // Disable threading mode
        bool was_threading = g_use_threading;
        g_use_threading = false;

        try {
            // Clear the PRNGs safely with multiple safeguards
            if (was_threading) {
                if (t_prng) {
                    // Disable thread-safe mode first to prevent new resource access
                    t_prng->prepareForCleanup();
                    t_prng = nullptr;  // Use nullptr instead of reset() to avoid potential issues
                }
            } else {
                try {
                    std::lock_guard<std::mutex> lock(g_prng_mutex);
                    if (g_prng) {
                        // Disable thread-safe mode first to prevent new resource access
                        g_prng->prepareForCleanup();
                        g_prng =
                            nullptr;  // Use nullptr instead of reset() to avoid potential issues
                    }
                } catch (...) {
                    // If mutex locking fails, try a more aggressive approach
                    g_prng = nullptr;
                }
            }
        } catch (...) {
            // Suppress any exceptions during PRNG cleanup
            Rcpp::Rcout << "Exception during PRNG pointer cleanup - suppressed" << std::endl;
        }

        // Clean up ZigguratNormal resources one more time
        try {
            ZigguratNormal::cleanup_thread_local_resources();
        } catch (...) {
            // Suppress any exceptions during ziggurat cleanup
            Rcpp::Rcout << "Exception during ziggurat cleanup - suppressed" << std::endl;
        }

        // Reset flag after successful cleanup
        cleanup_in_progress.store(false, std::memory_order_release);
        Rcpp::Rcout << "PRNG cleanup completed successfully" << std::endl;
    } catch (...) {
        // Always suppress exceptions in cleanup functions
        cleanup_in_progress.store(false, std::memory_order_release);  // Reset flag even on error
        Rcpp::warning("Exception occurred during PRNG cleanup");
    }
}

// [[Rcpp::export(".skipPRNG_")]]
void skipPRNG_(int n) {
    if (n <= 0)
        return;

    if (g_use_threading) {
        if (!t_prng) {
            throw std::runtime_error("Cannot skip: No active PRNG in current thread");
        }
        t_prng->skip(static_cast<uint64_t>(n));
    } else {
        std::lock_guard<std::mutex> lock(g_prng_mutex);
        if (!g_prng) {
            throw std::runtime_error("Cannot skip: No active global PRNG");
        }
        g_prng->skip(static_cast<uint64_t>(n));
    }
}

// [[Rcpp::export(".jumpAheadPRNG_")]]
void jumpAheadPRNG_(double n) {
    if (n <= 0)
        return;

    if (g_use_threading) {
        if (!t_prng) {
            throw std::runtime_error("Cannot jump ahead: No active PRNG in current thread");
        }
        t_prng->skip(static_cast<uint64_t>(n));
    } else {
        std::lock_guard<std::mutex> lock(g_prng_mutex);
        if (!g_prng) {
            throw std::runtime_error("Cannot jump ahead: No active global PRNG");
        }
        g_prng->skip(static_cast<uint64_t>(n));
    }
}

// [[Rcpp::export(".suppress_mpfr_warnings_")]]
bool suppress_mpfr_warnings_() {
    return qiprng::suppress_mpfr_warnings.load();
}

// [[Rcpp::export(".set_mpfr_warnings_")]]
void set_mpfr_warnings_(bool show_warnings) {
    qiprng::suppress_mpfr_warnings.store(!show_warnings);
}

// [[Rcpp::export(".cleanupPRNG_ThreadSafe_")]]
bool cleanupPRNG_ThreadSafe_() {
    try {
        // Properly cleanup resources in a thread-safe manner
        if (g_use_threading) {
            if (!t_prng) {
                Rcpp::warning("No active PRNG in current thread to clean up");
                return true;  // Nothing to clean up is not an error
            }

            // Prepare for cleanup (disable thread-safe mode, etc)
            t_prng->prepareForCleanup();

            // Perform actual cleanup
            t_prng->performCleanup();

            // Clear the pointer
            t_prng = nullptr;
        } else {
            std::lock_guard<std::mutex> lock(g_prng_mutex);
            if (!g_prng) {
                Rcpp::warning("No active global PRNG to clean up");
                return true;  // Nothing to clean up is not an error
            }

            // Prepare for cleanup (disable thread-safe mode, etc)
            g_prng->prepareForCleanup();

            // Perform actual cleanup
            g_prng->performCleanup();

            // Clear the pointer
            g_prng = nullptr;
        }

        // Clean up all thread resources, not just the current thread
        EnhancedPRNG::cleanupAllThreadResources();

        return true;
    } catch (const std::exception& e) {
        Rcpp::warning("Error during thread-safe PRNG cleanup: %s", e.what());
        return false;
    } catch (...) {
        Rcpp::warning("Unknown error during thread-safe PRNG cleanup");
        return false;
    }
}

// [[Rcpp::export(".cleanupPRNG_Final_")]]
bool cleanupPRNG_Final_() {
    try {
        // This is the final cleanup that should be called after all other cleanups

        // Clear thread-local and global PRNG pointers (don't invoke destructors)
        t_prng = nullptr;
        {
            std::lock_guard<std::mutex> lock(g_prng_mutex);
            g_prng = nullptr;
        }

        // Clean up all thread resources one more time
        EnhancedPRNG::cleanupAllThreadResources();

        // Reset global flags
        g_use_threading = false;
        cleanup_in_progress.store(false, std::memory_order_release);

        return true;
    } catch (...) {
        // We don't even try to log errors in the final cleanup
        return false;
    }
}

// [[Rcpp::export(".shutdown_thread_pool_")]]
void shutdown_thread_pool_() {
    // Safely shut down the global thread pool
    // This should be called from R's .onUnload before library unload
    try {
        qiprng::shutdown_global_thread_pool();
    } catch (...) {
        // Silently ignore errors during shutdown
    }
}

// [[Rcpp::export(".prepare_for_unload_")]]
void prepare_for_unload_() {
    // Comprehensive cleanup before R unloads the shared library
    // This function MUST be called from .onUnload BEFORE library.dynam.unload()
    // It ensures all resources are cleaned up in the correct order to prevent
    // segfaults from destructor ordering issues.

    try {
        // Step 1: Mark shutdown as in progress to prevent new resource access
        qiprng::mark_shutdown_started();

        // Step 2: Mark Ziggurat TLS managers as exiting
        try {
            ZigguratTLSManager::mark_thread_exiting();
        } catch (...) {
        }

        // Step 3: Prepare ZigguratNormal for shutdown
        try {
            ZigguratNormal::prepare_for_shutdown();
        } catch (...) {
        }

        // Step 4: Clean up EnhancedPRNG thread resources
        try {
            EnhancedPRNG::cleanupAllThreadResources();
        } catch (...) {
        }

        // Step 4.5: v0.7.3: Clean up all registered thread resources via ThreadManager
        // This invokes cleanup callbacks registered by any threads that used the PRNG
        try {
            ThreadManager::cleanupAllThreads();
        } catch (...) {
        }

        // Step 5: Clean up MultiQIOptimized thread-local data
        try {
            MultiQIOptimized::cleanup_thread_local_data();
        } catch (...) {
        }

        // Step 6: Clean up Ziggurat thread-local resources
        try {
            ZigguratNormal::cleanup_thread_local_resources();
        } catch (...) {
        }

        // Step 7: Clear PRNG pointers (this destroys the PRNG objects)
        try {
            t_prng = nullptr;
            {
                std::lock_guard<std::mutex> lock(g_prng_mutex);
                g_prng = nullptr;
            }
        } catch (...) {
        }

        // Step 8: Shut down thread pool
        try {
            qiprng::shutdown_global_thread_pool();
        } catch (...) {
        }

        // Step 9: Clean up MPFR global cache (must be done after all MPFR users are done)
        try {
            mpfr_free_cache2(MPFR_FREE_GLOBAL_CACHE);
        } catch (...) {
        }

        // Step 10: Reset flags
        g_use_threading = false;
        // v0.7.3: Keep cleanup marked as in progress to prevent any further cleanup
        // entry during unload - don't reset to false as that would re-enable cleanup
        cleanup_in_progress.store(true, std::memory_order_release);

        // Note: We intentionally do NOT call mark_shutdown_complete() here
        // because the library is about to be unloaded. Keeping the shutdown
        // flag set prevents any late access to resources.

    } catch (...) {
        // Suppress all exceptions during unload preparation
    }
}
