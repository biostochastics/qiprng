// File: rcpp_exports.cpp
// --------------------------------------------------------------
#include <Rcpp.h>
#include "enhanced_prng.hpp"
#include "prng_utils.hpp"
#include "ziggurat_normal.hpp"

using namespace Rcpp;
using namespace qiprng;

// [[Rcpp::export(".initialize_libsodium_")]]
void initialize_libsodium_() {
    static bool already_initialized = false;
    if (already_initialized) {
        return;
    }
    
    int ret = sodium_init();
    if (ret < 0) {
        // sodium_init() returns -1 on error, 0 on success, 1 if already initialized.
        throw std::runtime_error("Failed to initialize libsodium. The library may be unusable or insecure.");
    }
    
    // Set the global flag
    qiprng::sodium_initialized = true;
    already_initialized = true;
    qiprng::sodium_initialized_flag.store(true);
    
    Rcpp::Rcout << "Libsodium initialized successfully. Return code: " << ret << std::endl;
}

// Helper to parse Rcpp::List to PRNGConfig
PRNGConfig parsePRNGConfig(Rcpp::List rcfg) {
    PRNGConfig cfg;
    
    // Parse configuration from R list with type checking and defaults
    if (rcfg.containsElementNamed("a")) {
        SEXP a_sexp = rcfg["a"];
        if (TYPEOF(a_sexp) == INTSXP || TYPEOF(a_sexp) == REALSXP) {
            cfg.a = Rcpp::as<long>(a_sexp);
        }
    }
    
    if (rcfg.containsElementNamed("b")) {
        SEXP b_sexp = rcfg["b"];
        if (TYPEOF(b_sexp) == INTSXP || TYPEOF(b_sexp) == REALSXP) {
            cfg.b = Rcpp::as<long>(b_sexp);
        }
    }
    
    if (rcfg.containsElementNamed("c")) {
        SEXP c_sexp = rcfg["c"];
        if (TYPEOF(c_sexp) == INTSXP || TYPEOF(c_sexp) == REALSXP) {
            cfg.c = Rcpp::as<long>(c_sexp);
        }
    }
    
    if (rcfg.containsElementNamed("mpfr_precision")) {
        SEXP prec_sexp = rcfg["mpfr_precision"];
        if (TYPEOF(prec_sexp) == INTSXP || TYPEOF(prec_sexp) == REALSXP) {
            cfg.mpfr_precision = Rcpp::as<unsigned int>(prec_sexp);
        }
    }
    
    if (rcfg.containsElementNamed("buffer_size")) {
        SEXP buf_sexp = rcfg["buffer_size"];
        if (TYPEOF(buf_sexp) == INTSXP || TYPEOF(buf_sexp) == REALSXP) {
            cfg.buffer_size = Rcpp::as<size_t>(buf_sexp);
        }
    }
    
    if (rcfg.containsElementNamed("distribution")) {
        SEXP dist_sexp = rcfg["distribution"];
        if (TYPEOF(dist_sexp) == INTSXP) {
            int dist_int = Rcpp::as<int>(dist_sexp);
            if (dist_int >= 0 && dist_int <= 6) { // Check valid distribution enum range
                cfg.distribution = static_cast<PRNGConfig::Distribution>(dist_int);
            }
        } else if (TYPEOF(dist_sexp) == STRSXP) {
            std::string dist_str = Rcpp::as<std::string>(dist_sexp);
            if (dist_str == "uniform_01") cfg.distribution = PRNGConfig::UNIFORM_01;
            else if (dist_str == "uniform_range") cfg.distribution = PRNGConfig::UNIFORM_RANGE;
            else if (dist_str == "normal") cfg.distribution = PRNGConfig::NORMAL;
            else if (dist_str == "exponential") cfg.distribution = PRNGConfig::EXPONENTIAL;
            else if (dist_str == "poisson") cfg.distribution = PRNGConfig::POISSON;
            else if (dist_str == "gamma") cfg.distribution = PRNGConfig::GAMMA;
            else if (dist_str == "beta") cfg.distribution = PRNGConfig::BETA;
        }
    }
    
    if (rcfg.containsElementNamed("normal_method")) {
        SEXP method_sexp = rcfg["normal_method"];
        if (TYPEOF(method_sexp) == INTSXP) {
            int method_int = Rcpp::as<int>(method_sexp);
            if (method_int >= 0 && method_int <= 1) { // Check valid method enum range
                cfg.normal_method = static_cast<PRNGConfig::NormalMethod>(method_int);
            }
        } else if (TYPEOF(method_sexp) == STRSXP) {
            std::string method_str = Rcpp::as<std::string>(method_sexp);
            if (method_str == "box_muller") cfg.normal_method = PRNGConfig::BOX_MULLER;
            else if (method_str == "ziggurat") cfg.normal_method = PRNGConfig::ZIGGURAT;
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
        case PRNGConfig::UNIFORM_01: dist_str = "uniform_01"; break;
        case PRNGConfig::UNIFORM_RANGE: dist_str = "uniform_range"; break;
        case PRNGConfig::NORMAL: dist_str = "normal"; break;
        case PRNGConfig::EXPONENTIAL: dist_str = "exponential"; break;
        case PRNGConfig::POISSON: dist_str = "poisson"; break;
        case PRNGConfig::GAMMA: dist_str = "gamma"; break;
        case PRNGConfig::BETA: dist_str = "beta"; break;
        default: dist_str = "unknown"; break;
    }
    result["distribution"] = dist_str;
    
    // Convert normal method enum to string for R
    std::string method_str;
    switch (cfg.normal_method) {
        case PRNGConfig::BOX_MULLER: method_str = "box_muller"; break;
        case PRNGConfig::ZIGGURAT: method_str = "ziggurat"; break;
        default: method_str = "unknown"; break;
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
    
    return result;
}

// [[Rcpp::export(".createPRNG_")]]
void createPRNG_(Rcpp::List rcfg) {
    try {
        // First make sure libsodium is initialized
        if (!qiprng::sodium_initialized) {
            initialize_libsodium_();
        }
        
        PRNGConfig cfg = parsePRNGConfig(rcfg);
    
    // Validate critical parameters
    if (cfg.a == 0) {
        throw std::invalid_argument("Parameter 'a' cannot be 0 in quadratic irrational generator");
    }
    
    long long discriminant = static_cast<long long>(cfg.b) * cfg.b - 4LL * static_cast<long long>(cfg.a) * cfg.c;
    
    if (discriminant <= 0) {
        throw std::invalid_argument("Discriminant (b^2 - 4ac) must be positive");
    }
    
    // Make sure libsodium is initialized
    if (!qiprng::sodium_initialized) {
        Rcpp::stop("Libsodium not initialized before PRNG creation - this should not happen");
    }
    
    // Update global threading flag based on config
    g_use_threading = cfg.use_threading;
    
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
        if (!rcfg.containsElementNamed("a") && !rcfg.containsElementNamed("b") && !rcfg.containsElementNamed("c")) {
            cfg.a = a_val;
            cfg.b = b_val;
            cfg.c = c_val;
        }
        
        // Select quadratic irrationals based on precision and create thread-local PRNG
        int num_qis = cfg.mpfr_precision < 64 ? 2 : (cfg.mpfr_precision < 128 ? 3 : 5);
        std::vector<std::tuple<long, long, long>> abc_list = pickMultiQiSet(cfg.mpfr_precision, num_qis, 
                                                                           cfg.seed, cfg.has_seed);
        t_prng = std::make_unique<EnhancedPRNG>(cfg, abc_list);
        
        if (cfg.debug) {
            t_prng->dumpConfig();
        }
    } else {
        std::lock_guard<std::mutex> lock(g_prng_mutex);
        // Clean up any existing global PRNG
        g_prng.reset();
        
        // Select quadratic irrationals based on precision and create global PRNG
        int num_qis = cfg.mpfr_precision < 64 ? 2 : (cfg.mpfr_precision < 128 ? 3 : 5);
        std::vector<std::tuple<long, long, long>> abc_list = pickMultiQiSet(cfg.mpfr_precision, num_qis,
                                                                           cfg.seed, cfg.has_seed);
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
                if (rcfg.containsElementNamed("a")) new_cfg.a = cfg.a;
                if (rcfg.containsElementNamed("b")) new_cfg.b = cfg.b;
                if (rcfg.containsElementNamed("c")) new_cfg.c = cfg.c;
                // Ensure threading is enabled
                new_cfg.use_threading = true;
                
                // Get the MultiQI set for our PRNG
                int num_qis = new_cfg.mpfr_precision < 64 ? 2 : (new_cfg.mpfr_precision < 128 ? 3 : 5);
                std::vector<std::tuple<long, long, long>> abc_list = pickMultiQiSet(new_cfg.mpfr_precision, num_qis,
                                                                                   new_cfg.seed, new_cfg.has_seed);
                
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
                if (rcfg.containsElementNamed("a")) new_cfg.a = cfg.a;
                if (rcfg.containsElementNamed("b")) new_cfg.b = cfg.b;
                if (rcfg.containsElementNamed("c")) new_cfg.c = cfg.c;
                // Ensure threading is disabled
                new_cfg.use_threading = false;
                
                // Get the MultiQI set for our PRNG
                int num_qis = new_cfg.mpfr_precision < 64 ? 2 : (new_cfg.mpfr_precision < 128 ? 3 : 5);
                std::vector<std::tuple<long, long, long>> abc_list = pickMultiQiSet(new_cfg.mpfr_precision, num_qis,
                                                                                   new_cfg.seed, new_cfg.has_seed);
                
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
                        g_prng = nullptr;  // Use nullptr instead of reset() to avoid potential issues
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
    if (n <= 0) return;
    
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
    if (n <= 0) return;
    
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
    return qiprng::suppress_mpfr_warnings;
}

// [[Rcpp::export(".set_mpfr_warnings_")]]
void set_mpfr_warnings_(bool show_warnings) {
    qiprng::suppress_mpfr_warnings = !show_warnings;
}

// [[Rcpp::export(".cleanupPRNG_ThreadSafe_")]]
bool cleanupPRNG_ThreadSafe_() {
    try {
        // Properly cleanup resources in a thread-safe manner
        if (g_use_threading) {
            if (!t_prng) {
                Rcpp::warning("No active PRNG in current thread to clean up");
                return true; // Nothing to clean up is not an error
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
                return true; // Nothing to clean up is not an error
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
        cleanup_in_progress = false;
        
        return true;
    } catch (...) {
        // We don't even try to log errors in the final cleanup
        return false;
    }
}