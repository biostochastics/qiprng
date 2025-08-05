// File: prng_utils.cpp
// --------------------------------------------------------------
#include "prng_utils.hpp"
#include "enhanced_prng.hpp" // For accessing g_prng->getConfig(), t_prng->getConfig()
#include "deterministic_rng.hpp" // For DeterministicRNGFactory
#include <sodium.h>
#include <fstream>
#include <sstream>
#include <algorithm> // For std::min, std::shuffle, std::abs
#include <set>       // For std::set in chooseUniqueDiscriminant
#include <chrono>    // For std::chrono in chooseUniqueDiscriminant fallback
#include <numeric>   // For std::iota
#include <limits>    // For std::numeric_limits

// Define globals that were declared extern in prng_utils.hpp
namespace qiprng {

// LibSodium initialization
std::atomic<bool> sodium_initialized_flag(false); // Definition
static std::once_flag sodium_init_flag;

// Global PRNG state
bool g_use_threading = false; // Default to false
std::mutex g_prng_mutex;
thread_local std::unique_ptr<qiprng::EnhancedPRNG> t_prng = nullptr;
std::unique_ptr<qiprng::EnhancedPRNG> g_prng = nullptr;

// Discriminant utilities
std::mutex g_disc_mutex;
std::unordered_set<long long> g_used_discriminants;

// CSV Discriminant utilities
std::vector<std::tuple<long, long, long, long long>> g_csv_discriminants;
std::mutex g_csv_disc_mutex;
bool g_csv_discriminants_loaded = false;

// This ensures the global `sodium_initialized` used by CryptoMixer is this one.
bool sodium_initialized = false; // Will be set by Rcpp export `initialize_libsodium_`

} // namespace qiprng

// LibSodium initialization
void qiprng::initialize_libsodium_if_needed() {
    std::call_once(sodium_init_flag, []() {
        if (sodium_init() < 0) {
            Rcpp::warning("Failed to initialize libsodium. Cryptographic operations may fail or be insecure.");
            sodium_initialized_flag.store(false);
        } else {
            sodium_initialized_flag.store(true);
        }
    });
}

// Thread-local random engine
std::mt19937_64& qiprng::getThreadLocalEngine() {
    static thread_local std::unique_ptr<std::mt19937_64> t_engine_ptr;
    if (!t_engine_ptr) {
        std::random_device rd;
        std::array<std::uint32_t, std::mt19937_64::state_size / 2> seeds; // Correct seeding
        for(size_t i = 0; i < seeds.size(); ++i) seeds[i] = rd();
        
        // Incorporate thread ID for better seed diversity across threads
        auto tid_hash = std::hash<std::thread::id>()(std::this_thread::get_id());
        seeds[0] ^= static_cast<uint32_t>(tid_hash & 0xFFFFFFFFULL);
        if (seeds.size() > 1) {
             seeds[1] ^= static_cast<uint32_t>((tid_hash >> 32) & 0xFFFFFFFFULL);
        }
        std::seed_seq seq(seeds.begin(), seeds.end());
        t_engine_ptr = std::make_unique<std::mt19937_64>(seq);
    }
    return *t_engine_ptr;
}

// Deterministic thread-local random engine
std::mt19937_64& qiprng::getDeterministicThreadLocalEngine(uint64_t seed) {
    static thread_local std::unique_ptr<std::mt19937_64> t_det_engine_ptr;
    static thread_local uint64_t t_last_seed = 0;

    if (!t_det_engine_ptr || t_last_seed != seed) {
        // Get unique thread ID
        std::hash<std::thread::id> hasher;
        size_t thread_id = hasher(std::this_thread::get_id());

        // Create deterministic RNG for this thread
        t_det_engine_ptr = std::make_unique<std::mt19937_64>(
            DeterministicRNGFactory::create_for_thread(seed, thread_id)
        );
        t_last_seed = seed;
    }

    return *t_det_engine_ptr;
}

// Multi-QI parameter set selection
std::vector<std::tuple<long, long, long>> qiprng::pickMultiQiSet(int precision, int count, 
                                                                 uint64_t seed, bool has_seed) {
    std::vector<std::tuple<long, long, long>> result;
    
    // First, try to load and use excellent discriminants from CSV
    loadCSVDiscriminants();
    
    std::vector<std::tuple<long, long, long>> baseParams;
    
    // If CSV discriminants are loaded, use them; otherwise fallback to hardcoded values
    if (g_csv_discriminants_loaded && !g_csv_discriminants.empty()) {
        // Convert CSV discriminants (a,b,c,discriminant) to baseParams (a,b,c)
        for (const auto& disc : g_csv_discriminants) {
            baseParams.emplace_back(std::get<0>(disc), std::get<1>(disc), std::get<2>(disc));
        }
    } else {
        // Fallback to hardcoded excellent discriminants if CSV loading fails
        baseParams = {
            {1, 1, -1}, {2, 3, -1}, {1, 2, -1}, {1, 3, -2}, {2, 5, -3},
            {3, 5, -2}, {1, 4, -3}, {2, 7, -4}, {3, 7, -4}, {1, 5, -6},
            {2, 9, -10}, {3, 11, -10}, {4, 9, -5}, {5, 11, -6},
            {6, 13, -7}, {7, 15, -8}
        };
    }

    if (count <= 0) {
        return result; // Return empty if count is not positive
    }

    if (count <= static_cast<int>(baseParams.size())) {
        result.insert(result.end(), baseParams.begin(), baseParams.begin() + count);
    } else {
        result = baseParams;
        int remaining = count - baseParams.size();
        std::vector<int> primes = {2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53};
        
        // Get appropriate RNG based on whether we have a seed
        std::mt19937_64& rng = has_seed
            ? getDeterministicThreadLocalEngine(seed)
            : getThreadLocalEngine();

        for (int i = 0; i < remaining; i++) {
            size_t baseIdx = i % baseParams.size();
            int prime1_idx = i % primes.size();
            int prime2_idx = (i + 1 + (rng() % (primes.size()-1) )) % primes.size(); // Add some randomness to prime2 selection
            if(prime1_idx == prime2_idx) prime2_idx = (prime2_idx + 1) % primes.size();

            auto baseParam = baseParams[baseIdx];
            long a = std::get<0>(baseParam);
            long b = std::get<1>(baseParam);
            long c = std::get<2>(baseParam);

            // Apply more varied tweaks
            std::uniform_int_distribution<int> tweak_dist_small(-2, 2);
            std::uniform_int_distribution<int> tweak_dist_large(-primes[prime2_idx]/2, primes[prime2_idx]/2);

            a += tweak_dist_small(rng);
            b += primes[prime1_idx] + tweak_dist_large(rng);
            c += tweak_dist_small(rng);

            if (a == 0) a = (rng() % 2 == 0) ? 1 : -1; // Ensure a is not zero
            if (c == 0) c = (rng() % 2 == 0) ? -1 : 1; // Ensure c is not zero
            
            // Ensure discriminant b^2 - 4ac > 0
            if ((a > 0 && c > 0) || (a < 0 && c < 0)) { // a and c have same sign
                c = -c; // Flip sign of c to make -4ac positive
            }
            
            long long current_disc = static_cast<long long>(b) * b - 4LL * static_cast<long long>(a) * c;
            if (current_disc <= 0) {
                // Make |b| large relative to |ac|
                b = static_cast<long>(std::sqrt(std::abs(4.0 * a * c)) + 5 + (rng()%10));
                if (rng()%2 == 0 && b != 0) b = -b; // Randomize sign of b
                if (b==0) b = (rng()%2 == 0) ? 5 : -5; // Ensure b is not zero if sqrt was 0

                // Re-ensure a and c have opposite signs for safety
                if (a > 0) c = std::abs(c) * -1; else if (a < 0) c = std::abs(c); else a = 1, c = -1;
                if (c == 0) c = (a > 0) ? -1 : 1; // Final check for c

                current_disc = static_cast<long long>(b) * b - 4LL * static_cast<long long>(a) * c;
                if (current_disc <= 0) { // Ultimate fallback
                    a = std::get<0>(baseParams[i % baseParams.size()]); 
                    b = std::get<1>(baseParams[i % baseParams.size()]);
                    c = std::get<2>(baseParams[i % baseParams.size()]);
                }
            }
            result.push_back(std::make_tuple(a, b, c));
        }
    }
    return result;
}

// CSV discriminant loading
void qiprng::loadCSVDiscriminants() {
    // Use a static flag to avoid potential race conditions with the mutex
    static std::atomic<bool> load_attempt_in_progress(false);
    
    // First quick check without lock
    if (g_csv_discriminants_loaded) {
        return;
    }
    
    // Try to set the in-progress flag
    bool expected = false;
    if (!load_attempt_in_progress.compare_exchange_strong(expected, true)) {
        // Another thread is already loading, just wait
        int timeout_ms = 1000; // 1 second timeout
        for (int i = 0; i < timeout_ms/10; i++) {
            if (g_csv_discriminants_loaded) {
                return; // Successfully loaded by another thread
            }
            std::this_thread::sleep_for(std::chrono::milliseconds(10));
        }
        // If we get here, the other thread might be stuck
        return; // Give up waiting
    }
    
    // We got the flag, now get the mutex
    std::lock_guard<std::mutex> lock(g_csv_disc_mutex);
    
    // Double-check after acquiring the lock
    if (g_csv_discriminants_loaded) {
        load_attempt_in_progress.store(false);
        return;
    }

    // Proceed with loading the discriminants
    try {
        std::vector<std::tuple<long, long, long, long long>> temp_discriminants;
        
        // Try different possible paths for the excellent discriminants CSV file
        std::vector<std::string> possible_paths = {
            "inst/extdata/excellent_discriminants.csv",
            "../inst/extdata/excellent_discriminants.csv",
            "analysis/data/excellent_discriminants.csv",
            "../analysis/data/excellent_discriminants.csv",
            "../../analysis/data/excellent_discriminants.csv",
            "excellent_discriminants.csv",
            "discriminants.csv",
            "../discriminants.csv",
            "../../discriminants.csv"
        };
        
        std::ifstream file;
        std::string used_path;
        
        for (const auto& path : possible_paths) {
            file.open(path);
            if (file.is_open()) {
                used_path = path;
                break;
            }
            file.clear(); // Clear any error flags
        }

        if (!file.is_open()) {
            Rcpp::warning("Could not open excellent_discriminants.csv file in any standard location. Will use generated discriminants.");
            
            // Generate default discriminants instead
            for (int i = 0; i < 100; i++) {
                long a = 1 + (i % 10);
                long b = 3 + 2 * (i % 8);
                long c = -1 - (i % 5);
                long long disc = static_cast<long long>(b) * b - 4LL * static_cast<long long>(a) * c;
                if (disc > 0) {
                    temp_discriminants.emplace_back(a, b, c, disc);
                }
            }
            
            g_csv_discriminants.swap(temp_discriminants);
            Rcpp::Rcout << "Created " << g_csv_discriminants.size() << " default discriminants." << std::endl;
            g_csv_discriminants_loaded = true;
            load_attempt_in_progress.store(false);
            return;
        }

        std::string header;
        if (!std::getline(file, header)) {
            Rcpp::warning("Error reading CSV header from %s.", used_path.c_str());
            g_csv_discriminants_loaded = true;
            load_attempt_in_progress.store(false);
            return;
        }

        std::string line;
        size_t line_number = 1;
        while (std::getline(file, line)) {
            line_number++;
            if (line.empty() || line[0] == '#') continue;

            std::stringstream ss(line);
            std::string token;
            std::vector<std::string> tokens;
            while (std::getline(ss, token, ',')) {
                tokens.push_back(token);
            }

            if (tokens.size() < 4) {
                Rcpp::warning("Skipping line %d in %s: insufficient values (expected 4, got %d)",
                            (int)line_number, used_path.c_str(), (int)tokens.size());
                continue;
            }

            try {
                long a = std::stol(tokens[0]);
                long b = std::stol(tokens[1]);
                long c = std::stol(tokens[2]);
                long long discriminant = std::stoll(tokens[3]);

                if (a == 0) {
                    Rcpp::warning("Skipping line %d from CSV: 'a' parameter cannot be zero", (int)line_number);
                    continue;
                }
                long long calculated_disc = static_cast<long long>(b) * b - 4LL * static_cast<long long>(a) * c;
                if (discriminant != calculated_disc) {
                    Rcpp::warning("Skipping line %d from CSV: provided discriminant %lld does not match calculated %lld for a=%ld, b=%ld, c=%ld",
                                (int)line_number, discriminant, calculated_disc, a,b,c);
                    continue;
                }
                if (discriminant <= 0) {
                    Rcpp::warning("Skipping line %d from CSV: discriminant %lld must be positive", (int)line_number, discriminant);
                    continue;
                }
                temp_discriminants.emplace_back(a, b, c, discriminant);
            } catch (const std::invalid_argument& e) {
                Rcpp::warning("Skipping line %d from CSV: invalid numeric format (%s)", (int)line_number, e.what());
            } catch (const std::out_of_range& e) {
                Rcpp::warning("Skipping line %d from CSV: number out of range (%s)", (int)line_number, e.what());
            }
        }

        if (!temp_discriminants.empty()) {
            g_csv_discriminants.swap(temp_discriminants);
            Rcpp::Rcout << "Loaded " << g_csv_discriminants.size() << " discriminants from CSV file." << std::endl;
        } else {
            Rcpp::warning("No valid discriminants found in CSV file or file was empty. Generating default discriminants.");
            
            // Generate default discriminants instead
            for (int i = 0; i < 100; i++) {
                long a = 1 + (i % 10);
                long b = 3 + 2 * (i % 8);
                long c = -1 - (i % 5);
                long long disc = static_cast<long long>(b) * b - 4LL * static_cast<long long>(a) * c;
                if (disc > 0) {
                    g_csv_discriminants.emplace_back(a, b, c, disc);
                }
            }
            
            if (!g_csv_discriminants.empty()) {
                Rcpp::Rcout << "Generated " << g_csv_discriminants.size() << " default discriminants." << std::endl;
            } else {
                Rcpp::warning("Failed to generate default discriminants. Thread safety issues may occur.");
            }
        }
        
        // Always mark as loaded, even if we failed, to avoid repeated attempts
        g_csv_discriminants_loaded = true;
        
        // Reset the in-progress flag
        load_attempt_in_progress.store(false);
        
    } catch (const std::exception& e) {
        Rcpp::warning("Exception during CSV discriminants loading: %s", e.what());
        g_csv_discriminants_loaded = true;
        load_attempt_in_progress.store(false);
    } catch (...) {
        Rcpp::warning("Unknown exception during CSV discriminants loading");
        g_csv_discriminants_loaded = true;
        load_attempt_in_progress.store(false);
    }
}

// Discriminant selection
long long qiprng::chooseUniqueDiscriminant(long min_value, long max_value) {
    initialize_libsodium_if_needed(); // Ensure libsodium is ready if used by PRNG for config

    PRNGConfig current_config; // Default config
    bool use_csv = PRNGDefaults::use_csv_discriminants; // Default

    // Safely try to get current PRNG's config - thread-safe access
    if (g_use_threading) {
        if (t_prng) {
            current_config = t_prng->getConfig(); // Thread-local access is safe
        }
    } else {
        std::lock_guard<std::mutex> lock(g_prng_mutex);
        if (g_prng) {
            current_config = g_prng->getConfig();
        }
    }
    use_csv = current_config.use_csv_discriminants;
    
    // Get appropriate RNG based on whether we have a seed
    std::mt19937_64& rng = current_config.has_seed
        ? getDeterministicThreadLocalEngine(current_config.seed)
        : getThreadLocalEngine();

    // First approach: Use CSV discriminants if configured
    if (use_csv) {
        // Make sure CSV discriminants are loaded - this has its own thread safety
        loadCSVDiscriminants(); 
        
        // Create a local copy of CSV discriminants to work with outside the lock
        std::vector<std::tuple<long,long,long,long long>> local_csv_copy;
        {
            std::lock_guard<std::mutex> csv_lock(g_csv_disc_mutex);
            if(!g_csv_discriminants.empty()) {
                local_csv_copy = g_csv_discriminants;
            }
        }

        if (!local_csv_copy.empty()) {
            if (current_config.has_seed) {
                // Use seeded shuffle for deterministic but randomized selection
                std::mt19937_64 shuffle_rng(current_config.seed);
                std::shuffle(local_csv_copy.begin(), local_csv_copy.end(), shuffle_rng);
            } else {
                // Original random shuffle
                std::shuffle(local_csv_copy.begin(), local_csv_copy.end(), rng);
            }
            
            for (const auto& entry : local_csv_copy) {
                long long disc_candidate = std::get<3>(entry);
                long csv_a = std::get<0>(entry);
                long csv_b = std::get<1>(entry);
                long csv_c = std::get<2>(entry);
                
                // Critical section: Check and update discriminant usage
                bool is_new_discriminant = false;
                {
                    std::lock_guard<std::mutex> disc_lock(g_disc_mutex);
                    if (g_used_discriminants.find(disc_candidate) == g_used_discriminants.end()) {
                        g_used_discriminants.insert(disc_candidate);
                        is_new_discriminant = true;
                    }
                }
                
                // If we found a new discriminant, update config and return it
                if (is_new_discriminant) {
                    // Update the PRNG's a,b,c with the ones from CSV
                    if (g_use_threading && t_prng) {
                        PRNGConfig cfg = t_prng->getConfig();
                        cfg.a = csv_a; cfg.b = csv_b; cfg.c = csv_c;
                        t_prng->updateConfig(cfg);
                    } else if (!g_use_threading && g_prng) {
                        std::lock_guard<std::mutex> prng_lock(g_prng_mutex);
                        PRNGConfig cfg = g_prng->getConfig();
                        cfg.a = csv_a; cfg.b = csv_b; cfg.c = csv_c;
                        g_prng->updateConfig(cfg);
                    }
                    return disc_candidate;
                }
            }
            
            // If we get here, all CSV discriminants are used
            if (current_config.debug) {
                Rcpp::warning("All unique discriminants from CSV have been used. Falling back to random generation.");
            }
        } else if (g_csv_discriminants_loaded) {
            // CSV was loaded but was empty or all entries were invalid
            if (current_config.debug) {
                Rcpp::warning("CSV discriminants were requested but list is empty or failed to load. Falling back to random generation.");
            }
        }
    }

    // Second approach: Generate random discriminants
    std::uniform_int_distribution<long long> dist(min_value, max_value);
    int max_attempts = 1000; 
    
    for (int attempt = 0; attempt < max_attempts; ++attempt) {
        long long candidate = dist(rng);
        if (candidate <= 0) continue;

        // Basic square-free check
        bool is_sf = true;
        long limit = static_cast<long>(std::sqrt(static_cast<double>(candidate)));
        if (limit > 1000) limit = 1000; // Cap for performance
        
        for (long f = 2; f <= limit; ++f) {
            if (candidate % (f * f) == 0) {
                is_sf = false;
                break;
            }
        }
        
        if (!is_sf) continue;

        // Critical section: Check and update discriminant usage
        bool is_new_discriminant = false;
        {
            std::lock_guard<std::mutex> lock(g_disc_mutex);
            if (g_used_discriminants.find(candidate) == g_used_discriminants.end()) {
                g_used_discriminants.insert(candidate);
                is_new_discriminant = true;
            }
        }
        
        if (is_new_discriminant) {
            // Calculate a, b, c values for this discriminant
            try {
                auto abc = makeABCfromDelta(candidate);
                long a = std::get<0>(abc);
                long b = std::get<1>(abc);
                long c = std::get<2>(abc);
                
                // Update the PRNG with these values
                if (g_use_threading && t_prng) {
                    PRNGConfig cfg = t_prng->getConfig();
                    cfg.a = a; cfg.b = b; cfg.c = c;
                    t_prng->updateConfig(cfg);
                } else if (!g_use_threading && g_prng) {
                    std::lock_guard<std::mutex> prng_lock(g_prng_mutex);
                    PRNGConfig cfg = g_prng->getConfig();
                    cfg.a = a; cfg.b = b; cfg.c = c;
                    g_prng->updateConfig(cfg);
                }
                
                return candidate;
            } catch (const std::exception& e) {
                // Log error but continue looking
                if (current_config.debug) {
                    Rcpp::warning("Failed to compute a,b,c for discriminant %lld: %s", candidate, e.what());
                }
                continue;
            }
        }
    }
    
    // Fallback if we couldn't find a good discriminant
    long long fallback_disc = 41; // Default value (from 2,5,-2)
    try {
        auto abc = makeABCfromDelta(fallback_disc);
        long a = std::get<0>(abc);
        long b = std::get<1>(abc);
        long c = std::get<2>(abc);
        
        // Update the PRNG with these values
        if (g_use_threading && t_prng) {
            PRNGConfig cfg = t_prng->getConfig();
            cfg.a = a; cfg.b = b; cfg.c = c;
            t_prng->updateConfig(cfg);
        } else if (!g_use_threading && g_prng) {
            std::lock_guard<std::mutex> prng_lock(g_prng_mutex);
            PRNGConfig cfg = g_prng->getConfig();
            cfg.a = a; cfg.b = b; cfg.c = c;
            g_prng->updateConfig(cfg);
        }
    } catch (...) {
        // Ultimate fallback
        if (g_use_threading && t_prng) {
            PRNGConfig cfg = t_prng->getConfig();
            cfg.a = 2; cfg.b = 5; cfg.c = -2;
            t_prng->updateConfig(cfg);
        } else if (!g_use_threading && g_prng) {
            std::lock_guard<std::mutex> prng_lock(g_prng_mutex);
            PRNGConfig cfg = g_prng->getConfig();
            cfg.a = 2; cfg.b = 5; cfg.c = -2;
            g_prng->updateConfig(cfg);
        }
    }
    
    return fallback_disc;
}

// Generate a,b,c values from discriminant
std::tuple<long, long, long> qiprng::makeABCfromDelta(long long Delta) {
    // Return a default fallback for non-positive Delta
    if (Delta <= 0) {
        return {1, 5, -1}; // Safe default fallback
    }
    
    const int MAX_TRIES = 1000; // Maximum attempts to prevent infinite loops
    int total_tries = 0;
    
    try {
        // Use a thread-local PRNG for reproducibility within a thread but diversity between threads
        std::mt19937_64& rng = getThreadLocalEngine();
        
        // Choose a random a value with a preference for small absolute values
        std::vector<long> a_choices = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 15};
        // Randomize sign 50% of the time
        for (size_t i = 0; i < a_choices.size(); ++i) {
            if (rng() % 2 == 0) {
                a_choices[i] = -a_choices[i];
            }
        }
        
        // Prefer small values for performance
        std::shuffle(a_choices.begin(), a_choices.end(), rng);
        std::sort(a_choices.begin(), a_choices.end(), [](long a, long b) {
            return std::abs(a) < std::abs(b);
        });
        
        // Try each a value to find a suitable b and c
        for (long a_val : a_choices) {
            if (total_tries >= MAX_TRIES) {
                break; // Prevent runaway loops
            }
            if (a_val == 0) continue; // Skip a=0
            
            // Limit search space for b to reasonable values
            long b_search_limit = static_cast<long>(std::sqrt(static_cast<double>(Delta)) + 1000);
            b_search_limit = std::min(b_search_limit, 20000L); // Cap at 20000 for performance
            b_search_limit = std::max(b_search_limit, 100L);   // Ensure at least 100 to avoid tiny search space
    
            std::uniform_int_distribution<long> b_dist(-b_search_limit, b_search_limit);
    
            // Try multiple random b values for each a value
            for (int i = 0; i < 200 && total_tries < MAX_TRIES; ++i, ++total_tries) {
                long b_val = b_dist(rng);
                
                // Skip b=0 unless appropriate condition is met
                if (b_val == 0 && Delta % (4LL * a_val) != 0) continue;
                
                // Use safe computation for b^2 to avoid overflow
                // Check if b^2 would overflow a long long
                long long b_ll = static_cast<long long>(b_val);
                if (b_ll > 0 && b_ll > std::numeric_limits<long long>::max() / b_ll) {
                    continue; // Would overflow, skip this value
                }
                if (b_ll < 0 && b_ll < std::numeric_limits<long long>::min() / b_ll) {
                    continue; // Would overflow, skip this value
                }
                
                long long b_squared = b_ll * b_ll;
                
                // Compute numerator and denominator for c
                long long num = b_squared - Delta;
                long long den = 4LL * a_val;
                
                // Skip invalid cases
                if (den == 0) continue;  // Should not happen with a_choices
                
                // Check if we can get an integer c
                if (num % den == 0) {
                    long long c_ll = num / den;
                    
                    // Check if c fits in a long
                    if (c_ll >= std::numeric_limits<long>::min() && 
                        c_ll <= std::numeric_limits<long>::max()) {
                        
                        long c_val = static_cast<long>(c_ll);
                        
                        // Skip c=0 unless appropriate condition is met
                        if (c_val == 0 && b_squared != Delta) continue;
                        
                        // Verify the result actually gives the correct discriminant
                        // Check for overflow in 4*a*c calculation
                        long long a_ll = static_cast<long long>(a_val);
                        long long c_ll = static_cast<long long>(c_val);
                        
                        // First check 4*a for overflow
                        if (a_ll > 0 && 4LL > std::numeric_limits<long long>::max() / a_ll) {
                            continue; // Would overflow
                        }
                        
                        long long four_a = 4LL * a_ll;
                        
                        // Then check four_a * c for overflow
                        bool will_overflow = false;
                        if (four_a > 0 && c_ll > 0) {
                            if (four_a > std::numeric_limits<long long>::max() / c_ll) {
                                will_overflow = true;
                            }
                        } else if (four_a < 0 && c_ll < 0) {
                            if (four_a < std::numeric_limits<long long>::max() / c_ll) {
                                will_overflow = true;
                            }
                        } else if (four_a > 0 && c_ll < 0) {
                            if (c_ll < std::numeric_limits<long long>::min() / four_a) {
                                will_overflow = true;
                            }
                        } else if (four_a < 0 && c_ll > 0) {
                            if (four_a < std::numeric_limits<long long>::min() / c_ll) {
                                will_overflow = true;
                            }
                        }
                        
                        if (will_overflow) {
                            continue;  // Skip if multiplication would overflow
                        }
                        
                        long long four_ac = four_a * c_ll;
                        long long disc = b_squared - four_ac;
                        
                        if (disc == Delta) {
                            return {a_val, b_val, c_val};
                        }
                    }
                }
            }
        }
        
        // Fallback if no suitable a,b,c found
        long fallback_a = 1;
        
        // Calculate a reasonable b value for the fallback
        long fallback_b;
        if (Delta <= 4) {
            fallback_b = static_cast<long>(std::sqrt(static_cast<double>(Delta + 100)));
        } else {
            fallback_b = static_cast<long>(std::sqrt(static_cast<double>(Delta)));
        }
        
        // Choose c to satisfy b^2 - 4ac = Delta
        long long b_squared = static_cast<long long>(fallback_b) * fallback_b;
        long fallback_c = static_cast<long>((b_squared - Delta) / (4 * fallback_a));
        
        // Verify result
        long long test_disc = static_cast<long long>(fallback_b) * fallback_b - 
                              4LL * static_cast<long long>(fallback_a) * fallback_c;
        
        if (test_disc == Delta) {
            return {fallback_a, fallback_b, fallback_c};
        }
        
        // Ultimate fallback with nice round numbers
        return {1, 5, -1}; // This gives 41
    } catch (...) {
        // Even safer fallback in case of any errors
        return {1, 5, -1};
    }
}