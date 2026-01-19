// File: prng_utils.hpp
// --------------------------------------------------------------
#ifndef QIPRNG_UTILS_HPP
#define QIPRNG_UTILS_HPP

#include <atomic>
#include <memory>
#include <mutex>
#include <random>
#include <thread>
#include <tuple>
#include <unordered_set>
#include <vector>

#include "prng_config.hpp"

// Forward declarations
namespace qiprng {
class EnhancedPRNG;  // Forward declaration for globals
}

namespace qiprng {

// LibSodium initialization
extern std::atomic<bool> sodium_initialized_flag;
extern bool sodium_initialized;  // Legacy flag for CryptoMixer compatibility

// Thread-safe libsodium initialization
void initialize_libsodium_if_needed();
void ensure_libsodium_initialized();

// Thread-local random engine
std::mt19937_64& getThreadLocalEngine();
std::mt19937_64& getDeterministicThreadLocalEngine(uint64_t seed);
void resetDeterministicEngine();

// Global PRNG state
extern bool g_use_threading;
extern std::mutex g_prng_mutex;
extern thread_local std::unique_ptr<qiprng::EnhancedPRNG> t_prng;
extern std::unique_ptr<qiprng::EnhancedPRNG> g_prng;

// Discriminant utilities
extern std::mutex g_disc_mutex;
extern std::unordered_set<long long> g_used_discriminants;

// CSV Discriminant utilities
extern std::vector<std::tuple<long, long, long, long long>> g_csv_discriminants;
extern std::mutex g_csv_disc_mutex;
extern bool g_csv_discriminants_loaded;

// Core utility functions
std::vector<std::tuple<long, long, long>> pickMultiQiSet(int precision, int count,
                                                         uint64_t seed = 0, bool has_seed = false);
void loadCSVDiscriminants();  // Make this accessible for thread-safe initialization
long long chooseUniqueDiscriminant(long min_value = 5, long max_value = 1000000);
std::tuple<long, long, long> makeABCfromDelta(long long Delta);

}  // namespace qiprng

#endif  // QIPRNG_UTILS_HPP
