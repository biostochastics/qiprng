// File: deterministic_rng.hpp
// --------------------------------------------------------------
#ifndef QIPRNG_DETERMINISTIC_RNG_HPP
#define QIPRNG_DETERMINISTIC_RNG_HPP

#include <functional>
#include <random>
#include <string>

namespace qiprng {

class DeterministicRNGFactory {
   public:
    static std::mt19937_64 create(uint64_t seed, const std::string& context = "") {
        // Combine seed with context hash for unique but deterministic seeds
        std::hash<std::string> hasher;
        uint64_t context_hash = hasher(context);
        return std::mt19937_64(seed ^ context_hash);
    }

    static std::mt19937_64 create_for_thread(uint64_t seed, size_t thread_id) {
        return std::mt19937_64(seed + thread_id * 1000000007ULL);  // Large prime offset
    }
};

}  // namespace qiprng

#endif  // QIPRNG_DETERMINISTIC_RNG_HPP
