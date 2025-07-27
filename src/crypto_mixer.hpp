// File: crypto_mixer.hpp
// --------------------------------------------------------------
#ifndef QIPRNG_CRYPTO_MIXER_HPP
#define QIPRNG_CRYPTO_MIXER_HPP

#include "prng_common.hpp" // For SecureBuffer
#include <sodium.h>         // For libsodium functions
#include <Rcpp.h>           // For Rcpp::warning
#include <cstddef>        // For size_t
#include <cstdint>        // For uint64_t
#include <functional>     // For std::function
#include <random>         // For std::mt19937_64, std::uniform_real_distribution (used in .cpp)


// Forward declaration
namespace qiprng {
    std::mt19937_64& getThreadLocalEngine(); // Declared in prng_utils.hpp/cpp
    extern bool sodium_initialized; // Declared in prng_utils.hpp/cpp
}


namespace qiprng {

class CryptoMixer {
private:
    SecureBuffer<unsigned char> key_;
    SecureBuffer<unsigned char> nonce_;
    bool adhoc_corrections_;
    bool use_tie_breaking_;
    bool initialized_;

    void secure_random(unsigned char* buf, size_t len);

public:
    static constexpr uint64_t MANTISSA_MASK = 0x000FFFFFFFFFFFFFULL;
    static constexpr uint64_t ONE_BITS      = 0x3FF0000000000000ULL;

    CryptoMixer(bool adhoc_corrections, bool use_tie_breaking);
    ~CryptoMixer(); // SecureBuffer handles its own cleanup

    void reseed();
    bool is_initialized() const;
    bool mix(unsigned char* data, size_t len); // data is interpreted as array of doubles
};

} // namespace qiprng

#endif // QIPRNG_CRYPTO_MIXER_HPP 