// File: crypto_mixer.cpp
// --------------------------------------------------------------
#include "crypto_mixer.hpp"
#include "prng_utils.hpp" // For getThreadLocalEngine and sodium_initialized extern
#include <cstring>      // For std::memcpy
#include <cmath>        // For std::fmod, std::isnan, std::nextafter, std::floor
#include <limits>       // For std::numeric_limits
#include <algorithm>    // For std::clamp


namespace qiprng {

// Definition for the extern declared in crypto_mixer.hpp (and prng_utils.hpp)
// bool sodium_initialized; // This should be defined in prng_utils.cpp

void CryptoMixer::secure_random(unsigned char* buf, size_t len) {
    if (!buf || len == 0) {
        throw std::invalid_argument("CryptoMixer: Invalid buffer for secure random generation");
    }
    
    if (has_seed_) {
        // Use deterministic generation when seed is provided
        std::uniform_int_distribution<unsigned int> dist(0, 255);
        for (size_t i = 0; i < len; ++i) {
            buf[i] = static_cast<unsigned char>(dist(det_rng_));
        }
    } else if (qiprng::sodium_initialized) { // Check global flag
        randombytes_buf(buf, len);
    } else {
        // Libsodium not initialized - this is a critical security error
        // Throw an exception instead of using insecure fallback
        throw std::runtime_error("CryptoMixer: Libsodium not initialized, cannot generate secure random bytes. "
                                "Please ensure libsodium is properly initialized before using crypto features.");
    }
}

CryptoMixer::CryptoMixer(bool adhoc_corrections, bool use_tie_breaking,
                         uint64_t seed, bool has_seed)
    : key_(crypto_stream_chacha20_KEYBYTES),
      nonce_(crypto_stream_chacha20_NONCEBYTES),
      adhoc_corrections_(adhoc_corrections),
      use_tie_breaking_(use_tie_breaking),
      initialized_(false),
      seed_(seed),
      has_seed_(has_seed),
      det_rng_(has_seed ? seed : std::random_device{}()) {
    if (!qiprng::sodium_initialized) {
         Rcpp::warning("CryptoMixer: Libsodium not initialized at construction. Crypto mixing may be insecure or fail.");
        // Do not throw here, allow construction but it won't be secure until libsodium is init'd
        // and reseed() is successfully called.
        return; // initialized_ remains false
    }
    try {
        reseed(); // This will set initialized_ to true on success
    } catch (const std::exception& e) {
        Rcpp::warning("CryptoMixer: Failed to initialize crypto mixer during construction: %s. Will remain uninitialized.", e.what());
        initialized_ = false; // Explicitly ensure it's false on failure
    }
}

CryptoMixer::~CryptoMixer() {
    // SecureBuffer's destructor handles zeroing its memory
}

void CryptoMixer::reseed() {
    if (!qiprng::sodium_initialized) {
        initialized_ = false;
        throw std::runtime_error("CryptoMixer: Reseed failed because libsodium is not initialized.");
    }
    try {
        secure_random(key_.data(), key_.size());
        secure_random(nonce_.data(), nonce_.size());
        initialized_ = true;
    } catch (const std::exception& e) {
        initialized_ = false;
        throw std::runtime_error(std::string("CryptoMixer: Reseed failed: ") + e.what());
    }
}

bool CryptoMixer::is_initialized() const {
    return initialized_ && qiprng::sodium_initialized;
}

bool CryptoMixer::mix(unsigned char* data, size_t len) {
    if (!is_initialized()) { // Relies on is_initialized() checking the global sodium_initialized flag
        Rcpp::warning("CryptoMixer: Not initialized or libsodium is not ready, skipping mix operation.");
        // Try to re-initialize on demand if libsodium is now ready
        if (qiprng::sodium_initialized && !initialized_) {
            try {
                reseed();
                 Rcpp::Rcout << "CryptoMixer: Re-initialized successfully during mix()." << std::endl;
            } catch (const std::exception& e) {
                Rcpp::warning("CryptoMixer: Failed to re-initialize during mix: %s. Skipping mix.", e.what());
                return false;
            }
        } else if (!qiprng::sodium_initialized) {
            return false; // Libsodium still not ready
        }
        if(!initialized_) return false; // If reseed failed or wasn't attempted
    }

    if (!data || len == 0) {
        Rcpp::warning("CryptoMixer: Invalid data buffer provided to mix()");
        return false;
    }
    if (len % sizeof(double) != 0) {
        Rcpp::warning("CryptoMixer: Buffer length not aligned to double size");
        return false;
    }

    try {
        size_t num_doubles = len / sizeof(double);
        double* doubles = reinterpret_cast<double*>(data);

        SecureBuffer<unsigned char> random_bytes_buf(len); // Corrected name
        secure_random(random_bytes_buf.data(), len); // Use the member function that checks sodium_initialized

        double prev_val = (num_doubles > 0) ? doubles[0] : 0.0; // Initialize reasonably

        if (!adhoc_corrections_) { // partial modular addition
            for (size_t i = 0; i < num_doubles; i++) {
                 if (i * sizeof(double) + sizeof(uint64_t) > random_bytes_buf.size()) { // Check against random_bytes_buf
                    Rcpp::warning("CryptoMixer: Random buffer access out of bounds in modular addition path.");
                    return false; 
                }
                uint64_t crypto_bits_val = 0; // Renamed variable
                std::memcpy(&crypto_bits_val, &random_bytes_buf[i * sizeof(double)], sizeof(uint64_t));
                crypto_bits_val = (crypto_bits_val & MANTISSA_MASK) | ONE_BITS;
                double crypto_uniform;
                std::memcpy(&crypto_uniform, &crypto_bits_val, sizeof(double)); // Safe reinterpret_cast alternative
                crypto_uniform -= 1.0;

                // Simplified normalization to [0,1) range
                double mixed_val = doubles[i] + crypto_uniform;
                
                // Normalize to [0,1) range using a single approach
                mixed_val = mixed_val - std::floor(mixed_val);
                if (mixed_val < 0.0) mixed_val += 1.0;
                
                // Safety checks for numeric stability
                if (std::isnan(mixed_val) || std::isinf(mixed_val)) {
                    Rcpp::warning("CryptoMixer: Invalid result in mixing, using fallback value 0.5");
                    mixed_val = 0.5;
                }
                
                // Ensure result is strictly in [0, 1)
                if (mixed_val >= 1.0) {
                    mixed_val = std::nextafter(1.0, 0.0);
                }
                
                doubles[i] = mixed_val;

                if (use_tie_breaking_ && i > 0 && doubles[i] == prev_val) {
                     try {
                        // Use smaller epsilon to avoid underflow issues
                        static thread_local std::uniform_real_distribution<double> tiny_dist(-1e-15, 1e-15);
                        double eps = tiny_dist(qiprng::getThreadLocalEngine());
                        double new_val = doubles[i] + eps;
                        // Manual clamp implementation for C++14 compatibility
                        const double min_val = std::nextafter(0.0, 1.0);
                        const double max_val = std::nextafter(1.0, 0.0);
                        if (new_val < min_val) {
                            doubles[i] = min_val;
                        } else if (new_val > max_val) {
                            doubles[i] = max_val;
                        } else {
                            doubles[i] = new_val;
                        }
                     } catch (const std::exception& e) {
                        Rcpp::warning("CryptoMixer: Tie breaking failed in modular path: %s", e.what());
                     }
                }
                prev_val = doubles[i];
            }
        } else { // partial averaging approach
            for (size_t i = 0; i < num_doubles; i++) {
                if (i * sizeof(double) + sizeof(uint64_t) > random_bytes_buf.size()) { // Check against random_bytes_buf
                     Rcpp::warning("CryptoMixer: Random buffer access out of bounds in averaging path.");
                    return false;
                }
                uint64_t random_bits_val = 0; // Renamed variable
                std::memcpy(&random_bits_val, &random_bytes_buf[i * sizeof(double)], sizeof(uint64_t));
                uint64_t bits_val = (random_bits_val & MANTISSA_MASK) | ONE_BITS; // Renamed variable
                double crypto_uniform;
                std::memcpy(&crypto_uniform, &bits_val, sizeof(double)); // Safe reinterpret_cast alternative
                crypto_uniform -= 1.0;

                if (std::isnan(crypto_uniform) || std::isnan(doubles[i])) {
                    Rcpp::warning("CryptoMixer: NaN detected in averaging, using fallback 0.5");
                    doubles[i] = 0.5;
                } else {
                    doubles[i] = 0.5 * doubles[i] + 0.5 * crypto_uniform;
                }

                if (use_tie_breaking_ && i > 0 && doubles[i] == prev_val) {
                    try {
                        static thread_local std::uniform_real_distribution<double> tiny_dist(-1e-14, 1e-14);
                        double eps = tiny_dist(qiprng::getThreadLocalEngine()); // Fully qualify
                        doubles[i] += eps;
                    } catch (const std::exception& e) {
                        Rcpp::warning("CryptoMixer: Tie breaking failed in averaging path: %s", e.what());
                    }
                }
                if (doubles[i] >= 1.0) doubles[i] = std::nextafter(1.0, 0.0); // Clamping after potential tie-breaking
                if (doubles[i] < 0.0)  doubles[i] = std::nextafter(0.0, 1.0);

                prev_val = doubles[i];
            }
        }
        return true;
    } catch (const std::exception& e) {
        Rcpp::warning("CryptoMixer: Unexpected error during mixing: %s", e.what());
        return false;
    }
}

} // namespace qiprng 