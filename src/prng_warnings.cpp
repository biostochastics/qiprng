#include "prng_common.hpp"

namespace qiprng {

// Initialize global warning suppression flag (default to off)
bool suppress_mpfr_warnings = false;

// Initialize thread-local warning counter
thread_local int mpfr_warning_count = 0;

} // namespace qiprng