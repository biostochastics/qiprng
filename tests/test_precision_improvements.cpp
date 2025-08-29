// test_precision_improvements.cpp
// Test suite to verify algorithmic accuracy improvements

#include <iostream>
#include <iomanip>
#include <cmath>
#include <mpfr.h>
#include "../src/precision_utils.hpp"
#include "../src/prng_common.hpp"

using namespace qiprng;

// Count matching decimal digits between two numbers
int count_matching_digits(double a, double b) {
    if (std::abs(a - b) < 1e-15) return 15;
    
    double diff = std::abs(a - b);
    if (diff == 0) return 15;
    
    return static_cast<int>(-std::log10(diff / std::max(std::abs(a), std::abs(b))));
}

void test_pi_precision() {
    std::cout << "\n=== Testing PI Precision ===" << std::endl;
    
    // Get high-precision PI from MPFR (256 bits)
    mpfr_t pi_mpfr;
    mpfr_init2(pi_mpfr, 256);
    mpfr_const_pi(pi_mpfr, MPFR_RNDN);
    
    // Get PI using different methods
    double pi_hardcoded = 3.14159265358979323846;  // Old hardcoded value
    double pi_from_mpfr_direct = mpfr_get_d(pi_mpfr, MPFR_RNDN);
    double pi_from_utils = precision::get_high_precision_pi();
    
    // Reference value with maximum precision
    const double pi_reference = 3.141592653589793238462643383279502884197;
    
    std::cout << std::setprecision(20);
    std::cout << "Reference PI:     " << pi_reference << std::endl;
    std::cout << "Hardcoded PI:     " << pi_hardcoded << std::endl;
    std::cout << "MPFR direct:      " << pi_from_mpfr_direct << std::endl;
    std::cout << "Precision utils:  " << pi_from_utils << std::endl;
    
    int digits_hardcoded = count_matching_digits(pi_reference, pi_hardcoded);
    int digits_mpfr = count_matching_digits(pi_reference, pi_from_mpfr_direct);
    int digits_utils = count_matching_digits(pi_reference, pi_from_utils);
    
    std::cout << "\nMatching decimal digits:" << std::endl;
    std::cout << "Hardcoded:    " << digits_hardcoded << " digits" << std::endl;
    std::cout << "MPFR direct:  " << digits_mpfr << " digits" << std::endl;
    std::cout << "Utils method: " << digits_utils << " digits" << std::endl;
    
    if (digits_utils >= digits_hardcoded) {
        std::cout << "✓ Precision utils provides equal or better PI precision" << std::endl;
    } else {
        std::cout << "✗ Precision utils has worse PI precision" << std::endl;
    }
    
    mpfr_clear(pi_mpfr);
}

void test_conversion_precision() {
    std::cout << "\n=== Testing MPFR Conversion Precision ===" << std::endl;
    
    // Create a high-precision value
    mpfr_t high_prec;
    mpfr_init2(high_prec, 256);
    
    // Set to a value that requires high precision
    // Using e^pi as a test value
    mpfr_t pi, e;
    mpfr_init2(pi, 256);
    mpfr_init2(e, 256);
    mpfr_const_pi(pi, MPFR_RNDN);
    mpfr_exp(high_prec, pi, MPFR_RNDN);  // e^pi
    
    // Convert using different methods
    double direct_conversion = mpfr_get_d(high_prec, MPFR_RNDN);
    double safe_conversion = precision::safe_mpfr_to_double(high_prec, true);
    
    // Convert to string for comparison (highest precision reference)
    char buffer[100];
    mpfr_sprintf(buffer, "%.25Rf", high_prec);
    double reference = std::stod(buffer);
    
    std::cout << std::setprecision(20);
    std::cout << "e^pi reference:   " << buffer << std::endl;
    std::cout << "Direct convert:   " << direct_conversion << std::endl;
    std::cout << "Safe convert:     " << safe_conversion << std::endl;
    
    // Calculate precision loss
    double loss_direct = std::abs(reference - direct_conversion);
    double loss_safe = std::abs(reference - safe_conversion);
    
    std::cout << "\nAbsolute error:" << std::endl;
    std::cout << "Direct: " << std::scientific << loss_direct << std::endl;
    std::cout << "Safe:   " << std::scientific << loss_safe << std::endl;
    
    if (loss_safe <= loss_direct) {
        std::cout << "✓ Safe conversion preserves equal or better precision" << std::endl;
    } else {
        std::cout << "✗ Safe conversion has higher error" << std::endl;
    }
    
    // Check precision tracking
    double avg_bits_lost = precision::get_average_bits_lost();
    std::cout << "\nAverage bits lost in conversions: " << avg_bits_lost << std::endl;
    
    mpfr_clear(high_prec);
    mpfr_clear(pi);
    mpfr_clear(e);
}

void test_mpfr_wrapper_precision() {
    std::cout << "\n=== Testing MPFRWrapper Precision ===" << std::endl;
    
    // Create wrapper with high precision
    MPFRWrapper wrapper(256);
    
    // Set to sqrt(2) with high precision
    mpfr_sqrt_ui(*wrapper.get(), 2, MPFR_RNDN);
    
    // Test both conversion methods
    double standard = mpfr_get_d(*wrapper.get(), MPFR_RNDN);
    double extended = wrapper.to_double_extended();
    
    // Reference value
    const double sqrt2_ref = 1.4142135623730950488016887242096980785696;
    
    std::cout << std::setprecision(20);
    std::cout << "sqrt(2) reference: " << sqrt2_ref << std::endl;
    std::cout << "Standard convert:  " << standard << std::endl;
    std::cout << "Extended convert:  " << extended << std::endl;
    
    int digits_standard = count_matching_digits(sqrt2_ref, standard);
    int digits_extended = count_matching_digits(sqrt2_ref, extended);
    
    std::cout << "\nMatching digits:" << std::endl;
    std::cout << "Standard: " << digits_standard << " digits" << std::endl;
    std::cout << "Extended: " << digits_extended << " digits" << std::endl;
    
    if (digits_extended >= digits_standard) {
        std::cout << "✓ Extended conversion preserves equal or better precision" << std::endl;
    } else {
        std::cout << "✗ Extended conversion has worse precision" << std::endl;
    }
}

int main() {
    std::cout << "==============================================\n";
    std::cout << "   QIPRNG Precision Improvements Test Suite   \n";
    std::cout << "==============================================\n";
    
    // Reset precision tracking
    precision::reset_precision_stats();
    
    // Run tests
    test_pi_precision();
    test_conversion_precision();
    test_mpfr_wrapper_precision();
    
    // Final statistics
    std::cout << "\n=== Final Statistics ===" << std::endl;
    std::cout << "Total conversions tracked: " << precision::conversion_count.load() << std::endl;
    std::cout << "Average bits lost: " << precision::get_average_bits_lost() << std::endl;
    
    // Cleanup
    precision::PrecisionConstants::cleanup();
    
    std::cout << "\n✅ All precision tests completed\n" << std::endl;
    
    return 0;
}