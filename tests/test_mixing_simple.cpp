// Simple test for XOR mixing bug
#include <cstdint>
#include <cstring>
#include <iostream>

// Test the XOR mixing divisor issue
int main() {
    std::cout << "=== XOR Mixing Divisor Test ===\n\n";

    // Mantissa mask (52 bits)
    uint64_t mantissa_mask = 0x000FFFFFFFFFFFFFULL;
    uint64_t uint64_max = UINT64_MAX;

    std::cout << "Mantissa mask:  0x" << std::hex << mantissa_mask << std::dec << " = "
              << mantissa_mask << "\n";
    std::cout << "UINT64_MAX:     0x" << std::hex << uint64_max << std::dec << " = " << uint64_max
              << "\n\n";

    // Test value (arbitrary mantissa bits)
    uint64_t test_mantissa = 0x0008000000000000ULL;

    // Current (incorrect) implementation
    double wrong_result = static_cast<double>(test_mantissa) / static_cast<double>(UINT64_MAX);

    // Correct implementation
    double correct_result = static_cast<double>(test_mantissa) / static_cast<double>(mantissa_mask);

    std::cout << "Test mantissa: 0x" << std::hex << test_mantissa << std::dec << "\n";
    std::cout << "Wrong result (dividing by UINT64_MAX):   " << wrong_result << "\n";
    std::cout << "Correct result (dividing by mask):       " << correct_result << "\n\n";

    // The difference is significant!
    double ratio = correct_result / wrong_result;
    std::cout << "Ratio (correct/wrong): " << ratio << "\n";
    std::cout << "This means values are off by a factor of ~" << (int)ratio << "!\n\n";

    // Test mantissa extraction
    double test_double = 0.123456789;
    uint64_t bits;
    std::memcpy(&bits, &test_double, sizeof(double));
    uint64_t extracted = bits & mantissa_mask;

    std::cout << "Test double: " << test_double << "\n";
    std::cout << "Raw bits: 0x" << std::hex << bits << std::dec << "\n";
    std::cout << "Extracted mantissa: 0x" << std::hex << extracted << std::dec << "\n";

    return 0;
}
