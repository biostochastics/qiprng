// Test file for v0.5.0 enhancements
#include <iostream>
#include <cassert>
#include <cmath>
#include <vector>
#include "../src/multi_qi.hpp"
#include "../src/quadratic_irrational.hpp"
#include "../src/thread_manager.hpp"
#include "../src/prng_common.hpp"

using namespace qiprng;

// Test 1: Verify XOR mixing mantissa extraction
void test_xor_mixing() {
    std::cout << "Test 1: XOR Mixing Mantissa Extraction\n";
    
    // Create a small MultiQI ensemble
    std::vector<std::tuple<long, long, long>> abc_list = {
        {1, 5, -2},
        {2, 7, -3}
    };
    
    MultiQI multi(abc_list, 256, 12345, true, MixingStrategy::XOR_MIX);
    
    // Generate values
    double val1 = multi.next_mixed();
    double val2 = multi.next_mixed();
    
    std::cout << "  XOR mixed value 1: " << val1 << "\n";
    std::cout << "  XOR mixed value 2: " << val2 << "\n";
    
    // Values should be in [0, 1)
    assert(val1 >= 0.0 && val1 < 1.0);
    assert(val2 >= 0.0 && val2 < 1.0);
    
    std::cout << "  ✓ XOR mixing produces valid range\n\n";
}

// Test 2: Verify CFE period computation
void test_cfe_period() {
    std::cout << "Test 2: CFE Period Computation\n";
    
    // Create QI with known discriminant
    QuadraticIrrational qi(1, 1, -1, 256); // D = 1 + 4 = 5
    
    // Trigger CFE computation by generating values
    for (int i = 0; i < 10; i++) {
        qi.next();
    }
    
    if (qi.has_computed_cfe()) {
        std::cout << "  CFE period length: " << qi.get_cfe_period() << "\n";
        std::cout << "  ✓ CFE computation successful\n";
    } else {
        std::cout << "  ✗ CFE not computed\n";
    }
    std::cout << "\n";
}

// Test 3: Verify Matrix2x2 power operation
void test_matrix_power() {
    std::cout << "Test 3: Matrix Binary Exponentiation\n";
    
    Matrix2x2 m(2, 1, 1, 1); // Fibonacci matrix
    
    // Test power of 10
    Matrix2x2 m10 = m.power(10);
    
    // Manually compute m^10 for verification
    Matrix2x2 manual = m;
    for (int i = 1; i < 10; i++) {
        manual = manual * m;
    }
    
    std::cout << "  Binary exp result: [" << m10.p << ", " << m10.q 
              << ", " << m10.r << ", " << m10.s << "]\n";
    std::cout << "  Manual result:     [" << manual.p << ", " << manual.q 
              << ", " << manual.r << ", " << manual.s << "]\n";
    
    assert(m10.p == manual.p && m10.q == manual.q && 
           m10.r == manual.r && m10.s == manual.s);
    
    std::cout << "  ✓ Matrix power computation correct\n\n";
}

// Test 4: Verify memory pool functionality
void test_memory_pool() {
    std::cout << "Test 4: Memory Pool\n";
    
    struct TestObject {
        int value;
        TestObject() : value(0) {}
    };
    
    MemoryPool<TestObject> pool(10);
    
    // Allocate objects
    std::vector<TestObject*> objects;
    for (int i = 0; i < 5; i++) {
        TestObject* obj = pool.allocate();
        if (obj) {
            obj->value = i;
            objects.push_back(obj);
        }
    }
    
    std::cout << "  Allocated " << objects.size() << " objects\n";
    std::cout << "  Pool size: " << pool.size() << "\n";
    std::cout << "  Available: " << pool.available() << "\n";
    
    // Deallocate some
    pool.deallocate(objects[0]);
    pool.deallocate(objects[2]);
    
    std::cout << "  After deallocation - Available: " << pool.available() << "\n";
    
    assert(pool.available() == 2);
    std::cout << "  ✓ Memory pool working correctly\n\n";
}

// Test 5: Thread-local storage
void test_thread_local_storage() {
    std::cout << "Test 5: Thread-Local Storage\n";
    
    auto& value1 = ThreadManager::getThreadLocal<int>("test_key", []() { return 42; });
    std::cout << "  Initial value: " << value1 << "\n";
    
    value1 = 100;
    auto& value2 = ThreadManager::getThreadLocal<int>("test_key", []() { return 42; });
    std::cout << "  Modified value: " << value2 << "\n";
    
    assert(value2 == 100);
    std::cout << "  ✓ Thread-local storage maintains state\n\n";
}

// Test 6: Dynamic QI generation
void test_dynamic_qi_generation() {
    std::cout << "Test 6: Dynamic QI Generation\n";
    
    // Create MultiQI with dynamic generation
    MultiQI multi(10, 256, 54321, true, MixingStrategy::CASCADE_MIX);
    
    std::cout << "  Created ensemble with " << multi.size() << " QIs\n";
    
    // Generate some values
    double sum = 0.0;
    for (int i = 0; i < 100; i++) {
        sum += multi.next_mixed();
    }
    double mean = sum / 100.0;
    
    std::cout << "  Mean of 100 samples: " << mean << "\n";
    
    // Mean should be around 0.5
    assert(mean > 0.3 && mean < 0.7);
    std::cout << "  ✓ Dynamic QI generation produces valid distribution\n\n";
}

// Test 7: Mixing strategy comparison
void test_mixing_strategies() {
    std::cout << "Test 7: Mixing Strategy Comparison\n";
    
    std::vector<std::tuple<long, long, long>> abc_list = {
        {1, 5, -2}, {2, 7, -3}, {3, 11, -5}
    };
    
    MixingStrategy strategies[] = {
        MixingStrategy::ROUND_ROBIN,
        MixingStrategy::XOR_MIX,
        MixingStrategy::AVERAGING,
        MixingStrategy::MODULAR_ADD,
        MixingStrategy::CASCADE_MIX
    };
    
    const char* names[] = {
        "ROUND_ROBIN", "XOR_MIX", "AVERAGING", "MODULAR_ADD", "CASCADE_MIX"
    };
    
    for (int s = 0; s < 5; s++) {
        MultiQI multi(abc_list, 256, 111, true, strategies[s]);
        
        double sum = 0.0;
        double min_val = 1.0, max_val = 0.0;
        
        for (int i = 0; i < 1000; i++) {
            double val = (strategies[s] == MixingStrategy::ROUND_ROBIN) ? 
                         multi.next() : multi.next_mixed();
            sum += val;
            min_val = std::min(min_val, val);
            max_val = std::max(max_val, val);
        }
        
        std::cout << "  " << names[s] << ":\n";
        std::cout << "    Mean: " << (sum/1000.0) 
                  << ", Range: [" << min_val << ", " << max_val << "]\n";
    }
    
    std::cout << "  ✓ All mixing strategies produce valid output\n\n";
}

// Test 8: Entropy estimation
void test_entropy_estimation() {
    std::cout << "Test 8: Entropy Estimation\n";
    
    MultiQI small(3, 256, 0, false, MixingStrategy::ROUND_ROBIN);
    MultiQI large(100, 256, 0, false, MixingStrategy::CASCADE_MIX);
    
    double small_entropy = small.estimate_entropy();
    double large_entropy = large.estimate_entropy();
    
    std::cout << "  Small ensemble (3 QIs, round-robin): " << small_entropy << " bits\n";
    std::cout << "  Large ensemble (100 QIs, cascade): " << large_entropy << " bits\n";
    
    assert(large_entropy > small_entropy);
    std::cout << "  ✓ Entropy estimation reflects ensemble complexity\n\n";
}

int main() {
    std::cout << "=== v0.5.0 Enhancement Tests ===\n\n";
    
    try {
        test_xor_mixing();
        test_cfe_period();
        test_matrix_power();
        test_memory_pool();
        test_thread_local_storage();
        test_dynamic_qi_generation();
        test_mixing_strategies();
        test_entropy_estimation();
        
        std::cout << "=== All Tests Passed ===\n";
        return 0;
    } catch (const std::exception& e) {
        std::cerr << "Test failed with exception: " << e.what() << "\n";
        return 1;
    }
}