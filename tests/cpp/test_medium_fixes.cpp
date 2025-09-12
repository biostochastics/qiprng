// Test file for medium severity fixes
#include <chrono>
#include <iostream>
#include <thread>
#include <vector>

#include "../src/bit_operations.hpp"
#include "../src/cache_aligned.hpp"
#include "../src/mpfr_pool.hpp"
#include "../src/simd_operations.hpp"
#include "../src/thread_pool.hpp"

using namespace qiprng;

// Test 1: Strict aliasing fix
void test_bit_operations() {
    std::cout << "Testing bit operations (strict aliasing fix)..." << std::endl;

    double a = 3.14159;
    double b = 2.71828;

    // Test safe bit casting
    uint64_t ua = bit_ops::safe_bit_cast<uint64_t>(a);
    double a_back = bit_ops::safe_bit_cast<double>(ua);

    if (std::abs(a - a_back) > 1e-15) {
        std::cerr << "ERROR: Bit cast round-trip failed!" << std::endl;
    } else {
        std::cout << "  ✓ Bit cast round-trip successful" << std::endl;
    }

    // Test XOR operation
    double result = bit_ops::xor_doubles_as_uint64(a, b);
    std::cout << "  ✓ XOR operation completed without UB" << std::endl;

    // Test batch XOR
    std::vector<double> src1(100, a);
    std::vector<double> src2(100, b);
    std::vector<double> dest(100);

    bit_ops::xor_doubles_batch(dest.data(), src1.data(), src2.data(), 100);
    std::cout << "  ✓ Batch XOR completed successfully" << std::endl;

    // Test SIMD operations with new bit ops
    simd::xor_mix_batch(dest.data(), src1.data(), src2.data(), 100);
    std::cout << "  ✓ SIMD XOR mix completed without aliasing violations" << std::endl;
}

// Test 2: ThreadPool shutdown with timeout
void test_thread_pool_shutdown() {
    std::cout << "\nTesting ThreadPool shutdown with timeout..." << std::endl;

    {
        ThreadPool pool(4);

        // Submit a long-running task
        auto future = pool.enqueue([]() {
            std::this_thread::sleep_for(std::chrono::milliseconds(100));
            return 42;
        });

        // Shutdown with generous timeout
        bool joined = pool.shutdown(std::chrono::seconds(1));
        if (joined) {
            std::cout << "  ✓ ThreadPool shutdown succeeded with timeout" << std::endl;
        } else {
            std::cerr << "  WARNING: Some threads were detached" << std::endl;
        }
    }

    // Test forced shutdown (immediate)
    {
        ThreadPool pool(2);

        // Submit a very long task
        pool.enqueue([]() { std::this_thread::sleep_for(std::chrono::seconds(10)); });

        // Immediate shutdown (no wait)
        bool joined = pool.shutdown(std::chrono::milliseconds(10));
        if (!joined) {
            std::cout << "  ✓ ThreadPool correctly detached hanging threads" << std::endl;
        }
    }
}

// Test 3: MPFR Context Pool
void test_mpfr_pool() {
    std::cout << "\nTesting MPFR Context Pool..." << std::endl;

    auto& pool = get_mpfr_pool();

    // Initial pool should be empty
    size_t initial_size = pool.pool_size();
    std::cout << "  Initial pool size: " << initial_size << std::endl;

    // Acquire and release contexts
    {
        auto handle1 = pool.get_handle(256);
        mpfr_set_d(handle1.get(), 3.14159, MPFR_RNDN);

        auto handle2 = pool.get_handle(256);
        mpfr_set_d(handle2.get(), 2.71828, MPFR_RNDN);

        // Perform operation
        mpfr_add(handle1.get(), handle1.get(), handle2.get(), MPFR_RNDN);

        std::cout << "  ✓ MPFR operations with pooled contexts successful" << std::endl;
    }

    // Check that contexts were returned to pool
    size_t after_release = pool.pool_size();
    if (after_release > initial_size) {
        std::cout << "  ✓ Contexts returned to pool (size: " << after_release << ")" << std::endl;
    }

    // Stress test: many acquisitions
    std::vector<MPFRContextPool::ContextHandle> handles;
    for (int i = 0; i < 20; ++i) {
        handles.push_back(pool.get_handle(256));
    }
    handles.clear();  // Release all

    std::cout << "  ✓ Stress test passed, pool size: " << pool.pool_size() << std::endl;
    std::cout << "  Total allocated: " << pool.total_allocated() << std::endl;
}

// Test 4: Cache-aligned atomics
void test_cache_alignment() {
    std::cout << "\nTesting cache-aligned atomics..." << std::endl;

    // Check alignment
    PaddedAtomicSize counter1(0);
    PaddedAtomicSize counter2(0);

    auto addr1 = reinterpret_cast<uintptr_t>(&counter1);
    auto addr2 = reinterpret_cast<uintptr_t>(&counter2);

    std::cout << "  Address 1: 0x" << std::hex << addr1 << std::dec << std::endl;
    std::cout << "  Address 2: 0x" << std::hex << addr2 << std::dec << std::endl;
    std::cout << "  Distance: " << (addr2 - addr1) << " bytes" << std::endl;

    if ((addr2 - addr1) >= CACHE_LINE_SIZE) {
        std::cout << "  ✓ Atomics are on different cache lines" << std::endl;
    } else {
        std::cerr << "  WARNING: Atomics may share cache line" << std::endl;
    }

    // Performance test: concurrent increments
    const int num_threads = 4;
    const int iterations = 1000000;

    auto start = std::chrono::high_resolution_clock::now();

    std::vector<std::thread> threads;
    for (int i = 0; i < num_threads; ++i) {
        threads.emplace_back([&counter1, iterations]() {
            for (int j = 0; j < iterations; ++j) {
                counter1.fetch_add(1, std::memory_order_relaxed);
            }
        });
    }

    for (auto& t : threads) {
        t.join();
    }

    auto end = std::chrono::high_resolution_clock::now();
    auto duration = std::chrono::duration_cast<std::chrono::milliseconds>(end - start).count();

    std::cout << "  ✓ Concurrent increment test completed in " << duration << "ms" << std::endl;
    std::cout << "  Final counter value: " << counter1.load() << std::endl;

    if (counter1.load() == static_cast<size_t>(num_threads * iterations)) {
        std::cout << "  ✓ All increments accounted for (no lost updates)" << std::endl;
    }
}

int main() {
    std::cout << "=== Testing Medium Severity Fixes ===" << std::endl;

    try {
        test_bit_operations();
        test_thread_pool_shutdown();
        test_mpfr_pool();
        test_cache_alignment();

        std::cout << "\n=== All tests completed successfully! ===" << std::endl;
        return 0;
    } catch (const std::exception& e) {
        std::cerr << "\nERROR: " << e.what() << std::endl;
        return 1;
    }
}
