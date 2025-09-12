// Test program to verify thread safety fixes in ZigguratNormal
#include <atomic>
#include <chrono>
#include <cmath>
#include <iostream>
#include <random>
#include <thread>
#include <vector>

// Mock implementations for testing outside R environment
namespace Rcpp {
std::ostream& Rcout = std::cout;
}

// Include the actual implementation
#include "src/ziggurat_normal.cpp"

// Test harness
class ThreadSafetyTester {
   private:
    static std::atomic<int> thread_counter;
    static std::atomic<int> error_counter;
    static std::atomic<bool> stop_flag;

   public:
    static void stress_test_thread(int thread_id, int iterations) {
        try {
            thread_counter.fetch_add(1);

            // Create a uniform generator for this thread
            std::mt19937 rng(thread_id);
            std::uniform_real_distribution<double> uniform(0.0, 1.0);
            auto uniform_gen = [&]() { return uniform(rng); };

            // Create ZigguratNormal instance with thread-safe mode
            qiprng::ZigguratNormal ziggurat(uniform_gen, 0.0, 1.0, true);

            // Generate many random numbers
            std::vector<double> samples;
            samples.reserve(iterations);

            for (int i = 0; i < iterations && !stop_flag.load(); ++i) {
                double val = ziggurat.generate();
                samples.push_back(val);

                // Periodically yield to increase thread contention
                if (i % 100 == 0) {
                    std::this_thread::yield();
                }
            }

            // Basic statistical validation
            double sum = 0.0, sum_sq = 0.0;
            for (double val : samples) {
                sum += val;
                sum_sq += val * val;
            }

            double mean = sum / samples.size();
            double var = (sum_sq / samples.size()) - (mean * mean);
            double stddev = std::sqrt(var);

            // Check if results are reasonable (mean ~0, stddev ~1)
            if (std::abs(mean) > 0.1 || std::abs(stddev - 1.0) > 0.1) {
                std::cerr << "Thread " << thread_id << " statistical check failed: "
                          << "mean=" << mean << ", stddev=" << stddev << std::endl;
                error_counter.fetch_add(1);
            }

            thread_counter.fetch_sub(1);

        } catch (const std::exception& e) {
            std::cerr << "Thread " << thread_id << " exception: " << e.what() << std::endl;
            error_counter.fetch_add(1);
            thread_counter.fetch_sub(1);
        }
    }

    static void cleanup_test_thread(int thread_id) {
        try {
            thread_counter.fetch_add(1);

            // Create and destroy multiple instances rapidly
            for (int i = 0; i < 100 && !stop_flag.load(); ++i) {
                std::mt19937 rng(thread_id * 1000 + i);
                std::uniform_real_distribution<double> uniform(0.0, 1.0);
                auto uniform_gen = [&]() { return uniform(rng); };

                {
                    qiprng::ZigguratNormal ziggurat(uniform_gen, 0.0, 1.0, true);
                    // Generate a few numbers
                    for (int j = 0; j < 10; ++j) {
                        ziggurat.generate();
                    }
                }  // Destructor called here

                // Force cleanup
                qiprng::ZigguratNormal::cleanup_thread_local_resources();
            }

            thread_counter.fetch_sub(1);

        } catch (const std::exception& e) {
            std::cerr << "Cleanup thread " << thread_id << " exception: " << e.what() << std::endl;
            error_counter.fetch_add(1);
            thread_counter.fetch_sub(1);
        }
    }

    static bool run_tests() {
        std::cout << "=== Thread Safety Test Suite ===" << std::endl;

        // Test 1: Basic multi-threaded generation
        std::cout << "\nTest 1: Multi-threaded generation..." << std::endl;
        {
            stop_flag.store(false);
            error_counter.store(0);

            const int num_threads = 8;
            const int iterations = 10000;
            std::vector<std::thread> threads;

            for (int i = 0; i < num_threads; ++i) {
                threads.emplace_back(stress_test_thread, i, iterations);
            }

            for (auto& t : threads) {
                t.join();
            }

            if (error_counter.load() == 0) {
                std::cout << "✓ Multi-threaded generation test passed" << std::endl;
            } else {
                std::cout << "✗ Multi-threaded generation test failed with " << error_counter.load()
                          << " errors" << std::endl;
                return false;
            }
        }

        // Test 2: Rapid cleanup stress test
        std::cout << "\nTest 2: Rapid cleanup stress test..." << std::endl;
        {
            stop_flag.store(false);
            error_counter.store(0);

            const int num_threads = 4;
            std::vector<std::thread> threads;

            for (int i = 0; i < num_threads; ++i) {
                threads.emplace_back(cleanup_test_thread, i);
            }

            // Let it run for a bit
            std::this_thread::sleep_for(std::chrono::seconds(2));

            // Signal stop and prepare for shutdown
            stop_flag.store(true);
            qiprng::ZigguratNormal::prepare_for_shutdown();

            for (auto& t : threads) {
                t.join();
            }

            if (error_counter.load() == 0) {
                std::cout << "✓ Rapid cleanup test passed" << std::endl;
            } else {
                std::cout << "✗ Rapid cleanup test failed with " << error_counter.load()
                          << " errors" << std::endl;
                return false;
            }
        }

        // Test 3: Thread exit handling
        std::cout << "\nTest 3: Thread exit handling..." << std::endl;
        {
            stop_flag.store(false);
            error_counter.store(0);

            // Create threads that exit quickly
            for (int i = 0; i < 20; ++i) {
                std::thread t([i]() {
                    try {
                        std::mt19937 rng(i);
                        std::uniform_real_distribution<double> uniform(0.0, 1.0);
                        auto uniform_gen = [&]() { return uniform(rng); };

                        qiprng::ZigguratNormal ziggurat(uniform_gen, 0.0, 1.0, true);
                        ziggurat.generate();
                        // Thread exits immediately after one generation
                    } catch (...) {
                        error_counter.fetch_add(1);
                    }
                });
                t.detach();  // Detach to simulate uncontrolled thread exit
            }

            // Wait for threads to finish
            std::this_thread::sleep_for(std::chrono::milliseconds(500));

            if (error_counter.load() == 0) {
                std::cout << "✓ Thread exit handling test passed" << std::endl;
            } else {
                std::cout << "✗ Thread exit handling test failed with " << error_counter.load()
                          << " errors" << std::endl;
                return false;
            }
        }

        std::cout << "\n=== All tests completed successfully ===" << std::endl;
        return true;
    }
};

// Initialize static members
std::atomic<int> ThreadSafetyTester::thread_counter{0};
std::atomic<int> ThreadSafetyTester::error_counter{0};
std::atomic<bool> ThreadSafetyTester::stop_flag{false};

int main() {
    try {
        bool success = ThreadSafetyTester::run_tests();
        return success ? 0 : 1;
    } catch (const std::exception& e) {
        std::cerr << "Fatal error: " << e.what() << std::endl;
        return 2;
    }
}
