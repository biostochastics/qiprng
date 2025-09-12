// Test program to verify the fix for double-checked locking antipattern
// This test spawns multiple threads that all try to initialize the static tables simultaneously
// With the fix using std::call_once, there should be no race conditions

#include <atomic>
#include <chrono>
#include <iostream>
#include <mutex>
#include <random>
#include <thread>
#include <vector>

#include "src/ziggurat_normal.hpp"

// Counter to track how many threads are ready
std::atomic<int> threads_ready{0};
std::atomic<bool> start_flag{false};
std::mutex output_mutex;

// Thread function that tries to create and use ZigguratNormal
void thread_function(int thread_id, std::atomic<int>& success_count,
                     std::atomic<int>& failure_count) {
    try {
        // Signal that this thread is ready
        threads_ready++;

        // Wait for all threads to be ready (barrier synchronization)
        while (!start_flag.load()) {
            std::this_thread::yield();
        }

        // All threads try to initialize tables simultaneously
        // This is where the race condition would occur with double-checked locking
        auto uniform_gen = []() -> double {
            static thread_local std::mt19937 gen(std::random_device{}());
            static thread_local std::uniform_real_distribution<double> dist(0.0, 1.0);
            return dist(gen);
        };

        // Create ZigguratNormal instance - this triggers initialize_static_tables()
        qiprng::ZigguratNormal zig(uniform_gen, 0.0, 1.0, true);

        // Generate some numbers to ensure tables are actually used
        std::vector<double> samples;
        for (int i = 0; i < 100; ++i) {
            samples.push_back(zig.generate());
        }

        // Verify samples are reasonable (mean ~0, stddev ~1)
        double sum = 0.0;
        for (double s : samples) {
            sum += s;
        }
        double mean = sum / samples.size();

        double sum_sq = 0.0;
        for (double s : samples) {
            sum_sq += (s - mean) * (s - mean);
        }
        double stddev = std::sqrt(sum_sq / (samples.size() - 1));

        // Check if results are reasonable
        if (std::abs(mean) < 1.0 && stddev > 0.5 && stddev < 2.0) {
            success_count++;
            {
                std::lock_guard<std::mutex> lock(output_mutex);
                std::cout << "Thread " << thread_id << " SUCCESS: mean=" << mean
                          << ", stddev=" << stddev << std::endl;
            }
        } else {
            failure_count++;
            {
                std::lock_guard<std::mutex> lock(output_mutex);
                std::cout << "Thread " << thread_id << " FAILURE: unexpected statistics"
                          << " mean=" << mean << ", stddev=" << stddev << std::endl;
            }
        }
    } catch (const std::exception& e) {
        failure_count++;
        {
            std::lock_guard<std::mutex> lock(output_mutex);
            std::cout << "Thread " << thread_id << " EXCEPTION: " << e.what() << std::endl;
        }
    }
}

int main() {
    const int NUM_THREADS = 100;   // Large number of threads to stress test
    const int NUM_ITERATIONS = 5;  // Run multiple iterations

    std::cout << "Testing thread safety of ziggurat_normal initialization" << std::endl;
    std::cout << "Using std::call_once to fix double-checked locking antipattern" << std::endl;
    std::cout << "==========================================================" << std::endl;

    for (int iter = 0; iter < NUM_ITERATIONS; ++iter) {
        std::cout << "\nIteration " << (iter + 1) << " of " << NUM_ITERATIONS << std::endl;

        threads_ready = 0;
        start_flag = false;
        std::atomic<int> success_count{0};
        std::atomic<int> failure_count{0};

        // Create threads
        std::vector<std::thread> threads;
        for (int i = 0; i < NUM_THREADS; ++i) {
            threads.emplace_back(thread_function, i, std::ref(success_count),
                                 std::ref(failure_count));
        }

        // Wait for all threads to be ready
        while (threads_ready.load() < NUM_THREADS) {
            std::this_thread::sleep_for(std::chrono::milliseconds(1));
        }

        // Start all threads simultaneously
        std::cout << "Starting " << NUM_THREADS << " threads simultaneously..." << std::endl;
        start_flag = true;

        // Join all threads
        for (auto& t : threads) {
            t.join();
        }

        // Report results
        std::cout << "Results: " << success_count.load() << " successes, " << failure_count.load()
                  << " failures" << std::endl;

        if (failure_count.load() > 0) {
            std::cout << "WARNING: Thread safety issues detected!" << std::endl;
            return 1;
        }
    }

    std::cout << "\n==========================================================" << std::endl;
    std::cout << "SUCCESS: All threads completed without race conditions!" << std::endl;
    std::cout << "The std::call_once fix properly addresses the double-checked locking issue."
              << std::endl;

    return 0;
}
