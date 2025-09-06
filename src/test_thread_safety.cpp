#include <Rcpp.h>

#include "enhanced_prng.hpp"
#include "prng_common.hpp"
#include "prng_utils.hpp"

// Forward declaration using namespace
namespace qiprng {
void loadCSVDiscriminants();
}

// [[Rcpp::export]]
Rcpp::List test_choose_discriminant(int thread_count = 4, int iterations = 10) {
    Rcpp::Rcout << "\nTesting with " << thread_count << " threads and " << iterations
                << " iterations" << std::endl;

    // Initialize vector to store results from each thread
    std::vector<std::vector<long long>> results(thread_count);

    // Initialize libsodium first
    qiprng::initialize_libsodium_if_needed();

    // Make sure CSV discriminants are loaded first in main thread
    // This prevents race conditions when multiple threads try to load it
    qiprng::loadCSVDiscriminants();

    // Create threads
    std::vector<std::thread> threads;

    // Starting barrier
    std::atomic<int> ready_count(0);
    std::atomic<bool> start_flag(false);

    // Clear the global used discriminants set before starting test
    {
        std::lock_guard<std::mutex> lock(qiprng::g_disc_mutex);
        qiprng::g_used_discriminants.clear();
    }

    Rcpp::Rcout << "Checking CSV discriminants are loaded..." << std::endl;
    size_t csv_size = 0;
    {
        std::lock_guard<std::mutex> lock(qiprng::g_csv_disc_mutex);
        csv_size = qiprng::g_csv_discriminants.size();
    }
    Rcpp::Rcout << "CSV discriminants size: " << csv_size << std::endl;

    // Force CSV discriminants to be generated if empty
    if (csv_size == 0) {
        Rcpp::Rcout << "CSV discriminants is empty. Generating default values." << std::endl;

        {
            std::lock_guard<std::mutex> lock(qiprng::g_csv_disc_mutex);
            for (int i = 0; i < 100; i++) {
                long a = 1 + (i % 10);
                long b = 3 + 2 * (i % 8);
                long c = -1 - (i % 5);
                // Use safe discriminant calculation
                long long disc;
                std::string error_msg;
                if (qiprng::safe_calculate_discriminant(a, b, c, disc, error_msg) && disc > 0) {
                    qiprng::g_csv_discriminants.emplace_back(a, b, c, disc);
                }
            }
            Rcpp::Rcout << "Generated " << qiprng::g_csv_discriminants.size()
                        << " default discriminants" << std::endl;
        }
    }

    // Launch threads with additional safety and debug
    for (int t = 0; t < thread_count; t++) {
        Rcpp::Rcout << "Creating thread " << t << std::endl;
        try {
            threads.push_back(std::thread([t, iterations, &results, &ready_count, &start_flag]() {
                try {
                    Rcpp::Rcout << "Thread " << t << " starting" << std::endl;

                    // Wait for all threads to be ready
                    ready_count++;
                    while (!start_flag.load()) {
                        std::this_thread::yield();
                    }

                    Rcpp::Rcout << "Thread " << t << " running" << std::endl;

                    // Choose discriminants
                    results[t].reserve(iterations);
                    for (int i = 0; i < iterations; i++) {
                        try {
                            // Use a wider range to avoid collisions between threads
                            long long discriminant = qiprng::chooseUniqueDiscriminant(5, 10000000);

                            if (discriminant > 0) {
                                Rcpp::Rcout << "Thread " << t << " selected discriminant " << i
                                            << ": " << discriminant << std::endl;
                                results[t].push_back(discriminant);
                            } else {
                                Rcpp::Rcout << "Thread " << t
                                            << " got invalid discriminant: " << discriminant
                                            << std::endl;
                                results[t].push_back(-1);  // Error indicator
                            }
                        } catch (const std::exception& e) {
                            Rcpp::Rcout << "Thread " << t << " encountered error in iteration " << i
                                        << ": " << e.what() << std::endl;
                            results[t].push_back(-1);  // Error indicator
                        } catch (...) {
                            Rcpp::Rcout << "Thread " << t
                                        << " encountered unknown error in iteration " << i
                                        << std::endl;
                            results[t].push_back(-1);  // Error indicator
                        }
                    }

                    Rcpp::Rcout << "Thread " << t << " completed with " << results[t].size()
                                << " discriminants" << std::endl;

                } catch (const std::exception& e) {
                    Rcpp::Rcout << "Thread " << t << " failed with exception: " << e.what()
                                << std::endl;
                } catch (...) {
                    Rcpp::Rcout << "Thread " << t << " failed with unknown exception" << std::endl;
                }
            }));
        } catch (const std::exception& e) {
            Rcpp::Rcout << "Failed to create thread " << t << ": " << e.what() << std::endl;
        } catch (...) {
            Rcpp::Rcout << "Failed to create thread " << t << " with unknown error" << std::endl;
        }
    }

    // Wait for all threads to be ready
    while (ready_count.load() < thread_count) {
        std::this_thread::sleep_for(std::chrono::milliseconds(10));
    }

    // Start all threads at once
    start_flag.store(true);

    // Wait for all threads to complete
    for (auto& t : threads) {
        if (t.joinable()) {
            t.join();
        }
    }

    // Print summary of results
    int total_count = 0;
    for (int t = 0; t < thread_count; t++) {
        total_count += results[t].size();
        Rcpp::Rcout << "Thread " << t << " generated " << results[t].size() << " discriminants"
                    << std::endl;
    }
    Rcpp::Rcout << "Total discriminants: " << total_count << std::endl;

    // Convert results to R list
    Rcpp::List r_results(thread_count);
    for (int t = 0; t < thread_count; t++) {
        r_results[t] = Rcpp::NumericVector(results[t].begin(), results[t].end());
    }

    return r_results;
}

// [[Rcpp::export]]
bool check_discriminants_unique(Rcpp::List discriminant_lists) {
    std::unordered_set<long long> all_discriminants;

    for (int i = 0; i < discriminant_lists.size(); i++) {
        Rcpp::NumericVector discr_vec = discriminant_lists[i];

        for (int j = 0; j < discr_vec.size(); j++) {
            long long d = static_cast<long long>(discr_vec[j]);
            if (d == -1)
                continue;  // Skip error indicators

            if (all_discriminants.find(d) != all_discriminants.end()) {
                // Found duplicate
                Rcpp::Rcout << "Duplicate discriminant found: " << d << std::endl;
                return false;
            }

            all_discriminants.insert(d);
        }
    }

    Rcpp::Rcout << "All discriminants are unique (" << all_discriminants.size() << " total)"
                << std::endl;
    return true;
}
