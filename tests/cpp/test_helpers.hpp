#ifndef QIPRNG_TEST_HELPERS_HPP
#define QIPRNG_TEST_HELPERS_HPP

#ifdef QIPRNG_TESTING

#    include <Rcpp.h>

#    include <chrono>
#    include <string>
#    include <thread>
#    include <vector>

namespace qiprng {
namespace test {

// Test utility functions
inline void print_test_header(const std::string& test_name) {
    Rcpp::Rcout << "\n========================================\n";
    Rcpp::Rcout << "Running: " << test_name << "\n";
    Rcpp::Rcout << "========================================\n";
}

inline void print_test_result(bool passed, const std::string& message = "") {
    if (passed) {
        Rcpp::Rcout << "[PASS] ";
    } else {
        Rcpp::Rcout << "[FAIL] ";
    }
    if (!message.empty()) {
        Rcpp::Rcout << message;
    }
    Rcpp::Rcout << "\n";
}

// Timer utility for performance tests
class TestTimer {
   private:
    std::chrono::high_resolution_clock::time_point start_time;
    std::string test_name;

   public:
    TestTimer(const std::string& name) : test_name(name) {
        start_time = std::chrono::high_resolution_clock::now();
    }

    ~TestTimer() {
        auto end_time = std::chrono::high_resolution_clock::now();
        auto duration =
            std::chrono::duration_cast<std::chrono::milliseconds>(end_time - start_time).count();
        Rcpp::Rcout << test_name << " took " << duration << " ms\n";
    }
};

// Thread barrier for synchronization tests
class ThreadBarrier {
   private:
    std::mutex mutex;
    std::condition_variable cv;
    size_t count;
    size_t waiting;
    size_t generation;

   public:
    explicit ThreadBarrier(size_t count) : count(count), waiting(0), generation(0) {}

    void wait() {
        std::unique_lock<std::mutex> lock(mutex);
        size_t gen = generation;
        if (++waiting == count) {
            generation++;
            waiting = 0;
            cv.notify_all();
        } else {
            cv.wait(lock, [this, gen] { return gen != generation; });
        }
    }
};

}  // namespace test
}  // namespace qiprng

#endif  // QIPRNG_TESTING

#endif  // QIPRNG_TEST_HELPERS_HPP
