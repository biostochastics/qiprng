// File: buffer_manager.hpp
// --------------------------------------------------------------
// Modular buffer management for PRNG
// Part of Task #21: Refactor EnhancedPRNG for Better Modularity
#ifndef QIPRNG_BUFFER_MANAGER_HPP
#define QIPRNG_BUFFER_MANAGER_HPP

#include <atomic>
#include <memory>
#include <mutex>
#include <vector>

#include "crypto_mixer.hpp"
#include "multi_qi.hpp"
#include "prng_common.hpp"
#include "thread_pool.hpp"

namespace qiprng {

// Buffer filling strategy interface
class BufferFillStrategy {
   public:
    virtual ~BufferFillStrategy() = default;
    virtual void fill(SecureBuffer<double>& buffer, MultiQI& source,
                      CryptoMixer* crypto = nullptr) = 0;
};

// Sequential buffer filling
class SequentialFillStrategy : public BufferFillStrategy {
   public:
    void fill(SecureBuffer<double>& buffer, MultiQI& source,
              CryptoMixer* crypto = nullptr) override {
        size_t buffer_size = buffer.size();
        for (size_t i = 0; i < buffer_size; ++i) {
            buffer[i] = source.next();
        }

        if (crypto && crypto->is_initialized()) {
            crypto->mix(reinterpret_cast<unsigned char*>(buffer.data()),
                        buffer_size * sizeof(double));
        }
    }
};

// Parallel buffer filling with thread pool
class ParallelFillStrategy : public BufferFillStrategy {
   private:
    size_t thread_count_;
    std::unique_ptr<ThreadPool> pool_;

   public:
    ParallelFillStrategy(size_t thread_count = 0)
        : thread_count_(thread_count ? thread_count : std::thread::hardware_concurrency()),
          pool_(std::make_unique<ThreadPool>(thread_count_)) {}

    void fill(SecureBuffer<double>& buffer, MultiQI& source,
              CryptoMixer* crypto = nullptr) override {
        size_t buffer_size = buffer.size();
        size_t chunk_size = buffer_size / thread_count_;
        std::vector<std::future<void>> futures;

        for (size_t t = 0; t < thread_count_; ++t) {
            size_t start = t * chunk_size;
            size_t end = (t == thread_count_ - 1) ? buffer_size : (t + 1) * chunk_size;

            futures.push_back(pool_->enqueue([&buffer, &source, start, end, crypto]() {
                // Create thread-local QI for parallel generation
                auto thread_qi = source.clone();
                thread_qi->skip(start);  // Position for this thread's work

                for (size_t i = start; i < end; ++i) {
                    buffer[i] = thread_qi->next();
                }

                if (crypto && crypto->is_initialized()) {
                    crypto->mix(reinterpret_cast<unsigned char*>(&buffer[start]),
                                (end - start) * sizeof(double));
                }
            }));
        }

        // Wait for all tasks to complete
        for (auto& future : futures) {
            future.get();
        }
    }
};

#ifdef _OPENMP
#    include <omp.h>
// OpenMP buffer filling strategy
class OpenMPFillStrategy : public BufferFillStrategy {
   public:
    void fill(SecureBuffer<double>& buffer, MultiQI& source,
              CryptoMixer* crypto = nullptr) override {
        size_t buffer_size = buffer.size();

#    pragma omp parallel
        {
            int thread_id = omp_get_thread_num();
            int num_threads = omp_get_num_threads();
            size_t chunk_size = buffer_size / num_threads;
            size_t start = thread_id * chunk_size;
            size_t end =
                (thread_id == num_threads - 1) ? buffer_size : (thread_id + 1) * chunk_size;

            // Create thread-local QI
            auto thread_qi = source.clone();
            thread_qi->skip(start);

            for (size_t i = start; i < end; ++i) {
                buffer[i] = thread_qi->next();
            }

            if (crypto && crypto->is_initialized()) {
#    pragma omp critical
                {
                    crypto->mix(reinterpret_cast<unsigned char*>(&buffer[start]),
                                (end - start) * sizeof(double));
                }
            }
        }
    }
};
#endif

// Buffer manager class
class BufferManager {
   private:
    SecureBuffer<double> buffer_;
    size_t buffer_pos_;
    std::unique_ptr<BufferFillStrategy> fill_strategy_;
    MultiQI* source_;
    CryptoMixer* crypto_;
    std::mutex buffer_mutex_;

   public:
    BufferManager(size_t buffer_size, MultiQI* source, CryptoMixer* crypto = nullptr)
        : buffer_(buffer_size), buffer_pos_(0), source_(source), crypto_(crypto) {
        fill_strategy_ = std::make_unique<SequentialFillStrategy>();
    }

    void setFillStrategy(std::unique_ptr<BufferFillStrategy> strategy) {
        fill_strategy_ = std::move(strategy);
    }

    void refill() {
        std::lock_guard<std::mutex> lock(buffer_mutex_);
        fill_strategy_->fill(buffer_, *source_, crypto_);
        buffer_pos_ = 0;
    }

    double next() {
        std::lock_guard<std::mutex> lock(buffer_mutex_);
        if (buffer_pos_ >= buffer_.size()) {
            refill();
        }
        return buffer_[buffer_pos_++];
    }

    void fill_n(double* output, size_t n) {
        std::lock_guard<std::mutex> lock(buffer_mutex_);
        size_t copied = 0;

        while (copied < n) {
            size_t available = buffer_.size() - buffer_pos_;
            if (available == 0) {
                refill();
                available = buffer_.size();
            }

            size_t to_copy = std::min(available, n - copied);
            std::memcpy(output + copied, &buffer_[buffer_pos_], to_copy * sizeof(double));
            buffer_pos_ += to_copy;
            copied += to_copy;
        }
    }

    void reset() {
        std::lock_guard<std::mutex> lock(buffer_mutex_);
        buffer_pos_ = 0;
        buffer_.clear();
    }

    size_t capacity() const { return buffer_.size(); }
    size_t available() const { return buffer_.size() - buffer_pos_; }
};

}  // namespace qiprng

#endif  // QIPRNG_BUFFER_MANAGER_HPP
