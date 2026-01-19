#ifndef QIPRNG_THREAD_POOL_HPP
#define QIPRNG_THREAD_POOL_HPP

#include <algorithm>
#include <atomic>
#include <chrono>
#include <condition_variable>
#include <functional>
#include <future>
#include <memory>
#include <mutex>
#include <queue>
#include <thread>
#include <type_traits>
#include <vector>

#include "cache_aligned.hpp"

namespace qiprng {

// Thread-safe queue implementation
template <typename T>
class SafeQueue {
   private:
    std::queue<T> queue_;
    mutable std::mutex mutex_;  // mutable to allow locking in const methods
    std::condition_variable cond_;
    PaddedAtomicBool done_{false};

   public:
    SafeQueue() = default;
    ~SafeQueue() { done(); }

    bool push(T value) {
        {
            std::lock_guard<std::mutex> lock(mutex_);
            if (done_)
                return false;
            queue_.push(std::move(value));
        }
        cond_.notify_one();
        return true;
    }

    bool pop(T& value) {
        std::unique_lock<std::mutex> lock(mutex_);
        cond_.wait(lock, [this]() { return !queue_.empty() || done_; });
        if (done_ && queue_.empty())
            return false;
        value = std::move(queue_.front());
        queue_.pop();
        return true;
    }

    bool empty() const {
        std::lock_guard<std::mutex> lock(mutex_);
        return queue_.empty();
    }

    void done() {
        {
            std::lock_guard<std::mutex> lock(mutex_);
            done_ = true;
        }
        cond_.notify_all();
    }

    void reset() {
        std::lock_guard<std::mutex> lock(mutex_);
        done_ = false;
    }
};

// Main ThreadPool class
class ThreadPool {
   private:
    std::vector<std::thread> workers_;
    SafeQueue<std::function<void()>> tasks_;
    PaddedAtomicBool stop_{false};
    PaddedAtomicSize active_threads_{0};
    std::condition_variable all_done_;
    std::mutex all_done_mutex_;

   public:
    // Constructor creates the thread pool with specified number of threads
    explicit ThreadPool(size_t num_threads = 0) {
        // If num_threads is 0, use hardware concurrency
        if (num_threads == 0) {
            num_threads = std::thread::hardware_concurrency();
        }

        // Limit to a reasonable maximum
        num_threads = std::min(num_threads, size_t(16));

        // At least one thread
        num_threads = std::max(num_threads, size_t(1));

        // Create worker threads
        for (size_t i = 0; i < num_threads; ++i) {
            workers_.emplace_back([this] {
                while (true) {
                    std::function<void()> task;
                    if (!tasks_.pop(task)) {
                        // Queue is done, exit thread
                        break;
                    }

                    // Execute the task
                    try {
                        active_threads_.fetch_add(1);
                        task();
                    } catch (...) {
                        // Just catch any exceptions to keep the thread alive
                    }
                    active_threads_.fetch_sub(1);

                    // Notify if all tasks are done
                    if (tasks_.empty() && active_threads_ == 0) {
                        std::lock_guard<std::mutex> lock(all_done_mutex_);
                        all_done_.notify_all();
                    }
                }
            });
        }
    }

    // Destructor - calls safe shutdown
    ~ThreadPool() { shutdown(); }

    // Add a task to the pool
    template <typename F, typename... Args>
    auto enqueue(F&& f, Args&&... args)
        -> std::future<typename std::invoke_result<F, Args...>::type> {
        using return_type = typename std::invoke_result<F, Args...>::type;

        // Create a packaged task for the function and arguments
        auto task = std::make_shared<std::packaged_task<return_type()>>(
            std::bind(std::forward<F>(f), std::forward<Args>(args)...));

        // Get the future result before we move the task
        std::future<return_type> result = task->get_future();

        // Add the task to the queue
        if (!tasks_.push([task] { (*task)(); })) {
            throw std::runtime_error("Cannot enqueue task on stopped ThreadPool");
        }

        return result;
    }

    // Wait for all tasks to complete
    void wait_all() {
        if (tasks_.empty() && active_threads_ == 0) {
            return;
        }

        std::unique_lock<std::mutex> lock(all_done_mutex_);
        all_done_.wait(lock, [this] { return tasks_.empty() && active_threads_ == 0; });
    }

    // Safe blocking shutdown - always joins all threads
    // Avoids joining current thread to prevent deadlock when called from worker
    void shutdown() {
        // Atomic exchange ensures shutdown runs only once
        bool already_stopping = stop_.exchange(true, std::memory_order_acq_rel);
        if (already_stopping) {
            return;
        }

        // Signal all threads to wake up and exit
        tasks_.done();

        // Get current thread ID to avoid self-join deadlock
        std::thread::id current_id = std::this_thread::get_id();

        // Block until all threads complete - the ONLY safe approach
        // Never use detach() as it can cause use-after-free
        for (auto& worker : workers_) {
            if (worker.joinable()) {
                // Skip joining current thread to prevent deadlock
                if (worker.get_id() == current_id) {
                    // Cannot join self - detach to avoid std::system_error
                    // The thread will complete shortly since tasks_.done() was called
                    worker.detach();
                } else {
                    worker.join();
                }
            }
        }
        workers_.clear();
    }

    // Stop the thread pool (deprecated, calls shutdown)
    void stop() { shutdown(); }

    // Timeout-aware shutdown that returns false if timeout elapses before completion
    template <typename Rep, typename Period>
    bool shutdown(std::chrono::duration<Rep, Period> timeout) {
        // Ensure shutdown initiation happens only once
        bool already_stopping = stop_.exchange(true, std::memory_order_acq_rel);
        if (!already_stopping) {
            // Prevent new tasks from being enqueued and wake all worker threads
            tasks_.done();
        }

        // Get current thread ID to avoid self-join deadlock
        std::thread::id current_id = std::this_thread::get_id();

        // Wait up to the specified timeout for all tasks to complete
        {
            std::unique_lock<std::mutex> lock(all_done_mutex_);
            bool completed_in_time = all_done_.wait_for(
                lock, timeout, [this] { return tasks_.empty() && active_threads_ == 0; });

            if (!completed_in_time) {
                // Timed out before all tasks completed; detach remaining threads
                // so shutdown can return promptly as previously documented.
                for (auto& worker : workers_) {
                    if (worker.joinable()) {
                        worker.detach();
                    }
                }
                workers_.clear();
                return false;
            }
        }

        // All tasks completed within timeout, join all threads safely
        for (auto& worker : workers_) {
            if (worker.joinable()) {
                // Skip joining the current thread to prevent deadlock
                if (worker.get_id() == current_id) {
                    worker.detach();
                } else {
                    worker.join();
                }
            }
        }
        workers_.clear();
        return true;
    }

    // Get the number of threads in the pool
    size_t size() const { return workers_.size(); }

    // Reset the pool for reuse with specified or default thread count
    void reset(size_t num_threads = 0) {
        // Always call shutdown - it handles being called multiple times safely
        // via atomic exchange. This ensures we wait for any in-progress shutdown
        // to complete before resetting.
        shutdown();

        // Reset the stop flag
        stop_.store(false, std::memory_order_release);

        // Reset the task queue
        tasks_.reset();

        // Determine thread count
        size_t target = (num_threads > 0) ? num_threads : std::thread::hardware_concurrency();
        target = std::min(target, size_t(16));  // Limit to reasonable maximum
        target = std::max(target, size_t(1));   // At least one thread

        // Create new workers
        workers_.reserve(target);
        for (size_t i = 0; i < target; ++i) {
            workers_.emplace_back([this] {
                while (true) {
                    std::function<void()> task;
                    if (!tasks_.pop(task)) {
                        // Queue is done, exit thread
                        break;
                    }

                    // Execute the task
                    try {
                        active_threads_.fetch_add(1);
                        task();
                    } catch (...) {
                        // Just catch any exceptions to keep the thread alive
                    }
                    active_threads_.fetch_sub(1);

                    // Notify if all tasks are done
                    if (tasks_.empty() && active_threads_ == 0) {
                        std::lock_guard<std::mutex> lock(all_done_mutex_);
                        all_done_.notify_all();
                    }
                }
            });
        }
    }
};

// Global thread pool accessor
inline ThreadPool& global_thread_pool() {
    // Create a persistent thread pool with hardware concurrency
    static ThreadPool pool(std::thread::hardware_concurrency());
    return pool;
}

// Explicit shutdown for global thread pool
// Call this from R's .onUnload to ensure proper cleanup before static destruction
inline void shutdown_global_thread_pool() {
    global_thread_pool().shutdown();
}

}  // namespace qiprng

#endif  // QIPRNG_THREAD_POOL_HPP
