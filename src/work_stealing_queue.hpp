#ifndef WORK_STEALING_QUEUE_HPP
#define WORK_STEALING_QUEUE_HPP

#include <atomic>
#include <deque>
#include <mutex>
#include <optional>
#include <functional>
#include <vector>
#include <thread>
#include "cache_aligned.hpp"

namespace qiprng {

// Work item for parallel generation
struct WorkItem {
    size_t start_idx;
    size_t end_idx;
    size_t thread_id;
    std::function<void(size_t, size_t)> task;
};

// Lock-free work-stealing queue for load balancing
class WorkStealingQueue {
private:
    std::deque<WorkItem> queue_;
    mutable std::mutex mutex_;
    PaddedAtomicSize size_{0};
    
public:
    WorkStealingQueue() = default;
    
    // Push work to the back (owner thread)
    void push(WorkItem item) {
        std::lock_guard<std::mutex> lock(mutex_);
        queue_.push_back(std::move(item));
        size_.fetch_add(1);
    }
    
    // Try to pop from the front (owner thread)
    std::optional<WorkItem> try_pop() {
        std::lock_guard<std::mutex> lock(mutex_);
        if (queue_.empty()) {
            return std::nullopt;
        }
        WorkItem item = std::move(queue_.front());
        queue_.pop_front();
        size_.fetch_sub(1);
        return item;
    }
    
    // Try to steal from the back (other threads)
    std::optional<WorkItem> try_steal() {
        std::lock_guard<std::mutex> lock(mutex_);
        if (queue_.empty()) {
            return std::nullopt;
        }
        WorkItem item = std::move(queue_.back());
        queue_.pop_back();
        size_.fetch_sub(1);
        return item;
    }
    
    bool empty() const {
        return size_.load() == 0;
    }
    
    size_t size() const {
        return size_.load();
    }
};

// Thread-local work queue with stealing capability
class WorkStealingPool {
private:
    std::vector<std::unique_ptr<WorkStealingQueue>> queues_;
    PaddedAtomicBool done_{false};
    size_t num_threads_;
    
public:
    explicit WorkStealingPool(size_t num_threads) 
        : num_threads_(num_threads) {
        queues_.reserve(num_threads);
        for (size_t i = 0; i < num_threads; ++i) {
            queues_.emplace_back(std::make_unique<WorkStealingQueue>());
        }
    }
    
    // Submit work to a specific thread's queue
    void submit(size_t thread_id, WorkItem item) {
        if (thread_id < queues_.size()) {
            queues_[thread_id]->push(std::move(item));
        }
    }
    
    // Get work for a thread (with work stealing)
    std::optional<WorkItem> get_work(size_t thread_id) {
        // First try own queue
        if (thread_id < queues_.size()) {
            auto item = queues_[thread_id]->try_pop();
            if (item) return item;
        }
        
        // Try to steal from other queues
        for (size_t attempts = 0; attempts < num_threads_ * 2; ++attempts) {
            size_t victim = (thread_id + attempts + 1) % num_threads_;
            if (victim < queues_.size()) {
                auto item = queues_[victim]->try_steal();
                if (item) return item;
            }
        }
        
        return std::nullopt;
    }
    
    bool all_empty() const {
        for (const auto& queue : queues_) {
            if (!queue->empty()) return false;
        }
        return true;
    }
    
    void shutdown() {
        done_.store(true, std::memory_order_release);
    }
    
    bool is_done() const {
        return done_.load(std::memory_order_acquire);
    }
};

} // namespace qiprng

#endif // WORK_STEALING_QUEUE_HPP