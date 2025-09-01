#ifndef QIPRNG_THREAD_MANAGER_HPP
#define QIPRNG_THREAD_MANAGER_HPP

#include <Rcpp.h>

#include <atomic>
#include <functional>
#include <memory>
#include <mutex>
#include <string>
#include <thread>
#include <unordered_map>

namespace qiprng {

// Class for safe thread resource management
class ThreadManager {
   private:
    // Singleton pattern
    static ThreadManager& instance() {
        static ThreadManager instance;
        return instance;
    }

    // Thread registration data
    struct ThreadData {
        std::atomic<bool> registered;
        std::function<void()> cleanup_callback;

        ThreadData() : registered(false), cleanup_callback(nullptr) {}
    };

    // Map of thread IDs to thread data
    std::unordered_map<std::thread::id, std::shared_ptr<ThreadData>> thread_map_;
    std::mutex thread_map_mutex_;

    // Private constructor for singleton
    ThreadManager() {}

    // Non-copyable
    ThreadManager(const ThreadManager&) = delete;
    ThreadManager& operator=(const ThreadManager&) = delete;

   public:
    // Register a thread with cleanup callback
    static bool registerThread(std::function<void()> cleanup_callback) {
        try {
            auto& manager = instance();
            std::thread::id this_id = std::this_thread::get_id();

            std::lock_guard<std::mutex> lock(manager.thread_map_mutex_);

            // Create or update thread data
            auto& thread_data = manager.thread_map_[this_id];
            if (!thread_data) {
                thread_data = std::make_shared<ThreadData>();
            }

            thread_data->registered.store(true);
            thread_data->cleanup_callback = cleanup_callback;

            return true;
        } catch (...) {
            return false;
        }
    }

    // Unregister a thread and perform cleanup
    static bool unregisterThread() {
        try {
            auto& manager = instance();
            std::thread::id this_id = std::this_thread::get_id();

            std::function<void()> cleanup_func = nullptr;

            {
                std::lock_guard<std::mutex> lock(manager.thread_map_mutex_);

                auto it = manager.thread_map_.find(this_id);
                if (it != manager.thread_map_.end() && it->second) {
                    auto thread_data = it->second;

                    if (thread_data->registered.load()) {
                        cleanup_func = thread_data->cleanup_callback;
                        thread_data->registered.store(false);
                    }
                }
            }

            // Call cleanup function outside the lock
            if (cleanup_func) {
                try {
                    cleanup_func();
                } catch (...) {
                    // Ignore cleanup errors
                }
            }

            // Actually remove the thread from map
            {
                std::lock_guard<std::mutex> lock(manager.thread_map_mutex_);
                manager.thread_map_.erase(this_id);
            }

            return true;
        } catch (...) {
            return false;
        }
    }

    // Check if current thread is registered
    static bool isThreadRegistered() {
        try {
            auto& manager = instance();
            std::thread::id this_id = std::this_thread::get_id();

            std::lock_guard<std::mutex> lock(manager.thread_map_mutex_);

            auto it = manager.thread_map_.find(this_id);
            if (it != manager.thread_map_.end() && it->second) {
                return it->second->registered.load();
            }

            return false;
        } catch (...) {
            return false;
        }
    }

    // Clean up all thread resources (call at program exit)
    static void cleanupAllThreads() {
        try {
            auto& manager = instance();

            std::vector<std::function<void()>> cleanup_funcs;

            {
                std::lock_guard<std::mutex> lock(manager.thread_map_mutex_);

                // Collect all cleanup functions
                for (auto& pair : manager.thread_map_) {
                    if (pair.second && pair.second->registered.load()) {
                        if (pair.second->cleanup_callback) {
                            cleanup_funcs.push_back(pair.second->cleanup_callback);
                        }
                        pair.second->registered.store(false);
                    }
                }

                // Clear the map
                manager.thread_map_.clear();
            }

            // Call all cleanup functions outside the lock
            for (auto& func : cleanup_funcs) {
                try {
                    func();
                } catch (...) {
                    // Ignore cleanup errors
                }
            }
        } catch (...) {
            // Ignore all errors during final cleanup
        }
    }

    // v0.5.0: Thread-local storage support for zero-contention access
    template <typename T>
    static T& getThreadLocal(const std::string& key, std::function<T()> initializer) {
        static thread_local std::unordered_map<std::string, std::shared_ptr<void>> tls_map;

        auto it = tls_map.find(key);
        if (it == tls_map.end() || !it->second) {
            auto value = std::make_shared<T>(initializer());
            tls_map[key] = std::static_pointer_cast<void>(value);
            return *std::static_pointer_cast<T>(tls_map[key]);
        }

        return *std::static_pointer_cast<T>(it->second);
    }
};

// Automatic thread registration/unregistration RAII helper
class ThreadGuard {
   private:
    bool registered_;

   public:
    explicit ThreadGuard(std::function<void()> cleanup_callback) : registered_(false) {
        registered_ = ThreadManager::registerThread(cleanup_callback);
    }

    ~ThreadGuard() {
        if (registered_) {
            ThreadManager::unregisterThread();
        }
    }

    // Non-copyable
    ThreadGuard(const ThreadGuard&) = delete;
    ThreadGuard& operator=(const ThreadGuard&) = delete;
};

}  // namespace qiprng

#endif  // QIPRNG_THREAD_MANAGER_HPP