#ifndef QIPRNG_MPFR_POOL_HPP
#define QIPRNG_MPFR_POOL_HPP

#include <mpfr.h>

#include <chrono>
#include <memory>
#include <mutex>
#include <queue>

namespace qiprng {

// MPFR context wrapper with timestamp for cleanup
struct MPFRContext {
    mpfr_t value;
    std::chrono::steady_clock::time_point last_used;
    mpfr_prec_t precision;

    MPFRContext(mpfr_prec_t prec) : precision(prec) {
        mpfr_init2(value, prec);
        last_used = std::chrono::steady_clock::now();
    }

    ~MPFRContext() { mpfr_clear(value); }

    void touch() { last_used = std::chrono::steady_clock::now(); }

    bool is_stale(std::chrono::seconds timeout) const {
        auto now = std::chrono::steady_clock::now();
        return (now - last_used) > timeout;
    }
};

// Thread-local MPFR context pool
class MPFRContextPool {
   private:
    std::queue<std::unique_ptr<MPFRContext>> available_;
    std::mutex mutex_;
    mpfr_prec_t default_precision_;
    size_t max_pool_size_;
    size_t total_allocated_;
    std::chrono::steady_clock::time_point last_cleanup_;

    static constexpr size_t DEFAULT_MAX_POOL_SIZE = 8;
    static constexpr std::chrono::seconds CLEANUP_INTERVAL{30};
    static constexpr std::chrono::seconds STALE_TIMEOUT{60};

   public:
    explicit MPFRContextPool(mpfr_prec_t default_prec = 256)
        : default_precision_(default_prec), max_pool_size_(DEFAULT_MAX_POOL_SIZE),
          total_allocated_(0), last_cleanup_(std::chrono::steady_clock::now()) {}

    ~MPFRContextPool() {
        // Clear all contexts
        while (!available_.empty()) {
            available_.pop();
        }
        // Clean up thread-local MPFR cache
        mpfr_free_cache();
    }

    // Get a context from the pool or create a new one
    std::unique_ptr<MPFRContext> acquire(mpfr_prec_t precision = 0) {
        if (precision == 0) {
            precision = default_precision_;
        }

        std::lock_guard<std::mutex> lock(mutex_);

        // Periodic cleanup of stale contexts
        maybe_cleanup();

        // Try to find a context with matching precision
        std::unique_ptr<MPFRContext> context;
        std::queue<std::unique_ptr<MPFRContext>> temp_queue;

        while (!available_.empty()) {
            auto ctx = std::move(available_.front());
            available_.pop();

            if (!context && ctx->precision == precision) {
                context = std::move(ctx);
                context->touch();
            } else {
                temp_queue.push(std::move(ctx));
            }
        }

        // Put back the contexts we didn't use
        while (!temp_queue.empty()) {
            available_.push(std::move(temp_queue.front()));
            temp_queue.pop();
        }

        // If no suitable context found, create a new one
        if (!context) {
            context = std::make_unique<MPFRContext>(precision);
            ++total_allocated_;
        }

        return context;
    }

    // Return a context to the pool
    void release(std::unique_ptr<MPFRContext> context) {
        if (!context)
            return;

        std::lock_guard<std::mutex> lock(mutex_);

        // Reset the value to zero for cleanliness
        mpfr_set_zero(context->value, 1);
        context->touch();

        // Only keep if pool isn't full
        if (available_.size() < max_pool_size_) {
            available_.push(std::move(context));
        } else {
            // Let it be destroyed
            --total_allocated_;
        }
    }

    // RAII wrapper for automatic return to pool
    class ContextHandle {
       private:
        std::unique_ptr<MPFRContext> context_;
        MPFRContextPool* pool_;

       public:
        ContextHandle(std::unique_ptr<MPFRContext> ctx, MPFRContextPool* pool)
            : context_(std::move(ctx)), pool_(pool) {}

        ~ContextHandle() {
            if (context_ && pool_) {
                pool_->release(std::move(context_));
            }
        }

        // Move-only semantics
        ContextHandle(ContextHandle&& other) noexcept
            : context_(std::move(other.context_)), pool_(other.pool_) {
            other.pool_ = nullptr;
        }

        ContextHandle& operator=(ContextHandle&& other) noexcept {
            if (this != &other) {
                if (context_ && pool_) {
                    pool_->release(std::move(context_));
                }
                context_ = std::move(other.context_);
                pool_ = other.pool_;
                other.pool_ = nullptr;
            }
            return *this;
        }

        // Deleted copy operations
        ContextHandle(const ContextHandle&) = delete;
        ContextHandle& operator=(const ContextHandle&) = delete;

        // Access the MPFR value
        mpfr_t& get() { return context_->value; }
        const mpfr_t& get() const { return context_->value; }
        mpfr_ptr ptr() { return context_->value; }
        mpfr_srcptr srcptr() const { return context_->value; }
    };

    // Get a RAII handle that automatically returns to pool
    ContextHandle get_handle(mpfr_prec_t precision = 0) {
        return ContextHandle(acquire(precision), this);
    }

    // Get pool statistics
    size_t pool_size() const {
        std::lock_guard<std::mutex> lock(const_cast<std::mutex&>(mutex_));
        return available_.size();
    }

    size_t total_allocated() const { return total_allocated_; }

   private:
    void maybe_cleanup() {
        auto now = std::chrono::steady_clock::now();
        if ((now - last_cleanup_) < CLEANUP_INTERVAL) {
            return;
        }

        last_cleanup_ = now;

        // Remove stale contexts
        std::queue<std::unique_ptr<MPFRContext>> fresh_contexts;
        while (!available_.empty()) {
            auto ctx = std::move(available_.front());
            available_.pop();

            if (!ctx->is_stale(STALE_TIMEOUT)) {
                fresh_contexts.push(std::move(ctx));
            } else {
                --total_allocated_;
            }
        }

        available_ = std::move(fresh_contexts);

        // Free thread-local MPFR cache periodically
        mpfr_free_cache();
    }
};

// Thread-local pool instance
inline MPFRContextPool& get_mpfr_pool() {
    thread_local MPFRContextPool pool;
    return pool;
}

}  // namespace qiprng

#endif  // QIPRNG_MPFR_POOL_HPP
