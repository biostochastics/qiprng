// File: ziggurat_normal.hpp
// --------------------------------------------------------------
#ifndef QIPRNG_ZIGGURAT_NORMAL_HPP
#define QIPRNG_ZIGGURAT_NORMAL_HPP

#include <Rcpp.h> // For Rcpp::Rcout for debugging, remove for production
#include <vector>
#include <array>
#include <functional> // For std::function
#include <cmath>      // For std::exp, std::log, std::sqrt
#include <cstdint>    // For uint32_t
#include <limits>     // For UINT32_MAX
#include <atomic>     // For std::atomic
#include <mutex>      // For std::mutex
#include <thread>     // For std::thread
#include <future>     // For std::future and other threading utilities

#ifndef M_PI // Ensure M_PI is defined
#define M_PI 3.14159265358979323846
#endif

namespace qiprng {

// Forward declaration for TLS cleanup management
class ZigguratTLSManager;

class ZigguratNormal {
private:
    static constexpr int ZIGGURAT_TABLE_SIZE = 256; // Renamed for clarity
    static constexpr int ZIGGURAT_MASK = ZIGGURAT_TABLE_SIZE - 1;

    // Static cached tables shared by all instances
    static std::array<double, ZIGGURAT_TABLE_SIZE> cached_x_table_;
    static std::array<double, ZIGGURAT_TABLE_SIZE> cached_y_table_; // pdf at x_table_
    static std::array<uint32_t, ZIGGURAT_TABLE_SIZE> cached_k_table_; // scaled x_table_[i+1]/x_table_[i]
    static std::atomic<bool> tables_initialized_;
    static std::mutex tables_mutex_; // Mutex for thread-safe initialization
    
    // Flag to indicate if cleanup is in progress (used to avoid use-after-free)
    static std::atomic<bool> cleanup_in_progress_;
    
    // Thread-local mutex for instance methods
    std::mutex instance_mutex_; // NEW: Mutex for thread-safe instance operations

    // Instance tables - references to cached tables or local copies if parameters differ
    std::array<double, ZIGGURAT_TABLE_SIZE> x_table_;
    std::array<double, ZIGGURAT_TABLE_SIZE> y_table_; // pdf at x_table_
    std::array<uint32_t, ZIGGURAT_TABLE_SIZE> k_table_; // scaled x_table_[i+1]/x_table_[i]

    // Thread-specific local tables for concurrent access
    thread_local static std::array<double, ZIGGURAT_TABLE_SIZE> tls_x_table_;
    thread_local static std::array<double, ZIGGURAT_TABLE_SIZE> tls_y_table_;
    thread_local static std::array<uint32_t, ZIGGURAT_TABLE_SIZE> tls_k_table_;
    thread_local static bool tls_tables_initialized_;
    
    // Pre-generate random numbers to reduce mutex contention
    // This greatly reduces the need for locking during number generation
    static constexpr size_t RANDOM_CACHE_SIZE = 64;
    thread_local static std::array<double, RANDOM_CACHE_SIZE> tls_random_cache_;
    thread_local static size_t tls_random_cache_pos_;
    thread_local static bool tls_random_cache_initialized_;
    
    // TLS manager to handle thread-specific cleanup
    thread_local static ZigguratTLSManager* tls_manager_;
    
    // Atomic flag to track if TLS cleanup has been registered for this thread
    thread_local static std::atomic<bool> tls_cleanup_registered_;
    
    std::function<double()> uniform_generator_;
    double mean_;
    double stddev_;
    std::atomic<bool> is_thread_safe_mode_; // NEW: Flag for thread safe mode

    // Initialize static tables if needed
    static void initialize_static_tables();
    
    // Initialize thread-local tables
    void initialize_thread_local_tables();
    
    // Initialize and refill the thread-local random cache
    void initialize_random_cache();
    void refill_random_cache();
    
    // Get a random number from the thread-local cache (or refill if needed)
    double get_cached_uniform();
    
    // Instance methods
    void initialize_tables_original(); // Original Marsaglia & Tsang setup
    void use_cached_tables(); // Use cached tables for standard parameters
    double sample_from_tail_original(); // Original Marsaglia & Tsang tail sampling
    double sample_from_tail_thread_safe(); // Thread-safe variant of tail sampling
    double generate_internal();        // Internal generation logic
    double generate_internal_thread_safe(); // Thread-safe variant

public:
    ZigguratNormal(std::function<double()> uniform_generator,
                   double mean = 0.0, double stddev = 1.0,
                   bool thread_safe_mode = false);
    
    // Add destructor to ensure proper cleanup
    ~ZigguratNormal();

    void set_parameters(double mean, double stddev);
    void set_thread_safe_mode(bool enable); // NEW: Enable/disable thread safe mode
    bool is_thread_safe_mode() const; // NEW: Check if thread safe mode is enabled
    double generate(); // Public interface to get one number
    void generate_n(double* buffer, size_t count); // Public interface for multiple numbers
    
    // Enhanced thread-safe cleanup operations
    static void cleanup_thread_local_resources();
    static void prepare_for_shutdown();
    static bool is_cleanup_in_progress();
};

// TLS Manager to handle thread-local resources in a thread-safe way
class ZigguratTLSManager {
private:
    // Flag to indicate this manager is valid and operational
    std::atomic<bool> valid_;
    
    // Owner ZigguratNormal instance ID to prevent dangling references
    std::atomic<const void*> owner_id_;
    
    // Thread ID for which this manager is responsible
    std::thread::id thread_id_;
    
    // Cleanup already performed flag to prevent double-cleanup
    std::atomic<bool> cleanup_performed_;
    
    // Thread exit detection
    static thread_local bool thread_exiting_;
    
    // Private constructor - only the static instance() method creates instances
    ZigguratTLSManager() : 
        valid_(true), 
        owner_id_(nullptr),
        thread_id_(std::this_thread::get_id()),
        cleanup_performed_(false) {
            // Reset the thread exiting flag when creating a new manager
            thread_exiting_ = false;
        }
        
    // No copying or moving
    ZigguratTLSManager(const ZigguratTLSManager&) = delete;
    ZigguratTLSManager& operator=(const ZigguratTLSManager&) = delete;
    ZigguratTLSManager(ZigguratTLSManager&&) = delete;
    ZigguratTLSManager& operator=(ZigguratTLSManager&&) = delete;
    
public:
    // Get the singleton instance for this thread
    static ZigguratTLSManager& instance() {
        try {
            // If thread is exiting, don't create new manager
            if (thread_exiting_ || ZigguratNormal::is_cleanup_in_progress()) {
                // Use a dummy manager that's marked invalid to prevent operations
                static ZigguratTLSManager dummy_manager;
                dummy_manager.invalidate();
                return dummy_manager;
            }
            
            // Create or get thread-local manager
            static thread_local ZigguratTLSManager manager;
            return manager;
        } catch (...) {
            // If any error occurs, return an invalid manager
            static ZigguratTLSManager emergency_manager;
            emergency_manager.invalidate();
            return emergency_manager;
        }
    }
    
    // Set the owner of this thread's resources
    void set_owner(const void* owner) {
        if (valid_.load(std::memory_order_acquire)) {
            owner_id_.store(owner, std::memory_order_release);
        }
    }
    
    // Check if the manager is valid
    bool is_valid() const {
        // Check validity and that cleanup is not in progress
        return valid_.load(std::memory_order_acquire) && 
               !ZigguratNormal::is_cleanup_in_progress() &&
               !thread_exiting_;
    }
    
    // Mark thread as exiting
    static void mark_thread_exiting() {
        thread_exiting_ = true;
    }
    
    // Check if thread is exiting
    static bool is_thread_exiting() {
        return thread_exiting_;
    }
    
    // Invalidate this manager - called during cleanup
    void invalidate() {
        valid_.store(false, std::memory_order_release);
        owner_id_.store(nullptr, std::memory_order_release);
    }
    
    // Perform cleanup if needed
    void perform_cleanup_if_needed() {
        // Only clean up once and only if valid
        bool expected = false;
        if (valid_.load(std::memory_order_acquire) && 
            cleanup_performed_.compare_exchange_strong(expected, true, std::memory_order_acq_rel)) {
            // This is a safe guard to prevent calling cleanup_thread_local_resources 
            // if it's already in progress
            if (!ZigguratNormal::is_cleanup_in_progress()) {
                ZigguratNormal::cleanup_thread_local_resources();
            }
        }
    }
    
    // Destructor - automatically cleans up thread resources
    ~ZigguratTLSManager() {
        // Mark this thread as exiting to prevent new resource allocation
        thread_exiting_ = true;
        
        // Perform cleanup if not already done
        perform_cleanup_if_needed();
        
        // Ensure this manager is marked invalid
        invalidate();
    }
};

} // namespace qiprng

#endif // QIPRNG_ZIGGURAT_NORMAL_HPP