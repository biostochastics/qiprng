// File: multi_qi.cpp
// --------------------------------------------------------------
#include "multi_qi.hpp"
#include <stdexcept> // For std::runtime_error

namespace qiprng {

MultiQI::MultiQI(const std::vector<std::tuple<long, long, long>>& abc_list, int mpfr_prec)
    : idx_(0) {
    if (abc_list.empty()) {
        throw std::runtime_error("MultiQI: abc_list cannot be empty for initialization.");
    }
    qis_.reserve(abc_list.size());
    for (const auto& abc : abc_list) {
        long A, B, C;
        std::tie(A, B, C) = abc;
        // QuadraticIrrational constructor throws on error
        qis_.push_back(std::make_unique<QuadraticIrrational>(A, B, C, mpfr_prec));
    }
}

double MultiQI::next() {
    // Critical section with robust error handling
    try {
        std::lock_guard<std::mutex> lock(mutex_);
        
        // Safety check
        if (qis_.empty()) {
            // Empty state fallback - shouldn't happen in normal operation
            return 0.5;
        }
        
        // Check index bounds
        if (idx_ >= qis_.size()) {
            idx_ = 0; // Reset to valid index
        }
        
        // Get the current QuadraticIrrational
        QuadraticIrrational* current_qi = qis_[idx_].get();
        
        // Safety check for null pointer
        if (!current_qi) {
            // Advance index and return fallback value
            idx_ = (idx_ + 1) % qis_.size();
            return 0.5;
        }
        
        // Get next value and advance index
        double val;
        try {
            val = current_qi->next();
        } catch (...) {
            // If next() throws, use fallback
            val = 0.5;
        }
        
        // Safely advance the index
        idx_ = (idx_ + 1) % qis_.size();
        
        return val;
    } catch (...) {
        // Ultimate fallback for any exception
        return 0.5;
    }
}

void MultiQI::skip(uint64_t n) {
    jump_ahead(n);
}

void MultiQI::jump_ahead(uint64_t n) {
    std::lock_guard<std::mutex> lock(mutex_);
    if (qis_.empty() || n == 0) {
        return;
    }

    size_t num_qis = qis_.size();
    uint64_t num_full_jumps = n / num_qis;
    uint64_t remaining_jump = n % num_qis;

    // Each QI advances by the number of full rotations
    if (num_full_jumps > 0) {
        for (auto& qi : qis_) {
            qi->jump_ahead(num_full_jumps);
        }
    }

    // Handle the remaining jumps by advancing the next `remaining_jump` QIs one by one
    for (uint64_t i = 0; i < remaining_jump; ++i) {
        size_t current_qi_index = (idx_ + i) % num_qis;
        qis_[current_qi_index]->jump_ahead(1);
    }

    // Update the index to the new position
    idx_ = (idx_ + remaining_jump) % num_qis;
}


size_t MultiQI::size() const {
    return qis_.size();
}

void MultiQI::fill(double* buffer, size_t fill_size) {
    // Non-thread-safe version - uses the thread-safe next() but releases the lock between calls
    if (qis_.empty()) {
        // Fallback for empty state
        for (size_t i = 0; i < fill_size; ++i) {
            buffer[i] = 0.5;
        }
        return;
    }
    
    // Fill buffer using next() which has its own lock
    for (size_t i = 0; i < fill_size; i++) {
        buffer[i] = next();
    }
}

void MultiQI::fill_thread_safe(double* buffer, size_t fill_size) {
    // Safety check for null buffer
    if (!buffer) {
        return;
    }
    
    // Hold the lock for the entire operation - more efficient for large fills
    try {
        std::lock_guard<std::mutex> lock(mutex_);
        
        if (qis_.empty()) {
            // Fallback for empty state
            for (size_t i = 0; i < fill_size; ++i) {
                buffer[i] = 0.5;
            }
            return;
        }
        
        // Make sure index is valid
        if (idx_ >= qis_.size()) {
            idx_ = 0;
        }
        
        // Fill buffer directly with values from the QIs in sequence
        for (size_t i = 0; i < fill_size; i++) {
            try {
                // Check if the QI pointer is valid
                if (qis_[idx_]) {
                    buffer[i] = qis_[idx_]->next();
                } else {
                    buffer[i] = 0.5; // Fallback for null QI
                }
            } catch (...) {
                // If next() throws, use fallback
                buffer[i] = 0.5;
            }
            
            // Safely advance the index
            idx_ = (idx_ + 1) % qis_.size();
        }
    } catch (...) {
        // Ultimate fallback - fill with constant value
        for (size_t i = 0; i < fill_size; ++i) {
            buffer[i] = 0.5;
        }
    }
}

} // namespace qiprng 