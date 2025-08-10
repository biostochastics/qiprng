#ifndef PARALLEL_JUMP_AHEAD_HPP
#define PARALLEL_JUMP_AHEAD_HPP

#include "quadratic_irrational.hpp"
#include "multi_qi.hpp"
#include "work_stealing_queue.hpp"  // v0.5.0: Work-stealing for load balancing
#include <vector>
#include <memory>
#include <thread>
#include <future>
#include <chrono>

#ifdef _OPENMP
  #include <omp.h>
#endif

namespace qiprng {

// Parallel jump-ahead for creating independent streams
class ParallelJumpAhead {
public:
    // Create n independent streams from a base QI by jumping ahead
    static std::vector<std::unique_ptr<QuadraticIrrational>> 
    create_independent_streams(const QuadraticIrrational& base_qi, 
                              size_t num_streams,
                              uint64_t jump_distance = 1000000) {
        std::vector<std::unique_ptr<QuadraticIrrational>> streams;
        streams.reserve(num_streams);
        
        // Create first stream as copy of base
        streams.push_back(std::make_unique<QuadraticIrrational>(base_qi));
        
        // Create remaining streams with parallel jump-ahead
        const size_t num_threads = std::min(num_streams - 1, 
                                           size_t(std::thread::hardware_concurrency()));
        
        if (num_threads > 1 && num_streams > 2) {
            // Parallel creation for multiple streams
            std::vector<std::future<std::unique_ptr<QuadraticIrrational>>> futures;
            
            for (size_t i = 1; i < num_streams; ++i) {
                futures.push_back(std::async(std::launch::async, 
                    [&base_qi, i, jump_distance]() {
                        auto qi = std::make_unique<QuadraticIrrational>(base_qi);
                        // Jump ahead by i * jump_distance to ensure independence
                        qi->jump_ahead_optimized(i * jump_distance);
                        return qi;
                    }));
            }
            
            // Collect results
            for (auto& future : futures) {
                streams.push_back(future.get());
            }
        } else {
            // Sequential creation for small number of streams
            for (size_t i = 1; i < num_streams; ++i) {
                auto qi = std::make_unique<QuadraticIrrational>(base_qi);
                qi->jump_ahead_optimized(i * jump_distance);
                streams.push_back(std::move(qi));
            }
        }
        
        return streams;
    }
    
    // Create independent MultiQI ensembles for parallel generation
    static std::vector<std::unique_ptr<MultiQI>>
    create_independent_ensembles(const std::vector<std::tuple<long, long, long>>& base_params,
                                int mpfr_precision,
                                size_t num_ensembles,
                                uint64_t jump_distance = 1000000,
                                MixingStrategy strategy = MixingStrategy::XOR_MIXING) {
        std::vector<std::unique_ptr<MultiQI>> ensembles;
        ensembles.reserve(num_ensembles);
        
        const size_t num_threads = std::min(num_ensembles,
                                           size_t(std::thread::hardware_concurrency()));
        
        if (num_threads > 1) {
            // Parallel creation
            std::vector<std::future<std::unique_ptr<MultiQI>>> futures;
            
            for (size_t i = 0; i < num_ensembles; ++i) {
                futures.push_back(std::async(std::launch::async,
                    [base_params, mpfr_precision, i, jump_distance, strategy]() {
                        // Create ensemble with unique seed based on index
                        auto ensemble = std::make_unique<MultiQI>(
                            base_params, mpfr_precision, 
                            i * 12345, true,  // Unique seed per ensemble
                            strategy);
                        
                        // Jump ahead each QI in the ensemble
                        for (size_t j = 0; j < ensemble->size(); ++j) {
                            ensemble->jump_ahead(j, i * jump_distance);
                        }
                        
                        return ensemble;
                    }));
            }
            
            // Collect results
            for (auto& future : futures) {
                ensembles.push_back(future.get());
            }
        } else {
            // Sequential creation
            for (size_t i = 0; i < num_ensembles; ++i) {
                auto ensemble = std::make_unique<MultiQI>(
                    base_params, mpfr_precision,
                    i * 12345, true,
                    strategy);
                
                for (size_t j = 0; j < ensemble->size(); ++j) {
                    ensemble->jump_ahead(j, i * jump_distance);
                }
                
                ensembles.push_back(std::move(ensemble));
            }
        }
        
        return ensembles;
    }
    
    // Parallel buffer filling using multiple independent streams
    static void parallel_fill_with_streams(double* buffer, size_t buffer_size,
                                          std::vector<std::unique_ptr<MultiQI>>& ensembles) {
        if (ensembles.empty() || buffer_size == 0) return;
        
        const size_t num_streams = ensembles.size();
        const size_t chunk_size = buffer_size / num_streams;
        const size_t remainder = buffer_size % num_streams;
        
        // Parallel fill using all streams
        std::vector<std::future<void>> futures;
        
        for (size_t i = 0; i < num_streams; ++i) {
            size_t start = i * chunk_size;
            size_t size = (i == num_streams - 1) ? chunk_size + remainder : chunk_size;
            
            futures.push_back(std::async(std::launch::async,
                [&ensembles, i, buffer, start, size]() {
                    ensembles[i]->fill_thread_safe(buffer + start, size);
                }));
        }
        
        // Wait for all fills to complete
        for (auto& future : futures) {
            future.wait();
        }
    }
    
    // v0.5.0: Parallel fill with work-stealing for load balancing
    static void parallel_fill_with_work_stealing(double* buffer, size_t buffer_size,
                                                 std::vector<std::unique_ptr<MultiQI>>& ensembles) {
        if (ensembles.empty() || buffer_size == 0) return;
        
        const size_t num_threads = ensembles.size();
        WorkStealingPool work_pool(num_threads);
        
        // Create work items with varying sizes to simulate uneven load
        const size_t min_chunk = 256;
        const size_t num_chunks = std::max(size_t(num_threads * 4), 
                                          buffer_size / min_chunk);
        
        // Distribute work items across threads
        size_t current_pos = 0;
        for (size_t i = 0; i < num_chunks && current_pos < buffer_size; ++i) {
            // Vary chunk sizes to create load imbalance (testing work-stealing)
            size_t chunk_size = min_chunk;
            if (i % 3 == 0) chunk_size *= 2;  // Some chunks are larger
            if (i % 5 == 0) chunk_size *= 3;  // Some are even larger
            
            chunk_size = std::min(chunk_size, buffer_size - current_pos);
            
            WorkItem item;
            item.start_idx = current_pos;
            item.end_idx = current_pos + chunk_size;
            item.thread_id = i % num_threads;
            
            // Capture by value to avoid lifetime issues
            item.task = [buffer, &ensembles](size_t start, size_t end) {
                // Use round-robin to select ensemble
                size_t ensemble_idx = start % ensembles.size();
                ensembles[ensemble_idx]->fill_thread_safe(buffer + start, end - start);
            };
            
            work_pool.submit(item.thread_id, std::move(item));
            current_pos += chunk_size;
        }
        
        // Launch worker threads
        std::vector<std::thread> workers;
        workers.reserve(num_threads);
        
        std::atomic<size_t> completed_items{0};
        
        for (size_t tid = 0; tid < num_threads; ++tid) {
            workers.emplace_back([&work_pool, tid, &completed_items]() {
                size_t local_completed = 0;
                
                while (!work_pool.is_done()) {
                    auto work = work_pool.get_work(tid);
                    if (work) {
                        work->task(work->start_idx, work->end_idx);
                        local_completed++;
                    } else if (work_pool.all_empty()) {
                        break;
                    } else {
                        // Exponential backoff when no work available
                        std::this_thread::sleep_for(
                            std::chrono::microseconds(1 << std::min(tid % 5, size_t(4)))
                        );
                    }
                }
                
                completed_items.fetch_add(local_completed);
            });
        }
        
        // Wait for completion
        for (auto& worker : workers) {
            worker.join();
        }
        
        work_pool.shutdown();
    }
    
#ifdef _OPENMP
    // v0.5.0: OpenMP-based parallel fill
    static void parallel_fill_openmp(double* buffer, size_t buffer_size,
                                     std::vector<std::unique_ptr<MultiQI>>& ensembles) {
        if (ensembles.empty() || buffer_size == 0) return;
        
        #pragma omp parallel for schedule(dynamic, 256)
        for (size_t i = 0; i < buffer_size; i += 256) {
            size_t chunk_size = std::min(size_t(256), buffer_size - i);
            int tid = omp_get_thread_num();
            size_t ensemble_idx = tid % ensembles.size();
            ensembles[ensemble_idx]->fill_thread_safe(buffer + i, chunk_size);
        }
    }
#endif
};

} // namespace qiprng

#endif // PARALLEL_JUMP_AHEAD_HPP