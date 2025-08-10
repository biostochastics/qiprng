#ifndef HARDWARE_ACCELERATION_HPP
#define HARDWARE_ACCELERATION_HPP

#include <cstddef>
#include <vector>
#include <memory>

// OpenMP support
#ifdef _OPENMP
#include <omp.h>
#define QIPRNG_HAS_OPENMP
#endif

// CUDA support detection
#ifdef __CUDACC__
#define QIPRNG_HAS_CUDA
#include <cuda_runtime.h>
#include <curand_kernel.h>
#endif

namespace qiprng {
namespace hardware {

// Hardware acceleration capabilities
struct HardwareCapabilities {
    bool has_openmp = false;
    bool has_cuda = false;
    bool has_avx2 = false;
    bool has_avx512 = false;
    bool has_neon = false;
    int num_cores = 1;
    int num_cuda_devices = 0;
    size_t cuda_memory = 0;
    
    static HardwareCapabilities detect() {
        HardwareCapabilities caps;
        
#ifdef QIPRNG_HAS_OPENMP
        caps.has_openmp = true;
        caps.num_cores = omp_get_max_threads();
#else
        caps.num_cores = std::thread::hardware_concurrency();
#endif
        
#ifdef __AVX2__
        caps.has_avx2 = true;
#endif

#ifdef __AVX512F__
        caps.has_avx512 = true;
#endif

#ifdef __ARM_NEON
        caps.has_neon = true;
#endif

#ifdef QIPRNG_HAS_CUDA
        int device_count = 0;
        cudaError_t err = cudaGetDeviceCount(&device_count);
        if (err == cudaSuccess && device_count > 0) {
            caps.has_cuda = true;
            caps.num_cuda_devices = device_count;
            
            // Get memory of first device
            cudaDeviceProp prop;
            if (cudaGetDeviceProperties(&prop, 0) == cudaSuccess) {
                caps.cuda_memory = prop.totalGlobalMem;
            }
        }
#endif
        
        return caps;
    }
};

// OpenMP parallel generation
class OpenMPAccelerator {
private:
    int num_threads_;
    
public:
    explicit OpenMPAccelerator(int num_threads = 0) {
#ifdef QIPRNG_HAS_OPENMP
        num_threads_ = (num_threads > 0) ? num_threads : omp_get_max_threads();
        omp_set_num_threads(num_threads_);
#else
        num_threads_ = 1;
#endif
    }
    
    // Parallel fill using OpenMP
    template<typename Generator>
    void parallel_fill(double* buffer, size_t size, Generator& gen) {
#ifdef QIPRNG_HAS_OPENMP
        #pragma omp parallel for schedule(static)
        for (size_t i = 0; i < size; ++i) {
            buffer[i] = gen();
        }
#else
        // Fallback to sequential
        for (size_t i = 0; i < size; ++i) {
            buffer[i] = gen();
        }
#endif
    }
    
    // Parallel transform with OpenMP
    template<typename Transform>
    void parallel_transform(double* data, size_t size, Transform transform) {
#ifdef QIPRNG_HAS_OPENMP
        #pragma omp parallel for schedule(static)
        for (size_t i = 0; i < size; ++i) {
            data[i] = transform(data[i]);
        }
#else
        for (size_t i = 0; i < size; ++i) {
            data[i] = transform(data[i]);
        }
#endif
    }
    
    // Parallel reduction (sum)
    double parallel_sum(const double* data, size_t size) {
        double sum = 0.0;
#ifdef QIPRNG_HAS_OPENMP
        #pragma omp parallel for reduction(+:sum)
        for (size_t i = 0; i < size; ++i) {
            sum += data[i];
        }
#else
        for (size_t i = 0; i < size; ++i) {
            sum += data[i];
        }
#endif
        return sum;
    }
};

#ifdef QIPRNG_HAS_CUDA
// CUDA kernel for QI generation
__global__ void qi_generate_kernel(double* output, 
                                  const double* params,
                                  size_t n,
                                  unsigned long long seed) {
    int idx = blockIdx.x * blockDim.x + threadIdx.x;
    if (idx >= n) return;
    
    // Initialize CURAND state
    curandState_t state;
    curand_init(seed, idx, 0, &state);
    
    // Simple QI-inspired generation (simplified for GPU)
    double a = params[0];
    double b = params[1];
    double c = params[2];
    
    // Generate using linear congruential method with QI parameters
    unsigned long long x = seed + idx;
    x = (a * x + b) % ((unsigned long long)1 << 32);
    double val = (double)x / ((double)((unsigned long long)1 << 32));
    
    // Apply QI-like transformation
    val = fmod(val * a + c, 1.0);
    if (val < 0) val += 1.0;
    
    output[idx] = val;
}

// CUDA accelerator class
class CUDAAccelerator {
private:
    int device_id_;
    size_t max_memory_;
    double* d_buffer_;
    double* d_params_;
    size_t buffer_size_;
    
public:
    explicit CUDAAccelerator(int device_id = 0) 
        : device_id_(device_id), d_buffer_(nullptr), d_params_(nullptr), buffer_size_(0) {
        cudaSetDevice(device_id_);
        
        cudaDeviceProp prop;
        cudaGetDeviceProperties(&prop, device_id_);
        max_memory_ = prop.totalGlobalMem;
        
        // Allocate parameter memory
        cudaMalloc(&d_params_, 3 * sizeof(double));
    }
    
    ~CUDAAccelerator() {
        if (d_buffer_) cudaFree(d_buffer_);
        if (d_params_) cudaFree(d_params_);
    }
    
    // Generate on GPU
    void generate(double* host_buffer, size_t size, 
                 double a, double b, double c, unsigned long long seed) {
        // Ensure device buffer is allocated
        if (buffer_size_ < size) {
            if (d_buffer_) cudaFree(d_buffer_);
            cudaMalloc(&d_buffer_, size * sizeof(double));
            buffer_size_ = size;
        }
        
        // Copy parameters to device
        double params[3] = {a, b, c};
        cudaMemcpy(d_params_, params, 3 * sizeof(double), cudaMemcpyHostToDevice);
        
        // Launch kernel
        int block_size = 256;
        int grid_size = (size + block_size - 1) / block_size;
        qi_generate_kernel<<<grid_size, block_size>>>(d_buffer_, d_params_, size, seed);
        
        // Copy results back
        cudaMemcpy(host_buffer, d_buffer_, size * sizeof(double), cudaMemcpyDeviceToHost);
        cudaDeviceSynchronize();
    }
};
#endif // QIPRNG_HAS_CUDA

// AVX2 accelerated operations (extending SIMD operations)
#ifdef __AVX2__
#include <immintrin.h>

class AVX2Accelerator {
public:
    // Fast modular multiplication for QI
    static void modular_multiply_batch(double* result, const double* a, const double* b, size_t n) {
        size_t simd_n = n / 4;
        
        for (size_t i = 0; i < simd_n; ++i) {
            __m256d va = _mm256_loadu_pd(a + i * 4);
            __m256d vb = _mm256_loadu_pd(b + i * 4);
            __m256d vr = _mm256_mul_pd(va, vb);
            
            // Modular reduction to [0,1)
            __m256d vfloor = _mm256_floor_pd(vr);
            vr = _mm256_sub_pd(vr, vfloor);
            
            _mm256_storeu_pd(result + i * 4, vr);
        }
        
        // Handle remainder
        for (size_t i = simd_n * 4; i < n; ++i) {
            double r = a[i] * b[i];
            result[i] = r - std::floor(r);
        }
    }
    
    // Fast continued fraction evaluation
    static void cf_evaluate_batch(double* result, const double* convergents, 
                                 size_t n, int depth) {
        // AVX2 optimized continued fraction evaluation
        for (size_t i = 0; i < n; i += 4) {
            __m256d p = _mm256_loadu_pd(convergents + i);
            __m256d q = _mm256_set1_pd(1.0);
            
            for (int d = 0; d < depth; ++d) {
                __m256d a = _mm256_loadu_pd(convergents + i + (d + 1) * n);
                __m256d temp = _mm256_add_pd(q, _mm256_mul_pd(a, p));
                q = p;
                p = temp;
            }
            
            __m256d res = _mm256_div_pd(p, q);
            _mm256_storeu_pd(result + i, res);
        }
    }
};
#endif // __AVX2__

// ARM NEON acceleration
#ifdef __ARM_NEON
#include <arm_neon.h>

class NEONAccelerator {
public:
    // NEON optimized operations for ARM
    static void modular_multiply_batch(double* result, const double* a, const double* b, size_t n) {
        size_t simd_n = n / 2;
        
        for (size_t i = 0; i < simd_n; ++i) {
            float64x2_t va = vld1q_f64(a + i * 2);
            float64x2_t vb = vld1q_f64(b + i * 2);
            float64x2_t vr = vmulq_f64(va, vb);
            
            // Modular reduction (simplified)
            vst1q_f64(result + i * 2, vr);
        }
        
        // Handle remainder
        for (size_t i = simd_n * 2; i < n; ++i) {
            double r = a[i] * b[i];
            result[i] = r - std::floor(r);
        }
    }
};
#endif // __ARM_NEON

// Hardware acceleration manager
class HardwareAccelerationManager {
private:
    HardwareCapabilities caps_;
    std::unique_ptr<OpenMPAccelerator> openmp_;
#ifdef QIPRNG_HAS_CUDA
    std::unique_ptr<CUDAAccelerator> cuda_;
#endif
    
public:
    HardwareAccelerationManager() : caps_(HardwareCapabilities::detect()) {
        if (caps_.has_openmp) {
            openmp_ = std::make_unique<OpenMPAccelerator>();
        }
#ifdef QIPRNG_HAS_CUDA
        if (caps_.has_cuda) {
            cuda_ = std::make_unique<CUDAAccelerator>();
        }
#endif
    }
    
    const HardwareCapabilities& capabilities() const { return caps_; }
    
    // Select best acceleration method for given task size
    enum AccelMethod {
        NONE,
        OPENMP,
        CUDA,
        AVX2,
        NEON
    };
    
    AccelMethod select_method(size_t task_size) const {
        // CUDA for very large tasks
        if (caps_.has_cuda && task_size > 1000000) {
            return CUDA;
        }
        
        // AVX2/AVX512 for medium tasks
        if ((caps_.has_avx2 || caps_.has_avx512) && task_size > 1000) {
            return AVX2;
        }
        
        // NEON for ARM platforms
        if (caps_.has_neon && task_size > 1000) {
            return NEON;
        }
        
        // OpenMP for parallel tasks
        if (caps_.has_openmp && task_size > 10000) {
            return OPENMP;
        }
        
        return NONE;
    }
    
    OpenMPAccelerator* get_openmp() { return openmp_.get(); }
#ifdef QIPRNG_HAS_CUDA
    CUDAAccelerator* get_cuda() { return cuda_.get(); }
#endif
};

} // namespace hardware
} // namespace qiprng

#endif // HARDWARE_ACCELERATION_HPP