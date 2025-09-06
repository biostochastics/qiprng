cmake_minimum_required(VERSION 3.20)
project(qiprng VERSION 0.5.0 LANGUAGES CXX)

# This CMakeLists.txt is for standalone C++ testing and benchmarking
# The main build system is the R package build via src/Makevars

# Set C++17 standard
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

# Options
option(BUILD_TESTS "Build test suite" ON)
option(BUILD_BENCHMARKS "Build benchmarks" ON)
option(USE_CRYPTO "Enable cryptographic hardening with libsodium" ON)

# Export compile commands for IDE integration
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

# Set default build type if not specified
if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Release CACHE STRING
        "Choose the type of build (Debug, Release, RelWithDebInfo, MinSizeRel)" FORCE)
endif()

# Platform-specific optimizations
include(CheckCXXCompilerFlag)

# Compiler flags
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    # Common flags
    add_compile_options(-Wall -Wextra -Wpedantic)

    # Debug flags
    set(CMAKE_CXX_FLAGS_DEBUG "-g -O0 -DDEBUG -fsanitize=address,undefined")
    set(CMAKE_EXE_LINKER_FLAGS_DEBUG "-fsanitize=address,undefined")

    # Release flags with aggressive optimizations
    set(CMAKE_CXX_FLAGS_RELEASE "-O3 -DNDEBUG")

    # Check for native architecture support
    check_cxx_compiler_flag("-march=native" COMPILER_SUPPORTS_MARCH_NATIVE)
    if(COMPILER_SUPPORTS_MARCH_NATIVE)
        set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} -march=native")
    endif()

    # Check for fast-math
    check_cxx_compiler_flag("-ffast-math" COMPILER_SUPPORTS_FAST_MATH)
    if(COMPILER_SUPPORTS_FAST_MATH)
        set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} -ffast-math")
    endif()

    # Link-time optimization
    check_cxx_compiler_flag("-flto" COMPILER_SUPPORTS_LTO)
    if(COMPILER_SUPPORTS_LTO)
        set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} -flto")
        set(CMAKE_EXE_LINKER_FLAGS_RELEASE "${CMAKE_EXE_LINKER_FLAGS_RELEASE} -flto")
    endif()

elseif(MSVC)
    # MSVC flags
    add_compile_options(/W4 /permissive-)
    set(CMAKE_CXX_FLAGS_DEBUG "/Od /MDd /Zi /RTC1 /DDEBUG")
    set(CMAKE_CXX_FLAGS_RELEASE "/O2 /MD /DNDEBUG /GL")
    set(CMAKE_EXE_LINKER_FLAGS_RELEASE "/LTCG")
endif()

# Find required packages
find_package(Threads REQUIRED)
find_package(OpenMP)
find_package(CUDA QUIET)

# Use pkg-config to find dependencies (similar to configure script)
find_package(PkgConfig REQUIRED)

# Find GMP and MPFR
pkg_check_modules(GMP REQUIRED gmp)
pkg_check_modules(MPFR REQUIRED mpfr)

# Find libsodium (optional but recommended)
if(USE_CRYPTO)
    pkg_check_modules(SODIUM libsodium)
    if(NOT SODIUM_FOUND)
        message(WARNING "libsodium not found. Cryptographic features will be disabled.")
        set(USE_CRYPTO OFF)
    endif()
endif()

# Find Boost (optional for this build)
find_package(Boost 1.70)

# Include directories
include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}/src
    ${GMP_INCLUDE_DIRS}
    ${MPFR_INCLUDE_DIRS}
)

if(USE_CRYPTO AND SODIUM_FOUND)
    include_directories(${SODIUM_INCLUDE_DIRS})
    add_compile_definitions(USE_CRYPTO_HARDENING)
endif()

if(Boost_FOUND)
    include_directories(${Boost_INCLUDE_DIRS})
endif()

# Define USE_MPFR as in Makevars
add_compile_definitions(USE_MPFR)

# v0.5.0: Hardware acceleration support
if(OpenMP_FOUND)
    add_compile_definitions(QIPRNG_HAS_OPENMP)
    message(STATUS "OpenMP support: ENABLED")
else()
    message(STATUS "OpenMP support: DISABLED")
endif()

if(CUDA_FOUND)
    enable_language(CUDA)
    add_compile_definitions(QIPRNG_HAS_CUDA)
    message(STATUS "CUDA support: ENABLED")
else()
    message(STATUS "CUDA support: DISABLED")
endif()

# Check for SIMD instructions
include(CheckCXXCompilerFlag)
check_cxx_compiler_flag("-mavx2" COMPILER_SUPPORTS_AVX2)
if(COMPILER_SUPPORTS_AVX2)
    set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} -mavx2")
    add_compile_definitions(__AVX2__)
endif()

check_cxx_compiler_flag("-mavx512f" COMPILER_SUPPORTS_AVX512)
if(COMPILER_SUPPORTS_AVX512)
    set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} -mavx512f")
    add_compile_definitions(__AVX512F__)
endif()

# Exclude R-specific files from standalone build
set(QIPRNG_SOURCES
    src/quadratic_irrational.cpp
    src/enhanced_prng.cpp
    src/crypto_mixer.cpp
    src/multi_qi.cpp
    src/prng_utils.cpp
    src/ziggurat_normal.cpp
    src/prng_warnings.cpp
)

# Headers
set(QIPRNG_HEADERS
    src/quadratic_irrational.hpp
    src/enhanced_prng.hpp
    src/crypto_mixer.hpp
    src/multi_qi.hpp
    src/prng_utils.hpp
    src/prng_common.hpp
    src/prng_config.hpp
    src/thread_manager.hpp
    src/thread_pool.hpp
    src/deterministic_rng.hpp
    src/ziggurat_normal.hpp
)

# Create static library for testing
add_library(qiprng_standalone STATIC ${QIPRNG_SOURCES})

# Link libraries
target_link_libraries(qiprng_standalone
    PUBLIC
        ${MPFR_LINK_LIBRARIES}
        ${GMP_LINK_LIBRARIES}
        Threads::Threads
)

if(USE_CRYPTO AND SODIUM_FOUND)
    target_link_libraries(qiprng_standalone PUBLIC ${SODIUM_LINK_LIBRARIES})
endif()

if(Boost_FOUND)
    target_link_libraries(qiprng_standalone PUBLIC Boost::boost)
endif()

# Set library properties
set_target_properties(qiprng_standalone PROPERTIES
    VERSION ${PROJECT_VERSION}
    CXX_STANDARD 17
    CXX_STANDARD_REQUIRED ON
)

# Testing
if(BUILD_TESTS)
    # Fetch Catch2
    include(FetchContent)
    FetchContent_Declare(
        Catch2
        GIT_REPOSITORY https://github.com/catchorg/Catch2.git
        GIT_TAG v3.4.0
    )
    FetchContent_MakeAvailable(Catch2)

    enable_testing()

    # Create tests directory if needed
    file(MAKE_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/tests)

    # Example test file (to be created)
    if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/tests/test_standalone.cpp)
        add_executable(test_qiprng tests/test_standalone.cpp)
        target_link_libraries(test_qiprng
            PRIVATE
                qiprng_standalone
                Catch2::Catch2WithMain
        )

        # Register tests with CTest
        include(Catch)
        catch_discover_tests(test_qiprng)
    else()
        message(STATUS "No standalone tests found. Create tests/test_standalone.cpp to enable testing.")
    endif()
endif()

# Benchmarks
if(BUILD_BENCHMARKS)
    # Fetch Google Benchmark
    include(FetchContent)
    FetchContent_Declare(
        benchmark
        GIT_REPOSITORY https://github.com/google/benchmark.git
        GIT_TAG v1.8.3
    )
    set(BENCHMARK_ENABLE_TESTING OFF CACHE BOOL "Disable benchmark testing")
    FetchContent_MakeAvailable(benchmark)

    # Create benchmarks directory if needed
    file(MAKE_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/benchmarks)

    # Example benchmark file (to be created)
    if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/benchmarks/bench_performance.cpp)
        add_executable(bench_qiprng benchmarks/bench_performance.cpp)
        target_link_libraries(bench_qiprng
            PRIVATE
                qiprng_standalone
                benchmark::benchmark
        )
    else()
        message(STATUS "No benchmarks found. Create benchmarks/bench_performance.cpp to enable benchmarking.")
    endif()
endif()

# Print configuration summary
message(STATUS "")
message(STATUS "qiprng v${PROJECT_VERSION} Standalone Build Configuration:")
message(STATUS "  Build type:        ${CMAKE_BUILD_TYPE}")
message(STATUS "  C++ Standard:      C++${CMAKE_CXX_STANDARD}")
message(STATUS "  Compiler:          ${CMAKE_CXX_COMPILER_ID}")
message(STATUS "  Crypto hardening:  ${USE_CRYPTO}")
message(STATUS "  Build tests:       ${BUILD_TESTS}")
message(STATUS "  Build benchmarks:  ${BUILD_BENCHMARKS}")
message(STATUS "")
message(STATUS "Dependencies found:")
message(STATUS "  GMP:              ${GMP_FOUND}")
message(STATUS "  MPFR:             ${MPFR_FOUND}")
if(USE_CRYPTO)
    message(STATUS "  libsodium:        ${SODIUM_FOUND}")
endif()
if(Boost_FOUND)
    message(STATUS "  Boost:            ${Boost_VERSION}")
endif()
message(STATUS "")
message(STATUS "Note: This CMakeLists.txt is for standalone C++ testing/benchmarking.")
message(STATUS "      The main build system is the R package build via src/Makevars")
message(STATUS "")
