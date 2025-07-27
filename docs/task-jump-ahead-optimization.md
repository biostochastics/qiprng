# Task: Implement Efficient O(log n) Jump-Ahead for Quadratic Recurrence PRNG

**Status:** Not Started
**Owner:** Unassigned
**Difficulty:** High
**Related Files:** `src/quadratic_irrational.hpp`, `src/quadratic_irrational.cpp`, `tests/testthat/test-jump-ahead.R`

---

### Goal:
Replace the current inefficient `jump_ahead` method in the `QuadraticIrrational` class with a high-performance, O(log n) implementation.

### Background:
The core PRNG is based on the quadratic recurrence relation: `x_{n+1} = frac(a*x_n^2 + b*x_n + c)`. The current `jump_ahead` function simply calls the single-step function `n` times in a `for` loop. This is functionally correct for small `n` but is extremely slow for large jumps, defeating the purpose of a `jump_ahead` feature.

A common technique for efficient jumping is matrix exponentiation, but the standard 2x2 matrix method only applies to *linear* recurrences. A more advanced approach is required for this *quadratic* recurrence.

### Step-by-Step Guide:

**1. Research (The Most Critical Step):**
   - Your first task is to research methods for efficiently jumping ahead in non-linear generators.
   - **Keywords for your search:** "jumping ahead in quadratic congruential generator", "fast forward non-linear feedback shift register", "quadratic recurrence relation solver", "state-space linearization of non-linear recurrences".
   - **What to look for:** You are looking for an algorithm that allows you to calculate the state of the generator after `n` steps without iterating through all `n` steps. This often involves representing the quadratic recurrence as a linear operation in a higher-dimensional space.
   - **Deliverable:** Before writing any code, you must produce a short document (1-2 pages) that explains the algorithm you have chosen, why it works for our specific recurrence, and a high-level plan for how you will implement it in C++ with the MPFR library.

**2. C++ Implementation:**
   - **Files to Modify:** `src/quadratic_irrational.hpp` and `src/quadratic_irrational.cpp`.
   - **Core Task:** Implement the algorithm from your research inside the `QuadraticIrrational::jump_ahead(uint64_t n)` method, replacing the existing `for` loop.
   - **High-Precision Arithmetic:** The implementation **must** use the `MPFR` library for all calculations to maintain precision. The `QuadraticIrrational` class already uses `MPFRWrapper` objects, which you should continue to use. Pay close attention to initializing, clearing, and managing `mpfr_t` variables correctly to avoid memory leaks.

**3. Testing:**
   - **Correctness:** The existing test script `tests/testthat/test-jump-ahead.R` is your primary tool for verifying correctness. Your new implementation must pass all existing tests (with the `tolerance` parameter we added, as some floating-point differences are expected).
   - **Performance:** You must add a new test to `test-jump-ahead.R`. This test should:
     a. Use `microbenchmark` or a similar tool.
     b. Compare the execution time of your new `jump_ahead(n)` against a simple `for` loop calling `next()` `n` times.
     c. Use a very large jump size for `n` (e.g., `1e9` or larger) to clearly demonstrate the O(log n) performance improvement.

### Required Skills:
- Strong C++ programming ability.
- Experience with high-precision arithmetic libraries like MPFR or GMP is a major plus.
- The ability to read and understand technical/mathematical articles and translate algorithms into code.
- Basic familiarity with R and Rcpp for running the test suite.

### Definition of Done:
- The `jump_ahead` function is implemented with an efficient O(log n) algorithm.
- All tests in `test-jump-ahead.R` pass successfully.
- A new performance test is added that proves the significant speedup of the new implementation over the old one.
- The code is clean, well-documented, and follows the existing project style.
