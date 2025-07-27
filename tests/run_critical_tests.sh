#\!/bin/bash

echo "===== Running Critical Tests for Thread Safety ====="
echo "Date: $(date)"

# Create test results directory
mkdir -p test_results

# Function to run a test and log results
run_test() {
  test_name="$1"
  test_script="$2"
  
  echo -e "\n----- Testing: $test_name -----"
  echo "Running: $test_script"
  
  if Rscript "$test_script" > "test_results/${test_name}_output.log" 2>&1; then
    echo "[PASS] $test_name"
    return 0
  else
    echo "[FAIL] $test_name (Exit code: $?)"
    return 1
  fi
}

# Run the core tests that verify our thread safety fix
echo -e "\n----- Core Thread Safety Tests -----"

# Run our safest test first (most focused on the fix)
run_test "Safest Ziggurat" "safest_ziggurat_test.R"
CORE_TEST_1=$?

# Run a basic verification test
run_test "Basic Verification" "basic_verify.R"
CORE_TEST_2=$?

# Run the test scripts that were previously failing
echo -e "\n----- Previously Failing Tests -----"

# Run the parallel Ziggurat test (which was previously failing)
if [ -f "tests/testthat/test_parallel_ziggurat.R" ]; then
  run_test "Parallel Ziggurat" "tests/testthat/test_parallel_ziggurat.R"
  PARALLEL_TEST=$?
else
  echo "Parallel Ziggurat test not found"
  PARALLEL_TEST=2
fi

# Run the Ziggurat enabled test
if [ -f "tests/testthat/test_ziggurat_enabled.R" ]; then
  run_test "Ziggurat Enabled" "tests/testthat/test_ziggurat_enabled.R"
  ENABLED_TEST=$?
else
  echo "Ziggurat Enabled test not found"
  ENABLED_TEST=2
fi

# Run some basic Ziggurat tests
echo -e "\n----- Basic Ziggurat Tests -----"

# Run the Ziggurat basic test
if [ -f "tests/testthat/test_ziggurat_basic.R" ]; then
  run_test "Ziggurat Basic" "tests/testthat/test_ziggurat_basic.R"
  BASIC_TEST=$?
else
  echo "Ziggurat Basic test not found"
  BASIC_TEST=2
fi

# Run the Ziggurat cache test
if [ -f "tests/testthat/test_ziggurat_cache.R" ]; then
  run_test "Ziggurat Cache" "tests/testthat/test_ziggurat_cache.R"
  CACHE_TEST=$?
else
  echo "Ziggurat Cache test not found"
  CACHE_TEST=2
fi

# Print a summary
echo -e "\n===== Test Summary ====="
echo "Core Tests:"
[ $CORE_TEST_1 -eq 0 ] && echo "  ✅ Safest Ziggurat Test: PASSED" || echo "  ❌ Safest Ziggurat Test: FAILED"
[ $CORE_TEST_2 -eq 0 ] && echo "  ✅ Basic Verification: PASSED" || echo "  ❌ Basic Verification: FAILED"

echo -e "\nPreviously Failing Tests:"
[ $PARALLEL_TEST -eq 0 ] && echo "  ✅ Parallel Ziggurat: PASSED" || { [ $PARALLEL_TEST -eq 1 ] && echo "  ❌ Parallel Ziggurat: FAILED" || echo "  ❓ Parallel Ziggurat: NOT RUN"; }
[ $ENABLED_TEST -eq 0 ] && echo "  ✅ Ziggurat Enabled: PASSED" || { [ $ENABLED_TEST -eq 1 ] && echo "  ❌ Ziggurat Enabled: FAILED" || echo "  ❓ Ziggurat Enabled: NOT RUN"; }

echo -e "\nBasic Ziggurat Tests:"
[ $BASIC_TEST -eq 0 ] && echo "  ✅ Ziggurat Basic: PASSED" || { [ $BASIC_TEST -eq 1 ] && echo "  ❌ Ziggurat Basic: FAILED" || echo "  ❓ Ziggurat Basic: NOT RUN"; }
[ $CACHE_TEST -eq 0 ] && echo "  ✅ Ziggurat Cache: PASSED" || { [ $CACHE_TEST -eq 1 ] && echo "  ❌ Ziggurat Cache: FAILED" || echo "  ❓ Ziggurat Cache: NOT RUN"; }

# Overall result
if [ $CORE_TEST_1 -eq 0 ] && [ $CORE_TEST_2 -eq 0 ]; then
  echo -e "\n✅ CORE THREAD SAFETY TESTS PASSED - The primary fix is working correctly\!"
  
  if [ $BASIC_TEST -eq 0 ] && [ $CACHE_TEST -eq 0 ]; then
    echo "✅ BASIC ZIGGURAT TESTS PASSED - All basic functionality is working correctly\!"
  else
    echo "❌ SOME BASIC ZIGGURAT TESTS FAILED - There are still issues with some basic functionality."
  fi
  
  if [ $PARALLEL_TEST -eq 0 ] && [ $ENABLED_TEST -eq 0 ]; then
    echo "✅ ALL TESTS PASSED - Even the previously failing complex tests now work\!"
  else
    echo "⚠️ SOME COMPLEX TESTS STILL FAILING - The core fix is working, but more complex cases still need attention."
  fi
else
  echo -e "\n❌ CORE THREAD SAFETY TESTS FAILED - The primary fix is not working correctly\!"
fi

echo -e "\nResults output saved to test_results/ directory"
