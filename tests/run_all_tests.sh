#!/bin/bash

# Clear previous log file if it exists
LOG_FILE="test_results.log"
rm -f $LOG_FILE

# Function to log output
log() {
  echo "$(date '+%Y-%m-%d %H:%M:%S'): $1" | tee -a $LOG_FILE
}

# Function to run a test and log the output
run_test() {
  local test_name="$1"
  local test_cmd="$2"
  
  log "=========================================================="
  log "Starting test: $test_name"
  log "Command: $test_cmd"
  log "=========================================================="
  
  # Run the test and capture both stdout and stderr
  eval "$test_cmd" 2>&1 | tee -a $LOG_FILE
  
  local result=${PIPESTATUS[0]}
  if [ $result -eq 0 ]; then
    log "Test '$test_name' PASSED"
  else
    log "Test '$test_name' FAILED with exit code $result"
  fi
  log ""
}

# Create header for log file
log "QIPRNG TEST RESULTS"
log "===================="
log "Date: $(date)"
log "R Version: $(R --version | head -n 1)"
log "Working Directory: $(pwd)"
log ""

# Run the installation test first
log "Installing package..."
run_test "Package Installation" "R CMD INSTALL ."

# Run the basic tests
run_test "Basic Uniform Distribution" "./final_fix.R"
run_test "Normal Distribution" "./test_normal.R"

# Run thread safety and distribution tests
run_test "Thread Safety" "./test_thread_safety.R"
run_test "Discriminant Thread Safety" "./test_discriminant_thread_safety.R"
run_test "Ziggurat Basic" "./test_ziggurat_basic.R"
run_test "Ziggurat Parallel" "./test_parallel_ziggurat.R"
run_test "Ziggurat Caching" "./test_ziggurat_cache.R"
run_test "Ziggurat Enabled" "./test_ziggurat_enabled.R"
run_test "Buffer Alignment" "./buffer_alignment_test.R"
run_test "Parameter Validation" "./parameter_validation_test.R"
run_test "Reliability Fix" "./test_reliability_fix.R"
run_test "Final Verification" "./final_verification.R"

# Run testthat tests
run_test "Testthat Individual Tests" "Rscript -e \"testthat::test_file('tests/testthat/test-basic.R')\""
run_test "All Distributions Test" "Rscript tests/testthat/test_all_distributions.R"

# Run the statistical framework tests (with timeout)
log "Running statistical framework test (limited to 30 seconds)..."
run_test "Statistical Framework" "timeout 30s Rscript test_statistical_framework.R"

# Run the comprehensive test
run_test "Comprehensive Test" "./comprehensive_test.R"

# Summary
log "=========================================================="
log "TEST SUMMARY"
log "=========================================================="
log "All tests completed. Check test_results.log for full output."
log "Tests that succeeded without segfaults:"
grep -B 1 "PASSED" $LOG_FILE | grep "Starting test:" | sed 's/Starting test: /- /' | tee -a $LOG_FILE
log ""
log "Tests that failed or had segfaults:"
grep -B 1 "FAILED" $LOG_FILE | grep "Starting test:" | sed 's/Starting test: /- /' | tee -a $LOG_FILE
log ""

# Thread safety specific summary
log "THREAD SAFETY STATUS"
log "===================="
thread_safety_pass=$(grep -B 1 "PASSED" $LOG_FILE | grep "Starting test: Thread Safety" | wc -l)
ziggurat_pass=$(grep -B 1 "PASSED" $LOG_FILE | grep "Starting test: Ziggurat" | wc -l)
discriminant_pass=$(grep -B 1 "PASSED" $LOG_FILE | grep "Starting test: Discriminant Thread Safety" | wc -l)

# Count total thread safety tests
total_thread_tests=5  # Thread Safety, Discriminant, Ziggurat Basic, Parallel, and Caching

if [ $thread_safety_pass -eq 1 ] && [ $discriminant_pass -eq 1 ] && [ $ziggurat_pass -ge 3 ]; then
  log "✅ Thread safety fixes are WORKING CORRECTLY"
else
  log "❌ Thread safety fixes are INCOMPLETE"
fi

# Show thread safety test results specifically
log "Thread Safety Test Results:"
grep -B 1 -A 1 "Thread Safety\|Discriminant Thread Safety\|Ziggurat" $LOG_FILE | grep -E "Starting test:|PASSED|FAILED" | tee -a $LOG_FILE