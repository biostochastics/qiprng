# Makefile for building and running C++ tests
# This is only used for testing, not for package building

CXX = g++
CXXFLAGS = -std=c++17 -O2 -Wall -Wextra -DQIPRNG_TESTING -I../../src -I../../inst/include
LDFLAGS = -pthread

# Add R and Rcpp includes if available
R_INCLUDE = $(shell R CMD config --cppflags 2>/dev/null)
RCPP_INCLUDE = $(shell Rscript -e "cat(paste0('-I', system.file('include', package='Rcpp')))" 2>/dev/null)

ifneq ($(R_INCLUDE),)
    CXXFLAGS += $(R_INCLUDE) $(RCPP_INCLUDE)
endif

# Test source files
TEST_SOURCES = $(wildcard *.cpp)
TEST_OBJECTS = $(TEST_SOURCES:.cpp=.o)
TEST_EXECUTABLES = $(TEST_SOURCES:.cpp=)

# Default target
all: message

message:
	@echo "Test code has been isolated from production build."
	@echo "To run tests, use the R testing framework (testthat) or:"
	@echo "  make test-thread-safety"
	@echo "  make test-all"

# Individual test targets
test-thread-safety: test_thread_safety
	./test_thread_safety

test-all: $(TEST_EXECUTABLES)
	@for test in $(TEST_EXECUTABLES); do \
		echo "Running $$test..."; \
		./$$test || exit 1; \
	done

# Pattern rule for building test executables
%: %.cpp
	$(CXX) $(CXXFLAGS) -o $@ $< $(LDFLAGS)

# Clean target
clean:
	rm -f $(TEST_OBJECTS) $(TEST_EXECUTABLES)

.PHONY: all message test-thread-safety test-all clean
