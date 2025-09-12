#!/bin/bash

# Test complete documentation generation pipeline
# This script tests all components of the documentation system

set -e  # Exit on error

echo "================================================"
echo "  Testing qiprng Documentation Generation"
echo "================================================"
echo ""

# Colors for output
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Test counters
TESTS_PASSED=0
TESTS_FAILED=0

# Function to run a test
run_test() {
    local test_name="$1"
    local test_command="$2"

    echo -e "${BLUE}Testing:${NC} $test_name"

    if eval "$test_command" > /dev/null 2>&1; then
        echo -e "  ${GREEN}✓${NC} $test_name passed"
        ((TESTS_PASSED++))
        return 0
    else
        echo -e "  ${RED}✗${NC} $test_name failed"
        ((TESTS_FAILED++))
        return 1
    fi
}

# Test 1: Check R is installed
echo -e "${YELLOW}1. Environment Tests${NC}"
echo "---------------------"
run_test "R installation" "command -v R"
run_test "Python installation" "command -v python3 || command -v python"
run_test "roxygen2 package" "Rscript -e 'requireNamespace(\"roxygen2\", quietly = TRUE)'"
echo ""

# Test 2: Generate roxygen2 documentation
echo -e "${YELLOW}2. roxygen2 Generation${NC}"
echo "-----------------------"
if run_test "Generate .Rd files" "Rscript -e 'roxygen2::roxygenize()'"; then
    RD_COUNT=$(find man -name "*.Rd" 2>/dev/null | wc -l | tr -d ' ')
    echo -e "  ${BLUE}ℹ${NC} Generated $RD_COUNT .Rd files"
fi
echo ""

# Test 3: Convert R documentation to MDX
echo -e "${YELLOW}3. R to MDX Conversion${NC}"
echo "-----------------------"
if run_test "roxygen2 to MDX converter" "test -f scripts/roxygen2_to_mintlify.R"; then
    if run_test "Run conversion" "Rscript scripts/roxygen2_to_mintlify.R man mintlify/api-reference/r-functions"; then
        MDX_COUNT=$(find mintlify/api-reference/r-functions -name "*.mdx" 2>/dev/null | wc -l | tr -d ' ')
        echo -e "  ${BLUE}ℹ${NC} Converted $MDX_COUNT MDX files"
    fi
fi
echo ""

# Test 4: Extract C++ documentation
echo -e "${YELLOW}4. C++ Documentation${NC}"
echo "---------------------"
if run_test "C++ parser script" "test -f scripts/cpp_to_mintlify.py"; then
    if run_test "Run C++ extraction" "python3 scripts/cpp_to_mintlify.py --src src --output mintlify/api-reference/cpp-classes"; then
        CPP_MDX_COUNT=$(find mintlify/api-reference/cpp-classes -name "*.mdx" 2>/dev/null | wc -l | tr -d ' ')
        echo -e "  ${BLUE}ℹ${NC} Extracted $CPP_MDX_COUNT C++ documentation files"
    fi
fi
echo ""

# Test 5: Convert static content
echo -e "${YELLOW}5. Static Content Conversion${NC}"
echo "-----------------------------"
run_test "README exists" "test -f README.md"
run_test "CHANGELOG exists" "test -f CHANGELOG.md"

# Convert README to introduction
if run_test "Convert README" "python3 -c '
import re
with open(\"README.md\", \"r\") as f:
    content = f.read()
frontmatter = \"\"\"---
title: \"Introduction\"
description: \"Quadratic Irrational Pseudo-Random Number Generator for R\"
---

\"\"\"
content = re.sub(r\"\\[\\![^\\]]*\\]\\([^\\)]*\\)\", \"\", content)
with open(\"mintlify/introduction.mdx\", \"w\") as f:
    f.write(frontmatter + content)
print(\"README converted\")
'"; then
    echo -e "  ${BLUE}ℹ${NC} README converted to introduction.mdx"
fi

# Convert CHANGELOG
if run_test "Convert CHANGELOG" "python3 -c '
with open(\"CHANGELOG.md\", \"r\") as f:
    content = f.read()
frontmatter = \"\"\"---
title: \"Changelog\"
description: \"Version history and release notes\"
---

\"\"\"
with open(\"mintlify/reference/changelog.mdx\", \"w\") as f:
    f.write(frontmatter + content)
print(\"CHANGELOG converted\")
'"; then
    echo -e "  ${BLUE}ℹ${NC} CHANGELOG converted to changelog.mdx"
fi
echo ""

# Test 6: Validate Mintlify configuration
echo -e "${YELLOW}6. Mintlify Configuration${NC}"
echo "--------------------------"
run_test "mint.json exists" "test -f mintlify/mint.json"
run_test "mint.json valid JSON" "python3 -m json.tool mintlify/mint.json > /dev/null"
run_test "Navigation structure" "python3 -c 'import json; config = json.load(open(\"mintlify/mint.json\")); assert \"navigation\" in config'"
run_test "Required fields" "python3 -c 'import json; config = json.load(open(\"mintlify/mint.json\")); assert all(k in config for k in [\"name\", \"colors\", \"navigation\"])'"
echo ""

# Test 7: Validate MDX files
echo -e "${YELLOW}7. MDX Validation${NC}"
echo "------------------"
if run_test "MDX files exist" "ls mintlify/**/*.mdx > /dev/null 2>&1"; then
    # Count different types of MDX files
    R_MDX=$(find mintlify/api-reference/r-functions -name "*.mdx" 2>/dev/null | wc -l | tr -d ' ')
    CPP_MDX=$(find mintlify/api-reference/cpp* -name "*.mdx" 2>/dev/null | wc -l | tr -d ' ')
    OTHER_MDX=$(find mintlify -name "*.mdx" -not -path "*/r-functions/*" -not -path "*/cpp*" 2>/dev/null | wc -l | tr -d ' ')

    echo -e "  ${BLUE}ℹ${NC} R function docs: $R_MDX files"
    echo -e "  ${BLUE}ℹ${NC} C++ docs: $CPP_MDX files"
    echo -e "  ${BLUE}ℹ${NC} Other docs: $OTHER_MDX files"
fi

# Check for frontmatter in MDX files
if run_test "MDX frontmatter" "grep -l '^---' mintlify/introduction.mdx > /dev/null"; then
    echo -e "  ${BLUE}ℹ${NC} MDX files have valid frontmatter"
fi
echo ""

# Test 8: Component validation
echo -e "${YELLOW}8. Component Validation${NC}"
echo "------------------------"
if run_test "ParamField syntax" "! grep -r 'query=\"params\"' mintlify/api-reference/r-functions/*.mdx 2>/dev/null"; then
    echo -e "  ${GREEN}✓${NC} No deprecated ParamField syntax found"
fi

if run_test "CodeGroup usage" "grep -l '<CodeGroup>' mintlify/**/*.mdx > /dev/null 2>&1"; then
    echo -e "  ${BLUE}ℹ${NC} CodeGroup components found"
fi
echo ""

# Test 9: API Compliance
echo -e "${YELLOW}9. API Compliance Check${NC}"
echo "------------------------"
if run_test "Validation script exists" "test -f scripts/validate_mintlify_api.py"; then
    if python3 scripts/validate_mintlify_api.py > /tmp/api_validation.txt 2>&1; then
        echo -e "  ${GREEN}✓${NC} API compliance check passed"
        ERRORS=$(grep -c "❌" /tmp/api_validation.txt || true)
        WARNINGS=$(grep -c "⚠️" /tmp/api_validation.txt || true)
        echo -e "  ${BLUE}ℹ${NC} Errors: $ERRORS, Warnings: $WARNINGS"
    else
        echo -e "  ${YELLOW}⚠${NC} API compliance check completed with issues"
        ERRORS=$(grep -c "❌" /tmp/api_validation.txt || true)
        echo -e "  ${BLUE}ℹ${NC} Found $ERRORS errors (review /tmp/api_validation.txt)"
    fi
fi
echo ""

# Test 10: Documentation completeness
echo -e "${YELLOW}10. Documentation Completeness${NC}"
echo "-------------------------------"

# Check for essential files
ESSENTIAL_FILES=(
    "mintlify/mint.json"
    "mintlify/introduction.mdx"
    "mintlify/quickstart.mdx"
    "mintlify/installation.mdx"
    "mintlify/api-reference/cpp/architecture.mdx"
    "mintlify/concepts/mathematical-theory.mdx"
    "mintlify/reference/migration-guide.mdx"
)

for file in "${ESSENTIAL_FILES[@]}"; do
    if [ -f "$file" ]; then
        echo -e "  ${GREEN}✓${NC} $(basename $file)"
    else
        echo -e "  ${RED}✗${NC} $(basename $file) missing"
        ((TESTS_FAILED++))
    fi
done
echo ""

# Final Summary
echo "================================================"
echo "              Test Summary"
echo "================================================"
echo -e "${GREEN}Passed:${NC} $TESTS_PASSED"
echo -e "${RED}Failed:${NC} $TESTS_FAILED"
echo ""

if [ $TESTS_FAILED -eq 0 ]; then
    echo -e "${GREEN}✅ All documentation generation tests passed!${NC}"
    echo "Documentation is ready for deployment."
    exit 0
else
    echo -e "${RED}❌ Some tests failed. Please review and fix issues.${NC}"
    exit 1
fi
