#!/bin/bash

# Validate qiprng Mintlify Documentation Structure
# This script checks that all documentation components are in place

echo "================================================"
echo "  qiprng Documentation Validation"
echo "================================================"
echo ""

# Color codes for output
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Counters
PASS=0
FAIL=0
WARN=0

# Function to check file exists
check_file() {
    if [ -f "$1" ]; then
        echo -e "${GREEN}✓${NC} $2"
        ((PASS++))
        return 0
    else
        echo -e "${RED}✗${NC} $2 (missing: $1)"
        ((FAIL++))
        return 1
    fi
}

# Function to check directory exists
check_dir() {
    if [ -d "$1" ]; then
        echo -e "${GREEN}✓${NC} $2"
        ((PASS++))
        return 0
    else
        echo -e "${RED}✗${NC} $2 (missing: $1)"
        ((FAIL++))
        return 1
    fi
}

# Function to check JSON validity
check_json() {
    if [ -f "$1" ]; then
        if python -m json.tool "$1" > /dev/null 2>&1; then
            echo -e "${GREEN}✓${NC} $2 (valid JSON)"
            ((PASS++))
            return 0
        else
            echo -e "${RED}✗${NC} $2 (invalid JSON)"
            ((FAIL++))
            return 1
        fi
    else
        echo -e "${RED}✗${NC} $2 (file not found)"
        ((FAIL++))
        return 1
    fi
}

# Function to count files in directory
count_files() {
    if [ -d "$1" ]; then
        count=$(find "$1" -name "*.mdx" 2>/dev/null | wc -l | tr -d ' ')
        if [ "$count" -gt 0 ]; then
            echo -e "${GREEN}✓${NC} $2 ($count files)"
            ((PASS++))
        else
            echo -e "${YELLOW}⚠${NC} $2 (empty directory)"
            ((WARN++))
        fi
    else
        echo -e "${RED}✗${NC} $2 (directory not found)"
        ((FAIL++))
    fi
}

echo "1. Core Configuration"
echo "---------------------"
check_json "mintlify/mint.json" "Mintlify configuration"
check_file "mintlify/introduction.mdx" "Introduction page"
check_file "mintlify/quickstart.mdx" "Quickstart guide"
echo ""

echo "2. Directory Structure"
echo "----------------------"
check_dir "mintlify" "Main documentation directory"
check_dir "mintlify/api-reference" "API reference directory"
check_dir "mintlify/concepts" "Concepts directory"
check_dir "mintlify/reference" "Reference directory"
check_dir "scripts" "Scripts directory"
echo ""

echo "3. Conversion Scripts"
echo "---------------------"
check_file "scripts/roxygen2_to_mintlify.R" "R documentation converter"
check_file "scripts/cpp_to_mintlify.py" "C++ documentation parser"
check_file ".github/workflows/mintlify-deploy.yml" "GitHub Actions workflow"
echo ""

echo "4. Generated Documentation"
echo "--------------------------"
count_files "mintlify/api-reference/r-functions" "R function documentation"
count_files "mintlify/api-reference/cpp" "C++ class documentation"
echo ""

echo "5. Manual Documentation"
echo "-----------------------"
check_file "mintlify/concepts/mathematical-theory.mdx" "Mathematical foundations"
check_file "mintlify/api-reference/cpp/architecture.mdx" "C++ architecture"
check_file "mintlify/reference/migration-guide.mdx" "Migration guide"
check_file "mintlify/DEPLOYMENT.md" "Deployment instructions"
echo ""

echo "6. Source Documentation"
echo "-----------------------"
# Check if R files have roxygen2 comments
if grep -q "@export" R/*.R 2>/dev/null; then
    echo -e "${GREEN}✓${NC} R files have roxygen2 documentation"
    ((PASS++))
else
    echo -e "${YELLOW}⚠${NC} No roxygen2 documentation found in R files"
    ((WARN++))
fi

# Check if C++ files have inline documentation
if grep -q "/\*\*" src/*.hpp 2>/dev/null || grep -q "///" src/*.hpp 2>/dev/null; then
    echo -e "${GREEN}✓${NC} C++ headers have inline documentation"
    ((PASS++))
else
    echo -e "${YELLOW}⚠${NC} Limited inline documentation in C++ headers"
    ((WARN++))
fi
echo ""

echo "7. Dependencies Check"
echo "---------------------"
# Check for R
if command -v R &> /dev/null; then
    echo -e "${GREEN}✓${NC} R is installed"
    ((PASS++))
else
    echo -e "${RED}✗${NC} R is not installed"
    ((FAIL++))
fi

# Check for Python
if command -v python3 &> /dev/null || command -v python &> /dev/null; then
    echo -e "${GREEN}✓${NC} Python is installed"
    ((PASS++))
else
    echo -e "${RED}✗${NC} Python is not installed"
    ((FAIL++))
fi

# Check for Node.js
if command -v node &> /dev/null; then
    echo -e "${GREEN}✓${NC} Node.js is installed"
    ((PASS++))
else
    echo -e "${RED}✗${NC} Node.js is not installed"
    ((FAIL++))
fi
echo ""

echo "8. Navigation Structure"
echo "-----------------------"
# Check if mint.json has proper navigation
if [ -f "mintlify/mint.json" ]; then
    if grep -q '"navigation"' mintlify/mint.json; then
        echo -e "${GREEN}✓${NC} Navigation structure defined"
        ((PASS++))
    else
        echo -e "${RED}✗${NC} Navigation structure missing"
        ((FAIL++))
    fi

    if grep -q '"colors"' mintlify/mint.json; then
        echo -e "${GREEN}✓${NC} Color scheme configured"
        ((PASS++))
    else
        echo -e "${YELLOW}⚠${NC} Color scheme not configured"
        ((WARN++))
    fi

    if grep -q '"topbarLinks"' mintlify/mint.json; then
        echo -e "${GREEN}✓${NC} Topbar links configured"
        ((PASS++))
    else
        echo -e "${YELLOW}⚠${NC} Topbar links not configured"
        ((WARN++))
    fi
fi
echo ""

echo "================================================"
echo "              Validation Summary"
echo "================================================"
echo -e "${GREEN}Passed:${NC} $PASS"
echo -e "${YELLOW}Warnings:${NC} $WARN"
echo -e "${RED}Failed:${NC} $FAIL"
echo ""

if [ $FAIL -eq 0 ]; then
    if [ $WARN -eq 0 ]; then
        echo -e "${GREEN}✓ Documentation structure is complete and ready for deployment!${NC}"
        exit 0
    else
        echo -e "${YELLOW}⚠ Documentation structure is mostly complete with some warnings.${NC}"
        echo "  Consider addressing the warnings before deployment."
        exit 0
    fi
else
    echo -e "${RED}✗ Documentation structure has critical issues that need to be fixed.${NC}"
    echo "  Please address the failed checks before attempting deployment."
    exit 1
fi
