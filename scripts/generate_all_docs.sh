#!/bin/bash
# ----------------------------------------------------------------------
# Script: generate_all_docs.sh
# Purpose: Complete documentation generation pipeline for qiprng
# Integrates all documentation generation steps
# ----------------------------------------------------------------------

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Get script directory and project root
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"

echo -e "${BLUE}================================================${NC}"
echo -e "${BLUE}  qiprng Documentation Generation Pipeline${NC}"
echo -e "${BLUE}================================================${NC}"

# Function to check command success
check_status() {
    if [ $? -eq 0 ]; then
        echo -e "${GREEN}✓ $1${NC}"
    else
        echo -e "${RED}✗ $1 failed${NC}"
        exit 1
    fi
}

# Step 1: Generate roxygen2 documentation (if needed)
echo -e "\n${YELLOW}Step 1: Checking R documentation...${NC}"
if command -v R &> /dev/null; then
    cd "$PROJECT_ROOT"
    R -e "roxygen2::roxygenize()" > /dev/null 2>&1
    check_status "R documentation updated"
else
    echo -e "${YELLOW}⚠ R not found, skipping roxygen2 generation${NC}"
fi

# Step 2: Generate Mintlify documentation with all integrations
echo -e "\n${YELLOW}Step 2: Generating Mintlify documentation...${NC}"
echo "This includes:"
echo "  • README integration as Introduction"
echo "  • NIST validation data"
echo "  • Discriminant data documentation"
echo "  • API reference for R and C++ functions"
echo "  • Comprehensive guides"
echo ""

# Run the complete Mintlify generation
Rscript "$SCRIPT_DIR/generate_mintlify_complete.R" 2>&1 | while IFS= read -r line; do
    if [[ "$line" == *"✓"* ]]; then
        echo -e "${GREEN}$line${NC}"
    elif [[ "$line" == *"✗"* ]]; then
        echo -e "${RED}$line${NC}"
    elif [[ "$line" == *"Generated"* ]] || [[ "$line" == *"Created"* ]]; then
        echo -e "${GREEN}  $line${NC}"
    else
        echo "  $line"
    fi
done

check_status "Mintlify documentation generation"

# Step 3: Validate documentation structure
echo -e "\n${YELLOW}Step 3: Validating documentation structure...${NC}"

# Check mint.json
if [ -f "$PROJECT_ROOT/mint.json" ]; then
    python3 -m json.tool "$PROJECT_ROOT/mint.json" > /dev/null 2>&1
    check_status "mint.json validation"
else
    echo -e "${RED}✗ mint.json not found${NC}"
    exit 1
fi

# Check key directories
REQUIRED_DIRS=(
    "mintlify"
    "mintlify/api-reference"
    "mintlify/api-reference/r-functions"
    "mintlify/api-reference/cpp-classes"
    "mintlify/guides"
    "mintlify/reference"
    "mintlify/validation"
)

for dir in "${REQUIRED_DIRS[@]}"; do
    if [ -d "$PROJECT_ROOT/$dir" ]; then
        echo -e "${GREEN}  ✓ $dir exists${NC}"
    else
        echo -e "${RED}  ✗ $dir missing${NC}"
        mkdir -p "$PROJECT_ROOT/$dir"
        echo -e "${YELLOW}    Created $dir${NC}"
    fi
done

# Step 4: Check essential files
echo -e "\n${YELLOW}Step 4: Verifying essential documentation files...${NC}"

ESSENTIAL_FILES=(
    "mintlify/introduction.mdx"
    "mintlify/quickstart.mdx"
    "mintlify/installation.mdx"
    "mintlify/validation/nist-validation.mdx"
    "mintlify/validation/discriminant-data.mdx"
    "mintlify/api-reference/introduction.mdx"
)

missing_count=0
for file in "${ESSENTIAL_FILES[@]}"; do
    if [ -f "$PROJECT_ROOT/$file" ]; then
        echo -e "${GREEN}  ✓ $file${NC}"
    else
        echo -e "${RED}  ✗ $file missing${NC}"
        ((missing_count++))
    fi
done

if [ $missing_count -eq 0 ]; then
    check_status "All essential files present"
else
    echo -e "${RED}✗ $missing_count essential files missing${NC}"
fi

# Step 5: Count generated documentation
echo -e "\n${YELLOW}Step 5: Documentation statistics...${NC}"

# Count files in each category
r_func_count=$(find "$PROJECT_ROOT/mintlify/api-reference/r-functions" -name "*.mdx" 2>/dev/null | wc -l | xargs)
cpp_class_count=$(find "$PROJECT_ROOT/mintlify/api-reference/cpp-classes" -name "*.mdx" 2>/dev/null | wc -l | xargs)
guide_count=$(find "$PROJECT_ROOT/mintlify/guides" -name "*.mdx" 2>/dev/null | wc -l | xargs)
ref_count=$(find "$PROJECT_ROOT/mintlify/reference" -name "*.mdx" 2>/dev/null | wc -l | xargs)
val_count=$(find "$PROJECT_ROOT/mintlify/validation" -name "*.mdx" 2>/dev/null | wc -l | xargs)

echo -e "  ${GREEN}✓ R Functions: $r_func_count files${NC}"
echo -e "  ${GREEN}✓ C++ Classes: $cpp_class_count files${NC}"
echo -e "  ${GREEN}✓ Guides: $guide_count files${NC}"
echo -e "  ${GREEN}✓ Reference: $ref_count files${NC}"
echo -e "  ${GREEN}✓ Validation: $val_count files${NC}"

total_count=$((r_func_count + cpp_class_count + guide_count + ref_count + val_count + 3))
echo -e "  ${GREEN}✓ Total: $total_count documentation files${NC}"

# Step 6: Test Mintlify CLI (if available)
echo -e "\n${YELLOW}Step 6: Checking Mintlify CLI...${NC}"

if command -v mintlify &> /dev/null; then
    echo -e "${GREEN}✓ Mintlify CLI is installed${NC}"
    mintlify_version=$(mintlify --version 2>/dev/null || echo "unknown")
    echo -e "  Version: $mintlify_version"

    # Optional: Test server startup (commented out as it blocks)
    # echo -e "\n${YELLOW}Starting test server (press Ctrl+C to stop)...${NC}"
    # cd "$PROJECT_ROOT"
    # timeout 5 mintlify dev 2>&1 | head -10 || true
else
    echo -e "${YELLOW}⚠ Mintlify CLI not installed${NC}"
    echo "  Install with: npm install -g mintlify"
fi

# Final summary
echo -e "\n${BLUE}================================================${NC}"
echo -e "${BLUE}  Documentation Pipeline Complete!${NC}"
echo -e "${BLUE}================================================${NC}"

echo -e "\n${GREEN}✅ Successfully generated:${NC}"
echo "  • Complete Mintlify documentation"
echo "  • README integrated as Introduction"
echo "  • NIST validation results (98.4% pass rate)"
echo "  • Discriminant data documentation"
echo "  • API reference for core functions"
echo "  • Comprehensive user guides"
echo "  • Reference documentation"

echo -e "\n${GREEN}📊 Documentation includes:${NC}"
echo "  • $r_func_count R function references"
echo "  • $cpp_class_count C++ class descriptions"
echo "  • $guide_count user guides"
echo "  • $ref_count reference documents"
echo "  • $val_count validation documents"

echo -e "\n${GREEN}🚀 Next steps:${NC}"
echo "  1. Test locally: mintlify dev"
echo "  2. Review changes: git diff"
echo "  3. Commit: git add . && git commit -m 'Update documentation'"
echo "  4. Push to deploy: git push"

echo -e "\n${BLUE}Pipeline scripts available:${NC}"
echo "  • $SCRIPT_DIR/generate_all_docs.sh (this pipeline)"
echo "  • $SCRIPT_DIR/generate_mintlify_complete.R (Mintlify generation)"
echo "  • $SCRIPT_DIR/test_mintlify_deployment.sh (validation)"
echo "  • $SCRIPT_DIR/regenerate_docs.sh (quick regeneration)"

# Create a timestamp file for tracking
echo "$(date '+%Y-%m-%d %H:%M:%S')" > "$PROJECT_ROOT/.last_doc_generation"
echo -e "\n${GREEN}✓ Pipeline completed at $(date '+%Y-%m-%d %H:%M:%S')${NC}"
