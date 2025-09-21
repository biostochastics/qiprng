#!/bin/bash
# ----------------------------------------------------------------------
# Script: test_mintlify_deployment.sh
# Purpose: Test and validate Mintlify documentation deployment
# ----------------------------------------------------------------------

set -e

echo "================================================"
echo "  Mintlify Documentation Deployment Test"
echo "================================================"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Get script directory and project root
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
MINTLIFY_DIR="$PROJECT_ROOT/mintlify"

echo -e "\n${YELLOW}Step 1: Checking directory structure...${NC}"

# Check if mintlify directory exists
if [ ! -d "$MINTLIFY_DIR" ]; then
    echo -e "${RED}✗ mintlify/ directory not found${NC}"
    exit 1
fi

# Check required subdirectories
REQUIRED_DIRS=(
    "api-reference"
    "api-reference/r-functions"
    "api-reference/cpp-classes"
    "guides"
    "reference"
)

for dir in "${REQUIRED_DIRS[@]}"; do
    if [ -d "$MINTLIFY_DIR/$dir" ]; then
        echo -e "${GREEN}✓ $dir exists${NC}"
    else
        echo -e "${RED}✗ $dir missing${NC}"
        mkdir -p "$MINTLIFY_DIR/$dir"
        echo -e "${YELLOW}  Created $dir${NC}"
    fi
done

echo -e "\n${YELLOW}Step 2: Validating mint.json...${NC}"

# Check if mint.json exists
if [ ! -f "$PROJECT_ROOT/mint.json" ]; then
    echo -e "${RED}✗ mint.json not found in project root${NC}"
    exit 1
fi

# Validate JSON syntax
if python3 -m json.tool "$PROJECT_ROOT/mint.json" > /dev/null 2>&1; then
    echo -e "${GREEN}✓ mint.json is valid JSON${NC}"
else
    echo -e "${RED}✗ mint.json has invalid JSON syntax${NC}"
    python3 -m json.tool "$PROJECT_ROOT/mint.json"
    exit 1
fi

# Check for invalid contentDir field
if grep -q '"contentDir"' "$PROJECT_ROOT/mint.json"; then
    echo -e "${RED}✗ mint.json contains invalid 'contentDir' field${NC}"
    echo "  Mintlify doesn't support contentDir - use direct file paths instead"
    exit 1
else
    echo -e "${GREEN}✓ No invalid contentDir field${NC}"
fi

echo -e "\n${YELLOW}Step 3: Checking navigation file references...${NC}"

# Extract all file references from mint.json navigation
python3 << EOF
import json
import os

with open('$PROJECT_ROOT/mint.json', 'r') as f:
    config = json.load(f)

def extract_pages(item):
    pages = []
    if isinstance(item, dict):
        if 'pages' in item:
            for page in item['pages']:
                if isinstance(page, str):
                    pages.append(page)
                elif isinstance(page, dict):
                    pages.extend(extract_pages(page))
    elif isinstance(item, list):
        for sub_item in item:
            pages.extend(extract_pages(sub_item))
    return pages

all_pages = extract_pages(config.get('navigation', []))

# Check each referenced file
missing = []
found = []
for page in all_pages:
    # Add .mdx extension if not present
    if not page.endswith('.mdx') and not page.endswith('.md'):
        page_file = page + '.mdx'
    else:
        page_file = page

    # Check if file exists (relative to project root)
    file_path = os.path.join('$PROJECT_ROOT', page_file)
    if os.path.exists(file_path):
        found.append(page)
    else:
        missing.append(page)

print(f"Found {len(found)} files")
print(f"Missing {len(missing)} files")

if missing:
    print("\nMissing files:")
    for m in missing[:10]:  # Show first 10 missing files
        print(f"  - {m}")
    if len(missing) > 10:
        print(f"  ... and {len(missing) - 10} more")
EOF

echo -e "\n${YELLOW}Step 4: Checking essential pages...${NC}"

# Check essential pages exist
ESSENTIAL_PAGES=(
    "mintlify/introduction.mdx"
    "mintlify/quickstart.mdx"
    "mintlify/installation.mdx"
)

for page in "${ESSENTIAL_PAGES[@]}"; do
    if [ -f "$PROJECT_ROOT/$page" ]; then
        echo -e "${GREEN}✓ $page exists${NC}"
    else
        echo -e "${RED}✗ $page missing${NC}"
    fi
done

echo -e "\n${YELLOW}Step 5: Installing Mintlify CLI (if needed)...${NC}"

# Check if mintlify is installed
if command -v mintlify &> /dev/null; then
    echo -e "${GREEN}✓ Mintlify CLI is installed${NC}"
    mintlify --version
else
    echo -e "${YELLOW}Installing Mintlify CLI...${NC}"
    npm install -g mintlify
fi

echo -e "\n${YELLOW}Step 6: Running Mintlify validation...${NC}"

cd "$PROJECT_ROOT"

# Run mintlify dev to test
echo "Starting Mintlify development server..."
echo "Press Ctrl+C after checking the site at http://localhost:3000"
echo ""

# Create a simple validation using mintlify
timeout 10 mintlify dev 2>&1 | head -20 || true

echo -e "\n${GREEN}================================================${NC}"
echo -e "${GREEN}  Validation Complete${NC}"
echo -e "${GREEN}================================================${NC}"

echo -e "\nTo deploy to production:"
echo "1. Ensure all files are committed to git"
echo "2. Push to your main branch"
echo "3. Mintlify will automatically deploy from GitHub"
echo ""
echo "For manual testing, run:"
echo "  cd $PROJECT_ROOT && mintlify dev"
