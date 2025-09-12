#!/bin/bash
# Test the complete documentation generation pipeline

set -e  # Exit on error

echo "================================================"
echo "  Testing Documentation Generation Pipeline"
echo "================================================"

cd "$(dirname "$0")/.."
PROJECT_ROOT=$(pwd)

# 1. Test R script exists and is executable
echo ""
echo "1. Checking documentation generation script..."
if [ -f "scripts/generate_mintlify_docs.R" ]; then
    echo "✓ generate_mintlify_docs.R exists"
else
    echo "✗ generate_mintlify_docs.R not found"
    exit 1
fi

# 2. Run documentation generation
echo ""
echo "2. Running documentation generation..."
Rscript scripts/generate_mintlify_docs.R

# 3. Validate mint.json
echo ""
echo "3. Validating mint.json..."
if python3 -m json.tool mintlify/mint.json > /dev/null 2>&1; then
    echo "✓ mint.json is valid JSON"
else
    echo "✗ mint.json has invalid syntax"
    exit 1
fi

# 4. Check essential files
echo ""
echo "4. Checking essential documentation files..."
ESSENTIAL_FILES=(
    "mintlify/mint.json"
    "mintlify/introduction.mdx"
    "mintlify/quickstart.mdx"
    "mintlify/installation.mdx"
    "mintlify/api-reference/introduction.mdx"
    "mintlify/reference/changelog.mdx"
    "mintlify/reference/news.mdx"
)

for file in "${ESSENTIAL_FILES[@]}"; do
    if [ -f "$file" ]; then
        echo "✓ $(basename $file) exists"
    else
        echo "✗ $file missing"
        exit 1
    fi
done

# 5. Count documentation files
echo ""
echo "5. Documentation statistics:"
R_DOCS=$(find mintlify/api-reference/r-functions -name "*.mdx" 2>/dev/null | wc -l)
CPP_DOCS=$(find mintlify/api-reference/cpp-classes -name "*.mdx" 2>/dev/null | wc -l)
GUIDE_DOCS=$(find mintlify/guides -name "*.mdx" 2>/dev/null | wc -l)
TOTAL_DOCS=$(find mintlify -name "*.mdx" 2>/dev/null | wc -l)

echo "  - R function documentation: $R_DOCS files"
echo "  - C++ class documentation: $CPP_DOCS files"
echo "  - Guide documentation: $GUIDE_DOCS files"
echo "  - Total MDX files: $TOTAL_DOCS files"

# 6. Validate no problematic filenames
echo ""
echo "6. Checking for problematic filenames..."
PROBLEMATIC=$(find mintlify -name "*.mdx" -exec basename {} \; | grep -E '[{}:;<>|]' || true)
if [ -z "$PROBLEMATIC" ]; then
    echo "✓ No problematic filenames found"
else
    echo "✗ Found files with problematic characters:"
    echo "$PROBLEMATIC"
    exit 1
fi

# 7. Test local preview (if npx available)
echo ""
echo "7. Testing Mintlify CLI..."
if command -v npx &> /dev/null; then
    cd mintlify
    # Just check if mintlify CLI works, don't start server
    if npx mintlify version 2>/dev/null | grep -q "mintlify"; then
        echo "✓ Mintlify CLI is available"
    else
        echo "⚠ Mintlify CLI not properly installed"
    fi
    cd ..
else
    echo "⚠ npx not available - skipping Mintlify CLI test"
fi

# Summary
echo ""
echo "================================================"
echo "              Test Summary"
echo "================================================"
if [ $R_DOCS -gt 50 ] && [ $CPP_DOCS -gt 0 ] && [ $TOTAL_DOCS -gt 100 ]; then
    echo "✓ Documentation generation successful!"
    echo ""
    echo "Documentation is ready for deployment:"
    echo "  1. Commit changes to GitHub"
    echo "  2. Push to main branch"
    echo "  3. Mintlify will auto-deploy to qiprng.biostochastics.com"
    exit 0
else
    echo "⚠ Documentation may be incomplete"
    echo "  Expected: >50 R docs, >0 C++ docs, >100 total"
    echo "  Found: $R_DOCS R docs, $CPP_DOCS C++ docs, $TOTAL_DOCS total"
    exit 1
fi
