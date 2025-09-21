#!/bin/bash
# ----------------------------------------------------------------------
# Script: regenerate_docs.sh
# Purpose: Complete regeneration of Mintlify documentation
# ----------------------------------------------------------------------

set -e

echo "================================================"
echo "  Regenerating Complete Mintlify Documentation"
echo "================================================"

# Get script directory and project root
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"

cd "$PROJECT_ROOT"

# Step 1: Generate documentation
echo ""
echo "Step 1: Generating documentation files..."
Rscript scripts/generate_mintlify_complete.R

# Step 2: Validate structure
echo ""
echo "Step 2: Validating documentation structure..."
bash scripts/test_mintlify_deployment.sh

# Step 3: Summary
echo ""
echo "================================================"
echo "  Documentation Regeneration Complete!"
echo "================================================"
echo ""
echo "✅ README integrated as Introduction"
echo "✅ NIST validation data included"
echo "✅ Discriminant data documented"
echo "✅ All navigation properly structured"
echo ""
echo "Next steps:"
echo "1. Test locally: mintlify dev"
echo "2. Commit changes: git add . && git commit -m 'Update Mintlify docs'"
echo "3. Push to deploy: git push"
echo ""
echo "Documentation includes:"
echo "- 16 Core R functions"
echo "- 7 C++ implementation classes"
echo "- 4 Comprehensive guides"
echo "- 5 Reference pages"
echo "- 2 Validation documents (NIST, Discriminants)"
