# qiprng Documentation

This directory contains the Mintlify documentation for the qiprng R package, hosted at [qiprng.biostochastics.com](https://qiprng.biostochastics.com).

## Documentation Structure

```
mintlify/
├── mint.json                    # Main Mintlify configuration
├── introduction.mdx             # Package introduction (auto-generated from README)
├── quickstart.mdx              # Getting started guide
├── DEPLOYMENT.md               # Deployment instructions
├── api-reference/              # API documentation
│   ├── r-functions/           # R function docs (auto-generated)
│   ├── cpp/                   # C++ internals documentation
│   └── caching/               # Caching framework docs
├── concepts/                   # Conceptual documentation
│   └── mathematical-theory.mdx # Mathematical foundations
├── reference/                  # Reference materials
│   ├── migration-guide.mdx   # Version migration guide
│   └── changelog.mdx          # Version history (auto-generated)
└── images/                    # Documentation assets
```

## Automated Documentation Pipeline

The documentation system uses a hybrid approach:

### Automated Components

1. **R Function Documentation**: Generated from roxygen2 comments
2. **C++ Class Documentation**: Extracted from inline comments
3. **Changelog**: Converted from CHANGELOG.md
4. **Introduction**: Converted from README.md

### Manual Components

1. **Mathematical Theory**: Hand-written with LaTeX formulas
2. **Architecture Guide**: Detailed C++ design documentation
3. **Migration Guide**: Version upgrade instructions
4. **Quickstart Guide**: Getting started tutorial

## Local Development

### Prerequisites

```bash
# Install Node.js dependencies
npm install -g mintlify

# Install R dependencies
R -e "install.packages(c('roxygen2', 'jsonlite'))"

# Install Python dependencies
pip install pyyaml markdown2
```

### Build Documentation Locally

```bash
# Generate R documentation
Rscript -e "roxygen2::roxygenize()"

# Convert to MDX
Rscript scripts/roxygen2_to_mintlify.R

# Extract C++ docs
python scripts/cpp_to_mintlify.py

# Validate structure
./scripts/validate_docs.sh

# Preview locally
cd mintlify
mintlify dev
```

## Deployment

Documentation automatically deploys to production when changes are pushed to the `main` branch via GitHub Actions.

### Manual Deployment

```bash
cd mintlify
MINTLIFY_API_KEY=<your-key> mintlify deploy --prod
```

### DNS Configuration

The domain `qiprng.biostochastics.com` should have a CNAME record pointing to `hosting.mintlify.com`.

## Key Features

- **Automated Conversion**: roxygen2 → MDX pipeline
- **C++ Documentation**: Inline comment extraction
- **Mathematical Formulas**: KaTeX/LaTeX support
- **Code Examples**: Syntax-highlighted R and C++
- **Search**: Full-text search across all documentation
- **Responsive Design**: Mobile-friendly interface
- **Version Tracking**: Synchronized with package version

## Maintenance

### Adding New R Functions

1. Document function with roxygen2 in R/
2. Run `roxygen2::roxygenize()`
3. Push to main branch (auto-converts and deploys)

### Adding New C++ Classes

1. Add inline documentation to .hpp files
2. Push to main branch (auto-extracts and deploys)

### Updating Manual Pages

1. Edit .mdx files directly in mintlify/
2. Test locally with `mintlify dev`
3. Push to main branch

## Support

- **Documentation Issues**: [GitHub Issues](https://github.com/biostochastics/qiprng/issues)
- **Mintlify Support**: [support@mintlify.com](mailto:support@mintlify.com)
- **Package Maintainer**: [support@biostochastics.com](mailto:support@biostochastics.com)

## License

Documentation is provided under the same GPL-3 license as the qiprng package.
