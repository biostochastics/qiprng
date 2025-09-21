# qiprng Documentation Pipeline

## Overview

The qiprng project uses a comprehensive documentation pipeline that integrates multiple sources and generates complete Mintlify documentation with validation data, API references, and user guides.

## Pipeline Components

### 1. Main Pipeline Script

**File:** `generate_all_docs.sh`

- Complete documentation generation pipeline
- Integrates all documentation sources
- Validates output structure
- Provides statistics and next steps

### 2. Mintlify Generation

**File:** `generate_mintlify_complete.R`

- Converts README to Introduction
- Generates NIST validation documentation
- Creates discriminant data documentation
- Processes R function documentation (104 functions)
- Extracts C++ class documentation (9 classes)
- Creates comprehensive guides (4 guides)
- Generates reference pages (6 pages)

### 3. Validation Script

**File:** `test_mintlify_deployment.sh`

- Validates mint.json configuration
- Checks directory structure
- Verifies file references
- Tests Mintlify CLI

### 4. Quick Regeneration

**File:** `regenerate_docs.sh`

- Quick wrapper for documentation regeneration
- Runs generation and validation

## Documentation Structure

```
qiprng/
├── mint.json                    # Mintlify configuration
├── mintlify/
│   ├── introduction.mdx        # README as introduction
│   ├── quickstart.mdx          # Quick start guide
│   ├── installation.mdx        # Installation instructions
│   ├── api-reference/
│   │   ├── introduction.mdx    # API overview
│   │   ├── r-functions/        # 104 R function docs
│   │   └── cpp-classes/        # 9 C++ class docs
│   ├── guides/
│   │   ├── basic-usage.mdx
│   │   ├── distributions.mdx
│   │   ├── parallel-generation.mdx
│   │   └── performance-tuning.mdx
│   ├── validation/
│   │   ├── nist-validation.mdx    # NIST SP 800-22 results
│   │   └── discriminant-data.mdx  # Discriminant documentation
│   └── reference/
│       ├── changelog.mdx
│       ├── news.mdx
│       ├── benchmarks.mdx
│       ├── security.mdx
│       └── mathematical-theory.mdx
```

## Key Features

### Integrated Documentation

- **README Integration**: Automatically converts README.md to introduction.mdx
- **Validation Data**: Includes NIST SP 800-22 test results (98.4% pass rate)
- **Discriminant Data**: Documents pre-validated discriminants
- **API Reference**: Complete documentation for all functions and classes

### Statistics (Latest Generation)

- 104 R function references
- 9 C++ class descriptions
- 4 comprehensive user guides
- 6 reference documents
- 2 validation documents
- **Total**: 128 documentation files

## Usage

### Generate Complete Documentation

```bash
# Run the main pipeline
bash scripts/generate_all_docs.sh
```

### Quick Regeneration

```bash
# Regenerate documentation quickly
bash scripts/regenerate_docs.sh
```

### Test Documentation

```bash
# Validate structure
bash scripts/test_mintlify_deployment.sh

# Test locally with Mintlify
mintlify dev
```

### Deploy Documentation

```bash
# Commit and push to deploy
git add .
git commit -m "Update documentation"
git push
```

## Pipeline Features

### Automatic Integration

- Pulls content from README.md
- Extracts from CHANGELOG.md and NEWS.md
- Processes Roxygen2 documentation
- Parses C++ header files

### Validation

- JSON syntax validation
- File reference checking
- Directory structure verification
- Essential file validation

### Rich Formatting

- Mintlify components (Tabs, Cards, Steps, Notes, Warnings)
- Code syntax highlighting
- Mathematical expressions
- Interactive navigation

## Configuration

### mint.json

The main Mintlify configuration file includes:

- Navigation structure (7 groups, 32 pages)
- Color scheme and branding
- Social links and feedback options
- Search configuration

### Customization

Modify `generate_mintlify_complete.R` to:

- Add new documentation sections
- Change navigation structure
- Update styling and formatting

## Troubleshooting

### Common Issues

1. **Missing R packages**

   ```r
   install.packages(c("roxygen2", "jsonlite", "stringr"))
   ```

2. **Mintlify CLI not installed**

   ```bash
   npm install -g mintlify
   ```

3. **Permission errors**

   ```bash
   chmod +x scripts/*.sh
   ```

## Maintenance

### Adding New Functions

1. Document in R with Roxygen2
2. Run `roxygen2::roxygenize()`
3. Execute pipeline: `bash scripts/generate_all_docs.sh`

### Updating Validation Data

1. Edit validation results in the generation script
2. Regenerate: `Rscript scripts/generate_mintlify_complete.R`

### Modifying Navigation

1. Edit navigation structure in `generate_mintlify_complete.R`
2. Regenerate and test locally

## Performance

Pipeline execution time: ~5-10 seconds

- R documentation processing: ~3 seconds
- C++ extraction: ~1 second
- File generation: ~1 second
- Validation: ~1 second

## Support

For issues or improvements:

- Report at: <https://github.com/biostochastics/qiprng/issues>
- Contact: <sergey.kornilov@biostochastics.com>
