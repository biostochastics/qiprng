# QIPRNG CI/CD Complete Tutorial: From Zero to Production Pipeline

## Table of Contents
1. [Introduction - What We're Building](#introduction)
2. [Architecture Diagram](#architecture-diagram)
3. [Prerequisites - Getting Started](#prerequisites)
4. [Step 1: Setting Up Your First Workflow](#step-1-first-workflow)
5. [Step 2: Cross-Platform Testing](#step-2-cross-platform)
6. [Step 3: Code Coverage](#step-3-coverage)
7. [Step 4: Security Scanning](#step-4-security)
8. [Step 5: Performance Monitoring](#step-5-performance)
9. [Step 6: Documentation Automation](#step-6-documentation)
10. [Step 7: Release Automation](#step-7-releases)
11. [Troubleshooting Guide](#troubleshooting)
12. [Maintenance & Best Practices](#maintenance)

## Introduction - What We're Building {#introduction}

Welcome! This tutorial will walk you through building a complete CI/CD (Continuous Integration/Continuous Deployment) pipeline for an R package, using QIPRNG as our example. By the end, you'll have:

- ✅ Automatic testing on every code change
- ✅ Cross-platform compatibility (Windows, macOS, Linux)
- ✅ Security vulnerability scanning
- ✅ Performance regression detection
- ✅ Automated documentation websites
- ✅ One-click releases to CRAN and GitHub

**Time Required**: 2-3 hours to implement everything
**Difficulty**: Intermediate (we'll explain everything!)

### Why Do We Need CI/CD?

Imagine you're working on your R package and you:
- Make a change that breaks Windows compatibility (but you develop on Mac)
- Introduce a memory leak that only shows up after 1000 iterations
- Accidentally slow down a function by 50%
- Forget to update documentation

Without CI/CD, these issues might only be discovered by users after release. With CI/CD, they're caught automatically!

## Architecture Diagram

```ascii
┌─────────────────────────────────────────────────────────────────────────────────────────┐
│                                    QIPRNG CI/CD PIPELINE                                │
└─────────────────────────────────────────────────────────────────────────────────────────┘

┌─────────────────┐
│   DEVELOPER     │
│   ACTIONS       │
└────────┬────────┘
         │
         ▼
┌─────────────────────────────────────────────────────────────────────────────────────────┐
│                                    VERSION CONTROL                                      │
├─────────────────────────────────────────────────────────────────────────────────────────┤
│  ┌──────────────┐     ┌──────────────┐     ┌──────────────┐     ┌──────────────┐      │
│  │  git push    │     │  Pull Request│     │    Tag       │     │   Schedule   │      │
│  │  (branch)    │     │   (PR)       │     │  (v0.2.7)    │     │   (weekly)   │      │
│  └──────┬───────┘     └──────┬───────┘     └──────┬───────┘     └──────┬───────┘      │
│         │                     │                     │                     │              │
└─────────┼─────────────────────┼─────────────────────┼─────────────────────┼──────────────┘
          │                     │                     │                     │
          ▼                     ▼                     ▼                     ▼
┌─────────────────────────────────────────────────────────────────────────────────────────┐
│                                   GITHUB ACTIONS                                        │
├─────────────────────────────────────────────────────────────────────────────────────────┤
│                                                                                         │
│  ┌───────────────────────────────────────────────────────────────────────────────────┐ │
│  │                              R-CMD-CHECK WORKFLOW                                  │ │
│  ├───────────────────────────────────────────────────────────────────────────────────┤ │
│  │                                                                                    │ │
│  │  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐            │ │
│  │  │   Ubuntu    │  │   macOS     │  │   Windows   │  │   Matrix    │            │ │
│  │  │   latest    │  │   latest    │  │   latest    │  │  R-release  │            │ │
│  │  └──────┬──────┘  └──────┬──────┘  └──────┬──────┘  │  R-devel    │            │ │
│  │         │                 │                 │         │  R-oldrel   │            │ │
│  │         ▼                 ▼                 ▼         └─────────────┘            │ │
│  │  ┌────────────────────────────────────────────────┐                              │ │
│  │  │            System Dependencies                  │                              │ │
│  │  │  • apt-get: libmpfr-dev libgmp-dev            │                              │ │
│  │  │  • brew: mpfr gmp libsodium openssl           │                              │ │
│  │  │  • choco: rtools mpfr gmp                     │                              │ │
│  │  └────────────────────────────────────────────────┘                              │ │
│  │         │                                                                         │ │
│  │         ▼                                                                         │ │
│  │  ┌────────────────────────────────────────────────┐                              │ │
│  │  │              R Package Cache                    │                              │ │
│  │  │  • actions/cache@v3                            │                              │ │
│  │  │  • Key: ${{ runner.os }}-r-${{ matrix.r }}    │                              │ │
│  │  └────────────────────────────────────────────────┘                              │ │
│  │         │                                                                         │ │
│  │         ▼                                                                         │ │
│  │  ┌────────────────────────────────────────────────┐                              │ │
│  │  │            R CMD check --as-cran                │                              │ │
│  │  │  • Check DESCRIPTION                           │                              │ │
│  │  │  • Compile C++ code                            │                              │ │
│  │  │  • Run tests                                   │                              │ │
│  │  │  • Check examples                              │                              │ │
│  │  │  • Verify documentation                        │                              │ │
│  │  └────────────────────────────────────────────────┘                              │ │
│  │         │                                                                         │ │
│  │         ▼                                                                         │ │
│  │  ┌────────────────────────────────────────────────┐                              │ │
│  │  │          Upload Check Results                   │                              │ │
│  │  │  • actions/upload-artifact@v3                  │                              │ │
│  │  │  • Name: check-results-${{ matrix.os }}        │                              │ │
│  │  └────────────────────────────────────────────────┘                              │ │
│  │                                                                                    │ │
│  └────────────────────────────────────────────────────────────────────────────────────┘ │
│                                                                                         │
│  ┌───────────────────────────────────────────────────────────────────────────────────┐ │
│  │                            TEST COVERAGE WORKFLOW                                  │ │
│  ├───────────────────────────────────────────────────────────────────────────────────┤ │
│  │                                                                                    │ │
│  │  ┌─────────────┐     ┌─────────────────┐     ┌──────────────────┐               │ │
│  │  │   covr      │────▶│  Calculate      │────▶│   Upload to      │               │ │
│  │  │  package    │     │  Coverage       │     │   Codecov.io     │               │ │
│  │  └─────────────┘     └─────────────────┘     └──────────────────┘               │ │
│  │                                                                                    │ │
│  └────────────────────────────────────────────────────────────────────────────────────┘ │
│                                                                                         │
│  ┌───────────────────────────────────────────────────────────────────────────────────┐ │
│  │                           SECURITY SCANNING WORKFLOW                               │ │
│  ├───────────────────────────────────────────────────────────────────────────────────┤ │
│  │                                                                                    │ │
│  │  ┌─────────────────┐  ┌──────────────────┐  ┌───────────────────┐              │ │
│  │  │  CodeQL C++     │  │  Dependency      │  │   Memory Safety   │              │ │
│  │  │  Analysis       │  │  Scanning        │  │   • valgrind      │              │ │
│  │  │  • Buffer       │  │  • libsodium     │  │   • AddressSan    │              │ │
│  │  │  • Integer      │  │  • OpenSSL       │  │   • ThreadSan     │              │ │
│  │  │    overflow     │  │  • MPFR/GMP      │  │                   │              │ │
│  │  └─────────────────┘  └──────────────────┘  └───────────────────┘              │ │
│  │                                                                                    │ │
│  └────────────────────────────────────────────────────────────────────────────────────┘ │
│                                                                                         │
│  ┌───────────────────────────────────────────────────────────────────────────────────┐ │
│  │                          PERFORMANCE BENCHMARK WORKFLOW                            │ │
│  ├───────────────────────────────────────────────────────────────────────────────────┤ │
│  │                                                                                    │ │
│  │  ┌──────────────┐     ┌──────────────────┐     ┌──────────────────┐             │ │
│  │  │ Benchmark    │────▶│  Compare with    │────▶│  Flag            │             │ │
│  │  │ Suite        │     │  Previous        │     │  Regressions     │             │ │
│  │  │ • Speed      │     │  • Store in DB   │     │  • >10% slower   │             │ │
│  │  │ • Memory     │     │  • Graph trends  │     │  • Comment PR    │             │ │
│  │  └──────────────┘     └──────────────────┘     └──────────────────┘             │ │
│  │                                                                                    │ │
│  └────────────────────────────────────────────────────────────────────────────────────┘ │
│                                                                                         │
│  ┌───────────────────────────────────────────────────────────────────────────────────┐ │
│  │                            RELEASE WORKFLOW (on tag)                               │ │
│  ├───────────────────────────────────────────────────────────────────────────────────┤ │
│  │                                                                                    │ │
│  │     ┌────────────┐     ┌────────────┐     ┌────────────┐     ┌────────────┐     │ │
│  │     │   Build    │     │   Build    │     │  Generate  │     │   Create   │     │ │
│  │     │  Source    │────▶│  Binaries  │────▶│  Changelog │────▶│  Release   │     │ │
│  │     │  Tarball   │     │  • Win     │     │  from      │     │  • Upload  │     │ │
│  │     │            │     │  • Mac     │     │  commits   │     │    assets  │     │ │
│  │     └────────────┘     └────────────┘     └────────────┘     └─────┬──────┘     │ │
│  │                                                                      │            │ │
│  │                                                                      ▼            │ │
│  │                                                              ┌──────────────┐    │ │
│  │                                                              │  Submit to   │    │ │
│  │                                                              │    CRAN      │    │ │
│  │                                                              │  (manual)    │    │ │
│  │                                                              └──────────────┘    │ │
│  │                                                                                    │ │
│  └────────────────────────────────────────────────────────────────────────────────────┘ │
│                                                                                         │
└─────────────────────────────────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────────────────────────────────┐
│                                   QUALITY GATES                                         │
├─────────────────────────────────────────────────────────────────────────────────────────┤
│                                                                                         │
│  ┌─────────────────┐  ┌─────────────────┐  ┌─────────────────┐  ┌─────────────────┐  │
│  │   R CMD check   │  │  Code Coverage  │  │    Security     │  │   Performance   │  │
│  │   • No ERRORs   │  │   • > 80%       │  │  • No CVEs      │  │  • No regression│  │
│  │   • No WARNINGs │  │   • Core: 95%   │  │  • CodeQL pass  │  │  • Memory stable│  │
│  │   • Notes OK    │  │                 │  │  • Valgrind OK  │  │                 │  │
│  └─────────────────┘  └─────────────────┘  └─────────────────┘  └─────────────────┘  │
│                                                                                         │
└─────────────────────────────────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────────────────────────────────┐
│                                 DEPLOYMENT TARGETS                                      │
├─────────────────────────────────────────────────────────────────────────────────────────┤
│                                                                                         │
│  ┌──────────────────┐  ┌──────────────────┐  ┌──────────────────┐  ┌────────────────┐ │
│  │   GitHub Pages   │  │  GitHub Release  │  │      CRAN        │  │   R-universe   │ │
│  │   • pkgdown      │  │  • Source        │  │  • Source pkg    │  │  • Automated   │ │
│  │   • Vignettes    │  │  • Binaries      │  │  • Strict checks │  │  • Daily builds│ │
│  │   • Reference    │  │  • NEWS          │  │  • Manual review │  │                │ │
│  └──────────────────┘  └──────────────────┘  └──────────────────┘  └────────────────┘ │
│                                                                                         │
└─────────────────────────────────────────────────────────────────────────────────────────┘
```

## Prerequisites - Getting Started {#prerequisites}

Don't worry if you've never set up CI/CD before! We'll walk through each step together.

### What You'll Need

#### 1. A GitHub Account and Repository
If you don't have one:
1. Go to [github.com](https://github.com) and sign up (it's free!)
2. Create a new repository for your R package:
   - Click the green "New" button
   - Name it (e.g., "mypackage" or "qiprng")
   - Make it public (required for free CI/CD)
   - Initialize with README
   - Click "Create repository"

#### 2. Enable GitHub Actions
GitHub Actions is usually enabled by default, but let's check:
1. Go to your repository on GitHub
2. Click "Settings" (top menu)
3. Click "Actions" (left sidebar)
4. Select "Allow all actions and reusable workflows"
5. Click "Save"

#### 3. Set Up Codecov (5 minutes)
Codecov shows how much of your code is tested. Here's how:

1. Go to [codecov.io](https://codecov.io)
2. Click "Sign up" → "Sign up with GitHub"
3. Authorize Codecov to access your GitHub
4. Find your repository in the list and click "Setup repo"
5. Copy the token that appears (looks like: `a1b2c3d4-e5f6-g7h8-i9j0-k1l2m3n4o5p6`)

Now add this token to GitHub:
1. In your GitHub repository, go to Settings → Secrets and variables → Actions
2. Click "New repository secret"
3. Name: `CODECOV_TOKEN`
4. Value: Paste the token you copied
5. Click "Add secret"

#### 4. Enable GitHub Pages (2 minutes)
This will host your package documentation:

1. In your repository, go to Settings → Pages
2. Source: Select "Deploy from a branch"
3. Branch: Select "gh-pages" (we'll create this later)
4. Folder: Select "/ (root)"
5. Click "Save"

### Your First GitHub Action File

Let's understand the structure. GitHub Actions live in a special folder:

```
your-package/
├── .github/
│   └── workflows/
│       ├── check.yaml      # We'll create this first!
│       ├── coverage.yaml   # Then this
│       └── ...            # And more
├── R/
├── src/
└── DESCRIPTION
```

**Important**: The folder MUST be named `.github/workflows/` (note the dot!)

## Step 1: Setting Up Your First Workflow {#step-1-first-workflow}

Let's create your first GitHub Action! This will automatically check your package every time you push code.

### Creating the Workflow File

1. **In your local repository**, create the folders:
   ```bash
   mkdir -p .github/workflows
   ```
   
2. **Create a new file** called `.github/workflows/R-CMD-check.yaml`

3. **Copy this starter workflow** (I'll explain every line!):

```yaml
# This is the name that appears in the Actions tab
name: R-CMD-check

# When should this workflow run?
on:
  push:
    branches: [main, master]  # Run when pushing to main/master
  pull_request:
    branches: [main, master]  # Run on pull requests
  schedule:
    - cron: '0 0 * * 0'      # Also run weekly (Sunday midnight)

# What jobs should run?
jobs:
  R-CMD-check:
    # Run on Ubuntu Linux (we'll add more OS later)
    runs-on: ubuntu-latest
    
    # Environment variables available to all steps
    env:
      GITHUB_PAT: ${{ secrets.GITHUB_TOKEN }}
      R_KEEP_PKG_SOURCE: yes
    
    # The actual steps to run
    steps:
      # Step 1: Download your code
      - uses: actions/checkout@v4
      
      # Step 2: Set up R
      - uses: r-lib/actions/setup-r@v2
        with:
          r-version: 'release'  # Use latest R release
          use-public-rspm: true # Faster package installation
      
      # Step 3: Install system dependencies (for Linux)
      - name: Install system dependencies
        run: |
          sudo apt-get update
          sudo apt-get install -y libcurl4-openssl-dev
      
      # Step 4: Install R dependencies
      - uses: r-lib/actions/setup-r-dependencies@v2
        with:
          extra-packages: any::rcmdcheck
          needs: check
      
      # Step 5: Run R CMD check
      - uses: r-lib/actions/check-r-package@v2
      
      # Step 6: Upload results if something fails
      - name: Upload check results
        if: failure()
        uses: actions/upload-artifact@v4
        with:
          name: check-results
          path: check
```

### Understanding Each Part

Let me explain what each section does:

**The `on:` section** - Your workflow triggers:
- `push:` - Runs when you `git push`
- `pull_request:` - Runs when someone opens a PR
- `schedule:` - Runs automatically (cron format)
  - `0 0 * * 0` means: minute 0, hour 0, any day, any month, Sunday

**The `jobs:` section** - What actually runs:
- `runs-on: ubuntu-latest` - Uses Ubuntu Linux (free!)
- `env:` - Sets environment variables
  - `GITHUB_PAT` - Allows downloading packages from GitHub
  - `R_KEEP_PKG_SOURCE` - Keeps source code for better errors

**The `steps:` section** - Your recipe:
1. **Checkout** - Downloads your repository code
2. **Setup R** - Installs R (smart enough to cache!)
3. **System deps** - Installs Linux libraries (if needed)
4. **R deps** - Installs packages from your DESCRIPTION
5. **Check** - Runs `R CMD check` (the main test!)
6. **Upload** - Saves logs if something breaks

### Testing Your First Workflow

1. **Save the file** and commit:
   ```bash
   git add .github/workflows/R-CMD-check.yaml
   git commit -m "Add R CMD check workflow"
   git push
   ```

2. **Watch it run**:
   - Go to your GitHub repository
   - Click the "Actions" tab
   - You should see your workflow running!
   - Click on it to see live logs

3. **What success looks like**:
   - Green checkmark ✅ = Everything passed!
   - Red X ❌ = Something failed (click to see why)

### Common First-Time Issues

**"Package dependencies not found"**
Add them to your DESCRIPTION file:
```r
Imports:
    ggplot2,
    dplyr
```

**"System dependency missing"**
Add to the system dependencies step:
```yaml
- name: Install system dependencies
  run: |
    sudo apt-get update
    sudo apt-get install -y \
      libcurl4-openssl-dev \
      libxml2-dev          # Add what you need!
```

## Step 2: Cross-Platform Testing {#step-2-cross-platform}

Now let's make sure your package works on ALL operating systems! We'll upgrade your workflow to test on Windows, macOS, and Linux.

### Upgrading to Multi-Platform

Replace your entire `.github/workflows/R-CMD-check.yaml` with this enhanced version:

```yaml
name: R-CMD-check

on:
  push:
    branches: [main, master]
  pull_request:
    branches: [main, master]
  schedule:
    - cron: '0 0 * * 0'

jobs:
  R-CMD-check:
    # This is the magic line - it creates multiple jobs!
    runs-on: ${{ matrix.config.os }}
    
    name: ${{ matrix.config.os }} (${{ matrix.config.r }})
    
    # Define the test matrix
    strategy:
      fail-fast: false  # Don't cancel other jobs if one fails
      matrix:
        config:
          # Test on 3 R versions on Linux
          - {os: ubuntu-latest,   r: 'release'}
          - {os: ubuntu-latest,   r: 'devel'}
          - {os: ubuntu-latest,   r: 'oldrel'}
          # Test on latest R for Mac and Windows
          - {os: macOS-latest,    r: 'release'}
          - {os: windows-latest,  r: 'release'}
    
    env:
      GITHUB_PAT: ${{ secrets.GITHUB_TOKEN }}
      R_KEEP_PKG_SOURCE: yes
    
    steps:
      - uses: actions/checkout@v4
      
      - uses: r-lib/actions/setup-r@v2
        with:
          r-version: ${{ matrix.config.r }}
          use-public-rspm: true
      
      # Platform-specific system dependencies
      - name: Install Linux system dependencies
        if: runner.os == 'Linux'
        run: |
          sudo apt-get update
          sudo apt-get install -y \
            libcurl4-openssl-dev \
            libssl-dev \
            libxml2-dev
      
      - name: Install macOS system dependencies  
        if: runner.os == 'macOS'
        run: |
          brew install pkg-config
          brew install openssl@3
      
      # Windows dependencies usually come with Rtools
      
      - uses: r-lib/actions/setup-r-dependencies@v2
        with:
          extra-packages: any::rcmdcheck
          needs: check
      
      - uses: r-lib/actions/check-r-package@v2
        with:
          upload-snapshots: true
      
      - name: Upload check results
        if: failure()
        uses: actions/upload-artifact@v4
        with:
          name: ${{ runner.os }}-r${{ matrix.config.r }}-results
          path: check
```

### Understanding the Matrix Strategy

The magic happens in the `strategy:` section:

```yaml
strategy:
  fail-fast: false  # Keep running even if one fails
  matrix:
    config:
      - {os: ubuntu-latest,   r: 'release'}
      - {os: ubuntu-latest,   r: 'devel'}
      # ... more combinations
```

This creates 5 separate jobs:
1. Ubuntu + R release (current version)
2. Ubuntu + R devel (tomorrow's version)
3. Ubuntu + R oldrel (yesterday's version)
4. macOS + R release
5. Windows + R release

### Platform-Specific Dependencies

Notice the `if: runner.os == 'Linux'` conditions? These let you run different commands per OS:

**For Linux packages:**
```yaml
if: runner.os == 'Linux'
run: |
  sudo apt-get install -y libxml2-dev
```

**For macOS packages:**
```yaml
if: runner.os == 'macOS'
run: |
  brew install pkg-config
```

**For Windows:** Usually handled by Rtools, but you can add:
```yaml
if: runner.os == 'Windows'
run: |
  choco install somepackage
```

### Viewing Your Multi-Platform Results

After pushing this update:

1. Go to Actions tab
2. Click on your workflow run
3. You'll see 5 jobs running in parallel!
4. Each shows its OS and R version
5. Click any job to see its specific logs

### Handling Platform-Specific Failures

**Common Linux Issues:**
- Missing system libraries → Add to apt-get install
- Permission errors → Use sudo

**Common macOS Issues:**
- OpenSSL not found → `brew install openssl@3`
- pkg-config missing → `brew install pkg-config`

**Common Windows Issues:**
- Long path names → Keep paths under 260 chars
- Line endings → Use `.gitattributes` file:
  ```
  * text=auto eol=lf
  *.bat text eol=crlf
  ```

### Making Your R Code Cross-Platform

Here are tips for your actual R/C++ code:

```r
# BAD: Windows-specific path
path <- "C:\\Users\\name\\file.txt"

# GOOD: Cross-platform path
path <- file.path("~", "file.txt")

# BAD: System-specific command
system("ls -la")

# GOOD: R's cross-platform functions
list.files(all.files = TRUE)
```

For C++ code:
```cpp
// BAD: Platform-specific
#include <windows.h>

// GOOD: Use preprocessor directives
#ifdef _WIN32
  #include <windows.h>
#else
  #include <unistd.h>
#endif
```

## Step 3: Code Coverage {#step-3-coverage}

Let's add code coverage to see how well your tests cover your code!

### What is Code Coverage?

Code coverage shows what percentage of your code is actually tested. For example:
- ✅ 90% coverage = Great! Most code is tested
- ⚠️ 60% coverage = OK, but some code isn't tested
- ❌ 30% coverage = Uh oh, lots of untested code!

### Setting Up Coverage Workflow

Create a new file `.github/workflows/test-coverage.yaml`:

```yaml
name: test-coverage

on:
  push:
    branches: [main, master]
  pull_request:
    branches: [main, master]

jobs:
  test-coverage:
    runs-on: ubuntu-latest
    env:
      GITHUB_PAT: ${{ secrets.GITHUB_TOKEN }}
    
    steps:
      - uses: actions/checkout@v4
      
      - uses: r-lib/actions/setup-r@v2
        with:
          use-public-rspm: true
      
      - uses: r-lib/actions/setup-r-dependencies@v2
        with:
          extra-packages: any::covr
          needs: coverage
      
      - name: Test coverage
        run: |
          covr::codecov(
            quiet = FALSE,
            clean = FALSE,
            install_path = file.path(normalizePath(Sys.getenv("RUNNER_TEMP"), winslash = "/"), "package")
          )
        shell: Rscript {0}
      
      - name: Show coverage report
        run: |
          library(covr)
          cov <- package_coverage()
          print(cov)
          report(cov)
        shell: Rscript {0}
      
      - name: Upload coverage reports
        uses: codecov/codecov-action@v4
        with:
          token: ${{ secrets.CODECOV_TOKEN }}
          fail_ci_if_error: false
          verbose: true
```

### Adding the Coverage Badge

After your first coverage run:

1. Go to [codecov.io](https://codecov.io)
2. Find your repository
3. Click "Settings" → "Badge"
4. Copy the Markdown code
5. Add it to your README.md:

```markdown
# My Package

[![codecov](https://codecov.io/gh/yourusername/yourpackage/branch/main/graph/badge.svg)](https://codecov.io/gh/yourusername/yourpackage)

Your package description...
```

### Understanding Coverage Reports

When you click on your Codecov badge, you'll see:

- **Sunburst graph**: Visual representation of coverage
- **File browser**: Shows coverage per file
- **Line coverage**: Shows which lines are tested (green) or not (red)

### Improving Your Coverage

If your coverage is low, here's how to improve it:

1. **Find untested code** in Codecov:
   - Red lines = not tested
   - Yellow lines = partially tested
   - Green lines = fully tested

2. **Write tests** for red lines:
   ```r
   # If you have an untested function:
   my_function <- function(x) {
     if (x > 0) {
       return(x * 2)
     } else {
       return(0)
     }
   }
   
   # Write tests to cover all branches:
   test_that("my_function works", {
     expect_equal(my_function(5), 10)    # Test positive
     expect_equal(my_function(-5), 0)    # Test negative
     expect_equal(my_function(0), 0)     # Test edge case
   })
   ```

3. **Skip untestable code**:
   ```r
   my_function <- function() {
     # nocov start
     if (interactive()) {
       # This only runs interactively, hard to test
       plot(1:10)
     }
     # nocov end
     
     return(42)  # This will be tested
   }

```

## Step 4: Security Scanning {#step-4-security}

Let's make sure your package doesn't have security vulnerabilities!

### Why Security Scanning?

Even if your code is perfect, you might:
- Use a library with known vulnerabilities
- Have memory leaks in C/C++ code
- Accidentally expose sensitive data

Security scanning catches these automatically!

### Basic Security Workflow

Create `.github/workflows/security.yaml`:

```yaml
name: Security Scan

on:
  push:
    branches: [main]
  pull_request:
    branches: [main]
  schedule:
    - cron: '0 0 * * 1'  # Weekly on Monday

jobs:
  security:
    runs-on: ubuntu-latest
    
    steps:
      - uses: actions/checkout@v4
      
      # For packages with C/C++ code
      - name: Initialize CodeQL
        uses: github/codeql-action/init@v3
        with:
          languages: cpp
          queries: security-and-quality
      
      - name: Build package
        run: |
          R CMD INSTALL .
      
      - name: Perform CodeQL Analysis
        uses: github/codeql-action/analyze@v3
      
      # Check R dependencies for vulnerabilities
      - name: Check R package dependencies
        run: |
          Rscript -e "
          if (!require('oysteR')) install.packages('oysteR')
          library(oysteR)
          
          # Audit installed packages
          audit_results <- audit_installed_r_pkgs()
          
          if (nrow(audit_results) > 0) {
            print(audit_results)
            stop('Vulnerabilities found in dependencies!')
          } else {
            message('No known vulnerabilities found!')
          }
          "
```

### Understanding Security Results

**CodeQL Results** (for C/C++ code):
- Go to Security tab → Code scanning alerts
- Each alert shows:
  - Severity (High/Medium/Low)
  - Location in code
  - How to fix it

**Common C++ Security Issues:**

1. **Buffer overflow:**
   ```cpp
   // BAD:
   char buffer[10];
   strcpy(buffer, user_input);  // Could overflow!
   
   // GOOD:
   char buffer[10];
   strncpy(buffer, user_input, sizeof(buffer)-1);
   buffer[sizeof(buffer)-1] = '\0';
   ```

2. **Integer overflow:**
   ```cpp
   // BAD:
   int size = user_value * sizeof(double);  // Could overflow!
   
   // GOOD:
   size_t size;
   if (__builtin_mul_overflow(user_value, sizeof(double), &size)) {
     throw std::overflow_error("Size calculation overflow");
   }
   ```

### Memory Leak Detection

For packages with C++ code, add memory checking:

```yaml
- name: Run with valgrind
  run: |
    sudo apt-get install -y valgrind
    R -d "valgrind --leak-check=full" -e "library(yourpackage); your_function()"
```

### Making Your Package More Secure

1. **Never commit secrets:**
   ```r
   # BAD:
   api_key <- "sk-1234567890abcdef"
   
   # GOOD:
   api_key <- Sys.getenv("MY_API_KEY")
   ```

2. **Validate inputs:**
   ```r
   my_function <- function(x) {
     # Always validate
     if (!is.numeric(x)) {
       stop("x must be numeric")
     }
     if (x < 0 || x > 100) {
       stop("x must be between 0 and 100")
     }
     # Now safe to use x
   }
   ```

3. **Use secure random numbers:**
   ```cpp
   // BAD: Predictable
   srand(time(NULL));
   int random = rand();
   
   // GOOD: Cryptographically secure
   #include <sodium.h>
   uint32_t random;
   randombytes_buf(&random, sizeof(random));
   ```

## Step 5: Performance Monitoring {#step-5-performance}

Let's make sure your package stays fast!

### Setting Up Performance Benchmarks

First, create a benchmark script in `tests/performance/benchmark.R`:
```r
# tests/performance/benchmark.R
library(microbenchmark)
library(ggplot2)

# Benchmark your main functions
results <- microbenchmark(
  small = your_function(n = 100),
  medium = your_function(n = 1000),
  large = your_function(n = 10000),
  times = 100
)

# Save results
saveRDS(results, "benchmark_results.rds")

# Create plot
p <- autoplot(results) + 
  ggtitle("Performance Benchmark Results")
ggsave("benchmark_plot.png", p)

# Print summary
print(summary(results))
```

Now create `.github/workflows/benchmarks.yaml`:

```yaml
name: Performance Benchmarks

on:
  pull_request:
    branches: [main]
  push:
    branches: [main]

jobs:
  benchmark:
    runs-on: ubuntu-latest
    
    steps:
      - uses: actions/checkout@v4
        with:
          fetch-depth: 0  # Need history for comparison
      
      - uses: r-lib/actions/setup-r@v2
      
      - uses: r-lib/actions/setup-r-dependencies@v2
        with:
          extra-packages: |
            any::microbenchmark
            any::ggplot2
      
      - name: Run benchmarks
        run: |
          # Create directory
          mkdir -p tests/performance
          
          # Run current benchmarks
          Rscript tests/performance/benchmark.R
          
          # If PR, compare with base branch
          if [ "${{ github.event_name }}" == "pull_request" ]; then
            # Checkout base branch
            git checkout ${{ github.event.pull_request.base.ref }}
            
            # Run baseline benchmarks
            Rscript tests/performance/benchmark.R
            mv benchmark_results.rds baseline_results.rds
            
            # Back to PR branch
            git checkout -
            
            # Compare results
            Rscript -e "
            current <- readRDS('benchmark_results.rds')
            baseline <- readRDS('baseline_results.rds')
            
            # Calculate percentage change
            current_median <- median(current\$time)
            baseline_median <- median(baseline\$time)
            change <- ((current_median - baseline_median) / baseline_median) * 100
            
            cat('Performance change:', round(change, 2), '%\n')
            
            if (change > 10) {
              stop('Performance regression detected!')
            }
            "
          fi
      
      - name: Upload benchmark results
        uses: actions/upload-artifact@v4
        with:
          name: benchmark-results
          path: |
            benchmark_results.rds
            benchmark_plot.png
```

### Understanding Performance Results

The benchmark workflow:
1. Runs your benchmarks
2. If it's a PR, compares with the main branch
3. Fails if performance drops >10%
4. Uploads plots so you can see results

### Writing Good Benchmarks

1. **Test different input sizes:**
   ```r
   microbenchmark(
     small = my_function(10),
     medium = my_function(1000),
     large = my_function(100000),
     times = 100
   )
   ```

2. **Compare implementations:**
   ```r
   microbenchmark(
     method1 = old_way(data),
     method2 = new_way(data),
     method3 = optimized_way(data),
     times = 100
   )
   ```

3. **Memory benchmarks:**
   ```r
   library(bench)
   bench::mark(
     small = my_function(100),
     large = my_function(10000),
     check = FALSE,
     memory = TRUE
   )
   ```

### Optimizing Performance

1. **Profile first:**
   ```r
   # Find slow parts
   library(profvis)
   profvis({
     result <- my_slow_function(large_data)
   })
   ```

2. **Common R optimizations:**
   ```r
   # BAD: Growing vectors
   result <- c()
   for (i in 1:10000) {
     result <- c(result, i^2)
   }
   
   # GOOD: Pre-allocate
   result <- numeric(10000)
   for (i in 1:10000) {
     result[i] <- i^2
   }
   
   # BETTER: Vectorize
   result <- (1:10000)^2
   ```

3. **Use fast alternatives:**
   ```r
   # Slow: data.frame
   df <- data.frame(x = 1:1e6, y = rnorm(1e6))
   
   # Fast: data.table
   library(data.table)
   dt <- data.table(x = 1:1e6, y = rnorm(1e6))
   ```

## Step 6: Documentation Automation {#step-6-documentation}

Let's create a beautiful documentation website that updates automatically!

### Setting Up pkgdown

First, configure your package for pkgdown. Create `_pkgdown.yml`:

```yaml
url: https://yourusername.github.io/yourpackage/

template:
  bootstrap: 5
  bslib:
    primary: "#0054AD"
    border-radius: 0.5rem
    btn-border-radius: 0.25rem

navbar:
  title: "YourPackage"
  left:
    - text: "Home"
      href: index.html
    - text: "Reference"
      href: reference/index.html
    - text: "Articles"
      href: articles/index.html
    - text: "News"
      href: news/index.html
  right:
    - icon: fab fa-github
      href: https://github.com/yourusername/yourpackage

home:
  links:
    - text: Report a bug
      href: https://github.com/yourusername/yourpackage/issues

reference:
  - title: "Main Functions"
    desc: "Core functionality"
    contents:
    - has_concept("main")
  
  - title: "Helper Functions"
    desc: "Utility functions"
    contents:
    - has_concept("helpers")
```

Now create `.github/workflows/pkgdown.yaml`:

```yaml
name: pkgdown

on:
  push:
    branches: [main, master]
  release:
    types: [published]
  workflow_dispatch:

permissions:
  contents: write

jobs:
  pkgdown:
    runs-on: ubuntu-latest
    env:
      GITHUB_PAT: ${{ secrets.GITHUB_TOKEN }}
    
    steps:
      - uses: actions/checkout@v4
      
      - uses: r-lib/actions/setup-pandoc@v2
      
      - uses: r-lib/actions/setup-r@v2
        with:
          use-public-rspm: true
      
      - uses: r-lib/actions/setup-r-dependencies@v2
        with:
          extra-packages: any::pkgdown, local::.
          needs: website
      
      - name: Build site
        run: pkgdown::build_site_github_pages(new_process = FALSE, install = FALSE)
        shell: Rscript {0}
      
      - name: Deploy to GitHub pages 🚀
        if: github.event_name != 'pull_request'
        uses: JamesIves/github-pages-deploy-action@v4
        with:
          clean: false
          branch: gh-pages
          folder: docs
```

### Your First Documentation Site

After pushing this workflow:

1. Wait for it to complete (check Actions tab)
2. Go to Settings → Pages
3. You should see "Your site is live at https://yourusername.github.io/yourpackage/"
4. Click the link to see your site!

### Customizing Your Site

1. **Add a logo** - Place it at `man/figures/logo.png`

2. **Write articles** (vignettes):
   ```r
   usethis::use_vignette("getting-started")
   ```
   This creates `vignettes/getting-started.Rmd`

3. **Add badges** to README.md:
   ```markdown
   <!-- badges: start -->
   [![R-CMD-check](https://github.com/user/pkg/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/user/pkg/actions/workflows/R-CMD-check.yaml)
   [![codecov](https://codecov.io/gh/user/pkg/branch/main/graph/badge.svg)](https://codecov.io/gh/user/pkg)
   <!-- badges: end -->
   ```

4. **Group functions** in documentation:
   ```r
   #' @title Main Functions
   #' @description Core functionality
   #' @name main-functions
   #' @family main
   NULL
   
   #' My Important Function
   #' @family main
   #' @export
   my_function <- function() { }
   ```

## Step 7: Release Automation {#step-7-releases}

Let's automate your releases so publishing is just one click!

### Semantic Versioning Explained

First, understand version numbers:
- **1.0.0** = Major.Minor.Patch
- **Major** (1.x.x): Breaking changes
- **Minor** (x.1.x): New features, backward compatible
- **Patch** (x.x.1): Bug fixes only

Examples:
- 1.0.0 → 1.0.1: Fixed a bug
- 1.0.1 → 1.1.0: Added new function
- 1.1.0 → 2.0.0: Changed function arguments (breaking!)

### Release Workflow

Create `.github/workflows/release.yaml`:

```yaml
name: Release

on:
  push:
    tags:
      - 'v*.*.*'  # Triggers on v1.0.0, v2.1.3, etc.

jobs:
  release:
    runs-on: ubuntu-latest
    permissions:
      contents: write
    
    steps:
      - uses: actions/checkout@v4
        with:
          fetch-depth: 0  # Get all history for changelog
      
      - uses: r-lib/actions/setup-r@v2
      
      - name: Build source package
        run: |
          R CMD build . --no-build-vignettes
          echo "PKG_FILE=$(ls -1t *.tar.gz | head -n 1)" >> $GITHUB_ENV
      
      - name: Generate changelog
        run: |
          # Get previous tag
          PREV_TAG=$(git describe --tags --abbrev=0 HEAD^ 2>/dev/null || echo "")
          
          # Generate changelog
          if [ -z "$PREV_TAG" ]; then
            echo "Initial release" > CHANGELOG.md
          else
            echo "## What's Changed" > CHANGELOG.md
            echo "" >> CHANGELOG.md
            git log ${PREV_TAG}..HEAD --pretty=format:"* %s (%h)" >> CHANGELOG.md
          fi
          
          cat CHANGELOG.md
      
      - name: Create Release
        uses: softprops/action-gh-release@v2
        with:
          files: |
            ${{ env.PKG_FILE }}
            CHANGELOG.md
          body_path: CHANGELOG.md
          draft: false
          prerelease: false
          generate_release_notes: true
  
  # Build binaries for each platform
  build-binaries:
    needs: release
    strategy:
      matrix:
        config:
          - {os: windows-latest, r: 'release'}
          - {os: macOS-latest, r: 'release'}
    
    runs-on: ${{ matrix.config.os }}
    
    steps:
      - uses: actions/checkout@v4
      
      - uses: r-lib/actions/setup-r@v2
        with:
          r-version: ${{ matrix.config.r }}
      
      - uses: r-lib/actions/setup-r-dependencies@v2
      
      - name: Build binary package
        shell: bash
        run: |
          R CMD INSTALL . --build
          if [ "$RUNNER_OS" == "Windows" ]; then
            echo "BINARY_FILE=$(ls -1t *.zip | head -n 1)" >> $GITHUB_ENV
          else
            echo "BINARY_FILE=$(ls -1t *.tgz | head -n 1)" >> $GITHUB_ENV
          fi
      
      - name: Upload to release
        uses: softprops/action-gh-release@v2
        with:
          tag_name: ${{ github.ref_name }}
          files: ${{ env.BINARY_FILE }}
```

### How to Make a Release

1. **Update version** in DESCRIPTION:
   ```r
   usethis::use_version("minor")  # 1.0.0 → 1.1.0
   ```

2. **Update NEWS.md**:
   ```markdown
   # packagename 1.1.0
   
   * Added awesome new feature (#123)
   * Fixed annoying bug (#456)
   * Improved performance of slow_function()
   ```

3. **Commit and tag**:
   ```bash
   git add -A
   git commit -m "Release version 1.1.0"
   git tag v1.1.0
   git push origin main --tags
   ```

4. **Watch the magic**:
   - Go to Actions tab
   - See "Release" workflow running
   - Check Releases page when done
   - Find source + Windows + Mac packages!

### Preparing for CRAN

Before submitting to CRAN:

1. **Run checks locally**:
   ```r
   # Full check
   devtools::check()
   
   # CRAN-specific checks
   rhub::check_for_cran()
   ```

2. **Check on multiple platforms**:
   ```r
   # Windows
   devtools::check_win_devel()
   
   # macOS
   rhub::check_on_macos()
   ```

3. **Submit to CRAN**:
   ```r
   devtools::release()
   ```

## Troubleshooting Guide {#troubleshooting}

Things will break! Here's how to fix common issues:

### "My workflow isn't running"

1. **Check workflow syntax**:
   - Go to Actions tab
   - Look for syntax errors
   - YAML is indent-sensitive!

2. **Check branch names**:
   ```yaml
   on:
     push:
       branches: [main, master]  # Is your branch listed?
   ```

3. **Check workflow permissions**:
   - Settings → Actions → General
   - Ensure "Allow all actions" is selected

### "Package not found" errors

**Symptom**: `there is no package called 'xyz'`

**Solutions**:
1. Add to DESCRIPTION:
   ```
   Imports:
     xyz
   ```

2. Or install in workflow:
   ```yaml
   - name: Install extra packages
     run: |
       install.packages("xyz")
     shell: Rscript {0}
   ```

### "System dependency missing"

**Symptom**: `configure: error: libxml2 not found`

**Solutions by OS**:

```yaml
# Linux
- name: Install system deps
  if: runner.os == 'Linux'
  run: |
    sudo apt-get update
    sudo apt-get install -y libxml2-dev

# macOS
- name: Install system deps
  if: runner.os == 'macOS'
  run: |
    brew install libxml2

# Windows (usually automatic, but if needed)
- name: Install system deps
  if: runner.os == 'Windows'
  run: |
    choco install libxml2
```

### "Tests pass locally but fail in CI"

Common causes:

1. **File paths**:
   ```r
   # BAD: Assumes working directory
   data <- read.csv("data.csv")
   
   # GOOD: Use package paths
   data <- read.csv(system.file("extdata", "data.csv", package = "mypackage"))
   ```

2. **Timezone issues**:
   ```r
   # Set timezone in tests
   withr::local_timezone("UTC")
   ```

3. **Locale issues**:
   ```r
   # Force consistent locale
   withr::local_locale(c(LC_TIME = "C"))
   ```

### "Codecov not updating"

1. Check token is set:
   - Settings → Secrets → `CODECOV_TOKEN`

2. Check Codecov status:
   - Visit status.codecov.io

3. Try re-uploading:
   ```yaml
   - uses: codecov/codecov-action@v4
     with:
       token: ${{ secrets.CODECOV_TOKEN }}
       fail_ci_if_error: true
       verbose: true
   ```

## Maintenance & Best Practices {#maintenance}

### Weekly Maintenance Checklist

☐ **Check CI status**:
- Visit Actions tab
- Any red X's? Click to investigate
- Check scheduled runs completed

☐ **Review dependencies**:
```r
# Check for updates
old.packages()

# Check for security issues
if (!require("oysteR")) install.packages("oysteR")
audit_installed_r_pkgs()
```

☐ **Monitor performance**:
- Check benchmark trends
- Look for gradual slowdowns

### Monthly Tasks

1. **Update GitHub Actions**:
   ```yaml
   # Old:
   - uses: actions/checkout@v3
   
   # New:
   - uses: actions/checkout@v4  # Check for new versions
   ```

2. **Review and clean artifacts**:
   - Settings → Actions → Artifacts
   - Delete old test results

3. **Update dependencies**:
   ```r
   # Update all packages
   update.packages(ask = FALSE)
   
   # Rebuild documentation
   devtools::document()
   
   # Run checks
   devtools::check()
   ```

### Best Practices Summary

1. **Commit messages**:
   ```bash
   # Good:
   git commit -m "Fix memory leak in parse_data()"
   git commit -m "Add benchmarks for new sorting algorithm"
   
   # Bad:
   git commit -m "Fixed stuff"
   git commit -m "Update"
   ```

2. **Branch protection**:
   - Settings → Branches
   - Add rule for `main`
   - ☑ Require status checks
   - ☑ Require up-to-date branches
   - ☑ Include administrators

3. **PR workflow**:
   ```markdown
   ## Pull Request
   
   ### What does this PR do?
   Fixes memory leak when processing large files
   
   ### Checklist
   - [ ] Tests pass
   - [ ] Documentation updated
   - [ ] NEWS.md updated
   - [ ] No performance regression
   ```

### Getting Help

1. **GitHub Actions documentation**:
   - https://docs.github.com/actions

2. **R-specific actions**:
   - https://github.com/r-lib/actions

3. **Community**:
   - R-package-devel mailing list
   - RStudio Community forum
   - Stack Overflow [r] tag

### Final Tips

💡 **Start small**: Don't implement everything at once. Start with basic checks, then add features.

💡 **Copy from others**: Look at popular R packages on GitHub and learn from their workflows.

💡 **Test locally first**: Use `act` to run GitHub Actions locally:
```bash
brew install act  # macOS
act -j R-CMD-check  # Run locally
```

💡 **Don't fear failures**: CI/CD is meant to catch problems. Red X's are learning opportunities!

💡 **Celebrate green checks**: When everything passes, you've built something solid! 🎉

---

## Conclusion

Congratulations! You now have a production-grade CI/CD pipeline that:

✅ Tests on all platforms automatically
✅ Tracks code coverage
✅ Scans for security issues
✅ Monitors performance
✅ Builds documentation
✅ Automates releases

Your package is now professional, reliable, and ready for the world! Remember, this setup will save you countless hours of debugging and give your users confidence in your work.

Happy coding! 🚀


