#!/usr/bin/env Rscript
# ----------------------------------------------------------------------
# Script: generate_mintlify_docs_fixed.R
# Purpose: Fixed Mintlify documentation generation with proper structure
# Author: qiprng documentation pipeline - FIXED VERSION
# ----------------------------------------------------------------------

library(tools)
library(jsonlite)
library(stringr)

# Configuration
if (interactive()) {
  PROJECT_ROOT <- getwd()
} else {
  args <- commandArgs(trailingOnly = FALSE)
  script_path <- sub("--file=", "", args[grep("--file=", args)])
  if (length(script_path) == 0) {
    PROJECT_ROOT <- getwd()
  } else {
    PROJECT_ROOT <- normalizePath(file.path(dirname(script_path), ".."))
  }
}

MINTLIFY_DIR <- file.path(PROJECT_ROOT, "mintlify")
MAN_DIR <- file.path(PROJECT_ROOT, "man")
SRC_DIR <- file.path(PROJECT_ROOT, "src")
R_DIR <- file.path(PROJECT_ROOT, "R")

# Ensure output directories exist
dir.create(MINTLIFY_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(MINTLIFY_DIR, "api-reference"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(MINTLIFY_DIR, "api-reference", "r-functions"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(MINTLIFY_DIR, "api-reference", "cpp-classes"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(MINTLIFY_DIR, "guides"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(MINTLIFY_DIR, "reference"), showWarnings = FALSE, recursive = TRUE)

# Source the existing roxygen2_to_mintlify.R functions
source(file.path(PROJECT_ROOT, "scripts", "roxygen2_to_mintlify.R"))

#' Generate proper mint.json with correct navigation structure
generate_proper_mint_json <- function(r_functions, cpp_classes) {
  # Select only the most important functions to avoid overwhelming navigation
  core_r_functions <- c(
    "createPRNG", "generatePRNG", "updatePRNG", "cleanupPRNG",
    "jumpAheadPRNG", "reseedPRNG", "test_qiprng", "benchmark_qiprng",
    "validate_qiprng_framework", "generate_excellent_random"
  )

  core_cpp_classes <- c(
    "QuadraticIrrational", "EnhancedPRNG", "CryptoMixer",
    "Matrix2x2", "BufferManager", "ThreadManager", "DistributionGenerator"
  )

  # Filter to only include existing functions
  r_functions_filtered <- intersect(core_r_functions, r_functions)
  cpp_classes_filtered <- intersect(core_cpp_classes, cpp_classes)

  # Create the mint.json structure WITHOUT contentDir
  mint_config <- list(
    `$schema` = "https://mintlify.com/schema.json",
    name = "qiprng",
    logo = list(
      dark = "/logo/dark.svg",
      light = "/logo/light.svg"
    ),
    favicon = "/favicon.svg",
    colors = list(
      primary = "#0D9373",
      light = "#07C983",
      dark = "#0D9373",
      anchors = list(
        from = "#0D9373",
        to = "#07C983"
      )
    ),
    topbarLinks = list(
      list(
        name = "Support",
        url = "mailto:sergey.kornilov@biostochastics.com"
      )
    ),
    topbarCtaButton = list(
      name = "GitHub",
      url = "https://github.com/biostochastics/qiprng"
    ),
    anchors = list(
      list(
        name = "Documentation",
        icon = "book-open-cover",
        url = "/"
      ),
      list(
        name = "GitHub",
        icon = "github",
        url = "https://github.com/biostochastics/qiprng"
      ),
      list(
        name = "CRAN",
        icon = "r",
        url = "https://cran.r-project.org/package=qiprng"
      )
    ),
    # FIXED: Proper navigation structure - groups at top level
    navigation = list(
      list(
        group = "Get Started",
        pages = list(
          "mintlify/introduction",
          "mintlify/quickstart",
          "mintlify/installation"
        )
      ),
      list(
        group = "Guides",
        pages = list(
          "mintlify/guides/basic-usage",
          "mintlify/guides/distributions",
          "mintlify/guides/parallel-generation",
          "mintlify/guides/performance-tuning"
        )
      ),
      list(
        group = "API Reference",
        pages = c(
          list("mintlify/api-reference/introduction"),
          # Add individual function pages
          if (length(r_functions_filtered) > 0) {
            paste0("mintlify/api-reference/r-functions/", r_functions_filtered)
          } else {
            character(0)
          }
        )
      ),
      # Add C++ classes as separate group if they exist
      if (length(cpp_classes_filtered) > 0) {
        list(
          group = "C++ Implementation",
          pages = paste0("mintlify/api-reference/cpp-classes/", cpp_classes_filtered)
        )
      },
      list(
        group = "Testing & Validation",
        pages = list(
          "mintlify/api-reference/r-functions/test_qiprng",
          "mintlify/api-reference/r-functions/benchmark_qiprng",
          "mintlify/api-reference/r-functions/validate_qiprng_framework"
        )
      ),
      list(
        group = "Reference",
        pages = list(
          "mintlify/reference/changelog",
          "mintlify/reference/news",
          "mintlify/reference/benchmarks",
          "mintlify/reference/security"
        )
      )
    ),
    footerSocials = list(
      github = "https://github.com/biostochastics/qiprng",
      linkedin = "https://www.linkedin.com/company/biostochastics"
    ),
    feedback = list(
      thumbsRating = TRUE,
      suggestEdit = TRUE,
      raiseIssue = TRUE
    ),
    search = list(
      prompt = "Search qiprng documentation..."
    ),
    metadata = list(
      `og:title` = "qiprng Documentation",
      `og:description` = "High-quality pseudo-random number generator based on quadratic irrationals",
      `og:image` = "https://qiprng.biostochastics.com/images/og-image.png",
      `twitter:card` = "summary_large_image"
    )
  )

  # Remove empty groups
  mint_config$navigation <- Filter(function(x) {
    !is.null(x) && length(x$pages) > 0
  }, mint_config$navigation)

  # Write mint.json to PROJECT ROOT (not mintlify directory)
  json_content <- toJSON(mint_config, pretty = TRUE, auto_unbox = TRUE)
  writeLines(json_content, file.path(PROJECT_ROOT, "mint.json"))
  cat("✓ Generated fixed mint.json in project root\n")
  return(TRUE)
}

#' Convert README to introduction with proper path handling
convert_readme_to_introduction <- function() {
  readme_file <- file.path(PROJECT_ROOT, "README.md")
  if (!file.exists(readme_file)) {
    readme_file <- file.path(PROJECT_ROOT, "README.Md")
  }

  if (file.exists(readme_file)) {
    readme_content <- readLines(readme_file)

    # Create frontmatter
    frontmatter <- c(
      "---",
      "title: \"Introduction\"",
      "description: \"Quadratic Irrational Pseudo-Random Number Generator for R\"",
      "sidebarTitle: \"qiprng\"",
      "---"
    )

    # Process content - remove first H1 if it exists
    if (length(readme_content) > 0 && grepl("^# ", readme_content[1])) {
      readme_content <- readme_content[-1]
    }

    # Replace GitHub relative links with Mintlify links
    readme_content <- gsub("\\]\\(LICENSE\\)", "](https://github.com/biostochastics/qiprng/blob/main/LICENSE)", readme_content)
    readme_content <- gsub("\\]\\(CHANGELOG\\.md\\)", "](/mintlify/reference/changelog)", readme_content)
    readme_content <- gsub("\\]\\(NEWS\\.md\\)", "](/mintlify/reference/news)", readme_content)

    # Write introduction.mdx
    writeLines(
      c(frontmatter, "", readme_content),
      file.path(MINTLIFY_DIR, "introduction.mdx")
    )
    cat("✓ Generated introduction.mdx from README\n")
    return(TRUE)
  }
  cat("✗ README.md not found\n")
  return(FALSE)
}

#' Process only core R functions for cleaner navigation
process_core_roxygen_docs <- function() {
  # Core functions to document
  core_functions <- c(
    "createPRNG", "generatePRNG", "updatePRNG", "cleanupPRNG",
    "jumpAheadPRNG", "reseedPRNG", "test_qiprng", "benchmark_qiprng",
    "validate_qiprng_framework", "generate_excellent_random",
    "test_uniformity", "test_independence", "test_autocorrelation"
  )

  rd_files <- list.files(MAN_DIR, pattern = "\\.Rd$", full.names = TRUE)
  r_docs_count <- 0
  documented_functions <- character()

  for (rd_file in rd_files) {
    doc <- parse_rd_file(rd_file)
    if (!is.null(doc$name) && doc$name %in% core_functions) {
      mdx_content <- rd_to_mdx(doc, category = "R Functions")
      output_file <- file.path(
        MINTLIFY_DIR, "api-reference", "r-functions",
        paste0(doc$name, ".mdx")
      )
      writeLines(mdx_content, output_file)
      documented_functions <- c(documented_functions, doc$name)
      r_docs_count <- r_docs_count + 1
    }
  }

  cat(sprintf("✓ Generated %d core R function documentation files\n", r_docs_count))
  return(documented_functions)
}

#' Extract and convert core C++ classes
extract_core_cpp_documentation <- function() {
  # Core classes to document
  target_classes <- c(
    "QuadraticIrrational", "EnhancedPRNG", "CryptoMixer",
    "Matrix2x2", "BufferManager", "ThreadManager", "DistributionGenerator"
  )

  cpp_headers <- list.files(SRC_DIR, pattern = "\\.hpp$", full.names = TRUE)
  cpp_docs_count <- 0
  processed_classes <- character()

  for (header_file in cpp_headers) {
    header_content <- readLines(header_file, warn = FALSE)

    # Look for class definitions
    class_pattern <- "^\\s*(class|struct)\\s+(\\w+)\\s*(?:[:{]|$)"

    for (i in seq_along(header_content)) {
      line <- header_content[i]

      if (grepl(class_pattern, line)) {
        class_match <- regmatches(line, regexec(class_pattern, line))[[1]]
        if (length(class_match) >= 3) {
          class_name <- class_match[3]

          if (class_name %in% target_classes && !class_name %in% processed_classes) {
            # Create minimal MDX file for the class
            mdx_content <- c(
              "---",
              sprintf("title: \"%s\"", class_name),
              sprintf("description: \"C++ %s class\"", class_name),
              sprintf("sidebarTitle: \"%s\"", class_name),
              "---",
              "",
              sprintf("# %s", class_name),
              "",
              "## Description",
              "",
              sprintf("The `%s` class is part of the qiprng C++ implementation.", class_name),
              "",
              "## Source",
              "",
              sprintf("Defined in `src/%s`", basename(header_file))
            )

            # Write MDX file
            output_file <- file.path(
              MINTLIFY_DIR, "api-reference", "cpp-classes",
              paste0(class_name, ".mdx")
            )
            writeLines(mdx_content, output_file)
            processed_classes <- c(processed_classes, class_name)
            cpp_docs_count <- cpp_docs_count + 1
          }
        }
      }
    }
  }

  cat(sprintf("✓ Generated %d core C++ class documentation files\n", cpp_docs_count))
  return(processed_classes)
}

#' Main documentation generation function
generate_fixed_docs <- function() {
  cat("================================================\n")
  cat("  Generating Fixed Mintlify Documentation\n")
  cat("================================================\n\n")

  # 1. Convert README to introduction
  convert_readme_to_introduction()

  # 2. Create quickstart guide
  create_quickstart()

  # 3. Create installation guide
  create_installation_guide()

  # 4. Convert CHANGELOG and NEWS
  convert_changelog()
  convert_news()

  # 5. Process core R functions only
  r_functions <- process_core_roxygen_docs()

  # 6. Extract core C++ classes only
  cpp_classes <- extract_core_cpp_documentation()

  # 7. Generate proper mint.json with fixed structure
  generate_proper_mint_json(r_functions, cpp_classes)

  # 8. Create essential guide files if missing
  guides <- c("basic-usage", "distributions", "parallel-generation", "performance-tuning")
  for (guide in guides) {
    guide_file <- file.path(MINTLIFY_DIR, "guides", paste0(guide, ".mdx"))
    if (!file.exists(guide_file)) {
      guide_title <- gsub("-", " ", guide)
      guide_title <- paste(toupper(substring(guide_title, 1, 1)),
        substring(guide_title, 2),
        sep = ""
      )
      writeLines(c(
        "---",
        sprintf("title: \"%s\"", guide_title),
        sprintf("description: \"Guide to %s in qiprng\"", tolower(guide_title)),
        "---",
        "",
        sprintf("# %s", guide_title),
        "",
        "Coming soon. Check the [API Reference](/mintlify/api-reference/introduction) for detailed function documentation."
      ), guide_file)
    }
  }

  # 9. Create reference pages if missing
  ref_pages <- c("benchmarks", "security")
  for (page in ref_pages) {
    page_file <- file.path(MINTLIFY_DIR, "reference", paste0(page, ".mdx"))
    if (!file.exists(page_file)) {
      page_title <- paste(toupper(substring(page, 1, 1)), substring(page, 2), sep = "")
      writeLines(c(
        "---",
        sprintf("title: \"%s\"", page_title),
        sprintf("description: \"%s information for qiprng\"", page_title),
        "---",
        "",
        sprintf("# %s", page_title),
        "",
        "Documentation coming soon."
      ), page_file)
    }
  }

  # 10. Create API reference introduction
  api_intro <- c(
    "---",
    "title: \"API Reference\"",
    "description: \"Complete API documentation for qiprng\"",
    "---",
    "",
    "# API Reference",
    "",
    "## Core Functions",
    "",
    "### Generator Management",
    "- [`createPRNG()`](/mintlify/api-reference/r-functions/createPRNG) - Initialize generator",
    "- [`generatePRNG()`](/mintlify/api-reference/r-functions/generatePRNG) - Generate random values",
    "- [`updatePRNG()`](/mintlify/api-reference/r-functions/updatePRNG) - Update configuration",
    "- [`cleanupPRNG()`](/mintlify/api-reference/r-functions/cleanupPRNG) - Clean up resources",
    "",
    "### Advanced Features",
    "- [`jumpAheadPRNG()`](/mintlify/api-reference/r-functions/jumpAheadPRNG) - Jump ahead in sequence",
    "- [`reseedPRNG()`](/mintlify/api-reference/r-functions/reseedPRNG) - Reseed generator",
    "",
    "### Testing & Validation",
    "- [`test_qiprng()`](/mintlify/api-reference/r-functions/test_qiprng) - Run comprehensive tests",
    "- [`benchmark_qiprng()`](/mintlify/api-reference/r-functions/benchmark_qiprng) - Performance benchmarking",
    "",
    "## C++ Implementation",
    "",
    "Core classes implementing the quadratic irrational algorithm:",
    "- [`QuadraticIrrational`](/mintlify/api-reference/cpp-classes/QuadraticIrrational)",
    "- [`EnhancedPRNG`](/mintlify/api-reference/cpp-classes/EnhancedPRNG)",
    "- [`CryptoMixer`](/mintlify/api-reference/cpp-classes/CryptoMixer)"
  )
  writeLines(api_intro, file.path(MINTLIFY_DIR, "api-reference", "introduction.mdx"))

  # Summary
  cat("\n================================================\n")
  cat("           Fixed Documentation Complete\n")
  cat("================================================\n")
  cat(sprintf("✓ Generated %d core R function files\n", length(r_functions)))
  cat(sprintf("✓ Generated %d core C++ class files\n", length(cpp_classes)))
  cat("✓ Created proper mint.json with fixed navigation\n")
  cat("✓ All files use mintlify/ prefix for correct paths\n")
  cat("\nRun the deployment test:\n")
  cat("  bash scripts/test_mintlify_deployment.sh\n")
  cat("\nThen test locally with:\n")
  cat("  mintlify dev\n")
}

# Helper functions (include the ones from original that are still needed)
create_quickstart <- function() {
  quickstart_content <- c(
    "---",
    "title: \"Quick Start\"",
    "description: \"Get up and running with qiprng in 5 minutes\"",
    "---",
    "",
    "## Installation",
    "",
    "```r",
    "# Install from GitHub",
    "if (!require(\"remotes\")) {",
    "  install.packages(\"remotes\")",
    "}",
    "remotes::install_github(\"biostochastics/qiprng\")",
    "```",
    "",
    "## Basic Usage",
    "",
    "```r",
    "library(qiprng)",
    "",
    "# Create a PRNG with default settings",
    "createPRNG()",
    "",
    "# Generate uniform random numbers",
    "random_values <- generatePRNG(1000)",
    "",
    "# Clean up when done",
    "cleanupPRNG()",
    "```",
    "",
    "## Next Steps",
    "",
    "- [Installation Guide](/mintlify/installation) - Platform-specific setup",
    "- [API Reference](/mintlify/api-reference/introduction) - Complete function documentation",
    "- [Basic Usage Guide](/mintlify/guides/basic-usage) - Detailed examples"
  )

  writeLines(quickstart_content, file.path(MINTLIFY_DIR, "quickstart.mdx"))
  cat("✓ Generated quickstart.mdx\n")
  return(TRUE)
}

create_installation_guide <- function() {
  install_content <- c(
    "---",
    "title: \"Installation\"",
    "description: \"How to install and set up qiprng\"",
    "---",
    "",
    "# Installation",
    "",
    "## From GitHub",
    "",
    "```r",
    "# Using remotes",
    "remotes::install_github(\"biostochastics/qiprng\")",
    "",
    "# Using devtools",
    "devtools::install_github(\"biostochastics/qiprng\")",
    "```",
    "",
    "## System Requirements",
    "",
    "- **R** ≥ 4.0.0",
    "- **C++ Compiler** with C++17 support",
    "- **MPFR Library** for high-precision arithmetic",
    "",
    "### macOS",
    "```bash",
    "brew install mpfr gmp",
    "```",
    "",
    "### Linux",
    "```bash",
    "# Ubuntu/Debian",
    "sudo apt-get install libmpfr-dev libgmp-dev",
    "",
    "# Fedora/RHEL",
    "sudo dnf install mpfr-devel gmp-devel",
    "```",
    "",
    "### Windows",
    "Install [Rtools](https://cran.r-project.org/bin/windows/Rtools/) which includes MPFR.",
    "",
    "## Verify Installation",
    "",
    "```r",
    "library(qiprng)",
    "createPRNG()",
    "values <- generatePRNG(10)",
    "print(values)",
    "cleanupPRNG()",
    "```"
  )

  writeLines(install_content, file.path(MINTLIFY_DIR, "installation.mdx"))
  cat("✓ Generated installation.mdx\n")
  return(TRUE)
}

convert_changelog <- function() {
  changelog_file <- file.path(PROJECT_ROOT, "CHANGELOG.md")

  if (file.exists(changelog_file)) {
    changelog_content <- readLines(changelog_file)

    frontmatter <- c(
      "---",
      "title: \"Changelog\"",
      "description: \"Version history and release notes for qiprng\"",
      "---"
    )

    writeLines(
      c(frontmatter, "", changelog_content),
      file.path(MINTLIFY_DIR, "reference", "changelog.mdx")
    )
    cat("✓ Generated reference/changelog.mdx\n")
    return(TRUE)
  }
  cat("✗ CHANGELOG.md not found\n")
  return(FALSE)
}

convert_news <- function() {
  news_file <- file.path(PROJECT_ROOT, "NEWS.md")

  if (file.exists(news_file)) {
    news_content <- readLines(news_file)

    frontmatter <- c(
      "---",
      "title: \"News\"",
      "description: \"Latest updates and announcements for qiprng\"",
      "---"
    )

    writeLines(
      c(frontmatter, "", news_content),
      file.path(MINTLIFY_DIR, "reference", "news.mdx")
    )
    cat("✓ Generated reference/news.mdx\n")
    return(TRUE)
  }
  cat("✗ NEWS.md not found\n")
  return(FALSE)
}

# Run the fixed generation
generate_fixed_docs()
