#!/usr/bin/env Rscript
# ----------------------------------------------------------------------
# Script: generate_mintlify_docs.R
# Purpose: Comprehensive Mintlify documentation generation from all sources
# Author: qiprng documentation pipeline
# ----------------------------------------------------------------------

library(tools)
library(jsonlite)
library(stringr)

# Configuration
# Get the script directory and project root
if (interactive()) {
  PROJECT_ROOT <- getwd()
} else {
  # When run as script
  args <- commandArgs(trailingOnly = FALSE)
  script_path <- sub("--file=", "", args[grep("--file=", args)])
  if (length(script_path) == 0) {
    # Fallback to working directory
    PROJECT_ROOT <- getwd()
  } else {
    PROJECT_ROOT <- normalizePath(file.path(dirname(script_path), ".."))
  }
}

MINTLIFY_DIR <- file.path(PROJECT_ROOT, "mintlify")
MAN_DIR <- file.path(PROJECT_ROOT, "man")
SRC_DIR <- file.path(PROJECT_ROOT, "src")
R_DIR <- file.path(PROJECT_ROOT, "R")

# Ensure output directory exists
dir.create(MINTLIFY_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(MINTLIFY_DIR, "api-reference"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(MINTLIFY_DIR, "api-reference", "r-functions"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(MINTLIFY_DIR, "api-reference", "cpp-classes"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(MINTLIFY_DIR, "guides"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(MINTLIFY_DIR, "concepts"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(MINTLIFY_DIR, "reference"), showWarnings = FALSE, recursive = TRUE)

# Source the existing roxygen2_to_mintlify.R functions
source(file.path(PROJECT_ROOT, "scripts", "roxygen2_to_mintlify.R"))

#' Convert README.md to introduction.mdx
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
      "---"
    )

    # Process content - remove first H1 if it exists
    if (length(readme_content) > 0 && grepl("^# ", readme_content[1])) {
      readme_content <- readme_content[-1]
    }

    # Replace GitHub relative links with absolute or Mintlify links
    readme_content <- gsub("\\]\\(LICENSE\\)", "](https://github.com/biostochastics/qiprng/blob/main/LICENSE)", readme_content)
    readme_content <- gsub("\\]\\(CHANGELOG\\.md\\)", "](/reference/changelog)", readme_content)
    readme_content <- gsub("\\]\\(NEWS\\.md\\)", "](/reference/news)", readme_content)
    readme_content <- gsub("\\]\\(MATH\\.md\\)", "](/concepts/mathematical-theory)", readme_content)

    # Add H1 title
    readme_content <- c(
      "# qiprng: Quadratic Irrational Pseudo-Random Number Generator for R",
      "",
      readme_content
    )

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

#' Convert CHANGELOG.md to reference/changelog.mdx
convert_changelog <- function() {
  changelog_file <- file.path(PROJECT_ROOT, "CHANGELOG.md")

  if (file.exists(changelog_file)) {
    changelog_content <- readLines(changelog_file)

    # Create frontmatter
    frontmatter <- c(
      "---",
      "title: \"Changelog\"",
      "description: \"Version history and release notes for qiprng\"",
      "sidebarTitle: \"Changelog\"",
      "---"
    )

    # Write changelog.mdx
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

#' Convert NEWS.md to reference/news.mdx
convert_news <- function() {
  news_file <- file.path(PROJECT_ROOT, "NEWS.md")

  if (file.exists(news_file)) {
    news_content <- readLines(news_file)

    # Create frontmatter
    frontmatter <- c(
      "---",
      "title: \"News\"",
      "description: \"Latest updates and announcements for qiprng\"",
      "sidebarTitle: \"News\"",
      "---"
    )

    # Write news.mdx
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

#' Extract and convert C++ documentation from header files
extract_cpp_documentation <- function() {
  cpp_headers <- list.files(SRC_DIR, pattern = "\\.hpp$", full.names = TRUE)
  cpp_docs_count <- 0
  processed_classes <- character() # Track processed classes to avoid duplicates

  # Key classes to document
  target_classes <- c(
    "QuadraticIrrational", "EnhancedPRNG", "CryptoMixer",
    "Matrix2x2", "MultiQI", "BufferManager", "ThreadManager",
    "DistributionGenerator", "MPFRWrapper"
  )

  for (header_file in cpp_headers) {
    header_name <- tools::file_path_sans_ext(basename(header_file))
    header_content <- readLines(header_file, warn = FALSE)

    # Look for class/struct definitions and extract documentation
    # Updated pattern to match class or struct, and stop at inheritance or opening brace
    class_pattern <- "^\\s*(class|struct)\\s+(\\w+)\\s*(?:[:{]|$)"
    doc_pattern <- "^\\s*///?\\s*(.*)$|^\\s*/\\*\\*(.*)\\*/$"

    i <- 1
    while (i <= length(header_content)) {
      line <- header_content[i]

      # Check for class/struct definition directly (may not have preceding docs)
      if (grepl(class_pattern, line)) {
        # Extract class name, handling inheritance and templates
        class_match <- regmatches(line, regexec(class_pattern, line))[[1]]
        if (length(class_match) >= 3) {
          class_name <- class_match[3]

          # Skip if already processed or not a clean identifier
          if (class_name %in% processed_classes ||
            grepl("[^a-zA-Z0-9_]", class_name) ||
            nchar(class_name) == 0) {
            i <- i + 1
            next
          }

          # Only process target classes or if no targets specified
          if (length(target_classes) > 0 && !class_name %in% target_classes) {
            i <- i + 1
            next
          }

          # Look for documentation above the class
          doc_lines <- character()
          if (i > 1) {
            doc_start <- i - 1
            while (doc_start > 0 && grepl(doc_pattern, header_content[doc_start])) {
              doc_line <- gsub("^\\s*///?\\s*|^\\s*/\\*\\*?\\s*|\\s*\\*/$", "", header_content[doc_start])
              doc_line <- gsub("^\\*\\s*", "", doc_line)
              if (nchar(doc_line) > 0) {
                doc_lines <- c(doc_line, doc_lines) # Prepend to maintain order
              }
              doc_start <- doc_start - 1
            }
          }

          # Create MDX file for the class
          mdx_content <- c(
            "---",
            sprintf("title: \"%s\"", class_name),
            sprintf(
              "description: \"%s\"",
              if (length(doc_lines) > 0) doc_lines[1] else sprintf("C++ class %s", class_name)
            ),
            sprintf("sidebarTitle: \"%s\"", class_name),
            "---",
            "",
            sprintf("# %s", class_name),
            ""
          )

          if (length(doc_lines) > 0) {
            mdx_content <- c(mdx_content, "## Description", "", doc_lines, "")
          }

          # Add source file reference
          mdx_content <- c(
            mdx_content,
            "## Source",
            "",
            sprintf("Defined in `src/%s`", basename(header_file)),
            ""
          )

          # Extract methods and members
          class_end <- i + 1
          brace_count <- 0
          in_class <- FALSE

          while (class_end <= length(header_content)) {
            if (grepl("\\{", header_content[class_end])) {
              brace_count <- brace_count + str_count(header_content[class_end], "\\{")
              in_class <- TRUE
            }
            if (grepl("\\}", header_content[class_end])) {
              brace_count <- brace_count - str_count(header_content[class_end], "\\}")
              if (brace_count == 0 && in_class) {
                break
              }
            }
            class_end <- class_end + 1
          }

          # Extract public methods
          public_methods <- character()
          for (j in i:min(class_end, length(header_content))) {
            if (grepl("^\\s*public:", header_content[j])) {
              j <- j + 1
              while (j <= min(class_end, length(header_content)) &&
                !grepl("^\\s*(private|protected):", header_content[j])) {
                method_line <- trimws(header_content[j])
                if (nchar(method_line) > 0 && !grepl("^//", method_line) &&
                  grepl("\\(.*\\)", method_line)) {
                  public_methods <- c(public_methods, method_line)
                }
                j <- j + 1
              }
            }
          }

          if (length(public_methods) > 0) {
            mdx_content <- c(mdx_content, "## Public Methods", "", "```cpp")
            mdx_content <- c(mdx_content, public_methods)
            mdx_content <- c(mdx_content, "```", "")
          }

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
      i <- i + 1
    }
  }

  cat(sprintf("✓ Generated %d C++ class documentation files\n", cpp_docs_count))
  return(cpp_docs_count)
}

#' Process all Roxygen2 documentation
process_roxygen_docs <- function() {
  rd_files <- list.files(MAN_DIR, pattern = "\\.Rd$", full.names = TRUE)
  r_docs_count <- 0

  for (rd_file in rd_files) {
    doc <- parse_rd_file(rd_file)
    if (!is.null(doc$name)) {
      mdx_content <- rd_to_mdx(doc, category = "R Functions")
      output_file <- file.path(
        MINTLIFY_DIR, "api-reference", "r-functions",
        paste0(doc$name, ".mdx")
      )
      writeLines(mdx_content, output_file)
      r_docs_count <- r_docs_count + 1
    }
  }

  cat(sprintf("✓ Generated %d R function documentation files\n", r_docs_count))
  return(r_docs_count)
}

#' Create quickstart guide from examples
create_quickstart <- function() {
  quickstart_content <- c(
    "---",
    "title: 'Quick Start'",
    "description: 'Get up and running with qiprng in 5 minutes'",
    "---",
    "",
    "## Installation",
    "",
    "<Tabs>",
    "  <Tab title=\"From GitHub\">",
    "    ```r",
    "    # Install from GitHub (recommended)",
    "    if (!require(\"remotes\")) {",
    "      install.packages(\"remotes\")",
    "    }",
    "    remotes::install_github(\"biostochastics/qiprng\")",
    "    ```",
    "  </Tab>",
    "",
    "  <Tab title=\"From Source\">",
    "    ```bash",
    "    # Clone the repository",
    "    git clone https://github.com/biostochastics/qiprng.git",
    "    cd qiprng",
    "",
    "    # Install in R",
    "    R CMD INSTALL .",
    "    ```",
    "  </Tab>",
    "</Tabs>",
    "",
    "<Note>",
    "  Make sure you have the required system libraries installed. See the [Installation Guide](/installation) for platform-specific instructions.",
    "</Note>",
    "",
    "## Basic Usage",
    "",
    "### 1. Create and Use the Generator",
    "",
    "```r",
    "library(qiprng)",
    "",
    "# Create a PRNG with default settings",
    "createPRNG()",
    "",
    "# Generate 1000 uniform random numbers",
    "random_values <- generatePRNG(1000)",
    "",
    "# Check the distribution",
    "hist(random_values, breaks = 50,",
    "     main = \"qiprng Uniform Distribution\",",
    "     col = \"skyblue\")",
    "```",
    "",
    "### 2. Use Different Distributions",
    "",
    "<CodeGroup>",
    "```r Normal",
    "# Normal distribution",
    "createPRNG(list(",
    "  distribution = \"normal\",",
    "  normal_mean = 0,",
    "  normal_sd = 1",
    "))",
    "normal_samples <- generatePRNG(1000)",
    "```",
    "",
    "```r Exponential",
    "# Exponential distribution",
    "createPRNG(list(",
    "  distribution = \"exponential\",",
    "  exponential_lambda = 0.5",
    "))",
    "exp_samples <- generatePRNG(1000)",
    "```",
    "</CodeGroup>",
    "",
    "## Next Steps",
    "",
    "<CardGroup cols={2}>",
    "  <Card",
    "    title=\"Explore Distributions\"",
    "    icon=\"chart-mixed\"",
    "    href=\"/guides/distributions\"",
    "  >",
    "    Learn about all supported distributions",
    "  </Card>",
    "  <Card",
    "    title=\"API Reference\"",
    "    icon=\"book\"",
    "    href=\"/api-reference/r-functions/createPRNG\"",
    "  >",
    "    Dive deep into the API",
    "  </Card>",
    "</CardGroup>"
  )

  writeLines(quickstart_content, file.path(MINTLIFY_DIR, "quickstart.mdx"))
  cat("✓ Generated quickstart.mdx\n")
  return(TRUE)
}

#' Create installation guide
create_installation_guide <- function() {
  install_content <- c(
    "---",
    "title: 'Installation'",
    "description: 'How to install and set up qiprng'",
    "sidebarTitle: 'Installation'",
    "---",
    "",
    "# Installation",
    "",
    "The qiprng package can be installed from GitHub or CRAN (when available).",
    "",
    "## From GitHub (Development Version)",
    "",
    "<CodeGroup>",
    "```r R",
    "# Install devtools if not already installed",
    "if (!requireNamespace(\"devtools\", quietly = TRUE)) {",
    "  install.packages(\"devtools\")",
    "}",
    "",
    "# Install qiprng from GitHub",
    "devtools::install_github(\"biostochastics/qiprng\")",
    "```",
    "",
    "```bash Command Line",
    "# Clone and install locally",
    "git clone https://github.com/biostochastics/qiprng.git",
    "cd qiprng",
    "R CMD INSTALL .",
    "```",
    "</CodeGroup>",
    "",
    "## System Requirements",
    "",
    "### Required Dependencies",
    "",
    "- **R** ≥ 4.0.0",
    "- **C++ Compiler** with C++17 support",
    "- **MPFR Library** for high-precision arithmetic",
    "- **Rcpp** for R/C++ integration",
    "",
    "### Platform-Specific Installation",
    "",
    "<Accordion title=\"macOS\">",
    "Install dependencies using Homebrew:",
    "",
    "```bash",
    "brew install mpfr gmp",
    "brew install libomp  # For OpenMP support",
    "```",
    "",
    "Then install the R package:",
    "```r",
    "devtools::install_github(\"biostochastics/qiprng\")",
    "```",
    "</Accordion>",
    "",
    "<Accordion title=\"Linux\">",
    "Install system dependencies:",
    "",
    "```bash",
    "# Ubuntu/Debian",
    "sudo apt-get update",
    "sudo apt-get install libmpfr-dev libgmp-dev",
    "",
    "# Fedora/RHEL",
    "sudo dnf install mpfr-devel gmp-devel",
    "```",
    "",
    "Then install the R package:",
    "```r",
    "devtools::install_github(\"biostochastics/qiprng\")",
    "```",
    "</Accordion>",
    "",
    "<Accordion title=\"Windows\">",
    "Windows users need Rtools for compilation:",
    "",
    "1. Install [Rtools](https://cran.r-project.org/bin/windows/Rtools/)",
    "2. Install the package:",
    "",
    "```r",
    "devtools::install_github(\"biostochastics/qiprng\")",
    "```",
    "",
    "MPFR is included with Rtools.",
    "</Accordion>",
    "",
    "## Verify Installation",
    "",
    "After installation, verify everything is working:",
    "",
    "```r",
    "library(qiprng)",
    "",
    "# Create a PRNG instance",
    "createPRNG()",
    "",
    "# Generate some random numbers",
    "values <- generatePRNG(10)",
    "print(values)",
    "",
    "# Clean up",
    "cleanup_prng()",
    "```"
  )

  writeLines(install_content, file.path(MINTLIFY_DIR, "installation.mdx"))
  cat("✓ Generated installation.mdx\n")
  return(TRUE)
}

#' Update mint.json with complete navigation structure
update_mint_json <- function(r_functions, cpp_classes) {
  mint_json <- list(
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
    tabs = list(
      list(
        name = "API Reference",
        url = "api-reference"
      ),
      list(
        name = "Guides",
        url = "guides"
      )
    ),
    anchors = list(
      list(
        name = "Documentation",
        icon = "book-open-cover",
        url = "https://qiprng.biostochastics.com"
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
    navigation = list(
      list(
        group = "Get Started",
        pages = list("introduction", "quickstart", "installation")
      ),
      list(
        group = "Guides",
        pages = list(
          "guides/basic-usage",
          "guides/distributions",
          "guides/parallel-generation",
          "guides/performance-tuning"
        )
      ),
      list(
        group = "API Reference",
        pages = c(
          list("api-reference/introduction"),
          list(
            group = "R Functions",
            pages = paste0("api-reference/r-functions/", r_functions)
          ),
          if (length(cpp_classes) > 0) {
            list(
              group = "C++ Classes",
              pages = paste0("api-reference/cpp-classes/", cpp_classes)
            )
          }
        )
      ),
      list(
        group = "Reference",
        pages = list(
          "reference/changelog",
          "reference/news",
          "reference/benchmarks",
          "reference/security"
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

  # Write updated mint.json
  json_content <- toJSON(mint_json, pretty = TRUE, auto_unbox = TRUE)
  writeLines(json_content, file.path(MINTLIFY_DIR, "mint.json"))
  cat("✓ Updated mint.json with complete navigation\n")
  return(TRUE)
}

#' Main documentation generation function
generate_all_docs <- function() {
  cat("================================================\n")
  cat("  Generating Complete Mintlify Documentation\n")
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

  # 5. Process Roxygen2 documentation
  r_docs_count <- process_roxygen_docs()

  # 6. Extract C++ documentation
  cpp_docs_count <- extract_cpp_documentation()

  # 7. Get list of generated files for navigation
  r_functions <- gsub(
    "\\.mdx$", "",
    list.files(file.path(MINTLIFY_DIR, "api-reference", "r-functions"),
      pattern = "\\.mdx$"
    )
  )
  cpp_classes <- gsub(
    "\\.mdx$", "",
    list.files(file.path(MINTLIFY_DIR, "api-reference", "cpp-classes"),
      pattern = "\\.mdx$"
    )
  )

  # 8. Update mint.json with complete navigation
  update_mint_json(r_functions, cpp_classes)

  # 9. Create placeholder files for guides if they don't exist
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
        "This guide is under construction. Please check back soon."
      ), guide_file)
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
    "The qiprng package provides a comprehensive API for high-quality pseudo-random number generation.",
    "",
    "## Core Functions",
    "",
    "- [`createPRNG()`](/api-reference/r-functions/createPRNG) - Initialize a new generator",
    "- [`generatePRNG()`](/api-reference/r-functions/generatePRNG) - Generate random values",
    "- [`updatePRNG()`](/api-reference/r-functions/updatePRNG) - Update generator configuration",
    "- [`jumpAheadPRNG()`](/api-reference/r-functions/jumpAheadPRNG) - Jump ahead in sequence",
    "- [`cleanupPRNG()`](/api-reference/r-functions/cleanupPRNG) - Clean up resources",
    "",
    "## C++ Implementation",
    "",
    "The package is implemented in C++ for performance, with key classes including:",
    "",
    "- `QuadraticIrrational` - Core quadratic irrational implementation",
    "- `EnhancedPRNG` - Enhanced PRNG with distribution support",
    "- `CryptoMixer` - Cryptographic mixing for security",
    "- `Matrix2x2` - Matrix operations for jump-ahead"
  )
  writeLines(api_intro, file.path(MINTLIFY_DIR, "api-reference", "introduction.mdx"))

  # Summary
  cat("\n================================================\n")
  cat("           Documentation Generation Complete\n")
  cat("================================================\n")
  cat(sprintf("✓ Generated %d R function documentation files\n", r_docs_count))
  cat(sprintf("✓ Generated %d C++ class documentation files\n", cpp_docs_count))
  cat("✓ Created introduction, quickstart, and installation guides\n")
  cat("✓ Converted CHANGELOG and NEWS to reference pages\n")
  cat("✓ Updated mint.json with complete navigation\n")
  cat("\nDocumentation ready for deployment at: mintlify/\n")
}

# Run the generation
generate_all_docs()
