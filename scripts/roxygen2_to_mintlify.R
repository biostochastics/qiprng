#!/usr/bin/env Rscript
# ----------------------------------------------------------------------
# Script: roxygen2_to_mintlify.R
# Purpose: Convert roxygen2 documentation to Mintlify MDX format
# Author: qiprng documentation pipeline
# ----------------------------------------------------------------------

library(tools)
library(jsonlite)

#' Parse an Rd file and extract documentation components
#' @param rd_file Path to the .Rd file
#' @return List with parsed documentation
parse_rd_file <- function(rd_file) {
  # Parse the Rd file
  rd <- parse_Rd(rd_file)

  # Extract components
  doc <- list(
    name = NULL,
    title = NULL,
    description = NULL,
    usage = NULL,
    arguments = list(),
    value = NULL,
    details = NULL,
    sections = list(),
    examples = NULL,
    seealso = NULL,
    author = NULL,
    references = NULL
  )

  # Helper function to extract text from Rd tags
  extract_text <- function(x) {
    if (is.null(x)) {
      return(NULL)
    }
    paste(unlist(x), collapse = " ")
  }

  # Parse each tag
  for (i in seq_along(rd)) {
    tag <- attr(rd[[i]], "Rd_tag")

    if (tag == "\\name") {
      doc$name <- extract_text(rd[[i]])
    } else if (tag == "\\title") {
      doc$title <- extract_text(rd[[i]])
    } else if (tag == "\\description") {
      doc$description <- extract_text(rd[[i]])
    } else if (tag == "\\usage") {
      doc$usage <- extract_text(rd[[i]])
    } else if (tag == "\\arguments") {
      # Parse arguments
      args <- rd[[i]]
      for (j in seq_along(args)) {
        if (attr(args[[j]], "Rd_tag") == "\\item") {
          arg_name <- extract_text(args[[j]][[1]])
          arg_desc <- extract_text(args[[j]][[2]])
          doc$arguments[[arg_name]] <- arg_desc
        }
      }
    } else if (tag == "\\value") {
      doc$value <- extract_text(rd[[i]])
    } else if (tag == "\\details") {
      doc$details <- extract_text(rd[[i]])
    } else if (tag == "\\section") {
      section_title <- extract_text(rd[[i]][[1]])
      section_content <- extract_text(rd[[i]][[2]])
      doc$sections[[section_title]] <- section_content
    } else if (tag == "\\examples") {
      doc$examples <- extract_text(rd[[i]])
    } else if (tag == "\\seealso") {
      doc$seealso <- extract_text(rd[[i]])
    } else if (tag == "\\author") {
      doc$author <- extract_text(rd[[i]])
    } else if (tag == "\\references") {
      doc$references <- extract_text(rd[[i]])
    }
  }

  return(doc)
}

#' Convert parsed Rd to Mintlify MDX format
#' @param doc Parsed documentation list
#' @param category Category for the function (e.g., "Core Functions", "Utilities")
#' @return MDX formatted string
rd_to_mdx <- function(doc, category = "API Reference") {
  mdx <- character()

  # Frontmatter
  mdx <- c(mdx, "---")
  mdx <- c(mdx, sprintf("title: '%s'", doc$name))
  if (!is.null(doc$title)) {
    mdx <- c(mdx, sprintf("description: '%s'", gsub("'", "\\'", doc$title)))
  }
  mdx <- c(mdx, sprintf("sidebarTitle: '%s'", doc$name))
  mdx <- c(mdx, "---")
  mdx <- c(mdx, "")

  # Title and description
  if (!is.null(doc$title)) {
    mdx <- c(mdx, sprintf("# %s", doc$title))
    mdx <- c(mdx, "")
  }

  if (!is.null(doc$description)) {
    mdx <- c(mdx, doc$description)
    mdx <- c(mdx, "")
  }

  # Usage
  if (!is.null(doc$usage)) {
    mdx <- c(mdx, "## Usage")
    mdx <- c(mdx, "")
    mdx <- c(mdx, "```r")
    mdx <- c(mdx, doc$usage)
    mdx <- c(mdx, "```")
    mdx <- c(mdx, "")
  }

  # Arguments
  if (length(doc$arguments) > 0) {
    mdx <- c(mdx, "## Parameters")
    mdx <- c(mdx, "")

    # Create a parameter list with proper formatting
    # Start parameters section (no wrapper needed for R docs)

    for (arg_name in names(doc$arguments)) {
      # Use 'body' type for R function parameters (not API query params)
      mdx <- c(mdx, sprintf("<ParamField body=\"%s\" type=\"any\">", arg_name))
      mdx <- c(mdx, sprintf("  %s", doc$arguments[[arg_name]]))
      mdx <- c(mdx, "</ParamField>")
    }
    mdx <- c(mdx, "")
  }

  # Return value
  if (!is.null(doc$value)) {
    mdx <- c(mdx, "## Returns")
    mdx <- c(mdx, "")
    mdx <- c(mdx, doc$value)
    mdx <- c(mdx, "")
  }

  # Details
  if (!is.null(doc$details)) {
    mdx <- c(mdx, "## Details")
    mdx <- c(mdx, "")
    mdx <- c(mdx, doc$details)
    mdx <- c(mdx, "")
  }

  # Additional sections
  if (length(doc$sections) > 0) {
    for (section_name in names(doc$sections)) {
      # Clean up section name
      clean_name <- gsub(":", "", section_name)
      mdx <- c(mdx, sprintf("## %s", clean_name))
      mdx <- c(mdx, "")

      # Handle special formatting for certain sections
      if (grepl("Thread Safety|Performance|Security", section_name)) {
        mdx <- c(mdx, "<Note>")
        mdx <- c(mdx, doc$sections[[section_name]])
        mdx <- c(mdx, "</Note>")
      } else {
        mdx <- c(mdx, doc$sections[[section_name]])
      }
      mdx <- c(mdx, "")
    }
  }

  # Examples
  if (!is.null(doc$examples)) {
    mdx <- c(mdx, "## Examples")
    mdx <- c(mdx, "")

    # Parse example code blocks
    example_lines <- strsplit(doc$examples, "\n")[[1]]
    in_donttest <- FALSE
    code_block <- character()

    for (line in example_lines) {
      if (grepl("\\\\donttest\\{", line)) {
        if (length(code_block) > 0) {
          mdx <- c(mdx, "```r")
          mdx <- c(mdx, code_block)
          mdx <- c(mdx, "```")
          mdx <- c(mdx, "")
          code_block <- character()
        }
        mdx <- c(mdx, "<Accordion title=\"Advanced Example (may take longer to run)\">")
        mdx <- c(mdx, "")
        mdx <- c(mdx, "```r")
        in_donttest <- TRUE
      } else if (in_donttest && grepl("^\\}", line)) {
        mdx <- c(mdx, "```")
        mdx <- c(mdx, "")
        mdx <- c(mdx, "</Accordion>")
        mdx <- c(mdx, "")
        in_donttest <- FALSE
      } else {
        if (in_donttest) {
          mdx <- c(mdx, line)
        } else {
          code_block <- c(code_block, line)
        }
      }
    }

    if (length(code_block) > 0) {
      mdx <- c(mdx, "```r")
      mdx <- c(mdx, code_block)
      mdx <- c(mdx, "```")
      mdx <- c(mdx, "")
    }
  }

  # See also
  if (!is.null(doc$seealso)) {
    mdx <- c(mdx, "## See Also")
    mdx <- c(mdx, "")

    # Parse links and create proper references
    links <- strsplit(doc$seealso, ",")[[1]]
    mdx <- c(mdx, "<CardGroup cols={2}>")
    for (link in links) {
      link <- trimws(link)
      mdx <- c(mdx, sprintf("  <Card title=\"%s\" href=\"/api-reference/r-functions/%s\">", link, link))
      mdx <- c(mdx, sprintf("    Related function: %s", link))
      mdx <- c(mdx, "  </Card>")
    }
    mdx <- c(mdx, "</CardGroup>")
    mdx <- c(mdx, "")
  }

  # References
  if (!is.null(doc$references)) {
    mdx <- c(mdx, "## References")
    mdx <- c(mdx, "")
    mdx <- c(mdx, doc$references)
    mdx <- c(mdx, "")
  }

  return(paste(mdx, collapse = "\n"))
}

#' Main conversion function
#' @param input_dir Directory containing .Rd files (usually man/)
#' @param output_dir Directory for Mintlify MDX output
#' @param categories Named list mapping function patterns to categories
convert_roxygen2_to_mintlify <- function(
    input_dir = "man",
    output_dir = "mintlify/api-reference/r-functions",
    categories = list(
      "Core Functions" = c("createPRNG", "generatePRNG", "updatePRNG", "cleanupPRNG"),
      "Advanced Functions" = c("jumpAheadPRNG", "reseedPRNG", "skipPRNG"),
      "Caching" = c("cached_", "cache_", "clear_cache"),
      "Validation" = c("test_", "validate_", "benchmark_"),
      "Utilities" = c("default_config", "create_excellent_prng_config")
    )) {
  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Get all .Rd files
  rd_files <- list.files(input_dir, pattern = "\\.Rd$", full.names = TRUE)

  cat(sprintf("Found %d .Rd files to convert\n", length(rd_files)))

  # Process each file
  for (rd_file in rd_files) {
    cat(sprintf("Processing: %s\n", basename(rd_file)))

    tryCatch(
      {
        # Parse the Rd file
        doc <- parse_rd_file(rd_file)

        # Determine category
        category <- "API Reference"
        for (cat_name in names(categories)) {
          patterns <- categories[[cat_name]]
          if (any(sapply(patterns, function(p) grepl(p, doc$name)))) {
            category <- cat_name
            break
          }
        }

        # Convert to MDX
        mdx_content <- rd_to_mdx(doc, category)

        # Write output file
        output_file <- file.path(output_dir, paste0(doc$name, ".mdx"))
        writeLines(mdx_content, output_file)

        cat(sprintf("  ✓ Created: %s\n", output_file))
      },
      error = function(e) {
        cat(sprintf("  ✗ Error processing %s: %s\n", basename(rd_file), e$message))
      }
    )
  }

  # Generate index file
  generate_index_file(output_dir, rd_files, categories)

  cat("\nConversion complete!\n")
}

#' Generate an index file for the API reference
generate_index_file <- function(output_dir, rd_files, categories) {
  index_content <- c(
    "---",
    "title: 'R Functions'",
    "description: 'Complete API reference for qiprng R functions'",
    "---",
    "",
    "# R Function Reference",
    "",
    "Complete documentation for all qiprng R functions, automatically generated from roxygen2 documentation.",
    ""
  )

  # Group functions by category
  for (cat_name in names(categories)) {
    index_content <- c(index_content, sprintf("## %s", cat_name))
    index_content <- c(index_content, "")
    index_content <- c(index_content, "<CardGroup cols={2}>")

    patterns <- categories[[cat_name]]
    for (rd_file in rd_files) {
      doc <- parse_rd_file(rd_file)
      if (any(sapply(patterns, function(p) grepl(p, doc$name)))) {
        index_content <- c(
          index_content,
          sprintf(
            "  <Card title=\"%s\" href=\"/api-reference/r-functions/%s\">",
            doc$name, doc$name
          ),
          sprintf("    %s", doc$title),
          "  </Card>"
        )
      }
    }

    index_content <- c(index_content, "</CardGroup>")
    index_content <- c(index_content, "")
  }

  writeLines(index_content, file.path(output_dir, "index.mdx"))
}

# Run the conversion if this script is executed directly
if (!interactive()) {
  # Parse command line arguments
  args <- commandArgs(trailingOnly = TRUE)

  if (length(args) >= 2) {
    convert_roxygen2_to_mintlify(
      input_dir = args[1],
      output_dir = args[2]
    )
  } else {
    # Use defaults
    convert_roxygen2_to_mintlify()
  }
}
