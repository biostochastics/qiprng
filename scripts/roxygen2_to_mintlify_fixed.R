#!/usr/bin/env Rscript

# Fixed version of Roxygen2 to Mintlify conversion
# Properly handles MDX formatting requirements

library(tools)
library(stringr)

#' Parse Rd file with better structure preservation
parse_rd_file_fixed <- function(rd_file) {
  rd <- parse_Rd(rd_file)

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

  # Better text extraction that preserves structure
  extract_text <- function(x, preserve_structure = FALSE) {
    if (is.null(x)) {
      return(NULL)
    }

    if (preserve_structure) {
      # For examples and code blocks, preserve line breaks
      text <- character()
      for (item in x) {
        if (is.character(item)) {
          text <- c(text, item)
        } else if (!is.null(attr(item, "Rd_tag"))) {
          tag <- attr(item, "Rd_tag")
          if (tag == "\\dontrun") {
            # Skip dontrun blocks
            next
          } else if (tag == "\\code") {
            text <- c(text, paste(unlist(item), collapse = ""))
          } else {
            text <- c(text, unlist(item))
          }
        }
      }
      return(paste(text, collapse = "\n"))
    } else {
      # Regular text extraction
      return(paste(unlist(x), collapse = " "))
    }
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
      doc$usage <- extract_text(rd[[i]], preserve_structure = TRUE)
    } else if (tag == "\\arguments") {
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
    } else if (tag == "\\examples") {
      doc$examples <- extract_text(rd[[i]], preserve_structure = TRUE)
    }
  }

  return(doc)
}

#' Clean and format text for MDX
clean_mdx_text <- function(text) {
  if (is.null(text)) {
    return(NULL)
  }

  # Fix URLs to proper MDX link format
  text <- gsub("<(https?://[^>]+)>", "[\\1](\\1)", text)

  # Fix email addresses
  text <- gsub("<([^@]+@[^>]+)>", "[\\1](mailto:\\1)", text)

  # Remove Roxygen comment artifacts
  text <- gsub("#' @\\w+", "", text)
  text <- gsub("#'", "", text)

  # Fix bullet points (convert leading spaces to proper list markers)
  lines <- strsplit(text, "\n")[[1]]
  for (i in seq_along(lines)) {
    if (grepl("^\\s+\\w+.*:", lines[i]) && !grepl("^\\s*-", lines[i])) {
      lines[i] <- paste0("- ", trimws(lines[i]))
    }
  }
  text <- paste(lines, collapse = "\n")

  return(text)
}

#' Process examples into proper MDX format
format_examples_mdx <- function(examples_text) {
  if (is.null(examples_text)) {
    return(NULL)
  }

  mdx <- character()
  mdx <- c(mdx, "## Examples")
  mdx <- c(mdx, "")

  # Clean up the examples text
  examples_text <- gsub("#' @examples", "", examples_text)
  examples_text <- gsub("#'", "", examples_text)

  # Split examples by blank lines or comments
  lines <- strsplit(examples_text, "\n")[[1]]

  in_code_block <- FALSE
  current_section <- NULL
  code_buffer <- character()

  for (line in lines) {
    trimmed <- trimws(line)

    # Check if this looks like a section header (starts with capital letter, ends with comment or is descriptive)
    if (nchar(trimmed) > 0 && !grepl("^[a-z_]", trimmed) && grepl("^[A-Z]", trimmed) &&
      !grepl("^\\w+\\(", trimmed)) {
      # This is likely a section header
      if (in_code_block && length(code_buffer) > 0) {
        # Close previous code block
        mdx <- c(mdx, "```r")
        mdx <- c(mdx, code_buffer)
        mdx <- c(mdx, "```")
        mdx <- c(mdx, "")
        code_buffer <- character()
        in_code_block <- FALSE
      }

      # Add section header
      mdx <- c(mdx, paste0("### ", trimmed))
      mdx <- c(mdx, "")
      current_section <- trimmed
    } else if (nchar(trimmed) > 0) {
      # This is code
      if (!in_code_block) {
        in_code_block <- TRUE
      }
      code_buffer <- c(code_buffer, trimmed)
    } else if (in_code_block && length(code_buffer) > 0) {
      # Empty line while in code block - output the block
      mdx <- c(mdx, "```r")
      mdx <- c(mdx, code_buffer)
      mdx <- c(mdx, "```")
      mdx <- c(mdx, "")
      code_buffer <- character()
      in_code_block <- FALSE
    }
  }

  # Handle any remaining code
  if (length(code_buffer) > 0) {
    mdx <- c(mdx, "```r")
    mdx <- c(mdx, code_buffer)
    mdx <- c(mdx, "```")
    mdx <- c(mdx, "")
  }

  return(mdx)
}

#' Convert parsed Rd to properly formatted MDX
rd_to_mdx_fixed <- function(doc, category = "API Reference") {
  mdx <- character()

  # Frontmatter
  mdx <- c(mdx, "---")
  mdx <- c(mdx, sprintf("title: '%s'", doc$name))
  if (!is.null(doc$title)) {
    # Escape single quotes in title
    title_clean <- gsub("'", "\\'", doc$title)
    mdx <- c(mdx, sprintf("description: '%s'", title_clean))
  }
  mdx <- c(mdx, sprintf("sidebarTitle: '%s'", doc$name))
  mdx <- c(mdx, "---")
  mdx <- c(mdx, "")

  # Title and description
  if (!is.null(doc$title)) {
    mdx <- c(mdx, sprintf("# %s", clean_mdx_text(doc$title)))
    mdx <- c(mdx, "")
  }

  if (!is.null(doc$description)) {
    mdx <- c(mdx, clean_mdx_text(doc$description))
    mdx <- c(mdx, "")
  }

  # Usage
  if (!is.null(doc$usage)) {
    mdx <- c(mdx, "## Usage")
    mdx <- c(mdx, "")
    mdx <- c(mdx, "```r")
    mdx <- c(mdx, clean_mdx_text(doc$usage))
    mdx <- c(mdx, "```")
    mdx <- c(mdx, "")
  }

  # Arguments
  if (length(doc$arguments) > 0) {
    mdx <- c(mdx, "## Parameters")
    mdx <- c(mdx, "")

    for (arg_name in names(doc$arguments)) {
      arg_desc <- clean_mdx_text(doc$arguments[[arg_name]])

      # Format parameter description properly
      mdx <- c(mdx, sprintf("<ParamField body=\"%s\" type=\"any\">", arg_name))

      # Split description into bullet points if it contains multiple items
      if (grepl("\\.", arg_desc)) {
        desc_lines <- strsplit(arg_desc, "\\. ")[[1]]
        for (line in desc_lines) {
          if (nchar(trimws(line)) > 0) {
            mdx <- c(mdx, sprintf("  %s.", trimws(line)))
          }
        }
      } else {
        mdx <- c(mdx, sprintf("  %s", arg_desc))
      }

      mdx <- c(mdx, "</ParamField>")
      mdx <- c(mdx, "")
    }
  }

  # Return value
  if (!is.null(doc$value)) {
    mdx <- c(mdx, "## Returns")
    mdx <- c(mdx, "")
    mdx <- c(mdx, clean_mdx_text(doc$value))
    mdx <- c(mdx, "")
  }

  # Examples - properly formatted
  if (!is.null(doc$examples)) {
    examples_mdx <- format_examples_mdx(doc$examples)
    if (!is.null(examples_mdx)) {
      mdx <- c(mdx, examples_mdx)
    }
  }

  # Details
  if (!is.null(doc$details)) {
    mdx <- c(mdx, "## Details")
    mdx <- c(mdx, "")

    # Format details with proper lists
    details_text <- clean_mdx_text(doc$details)
    lines <- strsplit(details_text, "\n")[[1]]
    for (line in lines) {
      if (grepl("^\\s+", line) && !grepl("^-", trimws(line))) {
        mdx <- c(mdx, paste0("- ", trimws(line)))
      } else {
        mdx <- c(mdx, line)
      }
    }
    mdx <- c(mdx, "")
  }

  # Notes and warnings
  if ("Note" %in% names(doc$sections)) {
    mdx <- c(mdx, "<Note>")
    mdx <- c(mdx, clean_mdx_text(doc$sections[["Note"]]))
    mdx <- c(mdx, "</Note>")
    mdx <- c(mdx, "")
  }

  if ("Warning" %in% names(doc$sections)) {
    mdx <- c(mdx, "<Warning>")
    mdx <- c(mdx, clean_mdx_text(doc$sections[["Warning"]]))
    mdx <- c(mdx, "</Warning>")
    mdx <- c(mdx, "")
  }

  return(paste(mdx, collapse = "\n"))
}

# Export functions for use in other scripts
if (!interactive()) {
  # Make functions available when sourced
  parse_rd_file <- parse_rd_file_fixed
  rd_to_mdx <- rd_to_mdx_fixed
}
