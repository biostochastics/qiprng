# .Rprofile for qiprng package
# Configures R code formatting with styler
# Compatible with styler 1.10.0+ (stable as of 2024)
# Source: https://styler.r-lib.org/
# Last verified: 2025-01-01

# Load styler if available
if (requireNamespace("styler", quietly = TRUE)) {
  message("styler package loaded for code formatting")
  
  # Set default style options
  options(
    styler.cache_name = ".styler_cache",
    styler.cache_root = "."
  )
} else {
  message("Note: Install styler package for automatic code formatting")
  message("  install.packages('styler')")
}

# Custom formatting function for the project
format_r_code <- function(path = "R", ...) {
  if (!requireNamespace("styler", quietly = TRUE)) {
    stop("Please install the styler package: install.packages('styler')")
  }
  
  # Use tidyverse style guide with project-specific settings
  styler::style_dir(
    path = path,
    transformers = styler::tidyverse_style(
      scope = "tokens",
      strict = TRUE,
      indent_by = 2,
      start_comments_with_one_space = FALSE,
      reindention = styler::tidyverse_reindention(),
      math_token_spacing = styler::tidyverse_math_token_spacing()
    ),
    filetype = c("R", "Rmd", "Rmarkdown", "Rnw"),
    exclude_files = c("RcppExports.R"),
    ...
  )
}

# Function to format test files
format_test_code <- function() {
  format_r_code("tests/testthat")
}

# Function to check formatting without modifying
check_r_style <- function(path = "R") {
  if (!requireNamespace("styler", quietly = TRUE)) {
    stop("Please install the styler package: install.packages('styler')")
  }
  
  styled <- styler::style_dir(
    path = path,
    transformers = styler::tidyverse_style(),
    dry = "on",
    filetype = c("R", "Rmd", "Rmarkdown", "Rnw")
  )
  
  if (any(styled$changed)) {
    message("The following files need formatting:")
    print(styled[styled$changed, "file"])
    return(FALSE)
  } else {
    message("All R files are properly formatted")
    return(TRUE)
  }
}

# Add custom project functions to search path
.qiprng_env <- new.env()
.qiprng_env$format_r_code <- format_r_code
.qiprng_env$format_test_code <- format_test_code
.qiprng_env$check_r_style <- check_r_style
attach(.qiprng_env, name = "qiprng:formatting")

# Print available formatting commands on startup
if (interactive()) {
  cat("\n=== qiprng Code Formatting Tools ===\n")
  cat("R formatting commands available:\n")
  cat("  format_r_code()     - Format all R files\n")
  cat("  format_test_code()  - Format test files\n")
  cat("  check_r_style()     - Check formatting without changes\n")
  cat("\nC++ formatting:\n")
  cat("  clang-format -i src/*.cpp src/*.hpp\n")
  cat("=====================================\n\n")
}