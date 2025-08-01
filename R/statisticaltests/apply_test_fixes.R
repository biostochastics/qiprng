# File: apply_test_fixes.R
# ----------------------------------------------------------------------
#' Apply all test fixes to ensure proper test behavior
#'
#' This script sources and applies fixes for failing statistical tests

# Source the fixes
source_if_exists <- function(file) {
  if (file.exists(file)) {
    source(file)
    TRUE
  } else {
    pkg_file <- system.file(file, package = "qiprng")
    if (file.exists(pkg_file)) {
      source(pkg_file)
      TRUE
    } else {
      FALSE
    }
  }
}

# Apply multi-dimensional test fixes
apply_all_test_fixes <- function(verbose = TRUE) {
  
  if (verbose) {
    cat("Applying statistical test fixes...\n")
  }
  
  # Source fix file
  fix_file <- "R/statisticaltests/multidim_tests_fixes.R"
  if (!source_if_exists(fix_file)) {
    warning("Could not find multidim_tests_fixes.R")
    return(FALSE)
  }
  
  # Apply the fixes
  if (exists("apply_multidim_test_fixes")) {
    apply_multidim_test_fixes()
    
    if (verbose) {
      cat("\nTest fixes applied successfully!\n")
      cat("The following tests have been fixed:\n")
      cat("- Ripley's K function test (2D and 3D)\n")
      cat("- 3D scatter test (triplet area calculation)\n")
      cat("- 3D nearest neighbor test\n")
      cat("- 3D minimum distance test\n")
      cat("\nAll generators should now show improved pass rates.\n")
    }
    
    return(TRUE)
  } else {
    warning("apply_multidim_test_fixes function not found")
    return(FALSE)
  }
}

# Auto-apply fixes when this file is sourced
if (!exists(".test_fixes_applied", envir = .GlobalEnv)) {
  apply_all_test_fixes(verbose = FALSE)
  assign(".test_fixes_applied", TRUE, envir = .GlobalEnv)
}