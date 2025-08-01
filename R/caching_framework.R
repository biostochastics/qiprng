# File: caching_framework.R
# ----------------------------------------------------------------------
#' Caching Framework for QIPRNG
#' 
#' Provides caching functionality for computationally expensive operations
#' using the R.cache package. This framework significantly improves performance
#' for repeated calculations on the same data.
#'
#' @name qiprng-caching
#' @import R.cache
#' @import digest
NULL

#' Initialize cache directory
#' 
#' Creates the cache directory if it doesn't exist and sets up
#' R.cache options.
#' 
#' @param cache_dir Directory for cache storage
#' @param enabled Whether caching is enabled
#' @return Invisible NULL
#' @keywords internal
init_cache <- function(cache_dir = NULL, enabled = TRUE) {
  if (!enabled) {
    options(R.cache.enabled = FALSE)
    return(invisible(NULL))
  }
  
  options(R.cache.enabled = TRUE)
  
  if (!is.null(cache_dir)) {
    # Ensure cache directory exists
    if (!dir.exists(cache_dir)) {
      dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
    }
    R.cache::setCacheRootPath(cache_dir)
  }
  
  invisible(NULL)
}

#' Clear the cache
#' 
#' Removes all cached results or specific cache entries.
#' 
#' @param pattern Optional pattern to match specific cache entries
#' @return Invisible NULL
#' @export
clear_qiprng_cache <- function(pattern = NULL) {
  if (is.null(pattern)) {
    # Clear all QIPRNG cache entries
    R.cache::clearCache(dirs = "qiprng")
  } else {
    # Clear specific entries matching pattern
    # This is more complex and would require listing cache entries
    warning("Pattern-based cache clearing not yet implemented. Clearing all cache.")
    R.cache::clearCache(dirs = "qiprng")
  }
  invisible(NULL)
}

#' Get cache statistics
#' 
#' Returns information about cache usage and performance.
#' 
#' @return List with cache statistics
#' @export
qiprng_cache_stats <- function() {
  cache_dir <- R.cache::getCacheRootPath()
  
  if (!dir.exists(file.path(cache_dir, "qiprng"))) {
    return(list(
      enabled = getOption("R.cache.enabled", TRUE),
      cache_dir = cache_dir,
      size_mb = 0,
      num_entries = 0
    ))
  }
  
  # Get cache size
  cache_files <- list.files(file.path(cache_dir, "qiprng"), 
                           full.names = TRUE, recursive = TRUE)
  
  if (length(cache_files) == 0) {
    cache_size <- 0
  } else {
    cache_size <- sum(file.info(cache_files)$size, na.rm = TRUE) / 1024^2
  }
  
  list(
    enabled = getOption("R.cache.enabled", TRUE),
    cache_dir = cache_dir,
    size_mb = round(cache_size, 2),
    num_entries = length(cache_files)
  )
}

#' Cached ACF function
#' 
#' Cached wrapper for stats::acf() function. Uses R.cache to store
#' results for repeated calls with the same data and parameters.
#' 
#' @param x A univariate or multivariate time series
#' @param lag.max Maximum lag at which to calculate the acf
#' @param ... Additional arguments passed to acf()
#' @return An object of class "acf"
#' @export
cached_acf <- function(x, lag.max = NULL, ...) {
  # Extract plot argument if provided
  args <- list(...)
  plot_arg <- if ("plot" %in% names(args)) args$plot else FALSE
  
  # Remove plot from args to avoid duplication
  args$plot <- NULL
  
  if (!getOption("R.cache.enabled", TRUE)) {
    return(do.call(stats::acf, c(list(x = x, lag.max = lag.max, plot = plot_arg), args)))
  }
  
  # Create cache key
  key <- list(
    func = "acf",
    data_hash = digest::digest(x),
    lag.max = lag.max,
    plot = plot_arg,
    args = args
  )
  
  # Manual caching (memoizedCall has issues with some stats functions)
  cached_result <- R.cache::loadCache(key = key, dirs = "qiprng")
  
  if (!is.null(cached_result)) {
    return(cached_result)
  }
  
  # Compute ACF
  result <- do.call(stats::acf, c(list(x = x, lag.max = lag.max, plot = plot_arg), args))
  
  # Save to cache
  R.cache::saveCache(result, key = key, dirs = "qiprng")
  
  return(result)
}

#' Cached PACF function
#' 
#' Cached wrapper for stats::pacf() function. Uses R.cache to store
#' results for repeated calls with the same data and parameters.
#' 
#' @param x A univariate or multivariate time series
#' @param lag.max Maximum lag at which to calculate the pacf
#' @param ... Additional arguments passed to pacf()
#' @return An object of class "acf"
#' @export
cached_pacf <- function(x, lag.max = NULL, ...) {
  # Extract plot argument if provided
  args <- list(...)
  plot_arg <- if ("plot" %in% names(args)) args$plot else FALSE
  
  # Remove plot from args to avoid duplication
  args$plot <- NULL
  
  if (!getOption("R.cache.enabled", TRUE)) {
    return(do.call(stats::pacf, c(list(x = x, lag.max = lag.max, plot = plot_arg), args)))
  }
  
  # Create cache key
  key <- list(
    func = "pacf",
    data_hash = digest::digest(x),
    lag.max = lag.max,
    plot = plot_arg,
    args = args
  )
  
  # Manual caching (memoizedCall has issues with some stats functions)
  cached_result <- R.cache::loadCache(key = key, dirs = "qiprng")
  
  if (!is.null(cached_result)) {
    return(cached_result)
  }
  
  # Compute PACF
  result <- do.call(stats::pacf, c(list(x = x, lag.max = lag.max, plot = plot_arg), args))
  
  # Save to cache
  R.cache::saveCache(result, key = key, dirs = "qiprng")
  
  return(result)
}

#' Cached spectrum function
#' 
#' Cached wrapper for stats::spectrum() function. Uses R.cache to store
#' results for repeated calls with the same data and parameters.
#' 
#' @param x A univariate or multivariate time series
#' @param ... Additional arguments passed to spectrum()
#' @return An object of class "spec"
#' @export
cached_spectrum <- function(x, ...) {
  # Extract plot argument if provided
  args <- list(...)
  plot_arg <- if ("plot" %in% names(args)) args$plot else FALSE
  
  # Remove plot from args to avoid duplication
  args$plot <- NULL
  
  if (!getOption("R.cache.enabled", TRUE)) {
    return(do.call(stats::spectrum, c(list(x = x, plot = plot_arg), args)))
  }
  
  # Create cache key
  key <- list(
    func = "spectrum",
    data_hash = digest::digest(x),
    plot = plot_arg,
    args = args
  )
  
  # Manual caching for spectrum function (memoizedCall doesn't work well with spectrum)
  cached_result <- R.cache::loadCache(key = key, dirs = "qiprng")
  
  if (!is.null(cached_result)) {
    return(cached_result)
  }
  
  # Compute spectrum
  result <- do.call(stats::spectrum, c(list(x = x, plot = plot_arg), args))
  
  # Save to cache
  R.cache::saveCache(result, key = key, dirs = "qiprng")
  
  return(result)
}

#' Cached compression function
#' 
#' Cached wrapper for memCompress() function. Uses R.cache to store
#' results for repeated compressions with the same data and parameters.
#' 
#' @param x A raw vector to compress
#' @param type Compression type
#' @param ... Additional arguments passed to memCompress()
#' @return Compressed raw vector
#' @export
cached_compress <- function(x, type = c("gzip", "bzip2", "xz"), ...) {
  if (!getOption("R.cache.enabled", TRUE)) {
    return(memCompress(x, type = type, ...))
  }
  
  type <- match.arg(type)
  
  # Create cache key
  key <- list(
    func = "compress",
    data_hash = digest::digest(x),
    type = type,
    args = list(...)
  )
  
  # Use memoizedCall for caching
  R.cache::memoizedCall(
    FUN = memCompress,
    from = x,
    type = type,
    ...,
    key = key,
    dirs = "qiprng"
  )
}

#' Set cache TTL (Time To Live)
#' 
#' Configure how long cached results should be kept before expiration.
#' 
#' @param hours Number of hours before cache entries expire
#' @return Invisible NULL
#' @export
set_cache_ttl <- function(hours = 24) {
  options(R.cache.ttl = hours * 3600)  # Convert to seconds
  invisible(NULL)
}

#' Check if caching is enabled
#' 
#' @return Logical indicating if caching is enabled
#' @export
is_cache_enabled <- function() {
  getOption("R.cache.enabled", TRUE)
}

#' Enable or disable caching
#' 
#' @param enabled Logical indicating whether to enable caching
#' @return Previous cache state (invisible)
#' @export
set_cache_enabled <- function(enabled = TRUE) {
  old_state <- getOption("R.cache.enabled", TRUE)
  options(R.cache.enabled = enabled)
  invisible(old_state)
}

#' Memoized test result wrapper
#' 
#' Caches entire test results with category-specific TTL support.
#' This function wraps test execution to provide transparent caching
#' of test results, significantly improving performance for repeated
#' test runs on the same data.
#' 
#' @param test_func Function that performs the test
#' @param test_name Name of the specific test
#' @param test_category Category of the test (e.g., "basic", "correlation")
#' @param data The data to test
#' @param config Test configuration
#' @param ... Additional arguments passed to test_func
#' @return Test result (from cache or freshly computed)
#' @export
cached_test_result <- function(test_func, test_name, test_category, 
                               data, config, ...) {
  # Check if caching is enabled
  if (!getOption("R.cache.enabled", TRUE) || 
      !isTRUE(config$cache_test_results)) {
    return(test_func(data, config, ...))
  }
  
  # Get PRNG seed from package environment if available
  prng_seed <- NULL
  if (exists(".pkgenv", mode = "environment")) {
    pkgenv <- get(".pkgenv", mode = "environment")
    if (exists("current_config", envir = pkgenv)) {
      current_config <- get("current_config", envir = pkgenv)
      if (!is.null(current_config$seed)) {
        prng_seed <- current_config$seed
      }
    }
  }
  
  # Generate cache key including PRNG state
  key <- list(
    func = paste0("test_", test_category, "_", test_name),
    data_hash = digest::digest(data),
    config_hash = digest::digest(config),
    prng_seed = prng_seed,
    args = list(...)
  )
  
  # Get category-specific TTL
  ttl_config_name <- paste0("cache_ttl_", test_category, "_hours")
  ttl_hours <- config[[ttl_config_name]]
  if (is.null(ttl_hours)) {
    ttl_hours <- config$cache_ttl_hours
  }
  if (is.null(ttl_hours)) {
    ttl_hours <- 24  # Default to 24 hours
  }
  
  # Set TTL for this call
  old_ttl <- getOption("R.cache.ttl")
  options(R.cache.ttl = ttl_hours * 3600)
  on.exit(options(R.cache.ttl = old_ttl))
  
  # Use memoizedCall with compression
  result <- R.cache::memoizedCall(
    FUN = test_func,
    data = data,
    config = config,
    ...,
    key = key,
    dirs = c("qiprng", "test_results", test_category),
    compress = TRUE
  )
  
  return(result)
}

#' Clear test result cache
#' 
#' Removes cached test results for a specific category or all categories.
#' 
#' @param category Test category to clear (NULL for all categories)
#' @return Invisible NULL
#' @export
clear_test_cache <- function(category = NULL) {
  if (is.null(category)) {
    # Clear all test result caches
    R.cache::clearCache(dirs = c("qiprng", "test_results"))
  } else {
    # Clear specific category
    R.cache::clearCache(dirs = c("qiprng", "test_results", category))
  }
  invisible(NULL)
}

#' Get test cache statistics
#' 
#' Returns information about cached test results by category.
#' 
#' @return Data frame with cache statistics by category
#' @export
test_cache_stats <- function() {
  cache_dir <- R.cache::getCacheRootPath()
  base_path <- file.path(cache_dir, "qiprng", "test_results")
  
  if (!dir.exists(base_path)) {
    return(data.frame(
      category = character(0),
      num_entries = integer(0),
      size_mb = numeric(0),
      stringsAsFactors = FALSE
    ))
  }
  
  # Get all test categories
  categories <- list.dirs(base_path, full.names = FALSE, recursive = FALSE)
  
  if (length(categories) == 0) {
    return(data.frame(
      category = character(0),
      num_entries = integer(0),
      size_mb = numeric(0),
      stringsAsFactors = FALSE
    ))
  }
  
  # Calculate stats for each category
  stats <- lapply(categories, function(cat) {
    cat_path <- file.path(base_path, cat)
    files <- list.files(cat_path, full.names = TRUE, recursive = TRUE)
    
    if (length(files) == 0) {
      size_mb <- 0
      num_entries <- 0
    } else {
      size_mb <- sum(file.info(files)$size, na.rm = TRUE) / 1024^2
      num_entries <- length(files)
    }
    
    data.frame(
      category = cat,
      num_entries = num_entries,
      size_mb = round(size_mb, 2),
      stringsAsFactors = FALSE
    )
  })
  
  do.call(rbind, stats)
}

#' Export cached test results
#' 
#' Exports all cached test results to a file for archival or sharing.
#' 
#' @param file Path to export file (RDS format)
#' @param category Optional category filter
#' @return Invisible TRUE on success
#' @export
export_cached_results <- function(file, category = NULL) {
  # This is a placeholder - actual implementation would need to
  # traverse cache and reconstruct results
  warning("export_cached_results not yet implemented")
  invisible(FALSE)
}