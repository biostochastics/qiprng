# File: config_manager.R
# ----------------------------------------------------------------------
#' Configuration Management for External Testing Tools
#'
#' This module provides a flexible configuration management system for
#' external testing tools, supporting tool paths, parameters, and
#' platform-specific settings.
#'
#' Features:
#' \itemize{
#'   \item Platform-specific tool path detection
#'   \item Environment variable support
#'   \item JSON/YAML configuration file support
#'   \item Runtime configuration updates
#'   \item Configuration validation
#'   \item Default fallback values
#' }
#'
#' @name config-manager
#' @import jsonlite
#' @importFrom utils file_test
NULL

#' Default external tool configurations
#'
#' @format A list with tool-specific configurations
#' @export
default_external_tool_config <- list(
  # NIST Statistical Test Suite
  nist_sts = list(
    enabled = FALSE,
    paths = list(
      linux = "/usr/local/bin/assess",
      macos = "/usr/local/bin/assess",
      windows = "C:/NIST_STS/assess.exe"
    ),
    parameters = list(
      templates_dir = "./templates",
      experiments_dir = "./experiments",
      data_dir = "./data",
      bitstreams = 128,
      stream_length = 1000000,
      significance_level = 0.01,
      tests = c(
        "frequency", "block_frequency", "runs", "longest_run",
        "rank", "fft", "non_overlapping_template", "overlapping_template",
        "universal", "linear_complexity", "serial", "approximate_entropy",
        "cumulative_sums", "random_excursions", "random_excursions_variant"
      )
    )
  ),

  # Dieharder Test Suite
  dieharder = list(
    enabled = TRUE,
    paths = list(
      linux = "dieharder",
      macos = "dieharder",
      windows = "dieharder.exe"
    ),
    parameters = list(
      test_ids = c(0:17, 100:102, 200:203), # Default test IDs
      psamples = 100,
      tsamples = 100000,
      quiet = FALSE,
      brief = TRUE
    )
  ),

  # ENT - Entropy Test
  ent = list(
    enabled = TRUE,
    paths = list(
      linux = "ent",
      macos = "ent",
      windows = "ent.exe"
    ),
    parameters = list(
      binary_mode = FALSE,
      fold = FALSE,
      terse = FALSE
    )
  ),

  # TestU01
  testu01 = list(
    enabled = FALSE,
    paths = list(
      linux = "/usr/local/bin/testu01",
      macos = "/usr/local/bin/testu01",
      windows = "C:/TestU01/testu01.exe"
    ),
    parameters = list(
      battery = "smallcrush", # smallcrush, crush, bigcrush
      nb_tests = 15,
      res_file = "./testu01_results.txt"
    )
  ),

  # PractRand
  practrand = list(
    enabled = FALSE,
    paths = list(
      linux = "/usr/local/bin/RNG_test",
      macos = "/usr/local/bin/RNG_test",
      windows = "C:/PractRand/RNG_test.exe"
    ),
    parameters = list(
      length = "1GB",
      folding = 0,
      expanded = FALSE,
      multithreaded = TRUE
    )
  )
)

#' Default internal test configurations
#'
#' @format A list with test-specific parameters
#' @export
default_internal_test_config <- list(
  # Global test parameters
  global = list(
    default_sample_size = 10000,
    significance_level = 0.01,
    p_value_threshold = 0.01,
    max_iterations = 1000,
    tolerance = 1e-6,
    random_seed = NULL,
    verbose = FALSE
  ),

  # Performance and parallelization
  performance = list(
    parallel_enabled = TRUE,
    max_cores = parallel::detectCores() - 1,
    chunk_size = 1000,
    memory_limit_mb = 4096,
    gc_frequency = 100,
    cache_enabled = TRUE,
    cache_size_mb = 512
  ),

  # Classical tests parameters
  classical_tests = list(
    chi_square = list(
      bins = 10,
      min_expected_count = 5,
      significance_level = 0.01
    ),
    kolmogorov_smirnov = list(
      significance_level = 0.01,
      alternative = "two.sided"
    ),
    runs_test = list(
      significance_level = 0.01,
      min_runs = 5
    )
  ),

  # Binary tests parameters
  binary_tests = list(
    frequency = list(
      block_size = 128,
      significance_level = 0.01
    ),
    runs = list(
      significance_level = 0.01
    ),
    longest_run = list(
      block_size = 128,
      significance_level = 0.01
    )
  ),

  # Compression tests parameters
  compression_tests = list(
    algorithms = c("gzip", "bzip2", "xz"),
    compression_levels = list(
      gzip = 6,
      bzip2 = 9,
      xz = 6
    ),
    min_compression_ratio = 0.9,
    max_compression_ratio = 1.0
  ),

  # Correlation tests parameters
  correlation_tests = list(
    max_lag = 100,
    significance_level = 0.01,
    methods = c("pearson", "spearman", "kendall"),
    autocorrelation = list(
      plot = TRUE,
      partial = TRUE
    )
  ),

  # External tests parameters
  external_tests = list(
    sample_size = 1000000,
    output_format = "binary",
    timeout_seconds = 300,
    retry_attempts = 3,
    cleanup_temp_files = TRUE
  ),

  # Multidimensional tests parameters
  multidim_tests = list(
    dimensions = c(2, 3, 4),
    sample_size_per_dim = 1000,
    projection_methods = c("pca", "ica", "tsne"),
    distance_metrics = c("euclidean", "manhattan", "mahalanobis")
  ),

  # Visualization parameters
  visualization = list(
    enabled = TRUE,
    output_format = "png",
    dpi = 300,
    width = 8,
    height = 6,
    theme = "minimal",
    color_palette = "viridis"
  ),

  # Reporting parameters
  reporting = list(
    format = c("html", "pdf", "markdown"),
    include_plots = TRUE,
    include_raw_data = FALSE,
    summary_statistics = TRUE,
    detailed_results = TRUE,
    confidence_intervals = TRUE
  )
)

#' Merge default configurations
#'
#' Combines external tool config and internal test config
#' @return Complete default configuration
#' @export
get_default_config <- function() {
  list(
    external_tools = default_external_tool_config,
    internal_tests = default_internal_test_config
  )
}

#' Load configuration from file
#'
#' @param config_file Path to configuration file (JSON or YAML)
#' @param merge_defaults Whether to merge with default configuration
#' @return Configuration list
#' @export
load_config <- function(config_file = NULL, merge_defaults = TRUE) {
  config <- list()

  # Try to load from file
  if (!is.null(config_file) && file.exists(config_file)) {
    ext <- tolower(tools::file_ext(config_file))

    if (ext == "json") {
      config <- jsonlite::fromJSON(config_file, simplifyVector = FALSE)
    } else if (ext %in% c("yaml", "yml")) {
      if (requireNamespace("yaml", quietly = TRUE)) {
        config <- yaml::read_yaml(config_file)
      } else {
        warning("yaml package not installed. Cannot read YAML config file.")
      }
    } else {
      stop("Unsupported configuration file format. Use JSON or YAML.")
    }
  }

  # Load from environment variables
  config <- merge_env_config(config)

  # Merge with defaults if requested
  if (merge_defaults) {
    # Get complete default config
    defaults <- get_default_config()

    # Handle backward compatibility - if config has tool names at root level
    if (any(names(config) %in% names(default_external_tool_config))) {
      # Old format - convert to new structure
      old_config <- config
      config <- list(
        external_tools = old_config,
        internal_tests = list()
      )
    }

    # Merge external tools
    if (!is.null(config$external_tools)) {
      config$external_tools <- merge_configs(defaults$external_tools, config$external_tools)
    } else {
      config$external_tools <- defaults$external_tools
    }

    # Merge internal tests
    if (!is.null(config$internal_tests)) {
      config$internal_tests <- merge_configs(defaults$internal_tests, config$internal_tests)
    } else {
      config$internal_tests <- defaults$internal_tests
    }
  }

  return(config)
}

#' Merge environment variable configuration
#'
#' @param config Existing configuration
#' @return Updated configuration with environment variables
#' @keywords internal
merge_env_config <- function(config) {
  # Ensure proper structure
  if (is.null(config$external_tools)) {
    config$external_tools <- list()
  }
  if (is.null(config$internal_tests)) {
    config$internal_tests <- list()
  }

  # Check for tool-specific environment variables
  tools <- c("nist_sts", "dieharder", "ent", "testu01", "practrand")

  for (tool in tools) {
    # Check for path override
    env_var <- paste0("QIPRNG_", toupper(tool), "_PATH")
    path <- Sys.getenv(env_var)
    if (nzchar(path)) {
      if (is.null(config$external_tools[[tool]])) {
        config$external_tools[[tool]] <- list()
      }
      if (is.null(config$external_tools[[tool]]$paths)) {
        config$external_tools[[tool]]$paths <- list()
      }
      config$external_tools[[tool]]$paths[[get_platform()]] <- path
    }

    # Check for enabled status
    env_var_enabled <- paste0("QIPRNG_", toupper(tool), "_ENABLED")
    enabled <- Sys.getenv(env_var_enabled)
    if (nzchar(enabled)) {
      if (is.null(config$external_tools[[tool]])) {
        config$external_tools[[tool]] <- list()
      }
      config$external_tools[[tool]]$enabled <- as.logical(enabled)
    }
  }

  # Merge internal test environment variables
  config <- merge_internal_env_config(config)

  return(config)
}

#' Merge two configurations
#'
#' @param base Base configuration
#' @param override Override configuration
#' @return Merged configuration
#' @keywords internal
merge_configs <- function(base, override) {
  if (length(override) == 0) {
    return(base)
  }

  for (key in names(override)) {
    if (key %in% names(base) && is.list(base[[key]]) && is.list(override[[key]])) {
      base[[key]] <- merge_configs(base[[key]], override[[key]])
    } else {
      base[[key]] <- override[[key]]
    }
  }

  return(base)
}

#' Get current platform
#'
#' @return Platform name: "linux", "macos", or "windows"
#' @export
get_platform <- function() {
  if (.Platform$OS.type == "windows") {
    return("windows")
  } else if (Sys.info()["sysname"] == "Darwin") {
    return("macos")
  } else {
    return("linux")
  }
}

#' Get tool path for current platform
#'
#' @param tool_name Name of the tool
#' @param config Configuration list
#' @return Full path to the tool executable
#' @export
get_tool_path <- function(tool_name, config = NULL) {
  if (is.null(config)) {
    config <- load_config()
  }

  # Handle both old and new config structure
  if (!is.null(config$external_tools)) {
    # New structure
    if (!tool_name %in% names(config$external_tools)) {
      stop(paste("Unknown tool:", tool_name))
    }
    tool_config <- config$external_tools[[tool_name]]
  } else {
    # Old structure (backward compatibility)
    if (!tool_name %in% names(config)) {
      stop(paste("Unknown tool:", tool_name))
    }
    tool_config <- config[[tool_name]]
  }

  platform <- get_platform()

  # Get platform-specific path
  if (!is.null(tool_config$paths[[platform]])) {
    path <- tool_config$paths[[platform]]
  } else if (!is.null(tool_config$paths$default)) {
    path <- tool_config$paths$default
  } else {
    stop(paste("No path configured for", tool_name, "on", platform))
  }

  # Expand path
  path <- path.expand(path)

  # Check if it's in PATH
  if (!file.exists(path) && !grepl("/", path) && !grepl("\\\\", path)) {
    which_path <- Sys.which(path)
    if (nzchar(which_path)) {
      path <- which_path
    }
  }

  return(path)
}

#' Validate tool configuration
#'
#' @param tool_name Name of the tool to validate
#' @param config Configuration list
#' @return TRUE if valid, FALSE otherwise with warnings
#' @export
validate_tool_config <- function(tool_name, config = NULL) {
  if (is.null(config)) {
    config <- load_config()
  }

  # Handle both old and new config structure
  if (!is.null(config$external_tools)) {
    # New structure
    if (!tool_name %in% names(config$external_tools)) {
      warning(paste("Tool", tool_name, "not found in configuration"))
      return(FALSE)
    }
    tool_config <- config$external_tools[[tool_name]]
  } else {
    # Old structure (backward compatibility)
    if (!tool_name %in% names(config)) {
      warning(paste("Tool", tool_name, "not found in configuration"))
      return(FALSE)
    }
    tool_config <- config[[tool_name]]
  }

  # Check if enabled
  if (!isTRUE(tool_config$enabled)) {
    return(FALSE)
  }

  # Check if path exists
  tryCatch(
    {
      path <- get_tool_path(tool_name, config)
      if (!file.exists(path) && !nzchar(Sys.which(path))) {
        warning(paste("Tool", tool_name, "not found at path:", path))
        return(FALSE)
      }
    },
    error = function(e) {
      warning(paste("Error validating", tool_name, ":", e$message))
      return(FALSE)
    }
  )

  return(TRUE)
}

#' Save configuration to file
#'
#' @param config Configuration list
#' @param file_path Path to save configuration
#' @param format Format: "json" or "yaml"
#' @export
save_config <- function(config, file_path, format = "json") {
  format <- tolower(format)

  if (format == "json") {
    json_str <- jsonlite::toJSON(config, pretty = TRUE, auto_unbox = TRUE)
    writeLines(json_str, file_path)
  } else if (format %in% c("yaml", "yml")) {
    if (requireNamespace("yaml", quietly = TRUE)) {
      yaml::write_yaml(config, file_path)
    } else {
      stop("yaml package not installed. Cannot write YAML config file.")
    }
  } else {
    stop("Unsupported format. Use 'json' or 'yaml'.")
  }
}

#' Create example configuration file
#'
#' @param file_path Path to save example configuration
#' @param format Format: "json" or "yaml"
#' @export
create_example_config <- function(file_path = "qiprng_config.json", format = "json") {
  config <- get_default_config()

  # Add comments as metadata
  config$"_comments" <- list(
    description = "QIPRNG Complete Configuration (External Tools + Internal Tests)",
    version = "2.0.0",
    platform = get_platform(),
    note = "Adjust paths, parameters, and test settings according to your system and requirements",
    sections = list(
      external_tools = "Configuration for external testing tools (NIST STS, Dieharder, etc.)",
      internal_tests = "Configuration for internal test parameters and performance settings"
    )
  )

  save_config(config, file_path, format)
  message(paste("Example configuration saved to:", file_path))
}

#' Build command line for external tool
#'
#' @param tool_name Name of the tool
#' @param input_file Input data file path
#' @param output_file Output results file path
#' @param config Configuration list
#' @param extra_params Additional parameters
#' @return Command line string
#' @export
build_tool_command <- function(tool_name, input_file = NULL, output_file = NULL,
                               config = NULL, extra_params = list()) {
  if (is.null(config)) {
    config <- load_config()
  }

  tool_path <- get_tool_path(tool_name, config)

  # Get tool config from appropriate structure
  if (!is.null(config$external_tools)) {
    tool_config <- config$external_tools[[tool_name]]
  } else {
    tool_config <- config[[tool_name]]
  }

  params <- merge_configs(tool_config$parameters, extra_params)

  # Build command based on tool
  cmd <- switch(tool_name,
    dieharder = build_dieharder_command(tool_path, input_file, params),
    ent = build_ent_command(tool_path, input_file, params),
    nist_sts = build_nist_command(tool_path, input_file, output_file, params),
    testu01 = build_testu01_command(tool_path, input_file, output_file, params),
    practrand = build_practrand_command(tool_path, input_file, params),
    stop(paste("Unknown tool:", tool_name))
  )

  return(cmd)
}

#' Build Dieharder command
#' @keywords internal
build_dieharder_command <- function(path, input_file, params) {
  cmd <- path

  if (!is.null(input_file)) {
    cmd <- paste(cmd, "-g 201 -f", shQuote(input_file))
  }

  if (!is.null(params$test_ids)) {
    for (test_id in params$test_ids) {
      cmd <- paste(cmd, "-d", test_id)
    }
  }

  if (!is.null(params$psamples)) {
    cmd <- paste(cmd, "-p", params$psamples)
  }

  if (isTRUE(params$quiet)) {
    cmd <- paste(cmd, "-q")
  }

  return(cmd)
}

#' Build ENT command
#' @keywords internal
build_ent_command <- function(path, input_file, params) {
  cmd <- path

  if (isTRUE(params$binary_mode)) {
    cmd <- paste(cmd, "-b")
  }

  if (isTRUE(params$fold)) {
    cmd <- paste(cmd, "-f")
  }

  if (isTRUE(params$terse)) {
    cmd <- paste(cmd, "-t")
  }

  if (!is.null(input_file)) {
    cmd <- paste(cmd, shQuote(input_file))
  }

  return(cmd)
}

#' Build NIST STS command
#' @keywords internal
build_nist_command <- function(path, input_file, output_file, params) {
  # NIST STS typically requires a configuration file
  # This is a simplified version
  cmd <- paste(
    path,
    "-v", params$significance_level,
    "-n", params$bitstreams,
    "-l", params$stream_length
  )

  if (!is.null(input_file)) {
    cmd <- paste(cmd, "-i", shQuote(input_file))
  }

  if (!is.null(output_file)) {
    cmd <- paste(cmd, "-o", shQuote(output_file))
  }

  return(cmd)
}

#' Build TestU01 command
#' @keywords internal
build_testu01_command <- function(path, input_file, output_file, params) {
  cmd <- paste(path, "-", params$battery)

  if (!is.null(input_file)) {
    cmd <- paste(cmd, "-f", shQuote(input_file))
  }

  if (!is.null(output_file)) {
    cmd <- paste(cmd, ">", shQuote(output_file))
  }

  return(cmd)
}

#' Build PractRand command
#' @keywords internal
build_practrand_command <- function(path, input_file, params) {
  cmd <- paste(path, "stdin", params$length)

  if (!is.null(params$folding) && params$folding > 0) {
    cmd <- paste(cmd, "-tf", params$folding)
  }

  if (isTRUE(params$expanded)) {
    cmd <- paste(cmd, "-te")
  }

  if (isTRUE(params$multithreaded)) {
    cmd <- paste(cmd, "-multithreaded")
  }

  if (!is.null(input_file)) {
    cmd <- paste("cat", shQuote(input_file), "|", cmd)
  }

  return(cmd)
}

#' Get internal test parameter
#'
#' @param param_path Path to parameter (e.g., "global.significance_level" or "classical_tests.chi_square.bins")
#' @param config Configuration list (optional, will load defaults if NULL)
#' @param default Default value if parameter not found
#' @return Parameter value
#' @export
get_test_param <- function(param_path, config = NULL, default = NULL) {
  if (is.null(config)) {
    config <- load_config()
  }

  # Split the path
  path_parts <- strsplit(param_path, ".", fixed = TRUE)[[1]]

  # Navigate through the internal_tests config
  current <- config$internal_tests
  for (part in path_parts) {
    if (!is.null(current) && part %in% names(current)) {
      current <- current[[part]]
    } else {
      return(default)
    }
  }

  return(current)
}

#' Set internal test parameter
#'
#' @param param_path Path to parameter (e.g., "global.significance_level")
#' @param value New value for the parameter
#' @param config Configuration list
#' @return Updated configuration list
#' @export
set_test_param <- function(param_path, value, config = NULL) {
  if (is.null(config)) {
    config <- load_config()
  }

  # Split the path
  path_parts <- strsplit(param_path, ".", fixed = TRUE)[[1]]

  # Navigate and set the value
  if (length(path_parts) == 1) {
    config$internal_tests[[path_parts[1]]] <- value
  } else if (length(path_parts) == 2) {
    config$internal_tests[[path_parts[1]]][[path_parts[2]]] <- value
  } else if (length(path_parts) == 3) {
    config$internal_tests[[path_parts[1]]][[path_parts[2]]][[path_parts[3]]] <- value
  } else {
    # Handle deeper nesting
    current <- config$internal_tests
    for (i in 1:(length(path_parts) - 1)) {
      part <- path_parts[i]
      if (is.null(current[[part]])) {
        current[[part]] <- list()
      }
      if (i == length(path_parts) - 1) {
        current[[part]][[path_parts[i + 1]]] <- value
      } else {
        current <- current[[part]]
      }
    }
  }

  return(config)
}

#' Get test-specific configuration
#'
#' @param test_category Category of tests (e.g., "classical_tests", "binary_tests")
#' @param config Configuration list (optional)
#' @return Test category configuration
#' @export
get_test_config <- function(test_category, config = NULL) {
  if (is.null(config)) {
    config <- load_config()
  }

  if (test_category %in% names(config$internal_tests)) {
    return(config$internal_tests[[test_category]])
  } else {
    warning(paste("Test category", test_category, "not found in configuration"))
    return(list())
  }
}

#' Validate internal test configuration
#'
#' @param config Configuration list
#' @return TRUE if valid, FALSE otherwise with warnings
#' @export
validate_internal_config <- function(config = NULL) {
  if (is.null(config)) {
    config <- load_config()
  }

  valid <- TRUE

  # Check required global parameters
  required_global <- c("default_sample_size", "significance_level", "p_value_threshold")
  for (param in required_global) {
    if (is.null(config$internal_tests$global[[param]])) {
      warning(paste("Missing required global parameter:", param))
      valid <- FALSE
    }
  }

  # Validate numeric ranges
  if (!is.null(config$internal_tests$global$significance_level)) {
    sig_level <- config$internal_tests$global$significance_level
    if (sig_level <= 0 || sig_level >= 1) {
      warning("Significance level must be between 0 and 1")
      valid <- FALSE
    }
  }

  # Validate performance settings
  if (!is.null(config$internal_tests$performance$max_cores)) {
    max_cores <- config$internal_tests$performance$max_cores
    available_cores <- parallel::detectCores()
    if (max_cores > available_cores) {
      warning(paste("max_cores exceeds available cores. Setting to", available_cores - 1))
      config$internal_tests$performance$max_cores <- available_cores - 1
    }
  }

  return(valid)
}

#' Load environment variables for internal tests
#'
#' @param config Existing configuration
#' @return Updated configuration
#' @keywords internal
merge_internal_env_config <- function(config) {
  # Check for global parameter overrides
  env_vars <- list(
    "QIPRNG_SAMPLE_SIZE" = "global.default_sample_size",
    "QIPRNG_SIGNIFICANCE_LEVEL" = "global.significance_level",
    "QIPRNG_PARALLEL_ENABLED" = "performance.parallel_enabled",
    "QIPRNG_MAX_CORES" = "performance.max_cores",
    "QIPRNG_CACHE_ENABLED" = "performance.cache_enabled"
  )

  for (env_var in names(env_vars)) {
    value <- Sys.getenv(env_var)
    if (nzchar(value)) {
      param_path <- env_vars[[env_var]]

      # Convert to appropriate type
      if (grepl("enabled|ENABLED", env_var)) {
        value <- as.logical(value)
      } else if (grepl("size|SIZE|cores|CORES", env_var)) {
        value <- as.integer(value)
      } else if (grepl("level|LEVEL", env_var)) {
        value <- as.numeric(value)
      }

      config <- set_test_param(param_path, value, config)
    }
  }

  return(config)
}
