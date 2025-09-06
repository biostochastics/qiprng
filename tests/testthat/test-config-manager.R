# Test file for configuration management
library(testthat)
library(qiprng)

# Source the config manager - try multiple locations
config_manager_path <- system.file("R/statisticaltests/config_manager.R", package = "qiprng")
if (!file.exists(config_manager_path) || config_manager_path == "") {
  # Try relative path for development
  if (file.exists("../../R/statisticaltests/config_manager.R")) {
    config_manager_path <- "../../R/statisticaltests/config_manager.R"
  } else if (file.exists("R/statisticaltests/config_manager.R")) {
    config_manager_path <- "R/statisticaltests/config_manager.R"
  } else {
    skip("config_manager.R not found")
  }
}
source(config_manager_path)

test_that("Configuration manager loads default config", {
  config <- load_config()

  expect_type(config, "list")
  expect_true("external_tools" %in% names(config))
  expect_true("internal_tests" %in% names(config))

  # Check external tools
  expect_true("dieharder" %in% names(config$external_tools))
  expect_true("ent" %in% names(config$external_tools))
  expect_true("nist_sts" %in% names(config$external_tools))
})

test_that("Platform detection works correctly", {
  platform <- get_platform()

  expect_true(platform %in% c("linux", "macos", "windows"))
})

test_that("Tool path resolution works", {
  config <- load_config()

  # Test with a known tool - use the correct nested structure
  if (validate_tool_config("ent", config$external_tools)) {
    path <- get_tool_path("ent", config$external_tools)
    expect_type(path, "character")
    expect_true(nzchar(path))
  } else {
    skip("ENT tool not configured or available")
  }
})

test_that("Configuration merging works correctly", {
  base_config <- list(
    tool1 = list(enabled = TRUE, param1 = "value1"),
    tool2 = list(enabled = FALSE)
  )

  override_config <- list(
    tool1 = list(param1 = "new_value"),
    tool3 = list(enabled = TRUE)
  )

  merged <- merge_configs(base_config, override_config)

  expect_equal(merged$tool1$param1, "new_value")
  expect_true(merged$tool1$enabled)
  expect_false(merged$tool2$enabled)
  expect_true(merged$tool3$enabled)
})

test_that("Environment variable override works", {
  # Set test environment variable
  old_env <- Sys.getenv("QIPRNG_ENT_PATH")
  Sys.setenv(QIPRNG_ENT_PATH = "/custom/path/to/ent")

  config <- load_config()
  platform <- get_platform()

  # Check in the correct nested structure
  expect_equal(config$external_tools$ent$paths[[platform]], "/custom/path/to/ent")

  # Restore environment
  if (nzchar(old_env)) {
    Sys.setenv(QIPRNG_ENT_PATH = old_env)
  } else {
    Sys.unsetenv("QIPRNG_ENT_PATH")
  }
})

test_that("Command building works correctly", {
  config <- load_config()

  # Test Dieharder command - pass the external_tools config
  cmd <- build_tool_command("dieharder", "input.dat", config = config$external_tools)
  expect_type(cmd, "character")
  expect_match(cmd, "dieharder")
  expect_match(cmd, "input.dat")

  # Test ENT command
  cmd <- build_tool_command("ent", "input.dat", config = config$external_tools)
  expect_type(cmd, "character")
  expect_match(cmd, "ent")
  expect_match(cmd, "input.dat")
})

test_that("Configuration can be saved and loaded", {
  temp_file <- tempfile(fileext = ".json")

  # Create and save config
  config <- list(
    test_tool = list(
      enabled = TRUE,
      paths = list(default = "/usr/bin/test"),
      parameters = list(param1 = "value1")
    )
  )

  save_config(config, temp_file, "json")
  expect_true(file.exists(temp_file))

  # Load config
  loaded_config <- load_config(temp_file, merge_defaults = FALSE)
  expect_equal(loaded_config$test_tool$enabled, TRUE)
  expect_equal(loaded_config$test_tool$parameters$param1, "value1")

  unlink(temp_file)
})

test_that("Example configuration file can be created", {
  temp_file <- tempfile(fileext = ".json")

  create_example_config(temp_file, "json")
  expect_true(file.exists(temp_file))

  # Load and check structure
  config <- jsonlite::fromJSON(temp_file, simplifyVector = FALSE)
  # The example config might have different structure
  # Check for either old or new format
  if ("external_tools" %in% names(config)) {
    expect_true("dieharder" %in% names(config$external_tools))
  } else {
    # Might be old format with tools at root level
    expect_true("dieharder" %in% names(config) || "_comments" %in% names(config))
  }

  unlink(temp_file)
})
