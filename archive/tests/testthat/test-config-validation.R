# tests/testthat/test-config-validation.R
# Tests for configuration system edge cases and validation

library(testthat)

# Set working directory for tests
if (!file.exists("config/gps2_config.R")) {
  setwd(here::here())
}

# Source the configuration
source("config/gps2_config.R")

test_that("get_config handles invalid key paths", {
  # Test non-existent top-level keys
  expect_null(get_config("nonexistent"))
  expect_null(get_config("nonexistent", "subkey"))
  
  # Test non-existent nested keys
  expect_null(get_config("clustering", "nonexistent"))
  expect_null(get_config("database", "invalid_key"))
})

test_that("get_config handles empty and NULL inputs", {
  # Test with no arguments
  expect_identical(get_config(), GPS2_CONFIG)
  
  # Test with NULL
  expect_null(get_config(NULL))
  
  # Test with empty string
  expect_null(get_config(""))
})

test_that("config values are within expected ranges", {
  # Clustering parameters
  eps <- get_config("clustering", "default_eps")
  expect_gt(eps, 0)
  expect_lt(eps, 10000)  # Reasonable maximum for GPS clustering
  
  min_duration <- get_config("clustering", "min_duration_min")
  expect_gt(min_duration, 0)
  expect_lt(min_duration, 1440)  # Less than 24 hours
  
  # Speed thresholds
  speed_threshold <- get_config("filtering", "speed_threshold_mph")
  expect_gt(speed_threshold, 0)
  expect_lt(speed_threshold, 1000)  # Reasonable maximum
  
  stationary_threshold <- get_config("filtering", "stationary_threshold_mph")
  expect_gt(stationary_threshold, 0)
  expect_lt(stationary_threshold, speed_threshold)  # Should be less than max speed
  
  # Database parameters
  batch_size <- get_config("database", "batch_size")
  expect_gt(batch_size, 0)
  expect_lt(batch_size, 100000)  # Reasonable batch size limit
  
  timeout <- get_config("database", "connection_timeout")
  expect_gt(timeout, 0)
  expect_lt(timeout, 300)  # 5 minutes max
})

test_that("config structure is consistent", {
  # Check that all expected top-level keys exist
  expected_keys <- c("clustering", "filtering", "database", "geocoding", "visualization")
  actual_keys <- names(GPS2_CONFIG)
  
  expect_true(all(expected_keys %in% actual_keys))
  
  # Check that all values are lists (for nested config)
  expect_true(all(sapply(GPS2_CONFIG, is.list)))
})

test_that("geocoding config is valid", {
  nominatim_url <- get_config("geocoding", "nominatim_url")
  expect_true(grepl("^https?://", nominatim_url))
  
  delay <- get_config("geocoding", "delay_seconds")
  expect_gte(delay, 0)
  expect_lte(delay, 10)  # Reasonable delay limit
  
  max_retries <- get_config("geocoding", "max_retries")
  expect_gte(max_retries, 0)
  expect_lte(max_retries, 10)
})

test_that("visualization config is valid", {
  zoom <- get_config("visualization", "default_zoom")
  expect_gte(zoom, 1)
  expect_lte(zoom, 20)  # Valid zoom range for web maps
  
  popup_length <- get_config("visualization", "max_popup_length")
  expect_gt(popup_length, 0)
  expect_lt(popup_length, 10000)
  
  legend_position <- get_config("visualization", "legend_position")
  valid_positions <- c("topright", "topleft", "bottomright", "bottomleft")
  expect_true(legend_position %in% valid_positions)
})

test_that("config modifications don't persist", {
  # Test that modifying the returned config doesn't affect original
  original_eps <- get_config("clustering", "default_eps")
  
  # Get config and modify it
  config_copy <- get_config("clustering")
  config_copy$default_eps <- 999
  
  # Original should be unchanged
  expect_equal(get_config("clustering", "default_eps"), original_eps)
  expect_not_equal(get_config("clustering", "default_eps"), 999)
})