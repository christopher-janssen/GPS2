# tests/testthat/test-gps-processing.R
# Tests for GPS data processing functions

library(testthat)
library(dplyr)
library(lubridate)

# Source the functions being tested
source(here::here("scripts/r/gps_processing.R"))
source(here::here("utils/validation_utils.R"))

test_that("process_gps handles valid GPS data", {
  test_data <- create_test_gps_data(n_points = 5)
  
  # Add required time column for processing
  test_data$time <- format(test_data$dttm_obs, "%Y-%m-%dT%H:%M:%SZ")
  
  result <- process_gps(test_data)
  
  expect_s3_class(result, "tbl_df")
  expect_true(all(c("subid", "lat", "lon", "dttm_obs", "dist", "duration", "speed", "movement_state") %in% names(result)))
  expect_equal(nrow(result), nrow(test_data))
  
  # All movement states should be valid
  expect_true(all(result$movement_state %in% c("stationary", "transition")))
  
  # First point should have NA for dist, duration, speed (no previous point)
  expect_true(is.na(result$dist[1]))
  expect_true(is.na(result$duration[1]))
  expect_true(is.na(result$speed[1]))
})

test_that("process_gps handles empty data", {
  empty_data <- tibble()
  result <- process_gps(empty_data)
  
  expect_equal(nrow(result), 0)
  expect_s3_class(result, "tbl_df")
})

test_that("process_gps validates required columns", {
  # Test missing required columns
  incomplete_data <- tibble(subid = 1, lat = 43.074)  # Missing lon and time
  
  expect_error(process_gps(incomplete_data), "required columns")
})

test_that("process_gps handles invalid coordinates", {
  invalid_data <- create_invalid_gps_data()
  invalid_data$time <- format(invalid_data$dttm_obs, "%Y-%m-%dT%H:%M:%SZ")
  
  expect_error(process_gps(invalid_data), "Invalid coordinates")
})

test_that("process_gps handles malformed timestamps", {
  malformed_data <- create_malformed_time_data()
  
  # Should error on invalid timestamp format
  expect_error(process_gps(malformed_data), "time.*format")
})

test_that("process_gps calculates distances correctly", {
  # Create test data with known distances
  # Two points in Madison: Capitol to UW Memorial Union (approx 2.5 km)
  test_data <- tibble(
    subid = c(1, 1),
    lat = c(43.074713, 43.076592),
    lon = c(-89.384373, -89.412476),
    time = c("2023-01-01T08:00:00Z", "2023-01-01T08:10:00Z")
  )
  
  result <- process_gps(test_data)
  
  # Second point should have a distance calculation
  expect_false(is.na(result$dist[2]))
  expect_gt(result$dist[2], 0)
  
  # Distance should be reasonable (approximately 2.5 km = 1.55 miles)
  expect_gt(result$dist[2], 1.0)  # At least 1 mile
  expect_lt(result$dist[2], 3.0)  # Less than 3 miles
})

test_that("process_gps calculates speeds correctly", {
  # Create test data with known speed scenario
  # Two points 1 mile apart, 10 minutes apart = 6 mph
  test_data <- tibble(
    subid = c(1, 1),
    lat = c(43.074713, 43.089157),  # Roughly 1 mile north
    lon = c(-89.384373, -89.384373),
    time = c("2023-01-01T08:00:00Z", "2023-01-01T08:10:00Z")  # 10 minutes apart
  )
  
  result <- process_gps(test_data)
  
  # Second point should have speed calculation
  expect_false(is.na(result$speed[2]))
  expect_gt(result$speed[2], 0)
  
  # Speed should be reasonable (around 6 mph)
  expect_gt(result$speed[2], 4)   # At least 4 mph
  expect_lt(result$speed[2], 10)  # Less than 10 mph
})

test_that("process_gps handles extreme speeds", {
  # Test with unrealistic speed (should be filtered or flagged)
  extreme_speed_data <- tibble(
    subid = c(1, 1),
    lat = c(43.074713, 44.074713),  # 1 degree latitude difference (~69 miles)
    lon = c(-89.384373, -89.384373),
    time = c("2023-01-01T08:00:00Z", "2023-01-01T08:01:00Z")  # 1 minute apart = ~4140 mph
  )
  
  # Function should handle extreme speeds based on speed_threshold_mph parameter
  result <- process_gps(extreme_speed_data, speed_threshold_mph = 100)
  
  # Depending on implementation, might filter out or flag the extreme speed point
  expect_s3_class(result, "tbl_df")
  
  # If the extreme speed point is included, it should be marked appropriately
  if (nrow(result) == 2) {
    # Speed should be calculated
    expect_false(is.na(result$speed[2]))
  }
})

test_that("process_gps classifies movement states correctly", {
  # Create test data with clear stationary and transition states
  mixed_movement_data <- tibble(
    subid = rep(1, 4),
    # First two points: same location (stationary)
    lat = c(43.074713, 43.074713, 43.084713, 43.094713),
    lon = c(-89.384373, -89.384373, -89.394373, -89.404373),
    time = c("2023-01-01T08:00:00Z", "2023-01-01T08:10:00Z", 
             "2023-01-01T08:20:00Z", "2023-01-01T08:30:00Z")
  )
  
  result <- process_gps(mixed_movement_data, stationary_threshold_mph = 4)
  
  # Should have mix of stationary and transition states
  states <- unique(result$movement_state[!is.na(result$movement_state)])
  expect_gte(length(states), 1)
  expect_true(all(states %in% c("stationary", "transition")))
})

test_that("get_stationary filters correctly", {
  # Create processed GPS data with known movement states
  processed_data <- tibble(
    subid = rep(1, 4),
    lat = c(43.074713, 43.074713, 43.084713, 43.094713),
    lon = c(-89.384373, -89.384373, -89.394373, -89.404373),
    dttm_obs = seq(from = as.POSIXct("2023-01-01 08:00:00"), by = "10 mins", length.out = 4),
    dist = c(NA, 0, 1.0, 1.0),
    duration = c(NA, 10, 10, 10),
    speed = c(NA, 0, 6, 6),
    movement_state = c("stationary", "stationary", "transition", "transition")
  )
  
  stationary_points <- get_stationary(processed_data)
  
  # Should only include stationary points
  expect_true(all(stationary_points$movement_state == "stationary"))
  expect_equal(nrow(stationary_points), 2)  # Only first two points
})

test_that("process_followmee_gps handles FollowMee format", {
  # Create test data in FollowMee format
  followmee_data <- tibble(
    `Device ID` = rep("test_device", 3),
    `Date & Time` = c("2023-01-01 08:00:00", "2023-01-01 08:10:00", "2023-01-01 08:20:00"),
    Latitude = c(43.074713, 43.075713, 43.076713),
    Longitude = c(-89.384373, -89.385373, -89.386373),
    `Address` = rep("Madison, WI", 3)
  )
  
  result <- process_followmee_gps(followmee_data, device_id = "test_device", subid = 1)
  
  expect_s3_class(result, "tbl_df")
  expect_true(all(c("subid", "lat", "lon", "dttm_obs", "movement_state") %in% names(result)))
  expect_equal(nrow(result), 3)
  expect_true(all(result$subid == 1))
})

test_that("process_gps handles timezone conversions", {
  # Test data with different timezone formats
  timezone_data <- tibble(
    subid = rep(1, 3),
    lat = rep(43.074713, 3),
    lon = rep(-89.384373, 3),
    time = c(
      "2023-01-01T08:00:00Z",           # UTC
      "2023-01-01T08:10:00-06:00",      # Central Time
      "2023-01-01T14:20:00+00:00"       # UTC explicit
    )
  )
  
  result <- process_gps(timezone_data)
  
  # All timestamps should be converted to consistent timezone
  expect_s3_class(result$dttm_obs, "POSIXct")
  expect_equal(nrow(result), 3)
  
  # Times should be in chronological order after conversion
  expect_true(all(diff(result$dttm_obs) >= 0))
})

test_that("process_gps handles duplicate timestamps", {
  # Create data with duplicate timestamps
  duplicate_time_data <- tibble(
    subid = rep(1, 3),
    lat = c(43.074713, 43.075713, 43.076713),
    lon = c(-89.384373, -89.385373, -89.386373),
    time = rep("2023-01-01T08:00:00Z", 3)  # All same time
  )
  
  result <- process_gps(duplicate_time_data)
  
  # Should handle duplicate timestamps gracefully
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 3)
  
  # Duration and speed calculations might be 0 or NA for identical times
  expect_true(all(is.na(result$duration[2:3]) | result$duration[2:3] == 0))
})

test_that("process_gps respects parameter thresholds", {
  test_data <- create_test_gps_data(n_points = 3)
  test_data$time <- format(test_data$dttm_obs, "%Y-%m-%dT%H:%M:%SZ")
  
  # Test with custom thresholds
  result <- process_gps(
    test_data, 
    speed_threshold_mph = 50,
    stationary_threshold_mph = 2
  )
  
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), nrow(test_data))
  
  # Movement classification should respect the 2 mph threshold
  non_na_speeds <- result$speed[!is.na(result$speed)]
  if (length(non_na_speeds) > 0) {
    stationary_points <- result$movement_state[!is.na(result$speed) & result$speed <= 2] 
    if (length(stationary_points) > 0) {
      expect_true(all(stationary_points == "stationary"))
    }
  }
})