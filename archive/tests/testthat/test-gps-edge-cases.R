# tests/testthat/test-gps-edge-cases.R
# Tests for GPS processing edge cases and boundary conditions

library(testthat)
library(dplyr)
library(lubridate)
library(geosphere)

# Set working directory for tests
if (!file.exists("scripts/r/gps_processing.R")) {
  setwd(here::here())
}

# Source the functions being tested
source("scripts/r/gps_processing.R")
source("tests/testthat/helper-test-data.R")

test_that("process_gps handles missing required columns gracefully", {
  # Missing 'time' column
  incomplete_data <- tibble(
    subid = 1,
    lat = 43.074713,
    lon = -89.384373
  )
  
  expect_error(process_gps(incomplete_data), "time")
  
  # Missing 'lat' column
  incomplete_data2 <- tibble(
    subid = 1,
    lon = -89.384373,
    time = "2023-01-01T08:00:00Z"
  )
  
  expect_error(process_gps(incomplete_data2), "lat")
})

test_that("process_gps handles extreme coordinate values", {
  # Test boundary coordinates (poles and date line)
  boundary_data <- tibble(
    subid = rep(1, 4),
    lat = c(90, -90, 0, 0),
    lon = c(0, 0, 180, -180),
    time = c(
      "2023-01-01T08:00:00Z",
      "2023-01-01T08:10:00Z",
      "2023-01-01T08:20:00Z", 
      "2023-01-01T08:30:00Z"
    )
  )
  
  result <- process_gps(boundary_data)
  
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 4)
  expect_true(all(result$lat >= -90 & result$lat <= 90))
  expect_true(all(result$lon >= -180 & result$lon <= 180))
})

test_that("process_gps handles large time gaps", {
  # Create data with very large time gaps (days apart)
  large_gap_data <- tibble(
    subid = rep(1, 3),
    lat = rep(43.074713, 3),
    lon = rep(-89.384373, 3),
    time = c(
      "2023-01-01T08:00:00Z",
      "2023-01-15T08:00:00Z",  # 14 days later
      "2023-02-01T08:00:00Z"   # 17 days later
    )
  )
  
  result <- process_gps(large_gap_data)
  
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 3)
  
  # Check that large time gaps are handled (durations should be very large)
  expect_true(any(result$duration > 1000, na.rm = TRUE))  # > 1000 minutes
})

test_that("process_gps handles rapid consecutive points", {
  # Create data with points recorded seconds apart
  rapid_points <- tibble(
    subid = rep(1, 5),
    lat = c(43.074, 43.0741, 43.0742, 43.0743, 43.0744),
    lon = rep(-89.384373, 5),
    time = c(
      "2023-01-01T08:00:00Z",
      "2023-01-01T08:00:05Z",  # 5 seconds later
      "2023-01-01T08:00:10Z",  # 5 seconds later
      "2023-01-01T08:00:15Z",  # 5 seconds later  
      "2023-01-01T08:00:20Z"   # 5 seconds later
    )
  )
  
  result <- process_gps(rapid_points)
  
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 5)
  
  # Durations should be very small (< 1 minute)
  non_na_durations <- result$duration[!is.na(result$duration)]
  expect_true(all(non_na_durations < 1))
})

test_that("process_gps handles mixed timezone formats", {
  mixed_tz_data <- tibble(
    subid = rep(1, 4),
    lat = rep(43.074713, 4),
    lon = rep(-89.384373, 4),
    time = c(
      "2023-01-01T08:00:00Z",           # UTC
      "2023-01-01T08:10:00+00:00",      # UTC explicit
      "2023-01-01T02:20:00-06:00",      # Central Standard Time 
      "2023-01-01T14:30:00+00:00"       # UTC explicit
    )
  )
  
  result <- process_gps(mixed_tz_data)
  
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 4)
  
  # All times should be converted to consistent timezone
  expect_s3_class(result$dttm_obs, "POSIXct")
  expect_true(all(!is.na(result$dttm_obs)))
})

test_that("process_gps handles zero distances correctly", {
  # Create data with identical coordinates
  zero_distance_data <- tibble(
    subid = rep(1, 3),
    lat = rep(43.074713, 3),  # Exact same coordinates
    lon = rep(-89.384373, 3),
    time = c(
      "2023-01-01T08:00:00Z",
      "2023-01-01T08:10:00Z",
      "2023-01-01T08:20:00Z"
    )
  )
  
  result <- process_gps(zero_distance_data)
  
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 3)
  
  # Distance should be 0 for identical coordinates
  expect_true(all(result$dist[2:3] == 0, na.rm = TRUE))
  expect_true(all(result$speed[2:3] == 0, na.rm = TRUE))
})

test_that("distHaversine calculations are consistent", {
  # Test known distance calculation
  p1 <- c(-89.384373, 43.074713)  # Madison Capitol (lon, lat)
  p2 <- c(-89.412476, 43.076592)  # UW Memorial Union (lon, lat)
  
  distance <- distHaversine(p1, p2)
  
  # Should be approximately 2.5 km (2500 meters)
  expect_gt(distance, 2000)
  expect_lt(distance, 3000)
  
  # Test symmetry (distance from A to B equals B to A)
  reverse_distance <- distHaversine(p2, p1)
  expect_equal(distance, reverse_distance, tolerance = 1e-10)
})

test_that("speed calculations handle edge cases", {
  # Test division by zero protection
  zero_time_data <- tibble(
    subid = rep(1, 2),
    lat = c(43.074, 43.075),
    lon = c(-89.384, -89.385),
    time = rep("2023-01-01T08:00:00Z", 2)  # Same timestamp
  )
  
  result <- process_gps(zero_time_data)
  
  # Should not produce infinite speeds
  expect_true(all(is.finite(result$speed) | is.na(result$speed)))
  expect_false(any(is.infinite(result$speed), na.rm = TRUE))
})

test_that("process_gps handles data type inconsistencies", {
  # Test with numeric subid as character
  type_mismatch_data <- tibble(
    subid = c("1", "1", "1"),  # Character instead of numeric
    lat = c(43.074, 43.075, 43.076),
    lon = c(-89.384, -89.385, -89.386),
    time = c(
      "2023-01-01T08:00:00Z",
      "2023-01-01T08:10:00Z",
      "2023-01-01T08:20:00Z"
    )
  )
  
  # Should handle character subids gracefully
  expect_no_error({
    result <- process_gps(type_mismatch_data)
    expect_s3_class(result, "tbl_df")
  })
})

test_that("movement state classification handles boundary values", {
  # Test exactly at threshold
  threshold_data <- tibble(
    subid = rep(1, 3),
    lat = c(43.074, 43.075, 43.076),
    lon = c(-89.384, -89.384, -89.384),
    time = c(
      "2023-01-01T08:00:00Z",
      "2023-01-01T08:15:00Z",  # 15 minutes = exactly 4 mph for ~1 mile
      "2023-01-01T08:30:00Z"
    )
  )
  
  result <- process_gps(threshold_data, stationary_threshold_mph = 4)
  
  expect_s3_class(result, "tbl_df")
  expect_true(all(result$movement_state %in% c("stationary", "transition")))
  
  # Points at or below threshold should be stationary
  below_threshold <- result$speed <= 4 & !is.na(result$speed)
  if (any(below_threshold)) {
    expect_true(all(result$movement_state[below_threshold] == "stationary"))
  }
})

test_that("get_stationary handles empty movement_state column", {
  # Data without movement_state classification
  incomplete_data <- tibble(
    subid = rep(1, 3),
    lat = c(43.074, 43.075, 43.076),
    lon = c(-89.384, -89.385, -89.386),
    dttm_obs = seq(from = as.POSIXct("2023-01-01 08:00:00"), by = "10 mins", length.out = 3),
    dist = c(NA, 1, 1),
    duration = c(NA, 10, 10),
    speed = c(NA, 6, 6)
    # Missing movement_state column
  )
  
  expect_error(get_stationary(incomplete_data), "movement_state")
})