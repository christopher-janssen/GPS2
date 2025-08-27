# tests/testthat/test-validation-edge-cases.R
# Tests for validation utility edge cases and security

library(testthat)
library(dplyr)

# Set working directory for tests
if (!file.exists("scripts/utils/validation_utils.R")) {
  setwd(here::here())
}

# Source the functions being tested
source("scripts/utils/validation_utils.R")

test_that("validate_gps_data handles malicious input", {
  # SQL injection attempts in column names (hypothetical attack)
  malicious_data <- tibble(
    subid = 1,
    lat = 43.074,
    lon = -89.384
  )
  
  # Add column with malicious name
  malicious_data[["'; DROP TABLE users; --"]] <- "malicious"
  
  # Should still validate normally (focusing on required columns)
  expect_true(validate_gps_data(malicious_data))
})

test_that("validate_gps_data handles extreme coordinate precision", {
  # Very high precision coordinates (15+ decimal places)
  high_precision_data <- tibble(
    subid = 1,
    lat = 43.074713123456789,
    lon = -89.384373987654321
  )
  
  expect_true(validate_gps_data(high_precision_data))
})

test_that("validate_gps_data handles coordinate boundary values", {
  # Exact boundary values
  boundary_data <- tibble(
    subid = c(1, 2, 3, 4),
    lat = c(90, -90, 0, 0),
    lon = c(180, -180, 0, 0)
  )
  
  expect_true(validate_gps_data(boundary_data))
  
  # Just outside boundaries
  invalid_boundary_data <- tibble(
    subid = c(1, 2, 3, 4),
    lat = c(90.1, -90.1, 0, 0),
    lon = c(180.1, -180.1, 0, 0)
  )
  
  expect_error(validate_gps_data(invalid_boundary_data), "Invalid coordinates")
})

test_that("validate_gps_data handles missing values strategically", {
  # Missing essential coordinates
  missing_coords <- tibble(
    subid = c(1, 1, 1),
    lat = c(43.074, NA, 43.076),
    lon = c(-89.384, -89.385, NA)
  )
  
  expect_error(validate_gps_data(missing_coords), "Invalid coordinates")
  
  # All coordinates missing
  all_missing <- tibble(
    subid = c(1, 1, 1),
    lat = c(NA, NA, NA),
    lon = c(NA, NA, NA)
  )
  
  expect_error(validate_gps_data(all_missing), "Invalid coordinates")
})

test_that("validate_participant_exists handles edge cases", {
  # Empty dataset
  empty_data <- tibble()
  expect_error(validate_participant_exists(empty_data, 1), "subid.*column")
  
  # Dataset without subid column
  no_subid_data <- tibble(lat = 43.074, lon = -89.384)
  expect_error(validate_participant_exists(no_subid_data, 1), "subid.*column")
  
  # Dataset with subid but wrong type
  wrong_type_data <- tibble(
    subid = c("1", "2", "3"),  # Character instead of numeric
    lat = c(43.074, 43.075, 43.076),
    lon = c(-89.384, -89.385, -89.386)
  )
  
  # Should handle character subids that can be coerced
  result <- validate_participant_exists(wrong_type_data, "1")
  expect_s3_class(result, "tbl_df")
})

test_that("validate_clustering_params handles boundary conditions", {
  # Exact boundary values
  expect_true(validate_clustering_params(eps = 1))  # Minimum meaningful eps
  expect_true(validate_clustering_params(eps = 10000))  # Maximum allowed eps
  
  # Just outside boundaries
  expect_error(validate_clustering_params(eps = 0), "eps.*positive")
  expect_error(validate_clustering_params(eps = 10001), "eps.*10000")
  
  # Non-numeric inputs
  expect_error(validate_clustering_params(eps = "50"), "numeric")
  expect_error(validate_clustering_params(eps = NULL), "numeric")
  expect_error(validate_clustering_params(eps = c(50, 100)), "numeric")  # Vector instead of scalar
})

test_that("validate_participant_id handles malicious inputs", {
  # SQL injection attempts
  expect_error(validate_participant_id("1; DROP TABLE users;"), "numeric")
  expect_error(validate_participant_id("' OR 1=1 --"), "numeric")
  
  # Script injection attempts
  expect_error(validate_participant_id("<script>alert('xss')</script>"), "numeric")
  
  # Large numbers (potential overflow)
  expect_error(validate_participant_id(.Machine$double.xmax), "Expected numeric but got")
  
  # Negative numbers
  expect_error(validate_participant_id(-1), "positive")
  expect_error(validate_participant_id(0), "positive")
})

test_that("create_empty_cluster_result maintains schema consistency", {
  empty_result <- create_empty_cluster_result()
  
  # Should have all expected columns
  expected_cols <- c("subid", "lat", "lon", "n_points", "first_visit", 
                    "last_visit", "cluster", "total_visits", "total_duration_hours", 
                    "unique_days")
  
  expect_true(all(expected_cols %in% names(empty_result)))
  expect_equal(nrow(empty_result), 0)
  
  # Column types should be correct
  expect_true(is.numeric(empty_result$subid))
  expect_true(is.numeric(empty_result$lat))
  expect_true(is.numeric(empty_result$lon))
  expect_true(is.integer(empty_result$n_points))
  expect_s3_class(empty_result$first_visit, "POSIXct")
  expect_s3_class(empty_result$last_visit, "POSIXct")
})

test_that("validation functions handle special numeric values", {
  # Test with Inf, -Inf, NaN
  special_coords <- tibble(
    subid = c(1, 2, 3, 4),
    lat = c(Inf, -Inf, NaN, 43.074),
    lon = c(-89.384, -89.384, -89.384, NaN)
  )
  
  expect_error(validate_gps_data(special_coords), "Invalid coordinates")
  
  # Test clustering params with special values
  expect_error(validate_clustering_params(eps = Inf), "numeric")
  expect_error(validate_clustering_params(eps = NaN), "numeric")
  expect_error(validate_clustering_params(eps = -Inf), "positive")
})

test_that("validation handles memory-intensive attacks", {
  # Very long string inputs (potential DoS)
  long_string_data <- tibble(
    subid = 1,
    lat = 43.074,
    lon = -89.384
  )
  
  # Add a column with extremely long string
  long_string_data$malicious_column <- paste(rep("A", 10000), collapse = "")
  
  # Should still validate required columns efficiently
  expect_true(validate_gps_data(long_string_data))
})

test_that("coordinate validation handles floating point precision", {
  # Coordinates that are very close to boundaries
  near_boundary_data <- tibble(
    subid = 1,
    lat = 90 - .Machine$double.eps,  # Just under 90
    lon = 180 - .Machine$double.eps  # Just under 180
  )
  
  expect_true(validate_gps_data(near_boundary_data))
  
  # Coordinates that exceed due to floating point errors
  float_error_data <- tibble(
    subid = 1,
    lat = 90 + .Machine$double.eps,  # Just over 90
    lon = 180 + .Machine$double.eps  # Just over 180  
  )
  
  # Should still be considered invalid (strict boundary checking)
  expect_error(validate_gps_data(float_error_data), "Invalid coordinates")
})

test_that("validation functions are resistant to timing attacks", {
  # Test that validation time doesn't leak information about data content
  valid_data <- tibble(subid = 1, lat = 43.074, lon = -89.384)
  invalid_data <- tibble(subid = 1, lat = 200, lon = -89.384)
  
  # Both should complete quickly (timing attack resistance)
  start_time <- Sys.time()
  expect_true(validate_gps_data(valid_data))
  valid_time <- Sys.time()
  
  start_time2 <- Sys.time()  
  expect_error(validate_gps_data(invalid_data))
  invalid_time <- Sys.time()
  
  # Time difference should be minimal (< 0.1 seconds)
  time_diff <- abs(as.numeric(difftime(valid_time, start_time)) - 
                   as.numeric(difftime(invalid_time, start_time2)))
  expect_lt(time_diff, 0.1)
})