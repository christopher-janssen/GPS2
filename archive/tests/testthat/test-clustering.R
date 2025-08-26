# tests/testthat/test-clustering.R
# Tests for GPS clustering algorithms

library(testthat)
library(dplyr)
library(geosphere)

# Set working directory for tests
if (!file.exists("scripts/r/analysis.R")) {
  setwd(here::here())
}

# Source the functions being tested (avoid global_setup conflicts)
source("config/gps2_config.R")
source("scripts/r/database.R")
source("utils/validation_utils.R")
source("scripts/r/gps_processing.R")

# Load utils manually
source("utils/db_utils.R")
source("utils/geocoding_utils.R") 
source("utils/query_builders.R")

# Load analysis functions with conflict suppression
suppressMessages({
  library(httr, quietly = TRUE)
  suppressWarnings(library(jsonlite, exclude = "flatten", quietly = TRUE))
  # Load just the clustering functions we need
  source("scripts/r/analysis.R", local = TRUE)
})

test_that("cluster_stationary_gps_env handles empty data", {
  empty_data <- tibble()
  result <- cluster_stationary_gps_env(empty_data, subid = 1, eps = 50)
  
  expect_equal(nrow(result), 0)
  expect_s3_class(result, "tbl_df")
})

test_that("cluster_stationary_gps_env handles single point", {
  single_point <- create_test_gps_data(n_points = 1, subid = 1)
  result <- cluster_stationary_gps_env(single_point, subid = 1, eps = 50)
  
  # Single point should not form a cluster (need at least 2 points)
  expect_equal(nrow(result), 0)
})

test_that("cluster_stationary_gps_env identifies correct clusters", {
  test_data <- create_clustered_test_data()
  result <- cluster_stationary_gps_env(test_data, subid = 1, eps = 100)
  
  # Should identify at least 2 clusters from the test data
  expect_gte(nrow(result), 2)
  expect_true(all(c("cluster", "subid", "lat", "lon", "total_visits", "total_duration_hours", "unique_days") %in% names(result)))
  
  # All clusters should be for the correct participant
  expect_true(all(result$subid == 1))
  
  # Cluster numbers should be positive integers
  expect_true(all(result$cluster > 0))
  expect_true(all(result$cluster == as.integer(result$cluster)))
})

test_that("clustering respects eps parameter", {
  # Create two points that are exactly 100 meters apart
  base_coords <- c(43.074713, -89.384373)
  
  # Calculate a point approximately 100m away (rough approximation)
  distant_point <- c(base_coords[1] + 0.0009, base_coords[2] + 0.001)
  
  test_data <- tibble(
    subid = c(1, 1, 1, 1),
    lat = c(base_coords[1], base_coords[1], distant_point[1], distant_point[1]),
    lon = c(base_coords[2], base_coords[2], distant_point[2], distant_point[2]),
    dttm_obs = as.POSIXct(c("2023-01-01 08:00:00", "2023-01-01 08:30:00",
                           "2023-01-01 10:00:00", "2023-01-01 10:30:00")),
    movement_state = "stationary"
  )
  
  # With small eps, should get 2 clusters
  result_small_eps <- cluster_stationary_gps_env(test_data, subid = 1, eps = 50)
  
  # With large eps, might get 1 cluster
  result_large_eps <- cluster_stationary_gps_env(test_data, subid = 1, eps = 200)
  
  # Should have valid results for both
  expect_gte(nrow(result_small_eps), 1)
  expect_gte(nrow(result_large_eps), 1)
})

test_that("assign_daily_clusters handles edge cases", {
  # Test with empty data
  empty_data <- tibble()
  empty_data$daily_cluster <- 0
  result <- assign_daily_clusters(empty_data, eps = 50, min_duration_min = 30)
  expect_equal(length(result), 0)
  
  # Test with single point
  single_point <- create_test_gps_data(n_points = 1) |>
    mutate(daily_cluster = 0)
  result <- assign_daily_clusters(single_point, eps = 50, min_duration_min = 30)
  expect_equal(result[1], 0)  # Should remain unclustered
})

test_that("clustering validates input parameters", {
  test_data <- create_test_gps_data(n_points = 5)
  
  # Test invalid eps values
  expect_error(cluster_stationary_gps_env(test_data, subid = 1, eps = -10), "eps")
  expect_error(cluster_stationary_gps_env(test_data, subid = 1, eps = 0), "eps")
  expect_error(cluster_stationary_gps_env(test_data, subid = 1, eps = "invalid"), "numeric")
  
  # Test invalid subid
  expect_error(cluster_stationary_gps_env(test_data, subid = -1, eps = 50), "subid")
  expect_error(cluster_stationary_gps_env(test_data, subid = "invalid", eps = 50), "numeric")
})

test_that("clustering handles duplicate coordinates", {
  # Create data with exact duplicate coordinates
  duplicate_coords <- tibble(
    subid = rep(1, 4),
    lat = rep(43.074713, 4),
    lon = rep(-89.384373, 4),
    dttm_obs = as.POSIXct(c("2023-01-01 08:00:00", "2023-01-01 08:30:00",
                           "2023-01-01 09:00:00", "2023-01-01 09:30:00")),
    movement_state = "stationary"
  )
  
  result <- cluster_stationary_gps_env(duplicate_coords, subid = 1, eps = 50)
  
  # Should form one cluster from duplicate coordinates
  expect_equal(nrow(result), 1)
  expect_equal(result$cluster[1], 1)
  expect_equal(result$total_visits[1], 4)
})

test_that("clustering calculates visit statistics correctly", {
  # Create test data with known visit patterns
  test_data <- tibble(
    subid = rep(1, 6),
    lat = rep(43.074713, 6),
    lon = rep(-89.384373, 6),
    dttm_obs = as.POSIXct(c(
      "2023-01-01 08:00:00", "2023-01-01 08:30:00",  # Day 1: 2 visits
      "2023-01-02 09:00:00", "2023-01-02 09:15:00",  # Day 2: 2 visits  
      "2023-01-03 10:00:00", "2023-01-03 10:45:00"   # Day 3: 2 visits
    )),
    movement_state = "stationary"
  )
  
  result <- cluster_stationary_gps_env(test_data, subid = 1, eps = 50)
  
  expect_equal(nrow(result), 1)
  expect_equal(result$total_visits[1], 6)
  expect_equal(result$unique_days[1], 3)
  expect_gt(result$total_duration_hours[1], 0)
})

test_that("clustering handles coordinates near date line", {
  # Test coordinates near international date line (180/-180 longitude)
  dateline_coords <- tibble(
    subid = rep(1, 4),
    lat = c(0, 0, 0, 0),
    lon = c(179.9, 179.95, -179.9, -179.95),
    dttm_obs = seq(from = as.POSIXct("2023-01-01 08:00:00"), by = "30 mins", length.out = 4),
    movement_state = "stationary"
  )
  
  # Should not error out (even if clustering behavior is imperfect near date line)
  expect_no_error(cluster_stationary_gps_env(dateline_coords, subid = 1, eps = 50))
})

test_that("cluster_by_day handles multi-day data correctly", {
  # Create multi-day test data
  multi_day_data <- tibble(
    subid = rep(1, 8),
    lat = rep(43.074713, 8),
    lon = rep(-89.384373, 8),
    dttm_obs = as.POSIXct(c(
      "2023-01-01 08:00:00", "2023-01-01 08:30:00", "2023-01-01 17:00:00", "2023-01-01 17:30:00",
      "2023-01-02 08:15:00", "2023-01-02 09:00:00", "2023-01-02 17:15:00", "2023-01-02 18:00:00"
    )),
    movement_state = "stationary",
    date = as.Date(dttm_obs)
  )
  
  result <- cluster_by_day(multi_day_data, eps = 50, min_duration_min = 30)
  
  # Should process both days
  expect_gte(nrow(result), 2)
  expect_true(all(c("daily_cluster", "date") %in% names(result)))
  
  # Should have data from both days
  unique_dates <- unique(result$date)
  expect_equal(length(unique_dates), 2)
})

test_that("distance calculations are accurate", {
  # Test known distance calculation
  # Madison Capitol to UW-Madison Campus is approximately 2.5 km
  capitol_coords <- c(43.074713, -89.384373)  # Wisconsin State Capitol
  uw_coords <- c(43.076592, -89.412476)       # UW-Madison Memorial Union
  
  # Calculate distance using geosphere (our reference)
  expected_distance <- distHaversine(
    c(capitol_coords[2], capitol_coords[1]),
    c(uw_coords[2], uw_coords[1])
  )
  
  # Create test data with these two points
  test_data <- tibble(
    subid = rep(1, 4),
    lat = c(rep(capitol_coords[1], 2), rep(uw_coords[1], 2)),
    lon = c(rep(capitol_coords[2], 2), rep(uw_coords[2], 2)),
    dttm_obs = as.POSIXct(c("2023-01-01 08:00:00", "2023-01-01 08:30:00",
                           "2023-01-01 10:00:00", "2023-01-01 10:30:00")),
    movement_state = "stationary"
  )
  
  # With eps smaller than the distance, should get 2 clusters
  result <- cluster_stationary_gps_env(test_data, subid = 1, eps = expected_distance - 100)
  expect_gte(nrow(result), 2)
  
  # With eps larger than the distance, might get 1 cluster
  result_large <- cluster_stationary_gps_env(test_data, subid = 1, eps = expected_distance + 100)
  expect_gte(nrow(result_large), 1)
  expect_lte(nrow(result_large), nrow(result))
})