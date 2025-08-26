# tests/testthat/helper-test-data.R
# Helper functions for creating test data for GPS2 tests

library(tibble)
library(dplyr)
library(lubridate)

#' Create basic GPS test data
#' @param n_points Number of GPS points to generate
#' @param subid Participant ID
#' @param start_time Starting timestamp
#' @param location_center Center coordinates as c(lat, lon)
#' @param spread Maximum distance from center in decimal degrees
create_test_gps_data <- function(n_points = 10, subid = 1, 
                                start_time = as.POSIXct("2023-01-01 08:00:00"),
                                location_center = c(43.074713, -89.384373),
                                spread = 0.01) {
  tibble(
    subid = rep(subid, n_points),
    lat = location_center[1] + runif(n_points, -spread, spread),
    lon = location_center[2] + runif(n_points, -spread, spread),
    dttm_obs = seq(from = start_time, by = "10 mins", length.out = n_points),
    movement_state = "stationary",
    time = format(dttm_obs, "%Y-%m-%dT%H:%M:%SZ")
  )
}

#' Create GPS data that should form distinct clusters
#' @param cluster_distance Distance between clusters in decimal degrees
create_clustered_test_data <- function(cluster_distance = 0.01) {
  # Madison downtown area coordinates
  madison_coords <- c(43.074713, -89.384373)
  
  # Cluster 1: Home location (multiple visits over several days)
  cluster1 <- tibble(
    subid = 1,
    lat = rep(madison_coords[1], 6),
    lon = rep(madison_coords[2], 6),
    dttm_obs = as.POSIXct(c(
      "2023-01-01 08:00:00", "2023-01-01 08:30:00", "2023-01-01 17:00:00",
      "2023-01-02 08:00:00", "2023-01-02 17:30:00", "2023-01-03 08:15:00"
    )),
    movement_state = "stationary"
  )
  
  # Cluster 2: Work location (different area, multiple visits)
  cluster2 <- tibble(
    subid = 1,
    lat = rep(madison_coords[1] + cluster_distance, 4),
    lon = rep(madison_coords[2] + cluster_distance, 4),
    dttm_obs = as.POSIXct(c(
      "2023-01-01 09:00:00", "2023-01-01 12:00:00",
      "2023-01-02 09:30:00", "2023-01-02 13:00:00"
    )),
    movement_state = "stationary"
  )
  
  # Add some noise points that shouldn't cluster
  noise_points <- tibble(
    subid = 1,
    lat = madison_coords[1] + runif(3, -0.05, 0.05),
    lon = madison_coords[2] + runif(3, -0.05, 0.05),
    dttm_obs = as.POSIXct(c(
      "2023-01-01 10:30:00", "2023-01-02 15:00:00", "2023-01-03 11:00:00"
    )),
    movement_state = "stationary"
  )
  
  bind_rows(cluster1, cluster2, noise_points) |>
    arrange(dttm_obs)
}

#' Create invalid GPS data for testing error handling
create_invalid_gps_data <- function() {
  tibble(
    subid = c(1, 1, 1, 1, NA),
    lat = c(43.074, 200, -95, NA, 43.074),  # Invalid: >90, <-90, NA
    lon = c(-89.384, -89.384, -89.384, -200, -89.384),  # Invalid: <-180
    dttm_obs = as.POSIXct(c(
      "2023-01-01 08:00:00", "2023-01-01 09:00:00", 
      "2023-01-01 10:00:00", "2023-01-01 11:00:00",
      "2023-01-01 12:00:00"
    )),
    movement_state = c("stationary", "stationary", "stationary", "stationary", "stationary")
  )
}

#' Create GPS data with malformed timestamps
create_malformed_time_data <- function() {
  tibble(
    subid = c(1, 1, 1),
    lat = c(43.074, 43.075, 43.076),
    lon = c(-89.384, -89.385, -89.386),
    time = c("2023-01-01T08:00:00Z", "invalid-time", "2023-01-01T10:00:00Z")
  )
}

#' Create test data with edge case coordinates
create_edge_case_coordinates <- function() {
  tibble(
    subid = rep(1, 8),
    # Test boundary coordinates
    lat = c(90, -90, 0, 0, 89.999, -89.999, 0.0001, -0.0001),
    lon = c(0, 0, 180, -180, 179.999, -179.999, 0.0001, -0.0001),
    dttm_obs = seq(
      from = as.POSIXct("2023-01-01 08:00:00"), 
      by = "1 hour", 
      length.out = 8
    ),
    movement_state = "stationary"
  )
}

#' Create test data for performance testing
create_large_test_dataset <- function(n_participants = 5, n_points_per_participant = 1000) {
  all_data <- list()
  for (i in 1:n_participants) {
    all_data[[i]] <- create_test_gps_data(
      n_points = n_points_per_participant,
      subid = i,
      start_time = as.POSIXct("2023-01-01 00:00:00") + lubridate::days(i - 1),
      location_center = c(43.074 + runif(1, -0.1, 0.1), -89.384 + runif(1, -0.1, 0.1))
    )
  }
  do.call(rbind, all_data)
}

#' Create mock database connection for testing
create_mock_db_connection <- function() {
  # This would normally use DBI testing utilities
  # For now, returns a simple list structure
  list(
    host = "localhost",
    port = 5432,
    dbname = "test_gps2",
    user = "test_user",
    connected = TRUE
  )
}

#' Create test cluster result data
create_test_cluster_result <- function() {
  tibble(
    subid = c(1, 1, 1),
    cluster = c(1, 2, 3),
    lat = c(43.074713, 43.084713, 43.064713),
    lon = c(-89.384373, -89.394373, -89.374373),
    n_points = c(5, 3, 2),
    first_visit = as.POSIXct(c("2023-01-01 08:00:00", "2023-01-01 10:00:00", "2023-01-01 14:00:00")),
    last_visit = as.POSIXct(c("2023-01-03 17:00:00", "2023-01-02 15:00:00", "2023-01-01 16:00:00")),
    total_visits = c(10, 6, 2),
    total_duration_hours = c(25.5, 8.2, 2.0),
    unique_days = c(3, 2, 1),
    location_type = c("Routine", "Frequent", "Occasional")
  )
}

#' Clean up test database after tests
cleanup_test_database <- function() {
  # In a real implementation, this would clean up test data
  # For now, just a placeholder
  invisible(TRUE)
}

#' Setup test environment
setup_test_environment <- function() {
  # Set test-specific configuration
  options(
    gps2.test_mode = TRUE,
    gps2.db_timeout = 5,
    gps2.batch_size = 100
  )
}