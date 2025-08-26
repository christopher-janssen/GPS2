# tests/testthat/test-performance-benchmarks.R
# Performance tests and benchmarks for GPS2 functions

library(testthat)
library(dplyr)

# Set working directory for tests
if (!file.exists("scripts/r/gps_processing.R")) {
  setwd(here::here())
}

# Source the functions being tested
source("scripts/r/gps_processing.R")
source("scripts/r/analysis.R")
source("tests/testthat/helper-test-data.R")

test_that("GPS processing completes within reasonable time", {
  skip_on_ci()  # Skip performance tests on CI
  
  # Create medium-sized dataset
  medium_data <- create_test_gps_data(n_points = 1000)
  medium_data$time <- format(medium_data$dttm_obs, "%Y-%m-%dT%H:%M:%SZ")
  
  # Should complete within 5 seconds
  expect_lt({
    start_time <- Sys.time()
    result <- process_gps(medium_data)
    end_time <- Sys.time()
    as.numeric(difftime(end_time, start_time, units = "secs"))
  }, 5)
})

test_that("clustering scales reasonably with data size", {
  skip_on_ci()  # Skip performance tests on CI
  
  # Test different data sizes
  small_data <- create_test_gps_data(n_points = 100)
  small_data$movement_state <- "stationary"
  
  large_data <- create_test_gps_data(n_points = 2000)
  large_data$movement_state <- "stationary"
  
  # Measure clustering time for small dataset
  start_small <- Sys.time()
  result_small <- cluster_stationary_gps_env(small_data, subid = 1, eps = 100)
  time_small <- as.numeric(difftime(Sys.time(), start_small, units = "secs"))
  
  # Measure clustering time for large dataset
  start_large <- Sys.time()
  result_large <- cluster_stationary_gps_env(large_data, subid = 1, eps = 100)
  time_large <- as.numeric(difftime(Sys.time(), start_large, units = "secs"))
  
  # Large dataset should not take more than 100x longer than small
  # (indicates reasonable algorithmic complexity)
  expect_lt(time_large, time_small * 100)
  
  # Both should complete within reasonable time
  expect_lt(time_small, 2)   # Small dataset: < 2 seconds
  expect_lt(time_large, 30)  # Large dataset: < 30 seconds
})

test_that("memory usage is bounded during processing", {
  skip_on_ci()  # Skip on CI due to memory monitoring complexity
  
  # Force garbage collection to get baseline
  gc()
  initial_memory <- sum(gc()[, "used"])
  
  # Process moderately large dataset
  test_data <- create_test_gps_data(n_points = 5000)
  test_data$time <- format(test_data$dttm_obs, "%Y-%m-%dT%H:%M:%SZ")
  
  result <- process_gps(test_data)
  
  # Check memory after processing
  gc()
  final_memory <- sum(gc()[, "used"])
  memory_increase <- final_memory - initial_memory
  
  # Memory increase should be reasonable (< 100MB)
  expect_lt(memory_increase, 100)
  
  # Result should be reasonable size
  expect_lt(object.size(result), object.size(test_data) * 3)
})

test_that("distance calculations are efficient", {
  # Test geosphere::distHaversine performance
  n_calculations <- 10000
  
  # Create random coordinate pairs
  lats1 <- runif(n_calculations, -90, 90)
  lons1 <- runif(n_calculations, -180, 180)
  lats2 <- runif(n_calculations, -90, 90)
  lons2 <- runif(n_calculations, -180, 180)
  
  # Time the distance calculations
  start_time <- Sys.time()
  distances <- geosphere::distHaversine(
    cbind(lons1, lats1),
    cbind(lons2, lats2)
  )
  calculation_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  
  # Should handle 10k calculations in under 1 second
  expect_lt(calculation_time, 1.0)
  expect_equal(length(distances), n_calculations)
  expect_true(all(is.finite(distances)))
})

test_that("validation functions are fast", {
  # Test validation performance on larger datasets
  large_validation_data <- create_test_gps_data(n_points = 10000)
  
  # GPS data validation should be fast
  start_time <- Sys.time()
  result <- validate_gps_data(large_validation_data)
  validation_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  
  expect_true(result)
  expect_lt(validation_time, 0.5)  # Should validate 10k points in < 0.5 seconds
})

test_that("clustering handles worst-case scenarios efficiently", {
  skip_on_ci()  # Skip on CI due to time
  
  # Worst case: all points at same location (requires checking all pairs)
  worst_case_data <- tibble(
    subid = rep(1, 1000),
    lat = rep(43.074713, 1000),
    lon = rep(-89.384373, 1000),
    dttm_obs = seq(from = as.POSIXct("2023-01-01 08:00:00"), by = "1 min", length.out = 1000),
    movement_state = "stationary"
  )
  
  # Should still complete in reasonable time
  start_time <- Sys.time()
  result <- cluster_stationary_gps_env(worst_case_data, subid = 1, eps = 100)
  clustering_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  
  expect_lt(clustering_time, 10)  # Even worst case should be < 10 seconds
  expect_s3_class(result, "tbl_df")
})

test_that("database query performance is acceptable", {
  skip_if_not(exists("test_connection") && test_connection(), "Database not available")
  
  # Test simple query performance
  start_time <- Sys.time()
  result <- query_gps2_db("SELECT COUNT(*) as count FROM gps2.gps_stationary_points LIMIT 1000;")
  query_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  
  expect_lt(query_time, 2.0)  # Simple queries should be fast
  expect_s3_class(result, "data.frame")
})

test_that("repeated operations don't cause memory leaks", {
  skip_on_ci()  # Skip on CI
  
  gc()
  initial_memory <- sum(gc()[, "used"])
  
  # Perform repeated GPS processing operations
  for (i in 1:10) {
    test_data <- create_test_gps_data(n_points = 100)
    test_data$time <- format(test_data$dttm_obs, "%Y-%m-%dT%H:%M:%SZ")
    
    result <- process_gps(test_data)
    
    # Clean up explicitly
    rm(result, test_data)
    
    # Periodic garbage collection
    if (i %% 5 == 0) gc()
  }
  
  gc()
  final_memory <- sum(gc()[, "used"])
  memory_growth <- final_memory - initial_memory
  
  # Memory growth should be minimal (< 50MB after 10 iterations)
  expect_lt(memory_growth, 50)
})

test_that("function overhead is minimal", {
  # Test that wrapper functions don't add significant overhead
  test_data <- create_test_gps_data(n_points = 100)
  
  # Time direct operations
  start_direct <- Sys.time()
  for (i in 1:100) {
    validate_gps_data(test_data)
  }
  time_direct <- as.numeric(difftime(Sys.time(), start_direct, units = "secs"))
  
  # Should be very fast for 100 validations
  expect_lt(time_direct, 1.0)
})

test_that("configuration access is efficient", {
  # Test that repeated config access doesn't slow down
  start_time <- Sys.time()
  
  for (i in 1:1000) {
    eps <- get_config("clustering", "default_eps")
    threshold <- get_config("filtering", "stationary_threshold_mph")
    batch_size <- get_config("database", "batch_size")
  }
  
  config_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  
  # 1000 config accesses should be very fast
  expect_lt(config_time, 0.1)
})