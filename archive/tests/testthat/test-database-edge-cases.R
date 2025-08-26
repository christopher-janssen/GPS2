# tests/testthat/test-database-edge-cases.R
# Tests for database connection and transaction edge cases

library(testthat)
library(dplyr)

# Set working directory for tests
if (!file.exists("scripts/r/database.R")) {
  setwd(here::here())
}

# Source the functions being tested
source("scripts/r/database.R")
source("config/gps2_config.R")

test_that("database connection handles missing environment variables", {
  # Temporarily unset the password environment variable
  original_password <- Sys.getenv("GPS2_DB_PASSWORD")
  Sys.unsetenv("GPS2_DB_PASSWORD")
  
  # Should use default password
  expect_no_error({
    # Don't actually connect, just test the function logic
    tryCatch(connect_gps2_db(), error = function(e) {
      # Expected to fail since Docker isn't running, but shouldn't error on missing env var
      expect_true(grepl("could not connect|database", e$message, ignore.case = TRUE))
    })
  })
  
  # Restore original password
  if (nzchar(original_password)) {
    Sys.setenv("GPS2_DB_PASSWORD" = original_password)
  }
})

test_that("query_gps2_db handles malformed SQL", {
  skip_if_not(test_connection(), "Database not available")
  
  # Test malformed SQL
  expect_error(query_gps2_db("SELECT * FROM nonexistent_table;"), "does not exist|relation")
  expect_error(query_gps2_db("INVALID SQL SYNTAX;"), "syntax")
})

test_that("query_gps2_db handles SQL injection attempts", {
  skip_if_not(test_connection(), "Database not available")
  
  # Test parameterized query protection
  malicious_input <- "1; DROP TABLE gps2.gps_stationary_points; --"
  
  # Should safely handle malicious parameter
  expect_no_error({
    result <- query_gps2_db(
      "SELECT COUNT(*) as count FROM gps2.gps_stationary_points WHERE subid = $1", 
      list(malicious_input)
    )
    expect_equal(result$count, 0)  # Should return 0 matches, not execute DROP
  })
})

test_that("database functions handle NULL and empty parameters", {
  skip_if_not(test_connection(), "Database not available")
  
  # Test with NULL parameters
  expect_no_error(query_gps2_db("SELECT 1 as test", list()))
  
  # Test with empty parameter list
  result <- query_gps2_db("SELECT 1 as test", params = list())
  expect_equal(result$test, 1)
})

test_that("get_participant_data handles non-existent participants", {
  skip_if_not(test_connection(), "Database not available")
  
  # Use a participant ID that definitely doesn't exist
  result <- get_participant_data(99999999)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})

test_that("database summary handles empty database", {
  skip_if_not(test_connection(), "Database not available")
  
  # Should not error even if database is empty
  expect_no_error({
    summary <- get_database_summary()
    expect_true(all(c("total_points", "participants", "clusters", "geocoded") %in% names(summary)))
    expect_true(all(sapply(summary, is.numeric)))
  })
})

test_that("connection validation works correctly", {
  # Test with invalid connection object
  expect_false(dbIsValid(NULL))
  
  # Test disconnect with NULL connection
  expect_no_error(disconnect_gps2_db(NULL))
  
  # Test disconnect with invalid connection
  fake_con <- list(invalid = TRUE)
  class(fake_con) <- "PqConnection"
  expect_no_error(disconnect_gps2_db(fake_con))
})

test_that("database health check handles service failures", {
  # Mock system command to simulate container not running
  old_system <- base::system
  
  # Override system function to simulate container failure
  assignInNamespace("system", function(command, ignore.stdout = FALSE) {
    if (grepl("docker ps", command)) return(1)  # Container not found
    return(old_system(command, ignore.stdout))
  }, ns = "base")
  
  # Should handle container check failure gracefully
  result <- check_gps2_system()
  expect_false(result)
  
  # Restore original system function
  assignInNamespace("system", old_system, ns = "base")
})