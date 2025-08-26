# scripts/r/database.R
# Centralized database connection and utility functions

library(DBI)
library(RPostgres)

# ==============================================================================
# CONNECTION MANAGEMENT
# ==============================================================================

#' Create connection to GPS2 PostGIS database
#'
#' @description
#' Establishes connection to the GPS2 PostGIS database running in Docker.
#' Provides informative error messages for common connection issues.
#'
#' @return A DBI database connection object for the GPS2 PostGIS database
#'
#' @details
#' Connection parameters:
#' - Host: localhost:5432
#' - Database: gps2_geocoding
#' - User: gps2_researcher
#' - Password: from GPS2_DB_PASSWORD env var or default
#'
#' @examples
#' \dontrun{
#' # Connect to database
#' con <- connect_gps2_db()
#' # Remember to disconnect when done
#' dbDisconnect(con)
#' }
#'
#' @export
connect_gps2_db <- function() {
  tryCatch({
    con <- dbConnect(
      RPostgres::Postgres(),
      host = "localhost",
      port = 5432,
      dbname = "gps2_geocoding",
      user = "gps2_researcher",
      password = Sys.getenv("GPS2_DB_PASSWORD", "secure_research_password")
    )
    
    return(con)
    
  }, error = function(e) {
    if (grepl("could not connect", e$message, ignore.case = TRUE)) {
      stop("Cannot connect to PostGIS database.\n",
           "Run: docker-compose up -d\n",
           "Wait 30 seconds, then try again.")
    }
    stop("Database error: ", e$message)
  })
}

#' Safely disconnect from GPS2 database
#'
#' @description
#' Safely disconnects from database with validity checking.
#' No-op if connection is already closed or invalid.
#'
#' @param con A DBI database connection object
#'
#' @return NULL. Called for side effects.
#'
#' @examples
#' \dontrun{
#' con <- connect_gps2_db()
#' # ... do work ...
#' disconnect_gps2_db(con)
#' }
#'
#' @export
disconnect_gps2_db <- function(con) {
  if (!is.null(con) && dbIsValid(con)) {
    dbDisconnect(con)
  }
}

#' Execute SQL query with automatic connection management
#'
#' @description
#' Executes SQL SELECT queries against the GPS2 database with automatic
#' connection setup and cleanup. Supports parameterized queries for safety.
#'
#' @param sql Character string containing SQL SELECT statement
#' @param params Named list of parameters for parameterized queries.
#'   Default: empty list
#'
#' @return Data frame containing query results
#'
#' @details
#' This function handles all connection lifecycle:
#' 1. Opens connection
#' 2. Executes query (with parameters if provided)
#' 3. Closes connection (even on error)
#'
#' Use $1, $2, etc. in SQL for parameterized queries.
#'
#' @examples
#' \dontrun{
#' # Simple query
#' participants <- query_gps2_db("SELECT DISTINCT subid FROM gps2.gps_stationary_points")
#' 
#' # Parameterized query
#' data <- query_gps2_db("SELECT * FROM gps2.gps_stationary_points WHERE subid = $1",
#'                       list(participant_id))
#' }
#'
#' @export
query_gps2_db <- function(sql, params = list()) {
  con <- connect_gps2_db()
  tryCatch({
    if (length(params) > 0) {
      result <- dbGetQuery(con, sql, params = params)
    } else {
      result <- dbGetQuery(con, sql)
    }
    return(result)
  }, finally = {
    disconnect_gps2_db(con)
  })
}

#' Execute SQL statement with automatic connection management
#'
#' @description
#' Executes SQL INSERT, UPDATE, DELETE statements against the GPS2 database
#' with automatic connection management. Returns number of affected rows.
#'
#' @param sql Character string containing SQL statement (INSERT/UPDATE/DELETE)
#' @param params Named list of parameters for parameterized queries.
#'   Default: empty list
#'
#' @return Integer number of rows affected by the statement
#'
#' @details
#' Similar to query_gps2_db() but for statements that modify data.
#' Automatically handles connection lifecycle and cleanup.
#'
#' @examples
#' \dontrun{
#' # Delete test data
#' rows_affected <- execute_gps2_db("DELETE FROM gps2.gps_stationary_points WHERE subid = 999")
#' 
#' # Parameterized insert
#' execute_gps2_db("INSERT INTO gps2.test_table (id, name) VALUES ($1, $2)",
#'                 list(1, "test"))
#' }
#'
#' @export
execute_gps2_db <- function(sql, params = list()) {
  con <- connect_gps2_db()
  tryCatch({
    if (length(params) > 0) {
      result <- dbExecute(con, sql, params = params)
    } else {
      result <- dbExecute(con, sql)
    }
    return(result)
  }, finally = {
    disconnect_gps2_db(con)
  })
}

# ==============================================================================
# DATA ACCESS FUNCTIONS
# ==============================================================================

#' Retrieve GPS data for a specific participant
#'
#' @description
#' Fetches all stationary GPS points for a participant from the database,
#' ordered chronologically. Returns core GPS and movement data.
#'
#' @param participant_id Numeric participant identifier (subid)
#'
#' @return Data frame with columns: subid, lat, lon, dttm_obs, dist,
#'   duration, speed, movement_state. Empty data frame if participant not found.
#'
#' @examples
#' \dontrun{
#' # Get all GPS data for participant 19
#' participant_gps <- get_participant_data(19)
#' }
#'
#' @export
get_participant_data <- function(participant_id) {
  query_gps2_db("
    SELECT subid, lat, lon, dttm_obs, dist, duration, speed, movement_state
    FROM gps2.gps_stationary_points 
    WHERE subid = $1
    ORDER BY dttm_obs;
  ", list(participant_id))
}

#' Retrieve location clusters for a specific participant
#'
#' @description
#' Fetches all location clusters for a participant, ordered by significance
#' (duration and visit frequency). Returns cluster metadata and statistics.
#'
#' @param participant_id Numeric participant identifier (subid)
#'
#' @return Data frame with columns: cluster_id, lat, lon, n_points,
#'   first_visit, last_visit, total_visits, total_duration_hours, unique_days.
#'   Ordered by total_duration_hours DESC, total_visits DESC, cluster_id.
#'
#' @examples
#' \dontrun{
#' # Get clusters for participant 19
#' clusters <- get_participant_clusters(19)
#' }
#'
#' @export
get_participant_clusters <- function(participant_id) {
  query_gps2_db("
    SELECT cluster_id, lat, lon, n_points, first_visit, last_visit, 
           total_visits, total_duration_hours, unique_days
    FROM gps2.location_clusters 
    WHERE subid = $1
    ORDER BY total_duration_hours DESC, total_visits DESC, cluster_id;
  ", list(participant_id))
}

#' Find GPS points near a specific location
#'
#' @description
#' Uses PostGIS spatial queries to find GPS points within a specified
#' distance of target coordinates. Returns up to 20 nearest points.
#'
#' @param lat Numeric latitude of target location
#' @param lon Numeric longitude of target location
#' @param radius_meters Numeric search radius in meters. Default: 100
#'
#' @return Data frame with columns: subid, lat, lon, dttm_obs, distance_m.
#'   Limited to 20 results, ordered by distance.
#'
#' @details
#' Uses PostGIS geography functions for accurate distance calculations.
#' Useful for finding activity patterns around points of interest.
#'
#' @examples
#' \dontrun{
#' # Find points within 50m of coordinates
#' nearby <- get_nearby_points(43.0731, -89.4012, radius_meters = 50)
#' }
#'
#' @export
get_nearby_points <- function(lat, lon, radius_meters = 100) {
  query_gps2_db("
    SELECT subid, lat, lon, dttm_obs,
           ROUND(ST_Distance(location::geography, 
                            ST_SetSRID(ST_MakePoint($2, $1), 4326)::geography)) as distance_m
    FROM gps2.gps_stationary_points
    WHERE ST_DWithin(location::geography, 
                     ST_SetSRID(ST_MakePoint($2, $1), 4326)::geography, $3)
    ORDER BY distance_m
    LIMIT 20;
  ", list(lat, lon, radius_meters))
}

#' Get summary statistics for the GPS2 database
#'
#' @description
#' Returns key metrics about the current state of the GPS2 database
#' including record counts and geocoding status.
#'
#' @return Named list with elements:
#'   - total_points: Total GPS stationary points
#'   - participants: Number of unique participants
#'   - clusters: Total location clusters
#'   - geocoded: Number of successfully geocoded locations
#'
#' @examples
#' \dontrun{
#' # Get database overview
#' summary <- get_database_summary()
#' cat("Total GPS points:", summary$total_points)
#' }
#'
#' @export
get_database_summary <- function() {
  list(
    total_points = query_gps2_db("SELECT COUNT(*) as count FROM gps2.gps_stationary_points;")$count,
    participants = query_gps2_db("SELECT COUNT(DISTINCT subid) as count FROM gps2.gps_stationary_points;")$count,
    clusters = query_gps2_db("SELECT COUNT(*) as count FROM gps2.location_clusters;")$count,
    geocoded = query_gps2_db("SELECT COUNT(*) as count FROM gps2.cluster_geocoding WHERE display_name IS NOT NULL;")$count
  )
}

# ==============================================================================
# HEALTH CHECKS
# ==============================================================================

#' Comprehensive GPS2 system health check
#'
#' @description
#' Performs complete system health check including Docker containers,
#' database connectivity, PostGIS functionality, and data summary.
#' Provides detailed console output with status indicators.
#'
#' @return Logical TRUE if all systems are operational, FALSE otherwise
#'
#' @details
#' Checks performed:
#' 1. Docker container status
#' 2. PostGIS database connectivity
#' 3. Database schema presence
#' 4. Data summary (points, participants, clusters, geocoding)
#'
#' Console output uses ✅ for success and ❌ for failures.
#'
#' @examples
#' \dontrun{
#' # Run complete system check
#' if (check_gps2_system()) {
#'   cat("System ready for analysis\n")
#' }
#' }
#'
#' @export
check_gps2_system <- function() {
  cat("GPS2 System Health Check\n")
  cat("========================\n")
  
  # Container check
  container_status <- system("docker ps | grep gps2_geocoding", ignore.stdout = TRUE) == 0
  cat("Docker container:", if(container_status) "✅ Running" else "❌ Not running", "\n")
  
  if (!container_status) {
    cat("Start with: docker-compose up -d\n")
    return(FALSE)
  }
  
  # Database check
  tryCatch({
    con <- connect_gps2_db()
    postgis_version <- dbGetQuery(con, "SELECT PostGIS_Version();")
    cat("PostGIS connection: ✅ Connected\n")
    cat("PostGIS version:", postgis_version$postgis_version, "\n")
    
    # Schema check
    tables <- dbGetQuery(con, "SELECT COUNT(*) as count FROM information_schema.tables WHERE table_schema = 'gps2';")
    cat("GPS2 schema tables:", as.integer(tables$count), "\n")
    
    # Data summary
    summary <- get_database_summary()
    cat("GPS points:", format(as.integer(summary$total_points), big.mark = ","), "\n")
    cat("Participants:", as.integer(summary$participants), "\n")
    cat("Clusters:", as.integer(summary$clusters), "\n")
    cat("Geocoded:", as.integer(summary$geocoded), "\n")
    
    disconnect_gps2_db(con)
    cat("System status: ✅ All systems operational\n")
    return(TRUE)
    
  }, error = function(e) {
    cat("Database connection: ❌ Failed\n")
    cat("Error:", e$message, "\n")
    return(FALSE)
  })
}

#' Test database connectivity
#'
#' @description
#' Simple connectivity test that attempts a basic query to verify
#' database connection is working. Used internally by other functions.
#'
#' @return Logical TRUE if connection successful, FALSE otherwise
#'
#' @details
#' Executes "SELECT 1 as test" query and checks for expected result.
#' Catches all errors and returns FALSE rather than throwing.
#'
#' @examples
#' \dontrun{
#' # Check if database is accessible
#' if (test_connection()) {
#'   cat("Database is accessible\n")
#' }
#' }
#'
#' @export
test_connection <- function() {
  tryCatch({
    result <- query_gps2_db("SELECT 1 as test;")
    return(result$test == 1)
  }, error = function(e) {
    return(FALSE)
  })
}