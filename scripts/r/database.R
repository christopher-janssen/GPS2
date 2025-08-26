# scripts/r/database.R
# Centralized database connection and utility functions

library(DBI)
library(RPostgres)

# ==============================================================================
# CONNECTION MANAGEMENT
# ==============================================================================

# Single source of truth for database connection
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

# Safe disconnect wrapper
disconnect_gps2_db <- function(con) {
  if (!is.null(con) && dbIsValid(con)) {
    dbDisconnect(con)
  }
}

# Execute query with automatic connection management
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

# Execute statement with automatic connection management
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

# Get participant GPS data
get_participant_data <- function(participant_id) {
  query_gps2_db("
    SELECT subid, lat, lon, dttm_obs, dist, duration, speed, movement_state
    FROM gps2.gps_stationary_points 
    WHERE subid = $1
    ORDER BY dttm_obs;
  ", list(participant_id))
}

# Get participant clusters
get_participant_clusters <- function(participant_id) {
  query_gps2_db("
    SELECT cluster_id, lat, lon, n_points, first_visit, last_visit, 
           total_visits, total_duration_hours, unique_days
    FROM gps2.location_clusters 
    WHERE subid = $1
    ORDER BY total_duration_hours DESC, total_visits DESC, cluster_id;
  ", list(participant_id))
}

# Get nearby points
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

# Get database summary
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

# Comprehensive system check
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
    cat("GPS2 schema tables:", tables$count, "\n")
    
    # Data summary
    summary <- get_database_summary()
    cat("GPS points:", format(summary$total_points, big.mark = ","), "\n")
    cat("Participants:", summary$participants, "\n")
    cat("Clusters:", summary$clusters, "\n")
    cat("Geocoded:", summary$geocoded, "\n")
    
    disconnect_gps2_db(con)
    cat("System status: ✅ All systems operational\n")
    return(TRUE)
    
  }, error = function(e) {
    cat("Database connection: ❌ Failed\n")
    cat("Error:", e$message, "\n")
    return(FALSE)
  })
}

# Simple connectivity test
test_connection <- function() {
  tryCatch({
    result <- query_gps2_db("SELECT 1 as test;")
    return(result$test == 1)
  }, error = function(e) {
    return(FALSE)
  })
}