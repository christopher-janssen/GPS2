# Database Functions for GPS22 Project

#' Connect to GPS Analysis Database
connect_gps_db <- function() {
  safe_connect <- safely(~ dbConnect(RPostgres::Postgres(),
    host = GPS_DB_PARAMS$host,
    port = GPS_DB_PARAMS$port,
    dbname = GPS_DB_PARAMS$dbname,
    user = GPS_DB_PARAMS$user,
    password = GPS_DB_PARAMS$password
  ))
  
  result <- safe_connect()
  
  if (is.null(result$error)) {
    message("✓ Connected to GPS analysis database")
    return(result$result)
  } else {
    message("Database connection failed: ", result$error$message)
    message("Make sure PostGIS container is running: docker-compose up -d")
    return(NULL)
  }
}

#' Test database connection health
test_db_connection <- function() {
  con <- connect_gps_db()
  
  if (!is.null(con)) {
    dbDisconnect(con)
    return(TRUE)
  } else {
    message("✗ Database connection failed")
    return(FALSE)
  }
}

#' Safe database disconnect with error handling
disconnect_gps_db <- function(con) {
  if (!is.null(con)) {
    safe_disconnect <- possibly(~ {
      # Check if connection is still valid before attempting to close
      if (dbIsValid(con)) {
        dbDisconnect(con)
        message("✓ Database connection closed")
      } else {
        message("Connection already closed/invalid")
      }
    }, otherwise = NULL, quiet = FALSE)
    
    result <- safe_disconnect()
    if (is.null(result)) {
      message("Warning: Error closing database connection")
    }
  }
}

#' Check GPS data status and quality
check_gps_data_status <- function(con) {
  # Get data
  subject_count <- dbGetQuery(con, "SELECT COUNT(*) as count FROM subjects")$count
  gps_count <- dbGetQuery(con, "SELECT COUNT(*) as count FROM raw_gps_points")$count
  
  validation <- dbGetQuery(con, "
    SELECT 
      COUNT(*) as total_points,
      ROUND(COUNT(DISTINCT subject_id),1) as unique_subjects,
      SUM(CASE WHEN lat IS NULL OR lon IS NULL OR lat = 0 OR lon = 0 THEN 1 ELSE 0 END) as invalid_coords,
      SUM(CASE WHEN time IS NULL THEN 1 ELSE 0 END) as invalid_timestamps,
      MIN(time) as earliest_timestamp,
      MAX(time) as latest_timestamp
    FROM raw_gps_points
  ")
  
  duplicates <- dbGetQuery(con, "
    SELECT COUNT(*) - COUNT(DISTINCT (subject_id, lat, lon, time)) as duplicates
    FROM raw_gps_points
  ")$duplicates
  
  tibble(
    metric = c("Subjects", "GPS Points", "Unique Subjects", "Date Range", "Duplicates", "Invalid Coords", "Invalid Timestamps"),
    value = c(
      format(subject_count, big.mark = ","),
      format(gps_count, big.mark = ","),
      validation$unique_subjects,
      paste(validation$earliest_timestamp, "to", validation$latest_timestamp),
      duplicates,
      validation$invalid_coords,
      validation$invalid_timestamps
    )
  )
}

#' Standardized data loading with numbered table references
#' @param table_ref Integer reference: 1=raw_gps, 2=processed_gps, 3=clusters, 4=geocoded, 5=joined, 6=zoning, 7=adi
#' @param con Database connection (optional, will create if NULL)
#' @param ... Additional parameters passed to specific pull functions
pull_db <- function(table_ref, con = NULL, ...) {
  if (is.null(con)) con <- connect_gps_db()

  switch(as.character(table_ref),
    "1" = pull_raw_gps(con, ...),           # Raw GPS points
    "2" = pull_processed_gps(con, ...),     # Processed GPS with movement states
    "3" = pull_gps_clusters(con, ...),      # Location clusters
    "4" = pull_geocoded_results(con, ...),  # Reverse geocoding results
    "5" = pull_joined_data(con, ...),       # Pre-joined analysis-ready data
    "6" = pull_zoning_data(con, ...),       # Madison zoning districts
    "7" = pull_adi_data(con, ...),          # ADI block groups
    stop("Unknown table reference: ", table_ref, ". Use 1-7 for different tables.")
  )
}

#' Pull raw GPS points
pull_raw_gps <- function(con, subid = NULL, limit = NULL) {
  query <- "
    SELECT 
      rg.point_id, rg.lat, rg.lon, rg.time, rg.sgmnt_type,
      s.subid, s.id as subject_id
    FROM raw_gps_points rg
    JOIN subjects s ON rg.subject_id = s.id"
    
  params <- list()
  if (!is.null(subid)) {
    query <- paste(query, "WHERE s.subid = $1")
    params <- list(subid)
  }
  
  query <- paste(query, "ORDER BY s.subid, rg.time")
  
  if (!is.null(limit)) {
    query <- paste(query, "LIMIT", limit)
  }
  
  # Only pass params if there are actual parameters
  if (length(params) > 0) {
    dbGetQuery(con, query, params)
  } else {
    dbGetQuery(con, query)
  }
}

#' Pull processed GPS with movement states
pull_processed_gps <- function(con, subid = NULL, limit = NULL) {
  query <- "
    SELECT 
      pg.processed_id, pg.raw_point_id, pg.lat, pg.lon, pg.time, pg.movement_state,
      pg.is_stationary, pg.cluster_id, pg.speed_mph, pg.dist_miles,
      s.subid, s.id as subject_id
    FROM processed_gps_points pg
    JOIN subjects s ON pg.subject_id = s.id"
    
  params <- list()
  if (!is.null(subid)) {
    query <- paste(query, "WHERE s.subid = $1")
    params <- list(subid)
  }
  
  query <- paste(query, "ORDER BY s.subid, pg.time")
  
  if (!is.null(limit)) {
    query <- paste(query, "LIMIT", limit)
  }
  
  # Only pass params if there are actual parameters
  if (length(params) > 0) {
    dbGetQuery(con, query, params)
  } else {
    dbGetQuery(con, query)
  }
}

#' Pull GPS clusters with optional geocoding
pull_gps_clusters <- function(con, subid = NULL, include_geocoding = TRUE) {
  if (include_geocoding) {
    query <- "
      SELECT 
        gc.cluster_id, gc.lat, gc.lon, gc.n_points,
        gc.total_visits, gc.total_duration_hours,
        gc.first_visit, gc.last_visit, s.subid,
        rgr.address as display_name, rgr.city, rgr.state, rgr.postcode,
        CASE WHEN rgr.address IS NOT NULL AND rgr.address != '' THEN true ELSE false END as geocoding_success
      FROM gps_clusters gc
      JOIN subjects s ON gc.subject_id = s.id
      LEFT JOIN reverse_geocode_results rgr ON gc.cluster_id = rgr.cluster_id"
  } else {
    query <- "
      SELECT 
        gc.cluster_id, gc.lat, gc.lon, gc.n_points,
        gc.total_visits, gc.total_duration_hours,
        gc.first_visit, gc.last_visit, s.subid
      FROM gps_clusters gc
      JOIN subjects s ON gc.subject_id = s.id"
  }
    
  if (!is.null(subid)) {
    query <- paste(query, "WHERE s.subid = $1 ORDER BY gc.total_visits DESC")
    dbGetQuery(con, query, list(subid))
  } else {
    dbGetQuery(con, paste(query, "ORDER BY s.subid, gc.total_visits DESC"))
  }
}

#' Pull reverse geocoding results
pull_geocoded_results <- function(con, subid = NULL, successful_only = FALSE) {
  query <- "
    SELECT 
      rgr.cluster_id, gc.lat, gc.lon, rgr.address, rgr.city, 
      rgr.state, rgr.postcode, 
      CASE WHEN rgr.address IS NOT NULL AND rgr.address != '' THEN true ELSE false END as geocoding_success,
      s.subid
    FROM reverse_geocode_results rgr
    JOIN gps_clusters gc ON rgr.cluster_id = gc.cluster_id
    JOIN subjects s ON gc.subject_id = s.id"
  
  conditions <- c()
  params <- list()
  
  if (!is.null(subid)) {
    conditions <- c(conditions, "s.subid = $1")
    params <- append(params, subid)
  }
  
  if (successful_only) {
    conditions <- c(conditions, "rgr.address IS NOT NULL AND rgr.address != ''")
  }
  
  if (length(conditions) > 0) {
    query <- paste(query, "WHERE", paste(conditions, collapse = " AND "))
  }
  
  query <- paste(query, "ORDER BY s.subid, rgr.cluster_id")
  
  # Only pass params if there are actual parameters
  if (length(params) > 0) {
    dbGetQuery(con, query, params)
  } else {
    dbGetQuery(con, query)
  }
}

#' Pull pre-joined analysis-ready data
pull_joined_data <- function(con, subid = NULL, include_failed_geocoding = FALSE) {
  query <- "
    SELECT 
      gc.cluster_id, gc.lat, gc.lon, gc.n_points,
      gc.total_visits, gc.total_duration_hours,
      gc.first_visit, gc.last_visit, s.subid,
      rgr.address as display_name, rgr.city, rgr.state, rgr.postcode,
      CASE WHEN rgr.address IS NOT NULL AND rgr.address != '' THEN true ELSE false END as geocoding_success,
      -- Add location classification helpers
      CASE 
        WHEN gc.total_visits >= (
          SELECT PERCENTILE_CONT(0.75) WITHIN GROUP (ORDER BY total_visits)
          FROM gps_clusters gc2 WHERE gc2.subject_id = gc.subject_id
        ) THEN 'Routine'
        WHEN gc.total_visits >= (
          SELECT PERCENTILE_CONT(0.50) WITHIN GROUP (ORDER BY total_visits)
          FROM gps_clusters gc2 WHERE gc2.subject_id = gc.subject_id  
        ) THEN 'Frequent'
        WHEN gc.total_visits >= (
          SELECT PERCENTILE_CONT(0.25) WITHIN GROUP (ORDER BY total_visits)
          FROM gps_clusters gc2 WHERE gc2.subject_id = gc.subject_id
        ) THEN 'Occasional'
        ELSE 'Rare'
      END as visit_frequency_category
    FROM gps_clusters gc
    JOIN subjects s ON gc.subject_id = s.id
    LEFT JOIN reverse_geocode_results rgr ON gc.cluster_id = rgr.cluster_id"
  
  conditions <- c()
  params <- list()
  
  if (!is.null(subid)) {
    conditions <- c(conditions, "s.subid = $1")
    params <- append(params, subid)
  }
  
  # Remove meaningless condition - include_failed_geocoding logic is handled by LEFT JOIN
  
  if (length(conditions) > 0) {
    query <- paste(query, "WHERE", paste(conditions, collapse = " AND "))
  }
  
  query <- paste(query, "ORDER BY s.subid, gc.total_visits DESC")
  
  # Only pass params if there are actual parameters
  if (length(params) > 0) {
    dbGetQuery(con, query, params)
  } else {
    dbGetQuery(con, query)
  }
}

#' Pull Madison zoning districts
#' @param con Database connection
#' @param zone_category Filter by zone category (optional)
#' @param include_geometry Whether to include spatial geometry (default: TRUE for mapping)
pull_zoning_data <- function(con, zone_category = NULL, include_geometry = TRUE) {
  if (include_geometry) {
    query <- "
      SELECT 
        objectid, zone_code, zone_category, area_sqm,
        ST_AsText(geom) as geometry_wkt,
        ST_X(ST_Centroid(geom)) as center_lon,
        ST_Y(ST_Centroid(geom)) as center_lat
      FROM madison_zoning_districts"
  } else {
    query <- "
      SELECT 
        objectid, zone_code, zone_category, area_sqm,
        ST_X(ST_Centroid(geom)) as center_lon,
        ST_Y(ST_Centroid(geom)) as center_lat
      FROM madison_zoning_districts"
  }
  
  params <- list()
  if (!is.null(zone_category)) {
    query <- paste(query, "WHERE zone_category = $1")
    params <- list(zone_category)
  }
  
  query <- paste(query, "ORDER BY zone_category, zone_code")
  
  # Execute query
  if (length(params) > 0) {
    result <- dbGetQuery(con, query, params)
  } else {
    result <- dbGetQuery(con, query)
  }
  
  # Convert WKT to sf geometry if requested
  if (include_geometry && nrow(result) > 0 && "geometry_wkt" %in% names(result)) {
    result <- result |>
      mutate(geometry = st_as_sfc(geometry_wkt, crs = 4326)) |>
      select(-geometry_wkt) |>
      st_as_sf()
  }

  result
}

#' Pull ADI block group data
#' @param con Database connection
#' @param state_filter Optional state postal codes to filter (e.g., c("WI", "IL"))
#' @param deprivation_threshold Filter to ADI >= threshold (e.g., 70 for high deprivation)
#' @param include_geometry Whether to include spatial geometry (default: TRUE for mapping)
pull_adi_data <- function(con, state_filter = NULL, deprivation_threshold = NULL, include_geometry = TRUE) {
  if (include_geometry) {
    query <- "
      SELECT
        fips_2020, state_postal, state_fips, county_fips, tract_fips, block_group,
        adi_national_percentile, adi_state_decile,
        area_sqm, adi_year, data_source, data_coverage,
        ST_AsText(geom) as geometry_wkt,
        ST_X(ST_Centroid(geom)) as center_lon,
        ST_Y(ST_Centroid(geom)) as center_lat
      FROM adi_block_groups"
  } else {
    query <- "
      SELECT
        fips_2020, state_postal,
        adi_national_percentile, adi_state_decile,
        ST_X(ST_Centroid(geom)) as center_lon,
        ST_Y(ST_Centroid(geom)) as center_lat
      FROM adi_block_groups"
  }

  conditions <- c()
  if (!is.null(state_filter)) {
    state_list <- paste0("'", state_filter, "'", collapse = ",")
    conditions <- c(conditions, sprintf("state_postal IN (%s)", state_list))
  }
  if (!is.null(deprivation_threshold)) {
    conditions <- c(conditions, sprintf("adi_national_percentile >= %d", deprivation_threshold))
  }

  if (length(conditions) > 0) {
    query <- paste(query, "WHERE", paste(conditions, collapse = " AND "))
  }

  query <- paste(query, "ORDER BY state_postal, fips_2020")

  result <- dbGetQuery(con, query)

  # Convert to sf if geometry requested
  if (include_geometry && nrow(result) > 0 && "geometry_wkt" %in% names(result)) {
    result <- result |>
      mutate(geometry = st_as_sfc(geometry_wkt, crs = 4326)) |>
      select(-geometry_wkt) |>
      st_as_sf()
  }

  result
}