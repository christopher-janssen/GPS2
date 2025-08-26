# scripts/r/data_operations.R
# All data insertion and manipulation operations - refactored with utilities
# NO table creation logic here - tables should exist from bootstrap

source("scripts/r/global_setup.R")
source("scripts/r/database.R")
source("scripts/r/gps_processing.R")

# ==============================================================================
# DATA INSERTION
# ==============================================================================

#' Insert GPS stationary points to PostGIS database
#'
#' @description
#' Inserts GPS stationary points into the PostGIS database using batch
#' processing for efficiency. Includes data validation and optional cleanup.
#'
#' @param stationary_data Data frame containing stationary GPS points with
#'   required columns: subid, lat, lon, dttm_obs
#' @param clear_existing Logical whether to delete existing data before
#'   insertion (except subid 999). Default: FALSE
#'
#' @return Integer number of points successfully inserted
#'
#' @details
#' Processing steps:
#' 1. Validates required columns and data quality
#' 2. Optionally clears existing data
#' 3. Prepares data with PostGIS geometry and metadata
#' 4. Inserts in batches with progress reporting
#'
#' @examples
#' \dontrun{
#' # Insert new stationary points
#' count <- insert_stationary_points(stationary_gps)
#' 
#' # Replace all existing data
#' count <- insert_stationary_points(stationary_gps, clear_existing = TRUE)
#' }
#'
#' @export
insert_stationary_points <- function(stationary_data, clear_existing = FALSE) {
  cat("Inserting", nrow(stationary_data), "stationary points...\n")
  
  # Validation
  validate_gps_data(stationary_data, c("subid", "lat", "lon", "dttm_obs"))
  
  # Optional: clear existing data
  if (clear_existing) {
    cat("Clearing existing data...\n")
    execute_gps2_db("DELETE FROM gps2.gps_stationary_points WHERE subid != 999;")
  }
  
  # Prepare data
  gps_prepared <- prepare_gps_for_insert(stationary_data)
  
  # Insert using batch utility
  total_inserted <- batch_insert_with_progress(
    gps_prepared,
    insert_gps_batch,
    operation_name = "Inserting GPS points"
  )
  
  cat("✅ Successfully inserted", total_inserted, "stationary points\n")
  return(total_inserted)
}

#' Prepare GPS data for database insertion
#'
#' @description
#' Transforms GPS data frame to match database schema requirements.
#' Adds necessary columns and ensures proper data types.
#'
#' @param stationary_data Raw stationary GPS data frame
#'
#' @return Prepared data frame with database-ready columns:
#'   subid, lat, lon, dttm_obs, dist, duration, speed, transit,
#'   movement_state, date_observed
#'
#' @keywords internal
prepare_gps_for_insert <- function(stationary_data) {
  stationary_data |>
    mutate(
      date_observed = as.Date(dttm_obs),
      movement_state = "stationary"
    ) |>
    select(subid, lat, lon, dttm_obs, dist, duration, speed, transit, 
           movement_state, date_observed)
}

#' Helper: Insert GPS batch to database
insert_gps_batch <- function(con, batch) {
  
  # Create vectorized insert values
  batch_values <- paste0(
    "(", batch$subid, ", ",
    "ST_SetSRID(ST_MakePoint(", batch$lon, ", ", batch$lat, "), 4326), ",
    batch$lat, ", ", batch$lon, ", ",
    "'", batch$dttm_obs, "', ",
    coalesce_sql(batch$dist), ", ",
    coalesce_sql(batch$duration), ", ",
    coalesce_sql(batch$speed), ", ",
    "'", batch$transit, "', ",
    "'", batch$movement_state, "', ",
    "'", batch$date_observed, "')"
  )
  
  insert_sql <- paste0(
    "INSERT INTO gps2.gps_stationary_points ",
    "(subid, location, lat, lon, dttm_obs, dist, duration, speed, ",
    "transit, movement_state, date_observed) VALUES ",
    paste(batch_values, collapse = ", "), ";"
  )
  
  dbExecute(con, insert_sql)
}

#' Insert location clusters to PostGIS database
#'
#' @description
#' Inserts location clustering results into the PostGIS database with
#' transactional safety and conflict resolution.
#'
#' @param cluster_data Data frame containing cluster analysis results with
#'   required columns: cluster, subid, lat, lon, n_points, total_visits,
#'   total_duration_hours, unique_days
#' @param clear_existing Logical whether to delete all existing clusters
#'   before insertion. Default: FALSE
#'
#' @return Integer number of clusters successfully inserted, or NULL on failure
#'
#' @details
#' Uses ON CONFLICT DO UPDATE for upsert behavior. Existing clusters
#' are updated with new statistics if they already exist.
#'
#' @examples
#' \dontrun{
#' # Insert new clusters
#' count <- insert_cluster_data(cluster_results)
#' 
#' # Replace all clusters
#' count <- insert_cluster_data(cluster_results, clear_existing = TRUE)
#' }
#'
#' @export
insert_cluster_data <- function(cluster_data, clear_existing = FALSE) {
  cat("Inserting", nrow(cluster_data), "location clusters...\n")
  
  # Validation
  validate_cluster_data(cluster_data)
  
  if (clear_existing) {
    execute_gps2_db("DELETE FROM gps2.location_clusters;")
  }
  
  # Prepare cluster data
  clusters_prepared <- prepare_clusters_for_insert(cluster_data)
  
  # Insert using transaction utility
  result <- with_gps2_transaction(function(con) {
    insert_clusters_batch(con, clusters_prepared)
    return(nrow(clusters_prepared))
  }, "Failed to insert cluster data")
  
  if (!is.null(result)) {
    cat("✅ Successfully inserted", result, "clusters\n")
  }
  
  return(result)
}

#' Helper: Validate cluster data structure
validate_cluster_data <- function(cluster_data) {
  required_cols <- c("cluster", "subid", "lat", "lon", "n_points", 
                     "total_visits", "total_duration_hours", "unique_days")
  validate_gps_data(cluster_data, required_cols)
}

#' Helper: Prepare cluster data for insertion
prepare_clusters_for_insert <- function(cluster_data) {
  cluster_data |>
    select(cluster_id = cluster, subid, lat, lon, n_points, first_visit, 
           last_visit, total_visits, total_duration_hours, unique_days)
}

#' Helper: Insert clusters in batch
insert_clusters_batch <- function(con, clusters_prepared) {
  
  for (i in 1:nrow(clusters_prepared)) {
    row <- clusters_prepared[i, ]
    
    insert_sql <- "
    INSERT INTO gps2.location_clusters 
      (cluster_id, subid, location, lat, lon, n_points, first_visit, 
       last_visit, total_visits, total_duration_hours, unique_days)
    VALUES ($1, $2, ST_SetSRID(ST_MakePoint($4, $3), 4326), $3, $4, $5, $6, $7, $8, $9, $10)
    ON CONFLICT (subid, cluster_id) DO UPDATE SET
      location = EXCLUDED.location,
      lat = EXCLUDED.lat,
      lon = EXCLUDED.lon,
      n_points = EXCLUDED.n_points,
      total_visits = EXCLUDED.total_visits,
      total_duration_hours = EXCLUDED.total_duration_hours,
      unique_days = EXCLUDED.unique_days;
    "
    
    dbExecute(con, insert_sql, list(
      row$cluster_id, row$subid, row$lat, row$lon, row$n_points,
      row$first_visit, row$last_visit, row$total_visits,
      row$total_duration_hours, row$unique_days
    ))
  }
}

# ==============================================================================
# DATA PROCESSING WORKFLOWS
# ==============================================================================

#' Complete GPS data loading workflow from CSV to PostGIS
#'
#' @description
#' End-to-end workflow that loads GPS data from CSV file, processes it
#' through the filtering pipeline, and inserts stationary points into PostGIS.
#'
#' @param csv_file_path Character string path to CSV file containing raw GPS data.
#'   Must include columns: subid, lat, lon, time
#' @param clear_existing Logical whether to clear existing database records
#'   before loading new data. Default: FALSE
#'
#' @return Integer number of stationary points successfully inserted
#'
#' @details
#' Processing pipeline:
#' 1. Validates file exists and loads CSV
#' 2. Processes GPS data (filtering, speed calculation, movement classification)
#' 3. Extracts stationary points
#' 4. Inserts to PostGIS database with batch processing
#'
#' @examples
#' \dontrun{
#' # Load GPS data from file
#' points_loaded <- load_gps_data_to_postgis("data/gps_tracks.csv")
#' 
#' # Replace existing data
#' points_loaded <- load_gps_data_to_postgis("data/gps_tracks.csv", 
#'                                           clear_existing = TRUE)
#' }
#'
#' @export
load_gps_data_to_postgis <- function(csv_file_path, clear_existing = FALSE) {
  cat("Loading GPS data from", csv_file_path, "\n")
  
  # Validate file exists
  if (!file.exists(csv_file_path)) {
    stop("File not found: ", csv_file_path)
  }
  
  # Load and process GPS data
  gps_raw <- read_csv(csv_file_path, show_col_types = FALSE)
  
  cat("Processing", nrow(gps_raw), "raw GPS points...\n")
  gps_processed <- process_gps(gps_raw)
  
  stationary_data <- get_stationary(gps_processed)
  cat("Identified", nrow(stationary_data), "stationary points\n")
  
  # Insert to PostGIS using utilities
  total_inserted <- insert_stationary_points(stationary_data, clear_existing = clear_existing)
  
  return(total_inserted)
}

#' Complete clustering workflow for all participants
#'
#' @description
#' Runs location clustering analysis for all participants in the database.
#' Discovers participants automatically and processes each individually.
#'
#' @param eps Numeric clustering distance threshold in meters. If NULL,
#'   uses default from configuration (typically 50m)
#' @param update_existing Logical whether to replace existing clustering
#'   results. Default: FALSE
#'
#' @return Data frame containing all clustering results across participants,
#'   or empty data frame if no clusters found
#'
#' @details
#' Processing steps:
#' 1. Discovers all participants from GPS data (excluding test participant 999)
#' 2. Runs clustering algorithm for each participant individually
#' 3. Combines results across all participants
#' 4. Inserts final results to database
#'
#' Clustering algorithm uses DBSCAN-style approach with duration weighting.
#'
#' @examples
#' \dontrun{
#' # Cluster with default settings
#' all_clusters <- cluster_all_participants()
#' 
#' # Use custom distance threshold
#' all_clusters <- cluster_all_participants(eps = 75, update_existing = TRUE)
#' }
#'
#' @export
cluster_all_participants <- function(eps = NULL, update_existing = FALSE) {
  cat("Starting clustering analysis for all participants...\n")
  
  # Use config default if not specified
  if (is.null(eps)) eps <- get_config("clustering", "default_eps")
  
  # Get unique participants using query builder
  participants <- query_gps2_db(
    "SELECT DISTINCT subid FROM gps2.gps_stationary_points WHERE subid != 999 ORDER BY subid;"
  )$subid
  
  cat("Found", length(participants), "participants\n")
  
  # Process using clustering utilities
  all_clusters <- process_all_participants_clustering(participants, eps)
  
  # Insert cluster results using utilities
  if (nrow(all_clusters) > 0) {
    insert_cluster_data(all_clusters, clear_existing = update_existing)
    cat("✅ Clustering complete:", nrow(all_clusters), "total clusters\n")
  }
  
  return(all_clusters)
}

#' Helper: Process all participants for clustering
process_all_participants_clustering <- function(participants, eps) {
  all_clusters <- tibble()
  
  for (participant_id in participants) {
    tryCatch({
      # Get participant data using query builder
      query <- build_gps_query(participant_ids = participant_id, movement_state = "stationary")
      participant_data <- query_gps2_db(query)
      
      if (nrow(participant_data) > 0) {
        # Use clustering utility
        clusters <- cluster_stationary_gps_env(participant_data, participant_id, eps)
        if (nrow(clusters) > 0) {
          all_clusters <- bind_rows(all_clusters, clusters)
          cat("Participant", participant_id, ":", nrow(clusters), "clusters\n")
        }
      }
      
    }, error = function(e) {
      cat("Error clustering participant", participant_id, ":", e$message, "\n")
    })
  }
  
  return(all_clusters)
}

# ==============================================================================
# BATCH PROCESSING WORKFLOWS
# ==============================================================================

#' Batch process multiple GPS files
#'
#' @description
#' Processes multiple GPS CSV files in sequence using the complete
#' GPS processing pipeline. Useful for batch imports.
#'
#' @param file_pattern Character string glob pattern to match files.
#'   Default: "*.csv"
#' @param data_directory Character string directory path to search.
#'   Default: "data/"
#'
#' @return Integer total number of GPS points inserted across all files
#'
#' @details
#' - First file clears existing data, subsequent files append
#' - Each file processed independently with error handling
#' - Failed files are skipped with error messages
#' - Returns cumulative insertion count
#'
#' @examples
#' \dontrun{
#' # Process all CSV files in data directory
#' total_points <- load_multiple_gps_files()
#' 
#' # Process specific pattern in custom directory
#' total_points <- load_multiple_gps_files("participant_*.csv", "raw_data/")
#' }
#'
#' @export
load_multiple_gps_files <- function(file_pattern = "*.csv", data_directory = "data/") {
  
  # Find all matching files
  gps_files <- list.files(data_directory, pattern = file_pattern, full.names = TRUE)
  
  if (length(gps_files) == 0) {
    cat("No files found matching pattern:", file_pattern, "\n")
    return(0)
  }
  
  cat("Found", length(gps_files), "GPS files to process\n")
  
  total_inserted <- 0
  
  for (i in seq_along(gps_files)) {
    file <- gps_files[i]
    cat("\nProcessing file", i, "of", length(gps_files), ":", basename(file), "\n")
    
    tryCatch({
      inserted <- load_gps_data_to_postgis(file, clear_existing = (i == 1))
      total_inserted <- total_inserted + inserted
      
    }, error = function(e) {
      cat("Error processing", basename(file), ":", e$message, "\n")
    })
  }
  
  cat("\n✅ Batch processing complete:", total_inserted, "total points inserted\n")
  return(total_inserted)
}

#' Complete end-to-end GPS processing workflow
#'
#' @description
#' Master workflow that performs complete GPS analysis pipeline from
#' raw CSV data to final geocoded clusters with summary reporting.
#'
#' @param csv_file_path Character string path to GPS CSV file
#' @param run_clustering Logical whether to run clustering analysis.
#'   Default: TRUE
#' @param run_geocoding Logical whether to run reverse geocoding.
#'   Default: TRUE
#' @param eps Optional numeric clustering distance threshold in meters.
#'   Uses configuration default if NULL
#'
#' @return Logical TRUE if workflow completed successfully, FALSE otherwise
#'
#' @details
#' Complete workflow includes:
#' 1. GPS data loading and processing (with existing data clearing)
#' 2. Location clustering analysis (if enabled)
#' 3. Reverse geocoding of clusters (if enabled)
#' 4. Final database summary and reporting
#'
#' Each step depends on the previous step's success. The workflow stops
#' early if any step fails or produces no results.
#'
#' @examples
#' \dontrun{
#' # Complete analysis pipeline
#' success <- process_gps_data_complete("data/tracks.csv")
#' 
#' # Skip geocoding for faster processing
#' success <- process_gps_data_complete("data/tracks.csv", 
#'                                      run_geocoding = FALSE)
#' 
#' # Custom clustering threshold
#' success <- process_gps_data_complete("data/tracks.csv", eps = 100)
#' }
#'
#' @export
process_gps_data_complete <- function(csv_file_path, run_clustering = TRUE, 
                                      run_geocoding = TRUE, eps = NULL) {
  
  cat("GPS2 Complete Processing Workflow\n")
  cat("=================================\n")
  
  # Step 1: Load GPS data
  cat("\n1. Loading GPS data...\n")
  points_inserted <- load_gps_data_to_postgis(csv_file_path, clear_existing = TRUE)
  
  if (points_inserted == 0) {
    cat("No data loaded, stopping.\n")
    return(FALSE)
  }
  
  # Step 2: Run clustering
  if (run_clustering) {
    cat("\n2. Running clustering analysis...\n")
    clusters <- cluster_all_participants(eps = eps, update_existing = TRUE)
    
    if (nrow(clusters) == 0) {
      cat("No clusters created, skipping geocoding.\n")
      run_geocoding <- FALSE
    }
  }
  
  # Step 3: Run geocoding
  if (run_geocoding) {
    cat("\n3. Running reverse geocoding...\n")
    reverse_geocode_clusters_db(update_existing = TRUE)
  }
  
  # Step 4: Summary
  cat("\n4. Final summary:\n")
  summary <- get_database_summary()
  cat("GPS points:", format(summary$total_points, big.mark = ","), "\n")
  cat("Participants:", summary$participants, "\n")
  cat("Clusters:", summary$clusters, "\n")
  cat("Geocoded locations:", summary$geocoded, "\n")
  
  cat("\n✅ Complete processing workflow finished!\n")
  return(TRUE)
}