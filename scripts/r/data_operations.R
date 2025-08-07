# scripts/r/data_operations.R
# All data insertion and manipulation operations
# NO table creation logic here - tables should exist from bootstrap

source("scripts/r/database.R")
source("scripts/r/gps_processing.R")

# ==============================================================================
# DATA INSERTION
# ==============================================================================

# Insert GPS stationary points (assumes table exists)
insert_stationary_points <- function(stationary_data, clear_existing = FALSE) {
  cat("Inserting", nrow(stationary_data), "stationary points...\n")
  
  # Optional: clear existing data
  if (clear_existing) {
    cat("Clearing existing data...\n")
    execute_gps2_db("DELETE FROM gps2.gps_stationary_points WHERE subid != 999;")
  }
  
  # Prepare data
  gps_prepared <- stationary_data |>
    mutate(
      date_observed = as.Date(dttm_obs),
      movement_state = "stationary"
    ) |>
    select(subid, lat, lon, dttm_obs, dist, duration, speed, transit, 
           movement_state, date_observed)
  
  # Insert in batches
  batch_size <- 1000
  total_inserted <- 0
  
  con <- connect_gps2_db()
  tryCatch({
    for (i in seq(1, nrow(gps_prepared), batch_size)) {
      end_idx <- min(i + batch_size - 1, nrow(gps_prepared))
      batch <- gps_prepared[i:end_idx, ]
      
      # Use vectorized insert
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
      total_inserted <- total_inserted + nrow(batch)
      
      if (i %% (batch_size * 5) == 1) {
        cat("Inserted", total_inserted, "of", nrow(gps_prepared), "points\n")
      }
    }
    
    cat("✅ Successfully inserted", total_inserted, "stationary points\n")
    
  }, error = function(e) {
    cat("❌ Error inserting data:", e$message, "\n")
  }, finally = {
    disconnect_gps2_db(con)
  })
}

# Insert cluster data (assumes table exists)
insert_cluster_data <- function(cluster_data, clear_existing = FALSE) {
  cat("Inserting", nrow(cluster_data), "location clusters...\n")
  
  if (clear_existing) {
    execute_gps2_db("DELETE FROM gps2.location_clusters;")
  }
  
  # Prepare cluster data
  clusters_prepared <- cluster_data |>
    select(cluster_id = cluster, subid, lat, lon, n_points, first_visit, 
           last_visit, total_visits, total_duration_hours, unique_days)
  
  con <- connect_gps2_db()
  tryCatch({
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
    
    cat("✅ Successfully inserted cluster data\n")
    
  }, error = function(e) {
    cat("❌ Error inserting clusters:", e$message, "\n")
  }, finally = {
    disconnect_gps2_db(con)
  })
}

# ==============================================================================
# DATA PROCESSING WORKFLOWS
# ==============================================================================

# Complete workflow: CSV to PostGIS
load_gps_data_to_postgis <- function(csv_file_path, clear_existing = FALSE) {
  cat("Loading GPS data from", csv_file_path, "\n")
  
  # Load and process GPS data
  gps_raw <- read_csv(csv_file_path, show_col_types = FALSE)
  
  cat("Processing", nrow(gps_raw), "raw GPS points...\n")
  gps_processed <- process_gps(gps_raw)
  
  stationary_data <- get_stationary(gps_processed)
  cat("Identified", nrow(stationary_data), "stationary points\n")
  
  # Insert to PostGIS
  insert_stationary_points(stationary_data, clear_existing = clear_existing)
  
  return(nrow(stationary_data))
}

# Complete workflow: Clustering
cluster_all_participants <- function(eps = 50, update_existing = FALSE) {
  cat("Starting clustering analysis for all participants...\n")
  
  # Get unique participants
  participants <- query_gps2_db("SELECT DISTINCT subid FROM gps2.gps_stationary_points ORDER BY subid;")$subid
  
  cat("Found", length(participants), "participants\n")
  
  # Process each participant
  all_clusters <- data.frame()
  
  for (participant_id in participants) {
    tryCatch({
      # Get participant data
      participant_data <- query_gps2_db("
        SELECT subid, lat, lon, dttm_obs, dist, duration, speed, movement_state
        FROM gps2.gps_stationary_points 
        WHERE subid = $1 AND movement_state = 'stationary'
        ORDER BY dttm_obs;
      ", list(participant_id))
      
      if (nrow(participant_data) > 0) {
        clusters <- cluster_stationary_gps(participant_data, participant_id, eps)
        if (nrow(clusters) > 0) {
          all_clusters <- bind_rows(all_clusters, clusters)
          cat("Participant", participant_id, ":", nrow(clusters), "clusters\n")
        }
      }
      
    }, error = function(e) {
      cat("Error clustering participant", participant_id, ":", e$message, "\n")
    })
  }
  
  # Insert cluster results
  if (nrow(all_clusters) > 0) {
    insert_cluster_data(all_clusters, clear_existing = update_existing)
    cat("✅ Clustering complete:", nrow(all_clusters), "total clusters\n")
  }
  
  return(all_clusters)
}

# ==============================================================================
# UTILITY FUNCTIONS
# ==============================================================================

# Helper for SQL NULL handling
coalesce_sql <- function(x) {
  ifelse(is.na(x), "NULL", x)
}