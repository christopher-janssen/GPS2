# scripts/r/gps_data_transfer.R
# functions to transfer GPS data from R/CSV into PostGIS tables

library(DBI)
library(dplyr)
source("scripts/r/postgis_connection.R")

# function to insert stationary GPS points into PostGIS
insert_stationary_points <- function(stationary_data, clear_existing = FALSE) {
  cat("Transferring stationary GPS data to PostGIS...\n")
  
  con <- connect_to_gps2_db()
  
  tryCatch({
    # optional: clear existing data for fresh start
    if (clear_existing) {
      cat("Clearing existing stationary points...\n")
      dbExecute(con, "DELETE FROM gps2.gps_stationary_points;")
    }
    
    # prepare data for PostGIS
    gps_prepared <- stationary_data |>
      mutate(
        # create date column from timestamp
        date_observed = as.Date(dttm_obs),
        # ensure movement_state is consistent
        movement_state = "stationary"
      ) |>
      # select only columns that exist in our table
      select(subid, lat, lon, dttm_obs, dist, duration, speed, transit, 
             movement_state, date_observed)
    
    cat("Preparing", nrow(gps_prepared), "stationary points for", 
        length(unique(gps_prepared$subid)), "participants\n")
    
    # insert data in batches (more efficient for large datasets)
    batch_size <- 1000
    total_inserted <- 0
    
    for (i in seq(1, nrow(gps_prepared), batch_size)) {
      end_idx <- min(i + batch_size - 1, nrow(gps_prepared))
      batch <- gps_prepared[i:end_idx, ]
      
      # create SQL INSERT statement with PostGIS geometry
      insert_sql <- "
      INSERT INTO gps2.gps_stationary_points 
        (subid, location, lat, lon, dttm_obs, dist, duration, speed, transit, movement_state, date_observed)
      VALUES ($1, ST_SetSRID(ST_MakePoint($3, $2), 4326), $2, $3, $4, $5, $6, $7, $8, $9, $10)
      "
      
      # prepare the batch data
      batch_list <- list(
        batch$subid,
        batch$lat, 
        batch$lon,
        batch$dttm_obs,
        batch$dist,
        batch$duration, 
        batch$speed,
        batch$transit,
        batch$movement_state,
        batch$date_observed
      )
      
      # execute batch insert
      dbExecute(con, insert_sql, batch_list)
      total_inserted <- total_inserted + nrow(batch)
      
      cat("✅ Inserted batch", ceiling(i/batch_size), "- Total:", total_inserted, "points\n")
    }
    
    # verify the insert worked
    count_check <- dbGetQuery(con, "SELECT COUNT(*) as count FROM gps2.gps_stationary_points;")
    cat("Database now contains", count_check$count, "stationary points\n")
    
    # show summary by participant
    summary_check <- dbGetQuery(con, "
      SELECT subid, COUNT(*) as points, MIN(dttm_obs) as first_obs, MAX(dttm_obs) as last_obs
      FROM gps2.gps_stationary_points 
      GROUP BY subid 
      ORDER BY subid;
    ")
    
    cat("📈 Summary by participant:\n")
    print(summary_check)
    
  }, error = function(e) {
    cat("❌ Error transferring data:", e$message, "\n")
  }, finally = {
    dbDisconnect(con)
  })
}

# function to insert cluster results into PostGIS
insert_cluster_data <- function(cluster_data, clear_existing = FALSE) {
  cat("Transferring cluster data to PostGIS...\n")
  
  con <- connect_to_gps2_db()
  
  tryCatch({
    if (clear_existing) {
      cat("Clearing existing clusters...\n")
      dbExecute(con, "DELETE FROM gps2.location_clusters;")
    }
    
    # prepare cluster data
    clusters_prepared <- cluster_data |>
      select(cluster_id = cluster, subid, lat, lon, n_points, first_visit, 
             last_visit, total_visits, total_duration_hours, unique_days)
    
    cat("Preparing", nrow(clusters_prepared), "clusters for", 
        length(unique(clusters_prepared$subid)), "participants\n")
    
    # insert cluster data
    for (i in 1:nrow(clusters_prepared)) {
      row <- clusters_prepared[i, ]
      
      insert_sql <- "
      INSERT INTO gps2.location_clusters 
        (cluster_id, subid, location, lat, lon, n_points, first_visit, last_visit, 
         total_visits, total_duration_hours, unique_days)
      VALUES ($1, $2, ST_SetSRID(ST_MakePoint($4, $3), 4326), $3, $4, $5, $6, $7, $8, $9, $10)
      "
      
      dbExecute(con, insert_sql, list(
        row$cluster_id, row$subid, row$lat, row$lon, row$n_points,
        row$first_visit, row$last_visit, row$total_visits, 
        row$total_duration_hours, row$unique_days
      ))
    }
    
    # verify results
    count_check <- dbGetQuery(con, "SELECT COUNT(*) as count FROM gps2.location_clusters;")
    cat("Database now contains", count_check$count, "location clusters\n")
    
    cluster_summary <- dbGetQuery(con, "
      SELECT subid, COUNT(*) as clusters, 
             ROUND(AVG(total_visits)::numeric, 1) as avg_visits,
             ROUND(AVG(total_duration_hours)::numeric, 1) as avg_hours
      FROM gps2.location_clusters 
      GROUP BY subid 
      ORDER BY subid;
    ")
    
    cat("Cluster summary by participant:\n")
    print(cluster_summary)
    
  }, error = function(e) {
    cat("❌ Error transferring clusters:", e$message, "\n")
  }, finally = {
    dbDisconnect(con)
  })
}