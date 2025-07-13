# GPS DBSCAN Clustering for Stationary Points
# performs DBSCAN clustering on stationary GPS data points

library(dbscan)
library(dplyr)

cluster_stationary_gps <- function(gps_data, participant_id, eps = 50, min_pts = 5) {
  # Filter for specific participant and stationary points only
  participant_data <- gps_data %>%
    filter(subid == participant_id, movement_state == "stationary")
  
  if (nrow(participant_data) == 0) {
    stop(paste("No stationary data found for participant", participant_id))
  }
  
  # Extract coordinates for clustering
  coords <- participant_data %>%
    select(lon, lat) %>%
    as.matrix()
  
  # Convert lat/lon to approximate meters for DBSCAN
  mean_lat <- mean(coords[, 2])
  lat_scale <- 111000  # meters per degree latitude
  lon_scale <- 111000 * cos(mean_lat * pi / 180)  # meters per degree longitude
  
  coords_scaled <- cbind(coords[, 1] * lon_scale, coords[, 2] * lat_scale)
  
  # Run DBSCAN
  db_result <- dbscan(coords_scaled, eps = eps, minPts = min_pts)
  
  # Add cluster ID to the data
  participant_data$cluster <- db_result$cluster
  
  # Return representative points (cluster centers)
  representatives <- participant_data %>%
    filter(cluster != 0) %>%  # Exclude noise points
    group_by(cluster) %>%
    summarise(
      subid = first(subid),
      lat = mean(lat),
      lon = mean(lon),
      n_points = n(),
      first_visit = min(dttm_obs),
      last_visit = max(dttm_obs),
      .groups = "drop"
    )
  
  return(representatives)
}

# Usage:
# clustered_data <- cluster_stationary_gps(gps_data, participant_id = 19, eps = 50, min_pts = 5)
