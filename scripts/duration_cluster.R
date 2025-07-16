# duration_cluster.R
# day-by-day duration-based clustering for GPS data

library(dplyr)
library(geosphere)
library(lubridate)

cluster_stationary_gps <- function(gps_data, participant_id, eps = 20, min_pts = NULL) {
  
  # note: eps represents radius in meters, min_pts replaced with duration threshold
  radius_m <- eps
  min_duration_min <- 30  # Minimum duration for a meaningful stop within a day
  
  # filter for specific participant and stationary points only
  participant_data <- gps_data %>%
    filter(subid == participant_id, movement_state == "stationary") %>%
    mutate(date = as.Date(dttm_obs)) %>%
    arrange(dttm_obs)
  
  if (nrow(participant_data) == 0) {
    stop(paste("No stationary data found for participant", participant_id))
  }
  
  # process each day separately
  daily_clusters <- participant_data %>%
    group_by(date) %>%
    group_modify(~ {
      day_data <- .x
      day_data$daily_cluster <- 0
      cluster_id <- 1
      
      # cluster within this day
      for (i in 1:nrow(day_data)) {
        if (day_data$daily_cluster[i] != 0) next
        
        current_point <- day_data[i, ]
        
        # find remaining unassigned points within radius on this day
        remaining_points <- day_data[(i):nrow(day_data), ] %>%
          filter(daily_cluster == 0)
        
        if (nrow(remaining_points) == 0) next
        
        # calculate distances
        distances <- distHaversine(
          p1 = c(current_point$lon, current_point$lat),
          p2 = cbind(remaining_points$lon, remaining_points$lat)
        )
        
        # find points within radius
        nearby_indices <- which(distances <= radius_m)
        nearby_points <- remaining_points[nearby_indices, ]
        
        # check duration within this day
        if (nrow(nearby_points) >= 2) {
          time_span_min <- as.numeric(difftime(
            max(nearby_points$dttm_obs), 
            min(nearby_points$dttm_obs), 
            units = "mins"
          ))
          
          # if duration threshold met, assign cluster
          if (time_span_min >= min_duration_min) {
            original_indices <- which(day_data$dttm_obs %in% nearby_points$dttm_obs &
                                        day_data$daily_cluster == 0)
            day_data$daily_cluster[original_indices] <- cluster_id
            cluster_id <- cluster_id + 1
          }
        }
      }
      
      # return clustered day data
      day_data %>% filter(daily_cluster != 0)
    }) %>%
    ungroup()
  
  if (nrow(daily_clusters) == 0) {
    # return empty dataframe with correct structure if no clusters found
    return(data.frame(
      subid = numeric(0), lat = numeric(0), lon = numeric(0), 
      n_points = integer(0), first_visit = as.POSIXct(character(0)), 
      last_visit = as.POSIXct(character(0)), cluster = integer(0)
    ))
  }
  
  # create daily location summaries
  daily_locations <- daily_clusters %>%
    group_by(date, daily_cluster) %>%
    summarise(
      subid = first(subid),
      lat = mean(lat),
      lon = mean(lon),
      n_points = n(),
      start_time = min(dttm_obs),
      end_time = max(dttm_obs),
      duration_min = as.numeric(difftime(max(dttm_obs), min(dttm_obs), units = "mins")),
      .groups = "drop"
    )
  
  # aggregate daily locations across days based on geographic proximity
  if (nrow(daily_locations) == 0) {
    return(data.frame(
      subid = numeric(0), lat = numeric(0), lon = numeric(0), 
      n_points = integer(0), first_visit = as.POSIXct(character(0)), 
      last_visit = as.POSIXct(character(0)), cluster = integer(0)
    ))
  }
  
  daily_locations$final_cluster <- 0
  cluster_id <- 1
  
  for (i in 1:nrow(daily_locations)) {
    if (daily_locations$final_cluster[i] != 0) next
    
    current_location <- daily_locations[i, ]
    
    # find all locations (across all days) within radius
    distances_all <- distHaversine(
      p1 = c(current_location$lon, current_location$lat),
      p2 = cbind(daily_locations$lon, daily_locations$lat)
    )
    
    nearby_location_indices <- which(distances_all <= radius_m & daily_locations$final_cluster == 0)
    daily_locations$final_cluster[nearby_location_indices] <- cluster_id
    cluster_id <- cluster_id + 1
  }
  
  # create final cluster representatives
  representatives <- daily_locations %>%
    filter(final_cluster != 0) %>%
    group_by(final_cluster) %>%
    summarise(
      subid = first(subid),
      lat = mean(lat),
      lon = mean(lon),
      n_points = sum(n_points),  # total GPS points across all visits
      first_visit = min(start_time),
      last_visit = max(end_time),
      total_visits = n(),  # number of separate day-visits to this location
      total_duration_hours = sum(duration_min) / 60,
      unique_days = n_distinct(date),
      .groups = "drop"
    ) %>%
    rename(cluster = final_cluster)
  
  return(representatives)
}