# gps_filtering.R
# GPS data processing for location clustering
# Adapted from lab approach (thanks claire and john)

library(dplyr)
library(geosphere)
library(lubridate)

process_gps <- function(gps_data,
                        speed_threshold_mph = 100,
                        stationary_threshold_mph = 4) {
  
  # convert time to POSIXct and set timezone
  gps_data <- gps_data %>%
    mutate(
      dttm_obs = as.POSIXct(time, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      dttm_obs = with_tz(dttm_obs, tz = "America/Chicago")
    ) %>%
    arrange(subid, dttm_obs)
  
  # calculate distances, durations, and speeds
  # specifically calculate Haversine distance (great circle distance) between consecutive GPS points
  gps_processed <- gps_data %>%
    group_by(subid) %>%
    mutate(
      dist_m = ifelse(row_number() == 1, 0,
                      distHaversine(cbind(lag(lon), lag(lat)), 
                                    cbind(lon, lat))),
      dist = dist_m / 1609.344,
      duration = ifelse(row_number() == 1, 0,
                        as.numeric(difftime(dttm_obs, lag(dttm_obs), units = "mins"))),
      speed = ifelse(duration > 0, dist / (duration / 60), 0)
    ) %>%
    ungroup()
  
  # apply lab filtering rules
  gps_filtered <- gps_processed %>%
    mutate(
      duration = if_else(dist > 0.01 & duration == 0, NA_real_, duration),
      duration = if_else(speed > speed_threshold_mph, NA_real_, duration),
      duration = if_else(duration > 0.5 & dist > 0.31, NA_real_, duration),
      valid_record = !is.na(duration),
      speed = if_else(valid_record & duration > 0, dist / (duration / 60), 0)
    ) %>%
    filter(valid_record) %>%
    select(-valid_record)
  
  # movement state classification
  gps_classified <- gps_filtered %>%
    mutate(
      movement_state = if_else(speed <= stationary_threshold_mph, "stationary", "transition"),
      transit = if_else(speed <= stationary_threshold_mph, "no", "yes")
    )
  
  return(gps_classified)
}

# extract and write out stationary points
get_stationary <- function(processed_data) {
  stationary_points <- processed_data %>%
    filter(movement_state == "stationary") %>%
    select(subid, lat, lon, dttm_obs, dist, duration, speed, transit, movement_state)
  
  # Write to CSV file
  readr::write_csv(stationary_points, here::here(path_processed, "gps2_stationary.csv"))
  
  return(stationary_points)
}
