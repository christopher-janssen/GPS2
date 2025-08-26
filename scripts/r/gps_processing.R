# gps_processing.R
# GPS data processing for location clustering
# Adapted from lab approach (thanks claire and john)

library(geosphere, include.only = c("distHaversine"))

process_gps <- function(gps_data,
                        speed_threshold_mph = 100,
                        stationary_threshold_mph = 4) {
  
  # convert time to POSIXct and set timezone
  gps_data <- gps_data |>
    mutate(
      dttm_obs = as.POSIXct(time, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      dttm_obs = with_tz(dttm_obs, tz = "America/Chicago")
    ) |>
    arrange(subid, dttm_obs)
  
  # calculate distances, durations, and speeds
  # specifically calculate Haversine distance (great circle distance) between consecutive GPS points
  gps_processed <- gps_data |>
    group_by(subid) |>
    mutate(
      dist_m = ifelse(row_number() == 1, 0,
                      distHaversine(cbind(lag(lon), lag(lat)), 
                                    cbind(lon, lat))),
      dist = dist_m / 1609.344,
      duration = ifelse(row_number() == 1, 0,
                        as.numeric(difftime(dttm_obs, lag(dttm_obs), units = "mins"))),
      speed = ifelse(duration > 0, dist / (duration / 60), 0)
    ) |>
    ungroup()
  
  # apply lab filtering rules
  gps_filtered <- gps_processed |>
    mutate(
      duration = if_else(dist > 0.01 & duration == 0, NA_real_, duration),
      duration = if_else(speed > speed_threshold_mph, NA_real_, duration),
      duration = if_else(duration > 0.5 & dist > 0.31, NA_real_, duration),
      valid_record = !is.na(duration),
      speed = if_else(valid_record & duration > 0, dist / (duration / 60), 0)
    ) |>
    filter(valid_record) |>
    select(-valid_record)
  
  # movement state classification
  gps_classified <- gps_filtered |>
    mutate(
      movement_state = if_else(speed <= stationary_threshold_mph, "stationary", "transition"),
      transit = if_else(speed <= stationary_threshold_mph, "no", "yes")
    )
  
  return(gps_classified)
}

# Additional functions for processing FollowMe GPS data
# To be added to gps_filtering.R

process_followmee_gps <- function(followmee_data,
                                  speed_threshold_mph = 100,
                                  stationary_threshold_mph = 4) {
  
  # Map FollowMe columns to expected format and prepare data
  gps_data <- followmee_data |>
    # Map columns to match the expected format
    rename(
      original_name = Name,   # Keep original name for reference
      time = Date,            # Use Date as timestamp
      lat = Lat,              # Latitude
      lon = Lng               # Longitude (note: script expects 'lon' not 'lng')
    ) |>
    # Create participant IDs starting from 501
    mutate(
      subid = as.numeric(as.factor(original_name)) + 500
    ) |>
    # Remove rows with missing essential data
    filter(!is.na(lat), !is.na(lon), !is.na(time)) |>
    # Convert time to POSIXct and set timezone
    mutate(
      dttm_obs = case_when(
        # Handle different possible date formats
        grepl("T", time) ~ as.POSIXct(time, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
        grepl("/", time) ~ as.POSIXct(time, format = "%m/%d/%Y %H:%M:%S", tz = "UTC"),
        TRUE ~ as.POSIXct(time, tz = "UTC")
      ),
      dttm_obs = with_tz(dttm_obs, tz = "America/Chicago")
    ) |>
    arrange(subid, dttm_obs)
  
  # Calculate distances, durations, and speeds
  # Following the same logic as the original process_gps function
  gps_processed <- gps_data |>
    group_by(subid) |>
    mutate(
      # Calculate Haversine distance between consecutive GPS points
      dist_m = ifelse(row_number() == 1, 0,
                      distHaversine(cbind(lag(lon), lag(lat)), 
                                    cbind(lon, lat))),
      # Convert to miles
      dist = dist_m / 1609.344,
      # Calculate duration in minutes
      duration = ifelse(row_number() == 1, 0,
                        as.numeric(difftime(dttm_obs, lag(dttm_obs), units = "mins"))),
      # Calculate speed in mph
      speed = ifelse(duration > 0, dist / (duration / 60), 0)
    ) |>
    ungroup()
  
  # Apply lab filtering rules (same as original)
  gps_filtered <- gps_processed |>
    mutate(
      duration = if_else(dist > 0.01 & duration == 0, NA_real_, duration),
      duration = if_else(speed > speed_threshold_mph, NA_real_, duration),
      duration = if_else(duration > 0.5 & dist > 0.31, NA_real_, duration),
      valid_record = !is.na(duration),
      speed = if_else(valid_record & duration > 0, dist / (duration / 60), 0)
    ) |>
    filter(valid_record) |>
    select(-valid_record)
  
  # Movement state classification
  gps_classified <- gps_filtered |>
    mutate(
      movement_state = if_else(speed <= stationary_threshold_mph, "stationary", "transition"),
      transit = if_else(speed <= stationary_threshold_mph, "no", "yes")
    )
  
  return(gps_classified)
}

# Updated get_stationary function with optional file writing
get_stationary <- function(processed_data, write_file = FALSE, output_path = NULL) {
  stationary_points <- processed_data |>
    filter(movement_state == "stationary") |>
    select(subid, lat, lon, dttm_obs, dist, duration, speed, transit, movement_state)
  
  # Write to CSV file if requested
  if (write_file) {
    if (is.null(output_path)) {
      # Use default path if none provided
      output_path <- here::here(path_processed, "gps2_stationary.csv")
    }
    readr::write_csv(stationary_points, output_path)
    cat("Stationary points written to:", output_path, "\n")
  }
  
  return(stationary_points)
}