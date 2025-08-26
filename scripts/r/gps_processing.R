# gps_processing.R
# GPS data processing for location clustering
# Adapted from lab approach (thanks claire and john)

library(geosphere, include.only = c("distHaversine"))

#' Process raw GPS data with filtering and movement classification
#'
#' @description
#' Comprehensive GPS data processing pipeline that converts timestamps,
#' calculates distances and speeds, applies lab filtering rules, and
#' classifies movement states. Adapted from lab methodology.
#'
#' @param gps_data A data frame with columns: subid, lat, lon, time
#' @param speed_threshold_mph Numeric maximum realistic speed in mph.
#'   Records exceeding this are filtered out. Default: 100
#' @param stationary_threshold_mph Numeric speed threshold in mph below which
#'   points are classified as stationary. Default: 4
#'
#' @return A processed data frame with additional columns:
#'   - dttm_obs: POSIXct timestamps in America/Chicago timezone
#'   - dist_m: Distance to previous point in meters (Haversine)
#'   - dist: Distance to previous point in miles
#'   - duration: Time elapsed since previous point in minutes
#'   - speed: Calculated speed in mph
#'   - movement_state: "stationary" or "transition"
#'   - transit: "yes" or "no" based on movement state
#'
#' @details
#' Processing steps:
#' 1. Convert timestamps to POSIXct with proper timezone handling
#' 2. Calculate Haversine distances between consecutive points
#' 3. Compute durations and speeds
#' 4. Apply lab filtering rules for data quality
#' 5. Classify movement states based on speed thresholds
#'
#' Filtering rules remove records where:
#' - Speed exceeds realistic maximum (default 100 mph)
#' - Duration/distance relationships are inconsistent
#' - Very short durations with significant distances
#'
#' @examples
#' \dontrun{
#' # Process GPS data with default settings
#' processed <- process_gps(raw_gps_data)
#' 
#' # Use custom thresholds
#' processed <- process_gps(raw_gps_data, 
#'                          speed_threshold_mph = 80,
#'                          stationary_threshold_mph = 3)
#' }
#'
#' @export
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

#' Process FollowMe GPS data format
#'
#' @description
#' Specialized GPS processing for FollowMe app data format. Maps FollowMe
#' column names to standard format, creates participant IDs starting from 501,
#' then applies the same processing pipeline as process_gps().
#'
#' @param followmee_data A data frame with FollowMe format columns:
#'   Name, Date, Lat, Lng
#' @param speed_threshold_mph Numeric maximum realistic speed in mph.
#'   Default: 100
#' @param stationary_threshold_mph Numeric speed threshold in mph for
#'   stationary classification. Default: 4
#'
#' @return Processed data frame with same structure as process_gps() output
#'   plus original_name column preserving FollowMe participant names
#'
#' @details
#' Column mapping:
#' - Name -> original_name (preserved) + subid (numeric starting at 501)
#' - Date -> time
#' - Lat -> lat
#' - Lng -> lon
#' 
#' Handles multiple date formats automatically:
#' - ISO format with T: "YYYY-MM-DDTHH:MM:SSZ"
#' - US format with /: "MM/DD/YYYY HH:MM:SS"
#' - Other formats attempt automatic parsing
#'
#' @examples
#' \dontrun{
#' # Process FollowMe GPS export
#' followme_processed <- process_followmee_gps(followme_data)
#' 
#' # Check participant mapping
#' unique(followme_processed[c("original_name", "subid")])
#' }
#'
#' @export
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

#' Extract stationary GPS points from processed data
#'
#' @description
#' Filters processed GPS data to return only stationary points (movement_state
#' == "stationary"). Optionally writes results to CSV file for external use.
#'
#' @param processed_data A data frame from process_gps() or process_followmee_gps()
#' @param write_file Logical whether to write results to CSV file. Default: FALSE
#' @param output_path Optional character string for output file path.
#'   If NULL and write_file = TRUE, uses default path in path_processed
#'
#' @return A data frame containing only stationary GPS points with columns:
#'   subid, lat, lon, dttm_obs, dist, duration, speed, transit, movement_state
#'
#' @details
#' Stationary points are GPS records where calculated speed is at or below
#' the stationary threshold (typically 4 mph). These represent locations where
#' participants spent time rather than just passing through.
#'
#' @examples
#' \dontrun{
#' # Extract stationary points only
#' stationary <- get_stationary(processed_gps)
#' 
#' # Extract and save to file
#' stationary <- get_stationary(processed_gps, write_file = TRUE)
#' 
#' # Save to custom location
#' stationary <- get_stationary(processed_gps, 
#'                              write_file = TRUE,
#'                              output_path = "data/my_stationary.csv")
#' }
#'
#' @export
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