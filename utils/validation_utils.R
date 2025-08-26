# utils/validation_utils.R
#' data validation utilities for GPS2
# tibble and stringr loaded via tidyverse in global_setup.R

#' Create empty cluster result data frame
create_empty_cluster_result <- function() {
  tibble(
    subid = numeric(0), 
    lat = numeric(0), 
    lon = numeric(0), 
    n_points = integer(0), 
    first_visit = as.POSIXct(character(0)), 
    last_visit = as.POSIXct(character(0)), 
    cluster = integer(0),
    total_visits = integer(0), 
    total_duration_hours = numeric(0), 
    unique_days = integer(0)
  )
}

#' Validate GPS data structure and content
validate_gps_data <- function(data, required_cols = c("subid", "lat", "lon")) {
  if (nrow(data) == 0) {
    stop("No data provided")
  }
  
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", str_c(missing_cols, collapse = ", "))
  }
  
  # Coordinate validation
  invalid_coords <- !between(data$lat, -90, 90) | !between(data$lon, -180, 180)
  if (any(invalid_coords)) {
    stop("Invalid coordinates found")
  }
  
  return(TRUE)
}

#' Validate and filter participant data, return empty result if none found
validate_participant_exists <- function(data, subid) {
  if (!"subid" %in% names(data)) {
    stop("Data must contain 'subid' column")
  }
  
  participant_data <- data |> filter(subid == !!subid)
  if (nrow(participant_data) == 0) {
    cat("No data found for participant", subid, "\n")
    return(create_empty_cluster_result())
  }
  
  return(participant_data)
}

#' Validate clustering parameters
validate_clustering_params <- function(eps, min_duration_min = NULL) {
  if (!is.numeric(eps) || eps <= 0 || eps > 10000) {
    stop("eps must be a positive number <= 10000 meters")
  }
  
  if (!is.null(min_duration_min)) {
    if (!is.numeric(min_duration_min) || min_duration_min <= 0) {
      stop("min_duration_min must be a positive number")
    }
  }
  
  return(TRUE)
}

#' Validate participant ID
validate_participant_id <- function(subid) {
  if (!is.numeric(subid) || length(subid) != 1 || subid <= 0) {
    stop("subid must be a single positive number")
  }
  return(TRUE)
}