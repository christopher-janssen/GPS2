# utils/validation_utils.R
#' data validation utilities for GPS2
# tibble and stringr loaded via tidyverse in global_setup.R

#' Create empty clustering result with proper structure
#'
#' @description
#' Returns a properly structured empty tibble for cases where clustering
#' produces no results. Ensures consistent return structure across functions.
#'
#' @return A tibble with the expected cluster result columns but zero rows.
#'   Contains: subid, lat, lon, n_points, first_visit, last_visit, cluster,
#'   total_visits, total_duration_hours, unique_days
#'
#' @examples
#' \dontrun{
#' # Used when clustering fails or finds no clusters
#' empty_result <- create_empty_cluster_result()
#' }
#'
#' @export
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
#'
#' @description
#' Performs comprehensive validation of GPS data including column presence,
#' data types, coordinate ranges, and missing values. Throws informative
#' errors for validation failures.
#'
#' @param data A data frame containing GPS data to validate
#' @param required_cols Character vector of required column names.
#'   Default: c("subid", "lat", "lon")
#'
#' @return Invisible TRUE if validation passes. Throws error if validation fails.
#'
#' @details
#' Validation checks include:
#' - Required columns are present
#' - Latitude values are in valid range (-90, 90)
#' - Longitude values are in valid range (-180, 180)
#' - No missing values in coordinate columns
#' - Data frame is not empty
#'
#' @examples
#' \dontrun{
#' # Validate basic GPS data
#' validate_gps_data(gps_data)
#' 
#' # Validate with custom required columns
#' validate_gps_data(gps_data, c("subid", "lat", "lon", "dttm_obs"))
#' }
#'
#' @export
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

#' Validate and filter participant data by ID
#'
#' @description
#' Checks if a participant exists in the dataset and returns their data.
#' Returns an empty cluster result structure if no data is found.
#'
#' @param data A data frame containing GPS data with 'subid' column
#' @param subid Numeric participant ID to search for
#'
#' @return Either the filtered participant data or an empty cluster result
#'   tibble if no data is found for the specified participant
#'
#' @examples
#' \dontrun{
#' # Check if participant 101 exists and get their data
#' participant_data <- validate_participant_exists(gps_data, 101)
#' }
#'
#' @export
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

#' Validate clustering algorithm parameters
#'
#' @description
#' Validates parameters used in GPS clustering algorithms, ensuring they
#' are within acceptable ranges for meaningful analysis.
#'
#' @param eps Numeric clustering distance threshold in meters.
#'   Must be positive and <= 10000
#' @param min_duration_min Optional numeric minimum duration in minutes.
#'   Must be positive if provided
#'
#' @return TRUE if all parameters are valid. Throws error if validation fails.
#'
#' @details
#' - eps: Controls clustering sensitivity. Smaller values create more clusters
#' - min_duration_min: Minimum time spent at location to be considered significant
#'
#' @examples
#' \dontrun{
#' # Validate standard clustering parameters
#' validate_clustering_params(eps = 50, min_duration_min = 30)
#' }
#'
#' @export
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

#' Validate participant ID format and value
#'
#' @description
#' Ensures participant ID is a single positive numeric value suitable
#' for database queries and analysis operations.
#'
#' @param subid Participant identifier to validate
#'
#' @return TRUE if valid. Throws error if validation fails.
#'
#' @details
#' Requirements:
#' - Must be numeric
#' - Must be a single value (length 1)
#' - Must be positive (> 0)
#'
#' @examples
#' \dontrun{
#' # Validate a participant ID
#' validate_participant_id(101)
#' }
#'
#' @export
validate_participant_id <- function(subid) {
  if (!is.numeric(subid) || length(subid) != 1 || subid <= 0) {
    stop("subid must be a single positive number")
  }
  return(TRUE)
}