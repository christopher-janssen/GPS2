# config/gps2_config.R
GPS2_CONFIG <- list(
  # Clustering parameters
  clustering = list(
    default_eps = 50,
    min_duration_min = 30,
    max_cluster_radius = 200
  ),
  
  # GPS filtering
  filtering = list(
    speed_threshold_mph = 100,
    stationary_threshold_mph = 4,
    max_time_gap_hours = 24
  ),
  
  # Database operations
  database = list(
    batch_size = 1000,
    progress_interval = 5,
    connection_timeout = 30
  ),
  
  # Geocoding
  geocoding = list(
    nominatim_url = "http://localhost:8080",
    delay_seconds = 0.1,
    max_retries = 3
  ),
  
  # Visualization
  visualization = list(
    default_zoom = 12,
    max_popup_length = 500,
    legend_position = "bottomleft"
  )
)

# Helper function to get config values
get_config <- function(...) {
  keys <- list(...)
  result <- GPS2_CONFIG
  for (key in keys) {
    result <- result[[key]]
    if (is.null(result)) return(NULL)
  }
  return(result)
}