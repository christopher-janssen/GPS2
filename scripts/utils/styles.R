# scripts/utils/styles.R
#' Styling and color management for GPS2 visualizations
# purrr and stringr loaded via tidyverse in global_setup.R

GPS2_STYLES <- list(
  location_types = list(
    colors = list( 
      "Routine" = "#d73027",
      "Frequent" = "#fc8d59", 
      "Occasional" = "#91bfdb",
      "Rare" = "#999999"
    ),
    sizes = list(  
      "Routine" = 10,
      "Frequent" = 8,
      "Occasional" = 6,
      "Rare" = 4
    )
  ),
  zoning = list(
    colors = list(  
      "Residential" = "#90EE90",
      "Commercial" = "#FFB6C1", 
      "Mixed-Use" = "#DDA0DD",
      "Downtown" = "#FF6347",
      "Employment" = "#87CEEB",
      "Industrial" = "#D3D3D3", 
      "Special" = "#F0E68C",
      "Historic" = "#DEB887",
      "Other" = "#DCDCDC"
    )
  ),
  gps_points = list(
    badger_red = "#c5050c",
    default_size = 5,
    stroke_color = "#000",
    stroke_weight = 1
  )
)

get_location_color <- function(location_type) {
  colors <- c(  # Keep as c() but unname when using
    "Routine" = "#d73027",
    "Frequent" = "#fc8d59", 
    "Occasional" = "#91bfdb",
    "Rare" = "#999999"
  )
  color <- colors[location_type]
  return(ifelse(is.na(color), colors["Rare"], unname(color)))  # Always unname
}

get_location_size <- function(location_type) {
  sizes <- c(
    "Routine" = 10,
    "Frequent" = 8,
    "Occasional" = 6,
    "Rare" = 4
  )
  size <- sizes[location_type]
  return(ifelse(is.na(size), sizes["Rare"], unname(size)))  # Always unname
}

apply_location_styling <- function(data) {
  # Classify location types if not already done
  if (!"location_type" %in% names(data)) {
    data <- data |>
      mutate(
        location_type = case_when(
          unique_days >= 5 & total_visits >= 8 ~ "Routine",
          unique_days >= 3 & total_visits >= 5 ~ "Frequent", 
          unique_days >= 2 ~ "Occasional",
          TRUE ~ "Rare"
        )
      )
  }
  
  # Add styling columns - ensure no names
  data |>
    mutate(
      marker_color = map_chr(location_type, get_location_color),
      marker_size = map_dbl(location_type, get_location_size),
      # Importance scoring for dynamic sizing
      importance_score = scale(total_visits)[,1] + scale(total_duration_hours)[,1],
      marker_size_dynamic = pmax(4, pmin(12, 6 + importance_score * 2))
    )
}

create_location_legend_data <- function(data) {
  type_counts <- data |>
    count(location_type) |>
    arrange(match(location_type, c("Routine", "Frequent", "Occasional", "Rare")))
  
  # Create UNNAMED vectors for leaflet
  colors_vec <- map_chr(type_counts$location_type, get_location_color)
  labels_vec <- str_c(type_counts$location_type, " (", type_counts$n, ")")
  
  list(
    colors = colors_vec,  # Already unnamed
    labels = labels_vec,  # Simple character vector, no names
    title = paste0("Location Types<br><small>", nrow(data), " clusters</small>")
  )
}