# scripts/r/visualization.R
# GPS2 visualization functions - refactored with utilities

library(leaflet)
# htmlwidgets and sf functions used via namespace (::)

source("scripts/r/global_setup.R")
source("scripts/r/database.R")

# ==============================================================================
# MAIN VISUALIZATION FUNCTIONS
# ==============================================================================

#' Create GPS visualization from R environment data
#' 
#' @param data GPS or cluster data frame already loaded in R
#' @param subid Participant ID to visualize
#' @param viz_type "raw" or "clusters"
#' @param show_paths Show path lines for raw GPS
#' @param show_all_days Show all days at once for raw GPS
#' @param geocoded_data Optional geocoding data frame for clusters
#' 
#' @return Leaflet map object

visualize_gps_env <- function(data, 
                              subid,
                              viz_type = c("raw", "clusters"),
                              show_paths = FALSE,
                              show_all_days = FALSE,
                              geocoded_data = NULL) {
  
  viz_type <- match.arg(viz_type)
  
  # Validation
  validate_gps_data(data)
  participant_data <- validate_participant_exists(data, subid)
  
  if (nrow(participant_data) == 0) {
    stop(str_c("No data found for participant ", subid))
  }
  
  # Create appropriate visualization
  if (viz_type == "raw") {
    map <- create_raw_gps_map(participant_data, subid, show_paths, show_all_days)
  } else {
    map <- create_cluster_map_with_geocoding(participant_data, geocoded_data, subid)
  }
  
  return(map)
}

#' Create GPS visualization directly from database
#' 
#' @param subid Participant ID to visualize
#' @param viz_type "raw" or "clusters"
#' @param show_paths Show path lines for raw GPS
#' @param show_all_days Show all days at once for raw GPS
#' 
#' @return Leaflet map object

visualize_gps_db <- function(subid,
                             viz_type = c("raw", "clusters"),
                             show_paths = FALSE,
                             show_all_days = FALSE) {
  
  viz_type <- match.arg(viz_type)
  
  # Validation
  validate_participant_id(subid)
  
  # Create map based on type
  if (viz_type == "raw") {
    map <- create_raw_gps_map_from_db(subid, show_paths, show_all_days)
  } else {
    map <- create_cluster_map_from_db(subid)
  }
  
  return(map)
}

# ==============================================================================
# RAW GPS VISUALIZATION
# ==============================================================================

#' Create raw GPS map from database
create_raw_gps_map_from_db <- function(subid, show_paths, show_all_days) {
  
  # Get data using query builder
  query <- build_gps_query(participant_ids = subid)
  data <- query_gps2_db(query)
  
  if (nrow(data) == 0) {
    stop(str_c("No GPS data found for participant ", subid))
  }
  
  return(create_raw_gps_map(data, subid, show_paths, show_all_days))
}

#' Create raw GPS map from data
create_raw_gps_map <- function(data, subid, show_paths, show_all_days) {
  
  # Prepare data
  data <- prepare_raw_gps_data(data)
  
  # Initialize map
  map <- create_base_leaflet_map(data)
  
  # Add points and paths by day
  map <- add_gps_points_by_day(map, data, show_paths, show_all_days)
  
  # Add info box and controls
  map <- add_raw_gps_info_box(map, data, subid)
  map <- add_raw_gps_layer_controls(map, data, show_paths, show_all_days)
  
  return(map)
}

#' Helper: Prepare raw GPS data for visualization
prepare_raw_gps_data <- function(data) {
  data |>
    mutate(
      date = as.Date(dttm_obs),
      time_str = format(dttm_obs, "%H:%M:%S")
    ) |>
    arrange(dttm_obs)
}

#' Helper: Create base leaflet map
create_base_leaflet_map <- function(data) {
  default_zoom <- get_config("visualization", "default_zoom")
  
  leaflet() |>
    setView(lng = mean(data$lon), lat = mean(data$lat), zoom = default_zoom) |>
    addTiles()
}

#' Helper: Add GPS points by day
add_gps_points_by_day <- function(map, data, show_paths, show_all_days) {
  
  unique_dates <- sort(unique(data$date))
  badger_red <- GPS2_STYLES$gps_points$badger_red
  
  for (i in seq_along(unique_dates)) {
    date <- unique_dates[i]
    day_data <- data |> filter(date == !!date)
    date_str <- format(date, "%Y-%m-%d (%A)")
    
    # Add points using utility
    map <- add_daily_gps_markers(map, day_data, date_str, badger_red, show_all_days)
    
    # Add paths if requested
    if (show_paths && nrow(day_data) > 1) {
      map <- add_daily_gps_paths(map, day_data, date_str, show_all_days)
    }
  }
  
  return(map)
}

#' Helper: Add markers for a single day
add_daily_gps_markers <- function(map, day_data, date_str, color, show_all_days) {
  
  # Create popups using utility
  day_data <- day_data |>
    rowwise() |>
    mutate(popup_content = create_gps_popup(pick(everything()), date_str)) |>
    ungroup()
  
  map |>
    addCircleMarkers(
      data = day_data,
      lng = ~lon, lat = ~lat,
      color = GPS2_STYLES$gps_points$stroke_color,           
      fillColor = color,   
      radius = GPS2_STYLES$gps_points$default_size,               
      fillOpacity = 0.8,        
      stroke = TRUE, 
      weight = GPS2_STYLES$gps_points$stroke_weight,               
      popup = ~popup_content,
      group = if(show_all_days) "All Points" else date_str
    )
}

#' Helper: Add path lines for a single day
add_daily_gps_paths <- function(map, day_data, date_str, show_all_days) {
  map |>
    addPolylines(
      data = day_data,
      lng = ~lon, lat = ~lat,
      color = "#000", weight = 2, opacity = 0.5,
      group = if(show_all_days) "Paths" else str_c(date_str, " Path")
    )
}

#' Helper: Add info box for raw GPS
add_raw_gps_info_box <- function(map, data, subid) {
  
  unique_dates <- unique(data$date)
  
  info_html <- str_c(
    "<div style='background: rgba(255,255,255,0.9); padding: 10px; border-radius: 5px; border: 1px solid #ccc;'>",
    "<strong>Participant ", subid, " - Raw GPS</strong><br>",
    "<strong>", nrow(data), "</strong> GPS points<br>",
    "<strong>", length(unique_dates), "</strong> days<br>",
    "From ", min(data$date), " to ", max(data$date),
    "</div>"
  )
  
  map |>
    addControl(html = info_html, position = "topright")
}

#' Helper: Add layer controls for raw GPS
add_raw_gps_layer_controls <- function(map, data, show_paths, show_all_days) {
  
  unique_dates <- sort(unique(data$date))
  
  if (!show_all_days && length(unique_dates) > 1) {
    groups <- format(unique_dates, "%Y-%m-%d (%A)")
    if (show_paths) {
      groups <- c(groups, paste(groups, "Path"))
    }
    
    # Ensure groups is an unnamed vector
    groups <- unname(groups)
    
    collapsed <- length(unique_dates) > 7
    
    map <- map |>
      addLayersControl(
        baseGroups = if(!show_paths) groups else NULL,
        overlayGroups = if(show_paths) groups else NULL,
        options = layersControlOptions(collapsed = collapsed, autoZIndex = TRUE)
      )
  }
  
  return(map)
}

# ==============================================================================
# CLUSTER VISUALIZATION
# ==============================================================================

#' Create cluster map from database
create_cluster_map_from_db <- function(subid) {
  
  # Get data using query builder
  query <- build_cluster_query(
    participant_ids = subid,
    include_geocoding = TRUE,
    include_failed_geocoding = FALSE
  )
  
  data <- query_gps2_db(query)
  
  if (nrow(data) == 0) {
    stop(str_c("No clusters found for participant ", subid))
  }
  
  return(create_cluster_map_with_geocoding(data, NULL, subid))
}

#' Create cluster map with geocoding information
create_cluster_map_with_geocoding <- function(cluster_data, geocoded_data, subid) {
  
  # Handle column naming
  if ("cluster_id" %in% names(cluster_data) && !"cluster" %in% names(cluster_data)) {
    cluster_data <- cluster_data |> rename(cluster = cluster_id)
  }
  
  # Merge geocoding data if provided separately
  if (!is.null(geocoded_data)) {
    data <- merge_geocoding_data(cluster_data, geocoded_data)
  } else {
    data <- cluster_data
  }
  
  # Apply styling using utility
  data <- apply_location_styling(data)
  
  # Create popups using utility
  data <- data |>
    rowwise() |>
    mutate(popup_content = create_cluster_popup(pick(everything()))) |>
    ungroup()
  
  # Create short address labels using utility
  data <- data |>
    mutate(short_address = create_short_address_labels(pick(everything())))
  
  # Build map
  map <- create_base_leaflet_map(data)
  map <- add_cluster_markers_by_type(map, data)
  map <- add_cluster_info_box(map, data, subid)
  map <- add_cluster_legend_and_controls(map, data)
  
  return(map)
}

#' Helper: Merge geocoding data with cluster data
merge_geocoding_data <- function(cluster_data, geocoded_data) {
  
  if ("cluster_id" %in% names(geocoded_data) && !"cluster" %in% names(geocoded_data)) {
    geocoded_data <- geocoded_data |> rename(cluster = cluster_id)
  }
  
  cluster_data |>
    left_join(geocoded_data |> 
                select(any_of(c("cluster", "display_name", "road", "city", "state", 
                                "postcode", "geocoding_confidence", "place_type", "geocoding_method"))),
              by = "cluster")
}

#' Helper: Create short address labels
create_short_address_labels <- function(data) {
  
  sapply(1:nrow(data), function(i) {
    row <- data[i, ]
    
    # Check if geocoding columns exist and have data
    has_display_name <- "display_name" %in% names(row) && !is.na(row$display_name) && row$display_name != ""
    has_road <- "road" %in% names(row) && !is.na(row$road) && row$road != ""
    has_city <- "city" %in% names(row) && !is.na(row$city) && row$city != ""
    
    if (has_road && has_city) {
      return(str_c(row$road, ", ", row$city))
    } else if (has_road) {
      return(row$road)
    } else if (has_city) {
      return(row$city)
    } else if (has_display_name) {
      return(substr(row$display_name, 1, 50))
    } else {
      return(str_c("Location ", row$cluster))
    }
  })
}

#' Helper: Add cluster markers by location type
add_cluster_markers_by_type <- function(map, data) {
  
  ordered_types <- c("Routine", "Frequent", "Occasional", "Rare")
  existing_types <- ordered_types[ordered_types %in% data$location_type]
  
  for (type in existing_types) {
    type_data <- data |> filter(location_type == type)
    
    map <- map |>
      addCircleMarkers(
        data = type_data,
        lng = ~lon, lat = ~lat,
        radius = ~marker_size_dynamic,
        color = "#000",
        fillColor = ~marker_color,  # this should now be unnamed
        fillOpacity = 0.8,
        stroke = TRUE, 
        weight = 1,
        popup = ~popup_content,
        label = ~short_address,
        group = type  
      )
  }
  
  return(map)
}

create_raw_gps_map <- function(data, subid, show_paths, show_all_days) {
  
  # Prepare data
  data <- prepare_raw_gps_data(data)
  
  # Initialize map
  map <- create_base_leaflet_map(data)
  
  # Add points and paths by day
  map <- add_gps_points_by_day(map, data, show_paths, show_all_days)
  
  # Add info box and controls (fixed)
  map <- add_raw_gps_info_box(map, data, subid)
  map <- add_raw_gps_layer_controls(map, data, show_paths, show_all_days)  # Fixed function
  
  return(map)
}

#' Helper: Add cluster info box
add_cluster_info_box <- function(map, data, subid) {
  
  # Calculate statistics
  start_date <- min(data$first_visit)
  end_date <- max(data$last_visit)
  
  # Count geocoding success
  geocoding_stats <- calculate_geocoding_stats(data)
  
  info_html <- paste0(
    "<div style='background: rgba(255,255,255,0.95); padding: 12px; ",
    "border-radius: 8px; border: 2px solid #333; font-family: Arial;'>",
    "<strong>Participant ", subid, " - Location Clusters</strong><br>",
    "<strong>", nrow(data), "</strong> meaningful locations<br>",
    "<strong>", sum(data$total_visits), "</strong> total visits<br>",
    "<strong>", round(sum(data$total_duration_hours), 1), "</strong> hours tracked<br>",
    "<strong>Timeframe:</strong> [", format(start_date, "%m-%d-%Y"), " to ", 
    format(end_date, "%m-%d-%Y"), "]<br><br>",
    create_geocoding_info_html(geocoding_stats),
    "<em>Marker size = visit importance</em>",
    "</div>"
  )
  
  map |>
    addControl(html = info_html, position = "topright")
}

#' Helper: Calculate geocoding statistics
calculate_geocoding_stats <- function(data) {
  
  # Check if geocoding columns exist
  has_geocoding_cols <- any(c("display_name", "road", "city", "state", "postcode") %in% names(data))
  
  if (!has_geocoding_cols) {
    return(list(has_data = FALSE))
  }
  
  total_locations <- nrow(data)
  with_addresses <- sum(!is.na(data$display_name) & data$display_name != "", na.rm = TRUE)
  with_roads <- if ("road" %in% names(data)) sum(!is.na(data$road) & data$road != "", na.rm = TRUE) else 0
  with_cities <- if ("city" %in% names(data)) sum(!is.na(data$city) & data$city != "", na.rm = TRUE) else 0
  
  list(
    has_data = TRUE,
    total_locations = total_locations,
    with_addresses = with_addresses,
    with_roads = with_roads,
    with_cities = with_cities,
    success_rate = round(with_addresses / total_locations * 100, 1)
  )
}

#' Helper: Create geocoding info HTML
create_geocoding_info_html <- function(stats) {
  
  if (!stats$has_data) {
    return("<strong>No geocoding data available</strong><br>")
  }
  
  paste0(
    "<strong>Geocoding Results:</strong><br>",
    "- Complete addresses: ", stats$with_addresses, " (", stats$success_rate, "%)<br>",
    "- Street addresses: ", stats$with_roads, "<br>",
    "- City identified: ", stats$with_cities, "<br>"
  )
}

#' Helper: Add legend and layer controls
add_cluster_legend_and_controls <- function(map, data) {
  
  existing_types <- unique(data$location_type)
  existing_types <- existing_types[existing_types %in% c("Routine", "Frequent", "Occasional", "Rare")]
  
  if (length(existing_types) > 1) {
    map <- map |>
      addLayersControl(
        overlayGroups = unname(existing_types),  # Remove any names
        options = layersControlOptions(collapsed = FALSE)
      )
  }
  
  legend_data <- create_location_legend_data(data)
  
  map |>
    addLegend(
      position = "bottomleft",
      colors = legend_data$colors,      # Already unnamed
      labels = legend_data$labels,      # Already unnamed
      title = legend_data$title,
      opacity = 0.8
    )
}

# ==============================================================================
# CONVENIENCE FUNCTIONS
# ==============================================================================

#' Quick visualization functions using utilities
map_participant_clusters <- function(participant_id) {
  visualize_gps_db(participant_id, "clusters")
}

map_participant_gps <- function(participant_id) {
  visualize_gps_db(participant_id, "raw")
}

map_participant_geocoded <- function(participant_id) {
  visualize_gps_db(participant_id, "clusters")
}

#' Generate maps for multiple participants
generate_participant_maps <- function(participant_ids, map_type = "clusters", save_maps = FALSE) {
  
  maps <- list()
  
  for (participant_id in participant_ids) {
    tryCatch({
      map <- visualize_gps_db(participant_id, map_type)
      maps[[as.character(participant_id)]] <- map
      
      if (save_maps) {
        filename <- paste0("participant_", participant_id, "_", map_type, ".html")
        save_map(map, filename)
      }
      
      cat("✓ Created map for participant", participant_id, "\n")
      
    }, error = function(e) {
      cat("✗ Failed to create map for participant", participant_id, ":", e$message, "\n")
    })
  }
  
  return(maps)
}

#' Save map with validation
save_map <- function(map, filename) {
  if (!inherits(map, "leaflet")) {
    stop("Object is not a leaflet map")
  }
  
  # Ensure filename has .html extension
  if (!grepl("\\.html$", filename)) {
    filename <- paste0(filename, ".html")
  }
  
  # Create full path to maps folder
  filepath <- file.path("maps", filename)
  
  # Ensure maps directory exists
  if (!dir.exists("maps")) {
    dir.create("maps")
  }
  
  # Save the map
  htmlwidgets::saveWidget(map, file = filepath, selfcontained = TRUE)
  cat("✓ Map saved to:", filepath, "\n")
}

#' Get summary statistics for a participant using utilities
get_participant_summary <- function(subid, use_db = TRUE) {
  
  validate_participant_id(subid)
  
  summary <- list(subid = subid)
  
  if (use_db) {
    # Use query builders for consistent queries
    gps_query <- build_gps_query(participant_ids = subid)
    cluster_query <- build_cluster_query(participant_ids = subid, include_geocoding = TRUE)
    
    gps_stats <- query_gps2_db(paste0("
      SELECT 
        COUNT(*) as total_points,
        COUNT(DISTINCT DATE(dttm_obs)) as unique_days,
        MIN(dttm_obs) as first_point,
        MAX(dttm_obs) as last_point,
        SUM(CASE WHEN movement_state = 'stationary' THEN 1 ELSE 0 END) as stationary_points
      FROM (", gps_query, ") gps;
    "))
    
    cluster_stats <- query_gps2_db(paste0("
      SELECT 
        COUNT(*) as total_clusters,
        SUM(total_visits) as total_visits,
        SUM(total_duration_hours) as total_hours,
        MAX(unique_days) as max_days_at_location,
        COUNT(CASE WHEN display_name IS NOT NULL THEN 1 END) as geocoded_clusters
      FROM (", cluster_query, ") clusters;
    "))
    
    summary$gps <- gps_stats
    summary$clusters <- cluster_stats
  }
  
  # Print summary
  print_participant_summary(summary)
  
  return(invisible(summary))
}

#' Helper: Print participant summary
print_participant_summary <- function(summary) {
  cat("\nSummary for Participant", summary$subid, "\n")
  cat("=====================================\n")
  
  if (!is.null(summary$gps)) {
    cat("GPS Points:", summary$gps$total_points, "\n")
    cat("Unique Days:", summary$gps$unique_days, "\n")
    if (!is.null(summary$gps$stationary_points)) {
      cat("Stationary Points:", summary$gps$stationary_points, "\n")
    }
  }
  
  if (!is.null(summary$clusters)) {
    cat("\nClusters:", summary$clusters$total_clusters, "\n")
    cat("Total Visits:", summary$clusters$total_visits, "\n")
    cat("Total Hours:", round(summary$clusters$total_hours, 1), "\n")
    if (!is.null(summary$clusters$geocoded_clusters)) {
      cat("Geocoded Clusters:", summary$clusters$geocoded_clusters, "\n")
    }
  }
}