# scripts/r/visualization.R
# GPS data visualization and mapping functions
# Consolidates map_gps.R and cluster mapping functions

library(leaflet)
library(dplyr)
library(lubridate)
library(htmlwidgets)

source("scripts/r/database.R")

# ==============================================================================
# GPS POINT MAPPING
# ==============================================================================

# Create overview map showing all participants
map_gps_overview <- function(gps_data, show_paths = FALSE) {
  
  participants <- unique(gps_data$subid)
  colors <- c("#fbb4ae", "#b3cde3", "#ccebc5", "#decbe4", 
              "#fed9a6", "#ffffcc", "#e5d8bd", "#fddaec")
  
  # Prepare data with date information
  gps_data <- gps_data |>
    mutate(date = as.Date(dttm_obs)) |>
    arrange(subid, dttm_obs)
  
  # Create base map centered on data
  map <- leaflet() |>
    addTiles() |>
    setView(lng = mean(gps_data$lon), lat = mean(gps_data$lat), zoom = 11)
  
  # Add points and paths for each participant
  for (i in seq_along(participants)) {
    participant <- participants[i]
    participant_data <- gps_data |> filter(subid == participant)
    color <- colors[((i - 1) %% length(colors)) + 1]
    
    # Add circle markers
    map <- map |>
      addCircleMarkers(
        data = participant_data,
        lng = ~lon, 
        lat = ~lat,
        color = "#000",
        fillColor = color,
        radius = 2,
        fillOpacity = 1,
        stroke = TRUE,
        weight = 1,
        popup = ~paste0("<strong>Participant ", subid, "</strong><br>",
                        "Date: ", format(date, "%Y-%m-%d"), "<br>",
                        "Time: ", format(dttm_obs, "%H:%M:%S"), "<br>",
                        "Coordinates: ", round(lat, 4), ", ", round(lon, 4)),
        group = paste("Participant", participant)
      )
    
    # Add movement paths if requested
    if (show_paths && nrow(participant_data) > 1) {
      dates <- unique(participant_data$date)
      for (date in dates) {
        day_data <- participant_data |> filter(date == !!date)
        if (nrow(day_data) > 1) {
          map <- map |>
            addPolylines(
              data = day_data,
              lng = ~lon,
              lat = ~lat,
              color = color,
              weight = 1,
              opacity = 0.5,
              group = paste("Participant", participant)
            )
        }
      }
    }
  }
  
  # Add layer controls
  if (length(participants) > 1) {
    map <- map |>
      addLayersControl(
        overlayGroups = paste("Participant", participants),
        options = layersControlOptions(collapsed = FALSE)
      )
  }
  
  # Add summary info box
  map <- map |>
    addControl(
      html = paste0("<div style='background: rgba(255,255,255,0.9); padding: 10px; ",
                    "border-radius: 5px; border: 1px solid #ccc;'>",
                    "<strong>GPS Data Overview</strong><br>",
                    "Participants: ", length(participants), "<br>",
                    "Total points: ", nrow(gps_data), "<br>",
                    "Date range: ", min(gps_data$date), " to ", max(gps_data$date),
                    "</div>"),
      position = "bottomright"
    )
  
  return(map)
}

# Create individual participant map with day-by-day control
map_gps_individual <- function(gps_data, participant_id, show_all_days = FALSE) {
  
  # Filter for specific participant
  participant_data <- gps_data |>
    filter(subid == participant_id) |>
    mutate(date = as.Date(dttm_obs)) |>
    arrange(dttm_obs)
  
  if (nrow(participant_data) == 0) {
    stop(paste("No data found for participant", participant_id))
  }
  
  # Get unique dates
  unique_dates <- sort(unique(participant_data$date))
  
  # Color palette for days
  colors <- c("#fbb4ae", "#b3cde3", "#ccebc5", "#decbe4", 
              "#fed9a6", "#ffffcc", "#e5d8bd", "#fddaec")
  
  # Create base map
  map <- leaflet() |>
    addTiles() |>
    setView(lng = mean(participant_data$lon), lat = mean(participant_data$lat), zoom = 13)
  
  # Add each day as a separate layer
  for (i in seq_along(unique_dates)) {
    date <- unique_dates[i]
    day_data <- participant_data |> filter(date == !!date)
    
    # Color based on day
    day_color <- colors[((i - 1) %% length(colors)) + 1]
    date_str <- format(date, "%Y-%m-%d (%A)")
    
    # Enhanced popup with details
    popup_text <- ~paste0("<strong>", date_str, "</strong><br>",
                          "Time: ", format(dttm_obs, "%H:%M:%S"), "<br>",
                          if("movement_state" %in% names(day_data)) 
                            paste0("Movement: ", movement_state, "<br>") else "",
                          if("speed" %in% names(day_data)) 
                            paste0("Speed: ", round(speed, 2), " mph<br>") else "",
                          "Coordinates: ", round(lat, 4), ", ", round(lon, 4))
    
    map <- map |>
      addCircleMarkers(
        data = day_data,
        lng = ~lon, 
        lat = ~lat,
        color = "#000",
        fillColor = day_color,
        radius = 3,
        fillOpacity = 1,
        stroke = TRUE,
        weight = 1,
        popup = popup_text,
        group = if(show_all_days) "All Days" else date_str
      )
    
    # Add movement paths
    if (nrow(day_data) > 1) {
      map <- map |>
        addPolylines(
          data = day_data,
          lng = ~lon,
          lat = ~lat,
          color = "#000",
          weight = 1,
          opacity = 0.4,
          group = if(show_all_days) "All Days" else date_str
        )
    }
  }
  
  # Add layer control unless showing all days
  if (!show_all_days && length(unique_dates) > 1) {
    map <- map |>
      addLayersControl(
        baseGroups = format(unique_dates, "%Y-%m-%d (%A)"),
        options = layersControlOptions(collapsed = FALSE)
      )
  }
  
  # Add participant info box
  map <- map |>
    addControl(
      html = paste0("<div style='background: rgba(255,255,255,0.9); padding: 10px; ",
                    "border-radius: 5px; border: 1px solid #ccc;'>",
                    "<strong>Participant ", participant_id, "</strong><br>",
                    "Days tracked: ", length(unique_dates), "<br>",
                    "Total points: ", nrow(participant_data), "<br>",
                    "Date range: ", min(unique_dates), " to ", max(unique_dates),
                    "</div>"),
      position = "bottomright"
    )
  
  return(map)
}

# Main GPS mapping function with flexible options
map_gps <- function(gps_data, participant_id = NULL, show_paths = FALSE, show_all_days = FALSE) {
  
  if (is.null(participant_id)) {
    # Overview mode - all participants
    return(map_gps_overview(gps_data, show_paths = show_paths))
  } else {
    # Individual mode - specific participant
    return(map_gps_individual(gps_data, participant_id, show_all_days = show_all_days))
  }
}

# ==============================================================================
# CLUSTER MAPPING
# ==============================================================================

# Map cluster representatives with location type classification
map_cluster_representatives <- function(cluster_data, participant_id = NULL) {
  
  if (nrow(cluster_data) == 0) {
    return(leaflet() |> addTiles() |> setView(lng = -89.384373, lat = 43.074713, zoom = 10))
  }
  
  # Determine participant ID if not provided
  if (is.null(participant_id)) {
    participant_id <- cluster_data$subid[1]
  }
  
  # Map column names if coming from PostGIS
  if ("cluster_id" %in% names(cluster_data)) {
    cluster_data <- cluster_data |> rename(cluster = cluster_id)
  }
  
  # Add participant ID if missing
  if (!"subid" %in% names(cluster_data)) {
    cluster_data$subid <- participant_id
  }
  
  # Ensure datetime columns are properly formatted
  if ("first_visit" %in% names(cluster_data)) {
    cluster_data$first_visit <- as.POSIXct(cluster_data$first_visit)
    cluster_data$last_visit <- as.POSIXct(cluster_data$last_visit)
  }
  
  # Classify locations based on visit patterns
  clustered_data <- cluster_data |>
    mutate(
      location_type = case_when(
        unique_days >= 5 & total_visits >= 8 ~ "routine",           # Daily routine (work, home)
        unique_days >= 3 & total_visits >= 5 ~ "frequent",          # Regular spots (gym, store)
        unique_days >= 2 ~ "occasional",                            # Sometimes visited
        TRUE ~ "rare"                                               # One-off visits
      ),
      # Assign colors directly to avoid named vector issues
      marker_color = case_when(
        location_type == "routine" ~ "#d73027",     # Red - daily routine locations
        location_type == "frequent" ~ "#fc8d59",    # Orange - frequent visits  
        location_type == "occasional" ~ "#91bfdb",  # Light blue - occasional
        location_type == "rare" ~ "#999999"         # Gray - rare visits
      ),
      # Size based on importance (visits + duration)
      importance_score = scale(total_visits)[,1] + scale(total_duration_hours)[,1],
      marker_size = pmax(4, pmin(12, 6 + importance_score * 2))    # Size between 4-12
    )
  
  # Create base map
  map <- leaflet(clustered_data) |>
    addTiles() |>
    setView(lng = mean(clustered_data$lon), lat = mean(clustered_data$lat), zoom = 12)
  
  # Calculate date range and summary statistics
  start_date <- min(clustered_data$first_visit)
  end_date <- max(clustered_data$last_visit)
  total_days <- as.numeric(difftime(end_date, start_date, units = "days")) + 1
  
  # Add summary info box
  map <- map |>
    addControl(
      html = paste0(
        "<div style='background: rgba(255,255,255,0.95); padding: 12px; ",
        "border-radius: 8px; border: 2px solid #333; font-family: Arial;'>",
        "<strong>Participant ", participant_id, " - Location Summary</strong><br>",
        "<strong>", nrow(clustered_data), "</strong> meaningful locations<br>",
        "<strong>", sum(clustered_data$total_visits), "</strong> total visits<br>",
        "<strong>", round(sum(clustered_data$total_duration_hours), 1), "</strong> hours tracked<br>",
        "<strong>Timeframe:</strong> [", format(start_date, "%m-%d-%Y"), " to ", 
        format(end_date, "%m-%d-%Y"), "]<br>",
        "<em>Marker size = visit importance</em>",
        "</div>"
      ),
      position = "topright"
    )
  
  # Add markers for each location type separately
  location_types <- unique(clustered_data$location_type)
  
  for (type in location_types) {
    type_data <- clustered_data |> filter(location_type == type)
    
    map <- map |>
      addCircleMarkers(
        data = type_data,
        lng = ~lon, 
        lat = ~lat,
        radius = ~marker_size,
        color = "#000", 
        fillColor = ~marker_color,
        fillOpacity = 0.8, 
        stroke = TRUE, 
        weight = 1,
        popup = ~paste0(
          "<strong>Location Cluster ", cluster, "</strong><br>",
          "<em>", stringr::str_to_title(location_type), " location</em><br><br>",
          "<strong>Visit Pattern:</strong><br>",
          "• Total visits: ", total_visits, "<br>",
          "• Days visited: ", unique_days, "<br>",
          "• Total time: ", round(total_duration_hours, 1), " hours<br>",
          "• GPS points: ", n_points, "<br><br>",
          "<strong>Timeline:</strong><br>",
          "• First visit: ", format(first_visit, "%m/%d/%Y %H:%M"), "<br>",
          "• Last visit: ", format(last_visit, "%m/%d/%Y %H:%M"), "<br><br>",
          "<strong>Coordinates:</strong><br>",
          round(lat, 4), ", ", round(lon, 4)
        ),
        label = ~paste0("Cluster ", cluster, ": ", unique_days, " days, ", 
                        total_visits, " visits"),
        group = stringr::str_to_title(type)
      )
  }
  
  # Add layer control
  map <- map |>
    addLayersControl(
      overlayGroups = stringr::str_to_title(location_types),
      options = layersControlOptions(collapsed = FALSE)
    )
  
  # Create legend with manual color assignments
  legend_colors <- c()
  legend_labels <- c()
  
  for (type in location_types) {
    type_color <- case_when(
      type == "routine" ~ "#d73027",
      type == "frequent" ~ "#fc8d59", 
      type == "occasional" ~ "#91bfdb",
      type == "rare" ~ "#999999"
    )
    type_count <- sum(clustered_data$location_type == type)
    
    legend_colors <- c(legend_colors, type_color)
    legend_labels <- c(legend_labels, paste0(stringr::str_to_title(type), " (", type_count, ")"))
  }
  
  # Add legend
  map <- map |>
    addLegend(
      position = "bottomleft",
      colors = legend_colors,
      labels = legend_labels,
      title = "Location Types",
      opacity = 0.8
    )
  
  return(map)
}

# Convenience function to map clusters directly from database
map_participant_clusters <- function(participant_id) {
  
  # Get cluster data from database
  cluster_data <- get_participant_clusters(participant_id)
  
  # Check if any clusters were found
  if (nrow(cluster_data) == 0) {
    cat("No clusters found for participant", participant_id, "\n")
    return(leaflet() |> 
             addTiles() |> 
             setView(lng = -89.384373, lat = 43.074713, zoom = 10) |>
             addControl(
               html = paste0("<div style='background: rgba(255,255,255,0.95); padding: 12px; ",
                             "border-radius: 8px; border: 2px solid #333; font-family: Arial;'>",
                             "<strong>No clusters found for Participant ", participant_id, "</strong><br>",
                             "This participant may not have sufficient stationary GPS data.",
                             "</div>"),
               position = "topright"
             ))
  }
  
  # Create and return the map
  return(map_cluster_representatives(cluster_data, participant_id))
}

# ==============================================================================
# GEOCODED LOCATION MAPPING
# ==============================================================================

# Map geocoded clusters with address information
map_geocoded_clusters <- function(participant_id, show_failed = FALSE) {
  
  # Get geocoded cluster data
  geocoded_data <- get_geocoded_clusters(participant_ids = participant_id, include_failed = show_failed)
  
  if (nrow(geocoded_data) == 0) {
    cat("No geocoded clusters found for participant", participant_id, "\n")
    return(leaflet() |> addTiles() |> setView(lng = -89.384373, lat = 43.074713, zoom = 10))
  }
  
  # Classify locations and add colors
  geocoded_data <- geocoded_data |>
    mutate(
      location_type = case_when(
        unique_days >= 5 & total_visits >= 8 ~ "routine",
        unique_days >= 3 & total_visits >= 5 ~ "frequent",
        unique_days >= 2 ~ "occasional",
        TRUE ~ "rare"
      ),
      marker_color = case_when(
        location_type == "routine" ~ "#d73027",
        location_type == "frequent" ~ "#fc8d59",
        location_type == "occasional" ~ "#91bfdb",
        location_type == "rare" ~ "#999999"
      ),
      marker_size = pmax(4, pmin(12, 6 + scale(total_visits)[,1] * 2))
    )
  
  # Create base map
  map <- leaflet(geocoded_data) |>
    addTiles() |>
    setView(lng = mean(geocoded_data$lon), lat = mean(geocoded_data$lat), zoom = 12)
  
  # Add summary info
  map <- map |>
    addControl(
      html = paste0(
        "<div style='background: rgba(255,255,255,0.95); padding: 12px; ",
        "border-radius: 8px; border: 2px solid #333; font-family: Arial;'>",
        "<strong>Participant ", participant_id, " - Geocoded Locations</strong><br>",
        "<strong>", nrow(geocoded_data), "</strong> geocoded locations<br>",
        "<strong>", sum(geocoded_data$total_visits), "</strong> total visits<br>",
        "</div>"
      ),
      position = "topright"
    )
  
  # Add markers with geocoded information
  location_types <- unique(geocoded_data$location_type)
  
  for (type in location_types) {
    type_data <- geocoded_data |> filter(location_type == type)
    
    map <- map |>
      addCircleMarkers(
        data = type_data,
        lng = ~lon, 
        lat = ~lat,
        radius = ~marker_size,
        color = "#000", 
        fillColor = ~marker_color,
        fillOpacity = 0.8, 
        stroke = TRUE, 
        weight = 1,
        popup = ~paste0(
          "<strong>", if_else(is.na(display_name), "Unknown Location", display_name), "</strong><br>",
          "<em>", stringr::str_to_title(location_type), " location</em><br><br>",
          "<strong>Address:</strong><br>",
          if_else(is.na(road), "", paste0(road, "<br>")),
          if_else(is.na(city), "", paste0(city, ", ")),
          if_else(is.na(state), "", state), " ",
          if_else(is.na(postcode), "", postcode), "<br><br>",
          "<strong>Visit Pattern:</strong><br>",
          "• Total visits: ", total_visits, "<br>",
          "• Days visited: ", unique_days, "<br>",
          "• Total time: ", round(total_duration_hours, 1), " hours<br><br>",
          "<strong>Geocoding:</strong><br>",
          "• Method: ", geocoding_method, "<br>",
          "• Confidence: ", round(geocoding_confidence, 2)
        ),
        group = stringr::str_to_title(type)
      )
  }
  
  # Add layer control and legend
  map <- map |>
    addLayersControl(
      overlayGroups = stringr::str_to_title(location_types),
      options = layersControlOptions(collapsed = FALSE)
    )
  
  return(map)
}

# ==============================================================================
# UTILITY FUNCTIONS
# ==============================================================================

# Save leaflet map to HTML file
save_map <- function(map, filename, folder = "maps") {
  
  # Ensure filename has .html extension
  if (!grepl("\\.html$", filename)) {
    filename <- paste0(filename, ".html")
  }
  
  # Create folder if it doesn't exist
  if (!dir.exists(folder)) {
    dir.create(folder, recursive = TRUE)
  }
  
  # Create full path
  filepath <- file.path(folder, filename)
  
  # Save the map
  saveWidget(map, file = filepath, selfcontained = TRUE)
  
  # Confirmation
  cat("Map saved to:", filepath, "\n")
  return(filepath)
}

# Generate maps for multiple participants
generate_participant_maps <- function(participant_ids, map_type = "clusters", save_maps = TRUE) {
  
  maps_created <- list()
  
  for (participant_id in participant_ids) {
    cat("Creating", map_type, "map for participant", participant_id, "...\n")
    
    tryCatch({
      if (map_type == "clusters") {
        map <- map_participant_clusters(participant_id)
        filename <- paste0("clusters_participant_", participant_id)
      } else if (map_type == "geocoded") {
        map <- map_geocoded_clusters(participant_id)
        filename <- paste0("geocoded_participant_", participant_id)
      } else if (map_type == "gps") {
        gps_data <- get_participant_data(participant_id)
        map <- map_gps(gps_data, participant_id = participant_id)
        filename <- paste0("gps_participant_", participant_id)
      } else {
        stop("Invalid map_type. Use 'clusters', 'geocoded', or 'gps'")
      }
      
      if (save_maps) {
        save_map(map, filename)
      }
      
      maps_created[[as.character(participant_id)]] <- map
      
    }, error = function(e) {
      cat("Error creating map for participant", participant_id, ":", e$message, "\n")
    })
  }
  
  cat("✅ Created", length(maps_created), "maps\n")
  return(maps_created)
}