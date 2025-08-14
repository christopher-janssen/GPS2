# scripts/r/visualization.R
# Complete GPS2 visualization functions with zoning integration
# Includes maps for GPS data, clusters, geocoding, and zoning analysis

library(leaflet)
library(dplyr)
library(lubridate)
library(htmlwidgets)
library(sf)

source("scripts/r/database.R")

# ==============================================================================
# GPS POINT MAPPING
# ==============================================================================

# Create GPS overview map for all participants
map_gps_overview <- function(gps_data, show_paths = FALSE) {
  
  participants <- unique(gps_data$subid)
  colors <- c("#fbb4ae", "#b3cde3", "#ccebc5", "#decbe4", 
              "#fed9a6", "#ffffcc", "#e5d8bd", "#fddaec")
  
  gps_data <- gps_data |>
    mutate(date = as.Date(dttm_obs)) |>
    arrange(subid, dttm_obs)
  
  map <- leaflet() |>
    addTiles() |>
    setView(lng = mean(gps_data$lon), lat = mean(gps_data$lat), zoom = 11)
  
  for (i in seq_along(participants)) {
    participant <- participants[i]
    participant_data <- gps_data |> filter(subid == participant)
    color <- colors[((i - 1) %% length(colors)) + 1]
    
    map <- map |>
      addCircleMarkers(
        data = participant_data,
        lng = ~lon, lat = ~lat,
        color = "#000", fillColor = color,
        radius = 2, fillOpacity = 1, stroke = TRUE, weight = 1,
        popup = ~paste0("<strong>Participant ", subid, "</strong><br>",
                        "Date: ", format(date, "%Y-%m-%d"), "<br>",
                        "Time: ", format(dttm_obs, "%H:%M:%S")),
        group = paste("Participant", participant)
      )
    
    if (show_paths && nrow(participant_data) > 1) {
      dates <- unique(participant_data$date)
      for (date in dates) {
        day_data <- participant_data |> filter(date == !!date)
        if (nrow(day_data) > 1) {
          map <- map |>
            addPolylines(
              data = day_data, lng = ~lon, lat = ~lat,
              color = color, weight = 1, opacity = 0.5,
              group = paste("Participant", participant)
            )
        }
      }
    }
  }
  
  if (length(participants) > 1) {
    map <- map |>
      addLayersControl(
        overlayGroups = paste("Participant", participants),
        options = layersControlOptions(collapsed = FALSE)
      )
  }
  
  return(map)
}

# Create individual participant GPS map
map_gps_individual <- function(gps_data, participant_id, show_all_days = FALSE) {
  
  participant_data <- gps_data |>
    filter(subid == participant_id) |>
    mutate(date = as.Date(dttm_obs)) |>
    arrange(dttm_obs)
  
  if (nrow(participant_data) == 0) {
    stop(paste("No data found for participant", participant_id))
  }
  
  unique_dates <- sort(unique(participant_data$date))
  colors <- c("#fbb4ae", "#b3cde3", "#ccebc5", "#decbe4", "#fed9a6", "#ffffcc")
  
  map <- leaflet() |>
    addTiles() |>
    setView(lng = mean(participant_data$lon), lat = mean(participant_data$lat), zoom = 13)
  
  for (i in seq_along(unique_dates)) {
    date <- unique_dates[i]
    day_data <- participant_data |> filter(date == !!date)
    day_color <- colors[((i - 1) %% length(colors)) + 1]
    date_str <- format(date, "%Y-%m-%d (%A)")
    
    map <- map |>
      addCircleMarkers(
        data = day_data,
        lng = ~lon, lat = ~lat,
        color = "#000", fillColor = day_color,
        radius = 3, fillOpacity = 1, stroke = TRUE, weight = 1,
        popup = ~paste0("<strong>", date_str, "</strong><br>",
                        "Time: ", format(dttm_obs, "%H:%M:%S"), "<br>",
                        if("movement_state" %in% names(day_data)) 
                          paste0("Movement: ", movement_state, "<br>") else "",
                        "Coordinates: ", round(lat, 4), ", ", round(lon, 4)),
        group = if(show_all_days) "All Days" else date_str
      )
    
    if (nrow(day_data) > 1) {
      map <- map |>
        addPolylines(
          data = day_data, lng = ~lon, lat = ~lat,
          color = "#000", weight = 1, opacity = 0.4,
          group = if(show_all_days) "All Days" else date_str
        )
    }
  }
  
  if (!show_all_days && length(unique_dates) > 1) {
    map <- map |>
      addLayersControl(
        baseGroups = format(unique_dates, "%Y-%m-%d (%A)"),
        options = layersControlOptions(collapsed = FALSE)
      )
  }
  
  return(map)
}

# Main GPS mapping function
map_gps <- function(gps_data, participant_id = NULL, show_paths = FALSE, show_all_days = FALSE) {
  if (is.null(participant_id)) {
    return(map_gps_overview(gps_data, show_paths = show_paths))
  } else {
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
  
  if (is.null(participant_id)) {
    participant_id <- cluster_data$subid[1]
  }
  
  # Handle different column naming conventions
  if ("cluster_id" %in% names(cluster_data)) {
    cluster_data <- cluster_data |> rename(cluster = cluster_id)
  }
  if (!"subid" %in% names(cluster_data)) {
    cluster_data$subid <- participant_id
  }
  
  # Ensure datetime columns are proper format
  if ("first_visit" %in% names(cluster_data)) {
    cluster_data$first_visit <- as.POSIXct(cluster_data$first_visit)
    cluster_data$last_visit <- as.POSIXct(cluster_data$last_visit)
  }
  
  # Classify locations by visit patterns
  clustered_data <- cluster_data |>
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
      importance_score = scale(total_visits)[,1] + scale(total_duration_hours)[,1],
      marker_size = pmax(4, pmin(12, 6 + importance_score * 2))
    )
  
  map <- leaflet(clustered_data) |>
    addTiles() |>
    setView(lng = mean(clustered_data$lon), lat = mean(clustered_data$lat), zoom = 12)
  
  # Calculate summary statistics
  start_date <- min(clustered_data$first_visit)
  end_date <- max(clustered_data$last_visit)
  
  location_types <- c("routine", "frequent", "occasional", "rare")
  existing_types <- location_types[location_types %in% unique(clustered_data$location_type)]
  
  for (type in existing_types) {
    type_data <- clustered_data |> filter(location_type == type)
    
    map <- map |>
      addCircleMarkers(
        data = type_data,
        lng = ~lon, lat = ~lat,
        radius = ~marker_size,
        color = "#000", fillColor = ~marker_color,
        fillOpacity = 0.8, stroke = TRUE, weight = 1,
        popup = ~paste0(
          "<strong>Location Cluster ", cluster, "</strong><br>",
          "<em>", stringr::str_to_title(location_type), " location</em><br><br>",
          "<strong>Visit Pattern:</strong><br>",
          "• Total visits: ", total_visits, "<br>",
          "• Days visited: ", unique_days, "<br>", 
          "• Total time: ", round(total_duration_hours, 1), " hours<br>",
          if("n_points" %in% names(type_data)) paste0("• GPS points: ", n_points, "<br>") else "",
          "<br><strong>Timeline:</strong><br>",
          "• First visit: ", format(first_visit, "%m/%d/%Y %H:%M"), "<br>",
          "• Last visit: ", format(last_visit, "%m/%d/%Y %H:%M"), "<br><br>",
          "<strong>Coordinates:</strong><br>",
          round(lat, 4), ", ", round(lon, 4)
        ),
        group = stringr::str_to_title(type)
      )
  }
  
  map <- map |>
    addLayersControl(
      overlayGroups = stringr::str_to_title(existing_types),
      options = layersControlOptions(collapsed = FALSE)
    )
  
  # Add summary info box
  map <- map |>
    addControl(
      html = paste0(
        "<div style='background: rgba(255,255,255,0.95); padding: 12px; border-radius: 8px; border: 2px solid #333;'>",
        "<strong>Participant ", participant_id, " - Location Summary</strong><br>",
        "<strong>", nrow(clustered_data), "</strong> meaningful locations<br>",
        "<strong>", sum(clustered_data$total_visits), "</strong> total visits<br>",
        "<strong>", round(sum(clustered_data$total_duration_hours), 1), "</strong> hours tracked<br>",
        "<strong>Timeframe:</strong> ", format(start_date, "%m-%d-%Y"), " to ", format(end_date, "%m-%d-%Y"),
        "</div>"
      ),
      position = "topright"
    )
  
  return(map)
}

# ==============================================================================
# ZONING VISUALIZATION - UPDATED
# ==============================================================================

# Map Madison zoning districts with proper sf handling
map_zoning_districts <- function(show_categories = NULL, max_zones = 100) {
  
  cat("Loading Madison zoning districts...\n")
  
  # Build category filter
  category_filter <- ""
  if (!is.null(show_categories)) {
    category_list <- paste0("'", show_categories, "'", collapse = ",")
    category_filter <- paste0("WHERE zone_category IN (", category_list, ")")
  }
  
  # Get zone count
  count_query <- paste0("
    SELECT COUNT(*)::integer as count 
    FROM gps2.zoning_districts 
    WHERE geometry IS NOT NULL ", 
                        if (category_filter != "") gsub("WHERE", "AND", category_filter) else ""
  )
  total_zones <- as.integer(query_gps2_db(count_query)$count)
  
  if (total_zones == 0) {
    cat("❌ No zoning data found\n")
    return(leaflet() |> addTiles() |> setView(lng = -89.384373, lat = 43.074713, zoom = 10))
  }
  
  cat("   Found", total_zones, "zone types\n")
  
  # Read zoning data using sf
  zoning_query <- paste0("
    SELECT zone_code, zone_name, zone_category, geometry,
           ROUND(area_acres::numeric, 1) as area_acres
    FROM gps2.zoning_districts 
    WHERE geometry IS NOT NULL ", category_filter, "
    ORDER BY area_acres DESC;
  ")
  
  con <- connect_gps2_db()
  zoning_sf <- tryCatch({
    st_read(con, query = zoning_query, quiet = TRUE)
  }, error = function(e) {
    cat("❌ Error reading zoning data:", e$message, "\n")
    return(NULL)
  }, finally = {
    disconnect_gps2_db(con)
  })
  
  if (is.null(zoning_sf) || nrow(zoning_sf) == 0) {
    return(leaflet() |> addTiles() |> setView(lng = -89.384373, lat = 43.074713, zoom = 10))
  }
  
  # Ensure proper CRS
  if (st_crs(zoning_sf)$epsg != 4326) {
    zoning_sf <- st_transform(zoning_sf, 4326)
  }
  
  # Color scheme for zoning categories
  zone_colors <- c(
    "Residential" = "#90EE90", "Commercial" = "#FFB6C1", "Mixed-Use" = "#DDA0DD",
    "Downtown" = "#FF6347", "Employment" = "#87CEEB", "Industrial" = "#D3D3D3", 
    "Special" = "#F0E68C", "Historic" = "#DEB887", "Other" = "#DCDCDC"
  )
  
  # Create base map
  map <- leaflet() |>
    addTiles() |>
    setView(lng = -89.384373, lat = 43.074713, zoom = 11)
  
  # Add zones by category
  categories <- unique(zoning_sf$zone_category)
  
  for (category in categories) {
    category_zones <- zoning_sf[zoning_sf$zone_category == category, ]
    zone_color <- unname(zone_colors[category])  # Remove names to fix JSON issue
    if (is.na(zone_color)) zone_color <- unname(zone_colors["Other"])
    
    map <- map |>
      addPolygons(
        data = category_zones,
        color = "#000", weight = 1, opacity = 0.7,
        fillColor = zone_color, fillOpacity = 0.5,
        popup = ~paste0(
          "<strong>", zone_code, "</strong><br>",
          zone_name, "<br>",
          "<em>", zone_category, "</em><br>",
          "Area: ", area_acres, " acres"
        ),
        label = ~paste0(zone_code, " (", zone_category, ")"),
        group = category
      )
  }
  
  # Add controls and legend
  legend_subset <- zone_colors[names(zone_colors) %in% categories]
  map <- map |>
    addLayersControl(
      overlayGroups = categories,
      options = layersControlOptions(collapsed = FALSE)
    ) |>
    addLegend(
      position = "bottomright",
      colors = unname(legend_subset),  # Remove names to fix JSON issue
      labels = names(legend_subset),
      title = "Zoning Categories",
      opacity = 0.7
    )
  
  return(map)
}

# Map participant clusters with zoning context
map_clusters_with_zoning <- function(participant_id, show_zone_categories = NULL) {
  
  # Get cluster data with zoning info
  cluster_data <- query_gps2_db("
    SELECT 
      lc.cluster_id, lc.lat, lc.lon, lc.total_visits, lc.unique_days, 
      lc.total_duration_hours, cz.zone_code, cz.zone_name, cz.zone_category
    FROM gps2.location_clusters lc
    LEFT JOIN gps2.cluster_zoning cz ON lc.subid = cz.subid AND lc.cluster_id = cz.cluster_id
    WHERE lc.subid = $1
    ORDER BY lc.total_duration_hours DESC;
  ", list(participant_id))
  
  if (nrow(cluster_data) == 0) {
    cat("❌ No clusters found for participant", participant_id, "\n")
    return(leaflet() |> addTiles() |> setView(lng = -89.384373, lat = 43.074713, zoom = 10))
  }
  
  # Start with base cluster map
  map <- map_participant_clusters(participant_id)
  
  # Get zoning overlay for the area
  bbox_buffer <- 0.01
  min_lat <- min(cluster_data$lat) - bbox_buffer
  max_lat <- max(cluster_data$lat) + bbox_buffer
  min_lon <- min(cluster_data$lon) - bbox_buffer
  max_lon <- max(cluster_data$lon) + bbox_buffer
  
  # Filter by categories if specified
  category_filter <- ""
  if (!is.null(show_zone_categories)) {
    category_list <- paste0("'", show_zone_categories, "'", collapse = ",")
    category_filter <- paste0("AND zone_category IN (", category_list, ")")
  }
  
  # Get zoning overlay
  zoning_query <- paste0("
    SELECT zone_code, zone_name, zone_category, geometry
    FROM gps2.zoning_districts 
    WHERE geometry IS NOT NULL 
    AND ST_Intersects(geometry, ST_MakeEnvelope($1, $2, $3, $4, 4326)) 
    ", category_filter, ";
  ")
  
  con <- connect_gps2_db()
  zoning_overlay <- tryCatch({
    st_read(con, query = zoning_query, 
            query_parameters = list(min_lon, min_lat, max_lon, max_lat),
            quiet = TRUE)
  }, error = function(e) {
    return(NULL)
  }, finally = {
    disconnect_gps2_db(con)
  })
  
  # Add zoning boundaries
  if (!is.null(zoning_overlay) && nrow(zoning_overlay) > 0) {
    zone_colors <- c(
      "Residential" = "#90EE90", "Commercial" = "#FFB6C1", "Mixed-Use" = "#DDA0DD",
      "Downtown" = "#FF6347", "Employment" = "#87CEEB", "Industrial" = "#D3D3D3",
      "Special" = "#F0E68C", "Historic" = "#DEB887", "Other" = "#DCDCDC"
    )
    
    if (st_crs(zoning_overlay)$epsg != 4326) {
      zoning_overlay <- st_transform(zoning_overlay, 4326)
    }
    
    for (category in unique(zoning_overlay$zone_category)) {
      category_zones <- zoning_overlay[zoning_overlay$zone_category == category, ]
      zone_color <- unname(zone_colors[category])  # Remove names to fix JSON issue
      if (is.na(zone_color)) zone_color <- unname(zone_colors["Other"])
      
      map <- map |>
        addPolygons(
          data = category_zones,
          color = "#333", weight = 2, opacity = 0.8,
          fillColor = zone_color, fillOpacity = 0.3,
          popup = ~paste0("<strong>Zone: ", zone_code, "</strong><br>", zone_category),
          group = "Zoning Boundaries"
        )
    }
    
    # Update layer control
    map <- map |>
      addLayersControl(
        overlayGroups = c("Zoning Boundaries", "Routine", "Frequent", "Occasional", "Rare"),
        options = layersControlOptions(collapsed = FALSE)
      )
  }
  
  return(map)
}

# ==============================================================================
# CONVENIENCE FUNCTIONS FOR DATABASE INTEGRATION
# ==============================================================================

# Map participant clusters directly from database
map_participant_clusters <- function(participant_id) {
  cluster_data <- get_participant_clusters(participant_id)
  
  if (nrow(cluster_data) == 0) {
    cat("❌ No clusters found for participant", participant_id, "\n")
    return(leaflet() |> 
             addTiles() |> 
             setView(lng = -89.384373, lat = 43.074713, zoom = 10) |>
             addControl(
               html = paste0("<div style='background: rgba(255,255,255,0.95); padding: 12px; border-radius: 8px;'>",
                             "<strong>No clusters found for Participant ", participant_id, "</strong><br>",
                             "This participant may not have sufficient stationary GPS data.",
                             "</div>"),
               position = "topright"
             ))
  }
  
  return(map_cluster_representatives(cluster_data, participant_id))
}

# Map participant GPS points directly from database
map_participant_gps <- function(participant_id, show_all_days = FALSE) {
  gps_data <- get_participant_data(participant_id)
  
  if (nrow(gps_data) == 0) {
    cat("❌ No GPS data found for participant", participant_id, "\n")
    return(leaflet() |> addTiles() |> setView(lng = -89.384373, lat = 43.074713, zoom = 10))
  }
  
  return(map_gps(gps_data, participant_id = participant_id, show_all_days = show_all_days))
}

# ==============================================================================
# ZONING ANALYSIS MAPS
# ==============================================================================

# Map all clusters within a specific zoning category
map_zoning_category_analysis <- function(zone_category, participant_ids = NULL) {
  
  # Build participant filter
  participant_filter <- ""
  if (!is.null(participant_ids)) {
    participant_list <- paste(participant_ids, collapse = ",")
    participant_filter <- paste0("AND lc.subid IN (", participant_list, ")")
  }
  
  # Get clusters in specified zoning category
  cluster_analysis <- query_gps2_db(paste0("
    SELECT 
      lc.subid, lc.cluster_id, lc.lat, lc.lon,
      lc.total_visits, lc.unique_days, lc.total_duration_hours,
      cz.zone_code, cz.zone_name, cz.zone_category
    FROM gps2.location_clusters lc
    JOIN gps2.cluster_zoning cz ON lc.subid = cz.subid AND lc.cluster_id = cz.cluster_id
    WHERE cz.zone_category = $1 ", participant_filter, "
    ORDER BY lc.total_duration_hours DESC;
  "), list(zone_category))
  
  if (nrow(cluster_analysis) == 0) {
    cat("❌ No clusters found in", zone_category, "zones\n")
    return(leaflet() |> addTiles() |> setView(lng = -89.384373, lat = 43.074713, zoom = 10))
  }
  
  cat("✅ Found", nrow(cluster_analysis), "clusters in", zone_category, "zones from", 
      length(unique(cluster_analysis$subid)), "participants\n")
  
  # Create map
  map <- leaflet() |>
    addTiles() |>
    setView(lng = mean(cluster_analysis$lon), lat = mean(cluster_analysis$lat), zoom = 12)
  
  # Color by participant
  participants <- unique(cluster_analysis$subid)
  colors <- rainbow(length(participants))
  names(colors) <- participants
  
  for (participant in participants) {
    participant_clusters <- cluster_analysis[cluster_analysis$subid == participant, ]
    
    map <- map |>
      addCircleMarkers(
        data = participant_clusters,
        lng = ~lon, lat = ~lat,
        radius = ~pmax(4, pmin(15, total_visits / 2)),
        color = "#000", fillColor = colors[as.character(participant)],
        fillOpacity = 0.8, weight = 1,
        popup = ~paste0(
          "<strong>Participant ", subid, " - Cluster ", cluster_id, "</strong><br>",
          "Zone: ", zone_code, " (", zone_category, ")<br>",
          "Visits: ", total_visits, " | Days: ", unique_days, "<br>",
          "Duration: ", round(total_duration_hours, 1), " hours"
        ),
        group = paste("Participant", participant)
      )
  }
  
  # Add controls and summary
  map <- map |>
    addLayersControl(
      overlayGroups = paste("Participant", participants),
      options = layersControlOptions(collapsed = FALSE)
    ) |>
    addControl(
      html = paste0(
        "<div style='background: rgba(255,255,255,0.9); padding: 10px; border-radius: 5px;'>",
        "<strong>", zone_category, " Zone Analysis</strong><br>",
        "Clusters: ", nrow(cluster_analysis), "<br>",
        "Participants: ", length(participants), "<br>",
        "Total visits: ", sum(cluster_analysis$total_visits),
        "</div>"
      ),
      position = "topright"
    )
  
  return(map)
}

# ==============================================================================
# UTILITY FUNCTIONS
# ==============================================================================

# Save leaflet map to HTML file
save_map <- function(map, filename, folder = "maps") {
  
  if (!grepl("\\.html$", filename)) {
    filename <- paste0(filename, ".html")
  }
  
  if (!dir.exists(folder)) {
    dir.create(folder, recursive = TRUE)
  }
  
  filepath <- file.path(folder, filename)
  saveWidget(map, file = filepath, selfcontained = TRUE)
  
  cat("Map saved to:", filepath, "\n")
  return(filepath)
}

# Generate comprehensive maps for participants
generate_participant_maps <- function(participant_ids, map_types = c("clusters", "zoning"), save_maps = TRUE) {
  
  maps_created <- list()
  
  for (participant_id in participant_ids) {
    cat(" Creating maps for participant", participant_id, "...\n")
    
    for (map_type in map_types) {
      tryCatch({
        if (map_type == "clusters") {
          map <- map_participant_clusters(participant_id)
          filename <- paste0("clusters_participant_", participant_id)
        } else if (map_type == "zoning") {
          map <- map_clusters_with_zoning(participant_id)
          filename <- paste0("zoning_participant_", participant_id)
        } else if (map_type == "gps") {
          map <- map_participant_gps(participant_id)
          filename <- paste0("gps_participant_", participant_id)
        } else {
          next
        }
        
        if (save_maps) {
          save_map(map, filename)
        }
        
        maps_created[[paste0(participant_id, "_", map_type)]] <- map
        
      }, error = function(e) {
        cat("❌ Error creating", map_type, "map for participant", participant_id, ":", e$message, "\n")
      })
    }
  }
  
  cat("✅ Created", length(maps_created), "maps\n")
  return(maps_created)
}

# Generate comprehensive zoning analysis maps
generate_zoning_analysis <- function(save_maps = TRUE) {
  
  cat("Generating comprehensive zoning analysis...\n")
  maps_created <- list()
  
  # Overall zoning map
  overall_map <- map_zoning_districts()
  if (save_maps) save_map(overall_map, "madison_zoning_overview")
  maps_created[["overview"]] <- overall_map
  
  # Maps by major categories
  major_categories <- c("Residential", "Commercial", "Mixed-Use", "Downtown", "Employment")
  
  for (category in major_categories) {
    tryCatch({
      # Category zoning map
      category_map <- map_zoning_districts(show_categories = category)
      if (save_maps) save_map(category_map, paste0("zoning_", tolower(category)))
      maps_created[[paste0(tolower(category), "_zones")]] <- category_map
      
      # Cluster analysis for this category
      cluster_map <- map_zoning_category_analysis(category)
      if (save_maps) save_map(cluster_map, paste0("clusters_in_", tolower(category)))
      maps_created[[paste0(tolower(category), "_clusters")]] <- cluster_map
      
    }, error = function(e) {
      cat("❌ Error creating", category, "maps:", e$message, "\n")
    })
  }
  
  cat("✅ Created", length(maps_created), "zoning analysis maps\n")
  return(maps_created)
}