# GPS22 visualization functions - clean data-in, visualization-out workflow

#' Plot raw GPS points for a participant
#' @param gps_data Dataframe with lat, lon, time, movement_state columns
#' @param subid Participant ID for labeling
#' @param show_paths Whether to connect points with lines
plot_gps_points <- function(gps_data, subid = NULL, show_paths = FALSE) {
  
  # Infer subid if not provided
  if (is.null(subid) && "subid" %in% names(gps_data)) {
    subid <- gps_data$subid[1]
  }
  
  # Prepare data
  data <- gps_data |>
    mutate(
      time = as.POSIXct(time),
      date = as.Date(time),
      time_str = format(time, "%H:%M:%S"),
      movement_state = if("movement_state" %in% names(gps_data)) {
        coalesce(movement_state, "unknown")
      } else if("sgmnt_type" %in% names(gps_data)) {
        coalesce(sgmnt_type, "unknown")
      } else {
        "unknown"
      }
    ) |>
    arrange(time)
  
  # Create base map
  map <- leaflet() |>
    setView(lng = mean(data$lon), lat = mean(data$lat), zoom = 13) |>
    addTiles()
  
  # Add points by day
  unique_dates <- sort(unique(data$date))
  
  for (date in unique_dates) {
    day_data <- data |> filter(date == !!date)
    date_str <- format(date, "%Y-%m-%d (%A)")
    
    # Create popups
    day_data <- day_data |>
      rowwise() |>
      mutate(popup_content = create_gps_popup(pick(everything()), date_str)) |>
      ungroup()
    
    # Add markers
    map <- map |>
      addCircleMarkers(
        data = day_data,
        lng = ~lon, lat = ~lat,
        color = "#000000",
        fillColor = "#c5050c",
        radius = 4,
        fillOpacity = 0.8,
        stroke = TRUE,
        weight = 1,
        popup = ~popup_content,
        group = date_str
      )
    
    # Add paths if requested
    if (show_paths && nrow(day_data) > 1) {
      map <- map |>
        addPolylines(
          data = day_data,
          lng = ~lon, lat = ~lat,
          color = "#000", weight = 2, opacity = 0.5,
          group = str_c(date_str, " Path")
        )
    }
  }
  
  # Add info box FIRST (so it renders behind layer controls)
  info_html <- create_gps_info_box(data, subid)
  map <- map |> addControl(html = info_html, position = "topright")
  
  # Add layer controls AFTER info box (so they render on top)
  if (length(unique_dates) > 1) {
    groups <- format(unique_dates, "%Y-%m-%d (%A)")
    if (show_paths) {
      groups <- c(groups, paste(groups, "Path"))
    }
    
    map <- map |>
      addLayersControl(
        overlayGroups = groups,
        options = layersControlOptions(collapsed = length(unique_dates) > 7)
      )
  }
  
  map
}

#' Plot GPS clusters with visit frequency styling
#' @param cluster_data Dataframe with cluster info and optional geocoding
#' @param subid Participant ID for labeling (optional if in data)
#' @param color_by Coloring scheme: "visits", "duration", or "frequency"
plot_cluster_map <- function(cluster_data, subid = NULL, color_by = "visits", show_legend = TRUE) {
  
  # Infer subid if not provided
  if (is.null(subid) && "subid" %in% names(cluster_data)) {
    subid <- cluster_data$subid[1]
  }
  
  # Prepare data with consistent global classification
  data <- cluster_data |>
    classify_location_importance(method = color_by) |>
    assign_marker_styles(color_by = color_by)
  
  # Create short labels
  data <- data |>
    mutate(
      short_address = case_when(
        !is.na(display_name) & nchar(display_name) <= 50 ~ display_name,
        !is.na(display_name) ~ str_c(substr(display_name, 1, 47), "..."),
        !is.na(city) ~ city,
        TRUE ~ str_c("Location ", cluster_id)
      )
    )
  
  # Create popups (thresholds shown in legend)
  data <- data |>
    rowwise() |>
    mutate(popup_content = create_cluster_popup(pick(everything()))) |>
    ungroup()
  
  # Create base map
  map <- leaflet() |>
    setView(lng = mean(data$lon), lat = mean(data$lat), zoom = 13) |>
    addTiles()
  
  # Add markers by type in proper order for layer controls
  if (color_by == "duration") {
    # Order for duration categories (most important first)  
    type_order <- c("Extended Stays", "Long Stays", "Medium Stays", "Short Stays", "Brief Stops")
  } else {
    # Order for visit categories (most important first)
    type_order <- c("Routine", "Frequent", "Occasional", "Rare")
  }
  
  # Only process types that actually exist in the data, in the desired order
  existing_types <- type_order[type_order %in% unique(data$location_type)]
  
  for (type in existing_types) {
    type_data <- data |> filter(location_type == type)
    if (nrow(type_data) > 0) {
      map <- map |>
        addCircleMarkers(
          data = type_data,
          lng = ~lon, lat = ~lat,
          radius = ~marker_size,
          color = "#000",
          fillColor = ~marker_color,
          fillOpacity = 0.8,
          stroke = TRUE,
          weight = 1,
          popup = ~popup_content,
          label = ~short_address,
          group = type
        )
    }
  }
  
  # Add info box FIRST (so it renders behind layer controls)
  info_html <- create_cluster_info_box(data, subid)
  map <- map |> addControl(html = info_html, position = "topright")
  
  # Add layer controls AFTER info box (so they render on top)  
  if (length(existing_types) > 1) {
    map <- map |>
      addLayersControl(
        overlayGroups = existing_types,  # Use the ordered existing_types from above
        options = layersControlOptions(collapsed = FALSE)
      )
  }
  
  # Add legend if requested
  if (show_legend) {
    # Get global threshold labels with actual values
    global_thresholds <- get_global_thresholds(color_by)
    legend_labels <- global_thresholds$labels
    
    # Define colors to match the order returned by get_global_thresholds()
    if (color_by == "duration") {
      # Colors must match the order: Brief, Short, Medium, Long, Extended (as returned by get_global_thresholds)
      legend_colors <- c("#03a9f4", "#8bc34a", "#ff9800", "#f44336", "#9c27b0")
      # But we want to display them in importance order: Extended, Long, Medium, Short, Brief
      category_order <- c("Extended Stays", "Long Stays", "Medium Stays", "Short Stays", "Brief Stops")
    } else {
      # Colors must match the order: Rare, Occasional, Frequent, Routine (as returned by get_global_thresholds)
      legend_colors <- c("#03a9f4", "#8bc34a", "#ff9800", "#f44336")
      # But we want to display them in importance order: Routine, Frequent, Occasional, Rare
      category_order <- c("Routine", "Frequent", "Occasional", "Rare")
    }
    
    # Create a mapping between labels and colors, then reorder for display
    label_color_map <- setNames(legend_colors, legend_labels)
    
    # Filter to existing types and reorder for proper display (most important first)
    existing_types_ordered <- category_order[category_order %in% existing_types]
    
    # Get the corresponding labels and colors in our desired display order
    if (length(existing_types_ordered) > 0) {
      # Map category names back to their full labels
      display_labels <- c()
      display_colors <- c()
      
      for (category in existing_types_ordered) {
        # Find the matching label for this category
        matching_label <- legend_labels[grepl(category, legend_labels, fixed = TRUE)]
        if (length(matching_label) > 0) {
          display_labels <- c(display_labels, matching_label[1])
          display_colors <- c(display_colors, label_color_map[matching_label[1]])
        }
      }
      
      map <- map |>
        addLegend(
          position = "bottomleft",
          colors = display_colors,
          labels = display_labels, 
          title = paste("Location Types (", str_to_title(color_by), ")"),
          opacity = 0.8
        )
    }
  }
  
  map
}

# Helper functions for data classification and styling

#' Get global thresholds for consistent categorization across all participants
#' @param method "visits" or "duration" 
get_global_thresholds <- function(method = "visits") {
  if (method == "visits") {
    # Use empirically meaningful visit thresholds based on data distribution:
    # - 68% of locations have 1 visit (Rare)
    # - 13% have 2-4 visits (Occasional)  
    # - 10% have 5-9 visits (Frequent)
    # - 5% have 10+ visits (Routine)
    list(
      breaks = c(1, 2, 5, 10),
      labels = c("Rare (1 visit)", "Occasional (2-4 visits)", "Frequent (5-9 visits)", "Routine (10+ visits)")
    )
  } else if (method == "duration") {
    # Use empirically meaningful duration thresholds based on data distribution:
    # - 22.5% have <1h avg (Brief Stops)
    # - 23.1% have 1-2h avg (Short Stays) 
    # - 19.4% have 2-4h avg (Medium Stays)
    # - 14.4% have 4-8h avg (Long Stays)
    # - 20.7% have 8h+ avg (Extended Stays)
    list(
      breaks = c(1, 2, 4, 8),
      labels = c("Brief Stops (<1h avg)", "Short Stays (1-2h avg)", "Medium Stays (2-4h avg)", "Long Stays (4-8h avg)", "Extended Stays (8h+ avg)")
    )
  } else {
    stop("Method must be 'visits' or 'duration'")
  }
}

#' Classify location importance using consistent global thresholds
classify_location_importance <- function(data, method = "visits") {
  
  # Get global thresholds for consistency
  global_thresholds <- get_global_thresholds(method)
  
  if (method == "visits") {
    breaks <- global_thresholds$breaks
    data |>
      mutate(
        location_type = case_when(
          total_visits >= breaks[4] ~ "Routine",
          total_visits >= breaks[3] ~ "Frequent", 
          total_visits >= breaks[2] ~ "Occasional",
          TRUE ~ "Rare"
        )
      )
  } else if (method == "duration") {
    breaks <- global_thresholds$breaks
    data |>
      mutate(
        avg_hours_per_visit = total_duration_hours / pmax(total_visits, 1),
        location_type = case_when(
          avg_hours_per_visit >= breaks[4] ~ "Extended Stays",
          avg_hours_per_visit >= breaks[3] ~ "Long Stays",
          avg_hours_per_visit >= breaks[2] ~ "Medium Stays",
          avg_hours_per_visit >= breaks[1] ~ "Short Stays",
          TRUE ~ "Brief Stops"
        )
      )
  } else if (method == "frequency" && "visit_frequency_category" %in% names(data)) {
    data |>
      mutate(location_type = visit_frequency_category)
  } else {
    # Default to visits method
    classify_location_importance(data, "visits")
  }
}

#' Assign marker colors and sizes based on location type
assign_marker_styles <- function(data, color_by = "visits") {
  data |>
    mutate(
      # Assign colors using Material Design palette
      marker_color = case_when(
        # Visit-based categories
        location_type == "Routine" ~ "#f44336",      # Material Red
        location_type == "Frequent" ~ "#ff9800",     # Material Orange 
        location_type == "Occasional" ~ "#8bc34a",   # Material Green
        location_type == "Rare" ~ "#03a9f4",         # Material Blue
        # Duration-based categories (5 levels with distinct colors)
        location_type == "Extended Stays" ~ "#9c27b0",  # Purple - Most significant time
        location_type == "Long Stays" ~ "#f44336",      # Red - Major commitments  
        location_type == "Medium Stays" ~ "#ff9800",    # Orange - Extended activities
        location_type == "Short Stays" ~ "#8bc34a",     # Green - Brief visits
        location_type == "Brief Stops" ~ "#03a9f4",     # Blue - Quick stops
        TRUE ~ "#666666"  # fallback
      ),
      # Scale marker size based on appropriate metric
      marker_size = if(color_by == "duration") {
        # Size by average hours per visit for duration mode
        avg_duration_per_visit = total_duration_hours / pmax(total_visits, 1)
        pmax(5, pmin(15, 5 + (avg_duration_per_visit - min(avg_duration_per_visit, na.rm = TRUE)) / 
                             (max(avg_duration_per_visit, na.rm = TRUE) - min(avg_duration_per_visit, na.rm = TRUE)) * 10))
      } else {
        # Size by total visits for visit mode
        pmax(5, pmin(15, 5 + (total_visits - min(total_visits)) / 
                             (max(total_visits) - min(total_visits)) * 10))
      }
    )
}

# Additional visualization functions

#' Plot movement timeline for a participant
plot_movement_timeline <- function(gps_data, subid = NULL) {
  # Infer subid if not provided
  if (is.null(subid) && "subid" %in% names(gps_data)) {
    subid <- gps_data$subid[1]
  }
  
  gps_data |>
    mutate(
      time = as.POSIXct(time),
      date = as.Date(time),
      hour = lubridate::hour(time)
    ) |>
    ggplot(aes(x = time, y = movement_state, color = movement_state)) +
    geom_point(alpha = 0.6) +
    labs(
      title = paste("Movement Timeline:", subid),
      x = "Time", y = "Movement State"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

#' Create spatial density heatmap
plot_spatial_density <- function(gps_data, subid = NULL) {
  # Infer subid if not provided
  if (is.null(subid) && "subid" %in% names(gps_data)) {
    subid <- gps_data$subid[1]
  }
  
  gps_data |>
    ggplot(aes(x = lon, y = lat)) +
    stat_density_2d_filled(alpha = 0.7) +
    geom_point(size = 0.5, alpha = 0.3) +
    coord_fixed() +
    labs(
      title = paste("Spatial Density:", subid),
      x = "Longitude", y = "Latitude"
    ) +
    theme_void() +
    theme(legend.position = "bottom")
}

# Madison Zoning Visualization Functions

#' Plot interactive zoning map with color-coded categories
#' @param zoning_data sf dataframe with zoning polygons from pull_db(6)
#' @param show_legend Whether to display category legend (default: TRUE)
#' @param layer_controls Whether to add toggleable layer controls (default: TRUE)
#' @param gps_clusters Optional GPS cluster data to overlay
plot_zoning_map <- function(zoning_data, show_legend = TRUE, layer_controls = TRUE, gps_clusters = NULL) {
  
  # Validate data
  if (!"sf" %in% class(zoning_data)) {
    stop("zoning_data must be an sf object. Use pull_db(6) to get proper spatial data.")
  }
  
  # Define Material Design color palette for zoning categories
  zoning_colors <- c(
    "Residential" = "#8bc34a",    # Light Green - calming residential
    "Commercial" = "#ff9800",     # Orange - active business districts  
    "Mixed-Use" = "#03a9f4",      # Blue - versatile combined-use
    "Downtown" = "#f44336",       # Red - high-density urban core
    "Employment" = "#9c27b0",     # Purple - office/employment centers
    "Industrial" = "#607d8b",     # Blue Grey - manufacturing/industrial
    "Special" = "#795548"         # Brown - institutional/special use
  )
  
  # Add styling columns
  zoning_data <- zoning_data |>
    mutate(
      # Use case_when instead of named vector indexing to avoid jsonlite warning
      fill_color = case_when(
        zone_category == "Residential" ~ "#8bc34a",    # Light Green
        zone_category == "Commercial" ~ "#ff9800",     # Orange
        zone_category == "Mixed-Use" ~ "#03a9f4",      # Blue
        zone_category == "Downtown" ~ "#795548",       # Red
        zone_category == "Employment" ~ "#9c27b0",     # Purple
        zone_category == "Industrial" ~ "#607d8b",     # Blue Grey
        zone_category == "Special" ~ "#f44336",        # Brown
        TRUE ~ "#666666"  # Fallback for unmapped categories
      ),
      # Create popup content
      popup_content = create_zoning_popup(pick(everything()))
    )
  
  # Calculate map center from zoning data bounds
  bounds <- st_bbox(zoning_data)
  center_lat <- mean(c(bounds[2], bounds[4]))
  center_lon <- mean(c(bounds[1], bounds[3]))
  
  # Create base map
  map <- leaflet() |>
    setView(lng = center_lon, lat = center_lat, zoom = 11) |>
    addTiles()
  
  # Get unique categories in logical display order
  category_order <- c("Residential", "Commercial", "Mixed-Use", "Downtown", "Employment", "Industrial", "Special")
  existing_categories <- category_order[category_order %in% unique(zoning_data$zone_category)]
  
  # Add zoning polygons by category for proper layer control
  for (category in existing_categories) {
    category_data <- zoning_data |> filter(zone_category == category)
    if (nrow(category_data) > 0) {
      map <- map |>
        addPolygons(
          data = category_data,
          fillColor = ~fill_color,
          fillOpacity = 0.6,
          color = "#000000",
          weight = 1,
          opacity = 0.8,
          popup = ~popup_content,
          label = ~paste(zone_code, "-", zone_category),
          group = category
        )
    }
  }
  
  # Add GPS clusters overlay if provided
  if (!is.null(gps_clusters)) {
    # Ensure GPS data has required columns
    if (all(c("lat", "lon", "cluster_id") %in% names(gps_clusters))) {
      gps_clusters <- gps_clusters |>
        mutate(
          popup_gps = create_cluster_popup(pick(everything())),
          short_label = case_when(
            !is.na(display_name) & nchar(display_name) <= 30 ~ display_name,
            !is.na(display_name) ~ str_c(substr(display_name, 1, 27), "..."),
            !is.na(city) ~ city,
            TRUE ~ str_c("Cluster ", cluster_id)
          )
        )
      
      map <- map |>
        addCircleMarkers(
          data = gps_clusters,
          lng = ~lon, lat = ~lat,
          radius = 6,
          color = "#000000",
          fillColor = "#e91e63",  # Pink to contrast with zoning colors
          fillOpacity = 0.8,
          stroke = TRUE,
          weight = 2,
          popup = ~popup_gps,
          label = ~short_label,
          group = "GPS Clusters"
        )
      
      existing_categories <- c(existing_categories, "GPS Clusters")
    }
  }
  
  # Add info box with zoning statistics
  info_html <- create_zoning_info_box(zoning_data)
  map <- map |> addControl(html = info_html, position = "topright")
  
  # Add layer controls if requested
  if (layer_controls && length(existing_categories) > 1) {
    map <- map |>
      addLayersControl(
        overlayGroups = existing_categories,
        options = layersControlOptions(collapsed = length(existing_categories) > 6)
      )
  }
  
  # Add legend if requested
  if (show_legend) {
    # Only include categories that exist in the data
    legend_categories <- existing_categories[existing_categories != "GPS Clusters"]
    if (length(legend_categories) > 0) {
      legend_colors <- zoning_colors[legend_categories]
      
      # Calculate area statistics for legend labels
      area_stats <- zoning_data |>
        st_drop_geometry() |>
        group_by(zone_category) |>
        summarise(
          n_districts = n(),
          total_area_km2 = round(sum(area_sqm, na.rm = TRUE) / 1000000, 1),
          .groups = "drop"
        ) |>
        filter(zone_category %in% legend_categories)
      
      # Create legend colors and labels as separate vectors (not named)
      legend_colors_vec <- c()
      legend_labels_vec <- c()
      
      for (category in legend_categories) {
        # Get color using case_when logic
        color <- case_when(
          category == "Residential" ~ "#8bc34a",
          category == "Commercial" ~ "#ff9800", 
          category == "Mixed-Use" ~ "#03a9f4",
          category == "Downtown" ~ "#795548",
          category == "Employment" ~ "#9c27b0",
          category == "Industrial" ~ "#607d8b",
          category == "Special" ~ "#f44336",
          TRUE ~ "#666666"
        )
        legend_colors_vec <- c(legend_colors_vec, color)
        
        # Get label with stats
        stats <- area_stats[area_stats$zone_category == category, ]
        if (nrow(stats) > 0) {
          label <- paste0(category, " (", stats$n_districts, " districts, ", stats$total_area_km2, " km²)")
        } else {
          label <- category
        }
        legend_labels_vec <- c(legend_labels_vec, label)
      }
      
      map <- map |>
        addLegend(
          position = "bottomleft", 
          colors = legend_colors_vec,
          labels = legend_labels_vec,
          title = "Madison Zoning Districts",
          opacity = 0.8
        )
    }
  }
  
  map
}

# Helper functions for zoning visualization

#' Create popup content for zoning polygons
create_zoning_popup <- function(zone_data) {
  paste0(
    "<div style='font-size: 13px; line-height: 1.4;'>",
    "<strong>", zone_data$zone_code, " - ", zone_data$zone_category, "</strong><br/>",
    "Zone ID: ", zone_data$objectid, "<br/>",
    "Area: ", round(zone_data$area_sqm / 1000000, 3), " km²<br/>",
    if (!is.null(zone_data$center_lat) && !is.null(zone_data$center_lon)) {
      paste0("Center: ", round(zone_data$center_lat, 4), ", ", round(zone_data$center_lon, 4))
    } else {
      ""
    },
    "</div>"
  )
}

#' Create info box for zoning map
create_zoning_info_box <- function(zoning_data) {
  # Calculate summary statistics
  total_districts <- nrow(zoning_data)
  total_area_km2 <- round(sum(zoning_data$area_sqm, na.rm = TRUE) / 1000000, 1)
  n_categories <- length(unique(zoning_data$zone_category))
  
  # Most common category
  top_category <- zoning_data |>
    st_drop_geometry() |>
    count(zone_category, sort = TRUE) |>
    slice_head(n = 1) |>
    pull(zone_category)
  
  paste0(
    "<div style='background: rgba(255,255,255,0.9); padding: 10px; border-radius: 5px; ",
    "box-shadow: 0 2px 10px rgba(0,0,0,0.1); font-size: 12px; line-height: 1.3; max-width: 200px;'>",
    "<strong>Madison Zoning Overview</strong><br/>",
    "<strong>", format(total_districts, big.mark = ","), "</strong> districts<br/>",
    "<strong>", n_categories, "</strong> zone categories<br/>",
    "<strong>", total_area_km2, " km²</strong> total area<br/>",
    "Most common: <strong>", top_category, "</strong><br/>",
    "<em>Toggle layers to explore</em>",
    "</div>"
  )
}

# Convenience wrapper functions (DEPRECATED - use pull_db() + plot functions)

#' @deprecated Use pull_db(1, subid = "XXX") |> plot_gps_points() instead
visualize_gps_from_db <- function(con, subid, viz_type = "clusters") {
  warning("visualize_gps_from_db is deprecated. Use pull_db() + plot functions instead.")
  
  if (viz_type == "raw") {
    data <- pull_raw_gps(con, subid = subid)
    if (nrow(data) == 0) stop("No GPS data found for participant ", subid)
    plot_gps_points(data, subid)
  } else {
    data <- pull_gps_clusters(con, subid = subid)
    if (nrow(data) == 0) stop("No clusters found for participant ", subid)
    plot_cluster_map(data, subid)
  }
}