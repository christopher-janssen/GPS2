# scripts/r/visualization.R
# Improved GPS2 visualization functions with consolidated functionality
# Two main functions: one for environment data, one for database

library(leaflet)
library(dplyr)
library(lubridate)
library(htmlwidgets)
library(sf)

source("scripts/r/database.R")

# ==============================================================================
# MAIN VISUALIZATION FUNCTION - ENVIRONMENT DATA
# ==============================================================================

#' Create GPS visualization from R environment data
#' 
#' @param data GPS or cluster data frame already loaded in R
#' @param subid Participant ID to visualize
#' @param viz_type "raw" or "clusters" (clusters always include geocoding if available)
#' @param show_paths Show path lines for raw GPS (default FALSE)
#' @param show_all_days Show all days at once for raw GPS (default FALSE)
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
  
  # filter for participant
  if ("subid" %in% names(data)) {
    participant_data <- data |> filter(subid == !!subid)
  } else {
    stop("Data must contain 'subid' column")
  }
  
  if (nrow(participant_data) == 0) {
    stop(paste("No data found for participant", subid))
  }
  
  # create appropriate visualization
  if (viz_type == "raw") {
    map <- create_raw_gps_map(participant_data, subid, show_paths, show_all_days)
  } else {
    # clusters always include geocoding if available
    map <- create_cluster_map_with_geocoding(participant_data, geocoded_data, subid)
  }
  
  return(map)
}

# ==============================================================================
# MAIN VISUALIZATION FUNCTION - DATABASE
# ==============================================================================

#' Create GPS visualization directly from database
#' 
#' @param subid Participant ID to visualize
#' @param viz_type "raw" or "clusters" (clusters always include geocoding if available)
#' @param show_paths Show path lines for raw GPS (default FALSE)
#' @param show_all_days Show all days at once for raw GPS (default FALSE)
#' 
#' @return Leaflet map object

visualize_gps_db <- function(subid,
                             viz_type = c("raw", "clusters"),
                             show_paths = FALSE,
                             show_all_days = FALSE) {
  
  viz_type <- match.arg(viz_type)
  
  # fetch appropriate data from database
  if (viz_type == "raw") {
    data <- query_gps2_db("
      SELECT subid, lat, lon, dttm_obs, movement_state, speed, duration
      FROM gps2.gps_stationary_points 
      WHERE subid = $1
      ORDER BY dttm_obs;
    ", list(subid))
    
    if (nrow(data) == 0) {
      stop(paste("No GPS data found for participant", subid))
    }
    
    map <- create_raw_gps_map(data, subid, show_paths, show_all_days)
    
  } else {
    # for clusters, always get geocoding info if available
    data <- query_gps2_db("
      SELECT 
        lc.cluster_id, lc.lat, lc.lon, lc.n_points,
        lc.first_visit, lc.last_visit, lc.total_visits,
        lc.total_duration_hours, lc.unique_days,
        cg.display_name, cg.road, cg.city, cg.state, cg.postcode,
        cg.geocoding_confidence, cg.place_type
      FROM gps2.location_clusters lc
      LEFT JOIN gps2.cluster_geocoding cg 
        ON lc.subid = cg.subid AND lc.cluster_id = cg.cluster_id
      WHERE lc.subid = $1
      ORDER BY lc.total_duration_hours DESC;
    ", list(subid))
    
    if (nrow(data) == 0) {
      stop(paste("No clusters found for participant", subid))
    }
    
    map <- create_cluster_map_with_geocoding(data, NULL, subid)
  }
  
  return(map)
}

# ==============================================================================
# HELPER FUNCTIONS - RAW GPS VISUALIZATION
# ==============================================================================

create_raw_gps_map <- function(data, subid, show_paths, show_all_days) {
  
  # prepare data
  data <- data |>
    mutate(
      date = as.Date(dttm_obs),
      time_str = format(dttm_obs, "%H:%M:%S")
    ) |>
    arrange(dttm_obs)
  
  # initialize map
  map <- leaflet() |>
    setView(lng = mean(data$lon), lat = mean(data$lat), zoom = 13) |>
    addTiles()
  
  # UW-Madison Badger Red
  badger_red <- "#c5050c"
  unique_dates <- sort(unique(data$date))
  
  # add data by day
  for (i in seq_along(unique_dates)) {
    date <- unique_dates[i]
    day_data <- data |> filter(date == !!date)
    date_str <- format(date, "%Y-%m-%d (%A)")
    
    # add points 
    map <- map |>
      addCircleMarkers(
        data = day_data,
        lng = ~lon, lat = ~lat,
        color = "#000",           
        fillColor = badger_red,   
        radius = 5,               
        fillOpacity = 0.8,        # Same opacity as cluster points
        stroke = TRUE, 
        weight = 1,               
        popup = ~paste0(
          "<strong>", date_str, "</strong><br>",
          "Time: ", time_str, "<br>",
          "State: ", movement_state, "<br>",
          ifelse(!is.na(speed), paste0("Speed: ", round(speed, 1), " mph<br>"), ""),
          ifelse(!is.na(duration), paste0("Duration: ", round(duration, 1), " min<br>"), ""),
          "Location: ", round(lat, 5), ", ", round(lon, 5)
        ),
        group = if(show_all_days) "All Points" else date_str
      )
    
    # add paths if requested 
    if (show_paths && nrow(day_data) > 1) {
      map <- map |>
        addPolylines(
          data = day_data,
          lng = ~lon, lat = ~lat,
          color = "#000", weight = 2, opacity = 0.5,
          group = if(show_all_days) "Paths" else paste(date_str, "Path")
        )
    }
  }
  
  # add info box BEFORE layer controls for cleaner layout
  map <- map |>
    addControl(
      html = paste0(
        "<div style='background: rgba(255,255,255,0.9); padding: 10px; border-radius: 5px; border: 1px solid #ccc;'>",
        "<strong>Participant ", subid, " - Raw GPS</strong><br>",
        "<strong>", nrow(data), "</strong> GPS points<br>",
        "<strong>", length(unique_dates), "</strong> days<br>",
        "From ", min(data$date), " to ", max(data$date),
        "</div>"
      ),
      position = "topright"
    )
  
  # add layer controls for multiple days (IMPROVED: better UX for many days)
  if (!show_all_days && length(unique_dates) > 1) {
    groups <- format(unique_dates, "%Y-%m-%d (%A)")
    if (show_paths) {
      groups <- c(groups, paste(groups, "Path"))
    }
    
    # better approach for many days
    if (length(unique_dates) <= 7) {
      # few days: use regular controls
      map <- map |>
        addLayersControl(
          baseGroups = if(!show_paths) groups else NULL,
          overlayGroups = if(show_paths) groups else NULL,
          options = layersControlOptions(collapsed = FALSE)
        )
    } else {
      # many days: use collapsed control with first day visible
      map <- map |>
        addLayersControl(
          baseGroups = if(!show_paths) groups else NULL,
          overlayGroups = if(show_paths) groups else NULL,
          options = layersControlOptions(
            collapsed = TRUE,  # start collapsed for many days
            autoZIndex = TRUE
          )
        )
    }
  }
  
  return(map)
}

create_cluster_map_with_geocoding <- function(cluster_data, geocoded_data, subid) {
  
  # handle column naming
  if ("cluster_id" %in% names(cluster_data) && !"cluster" %in% names(cluster_data)) {
    cluster_data <- cluster_data |> rename(cluster = cluster_id)
  }
  
  # if geocoded_data provided separately, merge it
  if (!is.null(geocoded_data)) {
    if ("cluster_id" %in% names(geocoded_data) && !"cluster" %in% names(geocoded_data)) {
      geocoded_data <- geocoded_data |> rename(cluster = cluster_id)
    }
    
    # merge geocoding info
    data <- cluster_data |>
      left_join(geocoded_data |> 
                  select(any_of(c("cluster", "display_name", "road", "city", "state", 
                                  "postcode", "geocoding_confidence", "place_type", "geocoding_method"))),
                by = "cluster")
  } else {
    # assume geocoding info may already be in cluster_data
    data <- cluster_data
  }
  
  # ensure datetime columns are proper format
  if ("first_visit" %in% names(data)) {
    data$first_visit <- as.POSIXct(data$first_visit)
    data$last_visit <- as.POSIXct(data$last_visit)
  }
  
  # check what columns exist BEFORE the mutate call
  has_display_name_col <- "display_name" %in% names(data)
  has_road_col <- "road" %in% names(data)
  has_city_col <- "city" %in% names(data)
  has_state_col <- "state" %in% names(data)
  has_postcode_col <- "postcode" %in% names(data)
  has_confidence_col <- "geocoding_confidence" %in% names(data)
  has_place_type_col <- "place_type" %in% names(data)
  has_method_col <- "geocoding_method" %in% names(data)
  
  # enhanced classification and display preparation
  data <- data |>
    mutate(
      # location type classification
      location_type = case_when(
        unique_days >= 5 & total_visits >= 8 ~ "Routine",
        unique_days >= 3 & total_visits >= 5 ~ "Frequent", 
        unique_days >= 2 ~ "Occasional",
        TRUE ~ "Rare"
      ),
      
      marker_color = case_when(
        location_type == "Routine" ~ "#d73027",
        location_type == "Frequent" ~ "#fc8d59",
        location_type == "Occasional" ~ "#91bfdb", 
        location_type == "Rare" ~ "#999999"
      ),
      # importance scoring for marker sizing
      importance_score = scale(total_visits)[,1] + scale(total_duration_hours)[,1],
      marker_size = pmax(4, pmin(12, 6 + importance_score * 2)),
      
      # better geocoding status and address handling
      has_geocoding = if(has_display_name_col) {
        !is.na(display_name) & display_name != "" & display_name != "Unknown Location"
      } else {
        FALSE
      },
      
      # enhanced address display
      display_location = if(has_display_name_col) {
        case_when(
          has_geocoding ~ display_name,
          TRUE ~ paste0("Location ", cluster)
        )
      } else {
        paste0("Location ", cluster)
      }
    )
  
  # create short_address after the main mutate to avoid column reference issues
  if(has_display_name_col) {
    data$short_address <- sapply(1:nrow(data), function(i) {
      if(data$has_geocoding[i]) {
        if(has_road_col && has_city_col && !is.na(data$road[i]) && data$road[i] != "") {
          city_part <- if(!is.na(data$city[i]) && data$city[i] != "") paste0(", ", data$city[i]) else ""
          return(paste0(data$road[i], city_part))
        } else if(has_city_col && !is.na(data$city[i]) && data$city[i] != "") {
          return(data$city[i])
        } else {
          return(substr(data$display_name[i], 1, 50))
        }
      } else {
        return(paste0("Location ", data$cluster[i]))
      }
    })
  } else {
    data$short_address <- paste0("Location ", data$cluster)
  }
  
  # create popup content using a separate function to handle the complexity
  data$popup_content <- sapply(1:nrow(data), function(i) {
    row <- data[i, ]
    
    # base content
    content <- paste0(
      "<strong>Location Cluster ", row$cluster, "</strong><br>",
      "<em>", row$location_type, " location</em><br><br>"
    )
    
    # address section - only if geocoding columns exist
    if(any(c(has_road_col, has_city_col, has_state_col, has_postcode_col))) {
      content <- paste0(content, "<strong>Address:</strong><br>")
      
      if(has_road_col) {
        road_text <- if(!is.na(row$road) && row$road != "") row$road else "Street not identified"
        content <- paste0(content, road_text, "<br>")
      }
      
      city_text <- if(has_city_col && !is.na(row$city) && row$city != "") row$city else "City not identified"
      content <- paste0(content, city_text)
      
      if(has_state_col && !is.na(row$state) && row$state != "") {
        content <- paste0(content, ", ", row$state)
      }
      
      if(has_postcode_col && !is.na(row$postcode) && row$postcode != "") {
        content <- paste0(content, " ", row$postcode)
      }
      
      content <- paste0(content, "<br><br>")
    }
    
    # visit pattern section
    content <- paste0(content,
                      "<strong>Visit Pattern:</strong><br>",
                      "â€¢ Total visits: ", row$total_visits, "<br>",
                      "â€¢ Days visited: ", row$unique_days, "<br>", 
                      "â€¢ Total time: ", round(row$total_duration_hours, 1), " hours<br>")
    
    if("n_points" %in% names(row)) {
      content <- paste0(content, "â€¢ GPS points: ", row$n_points, "<br>")
    }
    content <- paste0(content, "<br>")
    
    # timeline section
    content <- paste0(content,
                      "<strong>Timeline:</strong><br>",
                      "â€¢ First visit: ", format(row$first_visit, "%m/%d/%Y %H:%M"), "<br>",
                      "â€¢ Last visit: ", format(row$last_visit, "%m/%d/%Y %H:%M"), "<br><br>")
    
    # geocoding info section
    if(any(c(has_method_col, has_confidence_col, has_place_type_col))) {
      content <- paste0(content, "<strong>Geocoding Info:</strong><br>")
      
      if(has_method_col) {
        method_text <- if(is.na(row$geocoding_method)) "Unknown" else as.character(row$geocoding_method)
        content <- paste0(content, "â€¢ Method: ", method_text, "<br>")
      }
      
      if(has_confidence_col) {
        conf_text <- if(is.na(row$geocoding_confidence)) "N/A" else paste0(round(row$geocoding_confidence * 100), "%")
        content <- paste0(content, "â€¢ Confidence: ", conf_text, "<br>")
      }
      
      if(has_place_type_col && !is.na(row$place_type)) {
        content <- paste0(content, "â€¢ Type: ", row$place_type, "<br>")
      }
      
      content <- paste0(content, "<br>")
    }
    
    # coordinates
    content <- paste0(content,
                      "<strong>Coordinates:</strong><br>",
                      round(row$lat, 4), ", ", round(row$lon, 4))
    
    return(content)
  })
  
  # initialize map
  map <- leaflet() |>
    setView(lng = mean(data$lon), lat = mean(data$lat), zoom = 12) |>
    addTiles()
  
  # add clusters by type
  ordered_types <- c("Routine", "Frequent", "Occasional", "Rare")
  existing_types <- ordered_types[ordered_types %in% data$location_type]
  
  for (type in existing_types) {
    type_data <- data |> filter(location_type == type)
    
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
  
  # enhanced summary statistics
  start_date <- min(data$first_visit)
  end_date <- max(data$last_visit)
  
  # count different address types
  address_stats <- data |>
    summarise(
      total_locations = n(),
      with_addresses = sum(has_geocoding),
      with_roads = if(has_road_col) sum(!is.na(road) & road != "") else 0,
      with_cities = if(has_city_col) sum(!is.na(city) & city != "") else 0
    )
  
  geocoding_rate <- round(address_stats$with_addresses / address_stats$total_locations * 100, 1)
  
  # enhanced info box
  map <- map |>
    addControl(
      html = paste0(
        "<div style='background: rgba(255,255,255,0.95); padding: 12px; ",
        "border-radius: 8px; border: 2px solid #333; font-family: Arial;'>",
        "<strong>Participant ", subid, " - Location Clusters</strong><br>",
        "<strong>", nrow(data), "</strong> meaningful locations<br>",
        "<strong>", sum(data$total_visits), "</strong> total visits<br>",
        "<strong>", round(sum(data$total_duration_hours), 1), "</strong> hours tracked<br>",
        "<strong>Timeframe:</strong> [", format(start_date, "%m-%d-%Y"), " to ", 
        format(end_date, "%m-%d-%Y"), "]<br><br>",
        
        # only show geocoding results if there are any geocoding columns
        if(any(c(has_display_name_col, has_road_col, has_city_col, has_state_col, has_postcode_col))) {
          paste0(
            "<strong>Geocoding Results:</strong><br>",
            "- Complete addresses: ", address_stats$with_addresses, " (", geocoding_rate, "%)<br>",
            "- Street addresses: ", address_stats$with_roads, "<br>",
            "- City identified: ", address_stats$with_cities, "<br>"
          )
        } else {
          "<strong>No geocoding data available</strong><br>"
        },
        
        "<em>Marker size = visit importance</em>",
        "</div>"
      ),
      position = "topright"
    )
  
  # add layer control
  if (length(existing_types) > 1) {
    map <- map |>
      addLayersControl(
        overlayGroups = existing_types,
        options = layersControlOptions(collapsed = FALSE)
      )
  }
  
  # enhanced legend with counts
  legend_colors <- c()
  legend_labels <- c()
  
  for (type in existing_types) {
    type_color <- case_when(
      type == "Routine" ~ "#d73027",
      type == "Frequent" ~ "#fc8d59",
      type == "Occasional" ~ "#91bfdb", 
      type == "Rare" ~ "#999999"
    )
    type_count <- sum(data$location_type == type)
    
    legend_colors <- c(legend_colors, type_color)
    legend_labels <- c(legend_labels, paste0(type, " (", type_count, ")"))
  }
  
  map <- map |>
    addLegend(
      position = "bottomleft",
      colors = legend_colors,
      labels = legend_labels,
      title = paste0("Location Types<br><small>", nrow(data), " clusters</small>"),
      opacity = 0.8
    )
  
  return(map)
}

#' Get summary statistics for a participant
#' 
#' @param subid Participant ID
#' @param use_db Use database (TRUE) or expect data in environment (FALSE)
#' 
#' @return List with summary statistics

get_participant_summary <- function(subid, use_db = TRUE) {
  
  summary <- list(subid = subid)
  
  if (use_db) {
    # get GPS point count
    gps_stats <- query_gps2_db("
      SELECT 
        ROUND(COUNT(*),1) as total_points,
        ROUND(COUNT(DISTINCT DATE(dttm_obs)),1) as unique_days,
        MIN(dttm_obs) as first_point,
        MAX(dttm_obs) as last_point,
        ROUND(SUM(CASE WHEN movement_state = 'stationary' THEN 1 ELSE 0 END),1) as stationary_points
      FROM gps2.gps_stationary_points
      WHERE subid = $1;
    ", list(subid))
    
    # get cluster stats
    cluster_stats <- query_gps2_db("
      SELECT 
        ROUND(COUNT(*),1) as total_clusters,
        ROUND(SUM(total_visits),1) as total_visits,
        ROUND(SUM(total_duration_hours),1) as total_hours,
        MAX(unique_days) as max_days_at_location
      FROM gps2.location_clusters
      WHERE subid = $1;
    ", list(subid))
    
    # get geocoding stats
    geocoding_stats <- query_gps2_db("
      SELECT 
      ROUND(COUNT(*),1) as geocoded_clusters
      FROM gps2.cluster_geocoding
      WHERE subid = $1 AND display_name IS NOT NULL;
    ", list(subid))
    
    summary$gps <- gps_stats
    summary$clusters <- cluster_stats
    summary$geocoding <- geocoding_stats
    
  } else {
    if (exists("gps_data")) {
      participant_gps <- gps_data |> filter(subid == !!subid)
      summary$gps <- list(
        total_points = nrow(participant_gps),
        unique_days = n_distinct(as.Date(participant_gps$dttm_obs))
      )
    }
    
    if (exists("cluster_data")) {
      participant_clusters <- cluster_data |> filter(subid == !!subid)
      summary$clusters <- list(
        total_clusters = nrow(participant_clusters),
        total_visits = sum(participant_clusters$total_visits),
        total_hours = sum(participant_clusters$total_duration_hours)
      )
    }
  }
  
  # print summary
  cat("\nSummary for Participant", subid, "\n")
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
  }
  
  if (!is.null(summary$geocoding) && !is.null(summary$geocoding$geocoded_clusters)) {
    cat("\nGeocoded Clusters:", summary$geocoding$geocoded_clusters, "\n")
  }
  
  return(invisible(summary))
}

# ==============================================================================
# ZONING VISUALIZATION
# ==============================================================================

# Map Madison zoning districts with proper sf handling
map_zoning_districts <- function(show_categories = NULL, max_zones = 100) {
  
  cat("Loading Madison zoning districts...\n")
  
  # build category filter
  category_filter <- ""
  if (!is.null(show_categories)) {
    category_list <- paste0("'", show_categories, "'", collapse = ",")
    category_filter <- paste0("WHERE zone_category IN (", category_list, ")")
  }
  
  # get zone count
  count_query <- paste0("
    SELECT COUNT(*)::integer as count 
    FROM gps2.zoning_districts 
    WHERE geometry IS NOT NULL ", 
                        if (category_filter != "") gsub("WHERE", "AND", category_filter) else ""
  )
  total_zones <- as.integer(query_gps2_db(count_query)$count)
  
  if (total_zones == 0) {
    cat("⚠ No zoning data found\n")
    return(leaflet() |> addTiles() |> setView(lng = -89.384373, lat = 43.074713, zoom = 10))
  }
  
  cat("   Found", total_zones, "zone types\n")
  
  # read zoning data using sf
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
    cat("⚠ Error reading zoning data:", e$message, "\n")
    return(NULL)
  }, finally = {
    disconnect_gps2_db(con)
  })
  
  if (is.null(zoning_sf) || nrow(zoning_sf) == 0) {
    return(leaflet() |> addTiles() |> setView(lng = -89.384373, lat = 43.074713, zoom = 10))
  }
  
  # ensure proper CRS
  if (st_crs(zoning_sf)$epsg != 4326) {
    zoning_sf <- st_transform(zoning_sf, 4326)
  }
  
  # color scheme for zoning categories
  zone_colors <- c(
    "Residential" = "#90EE90", "Commercial" = "#FFB6C1", "Mixed-Use" = "#DDA0DD",
    "Downtown" = "#FF6347", "Employment" = "#87CEEB", "Industrial" = "#D3D3D3", 
    "Special" = "#F0E68C", "Historic" = "#DEB887", "Other" = "#DCDCDC"
  )
  
  # create base map
  map <- leaflet() |>
    addTiles() |>
    setView(lng = -89.384373, lat = 43.074713, zoom = 11)
  
  # add zones by category and count them
  categories <- unique(zoning_sf$zone_category)
  category_counts <- c()
  
  for (category in categories) {
    category_zones <- zoning_sf[zoning_sf$zone_category == category, ]
    zone_color <- unname(zone_colors[category])  # remove names to fix JSON issue
    if (is.na(zone_color)) zone_color <- unname(zone_colors["Other"])
    
    # store count for legend
    category_counts[category] <- nrow(category_zones)
    
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
  
  # add layer control (positioned top right with improved options)
  map <- map |>
    addLayersControl(
      overlayGroups = categories,
      options = layersControlOptions(
        collapsed = TRUE,
        position = "topright"
      )
    )
  
  # create legend with counts (positioned bottom left)
  legend_subset <- zone_colors[names(zone_colors) %in% categories]
  legend_labels_with_counts <- c()
  
  for (category in names(legend_subset)) {
    count <- category_counts[category]
    legend_labels_with_counts <- c(legend_labels_with_counts, paste0(category, " (", count, ")"))
  }
  
  map <- map |>
    addLegend(
      position = "bottomleft",  # changed from bottomright to bottomleft
      colors = unname(legend_subset),  # remove names to fix JSON issue
      labels = legend_labels_with_counts,  # now includes counts
      title = "Zoning Categories",
      opacity = 0.7
    )
  
  return(map)
}