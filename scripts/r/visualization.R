# visualization.R
# Main visualization functions for GPS3 database

# Load required libraries
library(DBI)
library(RPostgres)
library(leaflet)
library(sf)
library(tidyverse)

# Source popup utilities
source(here::here("scripts/r/popup_utils.R"))


# GPS Points Visualization ------------------------------------------------

#' Plot GPS Points for a Subject
#'
#' Creates an interactive Leaflet map displaying GPS points from the processed_gps table.
#' Points can be colored by movement state and include dwell time and speed information.
#'
#' @param con Database connection object from connect_to_db()
#' @param subid Subject ID (integer) to visualize
#' @param show_movement_colors Logical. Color points by movement_state if TRUE (default TRUE)
#' @param show_speed Logical. Include speed information in popups if TRUE (default FALSE)
#'
#' @return A Leaflet map object
#'
#' @examples
#' con <- connect_to_db()
#' plot_gps_points(con, subid = 1)
#' plot_gps_points(con, subid = 1, show_movement_colors = TRUE, show_speed = TRUE)
#'
#' @export
plot_gps_points <- function(con, subid, show_movement_colors = TRUE, show_speed = FALSE) {
  # Validate connection
  validate_connection(con)

  # Check if subid exists
  subid_exists <- dbGetQuery(
    con,
    "SELECT COUNT(*) as n FROM risk1.subjects WHERE subid = $1",
    params = list(subid)
  )

  if (subid_exists$n == 0) {
    stop("Subject ID ", subid, " not found in database.", call. = FALSE)
  }

  # Query GPS data
  gps_data <- dbGetQuery(
    con,
    "SELECT subid, lat, lon, time, time_local, movement_state,
            speed_mph, dist_miles, dwell_time_seconds
     FROM risk1.processed_gps
     WHERE subid = $1
     ORDER BY time",
    params = list(subid)
  )

  # Check if data exists
  if (nrow(gps_data) == 0) {
    stop("No GPS data found for subject ID ", subid, call. = FALSE)
  }

  # Get movement colors
  movement_colors <- get_movement_colors()

  # Determine point colors
  if (show_movement_colors) {
    gps_data <- gps_data |>
      mutate(
        movement_state = ifelse(is.na(movement_state), "unknown", movement_state),
        color = movement_colors[movement_state]
      )
  } else {
    gps_data$color <- "#2196f3"  # Default blue
  }

  # Calculate map center
  center_lat <- mean(gps_data$lat, na.rm = TRUE)
  center_lon <- mean(gps_data$lon, na.rm = TRUE)

  # Create popups
  popups <- purrr::map_chr(1:nrow(gps_data), function(i) {
    create_gps_popup(gps_data[i, ], show_speed = show_speed)
  })

  # Create leaflet map
  map <- leaflet(gps_data) |>
    addTiles() |>
    setView(lng = center_lon, lat = center_lat, zoom = 12) |>
    addCircleMarkers(
      lng = ~lon,
      lat = ~lat,
      color = ~color,
      fillColor = ~color,
      fillOpacity = 0.7,
      radius = 5,
      weight = 1,
      popup = popups
    )

  # Add legend if showing movement colors
  if (show_movement_colors) {
    unique_states <- unique(gps_data$movement_state)
    map <- map |>
      addLegend(
        position = "bottomright",
        colors = movement_colors[unique_states],
        labels = unique_states,
        title = "Movement State",
        opacity = 0.7
      )
  }

  return(map)
}


# POI Visualization -------------------------------------------------------

#' Plot POI Map
#'
#' Creates an interactive Leaflet map displaying OpenStreetMap POI throughout Wisconsin.
#' Supports filtering by class/type combinations with layer controls.
#'
#' @param con Database connection object from connect_to_db()
#' @param class_types Named list of class/type combinations to display. Each element should be
#'   a list with 'class' and 'type' fields. If NULL, shows all POI grouped by class.
#' @param bbox Optional bounding box c(min_lon, min_lat, max_lon, max_lat) to limit display area
#' @param show_names Logical. Show POI names as labels if TRUE (default TRUE)
#'
#' @return A Leaflet map object
#'
#' @examples
#' con <- connect_to_db()
#'
#' # Show specific POI categories
#' alcohol_poi <- list(
#'   "Bars/Pubs" = list(class = "amenity", type = c("bar", "pub", "nightclub")),
#'   "Liquor Stores" = list(class = "shop", type = c("alcohol", "wine"))
#' )
#' plot_poi_map(con, class_types = alcohol_poi)
#'
#' # Show all POI (may be slow)
#' plot_poi_map(con)
#'
#' @export
plot_poi_map <- function(con, class_types = NULL, bbox = NULL, show_names = TRUE) {
  # Validate inputs
  validate_connection(con)
  validate_bbox(bbox)

  # Get POI colors
  poi_colors <- get_poi_colors()

  # Initialize map
  map <- leaflet() |>
    addTiles()

  # Query and add POI data
  if (!is.null(class_types)) {
    # User provided specific categories
    for (category_name in names(class_types)) {
      category <- class_types[[category_name]]

      # Build query
      query <- "
        SELECT osm_id, class, type, name, street, city, lat, lon
        FROM public_data.osm_poi
        WHERE class = $1 AND type = ANY($2::text[])
      "

      # Add bbox filter if provided
      if (!is.null(bbox)) {
        query <- paste0(
          query,
          " AND ST_MakePoint(lon, lat) && ST_MakeEnvelope($3, $4, $5, $6, 4326)"
        )
        poi_data <- dbGetQuery(
          con,
          query,
          params = list(category$class, category$type, bbox[1], bbox[2], bbox[3], bbox[4])
        )
      } else {
        poi_data <- dbGetQuery(
          con,
          query,
          params = list(category$class, category$type)
        )
      }

      # Skip if no data
      if (nrow(poi_data) == 0) next

      # Sample if too many points
      if (nrow(poi_data) > 10000) {
        message("Category '", category_name, "' contains ", nrow(poi_data),
                " points. Sampling 10,000 for display.")
        poi_data <- poi_data |>
          slice_sample(n = 10000)
      }

      # Get color for this category
      color <- ifelse(category_name %in% names(poi_colors),
                      poi_colors[category_name],
                      poi_colors["Default"])

      # Create popups
      popups <- purrr::map_chr(1:nrow(poi_data), function(i) {
        create_poi_popup(poi_data[i, ], category_name = category_name)
      })

      # Add markers to map
      map <- map |>
        addCircleMarkers(
          data = poi_data,
          lng = ~lon,
          lat = ~lat,
          color = color,
          fillColor = color,
          fillOpacity = 0.6,
          radius = 6,
          weight = 1,
          popup = popups,
          group = category_name
        )
    }

    # Add layer controls
    if (length(class_types) > 0) {
      map <- map |>
        addLayersControl(
          overlayGroups = names(class_types),
          options = layersControlOptions(collapsed = FALSE)
        )
    }

  } else {
    # Show all POI grouped by class
    # Query to get unique classes
    classes_query <- "SELECT DISTINCT class FROM public_data.osm_poi ORDER BY class"
    classes <- dbGetQuery(con, classes_query)$class

    for (poi_class in classes) {
      # Build query
      query <- "
        SELECT osm_id, class, type, name, street, city, lat, lon
        FROM public_data.osm_poi
        WHERE class = $1
      "

      # Add bbox filter if provided
      if (!is.null(bbox)) {
        query <- paste0(
          query,
          " AND ST_MakePoint(lon, lat) && ST_MakeEnvelope($2, $3, $4, $5, 4326)"
        )
        poi_data <- dbGetQuery(
          con,
          query,
          params = list(poi_class, bbox[1], bbox[2], bbox[3], bbox[4])
        )
      } else {
        poi_data <- dbGetQuery(
          con,
          query,
          params = list(poi_class)
        )
      }

      # Skip if no data
      if (nrow(poi_data) == 0) next

      # Sample if too many points
      if (nrow(poi_data) > 10000) {
        message("Class '", poi_class, "' contains ", nrow(poi_data),
                " points. Sampling 10,000 for display.")
        poi_data <- poi_data |>
          slice_sample(n = 10000)
      }

      # Use default color
      color <- poi_colors["Default"]

      # Create popups
      popups <- purrr::map_chr(1:nrow(poi_data), function(i) {
        create_poi_popup(poi_data[i, ], category_name = NULL)
      })

      # Add markers to map
      map <- map |>
        addCircleMarkers(
          data = poi_data,
          lng = ~lon,
          lat = ~lat,
          color = color,
          fillColor = color,
          fillOpacity = 0.6,
          radius = 6,
          weight = 1,
          popup = popups,
          group = poi_class
        )
    }

    # Add layer controls
    if (length(classes) > 0) {
      map <- map |>
        addLayersControl(
          overlayGroups = classes,
          options = layersControlOptions(collapsed = TRUE)
        )
    }
  }

  return(map)
}


# Landuse Visualization ---------------------------------------------------

#' Plot Landuse Map
#'
#' Creates an interactive Leaflet map displaying OpenStreetMap landuse polygons throughout Wisconsin.
#' Supports filtering by class/type combinations with layer controls.
#'
#' @param con Database connection object from connect_to_db()
#' @param landuse_types Named list of class/type combinations to display. Each element should be
#'   a list with 'class' and 'type' fields. If NULL, shows all landuse grouped by type.
#' @param bbox Optional bounding box c(min_lon, min_lat, max_lon, max_lat) to limit display area
#' @param show_areas Logical. Include area_sqkm in popups if TRUE (default TRUE)
#'
#' @return A Leaflet map object
#'
#' @examples
#' con <- connect_to_db()
#'
#' # Show specific landuse categories
#' green_spaces <- list(
#'   "Parks" = list(class = "leisure", type = c("park", "nature_reserve")),
#'   "Forests" = list(class = "landuse", type = c("forest", "meadow"))
#' )
#' plot_landuse_map(con, landuse_types = green_spaces)
#'
#' # Show all landuse
#' plot_landuse_map(con)
#'
#' @export
plot_landuse_map <- function(con, landuse_types = NULL, bbox = NULL, show_areas = TRUE) {
  # Validate inputs
  validate_connection(con)
  validate_bbox(bbox)

  # Get landuse colors
  landuse_colors <- get_landuse_colors()

  # Initialize map
  map <- leaflet() |>
    addTiles()

  # Query and add landuse data
  if (!is.null(landuse_types)) {
    # User provided specific categories
    for (category_name in names(landuse_types)) {
      category <- landuse_types[[category_name]]

      # Build query
      query <- "
        SELECT osm_id, class, type, name, area_sqkm, geom
        FROM public_data.osm_landuse
        WHERE class = $1 AND type = ANY($2::text[])
      "

      # Add bbox filter if provided
      if (!is.null(bbox)) {
        query <- paste0(
          query,
          " AND geom && ST_MakeEnvelope($3, $4, $5, $6, 4326)"
        )
        landuse_data <- st_read(
          con,
          query = query,
          params = list(category$class, category$type, bbox[1], bbox[2], bbox[3], bbox[4]),
          quiet = TRUE
        )
      } else {
        landuse_data <- st_read(
          con,
          query = query,
          params = list(category$class, category$type),
          quiet = TRUE
        )
      }

      # Skip if no data
      if (nrow(landuse_data) == 0) next

      # Simplify geometries if many polygons
      if (nrow(landuse_data) > 5000) {
        message("Category '", category_name, "' contains ", nrow(landuse_data),
                " polygons. Simplifying geometries for display.")
        landuse_data <- landuse_data |>
          st_simplify(dTolerance = 0.001, preserveTopology = TRUE)
      }

      # Get color for this category (try to match type)
      # Use first type in the category types list
      first_type <- category$type[1]
      color <- ifelse(first_type %in% names(landuse_colors),
                      landuse_colors[first_type],
                      landuse_colors["default"])

      # Create popups
      popups <- purrr::map_chr(1:nrow(landuse_data), function(i) {
        create_landuse_popup(landuse_data[i, ], category_name = category_name, show_area = show_areas)
      })

      # Add polygons to map
      map <- map |>
        addPolygons(
          data = landuse_data,
          color = color,
          fillColor = color,
          fillOpacity = 0.5,
          weight = 1,
          popup = popups,
          group = category_name
        )
    }

    # Add layer controls
    if (length(landuse_types) > 0) {
      map <- map |>
        addLayersControl(
          overlayGroups = names(landuse_types),
          options = layersControlOptions(collapsed = FALSE)
        )
    }

  } else {
    # Show all landuse grouped by type
    # Query to get unique types
    types_query <- "SELECT DISTINCT type FROM public_data.osm_landuse ORDER BY type"
    types <- dbGetQuery(con, types_query)$type

    for (landuse_type in types) {
      # Build query
      query <- "
        SELECT osm_id, class, type, name, area_sqkm, geom
        FROM public_data.osm_landuse
        WHERE type = $1
      "

      # Add bbox filter if provided
      if (!is.null(bbox)) {
        query <- paste0(
          query,
          " AND geom && ST_MakeEnvelope($2, $3, $4, $5, 4326)"
        )
        landuse_data <- st_read(
          con,
          query = query,
          params = list(landuse_type, bbox[1], bbox[2], bbox[3], bbox[4]),
          quiet = TRUE
        )
      } else {
        landuse_data <- st_read(
          con,
          query = query,
          params = list(landuse_type),
          quiet = TRUE
        )
      }

      # Skip if no data
      if (nrow(landuse_data) == 0) next

      # Simplify geometries if many polygons
      if (nrow(landuse_data) > 5000) {
        message("Type '", landuse_type, "' contains ", nrow(landuse_data),
                " polygons. Simplifying geometries for display.")
        landuse_data <- landuse_data |>
          st_simplify(dTolerance = 0.001, preserveTopology = TRUE)
      }

      # Get color for this type
      color <- ifelse(landuse_type %in% names(landuse_colors),
                      landuse_colors[landuse_type],
                      landuse_colors["default"])

      # Create popups
      popups <- purrr::map_chr(1:nrow(landuse_data), function(i) {
        create_landuse_popup(landuse_data[i, ], category_name = NULL, show_area = show_areas)
      })

      # Add polygons to map
      map <- map |>
        addPolygons(
          data = landuse_data,
          color = color,
          fillColor = color,
          fillOpacity = 0.5,
          weight = 1,
          popup = popups,
          group = landuse_type
        )
    }

    # Add layer controls
    if (length(types) > 0) {
      map <- map |>
        addLayersControl(
          overlayGroups = types,
          options = layersControlOptions(collapsed = TRUE)
        )
    }
  }

  return(map)
}


# ADI Visualization -------------------------------------------------------

#' Plot ADI Map
#'
#' Creates an interactive Leaflet choropleth map displaying Area Deprivation Index scores
#' by census block group throughout Wisconsin.
#'
#' @param con Database connection object from connect_to_db()
#' @param metric Which ADI metric to display: "state_decile" (1-10) or "national_percentile" (0-100)
#' @param bbox Optional bounding box c(min_lon, min_lat, max_lon, max_lat) to limit display area
#' @param show_legend Logical. Show color scale legend if TRUE (default TRUE)
#'
#' @return A Leaflet map object
#'
#' @examples
#' con <- connect_to_db()
#'
#' # State deciles (default)
#' plot_adi_map(con)
#'
#' # National percentiles
#' plot_adi_map(con, metric = "national_percentile")
#'
#' # Madison area only
#' madison_bbox <- c(-89.5, 43.0, -89.2, 43.15)
#' plot_adi_map(con, bbox = madison_bbox)
#'
#' @export
plot_adi_map <- function(con, metric = "state_decile", bbox = NULL, show_legend = TRUE) {
  # Validate inputs
  validate_connection(con)
  validate_bbox(bbox)

  if (!metric %in% c("state_decile", "national_percentile")) {
    stop("metric must be 'state_decile' or 'national_percentile'", call. = FALSE)
  }

  # Build query
  query <- "
    SELECT fips_2020, adi_state_decile, adi_national_percentile, area_sqm, geometry
    FROM public_data.adi_scores
  "

  # Add bbox filter if provided
  if (!is.null(bbox)) {
    query <- paste0(
      query,
      " WHERE geometry && ST_MakeEnvelope($1, $2, $3, $4, 4326)"
    )
    adi_data <- st_read(
      con,
      query = query,
      params = list(bbox[1], bbox[2], bbox[3], bbox[4]),
      quiet = TRUE
    )
  } else {
    adi_data <- st_read(
      con,
      query = query,
      quiet = TRUE
    )
  }

  # Check if data exists
  if (nrow(adi_data) == 0) {
    stop("No ADI data found", call. = FALSE)
  }

  # Get color palette
  pal <- get_adi_palette(metric)

  # Create popups
  popups <- purrr::map_chr(1:nrow(adi_data), function(i) {
    create_adi_popup(adi_data[i, ], metric = metric)
  })

  # Get metric values for coloring
  if (metric == "state_decile") {
    metric_values <- adi_data$adi_state_decile
    legend_title <- "ADI State Decile"
  } else {
    metric_values <- adi_data$adi_national_percentile
    legend_title <- "ADI National Percentile"
  }

  # Create map
  map <- leaflet(adi_data) |>
    addTiles() |>
    addPolygons(
      fillColor = ~pal(metric_values),
      fillOpacity = 0.7,
      color = "#444444",
      weight = 1,
      popup = popups
    )

  # Add legend if requested
  if (show_legend) {
    map <- map |>
      addLegend(
        position = "bottomright",
        pal = pal,
        values = metric_values,
        title = legend_title,
        opacity = 0.7
      )
  }

  return(map)
}
