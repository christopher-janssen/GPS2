# popup_utils.R
# Helper functions for GPS3 visualization: popup formatters, color palettes, validation

# Popup Formatter Functions -----------------------------------------------

#' Create GPS Point Popup
#'
#' Format popup HTML for GPS point markers
#'
#' @param row Data frame row with GPS point data
#' @param show_speed Logical. Include speed in popup if TRUE
#' @return HTML string for popup
create_gps_popup <- function(row, show_speed = FALSE) {
  popup_html <- paste0(
    "<div style='font-size: 13px; line-height: 1.5;'>",
    "<strong>GPS Point</strong><br/>",
    "<strong>Time:</strong> ", format(row$time_local, "%Y-%m-%d %H:%M:%S"), "<br/>",
    "<strong>Movement:</strong> ", ifelse(is.na(row$movement_state), "unknown", row$movement_state), "<br/>"
  )

  # Add speed if requested and available
  if (show_speed && !is.na(row$speed_mph)) {
    popup_html <- paste0(
      popup_html,
      "<strong>Speed:</strong> ", round(row$speed_mph, 1), " mph<br/>"
    )
  }

  # Add dwell time if available
  if (!is.na(row$dwell_time_seconds) && row$dwell_time_seconds > 0) {
    popup_html <- paste0(
      popup_html,
      "<strong>Dwell:</strong> ", round(row$dwell_time_seconds / 60, 1), " min<br/>"
    )
  }

  # Add coordinates
  popup_html <- paste0(
    popup_html,
    "<strong>Location:</strong> ", round(row$lat, 5), ", ", round(row$lon, 5),
    "</div>"
  )

  return(popup_html)
}


#' Create POI Popup
#'
#' Format popup HTML for POI markers
#'
#' @param row Data frame row with POI data
#' @param category_name Optional category name to display
#' @return HTML string for popup
create_poi_popup <- function(row, category_name = NULL) {
  # Build address string
  address_parts <- c(
    if ("street" %in% names(row)) row$street else NULL,
    if ("city" %in% names(row)) row$city else NULL
  )
  address <- paste(address_parts[!is.na(address_parts)], collapse = ", ")

  popup_html <- "<div style='font-size: 13px; line-height: 1.5;'>"

  # Add category name if provided
  if (!is.null(category_name)) {
    popup_html <- paste0(popup_html, "<strong>", category_name, "</strong><br/>")
  }

  # Add POI name if available
  if ("name" %in% names(row) && !is.na(row$name) && row$name != "") {
    popup_html <- paste0(popup_html, "<strong>", row$name, "</strong><br/>")
  }

  # Add class/type
  popup_html <- paste0(
    popup_html,
    "<em>", row$class, " - ", row$type, "</em><br/>"
  )

  # Add address if available
  if (address != "") {
    popup_html <- paste0(popup_html, address, "<br/>")
  }

  # Add OSM ID and coordinates
  popup_html <- paste0(
    popup_html,
    "<strong>ID:</strong> ", row$osm_id, "<br/>",
    "<strong>Coords:</strong> ", round(row$lat, 4), ", ", round(row$lon, 4),
    "</div>"
  )

  return(popup_html)
}


#' Create Landuse Popup
#'
#' Format popup HTML for landuse polygons
#'
#' @param row sf data frame row with landuse data
#' @param category_name Optional category name to display
#' @param show_area Logical. Include area in popup if TRUE
#' @return HTML string for popup
create_landuse_popup <- function(row, category_name = NULL, show_area = TRUE) {
  popup_html <- "<div style='font-size: 13px; line-height: 1.5;'>"

  # Add category name if provided
  if (!is.null(category_name)) {
    popup_html <- paste0(popup_html, "<strong>", category_name, "</strong><br/>")
  }

  # Add name or type label
  if ("name" %in% names(row) && !is.na(row$name) && row$name != "") {
    popup_html <- paste0(popup_html, "<strong>", row$name, "</strong><br/>")
  } else {
    popup_html <- paste0(
      popup_html,
      "<strong>", stringr::str_to_title(row$type), " Area</strong><br/>"
    )
  }

  # Add class/type
  popup_html <- paste0(
    popup_html,
    "<em>", row$class, " - ", row$type, "</em><br/>"
  )

  # Add area if requested
  if (show_area && "area_sqkm" %in% names(row) && !is.na(row$area_sqkm)) {
    popup_html <- paste0(
      popup_html,
      "<strong>Area:</strong> ", round(row$area_sqkm, 3), " km²<br/>"
    )
  }

  # Add OSM ID
  popup_html <- paste0(
    popup_html,
    "<strong>ID:</strong> ", row$osm_id,
    "</div>"
  )

  return(popup_html)
}


#' Create ADI Popup
#'
#' Format popup HTML for ADI polygons
#'
#' @param row sf data frame row with ADI data
#' @param metric Which ADI metric is being displayed ("state_decile" or "national_percentile")
#' @return HTML string for popup
create_adi_popup <- function(row, metric) {
  popup_html <- paste0(
    "<div style='font-size: 13px; line-height: 1.5;'>",
    "<strong>Area Deprivation Index</strong><br/>",
    "<strong>FIPS:</strong> ", row$fips_2020, "<br/>"
  )

  # Add appropriate metric value
  if (metric == "state_decile") {
    popup_html <- paste0(
      popup_html,
      "<strong>State Decile:</strong> ", row$adi_state_decile, "/10<br/>"
    )
  } else {
    popup_html <- paste0(
      popup_html,
      "<strong>National Percentile:</strong> ", round(row$adi_national_percentile, 1), "<br/>"
    )
  }

  # Add area
  popup_html <- paste0(
    popup_html,
    "<strong>Area:</strong> ", round(row$area_sqm / 1000000, 2), " km²",
    "</div>"
  )

  return(popup_html)
}


# Color Palette Functions -------------------------------------------------

#' Get Movement State Colors
#'
#' Returns Material Design color palette for GPS movement states
#'
#' @return Named vector of colors
get_movement_colors <- function() {
  c(
    "stationary" = "#f44336",   # Material Red - stops/dwell
    "transition" = "#8bc34a",   # Material Light Green - active travel
    "unknown" = "#9e9e9e"       # Material Grey - missing data
  )
}


#' Get POI Category Colors
#'
#' Returns Material Design color palette for POI categories
#'
#' @return Named vector of colors
get_poi_colors <- function() {
  c(
    "Alcohol" = "#f44336",        # Red - high risk
    "Food" = "#ff9800",           # Orange - dining
    "Healthcare" = "#2196f3",     # Blue - medical
    "Recreation" = "#4caf50",     # Green - fitness/leisure
    "Parks" = "#8bc34a",          # Light Green - outdoor
    "Religious" = "#9c27b0",      # Purple - spiritual
    "Education" = "#ffeb3b",      # Yellow - learning
    "Shopping" = "#ff5722",       # Deep Orange - retail
    "Services" = "#607d8b",       # Blue Grey - civic/professional
    "Transportation" = "#795548", # Brown - transit/automotive
    "Default" = "#9e9e9e"         # Grey - uncategorized
  )
}


#' Get Landuse Type Colors
#'
#' Returns color palette for common landuse types
#'
#' @return Named vector of colors
get_landuse_colors <- function() {
  c(
    # Natural/Parks
    "park" = "#4caf50",              # Green
    "nature_reserve" = "#1b5e20",    # Dark Green
    "forest" = "#33691e",            # Olive
    "grass" = "#8bc34a",             # Light Green
    "meadow" = "#aed581",            # Pale Green

    # Built environment
    "residential" = "#ffeb3b",       # Yellow
    "commercial" = "#ff9800",        # Orange
    "retail" = "#ff5722",            # Deep Orange
    "industrial" = "#607d8b",        # Blue Grey

    # Institutional
    "education" = "#2196f3",         # Blue
    "healthcare" = "#00bcd4",        # Cyan
    "religious" = "#9c27b0",         # Purple

    # Infrastructure
    "transportation" = "#795548",    # Brown
    "parking" = "#9e9e9e",          # Grey

    # Water/Special
    "water" = "#03a9f4",            # Light Blue
    "cemetery" = "#424242",         # Dark Grey

    "default" = "#e0e0e0"           # Light Grey
  )
}


#' Get ADI Color Palette
#'
#' Returns ColorBrewer palette for ADI choropleth
#'
#' @param metric Which ADI metric ("state_decile" or "national_percentile")
#' @return colorNumeric palette function
get_adi_palette <- function(metric = "state_decile") {
  if (metric == "state_decile") {
    leaflet::colorNumeric(
      palette = "RdYlGn",
      domain = 1:10,
      reverse = TRUE  # Red = high deprivation
    )
  } else {
    leaflet::colorNumeric(
      palette = "RdYlGn",
      domain = c(0, 100),
      reverse = TRUE
    )
  }
}


# Utility Functions -------------------------------------------------------

#' Format Address String
#'
#' Combine street and city into formatted address
#'
#' @param street Street name
#' @param city City name
#' @return Formatted address string
format_address <- function(street, city) {
  address_parts <- c(street, city)
  address_parts <- address_parts[!is.na(address_parts) & address_parts != ""]
  paste(address_parts, collapse = ", ")
}


#' Validate Database Connection
#'
#' Check if database connection is valid
#'
#' @param con Database connection object
#' @return NULL (stops with error if invalid)
validate_connection <- function(con) {
  if (is.null(con) || !DBI::dbIsValid(con)) {
    stop("Invalid database connection. Use connect_to_db() first.", call. = FALSE)
  }
  invisible(NULL)
}


#' Validate Bounding Box
#'
#' Check if bbox is properly formatted
#'
#' @param bbox Bounding box vector c(min_lon, min_lat, max_lon, max_lat)
#' @return NULL (stops with error if invalid)
validate_bbox <- function(bbox) {
  if (!is.null(bbox)) {
    if (length(bbox) != 4 || !all(is.numeric(bbox))) {
      stop("bbox must be numeric vector of length 4: c(min_lon, min_lat, max_lon, max_lat)",
           call. = FALSE)
    }
    if (bbox[1] >= bbox[3] || bbox[2] >= bbox[4]) {
      stop("Invalid bbox: min values must be less than max values", call. = FALSE)
    }
  }
  invisible(NULL)
}
