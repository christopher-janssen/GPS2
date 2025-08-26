# utils/popup_builders.R
#' HTML popup building utilities for GPS2 visualizations
# stringr loaded via tidyverse in global_setup.R

#' Create a formatted popup section with title and items
create_popup_section <- function(title, items = NULL, style = "strong") {
  if (is.null(items) || length(items) == 0) return("")
  
  content <- str_c("<", style, ">", title, ":</", style, "><br>")
  
  # Handle different item types
  if (is.list(items)) {
    for (name in names(items)) {
      if (!is.na(items[[name]]) && items[[name]] != "") {
        content <- str_c(content, "• ", name, ": ", items[[name]], "<br>")
      }
    }
  } else {
    for (item in items) {
      if (!is.na(item) && item != "") {
        content <- str_c(content, "• ", item, "<br>")
      }
    }
  }
  
  return(str_c(content, "<br>"))
}

#' Create address section for cluster popup
create_address_section <- function(row) {
  address_items <- list()
  
  # Build address components
  if ("road" %in% names(row) && !is.na(row$road) && row$road != "") {
    address_items[["Street"]] <- row$road
  }
  
  city_parts <- c()
  if ("city" %in% names(row) && !is.na(row$city) && row$city != "") {
    city_parts <- c(city_parts, row$city)
  }
  if ("state" %in% names(row) && !is.na(row$state) && row$state != "") {
    city_parts <- c(city_parts, row$state)
  }
  if ("postcode" %in% names(row) && !is.na(row$postcode) && row$postcode != "") {
    city_parts <- c(city_parts, row$postcode)
  }
  
  if (length(city_parts) > 0) {
    address_items[["City"]] <- str_c(city_parts, collapse = ", ")
  }
  
  if (length(address_items) == 0) {
    address_items[["Location"]] <- str_c("Cluster ", row$cluster)
  }
  
  return(create_popup_section("Address", address_items))
}

#' Create visit pattern section
create_visit_pattern_section <- function(row) {
  visit_items <- list(
    "Total visits" = row$total_visits,
    "Days visited" = row$unique_days,
    "Total time" = str_c(round(row$total_duration_hours, 1), " hours")
  )
  
  if ("n_points" %in% names(row)) {
    visit_items[["GPS points"]] <- row$n_points
  }
  
  return(create_popup_section("Visit Pattern", visit_items))
}

#' Create timeline section
create_timeline_section <- function(row) {
  timeline_items <- list(
    "First visit" = format(as.POSIXct(row$first_visit), "%m/%d/%Y %H:%M"),
    "Last visit" = format(as.POSIXct(row$last_visit), "%m/%d/%Y %H:%M")
  )
  
  return(create_popup_section("Timeline", timeline_items))
}

#' Create geocoding information section
create_geocoding_section <- function(row) {
  geocoding_items <- list()
  
  if ("geocoding_method" %in% names(row) && !is.na(row$geocoding_method)) {
    geocoding_items[["Method"]] <- row$geocoding_method
  }
  
  if ("geocoding_confidence" %in% names(row) && !is.na(row$geocoding_confidence)) {
    conf_pct <- round(as.numeric(row$geocoding_confidence) * 100)
    geocoding_items[["Confidence"]] <- str_c(conf_pct, "%")
  }
  
  if ("place_type" %in% names(row) && !is.na(row$place_type)) {
    geocoding_items[["Type"]] <- row$place_type
  }
  
  if (length(geocoding_items) == 0) return("")
  
  return(create_popup_section("Geocoding Info", geocoding_items))
}

#' Create coordinates section
create_coordinates_section <- function(row) {
  coords <- str_c(round(row$lat, 4), ", ", round(row$lon, 4))
  return(create_popup_section("Coordinates", coords))
}

#' Main function to create complete cluster popup
create_cluster_popup <- function(row) {
  # Header
  header <- str_c(
    "<strong>Location Cluster ", row$cluster, "</strong><br>",
    "<em>", row$location_type, " location</em><br><br>"
  )
  
  # Build all sections
  address <- create_address_section(row)
  visits <- create_visit_pattern_section(row)
  timeline <- create_timeline_section(row)
  geocoding <- create_geocoding_section(row)
  coords <- create_coordinates_section(row)
  
  return(str_c(header, address, visits, timeline, geocoding, coords))
}

#' Create popup for raw GPS points
create_gps_popup <- function(row, date_str) {
  popup_items <- list(
    "Date" = date_str,
    "Time" = row$time_str,
    "State" = row$movement_state
  )
  
  if ("speed" %in% names(row) && !is.na(row$speed)) {
    popup_items[["Speed"]] <- str_c(round(row$speed, 1), " mph")
  }
  
  if ("duration" %in% names(row) && !is.na(row$duration)) {
    popup_items[["Duration"]] <- str_c(round(row$duration, 1), " min")
  }
  
  popup_items[["Location"]] <- str_c(round(row$lat, 5), ", ", round(row$lon, 5))
  
  content <- str_c("<strong>GPS Point</strong><br>")
  for (name in names(popup_items)) {
    content <- str_c(content, name, ": ", popup_items[[name]], "<br>")
  }
  
  return(content)
}