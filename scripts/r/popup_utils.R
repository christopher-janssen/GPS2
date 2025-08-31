# GPS22 popup utilities

create_gps_popup <- function(row, date_str) {
  popup_items <- list(
    "Date" = date_str,
    "Time" = row$time_str,
    "State" = row$movement_state
  )
  
  if ("speed_mph" %in% names(row) && !is.na(row$speed_mph)) {
    popup_items[["Speed"]] <- str_c(round(row$speed_mph, 1), " mph")
  }
  
  if ("dist_m" %in% names(row) && !is.na(row$dist_m)) {
    popup_items[["Distance"]] <- str_c(round(row$dist_m, 1), " m")
  }
  
  popup_items[["Location"]] <- str_c(round(row$lat, 5), ", ", round(row$lon, 5))
  
  content <- str_c("<strong>GPS Point</strong><br>")
  for (name in names(popup_items)) {
    content <- str_c(content, name, ": ", popup_items[[name]], "<br>")
  }
  
  content
}

create_cluster_popup <- function(row, thresholds = NULL) {
  # Header with enhanced category information
  category_info <- if (!is.null(thresholds) && !is.na(row$location_type)) {
    get_category_description(row, thresholds)
  } else {
    row$location_type
  }
  
  header <- str_c(
    "<strong>Location Cluster ", row$cluster_id, "</strong><br>",
    "<em>", category_info, "</em><br><br>"
  )
  
  # Address section
  if (!is.na(row$display_name) && row$display_name != "") {
    address <- str_c("<strong>Address:</strong><br>• ", row$display_name, "<br><br>")
  } else if (!is.na(row$city) && row$city != "") {
    address <- str_c("<strong>City:</strong><br>• ", row$city, "<br><br>")
  } else {
    address <- str_c("<strong>Location:</strong><br>• Cluster ", row$cluster_id, "<br><br>")
  }
  
  # Visit pattern
  visits <- str_c(
    "<strong>Visit Pattern:</strong><br>",
    "• Total visits: ", row$total_visits, "<br>",
    "• Total time: ", round(row$total_duration_hours, 1), " hours<br>"
  )
  
  if ("n_points" %in% names(row)) {
    visits <- str_c(visits, "• GPS points: ", row$n_points, "<br>")
  }
  visits <- str_c(visits, "<br>")
  
  # Timeline
  timeline <- str_c(
    "<strong>Timeline:</strong><br>",
    "• First visit: ", format(as.POSIXct(row$first_visit), "%m/%d/%Y %H:%M"), "<br>",
    "• Last visit: ", format(as.POSIXct(row$last_visit), "%m/%d/%Y %H:%M"), "<br><br>"
  )
  
  # Coordinates
  coords <- str_c(
    "<strong>Coordinates:</strong><br>",
    round(row$lat, 4), ", ", round(row$lon, 4)
  )
  
  str_c(header, address, visits, timeline, coords)
}

#' Generate category description with threshold context
get_category_description <- function(row, thresholds) {
  location_type <- row$location_type
  
  # For visit-based categories
  if (location_type %in% c("Routine", "Frequent", "Occasional", "Rare") && "visits" %in% names(thresholds)) {
    visits <- row$total_visits
    breaks <- thresholds$visits
    
    description <- switch(location_type,
      "Routine" = str_c("Routine (≥", breaks[4], " visits)"),
      "Frequent" = str_c("Frequent (", breaks[3], "-", breaks[4]-1, " visits)"),
      "Occasional" = str_c("Occasional (", breaks[2], "-", breaks[3]-1, " visits)"),
      "Rare" = str_c("Rare (", breaks[1], "-", breaks[2]-1, " visits)"),
      location_type
    )
    
    str_c(description, " - This location has ", visits, " visits")
  }
  # For duration-based categories  
  else if (location_type %in% c("Long Stays", "Medium Stays", "Short Stays", "Brief Stops") && "duration" %in% names(thresholds)) {
    duration <- round(row$total_duration_hours, 1)
    breaks <- thresholds$duration
    
    description <- switch(location_type,
      "Long Stays" = str_c("Long Stays (≥", round(breaks[4], 1), " hours)"),
      "Medium Stays" = str_c("Medium Stays (", round(breaks[3], 1), "-", round(breaks[4], 1), " hours)"),
      "Short Stays" = str_c("Short Stays (", round(breaks[2], 1), "-", round(breaks[3], 1), " hours)"),
      "Brief Stops" = str_c("Brief Stops (", round(breaks[1], 1), "-", round(breaks[2], 1), " hours)"),
      location_type
    )
    
    str_c(description, " - This location has ", duration, " hours total")
  }
  else {
    location_type
  }
}

#' Create info box for GPS points visualization
create_gps_info_box <- function(data, subid) {
  unique_dates <- unique(as.Date(data$time))
  
  str_c(
    "<div style='background: rgba(255,255,255,0.95); padding: 12px; border-radius: 8px; border: 2px solid #333; font-family: Arial; margin-bottom: 10px;'>",
    "<strong>Participant ", subid, " - Raw GPS</strong><br>",
    "<strong>", nrow(data), "</strong> GPS points<br>",
    "<strong>", length(unique_dates), "</strong> days<br>",
    "From ", min(as.Date(data$time)), " to ", max(as.Date(data$time)),
    "</div>"
  )
}

#' Create info box for cluster visualization
create_cluster_info_box <- function(data, subid) {
  start_date <- min(as.Date(data$first_visit))
  end_date <- max(as.Date(data$last_visit))
  with_addresses <- sum(!is.na(data$display_name) & data$display_name != "")
  
  paste0(
    "<div style='background: rgba(255,255,255,0.95); padding: 12px; ",
    "border-radius: 8px; border: 2px solid #333; font-family: Arial; margin-bottom: 10px;'>",
    "<strong>Participant ", subid, " - Location Clusters</strong><br>",
    "<strong>", nrow(data), "</strong> meaningful locations<br>",
    "<strong>", sum(data$total_visits), "</strong> total visits<br>",
    "<strong>", round(sum(data$total_duration_hours), 1), "</strong> hours tracked<br>",
    "<strong>Timeframe:</strong> [", format(start_date, "%m-%d-%Y"), " to ", 
    format(end_date, "%m-%d-%Y"), "]<br><br>",
    "<strong>Geocoding Results:</strong><br>",
    "Complete addresses: ", with_addresses, " (", 
    round(with_addresses / nrow(data) * 100, 1), "%)<br>",
    "<em>Marker size = visit importance</em>",
    "</div>"
  )
}