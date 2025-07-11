# GPS Data Mapping Functions
# Provides both overview and detailed individual participant views

library(leaflet)
library(dplyr, exclude = c("filter", "lag"))
library(lubridate)

map_gps_overview <- function(gps_data, show_paths = FALSE) {
  
  participants <- unique(gps_data$subid)
  colors <- c("#FF1744", "#00E676", "#2979FF", "#FFEA00", 
              "#FF6D00", "#D500F9", "#00E5FF", "#76FF03")
  
  # improve date readability for buttons
  gps_data <- gps_data %>%
    mutate(date = as.Date(dttm_obs)) %>%
    arrange(subid, dttm_obs)
  
  # create the base map, centered on the lat/lon data
  map <- leaflet() %>%
    addTiles() %>%
    setView(lng = mean(gps_data$lon), lat = mean(gps_data$lat), zoom = 11)
  
  # add points and paths for each participant
  for (i in seq_along(participants)) {
    participant <- participants[i]
    participant_data <- gps_data %>% filter(subid == participant)
    color <- colors[((i - 1) %% length(colors)) + 1]
    
    # add circle markers for the data points
    map <- map %>%
      addCircleMarkers(
        data = participant_data,
        lng = ~lon, 
        lat = ~lat,
        color = color,
        radius = 4,
        fillOpacity = 0.7,
        stroke = TRUE,
        weight = 1,
        popup = ~paste0("<strong>Participant ", subid, "</strong><br>",
                        "Date: ", format(date, "%Y-%m-%d"), "<br>",
                        "Time: ", format(dttm_obs, "%H:%M:%S"), "<br>",
                        "Coordinates: ", round(lat, 4), ", ", round(lon, 4)),
        group = paste("Participant", participant)
      )
    
    # add movement paths if requested
    if (show_paths && nrow(participant_data) > 1) {
      # group by date and add paths for each day
      dates <- unique(participant_data$date)
      for (date in dates) {
        day_data <- participant_data %>% filter(date == !!date)
        if (nrow(day_data) > 1) {
          map <- map %>%
            addPolylines(
              data = day_data,
              lng = ~lon,
              lat = ~lat,
              color = color,
              weight = 2,
              opacity = 0.4,
              group = paste("Participant", participant)
            )
        }
      }
    }
  }
  
  # add layer controls
  if (length(participants) > 1) {
    map <- map %>%
      addLayersControl(
        overlayGroups = paste("Participant", participants),
        options = layersControlOptions(collapsed = FALSE)
      )
  }
  
  # add summary info box for clarity
  map <- map %>%
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


map_gps_individual <- function(gps_data, participant_id, show_all_days = FALSE) {
  
  # filter for specific participant
  participant_data <- gps_data %>%
    filter(subid == participant_id) %>%
    mutate(date = as.Date(dttm_obs)) %>%
    arrange(dttm_obs)
  
  if (nrow(participant_data) == 0) {
    stop(paste("No data found for participant", participant_id))
  }
  
  # get unique dates
  unique_dates <- sort(unique(participant_data$date))
  
  # same color pallete for days
  colors <- c("#FF1744", "#00E676", "#2979FF", "#FFEA00", 
              "#FF6D00", "#D500F9", "#00E5FF", "#76FF03")
  
  # create base map
  map <- leaflet() %>%
    addTiles() %>%
    setView(lng = mean(participant_data$lon), lat = mean(participant_data$lat), zoom = 13)
  
  # add each day as a separate layer
  for (i in seq_along(unique_dates)) {
    date <- unique_dates[i]
    day_data <- participant_data %>% filter(date == !!date)
    
    # color based on day
    day_color <- colors[((i - 1) %% length(colors)) + 1]
    date_str <- format(date, "%Y-%m-%d (%A)")  # Include day of week
    
    # enhanced popup with more details
    popup_text <- ~paste0("<strong>", date_str, "</strong><br>",
                          "Time: ", format(dttm_obs, "%H:%M:%S"), "<br>",
                          if("movement_state" %in% names(day_data)) 
                            paste0("Movement: ", movement_state, "<br>") else "",
                          if("speed" %in% names(day_data)) 
                            paste0("Speed: ", round(speed, 2), " m/s<br>") else "",
                          "Coordinates: ", round(lat, 4), ", ", round(lon, 4))
    
    map <- map %>%
      addCircleMarkers(
        data = day_data,
        lng = ~lon, 
        lat = ~lat,
        color = day_color,
        radius = 5,
        fillOpacity = 0.8,
        stroke = TRUE,
        weight = 1,
        popup = popup_text,
        group = if(show_all_days) "All Days" else date_str
      )
    
    # add lines to show movement path for the day
    if (nrow(day_data) > 1) {
      map <- map %>%
        addPolylines(
          data = day_data,
          lng = ~lon,
          lat = ~lat,
          color = day_color,
          weight = 3,
          opacity = 0.6,
          group = if(show_all_days) "All Days" else date_str
        )
    }
  }
  
  # add layer control unless showing all days
  if (!show_all_days && length(unique_dates) > 1) {
    map <- map %>%
      addLayersControl(
        baseGroups = format(unique_dates, "%Y-%m-%d (%A)"),
        options = layersControlOptions(collapsed = FALSE)
      )
  }
  
  # add participant info box again
  map <- map %>%
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

map_gps <- function(gps_data, participant_id = NULL, show_paths = FALSE, show_all_days = FALSE) {
  
  if (is.null(participant_id)) {
    # Overview mode - all participants
    return(map_gps_overview(gps_data, show_paths = show_paths))
  } else {
    # Individual mode - specific participant
    return(map_gps_individual(gps_data, participant_id, show_all_days = show_all_days))
  }
}

# Usage:
# # 1. Overview map showing all participants
# map_gps(gps_data)

# # 2. Overview map with movement paths
# map_gps(gps_data, show_paths = TRUE)

# # 3. Individual participant with day-by-day control
# map_gps(gps_data, participant_id = 19)

# # 4. Individual participant showing all days at once
# map_gps(gps_data, participant_id = 19, show_all_days = TRUE)
