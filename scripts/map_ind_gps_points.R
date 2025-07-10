# GPS Data Viewer - Individual Participants by Day

library(leaflet)
library(dplyr)
library(lubridate)

# separate maps for each participant
map_by_participant <- function(gps_data, participant_id) {
  
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
  
  # color palette for days
  colors <- c("red", "blue", "green", "purple", "orange", "brown", "black", "pink")
  
  # create base map
  map <- leaflet() %>%
    addTiles() %>%
    setView(lng = mean(participant_data$lon), lat = mean(participant_data$lat), zoom = 12)
  
  # add each day as a separate layer
  for (i in seq_along(unique_dates)) {
    date <- unique_dates[i]
    day_data <- participant_data %>% filter(date == !!date)
    
    # color based on day
    day_color <- colors[((i - 1) %% length(colors)) + 1]
    date_str <- format(date, "%Y-%m-%d (%A)")  # Include day of week
    
    map <- map %>%
      addCircleMarkers(
        data = day_data,
        lng = ~lon, 
        lat = ~lat,
        color = day_color,
        radius = 4,
        fillOpacity = 0.8,
        stroke = TRUE,
        weight = 1,
        popup = ~paste("Time:", format(dttm_obs, "%H:%M:%S"),
                       "<br>Movement:", movement_state,
                       "<br>Speed:", round(speed, 2), "m/s"),
        group = date_str
      )
    
    # add polylines to show movement path for the day
    if (nrow(day_data) > 1) {
      map <- map %>%
        addPolylines(
          data = day_data,
          lng = ~lon,
          lat = ~lat,
          color = day_color,
          weight = 2,
          opacity = 0.5,
          group = date_str
        )
    }
  }
  
  # layer control
  map <- map %>%
    addLayersControl(
      baseGroups = format(unique_dates, "%Y-%m-%d (%A)"),
      options = layersControlOptions(collapsed = FALSE)
    ) %>%
    addControl(
      html = paste0("<div style='background: white; padding: 10px; border-radius: 5px;'>",
                    "<strong>Participant ", participant_id, "</strong><br>",
                    "Total days: ", length(unique_dates), "<br>",
                    "Total points: ", nrow(participant_data), "</div>"),
      position = "topright"
    )
  
  return(map)
}

# Usage examples:

# View a specific participant across all their days
# map <- map_by_participant(gps_splice, participant_id = 19)
# map