# gps_splice <- read_csv(here::here(path_processed, "gps2_stationary.csv"), show_col_types = FALSE)

# GPS Data Viewer

library(leaflet)
library(dplyr)

map_gps_points <- function(gps_data) {
  
  participants <- unique(gps_data$subid)
  colors <- c("black", "red", "darkblue", "darkgreen", "purple", "orange", "brown", "darkgray")
  
  # create base map centered on data
  map <- leaflet() %>%
    addTiles() %>%
    setView(lng = mean(gps_data$lon), lat = mean(gps_data$lat), zoom = 11)
  
  # add points for each participant
  for (i in seq_along(participants)) {
    participant <- participants[i]
    participant_data <- gps_data %>% filter(subid == participant)
    
    map <- map %>%
      addCircleMarkers(
        data = participant_data,
        lng = ~lon, 
        lat = ~lat,
        color = colors[i],
        radius = 3,
        fillOpacity = 0.7,
        stroke = FALSE,
        popup = ~paste("Participant:", subid, "<br>Time:", dttm_obs),
        group = paste("Participant", participant)
      )
  }
  
  # add layer control if multiple participants
  if (length(participants) > 1) {
    map <- map %>%
      addLayersControl(
        overlayGroups = paste("Participant", participants),
        options = layersControlOptions(collapsed = FALSE)
      )
  }
  
  return(map)
}

# Usage:
# map <- map_gps_points(gps_splice)
# map