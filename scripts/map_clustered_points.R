# map_clustered_points.R

library(leaflet)
library(dplyr)

map_clustered_gps <- function(gps_data) {
  
  participants <- unique(gps_data$subid)
  participant_colors <- c("black", "red", "darkblue", "darkgreen", "purple", "orange", "brown", "darkgray")
  cluster_colors <- c("red", "blue", "green", "purple", "orange", "cyan", "magenta", "yellow")
  
  # create base map
  map <- leaflet() %>%
    addTiles() %>%
    setView(lng = mean(gps_data$lon), lat = mean(gps_data$lat), zoom = 11)
  
  # add raw GPS points by participant
  for (i in seq_along(participants)) {
    participant <- participants[i]
    participant_data <- gps_data %>% filter(subid == participant)
    
    map <- map %>%
      addCircleMarkers(
        data = participant_data,
        lng = ~lon, 
        lat = ~lat,
        color = participant_colors[i],
        radius = 3,
        fillOpacity = 0.7,
        stroke = FALSE,
        popup = ~paste("Participant:", subid, "<br>Time:", dttm_obs),
        group = paste("Raw - Participant", participant)
      )
  }
  
  # add clustered points by participant
  for (i in seq_along(participants)) {
    participant <- participants[i]
    participant_data <- gps_data %>% filter(subid == participant)
    
    map <- map %>%
      addCircleMarkers(
        data = participant_data,
        lng = ~lon, 
        lat = ~lat,
        color = cluster_colors[participant_data$cluster],
        radius = 4,
        fillOpacity = 0.8,
        stroke = TRUE,
        weight = 1,
        popup = ~paste("Participant:", subid, "<br>Cluster:", cluster, "<br>Time:", dttm_obs),
        group = paste("Clustered - Participant", participant)
      )
  }
  
  # create layer control
  raw_groups <- paste("Raw - Participant", participants)
  cluster_groups <- paste("Clustered - Participant", participants)
  
  map <- map %>%
    addLayersControl(
      overlayGroups = c(raw_groups, cluster_groups),
      options = layersControlOptions(collapsed = FALSE)
    ) %>%
    hideGroup(cluster_groups)
  
  return(map)
}