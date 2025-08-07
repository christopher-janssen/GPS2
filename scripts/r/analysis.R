# scripts/r/analysis.R
# GPS clustering and geocoding analysis functions
# Consolidates duration_cluster.R and nominatim_geocoding.R

library(dplyr)
library(geosphere)
library(lubridate)
library(httr)
library(jsonlite, exclude = "flatten")

source("scripts/r/database.R")

# ==============================================================================
# CLUSTERING ANALYSIS
# ==============================================================================

# Duration-based clustering algorithm for GPS data
cluster_stationary_gps <- function(gps_data, participant_id, eps = 20) {
  
  # eps represents radius in meters, min duration threshold for meaningful stops
  radius_m <- eps
  min_duration_min <- 30  # Minimum duration for a meaningful stop within a day
  
  # Filter for specific participant and stationary points only
  participant_data <- gps_data |>
    filter(subid == participant_id, movement_state == "stationary") |>
    mutate(date = as.Date(dttm_obs)) |>
    arrange(dttm_obs)
  
  if (nrow(participant_data) == 0) {
    return(data.frame(
      subid = numeric(0), lat = numeric(0), lon = numeric(0), 
      n_points = integer(0), first_visit = as.POSIXct(character(0)), 
      last_visit = as.POSIXct(character(0)), cluster = integer(0),
      total_visits = integer(0), total_duration_hours = numeric(0), 
      unique_days = integer(0)
    ))
  }
  
  # Process each day separately to account for temporal patterns
  daily_clusters <- participant_data |>
    group_by(date) |>
    group_modify(~ {
      day_data <- .x
      day_data$daily_cluster <- 0
      cluster_id <- 1
      
      # Cluster within this day based on proximity and duration
      for (i in 1:nrow(day_data)) {
        if (day_data$daily_cluster[i] != 0) next
        
        current_point <- day_data[i, ]
        remaining_points <- day_data[(i):nrow(day_data), ] |>
          filter(daily_cluster == 0)
        
        if (nrow(remaining_points) == 0) next
        
        # Calculate Haversine distances to remaining points
        distances <- distHaversine(
          p1 = c(current_point$lon, current_point$lat),
          p2 = cbind(remaining_points$lon, remaining_points$lat)
        )
        
        nearby_indices <- which(distances <= radius_m)
        nearby_points <- remaining_points[nearby_indices, ]
        
        # Check if duration threshold is met within this day
        if (nrow(nearby_points) >= 2) {
          time_span_min <- as.numeric(difftime(
            max(nearby_points$dttm_obs), 
            min(nearby_points$dttm_obs), 
            units = "mins"
          ))
          
          if (time_span_min >= min_duration_min) {
            original_indices <- which(day_data$dttm_obs %in% nearby_points$dttm_obs &
                                        day_data$daily_cluster == 0)
            day_data$daily_cluster[original_indices] <- cluster_id
            cluster_id <- cluster_id + 1
          }
        }
      }
      
      return(day_data |> filter(daily_cluster != 0))
    }) |>
    ungroup()
  
  if (nrow(daily_clusters) == 0) {
    return(data.frame(
      subid = numeric(0), lat = numeric(0), lon = numeric(0), 
      n_points = integer(0), first_visit = as.POSIXct(character(0)), 
      last_visit = as.POSIXct(character(0)), cluster = integer(0),
      total_visits = integer(0), total_duration_hours = numeric(0), 
      unique_days = integer(0)
    ))
  }
  
  # Aggregate daily locations across days based on geographic proximity
  daily_locations <- daily_clusters |>
    group_by(date, daily_cluster) |>
    summarise(
      subid = first(subid),
      lat = mean(lat),
      lon = mean(lon),
      n_points = n(),
      start_time = min(dttm_obs),
      end_time = max(dttm_obs),
      duration_min = as.numeric(difftime(max(dttm_obs), min(dttm_obs), units = "mins")),
      .groups = "drop"
    )
  
  # Final aggregation creates location representatives across all days
  daily_locations$final_cluster <- 0
  cluster_id <- 1
  
  for (i in 1:nrow(daily_locations)) {
    if (daily_locations$final_cluster[i] != 0) next
    
    current_location <- daily_locations[i, ]
    
    # Find all locations (across all days) within radius
    distances_all <- distHaversine(
      p1 = c(current_location$lon, current_location$lat),
      p2 = cbind(daily_locations$lon, daily_locations$lat)
    )
    
    nearby_location_indices <- which(distances_all <= radius_m & daily_locations$final_cluster == 0)
    daily_locations$final_cluster[nearby_location_indices] <- cluster_id
    cluster_id <- cluster_id + 1
  }
  
  # Create final cluster representatives
  representatives <- daily_locations |>
    filter(final_cluster != 0) |>
    group_by(final_cluster) |>
    summarise(
      subid = first(subid),
      lat = mean(lat),
      lon = mean(lon),
      n_points = sum(n_points),
      first_visit = min(start_time),
      last_visit = max(end_time),
      total_visits = n(),
      total_duration_hours = sum(duration_min) / 60,
      unique_days = n_distinct(date),
      .groups = "drop"
    ) |>
    rename(cluster = final_cluster)
  
  return(representatives)
}

# Run clustering analysis for all participants
analyze_all_participants <- function(eps = 50, participant_ids = NULL) {
  cat("Starting clustering analysis...\n")
  
  # Get participants to analyze
  if (is.null(participant_ids)) {
    participants <- query_gps2_db("SELECT DISTINCT subid FROM gps2.gps_stationary_points WHERE subid != 999 ORDER BY subid;")$subid
  } else {
    participants <- participant_ids
  }
  
  cat("Analyzing", length(participants), "participants\n")
  
  all_clusters <- data.frame()
  
  for (participant_id in participants) {
    tryCatch({
      # Get participant GPS data
      gps_data <- query_gps2_db("
        SELECT subid, lat, lon, dttm_obs, dist, duration, speed, movement_state
        FROM gps2.gps_stationary_points 
        WHERE subid = $1 AND movement_state = 'stationary'
        ORDER BY dttm_obs;
      ", list(participant_id))
      
      if (nrow(gps_data) > 0) {
        clusters <- cluster_stationary_gps(gps_data, participant_id, eps)
        if (nrow(clusters) > 0) {
          all_clusters <- bind_rows(all_clusters, clusters)
          cat("Participant", participant_id, ":", nrow(clusters), "clusters\n")
        }
      }
      
    }, error = function(e) {
      cat("Error clustering participant", participant_id, ":", e$message, "\n")
    })
  }
  
  cat("✅ Clustering complete:", nrow(all_clusters), "total clusters\n")
  return(all_clusters)
}

# ==============================================================================
# GEOCODING ANALYSIS
# ==============================================================================

# Test Nominatim service availability
test_nominatim_connection <- function(nominatim_url = "http://localhost:8080") {
  cat("Testing Nominatim geocoding service...\n")
  
  tryCatch({
    # Verify service availability
    status_response <- GET(paste0(nominatim_url, "/status.php"))
    
    if (status_code(status_response) != 200) {
      stop("Nominatim service not accessible at ", nominatim_url)
    }
    
    status_content <- content(status_response, "parsed")
    cat("Nominatim service operational\n")
    cat("Database status:", status_content$status, "\n")
    
    # Test reverse geocoding with Madison coordinates
    test_url <- paste0(nominatim_url, "/reverse?format=json&lat=43.074713&lon=-89.384373&addressdetails=1")
    test_response <- GET(test_url)
    
    if (status_code(test_response) == 200) {
      test_result <- content(test_response, "parsed")
      cat("Reverse geocoding test successful\n")
      cat("Sample address:", test_result$display_name, "\n")
      return(TRUE)
    } else {
      cat("Reverse geocoding test failed\n")
      return(FALSE)
    }
    
  }, error = function(e) {
    cat("Nominatim connection error:", e$message, "\n")
    cat("Verify container running: docker-compose ps\n")
    return(FALSE)
  })
}

# Execute reverse geocoding for location clusters
reverse_geocode_clusters <- function(participant_ids = NULL, update_existing = FALSE, 
                                     nominatim_url = "http://localhost:8080",
                                     batch_size = 10, delay_seconds = 0.1) {
  
  cat("Starting reverse geocoding using local Nominatim service...\n")
  
  # Verify Nominatim service availability
  if (!test_nominatim_connection(nominatim_url)) {
    stop("Nominatim service unavailable. Start with: docker-compose up -d")
  }
  
  # Build query for clusters needing geocoding
  participant_filter <- ""
  if (!is.null(participant_ids)) {
    participant_list <- paste(participant_ids, collapse = ",")
    participant_filter <- paste0("WHERE lc.subid IN (", participant_list, ")")
  }
  
  existing_filter <- ""
  if (!update_existing) {
    existing_filter <- ifelse(participant_filter == "", "WHERE", "AND")
    existing_filter <- paste0(existing_filter, " NOT EXISTS (
      SELECT 1 FROM gps2.cluster_geocoding cg 
      WHERE cg.subid = lc.subid AND cg.cluster_id = lc.cluster_id
    )")
  }
  
  clusters_query <- paste0("
    SELECT 
      lc.cluster_id, lc.subid, lc.lat, lc.lon,
      lc.total_visits, lc.unique_days, lc.total_duration_hours
    FROM gps2.location_clusters lc
    ", participant_filter, " ", existing_filter, "
    ORDER BY lc.total_visits DESC, lc.subid, lc.cluster_id;
  ")
  
  clusters_to_geocode <- query_gps2_db(clusters_query)
  
  if (nrow(clusters_to_geocode) == 0) {
    cat("No clusters requiring geocoding\n")
    return(invisible(NULL))
  }
  
  cat("Processing", nrow(clusters_to_geocode), "clusters for reverse geocoding\n")
  
  # Execute geocoding with batch processing and rate limiting
  successful_geocodes <- 0
  failed_geocodes <- 0
  
  con <- connect_gps2_db()
  tryCatch({
    for (i in 1:nrow(clusters_to_geocode)) {
      cluster <- clusters_to_geocode[i, ]
      
      tryCatch({
        # Construct Nominatim reverse geocoding request
        geocode_url <- paste0(
          nominatim_url, 
          "/reverse?format=json&lat=", cluster$lat,
          "&lon=", cluster$lon,
          "&addressdetails=1&extratags=1"
        )
        
        # Execute request
        response <- GET(geocode_url)
        
        if (status_code(response) == 200) {
          result <- content(response, "parsed")
          address <- result$address
          
          if (!is.null(result$display_name) && result$display_name != "") {
            
            # Insert geocoding result
            insert_sql <- "
            INSERT INTO gps2.cluster_geocoding 
              (cluster_id, subid, display_name, house_number, road, neighbourhood, 
               city, county, state, postcode, country, place_type, osm_type, osm_id,
               geocoding_confidence, geocoding_method)
            VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15, $16)
            ON CONFLICT (subid, cluster_id) 
            DO UPDATE SET
              display_name = EXCLUDED.display_name,
              processed_at = CURRENT_TIMESTAMP;
            "
            
            dbExecute(con, insert_sql, list(
              cluster$cluster_id, cluster$subid, result$display_name,
              if(is.null(address$house_number)) NA else address$house_number,
              if(is.null(address$road)) NA else address$road,
              if(is.null(address$neighbourhood)) {
                if(is.null(address$suburb)) NA else address$suburb
              } else address$neighbourhood,
              if(is.null(address$city)) {
                if(is.null(address$town)) {
                  if(is.null(address$village)) NA else address$village
                } else address$town
              } else address$city,
              if(is.null(address$county)) NA else address$county,
              if(is.null(address$state)) NA else address$state,
              if(is.null(address$postcode)) NA else address$postcode,
              if(is.null(address$country)) NA else address$country,
              if(is.null(result$type)) {
                if(is.null(result$class)) NA else result$class
              } else result$type,
              if(is.null(result$osm_type)) NA else result$osm_type,
              if(is.null(result$osm_id)) NA else as.numeric(result$osm_id),
              if(is.null(result$importance)) 0.5 else as.numeric(result$importance),
              "NOMINATIM"
            ))
            
            successful_geocodes <- successful_geocodes + 1
            
          } else {
            failed_geocodes <- failed_geocodes + 1
          }
          
        } else {
          failed_geocodes <- failed_geocodes + 1
        }
        
        # Progress reporting
        if (i %% batch_size == 0 || i == nrow(clusters_to_geocode)) {
          cat("Processed", i, "of", nrow(clusters_to_geocode), 
              "- Success:", successful_geocodes, "Failed:", failed_geocodes, "\n")
        }
        
        # Rate limiting
        if (i < nrow(clusters_to_geocode)) {
          Sys.sleep(delay_seconds)
        }
        
      }, error = function(e) {
        cat("Error geocoding cluster", cluster$cluster_id, ":", e$message, "\n")
        failed_geocodes <- failed_geocodes + 1
      })
    }
    
  }, finally = {
    disconnect_gps2_db(con)
  })
  
  cat("✅ Geocoding complete - Success:", successful_geocodes, "Failed:", failed_geocodes, "\n")
}

# Get geocoded cluster results
get_geocoded_clusters <- function(participant_ids = NULL, include_failed = FALSE) {
  
  participant_filter <- ""
  if (!is.null(participant_ids)) {
    participant_list <- paste(participant_ids, collapse = ",")
    participant_filter <- paste0("AND cg.subid IN (", participant_list, ")")
  }
  
  failed_filter <- ""
  if (!include_failed) {
    failed_filter <- "AND cg.display_name IS NOT NULL AND cg.display_name != 'No address found'"
  }
  
  query <- paste0("
    SELECT 
      cg.subid, cg.cluster_id, lc.lat, lc.lon,
      cg.display_name, cg.road, cg.city, cg.state, cg.postcode,
      cg.geocoding_confidence, cg.geocoding_method,
      lc.total_visits, lc.unique_days, lc.total_duration_hours,
      lc.first_visit, lc.last_visit
    FROM gps2.cluster_geocoding cg
    JOIN gps2.location_clusters lc ON cg.subid = lc.subid AND cg.cluster_id = lc.cluster_id
    WHERE 1=1 ", participant_filter, " ", failed_filter, "
    ORDER BY cg.subid, lc.total_duration_hours DESC, lc.total_visits DESC;
  ")
  
  return(query_gps2_db(query))
}

# Analyze geocoding success rates
analyze_geocoding_coverage <- function() {
  
  cat("Geocoding Coverage Analysis\n")
  cat("===========================\n")
  
  # Overall success metrics
  overall_stats <- query_gps2_db("
    SELECT 
      COUNT(DISTINCT lc.subid || '_' || lc.cluster_id) as total_clusters,
      COUNT(DISTINCT CASE WHEN cg.display_name IS NOT NULL AND cg.display_name != 'No address found' 
                         THEN cg.subid || '_' || cg.cluster_id END) as geocoded_clusters,
      ROUND(
        COUNT(DISTINCT CASE WHEN cg.display_name IS NOT NULL AND cg.display_name != 'No address found' 
                           THEN cg.subid || '_' || cg.cluster_id END) * 100.0 /
        COUNT(DISTINCT lc.subid || '_' || lc.cluster_id), 1
      ) as success_rate_percent
    FROM gps2.location_clusters lc
    LEFT JOIN gps2.cluster_geocoding cg ON lc.subid = cg.subid AND lc.cluster_id = cg.cluster_id;
  ")
  
  cat("Total clusters:", overall_stats$total_clusters, "\n")
  cat("Successfully geocoded:", overall_stats$geocoded_clusters, "\n")
  cat("Success rate:", overall_stats$success_rate_percent, "%\n\n")
  
  # Participant-level analysis
  participant_stats <- query_gps2_db("
    SELECT 
      lc.subid,
      COUNT(lc.cluster_id) as total_clusters,
      COUNT(CASE WHEN cg.display_name IS NOT NULL AND cg.display_name != 'No address found' 
                 THEN 1 END) as geocoded_clusters,
      ROUND(
        COUNT(CASE WHEN cg.display_name IS NOT NULL AND cg.display_name != 'No address found' 
                   THEN 1 END) * 100.0 / COUNT(lc.cluster_id), 1
      ) as success_rate_percent
    FROM gps2.location_clusters lc
    LEFT JOIN gps2.cluster_geocoding cg ON lc.subid = cg.subid AND lc.cluster_id = cg.cluster_id
    GROUP BY lc.subid
    ORDER BY success_rate_percent DESC, total_clusters DESC
    LIMIT 10;
  ")
  
  cat("Success rate by participant (top 10):\n")
  print(participant_stats)
  
  return(list(overall = overall_stats, by_participant = participant_stats))
}