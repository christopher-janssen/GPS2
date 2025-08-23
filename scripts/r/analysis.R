# scripts/r/analysis.R
# GPS clustering and geocoding analysis functions
# refactored to use utility functions

library(dplyr)
library(geosphere)
library(lubridate)
library(httr)
library(jsonlite, exclude = "flatten")

source("scripts/r/global_setup.R")
source("scripts/r/database.R")

# ==============================================================================
# CLUSTERING ANALYSIS - ENVIRONMENT FUNCTIONS
# ==============================================================================

#' Duration-based clustering algorithm for GPS data (environment version)
#' 
#' @param gps_data GPS data frame already loaded in R
#' @param subid Participant ID to analyze
#' @param eps Radius in meters for clustering (default from config)
#' 
#' @return Data frame with cluster representatives

cluster_stationary_gps_env <- function(gps_data, subid, eps = NULL) {
  
  # Use config defaults if not specified
  if (is.null(eps)) eps <- get_config("clustering", "default_eps")
  min_duration_min <- get_config("clustering", "min_duration_min")
  
  # Validation
  validate_participant_id(subid)
  validate_clustering_params(eps, min_duration_min)
  
  # Get participant data with validation
  participant_data <- validate_participant_exists(gps_data, subid)
  if (nrow(participant_data) == 0) return(create_empty_cluster_result())
  
  # Filter for stationary points only
  participant_data <- participant_data |>
    filter(movement_state == "stationary") |>
    mutate(date = as.Date(dttm_obs)) |>
    arrange(dttm_obs)
  
  if (nrow(participant_data) == 0) return(create_empty_cluster_result())
  
  # Process clustering by day
  daily_clusters <- cluster_by_day(participant_data, eps, min_duration_min)
  if (nrow(daily_clusters) == 0) return(create_empty_cluster_result())
  
  # Aggregate across days
  representatives <- aggregate_daily_clusters(daily_clusters, eps)
  
  return(representatives)
}

#' Helper: Cluster GPS points within each day
cluster_by_day <- function(participant_data, eps, min_duration_min) {
  participant_data |>
    group_by(date) |>
    group_modify(~ {
      day_data <- .x
      day_data$daily_cluster <- assign_daily_clusters(day_data, eps, min_duration_min)
      return(day_data |> filter(daily_cluster != 0))
    }) |>
    ungroup()
}

#' Helper: Assign cluster IDs within a single day
assign_daily_clusters <- function(day_data, eps, min_duration_min) {
  day_data$daily_cluster <- 0
  cluster_id <- 1
  
  for (i in 1:nrow(day_data)) {
    if (day_data$daily_cluster[i] != 0) next
    
    # Find nearby points
    nearby_points <- find_nearby_points(day_data, i, eps)
    
    # Check duration threshold
    if (meets_duration_threshold(nearby_points, min_duration_min)) {
      original_indices <- which(day_data$dttm_obs %in% nearby_points$dttm_obs &
                                  day_data$daily_cluster == 0)
      day_data$daily_cluster[original_indices] <- cluster_id
      cluster_id <- cluster_id + 1
    }
  }
  
  return(day_data$daily_cluster)
}

#' Helper: Find points within radius
find_nearby_points <- function(day_data, current_idx, eps) {
  current_point <- day_data[current_idx, ]
  remaining_points <- day_data[current_idx:nrow(day_data), ] |>
    filter(daily_cluster == 0)
  
  if (nrow(remaining_points) == 0) return(data.frame())
  
  distances <- distHaversine(
    p1 = c(current_point$lon, current_point$lat),
    p2 = cbind(remaining_points$lon, remaining_points$lat)
  )
  
  nearby_indices <- which(distances <= eps)
  return(remaining_points[nearby_indices, ])
}

#' Helper: Check if points meet duration threshold
meets_duration_threshold <- function(nearby_points, min_duration_min) {
  if (nrow(nearby_points) < 2) return(FALSE)
  
  time_span_min <- as.numeric(difftime(
    max(nearby_points$dttm_obs), 
    min(nearby_points$dttm_obs), 
    units = "mins"
  ))
  
  return(time_span_min >= min_duration_min)
}

#' Helper: Aggregate daily locations across all days
aggregate_daily_clusters <- function(daily_clusters, eps) {
  # Create daily location summaries
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
  
  # Cluster across days by proximity
  daily_locations$final_cluster <- assign_final_clusters(daily_locations, eps)
  
  # Create final representatives
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

#' Helper: Assign final cluster IDs across all days
assign_final_clusters <- function(daily_locations, eps) {
  daily_locations$final_cluster <- 0
  cluster_id <- 1
  
  for (i in 1:nrow(daily_locations)) {
    if (daily_locations$final_cluster[i] != 0) next
    
    current_location <- daily_locations[i, ]
    
    # Find all locations within radius
    distances_all <- distHaversine(
      p1 = c(current_location$lon, current_location$lat),
      p2 = cbind(daily_locations$lon, daily_locations$lat)
    )
    
    nearby_indices <- which(distances_all <= eps & daily_locations$final_cluster == 0)
    daily_locations$final_cluster[nearby_indices] <- cluster_id
    cluster_id <- cluster_id + 1
  }
  
  return(daily_locations$final_cluster)
}

# ==============================================================================
# CLUSTERING ANALYSIS - DATABASE FUNCTIONS
# ==============================================================================

#' Duration-based clustering algorithm for GPS data (database version)
#' 
#' @param subid Participant ID to analyze
#' @param eps Radius in meters for clustering (default from config)
#' 
#' @return Data frame with cluster representatives

cluster_stationary_gps_db <- function(subid, eps = NULL) {
  
  # Use config defaults if not specified
  if (is.null(eps)) eps <- get_config("clustering", "default_eps")
  
  # Validation
  validate_participant_id(subid)
  validate_clustering_params(eps)
  
  # Get participant GPS data using query builder
  query <- build_gps_query(participant_ids = subid, movement_state = "stationary")
  gps_data <- query_gps2_db(query)
  
  if (nrow(gps_data) == 0) {
    cat("No GPS data found for participant", subid, "\n")
    return(create_empty_cluster_result())
  }
  
  # Use environment version for actual clustering
  return(cluster_stationary_gps_env(gps_data, subid, eps))
}

#' Run clustering analysis for all participants (database version)
#' 
#' @param eps Radius in meters for clustering (default from config)
#' @param participant_ids Optional list of participant IDs to analyze
#' 
#' @return Data frame with all clusters

cluster_all_participants_db <- function(eps = NULL, participant_ids = NULL) {
  cat("Starting clustering analysis...\n")
  
  # Use config defaults
  if (is.null(eps)) eps <- get_config("clustering", "default_eps")
  
  # Get participants to analyze
  if (is.null(participant_ids)) {
    participants <- query_gps2_db(
      "SELECT DISTINCT subid 
       FROM gps2.gps_stationary_points 
       WHERE subid != 999 
       ORDER BY subid;"
    )$subid
  } else {
    participants <- participant_ids
  }
  
  cat("Analyzing", length(participants), "participants\n")
  
  # Process each participant
  all_clusters <- process_participants_for_clustering(participants, eps)
  
  cat("✅ Clustering complete:", nrow(all_clusters), "total clusters\n")
  return(all_clusters)
}

#' Helper: Process all participants for clustering
process_participants_for_clustering <- function(participants, eps) {
  all_clusters <- data.frame()
  
  for (subid in participants) {
    tryCatch({
      clusters <- cluster_stationary_gps_db(subid, eps)
      if (nrow(clusters) > 0) {
        all_clusters <- bind_rows(all_clusters, clusters)
        cat("Participant", subid, ":", nrow(clusters), "clusters\n")
      }
      
    }, error = function(e) {
      cat("Error clustering participant", subid, ":", e$message, "\n")
    })
  }
  
  return(all_clusters)
}

#' Get geocoded cluster results (database version)
#' 
#' @param participant_ids Optional participant filter
#' @param include_failed Include failed geocoding attempts
#' 
#' @return Data frame with cluster and geocoding info

get_geocoded_clusters_db <- function(participant_ids = NULL, include_failed = FALSE) {
  
  # Build query using utility
  query <- build_cluster_query(
    participant_ids = participant_ids,
    include_geocoding = TRUE,
    include_failed_geocoding = include_failed,
    order_by = "total_duration_hours DESC"
  )
  
  return(query_gps2_db(query))
}

# ==============================================================================
# GEOCODING ANALYSIS - DATABASE FUNCTIONS
# ==============================================================================

#' Execute reverse geocoding for location clusters
#' 
#' @param participant_ids Optional participant filter
#' @param update_existing Whether to update existing geocoding results
#' @param batch_size Number of requests to process before status update
#' 
#' @return Nothing (side effect: updates database)

reverse_geocode_clusters_db <- function(participant_ids = NULL, update_existing = FALSE, 
                                        batch_size = 10) {
  
  cat("Starting reverse geocoding using local Nominatim service...\n")
  
  # Test service availability
  if (!test_nominatim_with_retry()) {
    stop("Nominatim service unavailable. Start with: docker-compose up -d")
  }
  
  # Get clusters needing geocoding
  clusters_to_geocode <- get_clusters_for_geocoding(participant_ids, update_existing)
  
  if (nrow(clusters_to_geocode) == 0) {
    cat("No clusters requiring geocoding\n")
    return(invisible(NULL))
  }
  
  cat("Processing", nrow(clusters_to_geocode), "clusters for reverse geocoding\n")
  
  # Execute geocoding with batch processing
  result <- batch_process_geocoding(clusters_to_geocode, batch_size)
  
  cat("✅ Geocoding complete - Success:", result$successful, "Failed:", result$failed, "\n")
}

#' Helper: Get clusters that need geocoding
get_clusters_for_geocoding <- function(participant_ids, update_existing) {
  
  # Build base query
  query <- build_cluster_query(participant_ids = participant_ids, include_geocoding = FALSE)
  
  # Add existing filter if not updating
  if (!update_existing) {
    query <- paste(query, "AND NOT EXISTS (
      SELECT 1 FROM gps2.cluster_geocoding cg 
      WHERE cg.subid = lc.subid AND cg.cluster_id = lc.cluster_id
    )")
  }
  
  return(query_gps2_db(query))
}

#' Helper: Process geocoding in batches
batch_process_geocoding <- function(clusters_to_geocode, batch_size) {
  
  successful_geocodes <- 0
  failed_geocodes <- 0
  
  nominatim_url <- get_config("geocoding", "nominatim_url")
  delay_seconds <- get_config("geocoding", "delay_seconds")
  
  for (i in 1:nrow(clusters_to_geocode)) {
    cluster <- clusters_to_geocode[i, ]
    
    result <- geocode_single_cluster(cluster, nominatim_url)
    
    if (result$success) {
      successful_geocodes <- successful_geocodes + 1
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
  }
  
  return(list(successful = successful_geocodes, failed = failed_geocodes))
}

#' Helper: Geocode a single cluster
geocode_single_cluster <- function(cluster, nominatim_url) {
  
  tryCatch({
    # Construct request URL
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
      
      # Process response with utility
      processed <- process_nominatim_response(result)
      
      if (!is.null(processed)) {
        # Insert to database
        insert_geocoding_result(cluster, processed)
        return(list(success = TRUE))
      }
    }
    
    return(list(success = FALSE))
    
  }, error = function(e) {
    cat("Error geocoding cluster", cluster$cluster_id, ":", e$message, "\n")
    return(list(success = FALSE))
  })
}

#' Helper: Insert geocoding result to database
insert_geocoding_result <- function(cluster, processed) {
  
  params <- geocoding_response_to_params(processed, cluster$cluster_id, cluster$subid)
  
  with_gps2_transaction(function(con) {
    dbExecute(con, build_geocoding_insert_sql(), params)
  }, "Failed to insert geocoding result")
}

#' Analyze geocoding success rates
#' 
#' @return List with overall and participant-level statistics

analyze_geocoding_coverage_db <- function() {
  
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