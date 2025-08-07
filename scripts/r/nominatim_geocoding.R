# scripts/r/nominatim_geocoding.R
# Local Nominatim-based reverse geocoding for GPS2 clusters
# maintains privacy compliance through local OpenStreetMap processing

library(httr)
library(jsonlite, exclude = "flatten")
library(DBI)
library(dplyr)
source("scripts/r/postgis_connection.R")

# test Nominatim service availability and configuration
test_nominatim_connection <- function(nominatim_url = "http://localhost:8080") {
  
  cat("Testing Nominatim geocoding service connectivity...\n")
  
  tryCatch({
    # verify service availability through status endpoint
    status_response <- GET(paste0(nominatim_url, "/status.php"))
    
    if (status_code(status_response) != 200) {
      stop("Nominatim service not accessible at ", nominatim_url)
    }
    
    status_content <- content(status_response, "parsed")
    cat("Nominatim service operational\n")
    cat("Database status:", status_content$status, "\n")
    
    # verify reverse geocoding functionality with Madison coordinates
    test_url <- paste0(nominatim_url, "/reverse?format=json&lat=43.074713&lon=-89.384373&addressdetails=1")
    test_response <- GET(test_url)
    
    if (status_code(test_response) == 200) {
      test_result <- content(test_response, "parsed")
      cat("Reverse geocoding test successful\n")
      cat("Sample address:", test_result$display_name, "\n")
      return(TRUE)
    } else {
      cat("Reverse geocoding test failed with status code:", status_code(test_response), "\n")
      return(FALSE)
    }
    
  }, error = function(e) {
    cat("Nominatim connection error:", e$message, "\n")
    cat("Verify Nominatim container is running with: docker-compose ps\n")
    return(FALSE)
  })
}

# execute reverse geocoding for location clusters using local Nominatim service
reverse_geocode_clusters_nominatim <- function(participant_ids = NULL, update_existing = FALSE, 
                                               nominatim_url = "http://localhost:8080",
                                               batch_size = 10, delay_seconds = 0.1) {
  
  cat("Initiating reverse geocoding using local Nominatim service...\n")
  
  # verify Nominatim service availability before processing
  if (!test_nominatim_connection(nominatim_url)) {
    stop("Nominatim service unavailable. Start service with: docker-compose up -d nominatim")
  }
  
  con <- connect_to_gps2_db()
  
  tryCatch({
    # establish geocoding results table with comprehensive schema
    create_table_sql <- "
    CREATE TABLE IF NOT EXISTS gps2.cluster_geocoding (
      id SERIAL PRIMARY KEY,
      cluster_id INTEGER NOT NULL,
      subid INTEGER NOT NULL,
      display_name TEXT,
      house_number VARCHAR(20),
      road VARCHAR(200),
      neighbourhood VARCHAR(100),
      city VARCHAR(100),
      county VARCHAR(100),
      state VARCHAR(50),
      postcode VARCHAR(20),
      country VARCHAR(50),
      place_type VARCHAR(50),
      osm_type VARCHAR(10),
      osm_id BIGINT,
      geocoding_confidence NUMERIC(3,2),
      geocoding_method VARCHAR(50),
      processed_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      UNIQUE(subid, cluster_id)
    );
    "
    
    dbExecute(con, create_table_sql)
    cat("Geocoding results table initialized\n")
    
    # construct query for clusters requiring geocoding
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
        lc.cluster_id,
        lc.subid,
        lc.lat,
        lc.lon,
        lc.total_visits,
        lc.unique_days,
        lc.total_duration_hours
      FROM gps2.location_clusters lc
      ", participant_filter, " ", existing_filter, "
      ORDER BY lc.total_visits DESC, lc.subid, lc.cluster_id;
    ")
    
    clusters_to_geocode <- dbGetQuery(con, clusters_query)
    
    if (nrow(clusters_to_geocode) == 0) {
      cat("No clusters requiring geocoding identified\n")
      return(invisible(NULL))
    }
    
    cat("Processing", nrow(clusters_to_geocode), "clusters for reverse geocoding\n")
    cat("Batch size:", batch_size, "with", delay_seconds, "second intervals\n")
    
    # execute geocoding process with batch processing for service efficiency
    successful_geocodes <- 0
    failed_geocodes <- 0
    
    for (i in 1:nrow(clusters_to_geocode)) {
      cluster <- clusters_to_geocode[i, ]
      
      tryCatch({
        # construct Nominatim reverse geocoding request
        geocode_url <- paste0(
          nominatim_url, 
          "/reverse?format=json&lat=", cluster$lat,
          "&lon=", cluster$lon,
          "&addressdetails=1&extratags=1"
        )
        
        # execute request to local Nominatim service
        response <- GET(geocode_url)
        
        if (status_code(response) == 200) {
          result <- content(response, "parsed")
          
          # extract address components from Nominatim response structure
          address <- result$address
          
          if (!is.null(result$display_name) && result$display_name != "") {
            
            # insert successful geocoding result with comprehensive address components
            insert_sql <- "
            INSERT INTO gps2.cluster_geocoding 
              (cluster_id, subid, display_name, house_number, road, neighbourhood, 
               city, county, state, postcode, country, place_type, osm_type, osm_id,
               geocoding_confidence, geocoding_method)
            VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15, $16)
            ON CONFLICT (subid, cluster_id) 
            DO UPDATE SET
              display_name = EXCLUDED.display_name,
              house_number = EXCLUDED.house_number,
              road = EXCLUDED.road,
              neighbourhood = EXCLUDED.neighbourhood,
              city = EXCLUDED.city,
              county = EXCLUDED.county,
              state = EXCLUDED.state,
              postcode = EXCLUDED.postcode,
              country = EXCLUDED.country,
              place_type = EXCLUDED.place_type,
              osm_type = EXCLUDED.osm_type,
              osm_id = EXCLUDED.osm_id,
              geocoding_confidence = EXCLUDED.geocoding_confidence,
              geocoding_method = EXCLUDED.geocoding_method,
              processed_at = CURRENT_TIMESTAMP;
            "
            
            # execute database insertion with proper null handling
            dbExecute(con, insert_sql, list(
              cluster$cluster_id,
              cluster$subid,
              result$display_name,
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
            # record failed geocoding attempt to prevent reprocessing
            null_insert_sql <- "
            INSERT INTO gps2.cluster_geocoding 
              (cluster_id, subid, display_name, geocoding_method)
            VALUES ($1, $2, 'No address found', 'NOMINATIM_NULL')
            ON CONFLICT (subid, cluster_id) DO NOTHING;
            "
            
            dbExecute(con, null_insert_sql, list(cluster$cluster_id, cluster$subid))
            failed_geocodes <- failed_geocodes + 1
          }
          
        } else {
          failed_geocodes <- failed_geocodes + 1
          cat("HTTP error", status_code(response), "for cluster", cluster$cluster_id, 
              "participant", cluster$subid, "\n")
        }
        
        # progress reporting and service rate limiting
        if (i %% batch_size == 0 || i == nrow(clusters_to_geocode)) {
          cat("Processed", i, "of", nrow(clusters_to_geocode), "clusters",
              "- Success:", successful_geocodes, "Failed:", failed_geocodes, "\n")
        }
        
        # implement respectful delay between requests
        if (i < nrow(clusters_to_geocode)) {
          Sys.sleep(delay_seconds)
        }
        
      }, error = function(e) {
        cat("Error geocoding cluster", cluster$cluster_id, "for participant", cluster$subid, 
            ":", e$message, "\n")
        failed_geocodes <- failed_geocodes + 1
      })
    }
    
    cat("Nominatim reverse geocoding completed\n")
    cat("Successfully geocoded:", successful_geocodes, "clusters\n")
    cat("Failed geocoding:", failed_geocodes, "clusters\n")
    
    # generate sample results for verification
    sample_results <- dbGetQuery(con, "
      SELECT 
        cg.subid,
        cg.cluster_id,
        cg.display_name,
        cg.road,
        cg.city,
        cg.state,
        cg.postcode,
        cg.place_type,
        lc.total_visits,
        lc.unique_days
      FROM gps2.cluster_geocoding cg
      JOIN gps2.location_clusters lc ON cg.subid = lc.subid AND cg.cluster_id = lc.cluster_id
      WHERE cg.display_name IS NOT NULL AND cg.display_name != 'No address found'
      ORDER BY lc.total_duration_hours DESC, lc.total_visits DESC
      LIMIT 10;
    ")
    
    if (nrow(sample_results) > 0) {
      cat("Sample geocoding results:\n")
      print(sample_results)
    }
    
  }, error = function(e) {
    cat("Error in Nominatim geocoding process:", e$message, "\n")
  }, finally = {
    dbDisconnect(con)
  })
}

# retrieve geocoded cluster results with optional filtering
get_geocoded_clusters <- function(participant_ids = NULL, include_failed = FALSE) {
  
  con <- connect_to_gps2_db()
  
  tryCatch({
    # construct query with participant filtering
    participant_filter <- ""
    if (!is.null(participant_ids)) {
      participant_list <- paste(participant_ids, collapse = ",")
      participant_filter <- paste0("AND cg.subid IN (", participant_list, ")")
    }
    
    # apply filter for failed geocoding results
    failed_filter <- ""
    if (!include_failed) {
      failed_filter <- "AND cg.display_name IS NOT NULL AND cg.display_name != 'No address found'"
    }
    
    query <- paste0("
      SELECT 
        cg.subid,
        cg.cluster_id,
        lc.lat,
        lc.lon,
        cg.display_name,
        cg.road,
        cg.city,
        cg.state,
        cg.postcode,
        cg.geocoding_confidence,
        cg.geocoding_method,
        lc.total_visits,
        lc.unique_days,
        lc.total_duration_hours,
        lc.first_visit,
        lc.last_visit
      FROM gps2.cluster_geocoding cg
      JOIN gps2.location_clusters lc ON cg.subid = lc.subid AND cg.cluster_id = lc.cluster_id
      WHERE 1=1 ", participant_filter, " ", failed_filter, "
      ORDER BY cg.subid, lc.total_duration_hours DESC, lc.total_visits DESC;
    ")
    
    results <- dbGetQuery(con, query)
    return(results)
    
  }, error = function(e) {
    cat("Error retrieving geocoded clusters:", e$message, "\n")
    return(data.frame())
  }, finally = {
    dbDisconnect(con)
  })
}

# generate comprehensive analysis of geocoding success rates
analyze_geocoding_coverage <- function() {
  
  con <- connect_to_gps2_db()
  
  tryCatch({
    # calculate overall success metrics
    overall_stats <- dbGetQuery(con, "
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
    
    cat("Geocoding Coverage Analysis\n")
    cat("===========================\n")
    cat("Total clusters:", overall_stats$total_clusters, "\n")
    cat("Successfully geocoded:", overall_stats$geocoded_clusters, "\n")
    cat("Success rate:", overall_stats$success_rate_percent, "percent\n\n")
    
    # generate participant-level success analysis
    participant_stats <- dbGetQuery(con, "
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
      ORDER BY success_rate_percent DESC, total_clusters DESC;
    ")
    
    cat("Success rate by participant (top 10):\n")
    print(head(participant_stats, 10))
    
    return(list(overall = overall_stats, by_participant = participant_stats))
    
  }, error = function(e) {
    cat("Error analyzing geocoding coverage:", e$message, "\n")
    return(NULL)
  }, finally = {
    dbDisconnect(con)
  })
}

# Usage examples:
# 
# # Verify Nominatim service connectivity
# test_nominatim_connection()
# 
# # Execute geocoding for all clusters
# reverse_geocode_clusters_nominatim()
# 
# # Execute geocoding for specific participants
# reverse_geocode_clusters_nominatim(participant_ids = c(19, 56))
# 
# # Accelerate processing with larger batches if system permits
# reverse_geocode_clusters_nominatim(batch_size = 25, delay_seconds = 0.05)
# 
# # Retrieve geocoded results
# geocoded_data <- get_geocoded_clusters()
# participant_results <- get_geocoded_clusters(participant_ids = 19)
# 
# # Analyze geocoding performance
# coverage_analysis <- analyze_geocoding_coverage()