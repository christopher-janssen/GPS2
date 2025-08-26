# scripts/r/zoning_operations.R
# Complete Madison zoning data operations for GPS2 spatial analysis
# Handles loading, analysis, and spatial queries for Madison zoning districts

library(sf)
# DBI and RPostgres functions handled via database.R sourcing

source("scripts/r/database.R")

# ==============================================================================
# CORE ZONING DATA LOADING
# ==============================================================================

# Load Madison zoning data from GeoJSON with polygon combination
load_madison_zoning <- function(force_reload = FALSE) {
  cat("Madison Zoning Data Loader\n")
  cat("==========================\n")
  
  # Check if data already exists
  if (!force_reload) {
    existing_count <- get_zone_count()
    if (existing_count > 40) {
      cat("✅ Zoning data already loaded (", existing_count, "zone types)\n")
      cat("   Use force_reload = TRUE to reload from file\n")
      return(verify_zoning_data())
    }
  }
  
  # Get file path
  source("scripts/r/global_setup.R")
  zoning_file <- file.path(dirname(path_processed), "zoning", "madison_zoning_districts.geojson")
  
  if (!file.exists(zoning_file)) {
    cat("❌ Zoning file not found!\n")
    cat("   Expected: ", zoning_file, "\n")
    cat("   Download from: https://data-cityofmadison.opendata.arcgis.com/datasets/cityofmadison::zoning-districts-current\n")
    return(FALSE)
  }
  
  # Read and process data
  cat("Reading GeoJSON file...\n")
  original_sf <- st_read(zoning_file, quiet = TRUE)
  
  # Filter valid zones
  valid_zones <- original_sf |>
    filter(!is.na(ZONING_CODE), ZONING_CODE != "", ZONING_CODE != "NULL")
  
  cat("   Total polygons: ", nrow(valid_zones), "\n")
  cat("   Unique zone types: ", length(unique(valid_zones$ZONING_CODE)), "\n")
  
  # Fix geometries and ensure proper CRS
  if (st_crs(valid_zones)$epsg != 4326) {
    valid_zones <- st_transform(valid_zones, 4326)
  }
  valid_zones <- st_make_valid(valid_zones)
  
  # Combine multiple polygons per zone type into MultiPolygons
  cat("Combining polygons by zone type...\n")
  combined_zones <- valid_zones |>
    group_by(ZONING_CODE) |>
    summarise(geometry = st_union(geometry), .groups = "drop") |>
    mutate(geometry = st_cast(geometry, "MULTIPOLYGON"))
  
  # Prepare final data
  final_data <- combined_zones |>
    mutate(
      zone_code = ZONING_CODE,
      zone_name = ZONING_CODE,
      zone_category = classify_zone_category(ZONING_CODE),
      geometry_wkt = st_as_text(geometry)
    ) |>
    st_drop_geometry() |>
    select(zone_code, zone_name, zone_category, geometry_wkt)
  
  cat("   Prepared ", nrow(final_data), " zone records for insertion\n")
  
  # Insert into database
  cat("Loading into PostGIS database...\n")
  execute_gps2_db("DELETE FROM gps2.zoning_districts;")
  
  successful <- insert_zone_data(final_data)
  cat("   ✅ Successfully inserted: ", successful, " zones\n")
  
  return(verify_zoning_data())
}

# Helper function to classify zone codes into categories
classify_zone_category <- function(zone_codes) {
  sapply(zone_codes, function(code) {
    code_upper <- toupper(as.character(code))
    case_when(
      grepl("^(SR|TR|DR)", code_upper) ~ "Residential",
      grepl("^(LMX|NMX|MXC|RMX|UMX)", code_upper) ~ "Mixed-Use", 
      grepl("^(TSS|CC)", code_upper) ~ "Commercial",
      grepl("^(DC|UOR)", code_upper) ~ "Downtown",
      grepl("^(TE|SE|SEC|EC)", code_upper) ~ "Employment",
      grepl("^(IL|IG)", code_upper) ~ "Industrial",
      grepl("^(A|UA|CN|PR|AP|CI|PD|PMHP|ME|MC|THV)", code_upper) ~ "Special",
      grepl("^HIST", code_upper) ~ "Historic",
      TRUE ~ "Other"
    )
  })
}

# Insert zone data with proper transaction handling
insert_zone_data <- function(zone_data) {
  successful <- 0
  con <- connect_gps2_db()
  
  tryCatch({
    for (i in 1:nrow(zone_data)) {
      row <- zone_data[i, ]
      
      tryCatch({
        dbExecute(con, "
          INSERT INTO gps2.zoning_districts (zone_code, zone_name, zone_category, geometry)
          VALUES ($1, $2, $3, ST_SetSRID(ST_GeomFromText($4), 4326));
        ", list(row$zone_code, row$zone_name, row$zone_category, row$geometry_wkt))
        
        successful <- successful + 1
        
      }, error = function(e) {
        cat("   ⚠️  Failed to insert ", row$zone_code, ": ", e$message, "\n")
      })
    }
  }, finally = {
    disconnect_gps2_db(con)
  })
  
  return(successful)
}

# Get zone count with proper integer casting
get_zone_count <- function() {
  result <- query_gps2_db("SELECT COUNT(*)::integer as count 
                          FROM gps2.zoning_districts 
                          WHERE geometry IS NOT NULL;")
  return(as.integer(result$count))
}

# ==============================================================================
# ZONING DATA VERIFICATION AND ANALYSIS
# ==============================================================================

# Verify zoning data loaded correctly
verify_zoning_data <- function() {
  cat("\nZoning Data Verification\n")
  cat("===========================\n")
  
  total_zones <- get_zone_count()
  cat("Total zone types: ", total_zones, "\n")
  
  if (total_zones == 0) {
    cat("❌ No zoning data found\n")
    return(FALSE)
  }
  
  # Show breakdown by category
  category_summary <- query_gps2_db("
    SELECT zone_category, 
           COUNT(*)::integer as zone_types,
           ROUND(SUM(area_acres)::numeric, 1) as total_acres
    FROM gps2.zoning_districts 
    WHERE geometry IS NOT NULL
    GROUP BY zone_category 
    ORDER BY zone_types DESC;
  ")
  
  cat("\nZones by category:\n")
  print(category_summary)
  
  # Test spatial functionality
  test_result <- query_gps2_db("
    SELECT zone_code, zone_name 
    FROM gps2.get_zoning_for_point(43.074713, -89.384373) 
    LIMIT 1;
  ")
  
  if (nrow(test_result) > 0) {
    cat("\nSpatial queries working - Test point in zone: ", test_result$zone_code, "\n")
  } else {
    nearest <- query_gps2_db("
      SELECT zone_code, 
             ROUND(ST_Distance(geometry::geography, 
                              ST_SetSRID(ST_MakePoint(-89.384373, 43.074713), 4326)::geography)) as distance_m
      FROM gps2.zoning_districts 
      ORDER BY distance_m LIMIT 1;
    ")
    cat("\nSpatial queries working - Nearest zone: ", nearest$zone_code, " (", nearest$distance_m, "m away)\n")
  }
  
  if (total_zones >= 40) {
    cat("\n✅ SUCCESS: Madison zoning data loaded properly!\n")
    return(TRUE)
  } else {
    cat("\n⚠️  Warning: Only ", total_zones, " zones loaded (expected 40+)\n")
    return(FALSE)
  }
}

# ==============================================================================
# CLUSTER-ZONING INTEGRATION
# ==============================================================================

# Add zoning information to GPS location clusters
add_zoning_to_clusters <- function() {
  cat("Adding zoning information to location clusters...\n")
  
  # Create cluster-zoning lookup table
  execute_gps2_db("DROP TABLE IF EXISTS gps2.cluster_zoning;")
  execute_gps2_db("CREATE TABLE gps2.cluster_zoning 
                  AS SELECT * 
                  FROM gps2.get_zoning_for_clusters();")
  
  # Add indexes for performance
  execute_gps2_db("CREATE INDEX idx_cluster_zoning_subid_cluster 
                  ON gps2.cluster_zoning (subid, cluster_id);")
  execute_gps2_db("CREATE INDEX idx_cluster_zoning_category 
                  ON gps2.cluster_zoning (zone_category);")
  
  # Get summary
  summary <- query_gps2_db("
    SELECT 
      zone_category,
      COUNT(*)::integer as cluster_count,
      COUNT(DISTINCT subid)::integer as participant_count
    FROM gps2.cluster_zoning
    WHERE zone_category IS NOT NULL
    GROUP BY zone_category
    ORDER BY cluster_count DESC;
  ")
  
  cat("Zoning analysis complete!\n\n")
  cat("Clusters by zoning category:\n")
  print(summary)
  
  return(summary)
}

# Show comprehensive zoning distribution for participants
show_zoning_summary <- function(subids = NULL) {
  
  participant_filter <- ""
  if (!is.null(subids)) {
    participant_list <- paste(subids, collapse = ",")
    participant_filter <- paste0("AND lc.subid IN (", participant_list, ")")
  }
  
  summary <- query_gps2_db(paste0("
    SELECT 
      COALESCE(cz.zone_category, 'Unzoned') as zone_category,
      COUNT(*)::integer as cluster_count,
      COUNT(DISTINCT lc.subid)::integer as participant_count,
      SUM(lc.total_visits)::integer as total_visits,
      ROUND(SUM(lc.total_duration_hours)::numeric, 1) as total_hours,
      ROUND(AVG(lc.total_duration_hours)::numeric, 2) as avg_hours_per_cluster
    FROM gps2.location_clusters lc
    LEFT JOIN gps2.cluster_zoning cz ON lc.subid = cz.subid AND lc.cluster_id = cz.cluster_id
    WHERE 1=1 ", participant_filter, "
    GROUP BY COALESCE(cz.zone_category, 'Unzoned')
    ORDER BY total_hours DESC;
  "))
  
  cat("PARTICIPANT ZONING DISTRIBUTION\n")
  cat("===================================\n")
  
  return(summary)
}

# ==============================================================================
# SPATIAL ANALYSIS FUNCTIONS
# ==============================================================================

# Find zoning for specific GPS coordinates
get_point_zoning <- function(lat, lon) {
  result <- query_gps2_db("
    SELECT zone_code, zone_name, zone_category, zone_description
    FROM gps2.get_zoning_for_point($1, $2);
  ", list(lat, lon))
  
  if (nrow(result) == 0) {
    # Find nearest zone if point is not in any zone
    nearest <- query_gps2_db("
      SELECT zone_code, zone_name, zone_category,
             ROUND(ST_Distance(geometry::geography, 
                              ST_SetSRID(ST_MakePoint($2, $1), 4326)::geography)) as distance_m
      FROM gps2.zoning_districts 
      ORDER BY distance_m LIMIT 1;
    ", list(lat, lon))
    
    if (nrow(nearest) > 0) {
      cat("Point not in any zone. Nearest zone: ", nearest$zone_code, " (", nearest$distance_m, "m away)\n")
    }
    return(tibble())
  }
  
  return(result)
}

# Get clusters within specific zoning categories
get_clusters_by_zoning <- function(zone_category = NULL, participant_ids = NULL) {
  
  category_filter <- ""
  if (!is.null(zone_category)) {
    if (length(zone_category) == 1) {
      category_filter <- paste0("AND cz.zone_category = '", zone_category, "'")
    } else {
      category_list <- paste0("'", zone_category, "'", collapse = ",")
      category_filter <- paste0("AND cz.zone_category IN (", category_list, ")")
    }
  }
  
  participant_filter <- ""
  if (!is.null(participant_ids)) {
    participant_list <- paste(participant_ids, collapse = ",")
    participant_filter <- paste0("AND lc.subid IN (", participant_list, ")")
  }
  
  query <- paste0("
    SELECT 
      lc.subid, lc.cluster_id, lc.lat, lc.lon,
      lc.total_visits, lc.unique_days, lc.total_duration_hours,
      cz.zone_code, cz.zone_name, cz.zone_category
    FROM gps2.location_clusters lc
    JOIN gps2.cluster_zoning cz ON lc.subid = cz.subid AND lc.cluster_id = cz.cluster_id
    WHERE 1=1 ", category_filter, " ", participant_filter, "
    ORDER BY lc.total_duration_hours DESC;
  ")
  
  return(query_gps2_db(query))
}

# Analyze zoning patterns for specific participants
analyze_participant_zoning <- function(participant_ids) {
  
  if (length(participant_ids) == 1) {
    cat("Zoning Analysis for Participant ", participant_ids, "\n")
  } else {
    cat("Zoning Analysis for ", length(participant_ids), " Participants\n")
  }
  cat("==========================================\n")
  
  # Get detailed breakdown
  analysis <- query_gps2_db("
    SELECT 
      lc.subid,
      COALESCE(cz.zone_category, 'Unzoned') as zone_category,
      COUNT(*)::integer as clusters,
      SUM(lc.total_visits)::integer as total_visits,
      ROUND(SUM(lc.total_duration_hours)::numeric, 1) as total_hours,
      ROUND(AVG(lc.total_duration_hours)::numeric, 2) as avg_hours_per_cluster
    FROM gps2.location_clusters lc
    LEFT JOIN gps2.cluster_zoning cz ON lc.subid = cz.subid AND lc.cluster_id = cz.cluster_id
    WHERE lc.subid = ANY($1)
    GROUP BY lc.subid, COALESCE(cz.zone_category, 'Unzoned')
    ORDER BY lc.subid, total_hours DESC;
  ", list(participant_ids))
  
  # Show summary by zone category
  category_summary <- analysis |>
    group_by(zone_category) |>
    summarise(
      participants = n_distinct(subid),
      total_clusters = sum(clusters),
      total_visits = sum(total_visits),
      total_hours = sum(total_hours),
      .groups = "drop"
    ) |>
    arrange(desc(total_hours))
  
  cat("Summary by zone category:\n")
  print(category_summary)
  
  return(list(detailed = analysis, summary = category_summary))
}

# ==============================================================================
# SYSTEM SETUP AND MAINTENANCE
# ==============================================================================

# Complete zoning system setup
setup_zoning_system <- function() {
  cat("GPS2 Zoning System Setup\n")
  cat("============================\n")
  
  # Check database connectivity
  if (!test_connection()) {
    cat("❌ Database connection failed\n")
    return(FALSE)
  }
  
  # Check if zoning table exists
  table_exists <- as.integer(query_gps2_db("
    SELECT COUNT(*)::integer as count 
    FROM information_schema.tables 
    WHERE table_schema = 'gps2' AND table_name = 'zoning_districts';
  ")$count)
  
  if (table_exists == 0) {
    cat("❌ Zoning table doesn't exist. Run the SQL setup script first.\n")
    return(FALSE)
  }
  
  cat("✅ Database setup verified\n")
  
  # Load zoning data
  success <- load_madison_zoning()
  
  if (!success) {
    return(FALSE)
  }
  
  cat("\n Next Steps:\n")
  cat("   1. Run clustering: analyze_all_participants()\n")
  cat("   2. Add zoning to clusters: add_zoning_to_clusters()\n")
  cat("   3. Analyze patterns: show_zoning_summary()\n")
  cat("   4. Create maps: source('scripts/r/visualization.R')\n")
  
  return(TRUE)
}

# Check system status
check_zoning_status <- function() {
  cat("GPS2 Zoning System Status\n")
  cat("=============================\n")
  
  # Check zones
  zone_count <- get_zone_count()
  cat("Zone types loaded: ", zone_count, "\n")
  
  # Check clusters
  cluster_count <- as.integer(query_gps2_db("SELECT COUNT(*)::integer as count FROM gps2.location_clusters;")$count)
  cat("Location clusters: ", cluster_count, "\n")
  
  # Check cluster-zoning integration
  cluster_zoning_exists <- as.integer(query_gps2_db("
    SELECT COUNT(*)::integer as count 
    FROM information_schema.tables 
    WHERE table_schema = 'gps2' AND table_name = 'cluster_zoning';
  ")$count)
  
  if (cluster_zoning_exists > 0) {
    zoned_clusters <- as.integer(query_gps2_db("SELECT COUNT(*)::integer as count FROM gps2.cluster_zoning WHERE zone_category IS NOT NULL;")$count)
    cat("Clusters with zoning: ", zoned_clusters, "\n")
  } else {
    cat("Cluster-zoning integration not set up. Run: add_zoning_to_clusters()\n")
  }
  
  # Check participants
  participants <- as.integer(query_gps2_db("SELECT COUNT(DISTINCT subid)::integer as count FROM gps2.location_clusters;")$count)
  cat("Participants with clusters: ", participants, "\n")
  
  if (zone_count > 0 && cluster_count > 0) {
    cat("\n✅ System ready for spatial analysis!\n")
  }
}