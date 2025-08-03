# scripts/r/postgis_connection.R
# PostGIS database connection and testing functions for GPS2 project

library(DBI)
library(RPostgreSQL)

# primary function to establish connection to GPS2 PostGIS database
# This handles all connection logic and provides error messages
connect_to_gps2_db <- function() {
  tryCatch({
    # attempt database connection using standard PostgreSQL parameters
    con <- dbConnect(
      PostgreSQL(),
      host = "localhost",        # database runs locally in Docker
      port = 5432,              # standard PostgreSQL port
      dbname = "gps2_geocoding", # database name from docker-compose.yml
      user = "gps2_researcher",  # username from docker-compose.yml
      password = Sys.getenv("GPS2_DB_PASSWORD", "secure_research_password")
    )
    
    # verify that PostGIS extensions are properly loaded and functional
    postgis_version <- dbGetQuery(con, "SELECT PostGIS_Version();")
    cat("✅ Successfully connected to GPS2 PostGIS database\n")
    cat("PostGIS version:", postgis_version$postgis_version, "\n")
    
    # test that our GPS2 schema exists and is accessible
    schema_check <- dbGetQuery(con, 
      "SELECT schema_name FROM information_schema.schemata WHERE schema_name = 'gps2';")
    
    if (nrow(schema_check) > 0) {
      cat("✅ GPS2 research schema is properly configured\n")
    } else {
      warning("❌ GPS2 schema not found - database may not be fully initialized")
    }
    
    return(con)
    
  }, error = function(e) {
    # common connection error catching
    if (grepl("could not connect", e$message, ignore.case = TRUE)) {
      stop("❌ Cannot connect to PostGIS database.\n",
           "Troubleshooting steps:\n",
           "1. Ensure Docker container is running: docker-compose up -d\n",
           "2. Wait 30-60 seconds for database initialization\n",
           "3. Check container status: docker-compose ps\n",
           "4. View container logs: docker-compose logs postgis")
    } else if (grepl("authentication", e$message, ignore.case = TRUE)) {
      stop("❌ Database authentication failed.\n",
           "Check that GPS2_DB_PASSWORD environment variable matches docker-compose.yml")
    } else {
      stop("❌ Database connection error: ", e$message)
    }
  })
}

# test of PostGIS spatial functionality
test_postgis_spatial_functions <- function() {
  cat("Testing PostGIS spatial functionality for GPS2 analysis...\n")
  cat("=" %R% 55, "\n")
  
  con <- connect_to_gps2_db()
  
  tryCatch({
    # test 1: Verify test data exists and spatial table structure is correct
    test_data_count <- dbGetQuery(con, "SELECT COUNT(*) as count FROM gps2.connection_test;")
    cat("✅ Found", test_data_count$count, "test locations in database\n")
    
    # test 2: Verify spatial distance calculations work with real coordinates
    distance_results <- dbGetQuery(con, "SELECT * FROM gps2.test_spatial_functions();")
    cat("✅ Spatial distance calculations working:\n")
    for (i in 1:min(3, nrow(distance_results))) {
      row <- distance_results[i, ]
      cat("   ", row$from_location, "to", row$to_location, ":", 
          round(row$distance_meters), "meters\n")
    }
    
    # test 3: verify spatial indexing is functioning for performance
    index_check <- dbGetQuery(con, 
      "SELECT schemaname, tablename, indexname 
       FROM pg_indexes 
       WHERE schemaname = 'gps2' AND indexname LIKE 'idx_%';")
    cat("✅ Found", nrow(index_check), "spatial indexes for optimized queries\n")
    
    # test 4: test coordinate extraction and manipulation
    coordinate_test <- dbGetQuery(con,
      "SELECT 
         location_name,
         ROUND(ST_X(test_point)::numeric, 6) as longitude,
         ROUND(ST_Y(test_point)::numeric, 6) as latitude
       FROM gps2.connection_test 
       LIMIT 1;")
    cat("✅ Coordinate extraction working:", coordinate_test$location_name, 
        "is at", coordinate_test$latitude, ",", coordinate_test$longitude, "\n")
    
    # test 5: Verify geographic coordinate system handling
    crs_test <- dbGetQuery(con,
      "SELECT ST_SRID(test_point) as coordinate_system 
       FROM gps2.connection_test LIMIT 1;")
    cat("✅ Coordinate system verification: EPSG:", crs_test$coordinate_system, 
        "(WGS84 GPS standard)\n")
    
    cat("\n🎉 All PostGIS spatial functions verified and ready for GPS analysis!\n")
    
  }, error = function(e) {
    cat("❌ Spatial function test failed:", e$message, "\n")
    cat("This may indicate incomplete database initialization.\n")
    cat("Try: docker-compose down && docker-compose up -d\n")
  }, finally = {
    dbDisconnect(con)
  })
}

# function to display comprehensive database information
show_gps2_database_info <- function() {
  cat("GPS2 PostGIS Database Information\n")
  cat("================================\n")
  
  con <- connect_to_gps2_db()
  
  tryCatch({
    # display basic database connection information
    db_info <- dbGetQuery(con, 
      "SELECT 
         current_database() as database_name,
         current_user as connected_user,
         version() as postgres_version;")
    
    cat("Database name:", db_info$database_name, "\n")
    cat("Connected as:", db_info$connected_user, "\n")
    cat("PostgreSQL version:", substr(db_info$postgres_version, 1, 50), "...\n\n")
    
    # show available schemas (workspaces) in the database
    schemas <- dbGetQuery(con, 
      "SELECT schema_name 
       FROM information_schema.schemata 
       WHERE schema_name NOT LIKE 'pg_%' 
       AND schema_name NOT IN ('information_schema', 'tiger', 'tiger_data')
       ORDER BY schema_name;")
    cat("Available research schemas:", paste(schemas$schema_name, collapse = ", "), "\n")
    
    # show tables specifically in the GPS2 schema
    gps2_tables <- dbGetQuery(con, 
      "SELECT table_name, 
              (SELECT COUNT(*) FROM information_schema.columns 
               WHERE table_schema = 'gps2' AND table_name = t.table_name) as column_count
       FROM information_schema.tables t
       WHERE table_schema = 'gps2'
       ORDER BY table_name;")
    
    if (nrow(gps2_tables) > 0) {
      cat("Tables in GPS2 schema:\n")
      for (i in 1:nrow(gps2_tables)) {
        cat("  •", gps2_tables$table_name[i], "(", gps2_tables$column_count[i], "columns)\n")
      }
    } else {
      cat("No tables found in GPS2 schema\n")
    }
    
    # show PostGIS-specific information
    postgis_info <- dbGetQuery(con, 
      "SELECT PostGIS_Version() as postgis_version, 
              PostGIS_Full_Version() as full_version;")
    cat("\nPostGIS spatial extension:", postgis_info$postgis_version, "\n")
    
  }, error = function(e) {
    cat("Error retrieving database information:", e$message, "\n")
  }, finally = {
    dbDisconnect(con)
  })
  
  cat("\n")
}

# utility function to safely disconnect from database
# (always call this when you're done with database operations)
safe_disconnect <- function(connection) {
  if (!is.null(connection) && dbIsValid(connection)) {
    dbDisconnect(connection)
    cat("Database connection closed safely\n")
  }
}