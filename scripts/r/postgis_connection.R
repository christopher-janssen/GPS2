# scripts/r/postgis_connection.R
# PostGIS database connection and testing functions for GPS2 project

library(DBI)
library(RPostgres)  # Use RPostgres instead of RPostgreSQL

# primary function to establish connection to GPS2 PostGIS database
# This handles all connection logic and provides error messages
connect_to_gps2_db <- function() {
  tryCatch({
    # attempt database connection using RPostgres (more reliable)
    con <- dbConnect(
      RPostgres::Postgres(),  # Changed from PostgreSQL() to Postgres()
      host = "localhost",        
      port = 5432,              
      dbname = "gps2_geocoding", 
      user = "gps2_researcher",  
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
    # enhanced error messaging
    if (grepl("could not connect|connection refused", e$message, ignore.case = TRUE)) {
      stop("❌ Cannot connect to PostGIS database.\n",
           "Troubleshooting steps:\n",
           "1. Ensure Docker container is running: docker-compose up -d\n",
           "2. Wait 30-60 seconds for database initialization\n", 
           "3. Check container status: docker-compose ps\n",
           "4. View container logs: docker-compose logs postgis\n",
           "5. Test manual connection: docker exec -it gps2_geocoding psql -U gps2_researcher -d gps2_geocoding")
    } else if (grepl("authentication|password", e$message, ignore.case = TRUE)) {
      stop("❌ Database authentication failed.\n",
           "Check that password matches docker-compose.yml\n",
           "Set environment variable: Sys.setenv(GPS2_DB_PASSWORD = 'secure_research_password')")
    } else {
      stop("❌ Database connection error: ", e$message)
    }
  })
}

# test of PostGIS spatial functionality
test_spatial_data <- function() {
  cat("🧪 Testing spatial queries on your GPS data...\n")
  
  con <- connect_to_gps2_db()
  
  tryCatch({
    # Test 1: Basic data check
    data_summary <- dbGetQuery(con, "
      SELECT 
        COUNT(*) as total_points,
        COUNT(DISTINCT subid) as participants,
        MIN(dttm_obs) as earliest_data,
        MAX(dttm_obs) as latest_data
      FROM gps2.gps_stationary_points;
    ")
    
    cat("📊 Your GPS data in PostGIS:\n")
    print(data_summary)
    
    # Test 2: Spatial extent (bounding box of all your data)
    extent_check <- dbGetQuery(con, "
      SELECT 
        ROUND(ST_XMin(ST_Extent(location))::numeric, 4) as min_lon,
        ROUND(ST_YMin(ST_Extent(location))::numeric, 4) as min_lat,
        ROUND(ST_XMax(ST_Extent(location))::numeric, 4) as max_lon,
        ROUND(ST_YMax(ST_Extent(location))::numeric, 4) as max_lat
      FROM gps2.gps_stationary_points;
    ")
    
    cat("🗺️  Geographic extent of your data:\n")
    print(extent_check)
    
    # Test 3: Sample points by participant (much faster!)
    sample_by_participant <- dbGetQuery(con, "
      SELECT 
        subid,
        COUNT(*) as points,
        ROUND(ST_X(ST_Centroid(ST_Collect(location)))::numeric, 4) as center_lon,
        ROUND(ST_Y(ST_Centroid(ST_Collect(location)))::numeric, 4) as center_lat
      FROM gps2.gps_stationary_points 
      GROUP BY subid 
      ORDER BY points DESC
      LIMIT 5;
    ")
    
    cat("📍 Top 5 participants by GPS points:\n")
    print(sample_by_participant)
    
    # Test 4: Simple distance test with just a few points
    distance_test <- dbGetQuery(con, "
      WITH sample_points AS (
        SELECT subid, location, lat, lon, id
        FROM gps2.gps_stationary_points 
        WHERE subid = 19 
        ORDER BY dttm_obs 
        LIMIT 3
      )
      SELECT 
        a.id as point_a,
        b.id as point_b,
        ROUND(ST_Distance(a.location::geography, b.location::geography)) as distance_meters
      FROM sample_points a, sample_points b
      WHERE a.id < b.id;
    ")
    
    cat("📏 Sample distances between consecutive points (meters):\n")
    print(distance_test)
    
  }, error = function(e) {
    cat("❌ Error testing spatial data:", e$message, "\n")
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