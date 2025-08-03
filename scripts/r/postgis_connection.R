# Updated connect_to_gps2_db function using RPostgres driver
# Replace the existing function in your postgis_connection.R

library(DBI)
library(RPostgres)  # Use RPostgres instead of RPostgreSQL

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