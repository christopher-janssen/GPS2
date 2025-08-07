# scripts/bootstrap.R
# One-time setup script for the entire GPS2 environment
# Run this ONCE when setting up a new environment

library(here)
source(here("scripts/r/database.R"))

bootstrap_gps2_environment <- function(force_recreate = FALSE) {
  cat("GPS2 Environment Bootstrap\n")
  cat("==========================\n")
  
  # Step 1: Start Docker containers
  cat("1. Starting Docker containers...\n")
  system("cd docker-postgis && docker-compose up -d", ignore.stdout = TRUE)
  cat("   Waiting for services to initialize...\n")
  Sys.sleep(20)
  
  # Step 2: Check if schema already exists
  if (!force_recreate) {
    if (test_connection()) {
      existing_tables <- query_gps2_db("
        SELECT COUNT(*) as count 
        FROM information_schema.tables 
        WHERE table_schema = 'gps2';
      ")$count
      
      if (existing_tables > 0) {
        cat("   Schema already exists with", existing_tables, "tables.\n")
        cat("   Use force_recreate = TRUE to rebuild.\n")
        check_gps2_system()
        return(invisible(TRUE))
      }
    }
  }
  
  # Step 3: Verify database schema (created automatically by Docker)
  cat("2. Verifying database schema...\n")
  
  # Wait a bit more for initialization if needed
  max_attempts <- 6
  for (attempt in 1:max_attempts) {
    if (test_connection()) {
      tables <- query_gps2_db("
        SELECT COUNT(*) as count 
        FROM information_schema.tables 
        WHERE table_schema = 'gps2';
      ")$count
      
      if (tables >= 3) {
        cat("   ✅ Schema ready with", tables, "tables\n")
        break
      }
    }
    
    if (attempt < max_attempts) {
      cat("   Waiting for database initialization... (", attempt, "/", max_attempts, ")\n")
      Sys.sleep(5)
    } else {
      stop("Database schema not ready after ", max_attempts * 5, " seconds")
    }
  }
  
  # Step 4: Verify setup
  cat("3. Verifying setup...\n")
  success <- check_gps2_system()
  
  if (success) {
    cat("\n✅ GPS2 environment ready!\n")
    cat("\nNext steps:\n")
    cat("- Load GPS data: source('scripts/r/data_operations.R')\n")
    cat("- Run analysis: source('scripts/r/analysis.R')\n")
    cat("- Create visualizations: source('scripts/r/visualization.R')\n")
  }
  
  return(success)
}

# Run bootstrap if script is executed directly
if (!interactive()) {
  bootstrap_gps2_environment()
}