# Database Functions for GPS3 Project

#' Connect to Geolocation Database
#'
#' @param user Optional username override (defaults to GPS_DB_PARAMS or system user)
#' @param password Optional password override
#' @return Database connection or NULL if failed
connect_to_db <- function(user = NULL, password = NULL) {
  # Build connection parameters
  params <- list(
    host = GPS_DB_PARAMS$host,
    port = GPS_DB_PARAMS$port,
    dbname = GPS_DB_PARAMS$dbname
  )

  # Add user if provided or exists in GPS_DB_PARAMS
  if (!is.null(user)) {
    params$user <- user
  } else if (!is.null(GPS_DB_PARAMS$user)) {
    params$user <- GPS_DB_PARAMS$user
  }

  # Add password if provided or exists in GPS_DB_PARAMS
  if (!is.null(password)) {
    params$password <- password
  } else if (!is.null(GPS_DB_PARAMS$password)) {
    params$password <- GPS_DB_PARAMS$password
  }

  # Attempt connection with error handling
  safe_connect <- safely(~ do.call(dbConnect, c(list(RPostgres::Postgres()), params)))

  result <- safe_connect()

  if (is.null(result$error)) {
    message("✓ Connected to ", params$dbname, " database")
    return(result$result)
  } else {
    message("✗ Database connection failed: ", result$error$message)
    message("  Check that PostgreSQL is running and connection parameters are correct")
    return(NULL)
  }
}

#' Test database connection health
test_db_connection <- function() {
  con <- connect_to_db()

  if (!is.null(con)) {
    dbDisconnect(con)
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' Safe database disconnect with error handling
disconnect_db <- function(con) {
  if (!is.null(con)) {
    safe_disconnect <- possibly(~ {
      # Check if connection is still valid before attempting to close
      if (dbIsValid(con)) {
        dbDisconnect(con)
        message("✓ Database connection closed")
        return(TRUE)
      } else {
        message("Connection already closed/invalid")
        return(TRUE)
      }
    }, otherwise = NULL, quiet = FALSE)

    result <- safe_disconnect()
    if (is.null(result)) {
      message("Warning: Error closing database connection")
    }
  }
}
