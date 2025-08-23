# utils/db_utils.R
#' Database utility functions for GPS2

#' Execute function within database transaction with proper error handling
with_gps2_transaction <- function(func, error_msg = "Database operation failed") {
  con <- connect_gps2_db()
  tryCatch({
    result <- func(con)
    return(result)
  }, error = function(e) {
    cat(error_msg, ":", e$message, "\n")
    return(NULL)
  }, finally = {
    disconnect_gps2_db(con)
  })
}

#' Batch insert with vectorized SQL for better performance
batch_insert_with_progress <- function(data, insert_func, batch_size = NULL, operation_name = "Processing") {
  if (is.null(batch_size)) batch_size <- get_config("database", "batch_size")
  
  total_inserted <- 0
  total_rows <- nrow(data)
  
  for (i in seq(1, total_rows, batch_size)) {
    end_idx <- min(i + batch_size - 1, total_rows)
    batch <- data[i:end_idx, ]
    
    result <- with_gps2_transaction(function(con) {
      insert_func(con, batch)
      return(nrow(batch))
    }, paste("Failed to insert batch starting at row", i))
    
    if (!is.null(result)) {
      total_inserted <- total_inserted + result
    }
    
    # Progress reporting
    if (i %% (batch_size * 5) == 1 || end_idx == total_rows) {
      cat(operation_name, ":", total_inserted, "of", total_rows, "\n")
    }
  }
  
  return(total_inserted)
}

#' Helper for SQL NULL value handling
coalesce_sql <- function(x) {
  ifelse(is.na(x), "NULL", x)
}