# scripts/utils/query_builders.R
#' SQL query building utilities for GPS2

#' Build participant filter clause
# Paste these directly into your R console to override the broken functions:

build_participant_filter <- function(participant_ids, table_alias = "", column_name = "subid") {
  if (is.null(participant_ids) || length(participant_ids) == 0) return("")
  
  prefix <- if (table_alias != "") paste0(table_alias, ".") else ""
  participant_list <- paste(participant_ids, collapse = ",")
  return(paste0(" AND ", prefix, column_name, " IN (", participant_list, ")"))
}

build_geocoding_filter <- function(include_failed = FALSE, table_alias = "cg") {
  if (include_failed) return("")
  
  prefix <- if (table_alias != "") paste0(table_alias, ".") else ""
  return(paste0(" AND ", prefix, "display_name IS NOT NULL AND ", 
                prefix, "display_name != 'No address found'"))
}

build_cluster_query <- function(participant_ids = NULL, include_geocoding = TRUE, 
                                include_failed_geocoding = FALSE, order_by = "total_duration_hours DESC") {
  
  # Base SELECT clause
  base_select <- "
    SELECT 
      lc.cluster_id, lc.subid, lc.lat, lc.lon, lc.n_points,
      lc.first_visit, lc.last_visit, lc.total_visits,
      lc.total_duration_hours, lc.unique_days"
  
  # Add geocoding columns if requested
  if (include_geocoding) {
    geocoding_select <- ",
      cg.display_name, cg.road, cg.city, cg.state, cg.postcode,
      cg.geocoding_confidence, cg.place_type, cg.geocoding_method"
    base_select <- paste0(base_select, geocoding_select)
  }
  
  # FROM clause with optional JOIN
  from_clause <- "
    FROM gps2.location_clusters lc"
  
  if (include_geocoding) {
    from_clause <- paste0(from_clause, "
    LEFT JOIN gps2.cluster_geocoding cg 
      ON lc.subid = cg.subid AND lc.cluster_id = cg.cluster_id")
  }
  
  # WHERE clause - FIXED with proper spacing
  where_clause <- "
    WHERE 1=1"
  
  if (!is.null(participant_ids)) {
    where_clause <- paste0(where_clause, " AND lc.subid IN (", paste(participant_ids, collapse = ","), ")")
  }
  
  if (include_geocoding && !include_failed_geocoding) {
    where_clause <- paste0(where_clause, " AND cg.display_name IS NOT NULL AND cg.display_name != 'No address found'")
  }
  
  # ORDER BY clause
  order_clause <- paste0("
    ORDER BY lc.", order_by)
  
  return(paste(base_select, from_clause, where_clause, order_clause))
}

#' Build GPS data query with filters
build_gps_query <- function(participant_ids = NULL, movement_state = NULL, 
                            start_date = NULL, end_date = NULL) {
  
  base_query <- "
    SELECT subid, lat, lon, dttm_obs, dist, duration, speed, movement_state
    FROM gps2.gps_stationary_points"
  
  where_clause <- "
    WHERE 1=1"  # Added newline and spaces
  where_clause <- paste0(where_clause, build_participant_filter(participant_ids))
  where_clause <- paste0(where_clause, build_date_filter(start_date, end_date))
  
  if (!is.null(movement_state)) {
    where_clause <- paste0(where_clause, " AND movement_state = '", movement_state, "'")
  }
  
  order_clause <- "
    ORDER BY dttm_obs"
  
  return(paste(base_query, where_clause, order_clause))
}