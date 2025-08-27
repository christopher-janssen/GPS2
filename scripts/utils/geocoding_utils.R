# scripts/utils/geocoding_utils.R
#' geocoding response processing utilities

#' Extract address component with fallback options
extract_address_component <- function(address, primary_key, fallback_keys = NULL) {
  # Try primary key first
  if (!is.null(address[[primary_key]]) && address[[primary_key]] != "") {
    return(address[[primary_key]])
  }
  
  # Try fallback keys
  for (fallback in fallback_keys) {
    if (!is.null(address[[fallback]]) && address[[fallback]] != "") {
      return(address[[fallback]])
    }
  }
  
  return(NA)
}

#' Process Nominatim API response into standardized format
process_nominatim_response <- function(response) {
  if (is.null(response$display_name) || response$display_name == "") {
    return(NULL)
  }
  
  address <- response$address
  
  processed <- list(
    display_name = response$display_name,
    house_number = extract_address_component(address, "house_number"),
    road = extract_address_component(address, "road"),
    neighbourhood = extract_address_component(address, "neighbourhood", "suburb"),
    city = extract_address_component(address, "city", c("town", "village")),
    county = extract_address_component(address, "county"),
    state = extract_address_component(address, "state"),
    postcode = extract_address_component(address, "postcode"),
    country = extract_address_component(address, "country"),
    place_type = extract_address_component(response, "type", "class"),
    osm_type = extract_address_component(response, "osm_type"),
    osm_id = as.numeric(response$osm_id %||% NA),
    geocoding_confidence = as.numeric(response$importance %||% 0.5),
    geocoding_method = "NOMINATIM"
  )
  
  return(processed)
}

#' Build geocoding insert SQL with processed response
build_geocoding_insert_sql <- function() {
  "
  INSERT INTO gps2.cluster_geocoding 
    (cluster_id, subid, display_name, house_number, road, neighbourhood, 
     city, county, state, postcode, country, place_type, osm_type, osm_id,
     geocoding_confidence, geocoding_method)
  VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15, $16)
  ON CONFLICT (subid, cluster_id) 
  DO UPDATE SET
    display_name = EXCLUDED.display_name,
    processed_at = CURRENT_TIMESTAMP;
  "
}

#' Convert processed response to parameter list for SQL
geocoding_response_to_params <- function(processed, cluster_id, subid) {
  list(
    cluster_id, subid, processed$display_name,
    processed$house_number, processed$road, processed$neighbourhood,
    processed$city, processed$county, processed$state, 
    processed$postcode, processed$country, processed$place_type,
    processed$osm_type, processed$osm_id,
    processed$geocoding_confidence, processed$geocoding_method
  )
}

#' Test Nominatim connection with retry logic
test_nominatim_with_retry <- function(nominatim_url = NULL, max_retries = 3) {
  if (is.null(nominatim_url)) {
    nominatim_url <- get_config("geocoding", "nominatim_url")
  }
  
  for (attempt in 1:max_retries) {
    tryCatch({
      # Test status endpoint
      status_response <- GET(paste0(nominatim_url, "/status.php"))
      if (status_code(status_response) != 200) {
        stop("Service not accessible")
      }
      
      # Test reverse geocoding
      test_url <- paste0(nominatim_url, "/reverse?format=json&lat=43.074713&lon=-89.384373")
      test_response <- GET(test_url)
      
      if (status_code(test_response) == 200) {
        cat("✓ Nominatim service operational\n")
        return(TRUE)
      }
      
    }, error = function(e) {
      if (attempt < max_retries) {
        cat("Attempt", attempt, "failed, retrying...\n")
        Sys.sleep(2)
      } else {
        cat("✗ Nominatim connection failed after", max_retries, "attempts\n")
        return(FALSE)
      }
    })
  }
  
  return(FALSE)
}