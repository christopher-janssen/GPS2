# scripts/utils/address_matching.R
#' Robust address matching utilities for ground truth validation

#' Normalize address string for comparison
normalize_address <- function(address) {
  if (is.na(address) || is.null(address) || address == "") {
    return("")
  }
  
  address %>%
    # Convert to lowercase
    str_to_lower() %>%
    # Remove common address suffixes/prefixes that vary
    str_remove_all("\\b(street|st|avenue|ave|road|rd|drive|dr|lane|ln|way|blvd|boulevard|circle|cir|court|ct|place|pl|suite|ste|apt|apartment|#)\\b") %>%
    # Remove directional indicators
    str_remove_all("\\b(north|south|east|west|n|s|e|w|ne|nw|se|sw|northeast|northwest|southeast|southwest)\\b") %>%
    # Remove zip codes
    str_remove_all("\\b\\d{5}(-\\d{4})?\\b") %>%
    # Remove state abbreviations (assuming US)
    str_remove_all("\\b[A-Z]{2}\\b") %>%
    # Remove extra punctuation and whitespace
    str_remove_all("[,\\.]") %>%
    str_squish() %>%
    # Remove common words that might differ
    str_remove_all("\\b(the|and|&)\\b")
}

#' Extract key address components for matching
extract_address_components <- function(address) {
  if (is.na(address) || is.null(address) || address == "") {
    return(list(numbers = character(0), words = character(0)))
  }
  
  # Extract street numbers
  numbers <- str_extract_all(address, "\\b\\d+\\b")[[1]]
  
  # Extract meaningful words (after normalization)
  normalized <- normalize_address(address)
  words <- str_split(normalized, "\\s+")[[1]]
  words <- words[nchar(words) >= 3] # Filter short words
  
  list(numbers = numbers, words = words)
}

#' Calculate similarity score between two addresses
calculate_address_similarity <- function(addr1, addr2, method = "jaccard") {
  if (is.na(addr1) || is.na(addr2) || addr1 == "" || addr2 == "") {
    return(0)
  }
  
  components1 <- extract_address_components(addr1)
  components2 <- extract_address_components(addr2)
  
  # Street number matching (high weight)
  number_match <- length(intersect(components1$numbers, components2$numbers)) > 0
  number_score <- if(number_match) 0.4 else 0
  
  # Word similarity
  if (method == "jaccard") {
    all_words1 <- components1$words
    all_words2 <- components2$words
    
    if (length(all_words1) == 0 && length(all_words2) == 0) {
      word_score <- 0.6
    } else if (length(all_words1) == 0 || length(all_words2) == 0) {
      word_score <- 0
    } else {
      intersection <- length(intersect(all_words1, all_words2))
      union_size <- length(union(all_words1, all_words2))
      word_score <- (intersection / union_size) * 0.6
    }
  } else {
    # Simple normalized string distance
    norm1 <- normalize_address(addr1)
    norm2 <- normalize_address(addr2)
    
    if (norm1 == "" && norm2 == "") {
      word_score <- 0.6
    } else if (norm1 == "" || norm2 == "") {
      word_score <- 0
    } else {
      # Using adist for edit distance
      distance <- adist(norm1, norm2)[1,1]
      max_len <- max(nchar(norm1), nchar(norm2))
      word_score <- (1 - distance/max_len) * 0.6
    }
  }
  
  return(number_score + word_score)
}

#' Find best matching addresses from geocoded results
match_ground_truth_addresses <- function(ground_truth_df, geocoded_df, 
                                        gt_address_col = "address", 
                                        geocoded_address_col = "display_name",
                                        subid_col = "subid",
                                        similarity_threshold = 0.3) {
  
  # Ensure required columns exist
  required_gt_cols <- c(subid_col, gt_address_col)
  required_geo_cols <- c(subid_col, geocoded_address_col)
  
  if (!all(required_gt_cols %in% names(ground_truth_df))) {
    stop("Ground truth data missing required columns: ", 
         paste(setdiff(required_gt_cols, names(ground_truth_df)), collapse = ", "))
  }
  
  if (!all(required_geo_cols %in% names(geocoded_df))) {
    stop("Geocoded data missing required columns: ", 
         paste(setdiff(required_geo_cols, names(geocoded_df)), collapse = ", "))
  }
  
  # Prepare results
  matches <- tibble()
  
  # Process each participant
  for (participant in unique(ground_truth_df[[subid_col]])) {
    gt_participant <- ground_truth_df %>% filter(.data[[subid_col]] == participant)
    geo_participant <- geocoded_df %>% filter(.data[[subid_col]] == participant)
    
    if (nrow(geo_participant) == 0) {
      # No geocoded data for this participant
      participant_matches <- gt_participant %>%
        mutate(
          matched_geocoded_address = NA_character_,
          similarity_score = 0,
          match_quality = "no_geocoded_data",
          cluster_id = NA_integer_
        )
    } else {
      # Calculate similarities for all combinations
      participant_matches <- tibble()
      
      for (i in 1:nrow(gt_participant)) {
        gt_address <- gt_participant[[gt_address_col]][i]
        
        similarities <- map_dbl(geo_participant[[geocoded_address_col]], 
                               ~calculate_address_similarity(gt_address, .x))
        
        best_match_idx <- which.max(similarities)
        best_score <- similarities[best_match_idx]
        
        match_quality <- case_when(
          best_score >= 0.7 ~ "high_confidence",
          best_score >= similarity_threshold ~ "medium_confidence", 
          TRUE ~ "low_confidence"
        )
        
        match_row <- gt_participant[i, ] %>%
          mutate(
            matched_geocoded_address = if(best_score >= similarity_threshold) 
              geo_participant[[geocoded_address_col]][best_match_idx] else NA_character_,
            similarity_score = best_score,
            match_quality = match_quality,
            cluster_id = if(best_score >= similarity_threshold) 
              geo_participant$cluster_id[best_match_idx] else NA_integer_
          )
        
        participant_matches <- bind_rows(participant_matches, match_row)
      }
    }
    
    matches <- bind_rows(matches, participant_matches)
  }
  
  return(matches)
}

#' Generate matching summary statistics
summarize_address_matching <- function(matched_results) {
  summary <- matched_results %>%
    group_by(match_quality) %>%
    summarise(
      count = n(),
      avg_similarity = mean(similarity_score, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      percentage = round(count / sum(count) * 100, 1)
    )
  
  total_matched <- sum(summary$count[summary$match_quality %in% c("high_confidence", "medium_confidence")])
  match_rate <- round(total_matched / sum(summary$count) * 100, 1)
  
  cat("Address Matching Summary:\n")
  print(summary)
  cat("\nOverall match rate:", match_rate, "%\n")
  
  invisible(summary)
}

#' Validate matched addresses interactively (for manual review)
review_uncertain_matches <- function(matched_results, max_review = 20) {
  uncertain <- matched_results %>%
    filter(match_quality == "medium_confidence") %>%
    arrange(desc(similarity_score)) %>%
    head(max_review)
  
  if (nrow(uncertain) == 0) {
    cat("No uncertain matches to review\n")
    return(matched_results)
  }
  
  cat("Reviewing", nrow(uncertain), "uncertain matches...\n\n")
  
  for (i in 1:nrow(uncertain)) {
    cat("Match", i, "of", nrow(uncertain), "\n")
    cat("Ground Truth:", uncertain$address[i], "\n")
    cat("Geocoded:    ", uncertain$matched_geocoded_address[i], "\n")
    cat("Similarity:  ", round(uncertain$similarity_score[i], 3), "\n")
    cat("Accept match? (y/n/q to quit): ")
    
    response <- readline()
    if (tolower(response) == "q") break
    if (tolower(response) == "y") {
      matched_results$match_quality[matched_results$subid == uncertain$subid[i] & 
                                   matched_results$address == uncertain$address[i]] <- "manual_confirmed"
    } else {
      matched_results$matched_geocoded_address[matched_results$subid == uncertain$subid[i] & 
                                             matched_results$address == uncertain$address[i]] <- NA_character_
      matched_results$match_quality[matched_results$subid == uncertain$subid[i] & 
                                   matched_results$address == uncertain$address[i]] <- "manual_rejected"
    }
    cat("\n")
  }
  
  return(matched_results)
}