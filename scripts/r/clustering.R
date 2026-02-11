# scripts/r/clustering.R
#
# Shared helper functions for GPS clustering analyses.
# Dependencies: geosphere, dbscan, clue (for bipartite matching)
#
# Usage: source(here::here("scripts/r/clustering.R"))

# --- Stay Detection -----------------------------------------------------------

#' Detect stationary stays from GPS point data.
#'
#' @param gps_data  Data frame with columns: subid, time_local, lat, lon.
#' @param dist_threshold  Meters; movement beyond this starts a new stay.
#' @param time_threshold  Minutes; minimum dwell to qualify as a stay.
#' @param max_gap  Minutes; temporal gap that forces a new stay.
#' @param max_hours  Hours; stays longer than this are dropped.
#' @return Data frame: stay_id, subid, start_time, end_time, dwell_mins,
#'   n_points, lat, lon.
detect_stays <- function(gps_data,
                         dist_threshold = 50,
                         time_threshold = 10,
                         max_gap = 60,
                         max_hours = 12) {
  data <- gps_data |>
    dplyr::group_by(subid) |>
    dplyr::mutate(
      dist_from_prev = dplyr::if_else(
        dplyr::row_number() == 1,
        NA_real_,
        geosphere::distHaversine(
          cbind(dplyr::lag(lon), dplyr::lag(lat)),
          cbind(lon, lat)
        )
      ),
      time_gap_mins = dplyr::if_else(
        dplyr::row_number() == 1,
        NA_real_,
        as.numeric(difftime(time_local, dplyr::lag(time_local),
                            units = "mins"))
      )
    ) |>
    dplyr::ungroup()

  data <- data |>
    dplyr::group_by(subid) |>
    dplyr::mutate(
      is_new_stay = dplyr::row_number() == 1 |
        dist_from_prev > dist_threshold |
        time_gap_mins > max_gap,
      stay_id_subject = cumsum(is_new_stay)
    ) |>
    dplyr::ungroup()

  stays_raw <- data |>
    dplyr::group_by(subid, stay_id_subject) |>
    dplyr::summarize(
      start_time = min(time_local),
      end_time = max(time_local),
      dwell_mins = as.numeric(difftime(max(time_local),
                                       min(time_local), units = "mins")),
      n_points = dplyr::n(),
      lat = median(lat),
      lon = median(lon),
      .groups = "drop"
    )

  stays_raw |>
    dplyr::filter(dwell_mins >= time_threshold &
                    dwell_mins <= max_hours * 60) |>
    dplyr::mutate(stay_id = dplyr::row_number()) |>
    dplyr::select(stay_id, subid, start_time, end_time, dwell_mins,
                  n_points, lat, lon)
}


# --- DBSCAN Clustering -------------------------------------------------------

#' Run DBSCAN on a coordinate matrix using Haversine distances.
#'
#' @param coords  Two-column matrix (lon, lat).
#' @param eps  Epsilon in meters.
#' @param min_pts  Minimum points per cluster.
#' @return List with cluster (integer vector), n_clusters, n_noise.
run_dbscan <- function(coords, eps = 50, min_pts = 3) {
  if (nrow(coords) < 2) {
    return(list(
      cluster = rep(1L, nrow(coords)),
      n_clusters = 1L,
      n_noise = 0L
    ))
  }

  dist_mat <- geosphere::distm(coords, coords, fun = geosphere::distHaversine)
  result <- dbscan::dbscan(as.dist(dist_mat), eps = eps, minPts = min_pts)

  list(
    cluster = result$cluster,
    n_clusters = max(result$cluster),
    n_noise = sum(result$cluster == 0)
  )
}


# --- Cluster Centroids --------------------------------------------------------

#' Extract cluster centroids with summary statistics.
#'
#' @param stays_df  Data frame of stays with lat, lon, dwell_mins, start_time,
#'   and a cluster assignment column.
#' @param cluster_col  Name of the cluster column (string).
#' @return Data frame: cluster_id, lat, lon, total_dwell_hrs, n_stays, n_days.
get_centroids <- function(stays_df, cluster_col) {
  stays_df |>
    dplyr::filter(.data[[cluster_col]] > 0) |>
    dplyr::group_by(.data[[cluster_col]]) |>
    dplyr::summarize(
      lat = median(lat),
      lon = median(lon),
      total_dwell_hrs = sum(dwell_mins) / 60,
      n_stays = dplyr::n(),
      n_days = dplyr::n_distinct(as.Date(start_time)),
      .groups = "drop"
    ) |>
    dplyr::rename(cluster_id = 1)
}


# --- Deduplicate Locations ----------------------------------------------------

#' Merge nearby locations using single-linkage clustering (DBSCAN minPts = 1).
#'
#' @param df  Data frame with lat, lon, and intake columns.
#' @param threshold_m  Merge distance in meters.
#' @return Data frame: location_id, lat, lon, n_reports, intake.
dedupe_locations <- function(df, threshold_m = 50) {
  if (nrow(df) < 2) {
    return(df |> dplyr::mutate(location_id = 1L))
  }

  coords <- cbind(df$lon, df$lat)
  dist_mat <- geosphere::distm(coords, coords, fun = geosphere::distHaversine)
  clusters <- dbscan::dbscan(as.dist(dist_mat), eps = threshold_m, minPts = 1)

  df |>
    dplyr::mutate(location_id = clusters$cluster) |>
    dplyr::group_by(location_id) |>
    dplyr::summarize(
      lat = median(lat),
      lon = median(lon),
      n_reports = dplyr::n(),
      intake = max(intake),
      .groups = "drop"
    )
}


# --- Spatial Matching ---------------------------------------------------------

#' Bipartite (Hungarian) matching between two sets of locations.
#'
#' Enforces one-to-one assignment. A pair is eligible only if within
#' threshold_m meters.
#'
#' @param set_a  Data frame with lon, lat (e.g. DBSCAN centroids).
#' @param set_b  Data frame with lon, lat (e.g. legacy locations).
#' @param threshold_m  Maximum distance in meters for a valid match.
#' @return List: n_a, n_b, n_matched, matched_a (indices), matched_b (indices),
#'   unmatched_a, unmatched_b.
match_locations_bipartite <- function(set_a, set_b, threshold_m = 100) {
  na <- nrow(set_a)
  nb <- nrow(set_b)

  if (na == 0 || nb == 0) {
    return(list(
      n_a = na, n_b = nb, n_matched = 0L,
      matched_a = integer(0), matched_b = integer(0),
      unmatched_a = seq_len(na), unmatched_b = seq_len(nb)
    ))
  }

  coords_a <- cbind(set_a$lon, set_a$lat)
  coords_b <- cbind(set_b$lon, set_b$lat)
  dist_mat <- geosphere::distm(coords_a, coords_b,
                                fun = geosphere::distHaversine)

  eligible <- ifelse(dist_mat <= threshold_m, 1, 0)

  if (na <= nb) {
    assignment <- clue::solve_LSAP(eligible, maximum = TRUE)
    matched_pairs <- cbind(seq_len(na), as.integer(assignment))
  } else {
    assignment <- clue::solve_LSAP(t(eligible), maximum = TRUE)
    matched_pairs <- cbind(as.integer(assignment), seq_len(nb))
  }

  valid <- eligible[matched_pairs] == 1
  matched_a_idx <- matched_pairs[valid, 1]
  matched_b_idx <- matched_pairs[valid, 2]

  list(
    n_a = na,
    n_b = nb,
    n_matched = sum(valid),
    matched_a = matched_a_idx,
    matched_b = matched_b_idx,
    unmatched_a = setdiff(seq_len(na), matched_a_idx),
    unmatched_b = setdiff(seq_len(nb), matched_b_idx)
  )
}


#' Nearest-neighbor matching between two sets of locations (many-to-many).
#'
#' Allows multiple items from one set to match the same item in the other.
#' Useful for quick capture-rate estimates but inflates metrics when locations
#' are spatially dense.
#'
#' @param set_a  Data frame with lon, lat.
#' @param set_b  Data frame with lon, lat.
#' @param threshold_m  Maximum distance in meters for a valid match.
#' @return List: n_a, n_b, n_a_matched, n_b_matched.
match_locations_nn <- function(set_a, set_b, threshold_m = 100) {
  na <- nrow(set_a)
  nb <- nrow(set_b)

  if (na == 0 || nb == 0) {
    return(list(n_a = na, n_b = nb, n_a_matched = 0L, n_b_matched = 0L))
  }

  coords_a <- cbind(set_a$lon, set_a$lat)
  coords_b <- cbind(set_b$lon, set_b$lat)
  dist_mat <- geosphere::distm(coords_a, coords_b,
                                fun = geosphere::distHaversine)

  n_a_matched <- sum(apply(dist_mat, 1, min) <= threshold_m)
  n_b_matched <- sum(apply(dist_mat, 2, min) <= threshold_m)

  list(n_a = na, n_b = nb,
       n_a_matched = n_a_matched, n_b_matched = n_b_matched)
}


# --- Temporal Filtering -------------------------------------------------------

#' Filter legacy locations to those known by a given date.
#'
#' Intake locations (intake == 1) are always retained. Algorithm-derived
#' locations are kept only if their utc_datetime is on or before the
#' window end date.
#'
#' @param locs  Data frame with intake and utc_datetime columns.
#' @param window_end_date  Date object; end of the cumulative window.
#' @return Filtered data frame.
temporally_filter_legacy <- function(locs, window_end_date) {
  window_end_dt <- as.POSIXct(
    paste0(window_end_date, " 23:59:59"), tz = "UTC"
  )
  locs |>
    dplyr::filter(intake == 1 | utc_datetime <= window_end_dt)
}


# --- Filter Unvisitable Locations ---------------------------------------------

#' Drop locations with no nearby GPS stays.
#'
#' A location is "visitable" if at least one stay point falls within
#' threshold_m meters. Locations the participant never visited cannot
#' be discovered by any clustering algorithm.
#'
#' @param locations  Data frame with lat, lon.
#' @param stays  Data frame with lat, lon (the stays to check against).
#' @param threshold_m  Proximity threshold in meters.
#' @return Filtered data frame (rows of locations that have nearby stays).
filter_visitable <- function(locations, stays, threshold_m = 100) {
  if (nrow(locations) == 0 || nrow(stays) == 0) {
    return(locations[0, ])
  }

  stay_coords <- cbind(stays$lon, stays$lat)

  has_nearby <- purrr::map_lgl(seq_len(nrow(locations)), function(i) {
    loc_coord <- c(locations$lon[i], locations$lat[i])
    min(geosphere::distHaversine(loc_coord, stay_coords)) <= threshold_m
  })

  locations |> dplyr::filter(has_nearby)
}
