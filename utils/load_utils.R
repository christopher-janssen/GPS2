# utils/load_utils.R
#' Load all GPS2 utility functions
#'
#' @description
#' Sources all utility files in the utils/ directory except this loader file.
#' Uses purrr::walk() for functional iteration with error handling.
#' Provides console feedback on loading success/failure.
#'
#' @details
#' This function automatically discovers and loads all .R files in the utils/
#' directory. It excludes itself (load_utils.R) to prevent circular loading.
#' Loading failures are caught and reported but don't stop the process.
#'
#' @return Invisible NULL. Called for side effects (sourcing files).
#'
#' @examples
#' \dontrun{
#' # Load all GPS2 utilities
#' load_gps2_utils()
#' }
#'
#' @export
load_gps2_utils <- function() {
  utils_dir <- here::here("utils")
  
  # Source all utility files except this loader
  util_files <- list.files(utils_dir, pattern = "*.R", full.names = TRUE)
  util_files <- util_files[!grepl("load_utils.R", util_files)]
  
  util_files |>
    walk(~ {
      tryCatch({
        source(.x)
        cat("✓ Loaded:", basename(.x), "\n")
      }, error = function(e) {
        cat("✗ Failed to load:", basename(.x), "-", e$message, "\n")
      })
    })
  
  cat("GPS2 utilities loaded successfully!\n")
}