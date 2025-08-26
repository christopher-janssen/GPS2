# utils/load_utils.R
#' load all GPS2 utility functions
library(purrr)
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