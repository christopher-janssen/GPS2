# scripts/r/recode_poi_categories.R
#
# Changes:
# 1. Split recovery_facility → social_facility (new) + healthcare (expanded)
# 2. Move brewery from alcohol_retail → alcohol_venue
# 3. Split restaurant → restaurant (reduced) + fast_food (new)

# Setup ----
source(here::here("scripts/r/setup.R"))

# Load data ----
gps_path <- str_c(path_shared, "/gps_enriched_all.csv")
gps_enriched <- read_csv(gps_path, show_col_types = FALSE)

# Verify required columns exist
required_cols <- c("class", "type", "poi_category")
if (!all(required_cols %in% names(gps_enriched))) {
  stop("Missing required columns: ",
       paste(setdiff(required_cols, names(gps_enriched)), collapse = ", "))
}

# Current state ----
tibble(
  section = "CURRENT POI CATEGORIES",
  total_rows = nrow(gps_enriched),
  with_poi = sum(!is.na(gps_enriched$poi_category))
) |>
  knitr::kable()

gps_enriched |>
  count(poi_category, sort = TRUE) |>
  knitr::kable()

# Recode POI categories ----
gps_enriched <- gps_enriched |>
  mutate(
    poi_category_new = case_when(
      # NEW: social_facility (split from recovery_facility)
      class == "amenity" & type == "social_facility" ~ "social_facility",

      # EXPANDED: healthcare (merge recovery + healthcare)
      (class == "healthcare" & type %in% c("clinic", "centre")) |
      (class == "amenity" & type %in% c("hospital", "clinic", "doctors", "pharmacy")) ~ "healthcare",

      # EXPANDED: alcohol_venue (add brewery)
      (class == "amenity" & type %in% c("bar", "pub", "nightclub", "biergarten")) |
      (class == "craft" & type == "brewery") ~ "alcohol_venue",

      # REDUCED: alcohol_retail (remove brewery)
      class == "shop" & type %in% c("alcohol", "wine") ~ "alcohol_retail",

      # NEW: fast_food (split from restaurant)
      class == "amenity" & type %in% c("fast_food", "food_court") ~ "fast_food",

      # REDUCED: restaurant (remove fast_food/food_court)
      class == "amenity" & type %in% c("restaurant", "cafe") ~ "restaurant",

      # UNCHANGED: park
      class == "leisure" & type %in% c("park", "nature_reserve", "garden", "recreation_ground") ~ "park",

      # UNCHANGED: grocery
      class == "shop" & type %in% c("convenience", "supermarket", "general", "grocery") ~ "grocery",

      # Preserve NAs for non-POI points
      is.na(poi_category) ~ NA_character_,

      # Catch any unexpected combinations
      TRUE ~ NA_character_
    )
  )

# Validation ----
tibble(
  section = "VALIDATION CHECKS",
  old_poi_count = sum(!is.na(gps_enriched$poi_category)),
  new_poi_count = sum(!is.na(gps_enriched$poi_category_new)),
  unexpected_nas = sum(is.na(gps_enriched$poi_category_new) & !is.na(gps_enriched$poi_category))
) |>
  knitr::kable()

# Compare old vs new categories
comparison <- gps_enriched |>
  filter(!is.na(poi_category)) |>
  count(poi_category, poi_category_new) |>
  arrange(poi_category, poi_category_new)

tibble(
  section = "OLD → NEW CATEGORY MAPPING"
) |>
  knitr::kable()

comparison |>
  knitr::kable()

# Show changes summary
changes_summary <- gps_enriched |>
  filter(!is.na(poi_category)) |>
  mutate(
    changed = poi_category != poi_category_new
  ) |>
  summarise(
    total_poi_points = n(),
    changed_categories = sum(changed),
    unchanged_categories = sum(!changed),
    pct_changed = round(sum(changed) / n() * 100, 2)
  )

tibble(
  section = "CHANGES SUMMARY"
) |>
  knitr::kable()

changes_summary |>
  knitr::kable()

# save ----

# Replace poi_category column
gps_enriched <- gps_enriched |>
  select(-poi_category) |>
  rename(poi_category = poi_category_new)

# Write to original file
write_csv(gps_enriched, gps_path)

tibble(
  section = "FILE UPDATED",
  file = "gps_enriched_all.csv",
  file_size_mb = round(file.info(gps_path)$size / 1024^2, 2)
) |>
  knitr::kable()

# Final summary ----
tibble(
  section = "FINAL POI CATEGORIES (NEW)"
) |>
  knitr::kable()

gps_enriched |>
  count(poi_category, sort = TRUE) |>
  knitr::kable()

# Verify expected outcomes
tibble(
  section = "VERIFICATION",
  recovery_facility_count = sum(gps_enriched$poi_category == "recovery_facility", na.rm = TRUE),
  social_facility_count = sum(gps_enriched$poi_category == "social_facility", na.rm = TRUE),
  fast_food_count = sum(gps_enriched$poi_category == "fast_food", na.rm = TRUE)
) |>
  knitr::kable()

