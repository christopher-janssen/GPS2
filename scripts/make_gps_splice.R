# make_gps_splice.R
# Author: Christopher Janssen
# Date: 2025-07-01

# load libraries
library(tidyverse)
library(tidymodels, exclude = c("discard", "col_factor"))
options(conflicts.policy = "depends.ok")

# source lab_support
devtools::source_url("https://github.com/jjcurtin/lab_support/blob/main/format_path.R?raw=true")

# file paths
path_shared <- format_path(str_c("studydata/risk/data_processed/shared"))
path_processed <- format_path(str_c("studydata/risk/data_processed/gps2"))

# read in data
gps_full <- read_csv(here::here(path_shared, "gps.csv"), show_col_types = FALSE)

# sample a few participant IDs to get complete data for
set.seed(123)  
selected_subids <- gps_full |> 
  distinct(subid) |> 
  slice_sample(n = 5) |>  
  pull(subid)

# get ALL GPS data for these selected participants
gps_splice <- gps_full |> 
  filter(subid %in% selected_subids) |> 
  select(subid, lat, lon, time)

# write out
write_csv(gps_splice, here::here(path_processed, "gps2_splice.csv"))