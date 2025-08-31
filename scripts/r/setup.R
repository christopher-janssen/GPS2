# scripts/r/setup.R

# conflict policy
options(conflicts.policy = "depends.ok")

# package master-list
library(tidyverse)
library(here, include.only = "here")
library(DBI)
library(RPostgres)
library(sf)
library(httr)
library(jsonlite, exclude = "flatten")
library(geosphere, include.only = "distHaversine")
library(leaflet)

# source `format_path` function
devtools::source_url("https://github.com/jjcurtin/lab_support/blob/main/format_path.R?raw=true")

# set file paths
path_shared <- format_path(str_c("studydata/risk/data_processed/shared"))
path_processed <- format_path(str_c("studydata/risk/data_processed/gps2/data"))
path_gps <- format_path(str_c("studydata/risk/data_processed/gps2"))

# database configuration
GPS_DB_PARAMS <- list(
  host = "localhost",
  port = 5433,
  dbname = "gps_analysis",
  user = "postgres",
  password = "postgres"
)

