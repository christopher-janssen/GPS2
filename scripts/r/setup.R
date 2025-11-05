# scripts/r/setup.R

# conflict policy
options(conflicts.policy = "depends.ok")

# package master-list
library(tidyverse)
library(here, include.only = "here")
library(DBI)
library(RPostgres)
library(sf)
library(lubridate)
library(dbscan, include.only = "dbscan")

# source `format_path` function
source("https://raw.githubusercontent.com/jjcurtin/lab_support/main/format_path.R")

# set file paths
path_shared <- file.path(str_c("/Volumes/jjcurtin/studydata/risk/data_processed/shared"))
path_processed <- file.path(str_c("/Volumes/jjcurtin/studydata/risk/data_processed"))
path_gps2 <- file.path(str_c("/Volumes/jjcurtin/studydata/risk/data_processed/gps2"))

# database configuration
# For local testing or VM, adjust as needed
GPS_DB_PARAMS <- list(
  host = "localhost",
  port = 5432,
  dbname = "geolocation"
  # user and password omitted for peer authentication on VM
  # add them for local testing if needed
)
