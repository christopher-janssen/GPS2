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

# set file paths (cross-platform via format_path)
path_shared <- format_path("risk/data_processed/shared")
path_processed <- format_path("risk/data_processed")
path_gps2 <- format_path("risk/data_processed/gps2")
path_terrain <- format_path("risk/data_processed/terrain")

# database configuration
# Uses Unix socket connection for peer authentication on VM
# host parameter omitted to avoid TCP/IP password requirement
GPS_DB_PARAMS <- list(
  port = 5432,
  dbname = "geolocation"
  # user and password omitted for peer authentication
  # add host = "localhost" + credentials for remote connections if needed
)
