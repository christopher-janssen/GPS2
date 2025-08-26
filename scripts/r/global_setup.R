# scripts/r/global_setup.R
options(conflicts.policy = "depends.ok")
library(tidyverse)
library(here)

# source `format_path` function
devtools::source_url("https://github.com/jjcurtin/lab_support/blob/main/format_path.R?raw=true")

# set file paths
path_shared <- format_path(str_c("studydata/risk/data_processed/shared"))
path_processed <- format_path(str_c("studydata/risk/data_processed/gps2/data"))

# load configuration
source(here("config", "gps2_config.R"))

# load all utility functions
source(here("utils", "load_utils.R"))
load_gps2_utils()

