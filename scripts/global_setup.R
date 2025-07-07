# global_setup.R

options(conflicts.policy = "depends.ok")
library(tidyverse)
devtools::source_url("https://github.com/jjcurtin/lab_support/blob/main/format_path.R?raw=true")

# file paths
path_shared <- format_path(str_c("studydata/risk/data_processed/shared"))
path_processed <- format_path(str_c("studydata/risk/data_processed/gps2"))