#----------------------------------------------------------#
#
#
#           Fossil pollen data from Scandinavia
#
#       Merge chronology outputs, and sort data
#
#
#
#   O. Mottl, S. Flantua, K. Bhatta, V. Felde, A. Seddon
#                         2021
#
#----------------------------------------------------------#

# Load newest Chronology output and join with sorted data.
#   Do check of order of levels between levels, and raw_counts


#----------------------------------------------------------#
# 1. Set up  -----
#----------------------------------------------------------#

library(here)

# Load configuration
source(
  here::here("R/00_Config_file.R")
)

RUtilpol::output_heading(
  msg = "Loading Chronology outputs and merged data"
)


#----------------------------------------------------------#
# 2. Join sorted data an Chronology output  -----
#----------------------------------------------------------#

data_with_chronologies <-
  RFossilpol::chron_merge_results(
    dir = data_storage_path # [config_criteria]
  )


#----------------------------------------------------------#
# 4. Save the data  -----
#----------------------------------------------------------#

RUtilpol::save_latest_file(
  object_to_save = data_with_chronologies,
  dir = paste0(
    data_storage_path, # [config_criteria]
    "/Data/Processed/Data_with_chronologies"
  ),
  prefered_format = "rds",
  use_sha = TRUE
)
