#----------------------------------------------------------#
#
#
#           Fossil pollen data from Scandinavia
#
#             Explore the data compilation
#
#
#                       O. Mottl
#                         2025
#
#----------------------------------------------------------#

# Helper script to explore the FOSSILPOL workflow for
#   PALEOVERSE workshop 27.03.2025


#----------------------------------------------------------#
# 1. Set up -----
#----------------------------------------------------------#

library(here)

# Load configuration
source(
  here::here("R/00_Config_file.R")
)

#----------------------------------------------------------#
# 2. Outputs -----
#----------------------------------------------------------#

## 2.1. Data Assembly

# load
data_assembly <-
  RUtilpol::get_latest_file(
    file_name = "data_assembly",
    dir = paste0(
      data_storage_path, # [config_criteria]
      "/Outputs/Data/"
    )
  )

# check the size and column names
dplyr::glimpse(data_assembly)

# Take a look inside of RStudio
View(data_assembly)
