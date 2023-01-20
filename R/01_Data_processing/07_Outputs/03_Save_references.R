#----------------------------------------------------------#
#
#
#                 The FOSSILPOL workflow
#
#                 Save the reference tables
#
#
#   O. Mottl, S. Flantua, K. Bhatta, V. Felde, A. Seddon
#                         2021
#
#----------------------------------------------------------#

# Save reference meta tables


#----------------------------------------------------------#
# 1. Set up -----
#----------------------------------------------------------#

library(here)

# Load configuration
source(
  here::here("R/00_Config_file.R")
)

# set the current environment
current_env <- rlang::current_env()

RUtilpol::output_heading(
  msg = "Saving infomation about data assembly"
)


#----------------------------------------------------------#
# 2. Load data  -----
#----------------------------------------------------------#

data_assembly <-
  RUtilpol::get_latest_file(
    file_name = "data_assembly",
    dir = paste0(
      data_storage_path, # [config_criteria]
      "/Outputs/Data/"
    )
  )

# test the presence of data
RUtilpol::check_if_loaded(
  file_name = "data_assembly",
  env = current_env
)

# load dataset database
project_dataset_database <-
  readr::read_rds(
    paste0(
      data_storage_path, # [config_criteria]
      "/Data/Personal_database_storage/project_dataset_database.rds"
    )
  )


#----------------------------------------------------------#
# 3.  Save data assembly references -----
#----------------------------------------------------------#

# create a palette for coloring outputs
biomes_vec <-
  data_assembly %>%
  tidyr::drop_na(wwf_biome) %>%
  purrr::pluck("wwf_biome") %>%
  unique()

biome_palette <-
  colorRampPalette(
    palette_generic # [config_criteria]
  )(length(biomes_vec)) %>%
  rlang::set_names(
    nm = biomes_vec
  )

RFossilpol::proc_save_references(
  data_source = data_assembly,
  project_database = project_dataset_database,
  user_sel_variables = "wwf_biome", # [USER] Here can user select variables,
  # which have to be present in the  final data assembly
  selected_outputs = c(
    "meta_table",
    "author_table",
    # this trick will include `affiliation_table` only if
    #   data from sources other than Neotoma is present
    switch(isTRUE(use_other_datasource) + 1,
      NULL,
      "affiliation_table"
    ),
    "graphical_summary",
    "reproducibility_bundle"
  ),
  dir = data_storage_path, # [config_criteria]
  grouping_variable = "wwf_biome",
  drop_na = TRUE,
  line_width = 0.5,
  color_palette = biome_palette,
  legend_position = "bottom",
  image_width = 20,
  image_height = 12,
  image_units = "cm"
)
