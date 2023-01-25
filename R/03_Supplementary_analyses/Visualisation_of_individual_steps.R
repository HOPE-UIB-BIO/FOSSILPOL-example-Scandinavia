#----------------------------------------------------------#
#
#
#           Fossil pollen data from Scandinavia
#
#             Visualise the data compilation
#
#
#   O. Mottl, S. Flantua, K. Bhatta, V. Felde, A. Seddon
#                         2021
#
#----------------------------------------------------------#

# Create a figures for individual steps of data processing


#----------------------------------------------------------#
# 1. Set up -----
#----------------------------------------------------------#

library(here)

# Load configuration
source(
  here::here("R/00_Config_file.R")
)

current_env <- environment()

# install additional packages
# utils::install.packages("ggpubr")
# utils::install.packages("RColorBrewer")
library(ggpubr)
library(RColorBrewer)

map_margin <- 1.5
map_res <- 0.1

#----------------------------------------------------------#
# 2. Neotoma data download -----
#----------------------------------------------------------#

# load the data
neotoma_meta_samples <-
  RUtilpol::get_latest_file(
    file_name = "neotoma_meta_samples",
    dir = paste0(
      data_storage_path, # [config_criteria]
      "/Data/Processed/Neotoma_processed/Neotoma_meta"
    )
  )

# test the presence of data
RUtilpol::check_if_loaded(
  file_name = "neotoma_meta_samples",
  env = current_env
)

# referecne figure
(
  p0 <-
    RFossilpol::plot_map_of_data(
      data_source = neotoma_meta_samples,
      point_size = (point_size * 1.2), # [config_criteria]
      point_alpha = 0.7,
      point_colour = col_gray_middle, # [config_criteria]
      point_colour_accent = NULL, # [config_criteria]
      col_map_fill = col_gray_light, # [config_criteria]
      col_map_borders = col_gray_middle, # [config_criteria]
      map_data_margin = map_margin,
      text_size = text_size, # [config_criteria]
      line_size = line_size, # [config_criteria]
    ) +
    ggplot2::geom_point(
      size = min(0.1, point_size * 0.5),
      alpha = 1,
      colour = col_gray_dark
    )
)

(
  p1 <-
    p0 +
    ggplot2::geom_point(
      data = neotoma_meta_samples,
      colour = palette_shades[1], # [config_criteria],
      alpha = 0.7,
      size = point_size
    ) +
    ggplot2::geom_rect(
      ggplot2::aes(
        xmin = long_min, # [config_criteria]
        xmax = long_max, # [config_criteria]
        ymin = lat_min, # [config_criteria]
        ymax = lat_max # [config_criteria]
      ),
      fill = NA,
      colour = col_compl_blue, # [config_criteria]
      size = line_size
    ) +
    ggplot2::geom_point(
      size = min(0.1, point_size * 0.5),
      alpha = 1,
      colour = col_gray_dark
    )
  # + ggplot2::ggtitle("Filter by geographical location")
)


#----------------------------------------------------------#
# 3. Neotoma processed -----
#----------------------------------------------------------#

neotoma_processed <-
  RUtilpol::get_latest_file(
    file_name = "neotoma_processed",
    dir = paste0(
      data_storage_path, # [config_criteria]
      "/Data/Processed/Neotoma_processed"
    )
  )

# test the presence of data
RUtilpol::check_if_loaded(
  file_name = "neotoma_processed",
  env = current_env
)

(
  p2 <-
    p0 +
    ggplot2::geom_point(
      data = neotoma_processed,
      colour = palette_shades[2], # [config_criteria],
      alpha = 0.7,
      size = point_size
    ) +
    ggplot2::geom_point(
      size = min(0.1, point_size * 0.5),
      alpha = 1,
      colour = col_gray_dark
    )
  # + ggplot2::ggtitle("Neotoma processed")
)


#----------------------------------------------------------#
# 4. Chronologies-----
#----------------------------------------------------------#

data_with_chronologies <-
  RUtilpol::get_latest_file(
    file_name = "data_with_chronologies",
    dir = paste0(
      data_storage_path, # [config_criteria]
      "/Data/Processed/Data_with_chronologies"
    )
  )

# test the presence of data
RUtilpol::check_if_loaded(
  file_name = "data_with_chronologies",
  env = current_env
)

(
  p3 <-
    p0 +
    ggplot2::geom_point(
      data = data_with_chronologies,
      colour = palette_shades[3], # [config_criteria],
      alpha = 0.7,
      size = point_size
    ) +
    ggplot2::geom_point(
      size = min(0.1, point_size * 0.5),
      alpha = 1,
      colour = col_gray_dark
    )
  # + ggplot2::ggtitle("Data with chronologies")
)


#----------------------------------------------------------#
# 5. Main filtering-----
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

(
  p4 <-
    p0 +
    ggplot2::geom_point(
      data = data_assembly,
      colour = palette_shades[4], # [config_criteria],
      alpha = 0.7,
      size = point_size
    ) +
    ggplot2::geom_point(
      size = min(0.1, point_size * 0.5),
      alpha = 1,
      colour = col_gray_dark
    )
  # + ggplot2::ggtitle("Filtered data")
)


#----------------------------------------------------------#
# 6. Number of sequences -----
#----------------------------------------------------------#

data_step_size <-
  tibble::tibble(
    data_name = c(
      "Neotoma download",
      "Neotoma processed",
      "Data with chronologies",
      "Data filtered"
    ),
    n_seq = c(
      nrow(neotoma_meta_samples),
      nrow(neotoma_processed),
      nrow(data_with_chronologies),
      nrow(data_assembly)
    )
  )

(
  p5 <-
    data_step_size %>%
    ggplot2::ggplot(
      ggplot2::aes(
        x = forcats::fct_reorder(data_name, -n_seq),
        y = 0.5
      )
    ) +
    ggplot2::geom_point(
      ggplot2::aes(
        size = n_seq,
        col = n_seq
      )
    ) +
    ggplot2::geom_label(
      ggplot2::aes(
        y = 0.35,
        label = data_name
      )
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        y = 0.30,
        label = n_seq
      )
    ) +
    ggplot2::scale_size_continuous(
      range = c(10, 30)
    ) +
    ggplot2::scale_colour_gradient(
      trans = "log",
      high = palette_shades[1], # [config_criteria]
      low = palette_shades[5] # [config_criteria]
    ) +
    ggplot2::coord_cartesian(
      ylim = c(0, 1)
    ) +
    ggplot2::guides(
      colour = "none",
      size = "none"
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(
      text = ggplot2::element_text(size = text_size)
    )
)

#----------------------------------------------------------#
# 7. Data filtering plot -----
#----------------------------------------------------------#

p_merge <-
  ggpubr::ggarrange(
    p1 +
      ggpubr::rremove("xylab"),
    p2 +
      ggpubr::rremove("xylab"),
    p3 +
      ggpubr::rremove("xylab"),
    p4 +
      ggpubr::rremove("xylab"),
    nrow = 1,
    ncol = 4,
    labels = LETTERS[1:4]
  ) %>%
  ggpubr::annotate_figure(
    .,
    left = ggpubr::text_grob("Latitude", rot = 90, size = text_size),
    bottom = ggpubr::text_grob("Longitude", size = text_size)
  )

(
  p_fin <-
    ggpubr::ggarrange(
      p_merge,
      p5,
      nrow = 2,
      ncol = 1,
      heights = c(1, 0.3),
      labels = c("", "E")
    )
)

ggplot2::ggsave(
  plot = p_fin,
  filename = paste0(current_dir, "/Outputs/Figures/Supplementary/Data_filtering.pdf"),
  width = 25,
  height = 12,
  units = image_units,
  dpi = image_dpi
)

