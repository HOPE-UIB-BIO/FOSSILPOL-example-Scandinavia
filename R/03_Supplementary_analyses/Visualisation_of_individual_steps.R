#----------------------------------------------------------#
#
#
#                 The FOSSILPOL workflow 
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
  here::here("R/00_Config_file.R"))

current_env <- environment()

# install additional packages
utils::install.packages("ggpubr")
utils::install.packages("RColorBrewer")
library(ggpubr)
library(RColorBrewer)

#----------------------------------------------------------#
# 2. Neotoma data download -----
#----------------------------------------------------------#


# load the data
neotoma_meta_samples <-
  RFossilpol::util_load_latest_file(
    file_name = "neotoma_meta_samples",
    dir = paste0(data_storage_path, #[config_criteria]
                 "/Data/Processed/Neotoma_processed/Neotoma_meta"))

# test the presence of data
RFossilpol::util_check_if_loaded(
  file_name = "neotoma_meta_samples",
  env = current_env)

#referecne figure
p0 <-
  plot_map_of_data(
    data_source = neotoma_meta_samples,
    text_size = text_size, #[config_criteria]
    point_size = (point_size - 0.5),  #[config_criteria]
    point_alpha = 0.7,
    line_size = line_size,  #[config_criteria]
    point_colour = dark_gray, #[config_criteria]
    map_fill = light_gray, #[config_criteria]
    map_border = normal_gray #[config_criteria]
  )


(
  p1 <-
    plot_map_of_data(
      data_source = neotoma_meta_samples,
      text_size = text_size, #[config_criteria]
      point_size = point_size,  #[config_criteria]
      point_alpha = 0.7,
      line_size = line_size,  #[config_criteria]
      point_colour = highlight_orange, #[config_criteria]
      map_fill = light_gray, #[config_criteria]
      map_border = normal_gray #[config_criteria]
    ) +
    # ggplot2::labs(
    #   caption = paste("Number of datasets =", nrow(neotoma_meta_samples))
    # ) +
    ggplot2::geom_rect(
      ggplot2::aes(
        xmin = long_min,#[config_criteria]
        xmax = long_max, #[config_criteria]
        ymin = lat_min, #[config_criteria]
        ymax = lat_max #[config_criteria]
      ),
      fill = NA,
      colour = highlight_red, #[config_criteria]
      size = line_size
    )
  # + ggplot2::ggtitle("Filter by geographical location")
)


#----------------------------------------------------------#
# 3. Neotoma processed -----
#----------------------------------------------------------#

neotoma_processed <-
  RFossilpol::util_load_latest_file(
    file_name = "neotoma_processed", 
    dir = paste0(data_storage_path,  #[config_criteria]
                 "/Data/Processed/Neotoma_processed"))

# test the presence of data
RFossilpol::util_check_if_loaded(
  file_name = "neotoma_processed",
  env = current_env)


(
  p2 <-
    p0 +
    ggplot2::geom_point(
      data = neotoma_processed,
      colour = highlight_orange, #[config_criteria],
      alpha = 0.7,
      size = point_size
    )
  # ggplot2::labs(
  #   caption = paste("Number of datasets =", nrow(neotoma_processed))
  # ) +
  # ggplot2::ggtitle("Neotoma processed")
)


#----------------------------------------------------------#
# 4. Chronologies-----
#----------------------------------------------------------#

data_with_chronologies <- 
  RFossilpol::util_load_latest_file(
    file_name = "data_with_chronologies",
    dir = paste0(
      data_storage_path, #[config_criteria]
      "/Data/Processed/Data_with_chronologies"))

# test the presence of data
RFossilpol::util_check_if_loaded(
  file_name = "data_with_chronologies",
  env = current_env)

(
  p3 <-
    p0 +
    ggplot2::geom_point(
      data = data_with_chronologies,
      colour = highlight_orange, #[config_criteria],
      alpha = 0.7,
      size = point_size
    )
  # ggplot2::labs(
  #   caption = paste("Number of datasets =", nrow(data_with_chronologies))
  # ) +
  # ggplot2::ggtitle("Data with chronologies")
)


#----------------------------------------------------------#
# 5. Main filtering-----
#----------------------------------------------------------#

data_assembly <- 
  RFossilpol::util_load_latest_file(
    file_name = "data_assembly",
    dir =paste0(data_storage_path, #[config_criteria]
                "/Outputs/Data/"))

# test the presence of data
RFossilpol::util_check_if_loaded(
  file_name = "data_assembly",
  env = current_env)


(
  p4 <-
    p0 +
    ggplot2::geom_point(
      data = data_assembly,
      colour = highlight_orange, #[config_criteria],
      alpha = 0.7,
      size = point_size
    )
  # ggplot2::labs(
  #   caption = paste("Number of datasets =", nrow(data_assembly))
  # ) +
  # ggplot2::ggtitle("Filtered data")
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
    )+
    ggplot2::geom_label(
      ggplot2::aes(
        y = 0.35,
        label = data_name
      )
    )+
    ggplot2::geom_text(
      ggplot2::aes(
        y = 0.30,
        label = n_seq
      )
    )+
    ggplot2::scale_size_continuous(
      range  = c(10, 30)
    )+
    ggplot2::scale_colour_gradient(
      trans = "log",
      high = normal_gray, #[config_criteria]
      low = highlight_orange #[config_criteria]
    ) +
    ggplot2::coord_cartesian(
      ylim = c(0,1 )
    )+
    ggplot2::guides(
      colour = "none",
      size = "none"
    )+
    ggplot2::theme_void()+
    theme(
      text = element_text(size = text_size)
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
      p7,
      nrow = 2,
      ncol = 1,
      heights = c(1, 0.3),
      labels = c("", "E"))
)

ggplot2::ggsave(
  plot = p_fin,
  filename = paste0(current_dir, "/Outputs/Figures/Supplementary/Data_filtering.pdf"),
  width = 25,
  height = 12,
  units = image_units,
  dpi = image_dpi)


#----------------------------------------------------------#
# 8. Data spatio-temporal distribution -----
#----------------------------------------------------------#

# get the WWF biomes
res <- 0.1

biome_data <-
  # create a data.frame with all points for `res`
  expand.grid(
    long = seq(long_min, long_max ,res),
    lat = seq(lat_min, lat_max, res)
  ) %>% 
  # assign information for calibration curves 
  RFossilpol::geo_assign_value(
    data_source = .,
    dir = paste0(current_dir, "/Data/Input/Spatial/Biomes_shapefile/WWF"), 
    sel_method = "shapefile",
    file_name = "wwf_terr_biomes", 
    var = "BIOM_NAME",
    var_name = "wwf_biome") %>% 
  # drop all points without values
  tidyr::drop_na() %>% 
  # Now subset the data to only include terrestrial points
  RFossilpol::geo_assign_value(
    data_source = .,
    dir = paste0(current_dir, "/Data/Input/Spatial/Countries_shapefile"), 
    sel_method = "shapefile",
    file_name = "Countries_global", 
    var = "NAME",
    var_name = "country") %>% 
  tidyr::drop_na()

data_assembly_wwf <-
  data_assembly %>% 
  tidyr::drop_na(wwf_biome)

biomes_vec <-
  biome_data %>%
  purrr::pluck("wwf_biome") %>% 
  unique()

n_biomes <-
  biomes_vec %>% 
  length()

biome_palette <- 
  colorRampPalette(
    RColorBrewer::brewer.pal(8, "Set1"))(n_biomes) %>% 
  purrr::set_names(
    nm = biomes_vec
  )


(
  p6 <-
    biome_data %>% 
    ggplot2::ggplot(
      ggplot2::aes(
        x = long,
        y = lat,
        fill = wwf_biome
      )
    ) +
    ggplot2::geom_tile(
      colour = NA,
      alpha = 0.5
    ) +
    ggplot2::geom_point(
      data = data_assembly_wwf,
      size = point_size + 1,
      color = dark_gray
    )+
    ggplot2::geom_point(
      data = data_assembly_wwf,
      ggplot2::aes(col = wwf_biome),
      size = point_size
    )+
    ggplot2::coord_quickmap(
      xlim = c(long_min, long_max),
      ylim = c(lat_min, lat_max)
    ) +
    ggplot2::scale_color_manual(
      values = biome_palette
    ) +
    ggplot2::scale_fill_manual(
      values = biome_palette
    ) +
    ggplot2::theme_classic()+
    ggplot2::theme(
      legend.position = "none",
      text = ggplot2::element_text(size = text_size),
      line = ggplot2::element_line(size = line_size)
    ) +
    ggplot2::labs(
      x = "Longitude",
      y = "Latitude"
    )
)

(
  p7 <-
    data_assembly_wwf %>% 
    dplyr::count(wwf_biome) %>% 
    ggplot2::ggplot(
      ggplot2::aes(
        x = forcats::fct_reorder(wwf_biome, -n),
        y = n)
    ) +
    ggplot2::geom_bar(
      ggplot2::aes(fill = wwf_biome),
      stat = "identity",
      colour = normal_gray
    )+
    ggplot2::geom_text(
      ggplot2::aes(label = wwf_biome),
      angle= 90,
      hjust = 0,
      nudge_y = 2,
      size = 2
    )+
    ggplot2::coord_cartesian(
      ylim = c(0, 80)
    )+
    ggplot2::scale_fill_manual(
      values = biome_palette
    )+
    ggplot2::theme_classic()+
    ggplot2::theme(
      line = ggplot2::element_line(size = line_size),
      text = ggplot2::element_text(size = text_size),
      legend.position = "none",
      axis.line.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank()
    )+
    ggplot2::labs(
      y = "Number of datatasets"
    )
)

p8 <-
  data_assembly_wwf %>% 
  ggplot2::ggplot(
    ggplot2::aes(
      y = forcats::fct_reorder(dataset_id, wwf_biome), 
      x = age_min)
  ) +
  ggplot2::geom_segment(
    ggplot2::aes(
      yend = dataset_id,
      xend = age_max,
      col = wwf_biome
    )
  ) +
  ggplot2::scale_color_manual(
    values = biome_palette
  ) +
  ggplot2::scale_x_continuous(
    trans = "reverse"
  ) +
  ggplot2::theme_classic()+
  ggplot2::theme(
    line = ggplot2::element_line(size = line_size),
    text = ggplot2::element_text(size = text_size),
    legend.position = "none",
    axis.line.y = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank()
  )+
  ggplot2::labs(
    y = "Datasets",
    x = "Age (cal yr BP)"
    
  )

(
  p_spatio_temporal_dist <-
    ggpubr::ggarrange(
      p6, p7, p8,
      nrow = 1,
      ncol = 3,
      labels = LETTERS[1:3]
    )
)

ggplot2::ggsave(
  plot = p_spatio_temporal_dist,
  filename = paste0(current_dir, "/Outputs/Figures/Supplementary/Spatio_temporal_dist.pdf"),
  width = 20,
  height = 10,
  units = image_units,
  dpi = image_dpi)
