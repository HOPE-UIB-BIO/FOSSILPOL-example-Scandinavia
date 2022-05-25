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
utils::install.packages("wordcloud")
utils::install.packages("tm")
utils::install.packages("ggpubr")
utils::install.packages("RColorBrewer")
library(wordcloud)
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
    point_alpha = 0.5,
    line_size = line_size,  #[config_criteria]
    point_colour = normal_gray, #[config_criteria]
    map_fill = light_gray, #[config_criteria]
    map_border = normal_gray #[config_criteria]
  )


(
  p1 <-
    plot_map_of_data(
      data_source = neotoma_meta_samples,
      text_size = text_size, #[config_criteria]
      point_size = point_size,  #[config_criteria]
      point_alpha = 0.5,
      line_size = line_size,  #[config_criteria]
      point_colour = dark_gray, #[config_criteria]
      map_fill = light_gray, #[config_criteria]
      map_border = normal_gray #[config_criteria]
    ) +
    ggplot2::labs(
      caption = paste("Number of datasets =", nrow(neotoma_meta_samples))
    ) +
    ggplot2::geom_rect(
      ggplot2::aes(
        xmin = long_min,#[config_criteria]
        xmax = long_max, #[config_criteria]
        ymin = lat_min, #[config_criteria]
        ymax = lat_max #[config_criteria]
      ),
      fill = NA,
      colour = highlight_red, #[config_criteria]
      size = 1
    )+
    ggplot2::ggtitle("Filter by geographical location")
)

ggplot2::ggsave(
  plot = p1,
  filename = paste0(current_dir, "/Outputs/Figures/Supplementary/Individual_steps/01-download.pdf"),
  width = 12,
  height = 17,
  units = image_units,
  dpi = image_dpi)


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
      colour = highlight_red, #[config_criteria],
      alpha = 0.7,
      size = point_size
    )+
    ggplot2::labs(
      caption = paste("Number of datasets =", nrow(neotoma_processed))
    ) +
    ggplot2::ggtitle("Neotoma processed")
)

ggplot2::ggsave(
  plot = p2,
  filename = paste0(current_dir, "/Outputs/Figures/Supplementary/Individual_steps/02-neotoma_processed.pdf"),
  width = 12,
  height = 17,
  units = image_units,
  dpi = image_dpi)


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
      colour = highlight_red, #[config_criteria],
      alpha = 0.7,
      size = point_size
    )+
    ggplot2::labs(
      caption = paste("Number of datasets =", nrow(data_with_chronologies))
    ) +
    ggplot2::ggtitle("Data with chronologies")
)

ggplot2::ggsave(
  plot = p3,
  filename = paste0(current_dir, "/Outputs/Figures/Supplementary/Individual_steps/03-chronologies.pdf"),
  width = 12,
  height = 17,
  units = image_units,
  dpi = image_dpi)

# ├ 4.1 chronology control point numbers example  -----
(
  p4 <-
    plot_map_of_data(
      data_source = data_with_chronologies,
      text_size = text_size, #[config_criteria]
      point_size = 1,  #[config_criteria]
      point_alpha = 1,
      line_size = line_size,  #[config_criteria]
      point_colour = light_gray, #[config_criteria]
      map_fill = light_gray #[config_criteria]
    ) +
    ggplot2::geom_point(
      ggplot2::aes(
        col = n_chron_control, 
        size = n_chron_control ),
      alpha = 0.75
    )+
    ggplot2::scale_size_continuous(
      trans = "log"
    ) +
    ggplot2::scale_colour_gradient(
      trans = "log",
      low = normal_gray, #[config_criteria]
      high = highlight_orange #[config_criteria]
    ) +
    ggplot2::guides(
      colour = "none",
      size = "none"
    )+
    ggplot2::ggtitle("Number of chron.control points")
)


ggplot2::ggsave(
  plot = p4,
  filename = paste0(current_dir, "/Outputs/Figures/Supplementary/Individual_steps/04-chron_control_points.pdf"),
  width = 12,
  height = 17,
  units = image_units,
  dpi = image_dpi)


# ├ 4.4 Age-depth models example  -----

#load bchron models
chron_mod_output <- 
  readr::read_rds(
    paste0(data_storage_path, 
           "/Data/Processed/Chronology/Models_full/chron_mod_output-2022-05-25.rds"
    )
  )

# select first 25
chron_mod_output_example <-
  chron_mod_output %>% 
  dplyr::slice(1:25)

# to clean the memory
rm(chron_mod_output)

ad_plot_list <-
  purrr::map2(
    .x = chron_mod_output_example$bchron_mod,
    .y = chron_mod_output_example$dataset_id,
    .f = ~ plot(.x) + 
      ggplot2::theme_classic() + 
      ggplot2::theme(
        text = ggplot2::element_text(
          size = text_size
        ),
        line = ggplot2::element_line(
          size = line_size
        ),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank()
      )+
      ggplot2::labs(
        caption = .y)
  )

(
  p5 <-
    ggpubr::ggarrange(
      plotlist = ad_plot_list,
      nrow = 5,
      ncol = 5
    ) %>% 
    ggpubr::annotate_figure(
      p = .,
      left = ggpubr::text_grob(
        label ="Depth (cm)",
        size = text_size,
        rot = 90
      ),
      bottom = ggpubr::text_grob(
        label = "Age (cal years BP)",
        size = text_size
      )
    )
)


ggplot2::ggsave(
  plot = p5,
  filename = paste0(current_dir, "/Outputs/Figures/Supplementary/Individual_steps/05-ad_models.pdf"),
  width = 35,
  height = 17,
  units = image_units,
  dpi = image_dpi)

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
  p6 <-
    p0 +
    ggplot2::geom_point(
      data = data_assembly,
      colour = highlight_red, #[config_criteria],
      alpha = 0.7,
      size = point_size
    )+
    ggplot2::labs(
      caption = paste("Number of datasets =", nrow(data_assembly))
    ) +
    ggplot2::ggtitle("Filtered data")
)

ggplot2::ggsave(
  plot = p6,
  filename = paste0(current_dir, "/Outputs/Figures/Supplementary/Individual_steps/06-filtered.pdf"),
  width = 12,
  height = 17,
  units = image_units,
  dpi = image_dpi)

# ├ 5.1 Number of sequences example -----

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
  p7 <- 
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

ggplot2::ggsave(
  plot = p7,
  filename = paste0(current_dir, "/Outputs/Figures/Supplementary/Individual_steps/07-steps.pdf"),
  width = 18,
  height = 10,
  units = image_units,
  dpi = image_dpi)


#----------------------------------------------------------#
# 6. Additional features-----
#----------------------------------------------------------#

# ├ 6.1 WWF biome example -----

res <- 0.25

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
  p8 <-
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
  p9 <-
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
      nudge_y = 2
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

p10 <-
  ggpubr::ggarrange(
    p8, p9
  )


ggplot2::ggsave(
  plot = p10,
  filename = paste0(current_dir, "/Outputs/Figures/Supplementary/Individual_steps/10-wwf.pdf"),
  width = 20,
  height = 17,
  units = image_units,
  dpi = image_dpi)
