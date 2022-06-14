#----------------------------------------------------------#
#
#
#                 The FOSSILPOL workflow 
#
#             Get the data for all Neotoma sequences
#                 
#
#   O. Mottl, S. Flantua, K. Bhatta, V. Felde, A. Seddon 
#                         2021
#
#----------------------------------------------------------#

# Obtain information about all Neotoma sequences and make plots


#----------------------------------------------------------#
# 1. Set up -----
#----------------------------------------------------------#

library(here)

# Load configuration
source(
  here::here("R/00_Config_file.R"))

#----------------------------------------------------------#
# 2. Get Neotoma data -----
#----------------------------------------------------------#

# api paths
rawdatasets <- "https://api.neotomadb.org/v2.0/data/datasets/"

# request all data of selected type from Neotoma 2.0
pollends <- 
  httr::GET(
    rawdatasets, 
    query = list(
      datasettype = "pollen", #[config_criteria]
      limit = 99999,
      offset = 0))

# Extract all data 
datasets <- httr::content(pollends)$data

# Create a tibble with dataset_id, and coordinates
dataset_geo <-
  RFossilpol:::proc_neo_get_coord(datasets)

datasets_age <-
  datasets %>% 
  purrr::map(
    "site") %>% 
  purrr::map(
    "datasets") %>% 
  purrr::map_dfr(
    .f = ~ {
      
      sel_datase_id <- 
        .x[[1]]$datasetid
      
      dataset_info <-
        .x[[1]]$agerange
      
      purrr::map_dfr(
        dataset_info,
        .f = ~ tibble::tibble(
          dsid = sel_datase_id,
          ageold = .x$ageold,
          ageyoung = .x$ageyoung)
      )
    }) %>% 
  dplyr::distinct(dsid, .keep_all = TRUE)

datasets_full <-
  dplyr::inner_join(
    dataset_geo,
    datasets_age,
    by = "dsid") %>% 
  dplyr::filter(
    lat <= 90 & lat >= -90) %>% 
  dplyr::filter(
    long <= 180 & long >= -180) %>% 
  dplyr::mutate(
    agemean = (ageold + ageyoung)/2,
    age_dif = ageold - ageyoung) %>% 
  dplyr::filter(age_dif > 0) %>% 
  dplyr::filter(ageyoung > -75) %>% 
  dplyr::filter(ageold < 50e3) %>% 
  dplyr::arrange(agemean) %>% 
  dplyr::mutate(
    row_n = dplyr::row_number()) 

#----------------------------------------------------------#
# 3. geographical distribution -----
#----------------------------------------------------------#

lat_bin_seq <-
  with(datasets_full, seq(from = min(lat), to = max(lat), by = 3))

long__bin_seq <-
  with(datasets_full, seq(from = min(long), to = max(long), by = 3))

lat_seq <-
  seq(-90, 90, 30)

long_seq <-
  seq(-180, 180, 30)

n_seq <- 
  c(1, 5, 10, 20, 50, 150, 250)

seq_labels <-
  paste(n_seq[-length(n_seq)], n_seq[-1], sep = "-")

bin_palette_geo <-
  colorRampPalette(c(palette_matching))(length(n_seq)-1) %>% 
  purrr::set_names(as.character(n_seq[-length(n_seq)]))

dat_geo <-
  datasets_full %>% 
  dplyr::mutate(
    lat_bin = lat_bin_seq[findInterval(lat, lat_bin_seq, all.inside = TRUE)],
    long_bin = long__bin_seq[findInterval(long, long__bin_seq, all.inside = TRUE)]
  ) %>% 
  dplyr::group_by(lat_bin, long_bin) %>% 
  dplyr::summarise(
    .groups = "drop",
    N = dplyr::n()
  ) %>% 
  dplyr::mutate(
    N_bin = n_seq[findInterval(N, n_seq, all.inside = TRUE)]
  )

(
  p1 <-
    dat_geo %>% 
    ggplot2::ggplot(
      ggplot2::aes(
        x = long_bin,
        y = lat_bin
      )
    )+
    ggplot2::borders(
      fill = col_gray_light,
      colour = col_gray_light,
      size = 0.01
    )+
    ggplot2::geom_tile(
      ggplot2::aes(
        fill = as.factor(N_bin)
      ),
      colour = NA,
      alpha = 1
    ) + 
    ggplot2::coord_quickmap(
      xlim = range(long__bin_seq),
      ylim = range(lat_bin_seq)
    )+
    ggplot2::scale_y_continuous(breaks = lat_seq)+
    ggplot2::scale_x_continuous(breaks = long_seq)+
    ggplot2::scale_fill_manual(
      values = bin_palette_geo,
      labels = seq_labels,
      guide = ggplot2::guide_legend(
        nrow = 2,
        byrow = TRUE
      )
    )+
    ggplot2::labs(
      x = "Longitude",
      y = "Latitude",
      fill = "The number of sequences \nper 3 degrees"
    )+
    ggplot2::theme_classic()+
    ggplot2::theme(
      legend.direction = "horizontal",
      legend.position = "bottom",
      text = ggplot2::element_text(size = text_size),
      line = ggplot2::element_line(size = line_size)
    )
)

#----------------------------------------------------------#
# 4. spatial  distribution -----
#----------------------------------------------------------#

bin_vec <-
  c(
    -100,
    seq(5e3, 50e3, 5e3)
  )

for(i in 1:(length(bin_vec) - 1)){
  
  if(i == 1){
    time_bins <- 
      tibble::tibble(
        name = paste("present",  bin_vec[i+1], sep = " - "),
        start = bin_vec[i],
        end = bin_vec[i+1]) 
    
  } else {
    time_bins <-
      dplyr::bind_rows(
        time_bins,
        tibble::tibble(
          name = paste(bin_vec[i],  bin_vec[i+1], sep = " - "),
          start = bin_vec[i],
          end = bin_vec[i+1]
        ))
  }
}

time_bins_counts <-
  time_bins %>% 
  dplyr::mutate(
    N = purrr::map2_dbl(
      .x = start,
      .y = end,
      .f = ~ datasets_full %>% 
        dplyr::filter(
          ageyoung < .y) %>%
        dplyr::filter(
          ageold > .x) %>% 
        nrow()
    )
  ) %>%
  dplyr::arrange(N) %>% 
  dplyr::mutate(
    row_n = dplyr::row_number())

bin_palette_temp <-
  colorRampPalette(c(col_orange_dark, col_orange_light))(nrow(time_bins_counts)) %>% 
  purrr::set_names(as.character(time_bins_counts$N))

(p2 <-
    time_bins_counts %>% 
    ggplot2::ggplot(
      ggplot2::aes(y = N,
                   x = row_n)
    ) +
    ggplot2::geom_bar(
      ggplot2::aes(fill = as.factor(N)),
      stat = "identity",
      colour = col_gray_dark,
      size = line_size
    ) +
    ggplot2::scale_x_continuous(
      breaks = time_bins_counts$row_n,
      labels = time_bins_counts$name)+
    ggplot2::scale_fill_manual(
      values = bin_palette_temp
    )+
    ggplot2::labs(
      x = "Age (cal yr BP)",
      y = "Number of sequences \npresented in the time period")+
    ggplot2::theme_classic()+
    ggplot2::theme(
      legend.position = "none",
      text = ggplot2::element_text(size = text_size),
      line = ggplot2::element_line(size = line_size),
      axis.text.x = ggplot2::element_text(
        size = text_size * 0.75,
        angle = 90,
        vjust = 0.5,
        hjust = 0.9)
    )
)


(p_fin <-
    ggpubr::ggarrange(
      p1, p2,
      widths = c(1,0.4),
      labels = c("A", "B")
    ))

ggplot2::ggsave(
  plot = p_fin,
  filename = paste0(current_dir, "/Outputs/Figures/Supplementary/Neotoma_data_full-",current_date, ".pdf"),
  width = 25,
  height = 12,
  units = image_units,
  dpi = image_dpi)

