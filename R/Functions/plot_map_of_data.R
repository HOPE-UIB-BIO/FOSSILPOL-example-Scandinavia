plot_map_of_data <- 
  function(data_source,
           margin = 5,
           point_size = 3,
           point_alpha = 1,
           point_colour = "gray30",
           text_size = 16,
           line_size = 0.1,
           map_fill = "gray90",
           map_border = NA
           ) {
    
    RFossilpol:::util_check_class("data_source", "data.frame")
    
    RFossilpol:::util_check_class("margin", "numeric")
    
    RFossilpol:::util_check_class("point_size", "numeric")
    
    RFossilpol:::util_check_class("point_alpha", "numeric")
    
    assertthat::assert_that(
      point_alpha <= 1 & point_alpha >= 0,
      msg = "'point_alpha' must be between 0 and 1"
    )
   
    RFossilpol:::util_check_class("point_colour", "character")
    
    RFossilpol:::util_check_class("text_size", "numeric")
     
    RFossilpol:::util_check_class("line_size", "numeric")
    
    RFossilpol:::util_check_class("map_fill", "character")
 
    RFossilpol:::util_check_class("map_fill", c("character", "logical"))
       
    RFossilpol:::util_check_col_names(
      "data_source" ,c("long", "lat")
    )
    
    x_range <-
      data_source %>% 
      purrr::pluck("long") %>% 
      range()
    
    x_lim <- 
      c(
        min(x_range) - margin,
        max(x_range) + margin)
    
    y_range <-
      data_source %>% 
      purrr::pluck("lat") %>% 
      range()
    
    y_lim <- 
      c(
        min(y_range) - margin,
        max(y_range) + margin)
    
    data_source %>% 
      ggplot2::ggplot(
        ggplot2::aes(
          x = long,
          y = lat
        )
      ) +
      ggplot2::borders(
        fill = map_fill,
        colour = map_border,
        size = line_size)+
      ggplot2::geom_point(
        size = point_size,
        alpha = point_alpha,
        colour = point_colour
      )+
      ggplot2::coord_quickmap(
        xlim = x_lim,
        ylim = y_lim
      ) +
      ggplot2::labs(
        x = "Longitude",
        y = "Latitude") +
      ggplot2::theme_classic()+
      ggplot2::theme(
        text = ggplot2::element_text(size = text_size),
        line = ggplot2::element_line(size = line_size)
      ) %>% 
      return()
    
  }
