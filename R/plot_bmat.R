
plot_bmat <- function(data,
                      iso_alpha_3_code,
                      caption,
                      has_observed_data,
                      has_comparison,
                      alphavalue = .375) {
  data <- data %>%
    dplyr::rename(source_type = type) %>%
    dplyr::mutate(parameter = toupper(parameter)) %>%
    dplyr::rename(estimate = estimate_version) %>%
    dplyr::mutate(estimate = as.character(estimate)) %>%
    dplyr::mutate(alpha = ifelse(value_type == "model adjusted data" | source_type == "specialized study", 1, 0))
  
  # if no estimates to compare rename legend
  if (!has_comparison) {
    data <- data %>%
      dplyr::mutate(estimate = ifelse(!is.na(estimate), "80% UI", NA))
  }
  
  
  data$color1 <- "1"
  data$color2 <- "2"
  first_year <- data %>% dplyr::pull(year_mid) %>% min()
  last_year <- data %>% dplyr::pull(year_mid) %>% max()
  breaks = seq(first_year,
               last_year,
               by = 5)
  # colorblind pallet
  cbp2 <- c("#000000",#black 1
            "#E69F00",#orange 2
            "#56B4E9",#lightblue 3
            "#009E73",#green 4
            "#F0E442",#yellow 5
            "#0072B2",#blue 6
            "#D55E00",#red 7
            "#CC79A7")#pink 8
  
  
  
  #plotting starts with estimates as they determine the x of the plot.
  plot <- data %>%
    ggplot2::ggplot(ggplot2::aes(x = year_mid)) +
    ggplot2::ggtitle(iso_alpha_3_code) +
    ggplot2::scale_x_continuous(name = "year", breaks = breaks) +
    ggplot2::ylab("value") +
    ggplot2::labs(caption = caption) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, size = 16),
      axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, size = 12),
      axis.text.y = ggplot2::element_text(size = 12),
      axis.title.x = ggplot2::element_text(size = 16),
      axis.title.y = ggplot2::element_text(size = 16),
      strip.text.x = ggplot2::element_text(size = 16), #the sub titltes MMR and PM
      legend.text = ggplot2::element_text(size = 14), 
      legend.title = ggplot2::element_text(size = 14), 
      plot.caption = ggplot2::element_text(vjust= -2.5, hjust = 0,face = "italic"),
      plot.caption.position =  "plot"
    ) +
    ggplot2::geom_line(ggplot2::aes(y = `0.5`, color = estimate), alpha = .6) +
    ggplot2::geom_ribbon(ggplot2::aes(
      ymin = `0.1`,
      ymax = `0.9`,
      fill = estimate
    ), alpha = alphavalue) +
    ggplot2::scale_fill_manual(values = c(cbp2[4], cbp2[8], cbp2[1]), na.translate = F) +
    ggplot2::scale_color_manual(values = c(cbp2[4], cbp2[8], cbp2[1]), na.translate = F)
  
  if(has_observed_data) {
    plot <- plot +
      ggnewscale::new_scale_color() +
      ggplot2::geom_point(
        ggplot2::aes_string(y = "value", shape = "source_type", color = "value_type"),
        alpha = .7,
        size = 2,
        stroke = .75
      ) +
      ggplot2::scale_shape_discrete(na.translate = F) +
      ggplot2::geom_linerange(
        ggplot2::aes_string(
          y = "value",
          ymax = "value_up",
          ymin = "value_low", color = "value_type"
        ),
        size = .1,
        alpha = 1
      ) + 
      ggplot2::geom_linerange(
        ggplot2::aes_string(
          y = "value",
          xmax = "year_end",
          xmin = "year_start", 
          color = "value_type",
          alpha = "alpha"
        ),
        size = .5,
      ) + 
      ggplot2:: scale_alpha(guide = 'none') +
      ggplot2::scale_color_manual(values = c(cbp2[2], cbp2[3]), na.translate = F) +
      ggplot2::labs(color = "value type", shape = "source type")
  }
  plot <- plot +
    ggplot2::facet_wrap(ggplot2::vars(parameter), scale = "free")
  return(plot)
}
