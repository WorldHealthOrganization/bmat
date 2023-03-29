

plot_bmis_sub <- function(tot_data, bmisisos, params, dataonly=T){
  
  data_b <- tot_data%>%
    dplyr::filter(parameter %in% params & iso_alpha_3_code %in% bmisisos ) 
  

  plot_bmis_data= data_b %>%
    ggplot2::ggplot(ggplot2::aes(x = year_mid)) +
    ggplot2::scale_x_continuous(name = "midyear", breaks = c(1985, 1995, 2005, 2015)) +
    ggplot2::ylab("value") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)) +
    ggplot2::geom_point(
      ggplot2::aes_string(
        y = "value",
        col = "data_type"),
      alpha = .7,
      size = 2,
      stroke = .75
    ) +
    # ggplot2::geom_segment(
    #   ggplot2::aes_string(
    #     x = "year_start",
    #     xend = "year_end",
    #     y = "value",
    #     yend = "value", col = "data_type"),
    #   alpha = .5,
    #   size = 1,
    #   stroke = .75
    # ) +
    #ggplot2::scale_fill_manual(values = c(cbp2[5], cbp2[6], cbp2[4]), na.translate = F) +
    ggplot2::scale_color_manual(name = "Data Source", values = c("blue", "orange"), na.translate = F) +
    ggplot2::geom_errorbar(
      ggplot2::aes_string(
        y = "value",
        ymax = "value_up",
        ymin = "value_low", color = "data_type"
      ),
      width = 0,
      alpha = 1
    )  +
    ggplot2::facet_grid(paramf ~ isof, scales = "free")+
    theme(strip.text = element_text(size = 10, color = "black"),
          axis.title.x = element_text(size = 10),
          axis.title.y = element_text(size =10),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          legend.text = element_text(size = 10),legend.box = "vertical"
    )
  
    
  
 plot_bmis_with_estimates = plot_bmis_data +
    ggnewscale::new_scale_color() +
    ggplot2::geom_point(ggplot2::aes(y = `0.5`, x = year_mid, col=version),  size = 1) +
    ggplot2::geom_segment(ggplot2::aes(
      y= `0.1`,
      yend = `0.9`,
      x = year_mid,
      xend = year_mid,
      col = version
    ), alpha = 0.5) +
    ggplot2::scale_fill_manual(values = c("gray", "red"), na.translate = F, name = "") +
    ggplot2::scale_color_manual(values = c("gray", "red"), na.translate = F, name = "Estimate Type") +
    ggplot2::facet_grid(paramf ~ isof, scales = "free")
  
  
 plot_bmis_with_estimates
 
 if(dataonly)
  return(plot_bmis_data)
    else{
      return(plot_bmis_with_estimates)
    }
}