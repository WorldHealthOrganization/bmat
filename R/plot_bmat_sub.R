plot_bmat_sub <- function(tot_data,isos, include_study_period=F, params){
  library(ggplot2)
  
  cbp2 <- c("#000000",#black 1
            "#E69F00",#orange 2
            "#56B4E9",#lightblue 3
            "#E69F00",#gold 5
            "darkgreen",#green 4
            "blue",#blue 6
            "#D55E00",#red 7
            "#CC79A7")#pink 8
  
  
 
  
  plot_new<- tot_data %>%
    filter(iso_alpha_3_code %in% isos & parameter %in% params & version %in% c("Final Model Estimate", NA)) %>%
    dplyr::mutate(paramf = factor(parameter, levels = c("sens", "spec", "pm", "CRVSadj"), labels = c("sens", "spec", "PM", "CRVS adj")),
                  year_mid = floor(year_mid))
   
  
  #hack
  plot_new$data_type = ifelse(plot_new$data_type == "DHS", "Survey", plot_new$data_type)
    
  inq_start = inq_end = rep(NA, length(isos))
  for(i in 1:length(isos)){
    temp <- plot_new %>% filter(data_type == "Special Inquiry" & value_type == "original" & parameter == "sens" & iso_alpha_3_code == isos[i])
    
  if(length(temp$year_mid)>0){
    inq_start[i] = floor(min(temp$year_mid))
    inq_end[i] = floor(max(temp$year_mid) )
  }else{
    inq_start[i] = NA
    inq_end[i] = NA
  }
  }
  
  ploty = plot_new %>%
    ggplot2::ggplot(ggplot2::aes(x = floor(year_mid))) +
    ggplot2::scale_x_continuous(name = "midyear", breaks = c(1985, 1995, 2005, 2015)) +
    ggplot2::ylab("value") +
    # ggplot2::ggtitle(titles) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)) +
    ggplot2::geom_point(
      ggplot2::aes_string( x= "year_mid", y = "value", shape = "data_type", color = "adjustment_type"),
      alpha = .7,
      size = 3,
      stroke = .75
    ) +
    ggplot2::geom_errorbar(
      ggplot2::aes_string(x = "year_mid",  y = "value",ymax = "value_up", ymin = "value_low", color = "adjustment_type"),
      width = 0,
      alpha = 0.4
    ) +
    ggplot2::scale_color_manual(name = "Adjustment Type", values = c(cbp2[6], cbp2[5], cbp2[4]), na.translate = F) +
    ggplot2::scale_shape_manual(na.translate = F, name = "Data Type", values = c(16, 4, 18, 15)) +
    theme(strip.text = element_text(size = 8, color = "black"),
          axis.title.x = element_text(size = 8),
          axis.title.y = element_text(size = 8),
          axis.text.x = element_text(size = 8),
          axis.text.y = element_text(size = 8),
          legend.title = element_text(size = 8), 
          legend.text  = element_text(size = 8)) 
  
  
  
  
  if(include_study_period){
    ploty = ploty +
      geom_vline(data = data.frame(xint=inq_start), aes(xintercept = xint), linetype = "dotted") +
      geom_vline(data = data.frame(xint=inq_end), aes(xintercept = xint), linetype = "dotted") 
  }
  
    ploty <- ploty +
      ggplot2::facet_grid(paramf ~ casef,  scales = "free") 
  
  ploty = ploty +
    ggnewscale::new_scale_color() +
    ggplot2::geom_line(ggplot2::aes(y = `0.5`, col=version), size=0.7) +
    ggplot2::geom_ribbon(ggplot2::aes(
      x = year_mid,
      ymin = `0.1`,
      ymax = `0.9`, col=version, fill = version),  alpha = .2) +
    ggplot2::scale_fill_manual(values = c("red", "black"), na.translate = F, name = "", guide = F) +
    ggplot2::scale_color_manual(values = c("red", "black"), na.translate = F, name = "Estimate Type") 

  
    
  return(ploty)
}
