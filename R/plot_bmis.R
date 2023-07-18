


plot_bmis_one_country <- function(
    country_ref,
    estimates,
    estimates_old = NULL,
    sens_spec = NULL, 
    main_data_for_plots,
    iso_alpha_3_code
) {
  #plot code starts here
  country_name <- country_ref %>% 
    dplyr::filter(iso_alpha_3_code == !!iso_alpha_3_code) %>%
    dplyr::pull(name_short_en)
  estimates <- estimates$parameter_estimates_ct
  first_year <- estimates$year_reference %>% min %>% floor
  last_year <- estimates$year_reference %>% max  %>% floor
  breaks <- seq(first_year,
                last_year,
                by = 5)
  
  
  # filter observed data for a particular country
  observed <- main_data_for_plots %>%
    dplyr::filter(iso_alpha_3_code == !!iso_alpha_3_code) %>%
    dplyr::mutate(value_type = "observed") %>% #add an indentifier before we rbind these data sets
    dplyr::mutate(y_upper = as.numeric(`0.9`)) %>%
    dplyr::mutate(y = as.numeric(`0.5`)) %>%
    dplyr::mutate(y_lower = as.numeric(`0.1`)) %>%
    dplyr::select(-`0.9`) %>%
    dplyr::select(-`0.5`) %>%
    dplyr::select(-`0.1`)
  years_observed <- observed %>%
    dplyr::pull(year_reference) %>%
    unique
  years_observed <- floor(years_observed) #estimates are for mid years
  # filter estimates for a particular country and by years in which observed data exists i.e. study period
  est1 <- estimates  %>%
    dplyr::filter(iso_alpha_3_code == !!iso_alpha_3_code) %>%
    dplyr::mutate(value_type = "model_estimate") %>%
    dplyr::mutate(year_reference = floor(year_reference)) 
  #dplyr::filter(year_reference %in% years_observed)
  
  
  if(!is.null(sens_spec)){
    gc_estimate_reformat <- sens_spec %>%
      dplyr::select(t, sens, spec, iso_alpha_3_code) %>%
      tidyr::gather(key = "parameter", value = "0.5", sens:spec )
    
    
    est_gc <- gc_estimate_reformat  %>%
      dplyr::filter(iso_alpha_3_code %in% iso_alpha_3_code) %>%
      dplyr::mutate(value_type = "global_conv_estimate") %>% #add an indentifier before we rbind these data sets
      dplyr::mutate(year_reference = t + 1985-1) 
    #dplyr::filter(year_reference %in% years_observed)
    
    
    pdata1 <- observed %>% 
      dplyr::bind_rows(est1) %>% 
      dplyr::bind_rows(est_gc)
    
  }else{
    pdata1 <- observed %>% 
      dplyr::bind_rows(est1)
  }
  
  
  
  
  if(!is.null(estimates_old)) {
    est2 <- estimates_old  %>%
      dplyr::filter(iso_alpha_3_code %in% !!iso_alpha_3_code) %>%
      dplyr::mutate(value_type = "model_estimate_old") %>% #add an indentifier before we rbind these data sets
      dplyr::mutate(year_reference = floor(year_reference)) 
    #dplyr::filter(year_reference %in% years_observed)
    pdata <- pdata1 %>% 
      # dplyr::bind_rows(est1) %>% 
      dplyr::bind_rows(est2)
  } else {
    pdata <- pdata1 
    # %>% dplyr::bind_rows(est1)
  }
  
  
  pdata <- pdata %>%
    dplyr::filter(parameter %in% c( "gamma_tn", "gamma_tp", "gamma_fp", "gamma_fn", "gamma_mat_vr", "gamma_truemat_vr", "sens", "spec", "vradj.x")) %>%
    dplyr::mutate(parameter = ifelse(parameter == "vradj.x", "CRVS adjustment", parameter)) %>%
    dplyr::mutate(parameter = ifelse(parameter == "sens", "sensitivity", parameter)) %>%
    dplyr::mutate(parameter = ifelse(parameter == "spec", "specificity", parameter))
  
  
  
  ###To Greg- We want to show se and sp estimates for all years (not for other params) in these plots. So created function to add these to the pdata.
  `%!in%` = Negate(`%in%`)
  exclude = which(pdata$parameter %in% c( "gamma_tn", "gamma_tp", "gamma_fp", "gamma_fn", "gamma_mat_vr", "gamma_truemat_vr")  & pdata$year_reference %!in% years_observed)
  pdata = pdata[-exclude,]
  
  plot_bmis_one_country_core(pdata, breaks, country_name)
}

plot_bmis_one_country_core <- function(pdata,
                                       breaks,
                                       country_name
) {
  iso_alpha_3_code <- pdata$iso_alpha_3_code %>% unique()
  temp = factor(pdata$parameter, levels=c( "gamma_tn", "gamma_tp", "gamma_fp", "gamma_fn", "gamma_mat_vr", "gamma_truemat_vr", "CRVS adjustment", "sensitivity", "specificity"))
  p <- pdata %>%
    ggplot2::ggplot(ggplot2::aes(x = year_reference)) +
    ggplot2::ggtitle(paste0("BMis estimates: ", country_name, " (", iso_alpha_3_code, ")")) +
    ggplot2::scale_x_continuous(name = "year", breaks = breaks, labels = breaks, limits = c(breaks[1], breaks[length(breaks)]), guide = ggplot2::guide_axis(angle = 90)) +
    ggplot2::scale_y_continuous(name = "value") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      axis.text.x = ggplot2::element_text(angle = 90, hjust = 1),
      strip.text.x = ggplot2::element_text(
        size = 5.5
      )
    ) +
    ggplot2::geom_line(ggplot2::aes(y = `0.5`, color = value_type), alpha = .6) +
    ggplot2::geom_ribbon(ggplot2::aes(
      ymin = `0.1`,
      ymax = `0.9`,
      fill = value_type
    ), alpha = .3) +
    # ggplot2::scale_fill_manual(values = c(cbp2[4], cbp2[8], cbp2[1]), na.translate = F) +
    # ggplot2::scale_color_manual(values = c(cbp2[4], cbp2[8], cbp2[1]), na.translate = F) +
    ggnewscale::new_scale_color() +
    ggplot2::geom_point(
      ggplot2::aes(y = y,
                   col = value_type),
      # col = "black",
      # pch = 1, # 21 if we want outline and replace col with fill
      position = ggplot2::position_dodge(0.3),
      size = 1,
      alpha = .8
    ) +
    ggplot2::geom_errorbar(ggplot2::aes(
      ymin = y_lower,
      ymax = y_upper,
      col = value_type),
      position = ggplot2::position_dodge(0.3),
      size = .75,
      width = 0,
      alpha = .8,
    ) +
    ggplot2::facet_wrap(temp, scale = "free")
  # ggplot2::geom_vline(xintercept = min(years_observed) -.3, linetype = "dotted") +
  
  
  return(p)
  
}
# ribbon version for years without data, looks non-sensible so leaving out for now
# est1 %>%
#   ggplot2::ggplot(ggplot2::aes(x = year_reference)) +
#   ggplot2::ggtitle(iso_alpha_3_code) +
#   ggplot2::scale_x_continuous(name = "year", breaks = breaks, labels = breaks, limits = c(breaks[1], breaks[length(breaks)]), guide = ggplot2::guide_axis(angle = 90)) +
#   ggplot2::scale_y_continuous(name = "estimate") +
#   ggplot2::theme_bw() +
#   ggplot2::theme(
#     plot.title = ggplot2::element_text(hjust = 0.5),
#     axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)
#   ) +
#   ggplot2::geom_line(
#     ggplot2::aes(y = `0.5`),
#     # col = "black",
#     # pch = 1, # 21 if we want outline and replace col with fill
#     size = 1,
#     alpha = 1
#   ) +
#   ggplot2::geom_ribbon(ggplot2::aes(
#     ymin = `0.1`,
#     ymax = `0.9`),
#     alpha = .5,
#   ) +
#   ggplot2::geom_vline(xintercept = min(years_observed) -.3, linetype = "dotted",position = ggplot2::position_dodge(0.3)) +
#   ggplot2::facet_wrap(vars(parameter), scale = "free")
