plot_bmis <- function(round_name, round_last_year, iso_alpha_3_codes) {
  pdf(here::here("output", round_name, "bmis_estimate_plots.pdf"))
  for (selected_country_iso in iso_alpha_3_codes){
    estimates <- readRDS(here::here("output", round_name, "bmis_onecountry", selected_country_iso, "estimates.rds"))
    main_data_for_plots <-
      readRDS(here::here("output", round_name, "bmis_onecountry", selected_country_iso, "main_data_for_plots.rds"))
    sens_spec <-
      readRDS(here::here("output", round_name, "bmis_onecountry", "sens_spec_countries_w_data.rds")) %>%
      dplyr::filter(iso_alpha_3_code == selected_country_iso)
    print(plot_bmis_one_country(
      country_ref = country_ref,
      estimates = estimates,
      estimates_old = NULL,
      sens_spec = sens_spec,
      main_data_for_plots = main_data_for_plots,
      iso_alpha_3_code = selected_country_iso
    ) )
  }
  dev.off()
  pdf(here::here("output", round_name, "bmis_global_crvs_adjustment.pdf"))
  print(plot_bmis_global_adjustment(round_name,
                                    round_last_year,
                                    global_run = TRUE))
  dev.off()
}

plot_bmis_global_adjustment <- function(round_name,
                                        round_last_year,
                                        global_run = TRUE) {
  if (global_run) {
    sens_spec_global <- readRDS(here::here("output", round_name, "bmis_global", "sens_spec_global.rds"))
    global_sens <- sens_spec_global %>% dplyr::filter(year_start == round_last_year) %>% dplyr::pull(sens)
    truepm_vec <- seq(0,0.05,0.0001)
    lengthpm <- length(truepm_vec)
    spec_vec <- rep(1,lengthpm)
    spec_vec2 <- rep(.9999,lengthpm)
    spec_vec3 <- rep(.9993,lengthpm)
    spec_vec4 <- rep(.999,lengthpm)
    temp <- data.frame(truepm = rep(truepm_vec,4), 
                       spec = c(spec_vec, spec_vec2, spec_vec3, spec_vec4),
                       sens = rep(global_sens, lengthpm*4)) %>%
      dplyr::mutate(adjustment = truepm / (truepm*sens + (1-spec)*(1-truepm))) %>%
      dplyr::filter(truepm > .001) %>%
      dplyr::filter(adjustment > 1.1)
    #truepm / (truepm*sens + (1-spec)*(1-truepm))
    pl <- ggplot2::ggplot(temp, ggplot2::aes(x=truepm, y=adjustment, col =as.factor(spec))) +
      ggplot2::geom_line(size=1) +
      ggplot2::ylim(1, max(temp$adjustment + .1)) +
      ggplot2::theme_gray(base_size = 20) +
      ggplot2::ylab("CRVS adjustment") +
      ggplot2::xlab("True PM") + 
      ggplot2::scale_color_discrete(name = "Specificity")
    return(pl)
  }
}


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
