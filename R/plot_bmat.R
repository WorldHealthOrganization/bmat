plot_bmat_w_inputs <- function(round_name, 
                               round_name_2 = NULL, 
                               iso_alpha_3_codes) {
  library(ggplot2) #ggtitle requires loading the package (qualifying it alone does not work)
  
  country_ref = read.csv( 
    here::here("output", round_name, "country_ref.csv"))
  metanew = readRDS(here::here("output", round_name, "meta.rds"))
  
  if (!is.null(round_name_2)) {
    metaold = readRDS(here::here("output", round_name, "meta.rds"))
    series_name_new = round_name 
    series_name_old = round_name_2
  } else {
    metaold = NULL
    series_name_new = NULL 
    series_name_old = NULL
  }
  
  input_data <- input_data_c_wrapper(
    metanew = metanew,
    metaold = metaold, 
    series_name_new = series_name_new, 
    series_name_old = series_name_old
  )
  pl <- list()
  pl2 <- list()
  for (iso in iso_alpha_3_codes) {
    name <- country_ref %>% dplyr::filter(iso_alpha_3_code == iso) %>% dplyr::pull(name_short_en)
    pl[[iso]] <- bmat:::plot_bmat_onecountry(
      round_name =  round_name,
      round_name_2 = round_name_2,
      iso_alpha_3_code = iso,
      year_range = c(2000, round_last_year)) + ggtitle(name)
    
    
    pl2[[iso]] <- ggplot2::ggplot(input_data %>% dplyr::filter(iso_alpha_3_code == !!iso)) +
      ggplot2::geom_line(aes(x = year, y = value, col = series_name)) +
      ggplot2::ggtitle(name) +
      ggplot2::facet_wrap(~value_name, scales = "free_y")
  }
  
  pdf(here::here("output", round_name, "bmat_plots_w_inputs.pdf"), 16,8)
  for(i in 1:length(pl)) {
    layout(matrix(1:2, ncol=2))
    
    print(pl[[i]])
    print(pl2[[i]])
  }
  dev.off()
}

#' plot_bmat_onecountry
#' @export
#'
#' @examples
plot_bmat_onecountry <- function(
  tar_depends = NULL,
  round_name = NULL,
  round_name_2 = NULL,
  iso_alpha_3_code,
  year_range = NULL,
  caption = "",
  estimates_to_compare_la1 = NULL,
  meta_for_compare_la1 = NULL
){
  country_ref = read.csv( 
    here::here("output", round_name, "country_ref.csv"))
  # suppressMessages({
  # for the purpose of plotting we are only using pm and mmr , we also want a column to specify which version is which in the plot
  main_path <- make_output_directory_return_path(round_name, iso_alpha_3_code, global_run = FALSE, bmis_or_bmat = "bmat")
  estimates_onecountry <- readRDS(here::here(main_path, "estimates.rds")) %>% dplyr::filter(parameter %in% c("pm", "mmr"))
  estimates_onecountry <- estimates_onecountry %>% 
    dplyr::filter(iso_alpha_3_code == !!iso_alpha_3_code) %>%
    dplyr::filter(year_mid %in% seq(min(year_range), max(year_range))) %>%
    tibble::add_column(estimate_version = paste0("80% CI ", round_name)) %>%
    dplyr::mutate(estimate_version = as.factor(estimate_version))
  
  if(!is.null(round_name_2)) {
    main_path_2 <- make_output_directory_return_path(round_name_2, iso_alpha_3_code, global_run = FALSE, bmis_or_bmat = "bmat")
    estimates_onecountry_2 <- readRDS(here::here(main_path_2, "estimates.rds")) %>% dplyr::filter(parameter %in% c("pm", "mmr"))
    estimates_onecountry_2 <- estimates_onecountry_2 %>% 
      dplyr::filter(iso_alpha_3_code == !!iso_alpha_3_code) %>%
      dplyr::filter(year_mid %in% seq(min(year_range), max(year_range))) %>%
      tibble::add_column(estimate_version = paste0("80% CI ", round_name_2)) %>%
      dplyr::mutate(estimate_version = as.factor(estimate_version))
  } else {
    estimates_onecountry_2 <- NULL
  }


  
  plot_bmat_onecountry_sub(
      main_data_adjusted = readRDS(here::here(main_path, "main_data_adjusted.rds")),
      estimates_onecountry = estimates_onecountry,
      estimates_onecountry_2 = estimates_onecountry_2,
      iso_alpha_3_code = iso_alpha_3_code,
      year_range = year_range,
      caption = caption,
      estimates_to_compare_la1 = estimates_to_compare_la1,
      meta_for_compare_la1 = meta_for_compare_la1,
      country_ref = country_ref
    )
  # ml <- gridExtra::grid.arrange(plots[[1]], plots[[2]], nrow = 1)
  # ggplot2::ggsave(here::here(main_path, "plots.pdf"), ml, width = 12, height = 5)
  # dev.off()
  # })
}



#' plot_bmat_onecountry_sub
#'
#' @param fit_list
#'
#' @return
#' @export
#'
plot_bmat_onecountry_sub <- function(
  main_data_adjusted,
  estimates_onecountry,
  estimates_onecountry_2 = NULL,
  iso_alpha_3_code,
  year_range,
  caption,
  estimates_to_compare_la1,
  meta_for_compare_la1,
  country_ref
){
  country_ref <- country_ref %>% dplyr::select(iso_alpha_3_code, name_short_en)
  main_data_adjusted <- main_data_adjusted %>% 
    dplyr::filter(iso_alpha_3_code == !!iso_alpha_3_code) %>%
    dplyr::mutate(type = as.character(type)) %>%
    dplyr::mutate(type = ifelse(type == "inq", "specialized study",
                                ifelse(type == "vr", "CRVS",
                                       ifelse(type == "misc", "miscellaneous",
                                              ifelse(type == "dhs", "survey",
                                                     type))))) %>%
    dplyr::mutate(year_mid_floored = floor(year_mid)) %>%
    dplyr::filter(year_mid_floored %in% seq(min(year_range), max(year_range))) %>%
    dplyr::left_join(country_ref, by = c("iso_alpha_3_code"))
  
  estimates_onecountry <- estimates_onecountry %>% 
    dplyr::filter(iso_alpha_3_code == !!iso_alpha_3_code) %>%
    dplyr::filter(year_mid %in% seq(min(year_range), max(year_range)))
  
  if(!is.null(estimates_onecountry_2)){
  estimates_onecountry_2 <- estimates_onecountry_2 %>% 
    dplyr::filter(iso_alpha_3_code == !!iso_alpha_3_code) %>%
    dplyr::filter(year_mid %in% seq(min(year_range), max(year_range)))
  }
  
  mmrdata <- main_data_adjusted %>%
    dplyr::select(iso_alpha_3_code, year_mid, year_start, year_end, mmr.adj.postmod, mmr.CI.up.postmod, mmr.CI.low.postmod, type, include) %>%
    dplyr::rename(value = mmr.adj.postmod,
                  value_low = mmr.CI.up.postmod,
                  value_up = mmr.CI.low.postmod) %>%
    tibble::add_column(parameter = "mmr") %>%
    dplyr::mutate(value_type = ifelse(type == "specialized study", "input data","model adjusted data")) %>%
    dplyr::mutate(value = value*100000) %>%
    dplyr::mutate(value_low = value_low*100000) %>%
    dplyr::mutate(value_up = value_up*100000)

  mmrdata2 <- main_data_adjusted %>%
    dplyr::select(iso_alpha_3_code, year_mid, year_start, year_end, final_mmr, type, include) %>%
    dplyr::rename(value = final_mmr) %>%
    tibble::add_column(parameter = "mmr") %>%
    tibble::add_column(value_type = "input data") %>%
    dplyr::mutate(value = value*100000) %>%
    dplyr::filter(type != "specialized study")
  
  
  pmdata <- main_data_adjusted %>%
    dplyr::select(iso_alpha_3_code, year_mid, year_start, year_end, pm.adj.postmod, CI.up.postmod, CI.low.postmod, type, include) %>%
    dplyr::rename(value = pm.adj.postmod,
                  value_low = CI.up.postmod,
                  value_up = CI.low.postmod) %>%
    tibble::add_column(parameter = "pm") %>%
    dplyr::mutate(value_type = ifelse(type == "specialized study", "input data","model adjusted data"))
    
  pmdata2 <- main_data_adjusted %>%
    dplyr::select(iso_alpha_3_code, year_mid, year_start, year_end, final_pm, type, include) %>%
    dplyr::rename(value = final_pm) %>%
    tibble::add_column(parameter = "pm")%>%
    tibble::add_column(value_type = "input data")  %>%
    dplyr::filter(type != "specialized study")
  if(nrow(mmrdata) > 0) {
    observed_data <- dplyr::bind_rows(mmrdata, mmrdata2, pmdata, pmdata2)
    # observed_data <- dplyr::bind_rows(pmdata, pmdata2)
    
  } else {
    observed_data <- mmrdata
  }
  observed_data <- observed_data %>%
    dplyr::filter(include)
  
  
  # Hacking LA1 to look like our current format - Overwrites estimates_onecountry_2 with la1 if given as an input
  if(!is.null(estimates_to_compare_la1)) {
  estimates_to_compare_mmr <- estimates_to_compare_la1 %>%
    pluck_and_reformat_ci("mmr.cqt", iso_alpha_3_code, meta = meta_for_compare_la1) %>%
    tibble::add_column(estimate_version = "80% CI version la1(similar to m60)") %>%
    dplyr::mutate(estimate_version = as.factor(estimate_version)) %>%
    dplyr::mutate(year_mid = as.numeric(year_mid)) %>%
    dplyr::mutate(q0.1 = q0.1*100000) %>%
    dplyr::mutate(q0.5 = q0.5*100000) %>%
    dplyr::mutate(q0.9 = q0.9*100000) %>%
    dplyr::rename(`0.1` = q0.1,
                  `0.5` = q0.5,
                  `0.9` = q0.9) %>%
    tibble::add_column(parameter = "mmr") %>%
    dplyr::filter(year_mid %in% seq(min(year_range), max(year_range)))
 
  estimates_to_compare_pm <- estimates_to_compare_la1 %>%
    pluck_and_reformat_ci("pm.cqt", iso_alpha_3_code, meta = meta_for_compare_la1) %>%
    tibble::add_column(estimate_version = "80% CI version la1(similar to m60)") %>%
    dplyr::mutate(estimate_version = as.factor(estimate_version)) %>%
    dplyr::mutate(year_mid = as.numeric(year_mid)) %>%
    dplyr::rename(`0.1` = q0.1,
                  `0.5` = q0.5,
                  `0.9` = q0.9) %>%
    tibble::add_column(parameter = "pm") %>%
    dplyr::filter(year_mid %in% seq(min(year_range), max(year_range)))

  estimates_onecountry_2 <- rbind(estimates_to_compare_mmr, estimates_to_compare_pm) %>%
    tibble::add_column(iso_alpha_3_code = !!iso_alpha_3_code)  %>%
    dplyr::filter(year_mid %in% seq(min(year_range), max(year_range)))
  }
  ############################################################################
  

  if(is.null(estimates_onecountry_2)) {
    estimates <- estimates_onecountry
  } else {
    estimates <- rbind(estimates_onecountry, estimates_onecountry_2)
  }
  
  
  
  estimates <- estimates %>% dplyr::mutate(year_mid = as.character(year_mid))
  observed_data <- observed_data %>% dplyr::mutate(year_mid = as.character(year_mid))
  
  data <- estimates %>% 
    dplyr::full_join(observed_data, by = c("year_mid", "parameter"))  %>%
    dplyr::mutate(parameter = as.factor(parameter)) %>% 
    dplyr::mutate(year_mid = as.numeric(year_mid))
  
  has_observed_data <- nrow(observed_data) > 0
  # temp <- data[order(data$parameter),]
  p <- plot_bmat(data,
            iso_alpha_3_code,
            caption = caption,
            has_observed_data = has_observed_data,
            has_comparison = !(is.null(estimates_onecountry_2) & is.null(estimates_to_compare_la1)))
  return(p)

}




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

