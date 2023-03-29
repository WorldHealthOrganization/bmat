

plot_bmat_onecountry3 <- function(
    tar_depends = NULL,
    round_name = NULL,
    round_name_2 = NULL,
    round_name_3 = NULL,
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
  
    main_path_2 <- make_output_directory_return_path(round_name_2, iso_alpha_3_code, global_run = FALSE, bmis_or_bmat = "bmat")
    estimates_onecountry_2 <- readRDS(here::here(main_path_2, "estimates.rds")) %>% dplyr::filter(parameter %in% c("pm", "mmr"))
    estimates_onecountry_2 <- estimates_onecountry_2 %>% 
      dplyr::filter(iso_alpha_3_code == !!iso_alpha_3_code) %>%
      dplyr::filter(year_mid %in% seq(min(year_range), max(year_range))) %>%
      tibble::add_column(estimate_version = paste0("80% CI ", round_name_2)) %>%
      dplyr::mutate(estimate_version = as.factor(estimate_version))
    main_path_3 <- make_output_directory_return_path(round_name_3, iso_alpha_3_code, global_run = FALSE, bmis_or_bmat = "bmat")
    estimates_onecountry_3 <- readRDS(here::here(main_path_3, "estimates.rds")) %>% dplyr::filter(parameter %in% c("pm", "mmr"))
    estimates_onecountry_3 <- estimates_onecountry_3 %>% 
      dplyr::filter(iso_alpha_3_code == !!iso_alpha_3_code) %>%
      dplyr::filter(year_mid %in% seq(min(year_range), max(year_range))) %>%
      tibble::add_column(estimate_version = paste0("80% CI ", round_name_3)) %>%
      dplyr::mutate(estimate_version = as.factor(estimate_version))
  
  
  plot_bmat_onecountry_sub3(
    main_data_adjusted = readRDS(here::here(main_path, "main_data_adjusted.rds")),
    estimates_onecountry = estimates_onecountry,
    estimates_onecountry_2 = estimates_onecountry_2,
    estimates_onecountry_3 = estimates_onecountry_3,
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
plot_bmat_onecountry_sub3 <- function(
    main_data_adjusted,
    estimates_onecountry,
    estimates_onecountry_2 = NULL,
    estimates_onecountry_3 = NULL,
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
  

    estimates_onecountry_2 <- estimates_onecountry_2 %>% 
      dplyr::filter(iso_alpha_3_code == !!iso_alpha_3_code) %>%
      dplyr::filter(year_mid %in% seq(min(year_range), max(year_range)))
    
    estimates_onecountry_3 <- estimates_onecountry_3 %>% 
      dplyr::filter(iso_alpha_3_code == !!iso_alpha_3_code) %>%
      dplyr::filter(year_mid %in% seq(min(year_range), max(year_range)))
  
  
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
    estimates <- rbind(estimates_onecountry, estimates_onecountry_2, estimates_onecountry_3)
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
                 alphavalue = .225,
                 has_comparison = !(is.null(estimates_onecountry_2) & is.null(estimates_to_compare_la1)))
  return(p)
  
}
