
calculate_bmat <- function(
  fit,
  meta,
  jags_list,
  quantiles = c(0.1, 0.5, 0.9),
  nlag = 1,
  arr_periods = list(c(2000,2015), c(2000, 2020), c(2010, 2020), c(2000, 2005), c(2005, 2010), c(2010, 2015), c(2015, 2020), c(2016, 2020)),
  main_path
) {
  
  mu_draws <- fit$BUGSoutput %>% tidybayes::spread_draws(mu.ct[c,t]) # warning: requires R2jags or associated package to be loaded to understand this is an mcmc object
  
  crisis_deaths <- matrix_ct_to_df(matrix = meta$crisisdeaths.ct, C = meta$C, value_name = "crisis_deaths")
  deaths <- matrix_ct_to_df(matrix = meta$deaths.ct, C = meta$C, value_name = "deaths")
  births <- matrix_ct_to_df(matrix = meta$births.ct, C = meta$C, value_name = "births")
  aids <-  matrix_ct_to_df(matrix = jags_list$muaids.ct, C = meta$C, value_name = "aids")
  gfr <-  matrix_ct_to_df(matrix = meta$gfr.ct, C = meta$C, value_name = "gfr")
  t15t50 <- matrix_ct_to_df(matrix = meta$t15t50.ct, C = meta$C, value_name = "t15t50")
  l15 <- matrix_ct_to_df(matrix = meta$l15.ct, C = meta$C, value_name = "l15")
  
  meta_df <- crisis_deaths %>% 
    dplyr::left_join(deaths) %>% 
    dplyr::left_join(births) %>% 
    dplyr::left_join(aids) %>%
    dplyr::left_join(gfr) %>%
    dplyr::left_join(t15t50) %>%
    dplyr::left_join(l15) %>%
    dplyr::mutate(women = births/gfr)
  
  isos <- meta$iso.c
  years <- meta$year.t
  iso_df <- data.frame(c = 1:length(isos), iso_alpha_3_code = isos)
  years_df <- data.frame(t = 1:length(years), year = years)
  
  # join columns with readable years and isos, join covariate data
  mu_draws <- mu_draws %>% 
    dplyr::left_join(meta_df) %>%
    dplyr::left_join(iso_df) %>% 
    dplyr::left_join(years_df)
  
  # adjust for crisis deaths
  draws_w_calculation <- mu_draws %>% 
    dplyr::group_by(iso_alpha_3_code, year) %>%
    dplyr::mutate(
      mmr = ifelse(crisis_deaths > 0,
                   add_crisis_uncertainty(mu = mu.ct,
                              births = births, 
                              crisis_deaths = crisis_deaths),
                   mu.ct
      ),
      women = births/gfr,
      ltr = mmr*births/women*t15t50/l15,
      maternal_deaths = mmr*births,
      mmrate = mmr*births/women
    ) %>%
    dplyr::group_by(iso_alpha_3_code, year,`.draw`)
  rm("mu_draws")
  # above here is all generic
  ###################################
  

  arr_list<-list()
  for(i in 1:length(arr_periods)) {
    arr_year_start <- arr_periods[[i]][1]
    arr_year_end <- arr_periods[[i]][2]
    arr_list[[i]] <- draws_w_calculation %>%
      dplyr::ungroup(year) %>%
      dplyr::filter(year %in% c(arr_year_start, arr_year_end)) %>%
      dplyr::mutate(yearlag = year - dplyr::lag(year, n = nlag)) %>%
      dplyr::mutate(mmrlag = dplyr::lag(mmr, n = nlag)) %>%
      dplyr::mutate(arr = -(log(mmr/mmrlag)/yearlag)*100) %>% # using continuous form of reduction
      dplyr::mutate(percent_change_in_mmr = ((mmrlag - mmr)/mmrlag)*100) %>% 
      tidyr::drop_na() %>%
      dplyr::ungroup(`.draw`) %>%
      dplyr::summarise(arr = quantile(arr, quantiles, na.rm = TRUE),
                       percent_change_in_mmr = quantile(percent_change_in_mmr, quantiles, na.rm = TRUE)) %>%
      dplyr::mutate(quantiles = quantiles) %>%
      dplyr::mutate(period = paste0(arr_year_start,", ", arr_year_end)) %>%
      dplyr::relocate(period)%>%
      tidyr::pivot_longer(cols = c("arr",
                                   "percent_change_in_mmr"), names_to = "parameter", values_to = "value") %>%
      tidyr::pivot_wider(names_from = quantiles, values_from = value)
  }
  estimates_arr <- dplyr::bind_rows(arr_list)
  # quantiles summarise
  estimates <- calculate_bmat_summarise(draws_w_calculation = draws_w_calculation,
                           quantiles = quantiles
  )
  
  saveRDS(draws_w_calculation, here::here(file.path(main_path, "draws_w_calculation.rds")))
  saveRDS(estimates, here::here(file.path(main_path, "estimates.rds")))
  saveRDS(estimates_arr, here::here(file.path(main_path, "estimates_arr.rds")))
  return(list(estimates = estimates,
              estimates_arr = estimates_arr,
              draws_w_calculation = draws_w_calculation))
} 




matrix_ct_to_df <- function(matrix, C, value_name) {
  df <- matrix %>%
    as.data.frame() %>%
    dplyr::mutate(c = 1:C) %>%
    tidyr::gather("t", {{value_name}}, -c) %>%
    dplyr::mutate(t = t %>% stringr::str_extract_all("[0-9]+") %>% as.numeric)
  return(df)
}
matrix_ct_to_df_with_isoyear <- function(meta, value_matrix, value_name = "value") {
  matrix <- matrix_ct_to_df(matrix = value_matrix, C = meta$C, value_name = !!value_name)
  isos <- meta$iso.c
  years <- meta$year.t
  iso_df <- data.frame(c = 1:length(isos), iso_alpha_3_code = isos)
  years_df <- data.frame(t = 1:length(years), year = years)
  value_df <- matrix %>%
    dplyr::left_join(years_df, by = c("t")) %>%
    dplyr::left_join(iso_df, by = c("c")) %>%
    dplyr::mutate(iso_alpha_3_code = as.character(iso_alpha_3_code))
}

add_crisis_uncertainty <- function(mu, #deaths,
                       births, 
                       crisis_deaths){
  matdeaths_nocrisis.s <- mu*births
  set.seed(1234)
  # upper bound based on ratio max maternal deaths plus x% of crisis deaths to max maternal deahts
  ratio_max <- min(1.2, (max(matdeaths_nocrisis.s) + 0.5*crisis_deaths)/max(matdeaths_nocrisis.s))
  mult <- sort(truncnorm::rtruncnorm(length(matdeaths_nocrisis.s), mean = 1, sd = 0.15, a = 1- (ratio_max - 1)/2, b = ratio_max))
  matdeaths.s <- matdeaths_nocrisis.s*(mult[rank(matdeaths_nocrisis.s)])
  if (any((matdeaths.s - matdeaths_nocrisis.s) > crisis_deaths)){
    print("additional deaths for crisis uncertainty greater than crisis deaths, no unc added")
    matdeaths.s <- matdeaths_nocrisis.s
  }
  mmr.s <- matdeaths.s/births
  return(mmr.s)
}

