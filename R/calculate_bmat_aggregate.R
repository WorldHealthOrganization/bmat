calculate_bmat_aggregate <- function(round_name,
                                     iso_alpha_3_codes,
                                     aggregate_group_data,
                                     arr_periods = NULL,
                                     nlag = 1,
                                     quantiles = c(0.1, 0.5, 0.9),
                                     type_of_group = NULL) {
  if(is.null(type_of_group)) {
    stop("need type_of_group input to name the files with appropriate names")
  }
  dl <- list()
  dl2 <- list()
  for(iso in iso_alpha_3_codes) {
    main_path <- make_output_directory_return_path(round_name, iso, FALSE, bmis_or_bmat = "bmat")
    dl[[iso]] <- readRDS(here::here(main_path, "draws_w_calculation.rds"))
    dl2[[iso]] <- readRDS(here::here(main_path, "estimates.rds")) %>%
      dplyr::filter(parameter == "maternal_deaths") %>%
      dplyr::select(iso_alpha_3_code, year = year_mid, maternal_deaths = `0.5`)
  }
  # WHO prefers sum of country level estimated maternal deaths opposed to aggregated maternal death samples so we need this extra df
  mat_deaths_df <- dplyr::bind_rows(dl2) %>%
    dplyr::left_join(aggregate_group_data, by = c("iso_alpha_3_code")) %>%
    dplyr::group_by(group, year) %>%
    dplyr::summarise(maternal_deaths_summation_of_country_estimates = sum(maternal_deaths))
  # this is the main df of samples being manipulated before aggregation in the following function
  draws_w_calculation <- dplyr::bind_rows(dl)
  draws_w_calculation <- aggregate_group_data %>%
    dplyr::ungroup() %>%
    dplyr::left_join(draws_w_calculation, by = c("iso_alpha_3_code")) %>%
    # dplyr::mutate(women = births/gfr,
    #               ltr = mmr*births/women*t15t50/l15,
    #               mmrate = mmr*births/women,
    #               maternal_deaths = mmr*births,
    #               survival = t15t50/l15) %>%
    dplyr::group_by(group, year, `.draw`) %>%
    dplyr::mutate(weight_denominator = sum(births)) %>%
    dplyr::mutate(weight_denominator_ltr = sum(women*l15)) %>%
    dplyr::mutate(weight_denominator_mmrate = sum(women)) %>%
    # dplyr::mutate(weight_denominator_survival = sum(l15)) %>%
    dplyr::mutate(weight = births / weight_denominator) %>%
    dplyr::mutate(weight_ltr = (women*l15) / weight_denominator_ltr) %>%
    # dplyr::mutate(weight_mmrate = (women) / weight_denominator_mmrate) %>%
    # dplyr::mutate(weight_survival = (l15) / weight_denominator_survival) %>%
    dplyr::summarise(
      mmr = sum(mmr * weight),
      aids = sum(aids * weight),
      # mmrate = sum(mmrate * weight_mmrate),
      # survival = sum(survival * weight_survival),
      ltr = sum(ltr * weight_ltr),
      # ltr2 = sum(mmrate * survival),
      births = sum(births),
      deaths = sum(deaths),
      women = sum(women),
      maternal_deaths = sum(maternal_deaths)
    ) %>%
    dplyr::group_by(group, year, `.draw`)
  
  
  aggregate_estimates <-
    calculate_bmat_summarise(draws_w_calculation,
                             mat_deaths_df = mat_deaths_df,
                             quantiles = quantiles)
  
  write.csv(aggregate_estimates, here::here("output",round_name, paste0("aggregate_estimates_rt_", type_of_group,".csv")))
  write.csv(aggregate_estimates %>%
              dplyr::filter(year_mid == 2017) %>%
              dplyr::filter(parameter == "mmr"), here::here("output",round_name, paste0("aggregate_estimates_r_", type_of_group,".csv")))
  
  if (length(arr_periods)>1) {
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
                       percent_change_in_mmr = quantile(percent_change_in_mmr, !!quantiles, na.rm = TRUE)) %>%
      dplyr::mutate(quantiles = quantiles) %>%
      dplyr::mutate(period = paste0(arr_year_start,", ", arr_year_end)) %>%
      dplyr::relocate(period)%>%
      tidyr::pivot_longer(cols = c("arr",
                                   "percent_change_in_mmr"), names_to = "parameter", values_to = "value") %>%
      tidyr::pivot_wider(names_from = quantiles, values_from = value)
  }
  estimates_arr <- dplyr::bind_rows(arr_list)
  # write.csv(aggregate_estimates, here::here("output",round_name, paste0("aggregate_estimates_rt_", type_of_group,".csv")))
  write.csv(estimates_arr, here::here("output",round_name, paste0("aggregate_arr_r_", type_of_group,".csv")))
  }
}