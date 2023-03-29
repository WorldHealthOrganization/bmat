calculate_bmat_summarise <- function(draws_w_calculation,
                                     quantiles = c(0.1, 0.5, 0.9),
                                     mat_deaths_df = NULL
) {
  estimates <- draws_w_calculation %>%
    dplyr::ungroup(`.draw`) %>%
    dplyr::summarise(mmr = quantile(mmr, quantiles, na.rm = TRUE),
                     maternal_deaths = quantile(maternal_deaths, quantiles, na.rm = TRUE),
                     lifetime_risk = quantile(ltr, quantiles, na.rm = TRUE),
                     mmr_mean = mean(mmr),
                     births = births[1],
                     aids = aids[1],
                     deaths = deaths[1],
                     women = women[1]
    ) %>%
    dplyr::mutate(quantiles = quantiles) %>%
    dplyr::rename(hiv_related_indirect_mmr = aids)
  
  # only have this df if we are aggregating
  if(!is.null(mat_deaths_df)) {
  estimates <- estimates %>%
    dplyr::rename(maternal_deaths_aggregated_samples = maternal_deaths) %>%
    dplyr::left_join(mat_deaths_df, by = c("group", "year")) %>%
    dplyr::mutate(
                  mmr_rate = maternal_deaths_summation_of_country_estimates/women,
                  hiv_related_indirect_maternal_deaths = hiv_related_indirect_mmr*births,
                  hiv_related_indirect_percentage = hiv_related_indirect_maternal_deaths/maternal_deaths_summation_of_country_estimates*100,
                  pm = mmr*births/deaths,
                  pm_mean = mmr_mean*births/deaths,
                  mmr = mmr*100000,
                  mmr_mean = mmr_mean*100000,
                  births = births,
                  lifetime_risk_1_in = round(1/lifetime_risk) #(lifetime_risk)/(1-lifetime_risk)) #if we want odds
    ) %>% 
    dplyr::select(-women, -deaths) %>%
    dplyr::rename(year_mid = year) %>%
    tidyr::pivot_longer(cols = c(
                                 "mmr",
                                 "pm",
                                 "maternal_deaths_aggregated_samples",
                                 "maternal_deaths_summation_of_country_estimates",
                                 "mmr_rate",
                                 "pm",
                                 "pm_mean",
                                 "mmr_mean",
                                 "births",
                                 "hiv_related_indirect_mmr",
                                 "hiv_related_indirect_maternal_deaths",
                                 "hiv_related_indirect_percentage",
                                 "lifetime_risk",
                                 "lifetime_risk_1_in"), names_to = "parameter", values_to = "value") %>%
    tidyr::pivot_wider(names_from = quantiles, values_from = value)
  } else {
    estimates <- estimates %>%
      dplyr::mutate(
        mmr_rate = maternal_deaths/women,
        hiv_related_indirect_maternal_deaths = hiv_related_indirect_mmr*births,
        hiv_related_indirect_percentage = hiv_related_indirect_maternal_deaths/maternal_deaths*100,
        pm = mmr*births/deaths,
        pm_mean = mmr_mean*births/deaths,
        mmr = mmr*100000,
        mmr_mean = mmr_mean*100000,
        births = births,
        lifetime_risk_1_in = round(1/lifetime_risk) #(lifetime_risk)/(1-lifetime_risk)) #if we want odds
      ) %>% 
      dplyr::select(-women, -deaths) %>%
      dplyr::rename(year_mid = year) %>%
      tidyr::pivot_longer(cols = c(
        "mmr",
        "pm",
        "maternal_deaths",
        "mmr_rate",
        "pm",
        "pm_mean",
        "mmr_mean",
        "births",
        "hiv_related_indirect_mmr",
        "hiv_related_indirect_maternal_deaths",
        "hiv_related_indirect_percentage",
        "lifetime_risk",
        "lifetime_risk_1_in"), names_to = "parameter", values_to = "value") %>%
      tidyr::pivot_wider(names_from = quantiles, values_from = value)
  }
  return(estimates)
}