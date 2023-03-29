process_census_data <- function(
  census_data,
  meta,
  meta_precrisis,
  round_first_year,
  round_last_year,
  round_name
  ) {
  censusdata <- census_data %>%
    dplyr::rename(iso_alpha_3_code = iso,
                  env_pregrelated = pregdeaths,
                  env_total = deaths,
                  year_start = start.date,
                  year_end = end.date) %>%
    dplyr::mutate(env_pregrelated = as.numeric(env_pregrelated),
                  env_total = as.numeric(env_total),
                  pm = as.numeric(pm),
                  mmr = as.numeric(mmr))
  
  censusdata$citation_short <- "Census"
  
  census2 <- censusdata %>%
    dplyr::filter(include_reason != "duplicate in miscellaneous data" | is.na(include_reason)) %>% # allows for NA's and just removes if duplicate in misc
    dplyr::filter(iso_alpha_3_code %in% meta$iso.c) %>%
    dplyr::mutate(include = include,
                  include_reason = ifelse(include, NA, "Quality of census measurement not clear")) %>%
    dplyr::mutate(
      year_mid = 1 / 2 * (year_start + year_end)
    ) 
    ###############  Calculations if PM is missing ######################
    
  census3 <- census2 %>%
    dplyr::rowwise() %>%
    dplyr::mutate(env_total_imputed = ifelse(year_start >= round_first_year & year_end <= round_first_year & iso_alpha_3_code %in% meta$iso.c,
                                             impute_env_total(year_start = year_start,
                                                              year_end = year_end,
                                                              iso_alpha_3_code = iso_alpha_3_code,
                                                              meta = meta_precrisis),
                                             NA)) %>%
    dplyr::mutate(live_births_imputed = ifelse(year_start >= round_first_year & year_end <= round_last_year & iso_alpha_3_code %in% meta$iso.c,
                                               impute_live_births(year_start = year_start,
                                                                  year_end = year_end,
                                                                  iso_alpha_3_code = iso_alpha_3_code,
                                                                  meta = meta_precrisis),
                                               NA))
  census3 <- census3 %>%
    dplyr::mutate(
      final_pm = 
        ifelse(is.na(pm) & !is.na(env_pregrelated) & !is.na(env_total),
               env_pregrelated / env_total,
               ifelse(is.na(pm) & !is.na(mmr),
                      mmr * live_births_imputed * 10^(-5) / env_total_imputed, 
                      pm
               )
        )
    ) %>%
    dplyr::mutate(include_reason = ifelse(is.na(final_pm) & include, "PM could not be obtained", include_reason),
                  include_reason = ifelse(final_pm > 1 & include, "PM could not be obtained", include_reason),
                  include = ifelse(!is.na(include_reason), FALSE, include))
    
    ####################################################
    
  census3 <- census3 %>%
    dplyr::select(iso_alpha_3_code, year_mid, year_start, year_end, final_pm, include, include_reason,
                  citation_short, env_total_imputed, live_births_imputed) %>%
    dplyr::mutate(
      obs_selogpm = NA,
      type = "census",
      definition = "pregn"
    )
  write.csv(census3, row.names = FALSE, here::here("output", round_name, "census.csv"))
  return(NULL)
}