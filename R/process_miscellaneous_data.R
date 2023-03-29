process_miscellaneous_data <- function(
  misc_data,
  meta,
  meta_precrisis,
  round_first_year,
  round_last_year,
  round_name
) {
  #impute live births and evn_total first
  misc_data_p <- misc_data %>%
    # dplyr::mutate(include_reason = ifelse(!iso_alpha_3_code %in% meta$iso.c, "No lifetables for this country or state", NA)) %>%
    dplyr::filter(iso_alpha_3_code %in% meta$iso.c) %>%
    dplyr::mutate(include_reason = ifelse(include_reason == "subnational", "Study not nationally representative", include_reason)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(env_total_lifetables = ifelse(year_start >= round_first_year & year_end <= round_last_year + 1 & iso_alpha_3_code %in% meta$iso.c,
                                             impute_env_total(year_start = year_start,
                                                              year_end = year_end,
                                                              iso_alpha_3_code = iso_alpha_3_code,
                                                              meta = meta_precrisis),
                                             NA)) %>%
    dplyr::mutate(live_births_lifetables = ifelse(year_start >= round_first_year & year_end <= round_last_year + 1 & iso_alpha_3_code %in% meta$iso.c,
                                               impute_live_births(year_start = year_start,
                                                                  year_end = year_end,
                                                                  iso_alpha_3_code = iso_alpha_3_code,
                                                                  meta = meta_precrisis),
                                               NA))
  # calculate pm if possible
  misc_data_p <- misc_data_p %>%
    dplyr::mutate(
      mmr = ifelse(is.na(mmr) & !is.na(env_mat) & !is.na(live_births), env_mat/live_births * 10^(5), mmr)
    ) %>%
    dplyr::mutate(
      final_pm = ifelse(is.na(final_pm) & definition == "maternal" & !is.na(env_mat) & !is.na(env_total), env_mat / env_total,
                        ifelse(is.na(final_pm) & definition == "pregn" & is.na(env_mat) & !is.na(env_pregrelated) & !is.na(env_total), env_pregrelated / env_total, final_pm
        )
      )
    ) %>%
    dplyr::mutate(
      final_pm = ifelse(
        is.na(final_pm) & !is.na(mmr), mmr * live_births_lifetables * 10^(-5) / env_total_lifetables, final_pm
      )
    ) %>%
    dplyr::mutate(year_mid = 1 / 2 * (year_start + year_end),
                  type = "misc",
                  obs_selogpm = NA,
                  include_reason = ifelse(is.na(final_pm) & include, "PM could not be obtained", include_reason),
                  include_reason = ifelse(final_pm > 1 & include, "PM could not be obtained", include_reason),
                  include = ifelse(!is.na(include_reason), FALSE, include))
  write.csv(misc_data_p, row.names = FALSE, here::here("output", round_name, "miscellaneous.csv"))
}