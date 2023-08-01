
target_table <- function(
    estimates,
    country_ref) {
  country_ref <- country_ref %>% 
    dplyr::select(iso_alpha_3_code, name_short_en, SDG_Region, SDG_Sub_Region)
  targets <- estimates %>% 
    dplyr::filter(parameter == "mmr") %>%
    dplyr::rename(mmr = X0.5) %>%
    dplyr::select(year = year_mid, iso_alpha_3_code, mmr) %>%
    dplyr::filter(year %in% c(2010, 2015, 2020)) %>% 
    dplyr::mutate(target = ifelse(mmr < 420, 1/3*mmr,
                                  ifelse(mmr > 420, 140, NA))
    ) %>%
    dplyr::group_by(iso_alpha_3_code) %>%
    dplyr::mutate(target = round(target[1])) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(mmr = round(mmr)) %>%
    dplyr::rename(mmr = mmr, mmr_target_2030 = target) %>%
    dplyr::left_join(country_ref) %>%
    dplyr::select(SDG_Region, SDG_Sub_Region, iso_alpha_3_code, name_short_en, year, mmr, mmr_target_2030) %>%
    tidyr::pivot_wider(values_from = mmr, names_from = year, names_prefix = "mmr_") %>%
    dplyr::select(SDG_Region, SDG_Sub_Region, iso_alpha_3_code, name_short_en, mmr_2010, mmr_2015, mmr_2020, mmr_target_2030) 
  write.csv(targets, here::here("output", round_name, "EPMM_targets.csv"), row.names = FALSE)
}
