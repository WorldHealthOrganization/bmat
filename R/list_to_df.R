list_to_df <- function(main_data) {
temp1 <- main_data %>%
  purrr::map_df(tibble::enframe) %>%
  dplyr::filter(
    !name %in% c(
      "rho",
      "ytot_vr",
      "ytot",
      "ymat_vr",
      "ynonmat_vr",
      "icd_utilized",
      "year_mid_index"
    )
  ) %>%
  dplyr::mutate(row_id = purrr::pmap(list(1:length(main_data), length(unique(name))), rep) %>% unlist) %>%
  tidyr::spread(name, value)

# we need ztot_vr and ztot
temp2 <- main_data %>%
  purrr::map_df(tibble::enframe) %>%
  dplyr::filter(
    name %in% c(
      "ytot",
      "ytot_vr",
      "ymat_vr",
      "ynonmat_vr"
    )
  ) %>%
  tidyr::unnest_wider(value) %>%
  dplyr::mutate(sum = rowSums(.[2:ncol(.)], na.rm = TRUE)) %>%
  dplyr::mutate(row_id = purrr::pmap(list(1:length(main_data), length(unique(name))), rep) %>% unlist) %>%
  dplyr::select(name, sum, row_id) %>%
  tidyr::spread(name, sum) %>%
  dplyr::rename(
    tot = ytot,
    tot_vr = ytot_vr,
    mat_vr = ymat_vr,
    nonmat_vr = ynonmat_vr)




temp3 <- main_data %>%
  purrr::map_df(tibble::enframe) %>%
  dplyr::filter(
    name %in% c(
      "rho",
      "ymat_vr",
      "ytot_vr",
      "ytot",
      "year_mid_index"
    )
  ) %>%
  dplyr::mutate(row_id = purrr::pmap(list(1:length(main_data), length(unique(name))), rep) %>% unlist) %>%
  dplyr::group_by(row_id) %>%
  dplyr::mutate(year_mid_index = rep(value[[1]][1],dplyr::n())) %>%
  tidyr::unnest_longer(value) %>%
  dplyr::filter(name != "year_mid_index") %>%
  dplyr::group_by(row_id, name) %>%
  dplyr::summarise(value = value[year_mid_index[1]]) %>% #we grab vr values which correspond to mid year of study period
  tidyr::spread(name, value) %>%
  dplyr::rename(rho_ref = rho,
                mat_vr_ref = ymat_vr,
                tot_vr_ref = ytot_vr,
                tot_ref = ytot)
main_data <- temp1 %>%
  dplyr::left_join(temp2, by = "row_id") %>%
  dplyr::left_join(temp3, by = "row_id") %>%
  dplyr::mutate_all(unlist)
main_data <- main_data %>%
  dplyr::mutate(mat_vr = ifelse(study_period_coverage == "none", NA, mat_vr),
                nonmat_vr = ifelse(study_period_coverage == "none", NA, nonmat_vr),
                tot = ifelse(study_period_coverage == "none", NA, tot),
                tot_vr = ifelse(study_period_coverage == "none", NA, tot_vr))
return(main_data)
}
