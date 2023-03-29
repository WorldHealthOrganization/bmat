compare_input_data <- function(metanew, metaold, value_matrixnew, value_matrixold, forweight.ct, country_ref) {
  dfnew <- matrix_ct_to_df_with_isoyear(metanew, value_matrix= value_matrixnew, value_name = "value")  %>%
    dplyr::filter(year > 1999) %>%
    dplyr::mutate(value.y = value)
  dfold <- matrix_ct_to_df_with_isoyear(metaold, value_matrix= value_matrixold, value_name = "value")  %>%
    dplyr::filter(year > 1999) %>%
    dplyr::mutate(value.x = value)
  df <- dplyr::full_join(dfnew, dfold, by = c("iso_alpha_3_code", "year"))

  df_w <- matrix_ct_to_df_with_isoyear(metanew, forweight.ct, value_name = "forweight") %>%
    dplyr::filter(year > 1999)
  df <- df %>% dplyr::left_join(df_w, by = c("iso_alpha_3_code", "year")) %>%
    dplyr::mutate(change_abs = abs(value.x - value.y) / (value.y)*100) %>%
    dplyr::mutate(change = (value.x - value.y) / (value.y)*100) %>%
    dplyr::left_join(country_ref %>% dplyr::select(iso_alpha_3_code, region_who), by = c("iso_alpha_3_code"))



  # weighted absolute changes summarized by region
  df1 <- df %>%
    tidyr::drop_na() %>%
    group_by(region_who, year) %>%
    dplyr::mutate(weight_denominator = sum(forweight)) %>%
    dplyr::mutate(weight = forweight / weight_denominator) %>%
    dplyr::mutate(change_weighted_abs = change_abs * weight) %>%
    dplyr::mutate(change_weighted = change * weight) %>%
    dplyr::summarize(aggregate_weighted_abs_change_value = sum(change_weighted_abs),
                     aggregate_weighted_change_value = sum(change_weighted),
                     aggregate_abs_change_value = sum(change_abs),
                     aggregate_change_value = sum(change),
                     aggregate_value_old = sum(value.x),
                     aggregate_value_new = sum(value.y))

  df2 <- df %>%
    tidyr::drop_na() %>%
    group_by(region_who) %>%
    dplyr::mutate(weight_denominator = sum(forweight)) %>%
    dplyr::mutate(weight = forweight / weight_denominator) %>%
    dplyr::mutate(change_weighted_abs = change_abs * weight) %>%
    dplyr::mutate(change_weighted = change * weight) %>%
    dplyr::summarize(aggregate_weighted_abs_change_value = sum(change_weighted_abs),
                     aggregate_weighted_change_value = sum(change_weighted),
                     aggregate_abs_change_value = sum(change_abs),
                     aggregate_change_value = sum(change),
                     aggregate_value_old = sum(value.x),
                     aggregate_value_new = sum(value.y))

  # # weighted changes overtime summarized by region
  # weighted_change_rs <- df %>%
  #   tidyr::drop_na() %>%
  #   group_by(region_who) %>%
  #   dplyr::mutate(weight_denominator = sum(births)) %>%
  #   dplyr::mutate(weight = births / weight_denominator) %>%
  #   dplyr::mutate(change_weighted = change * weight) %>%
  #   dplyr::mutate(death_weighted.x = value.x * weight) %>%
  #   dplyr::mutate(death_weighted.y = value.y * weight) %>%
  #   summarize(mean_weighted_change = sum(change_weighted),
  #             weighted_death.x = sum(death_weighted.x),
  #             weighted_death.y = sum(death_weighted.y))
  return(list(rt = df1, r = df2))
  
}
