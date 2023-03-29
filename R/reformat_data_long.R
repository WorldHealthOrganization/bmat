reformat_data_long <- function(data, med, low, up, parameter_name) {
  data <- data %>%
    dplyr::select(iso_alpha_3_code, year_mid, !!med, !!up, !!low) %>%
    dplyr::rename(`0.5` = !!med,
                  `0.1` = !!low,
                  `0.9` = !!up) %>%
    tidyr::gather(key = "quantiles", value = "value", -iso_alpha_3_code, -year_mid) %>%
    tibble::add_column(parameter = !!parameter_name)
  return(data)
}