pluck_and_reformat_ci_onecountry <- function(ci_list, variable) {
  data <- ci_list %>%
    purrr::pluck({variable}) %>%
    .[1, , ] %>%
    t() %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = "year_mid") %>%
    tibble::as_tibble()
  return(data)
}


pluck_and_reformat_ci_global <- function(ci_list, variable, iso_alpha_3_code, meta) {
  data <- ci_list %>%
    purrr::pluck({variable}) %>%
    .[which(meta$iso.c == iso_alpha_3_code), , ] %>%
    t() %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = "year_mid") %>%
    tibble::as_tibble()
  return(data)
}

pluck_and_reformat_ci <- function(ci_list, variable, iso_alpha_3_code, meta) {
  indicator_data <- ci_list %>%
    purrr::pluck({
      variable
    })
  indicator_data <- indicator_data[which(meta$iso.c == iso_alpha_3_code), , ] %>%
    t() %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = "year_mid") %>%
    tibble::as_tibble()
  return(indicator_data)
}
