impute_env_total <- function(iso_alpha_3_code, year_start, year_end, meta) {
  env_total_from_who_matrix <- matrix_ct_to_df(matrix = meta$deaths.ct, C = meta$C, value_name = "value")
  isos <- meta$iso.c
  years <- meta$year.t
  iso_df <- data.frame(c = 1:length(isos), iso_alpha_3_code = isos)
  years_df <- data.frame(t = 1:length(years), year = years)
  value_df <- env_total_from_who_matrix %>%
    dplyr::left_join(years_df, by = c("t")) %>%
    dplyr::left_join(iso_df, by = c("c")) %>%
    dplyr::mutate(iso_alpha_3_code = as.character(iso_alpha_3_code))
  
  year_start_floored <- floor(year_start)
  year_end_ceiling_minus <- ceiling(year_end) - 1
  value_t <- value_df %>%
    dplyr::filter(iso_alpha_3_code == !!iso_alpha_3_code) %>%
    dplyr::filter(year >= year_start_floored) %>%
    dplyr::filter(year <= year_end_ceiling_minus) %>%
    dplyr::pull(value)
  #re calculate t=1 (first year) and t=T (last year) for decimal years
  value_t[1] <- value_t[1] * ( 1 - (year_start - year_start_floored) )
  endT <- length(value_t)
  value_t[endT] <- value_t[endT] * (year_end - year_end_ceiling_minus)
  value_for_period <- value_t %>% sum()
  return(value_for_period)
}

impute_live_births <- function(iso_alpha_3_code, year_start, year_end, meta) {
  births_matrix <- matrix_ct_to_df(matrix = meta$births.ct, C = meta$C, value_name = "value") 
  isos <- meta$iso.c
  years <- meta$year.t
  iso_df <- data.frame(c = 1:length(isos), iso_alpha_3_code = isos)
  years_df <- data.frame(t = 1:length(years), year = years)
  value_df <- births_matrix %>%
    dplyr::left_join(years_df, by = c("t")) %>%
    dplyr::left_join(iso_df, by = c("c")) %>%
    dplyr::mutate(iso_alpha_3_code = as.character(iso_alpha_3_code))
  
  year_start_floored <- floor(year_start)
  year_end_ceiling_minus <- ceiling(year_end) - 1
  value_t <- value_df %>%
    dplyr::filter(iso_alpha_3_code == !!iso_alpha_3_code) %>%
    dplyr::filter(year >= year_start_floored) %>%
    dplyr::filter(year <= year_end_ceiling_minus) %>%
    dplyr::pull(value)
  #re calculate t=1 (first year) and t=T (last year) for decimal years
  value_t[1] <- value_t[1] * ( 1 - (year_start - year_start_floored) )
  endT <- length(value_t)
  value_t[endT] <- value_t[endT] * (year_end - year_end_ceiling_minus)
  value_for_period <- value_t %>% sum()
  return(value_for_period)
}