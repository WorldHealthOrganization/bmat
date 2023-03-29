process_survey_data <- function(
  survey_data,
  round_name
) {
  survey_data <- survey_data %>%
    dplyr::rename( citation_short = source.detail) %>%
    dplyr::rename(iso_alpha_3_code = iso.code)
  
  survey_data <- survey_data %>%
    dplyr::mutate(
      final_pm = ifelse(
        !is.na(log.pmdf.calc),
        exp(log.pmdf.calc - log(100)),
        pmdf
      ),
      obs_selogpm = se.log.pmdf.calc,
      year_start = start.date,
      year_end = end.date,
      year_mid = 1 / 2 * (year_start + year_end)
    ) %>%
    dplyr::select(iso_alpha_3_code, year_mid, year_start, year_end, final_pm, obs_selogpm, citation_short) %>%
    dplyr::mutate(
      include = TRUE,
      type = "dhs",
      definition = "pregn"
    )
  write.csv(survey_data, row.names = FALSE, here::here("output", round_name, "survey.csv"))
  return(NULL)
}