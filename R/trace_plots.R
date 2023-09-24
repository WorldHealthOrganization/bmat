trace_plots <- function(fit,
                        round_name,
                        main_path,
                        hyperpars_to_plot,
                        jags_list,
                        global_run,
                        iso_alpha_3_code,
                        which = "BMat") {

  summ <-
    MCMCvis::MCMCsummary(fit, params = hyperpars_to_plot) %>%
    tibble::rownames_to_column(var = "pars") %>%
    dplyr::mutate(pars2 = pars %>% remove_numbers_and_brackets)
  
  if (!global_run) {
    country_ref <- read.csv(here::here("output", round_name, "country_ref.csv"))
    iso_crosswalk <-
      data.frame(
        c = 1,
        iso_alpha_3_code = iso_alpha_3_code
      ) %>%
      dplyr::left_join(country_ref %>% dplyr::select(iso_alpha_3_code, name_short_en))
    temp <- summ %>%
      dplyr::filter(pars2 %in% hyperpars_to_plot) %>%
      dplyr::mutate(c = get_c_value(pars)) %>%
      dplyr::left_join(iso_crosswalk) %>%
      dplyr::mutate(parsv3 = paste0(iso_alpha_3_code, "_", name_short_en, "_", pars))
    write.csv(temp, row.names = FALSE, here::here(file.path(main_path, "summary.csv")))
    MCMCvis::MCMCtrace(
      fit,
      params = temp$pars,
      ISB = FALSE,
      pdf = TRUE,
      post_zm = TRUE,
      open_pdf = FALSE,
      Rhat = TRUE,
      filename = "diagnostic_trace_plot.pdf",
      wd = main_path,
      main_tr = temp$parsv3
    )
  } else{
    # let's think about implementation where global runs also have country names on paramters, its a bit tricky with these mixed in with non-country specific pars and needs the meta objects
    # country_ref <- read.csv(here::here("output", round_name, "country_ref.csv"))
    # iso_crosswalk <-
    #   data.frame(
    #     c = 1:length(meta$iso.c),
    #     iso_alpha_3_code = meta$iso.c
    #   ) %>%
    #   dplyr::left_join(country_ref %>% dplyr::select(iso_alpha_3_code, name_short_en))
    # temp <- summ %>%
    #   dplyr::filter(pars2 %in% hyperpars_to_plot) %>%
    #   dplyr::mutate(c = get_c_value(pars)) %>%
    #   dplyr::left_join(iso_crosswalk) %>%
    #   dplyr::mutate(parsv3 = paste0(iso_alpha_3_code, "_", name_short_en, "_", pars))
    
    
    temp <- summ %>%
      dplyr::filter(pars2 %in% hyperpars_to_plot)
    write.csv(temp, row.names = FALSE, here::here(file.path(main_path, "summary.csv")))
    MCMCvis::MCMCtrace(
      fit,
      params = temp$pars,
      ISB = FALSE,
      pdf = TRUE,
      post_zm = TRUE,
      open_pdf = FALSE,
      Rhat = TRUE,
      filename = "diagnostic_trace_plot.pdf",
      wd = main_path,
      main_tr = temp$pars)
  }

} # end function

# utility functions trace plot function
remove_numbers_and_brackets <- function(input_string) {
  # Define the regular expression pattern to match numbers, commas, and brackets
  pattern <- "[0-9,\\[\\]]"
  # Use gsub to replace the matched pattern with an empty string
  cleaned_string <-
    stringr::str_replace_all(input_string, pattern, "")
  return(cleaned_string)
}
get_c_value <- function(s) {
  # Remove all non-essential characters before and including "."
  s <- gsub(".*\\.", "", s)
  # Find position of 'c' in the string
  c_position <- regexpr("c", s)[1]
  # Remove all non-numeric characters
  s <- gsub("[^0-9,]", "", s)
  # Split string by comma
  split_string <- strsplit(s, ",")[[1]]
  # Return the number corresponding to the position of 'c'
  return(as.integer(split_string[c_position]))
}