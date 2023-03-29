#gets vr totals e.g. tot, tot_vr, mat_vr,
process_ssdata_w_vr_calculations <- function(vrdata, 
                                             ssdata,
                                             first_year = 1985) {
  main_data <- main_data_list(vrdata = vrdata %>% dplyr::filter(include) %>% dplyr::filter(!special_case), 
                              ssdata = ssdata,
                              first_year = first_year)
  dm_i <- dm_for_each_observation(main_data = main_data, dm_rules = dm_rules())
  ssdata_w_vr_calculations <- list_to_df(main_data) %>%
    dplyr::rename_all(tolower) %>%
    dplyr::mutate(dmname = dm_i) %>%
    dplyr::mutate_all(unlist)
  return(ssdata_w_vr_calculations)
}