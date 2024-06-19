vr_completness_data_for_steakholders <- function(vrdata) {
  completness_sheet <- vrdata %>% 
    dplyr::select(iso_alpha_3_code, 
                  year_start, 
                  year_end, 
                  allcause_1549_vr = obs_env, 
                  allcause_1549_wpp = env_total_who_estimated, 
                  completeness = rho_bmat, 
                  usability = usability_percentage) %>%
    dplyr::mutate(`ratio_vr/wpp` = allcause_1549_vr/allcause_1549_wpp)
  write.csv(completness_sheet,
            here::here("output", round_name, "vrdata_completness.csv"),
            row.names = FALSE)
}