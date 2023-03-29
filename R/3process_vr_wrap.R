process_vr_wrapper <- function(with_frozen = FALSE,
                               frozen_vr_data,
                               meta_precrisis,
                               vr_data,
                               vr_special_case,
                               round_name) {
  
  dat_merged <- process_vr_data(
    vr_data,
    meta_precrisis = meta_precrisis
  )
  if (with_frozen) {
  dat_merged <- overwrite_w_frozen_vr_for_ss(dat_merged, frozen_vr_data) 
  }
  
  dat_merged_withwhoenv <- add_who_env_tovrdat(meta_precrisis,
                                               dat_merged)
  res4 <- process_vrenvelope(dat_merged_withwhoenv)
  vrdata <- res4 %>%
    dplyr::rename(
      iso_country_name = name,
      iso_alpha_3_code = iso,
      who_code_2 = whoiso,
      year_mid = year,
      year_start = start,
      year_end = end,
      ill_defined_death_proportion = propill,
      ill_defined_death_proportion2 = propgrp2,
      exclude_reason = exclude.reason,
      obs_matdeaths = mat,
      obs_matdeaths_late = mat.late,
      obs_matdeaths_incl_late = mat.incl.late,
      obs_env = env,
      obs_ill = ill,
      env_total_who_estimated = whoenv,
      ytot_bmis = ytot_vradj,
      ytot_bmat = ytot_bmat,
      rho_bmis = rhovrfinal_vradj,
      rho_bmat = rhovrfinal_bmat,
      rho_period = rhovr_period,
      rho_period_min = rhovrmin_period,
      rho_period_max = rhovrmax_period,
      rho_intermediate = rhointermediate,
      usability_percentage = usa,
      icd = icd,
      icd_utilized = isicd10
    ) %>%
    dplyr::mutate(
      final_env = obs_env,
      final_pm = obs_matdeaths / final_env,
      obs_selogpm = NA,
      type = "vr",
      definition = "maternal"
    ) %>%
    dplyr::mutate(include_reason = ifelse(is.na(include), "Missing include column value in CRVS dataset", include_reason)) %>%
    dplyr::mutate(include = ifelse(!is.na(include_reason), FALSE, include))
  
  vr_special_case_indicator <- vr_special_case %>%
    dplyr::filter(is.na(ori_vr_data_from_paho_update)) %>%
    dplyr::mutate(special_case = TRUE) %>%
    dplyr::rename(year_start = year,
                  iso_alpha_3_code = iso)
  vrdata <- vrdata %>%
    dplyr::left_join(vr_special_case_indicator,
                     by = c("year_start", "iso_alpha_3_code")) %>%
    dplyr::mutate(special_case = ifelse(is.na(special_case), FALSE, special_case))
                                                        
  if (with_frozen) {
    write.csv(vrdata,
              here::here("output", round_name, "vrdata_w_frozen.csv"),
              row.names = FALSE)
  } else {
    write.csv(vrdata,
              here::here("output", round_name, "vrdata.csv"),
              row.names = FALSE)
    
  }
}