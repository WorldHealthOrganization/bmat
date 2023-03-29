# GG added this here, check with LA to make sure it is correct
preprocess_main_data <- function(main_data, iso_alpha_3_code, meta_filtered){
  dat2 <- main_data %>%
    dplyr::filter(iso_alpha_3_code == !!iso_alpha_3_code)
  # calculate final_pm: based on deaths and env, then pm_obs, then mmr_obs

  indices_missingpm <- which(is.na(dat2$final_pm))
  if (length(indices_missingpm)>0){
    # dat2$final_pm[indices_missingpm] <- GetPMForMMR(meta_filtered, dat2[indices_missingpm,]) # this was a GG edit because of getmmr in global? reverting for now
    dat2 <-
      dat2 %>%
      dplyr::mutate(final_pm = ifelse(!is.na(obs_matdeaths) & !is.na(obs_env), obs_matdeaths/obs_env, final_pm))
  }
  # get final_mmr based on final_pm
  if (nrow(dat2) != 0) {
    dat2$final_mmr <- GetMMRForPM(meta_filtered,
                                  dat = dat2 %>% dplyr::rename(pm = final_pm)
                                  )
  }

  # write csv to info and rds
  return(dat2)
}
