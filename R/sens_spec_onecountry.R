calculate_sens_spec_onecountry <- function(main_data_for_plots,
                                 jags_list,
                                 jags_fit,
                                 estimates_fixed_from_global,
                                 sens_spec_global) {

  median_se_global <-
    estimates_fixed_from_global$parameter_estimates_global %>% dplyr::filter(parameter == "sensworld") %>% dplyr::pull(`0.5`)
  median_sp_global <-
    estimates_fixed_from_global$parameter_estimates_global %>% dplyr::filter(parameter == "specworld") %>% dplyr::pull(`0.5`)
  
  
  mcmc_array <- jags_fit$BUGSoutput$sims.array
  S <- dim(mcmc_array)[1] * dim(mcmc_array)[2]
  
  iso_alpha_3_code = jags_list$iso_alpha_3_code.c
  startyear <- jags_list$startyear
  tref = jags_list$tref
  nyears = jags_list$nyears
  tref.c = jags_list$tref.c
  
  #######################################
  #Section 2: Create BMAT output for countries with data
  #######################################
  
  
  #Get samples of global se and sp. These are needed to be able to extrap back to global values.
  sesp_with_raw.cjst <- array(NA, c(1, 2, S, nyears))
  #For each country create (1) rawoutput, (2) overwrite output, which has constant extrapolations outside study period, (3) global convergence outside study period under two criteria (applied either)
  #(a) criteria where se> median global se and sp >median global sp or (b) se > median global se only.
  
  #Get country tstart and tend earliest and latest years with study data.
  start_study_year <- min(as.numeric(main_data_for_plots$year_start)) - startyear + 1
  end_study_year <- max(as.numeric(main_data_for_plots$year_start)) - startyear + 1
  #raw estimates without any changes
  
  for(t in 1:jags_list$nyears){
    sesp_with_raw.cjst[1,1,1:S,t] <-  c(mcmc_array[,,paste0("sens.ct[", 1, ",", t, "]")])
    sesp_with_raw.cjst[1,2,1:S,t] <-  c(mcmc_array[,,paste0("spec.ct[", 1, ",", t, "]")])
  }
  
  
  rawoutput_c <- suppressWarnings(summarize_se_sp_tibble(se_samples.st = sesp_with_raw.cjst[1,1,,], 
                                                         sp_samples.st = sesp_with_raw.cjst[1,2,,]))
  rawoutput_c$iso_alpha_3_code <- iso_alpha_3_code
  
  #-----------------------------------------
  
  #Set to constant from most recent onwards/backwards
  constant_overwrite_output_c <- list()
  constant_overwrite_output_c <- rawoutput_c
  
  if(start_study_year > 1){
    for(t in 1:(start_study_year-1)){
      constant_overwrite_output_c[t,"sens"] <- rawoutput_c[start_study_year,"sens"]
      constant_overwrite_output_c[t,"spec"] <- rawoutput_c[start_study_year,"spec"]
      constant_overwrite_output_c[t,"sens_sq"] <- rawoutput_c[start_study_year,"sens_sq"]
      constant_overwrite_output_c[t,"oneminspec_sq"] <- rawoutput_c[start_study_year,"oneminspec_sq"]
      constant_overwrite_output_c[t, "rho_sesp"] <- rawoutput_c[start_study_year, "rho_sesp"]
    }}
  if(end_study_year < jags_list$nyears){
    for(t in (end_study_year + 1): jags_list$nyears){
      constant_overwrite_output_c[t,"sens"] <- rawoutput_c[end_study_year,"sens"]
      constant_overwrite_output_c[t,"spec"] <- rawoutput_c[end_study_year,"spec"]
      constant_overwrite_output_c[t,"sens_sq"] <- rawoutput_c[end_study_year,"sens_sq"]
      constant_overwrite_output_c[t,"oneminspec_sq"] <- rawoutput_c[end_study_year,"oneminspec_sq"]
      constant_overwrite_output_c[t, "rho_sesp"] <- rawoutput_c[end_study_year, "rho_sesp"]
    }}
  
  constant_overwrite_output_c$iso_alpha_3_code <- iso_alpha_3_code
  
  #-----------------------------------------
  #  Global conv in back extrapolations, ie converge to global values within 5-year period.
  indicators <- c("sens", "var_sens", "sens_sq", "oneminsens_sq","spec", "var_spec", "spec_sq", "oneminspec_sq", "rho_sesp")
  
  
  sesp_c <- get_global_convergence_output(sens_spec_global = sens_spec_global, 
                                          indicators = indicators,
                                          tref = tref, 
                                          constant_overwrite_output_c = constant_overwrite_output_c,
                                          start_study_year = start_study_year)
  
  
  sesp_c <- sesp_c %>% 
    dplyr::mutate(year_start = t + jags_list$startyear - 1)
  return(sesp_c)
}
