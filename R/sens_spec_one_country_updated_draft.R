
calculate_sens_spec_updated <- function(main_data_for_plots,
                                 jags_list,
                                 jags_fit,
                                 estimates_fixed_from_global, return_samples = F) {

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
  
#start with raw se and sp estimates  
  sesp_with_raw.cjst <- array(NA, c(1, 2, S, nyears))
   
  #Get country tstart and tend earliest and latest years with study data.
  start_study_year <- min(as.numeric(main_data_for_plots$year_reference)) - startyear + 1
  end_study_year <- max(as.numeric(main_data_for_plots$year_reference)) - startyear + 1
  #raw estimates without any changes
  
  for(t in 1:jags_list$nyears){
    sesp_with_raw.cjst[1,1,1:S,t] <-  c(mcmc_array[,,paste0("sens.ct[", 1, ",", t, "]")])
    sesp_with_raw.cjst[1,2,1:S,t] <-  c(mcmc_array[,,paste0("spec.ct[", 1, ",", t, "]")])
  }
  
  #raw summary measures
  rawoutput_c <- suppressWarnings(summarize_se_sp_tibble(se_samples.st = sesp_with_raw.cjst[1,1,,], 
                                                        sp_samples.st = sesp_with_raw.cjst[1,2,,]))
  rawoutput_c$iso_alpha_3_code <- iso_alpha_3_code
   
  #-----------------------------------------

  overwrite_samples.cjst <- sesp_with_raw.cjst
  #For forward extrapolation adjust towards se and sp at tend. 
 if(end_study_year < jags_list$nyears){
    
    
    for(t in (end_study_year + 1): jags_list$nyears){
      forward_ratio_se = rawoutput_c[end_study_year, "sens"]/rawoutput_c[t,"sens"]
      forward_ratio_sp = rawoutput_c[end_study_year, "spec"]/rawoutput_c[t,"spec"]
      
      overwrite_samples.cjst[1,1,1:S,t] <- sesp_with_raw.cjst[1,1,1:S,t] * as.numeric(forward_ratio_se)
      overwrite_samples.cjst[1,2,1:S,t] <- sesp_with_raw.cjst[1,2,1:S,t] * as.numeric(forward_ratio_sp)
    }
    }
  
  
  #-----------------------------------------
  #  Global conv in back extrapolations, 
  #(1) IF se_median_tstart>se_median_global converge to global values within 5-year period 
  
  if( rawoutput_c[start_study_year,"sens"]  > median_se_global
       # & rawoutput_c[start_study_year,"spec"]  > median_sp_global
      ){
  
  #For years between tstart-4 and tstart-1 linear interpolation process applied to samples.
    years_interp <- seq(start_study_year-5, start_study_year-1)
    indices <- years_interp[years_interp > 0]
    min_ind <- min(indices)
    
    min_index <- ifelse(min_ind<5, min_ind, start_study_year -5)
    
    #Interpolate the ratio across 4 years
   se_backward_ratio = median_se_global/rawoutput_c[min_index,"sens"]
   sp_backward_ratio = median_sp_global/rawoutput_c[min_index,"spec"]
   
   interp_ratio_se = approx(x = c(min_index, start_study_year),
                            y = c(se_backward_ratio,  1),
                            xout = indices)$y
   interp_ratio_sp = approx(x = c(min_index, start_study_year),
                            y = c(sp_backward_ratio,  1),
                            xout = indices)$y
    for(t in 1:length(indices)){
      overwrite_samples.cjst[1,1,1:S,indices[t]] <- sesp_with_raw.cjst[1,1,1:S,indices[t]] * as.numeric(interp_ratio_se[t])
      overwrite_samples.cjst[1,2,1:S,indices[t]] <- sesp_with_raw.cjst[1,2,1:S,indices[t]] * as.numeric(interp_ratio_sp[t])
      
    }
    
    #THIS DOES NOT WORK. THE VARIANCE AT TSTART-4 GETS VERY SMALL BECAUSE ALL SAMPLES CONVERGE TO 1 POINT.
    # for(s in 1:S){
    # 
    #   interp_se= approx(x = c(start_study_year-5, start_study_year),
    #          y = c(se_global.s[s],  sesp_with_raw.cjst[1,1,s,start_study_year]),
    #          xout = indices)
    # 
    #   interp_sp= approx(x = c(start_study_year-5, start_study_year),
    #                     y = c(sp_global.s[s],  sesp_with_raw.cjst[1,2,s,start_study_year]),
    #                     xout = indices)
    # 
    # overwrite_samples.cjst[1,1,s,indices] <- interp_se$y
    # overwrite_samples.cjst[1,2,s,indices] <- interp_sp$y
    # }
      
   for(t in 1: (start_study_year -5)){
     if(t>0){
     backward_ratio_se = median_se_global/rawoutput_c[t,"sens"]
     backward_ratio_sp = median_sp_global/rawoutput_c[t,"spec"]
     
     
     overwrite_samples.cjst[1,1,1:S,t] <- sesp_with_raw.cjst[1,1,1:S,t] * as.numeric(backward_ratio_se)
     overwrite_samples.cjst[1,2,1:S,t] <- sesp_with_raw.cjst[1,2,1:S,t] * as.numeric(backward_ratio_sp)
   }}
   


  # end global convergence
}else{
  #If condition not met, apply use same approach as forward extrapolation 
  
  if(start_study_year >1){
    
    
    for(t in 1: (start_study_year-1)){
      backward_ratio_se = rawoutput_c[start_study_year, "sens"]/rawoutput_c[t,"sens"]
      backward_ratio_sp = rawoutput_c[start_study_year, "spec"]/rawoutput_c[t,"spec"]
      
      overwrite_samples.cjst[1,1,1:S,t] <- sesp_with_raw.cjst[1,1,1:S,t] * as.numeric(backward_ratio_se)
      overwrite_samples.cjst[1,2,1:S,t] <- sesp_with_raw.cjst[1,2,1:S,t] * as.numeric(backward_ratio_sp)
    }
    }
  }#end constant convergence
  

  
  overwrite_output_c <- suppressWarnings(summarize_se_sp_tibble(se_samples.st = overwrite_samples.cjst[1,1,,], 
                                                         sp_samples.st = overwrite_samples.cjst[1,2,,]))
  overwrite_output_c$iso_alpha_3_code <- iso_alpha_3_code
  
  
  overwrite_output_c$cov.t<-
    summarize_cov_bmat(rho = overwrite_output_c$rho_sesp,
                       var_sens = overwrite_output_c$var_sens ,
                       var_spec = overwrite_output_c$var_spec )

  
  sesp_c <- overwrite_output_c %>% 
    dplyr::mutate(year_start = t + jags_list$startyear - 1)
  
  # out = list()
  # 
  # out[["sesp_c"]] = sesp_c #overwritten estimates
  # out[["rawsesp_c"]] = rawoutput_c #raw estimates
  if(!return_samples){
  return(sesp_c)
  }else{
  return(overwrite_samples.cjst)
  }
  }

