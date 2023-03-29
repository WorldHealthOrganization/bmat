main_data_list <- function(vrdata,
                           ssdata,
                           global_run = TRUE, 
                           iso_alpha_3_code = NULL,
                           first_year,
                           bmis_run = FALSE) {
  
  ssdata <- ssdata %>%
    dplyr::mutate(isfraction = (is.fraction(year_start) | is.fraction(year_end))) %>%
    dplyr::mutate(year_start = floor(year_start)) %>%
    dplyr::mutate(year_end = ceiling(year_end))
    # 
  vrdata <- vrdata %>% 
    dplyr::filter(!is.na(include)) %>%
    dplyr::filter(include) %>%
    dplyr::filter(!special_case)
  studyfinalg <- list()
  if(!global_run) {
    ssdata <- ssdata %>% dplyr::filter(iso_alpha_3_code %in% !!iso_alpha_3_code)
    vrdata <- vrdata %>% dplyr::filter(iso_alpha_3_code %in% !!iso_alpha_3_code)
  }
  
  #choose ytot and impute with linear interpolation
  vrdata <- vrdata %>%
    dplyr::group_by(iso_alpha_3_code) %>%
    dplyr::arrange(iso_alpha_3_code, year_start) %>%
    dplyr::mutate(t = year_start - first_year + 1) %>%
    dplyr::mutate(ytot = ifelse(rho_bmis != 1 & global_run, ytot_bmis, #uses a scaled up version of vr env
                                ifelse(rho_bmis != 1 & !global_run, ytot_bmat, # this is using who envelope if vr env < who env
                                       ifelse(rho_bmis == 1, obs_env,
                                              NA)
                                ))) %>%
    dplyr::mutate(not_na_count_c = sum(!is.na(ytot))) %>%
    dplyr::mutate(ytot = ifelse(not_na_count_c > 1, approx(x = t, y = ytot, xout = t, rule = 2)$y, #rule 2 uses the min and max value imputation outside the interpolation interval of [min(x), max(x)].
                                ifelse(not_na_count_c == 1, mean(ytot, na.rm = T),
                                       ifelse(all(is.na(ytot)), stop(paste0(iso_alpha_3_code, " has no ytot data")),
                                              ytot
                                       )
                                )
    )
    )
  
  
  novr <- c()
  for (i in 1:nrow(ssdata)) {
    studyfinalg[[i]] <- ssdata[i,] %>% as.list()
    start <- floor(ssdata$year_start[i])
    end <- ceiling(ssdata$year_end[i])
    mid <- floor(as.numeric(ssdata$year_mid[i]))
    env_total_from_study <- ssdata$env_total[i]
    period_length <- length(seq(start, end - 1)) 
    vrdata_subset <- vrdata %>%
      dplyr::filter(iso_alpha_3_code == as.character(ssdata$iso_alpha_3_code[i])) %>%
      dplyr::filter(year_start >= start) %>%
      dplyr::filter(year_end <= end) %>%
      dplyr::mutate(year_mid_index = ifelse(any(year_start == mid), which(year_start == mid), NA))
    # might not need this
    # if(nrow(vrdata_subset) == 0) {
    #   vrdata_subset <- vrdata_subset %>%
    #     dplyr::ungroup() %>%
    #     tibble::add_row(year_mid_index = NA)
    # }
    has_vr_in_ref_year <- any(vrdata_subset$year_start == mid)
    # get multiplier to use if year is a fraction of a year
    complete <- ifelse(min(vrdata_subset$rho_bmis, na.rm = TRUE) == 1, TRUE, FALSE)
    check <- ifelse(ssdata$check_outside_of_vr[i] == "Y", TRUE, FALSE)
    if(studyfinalg[[i]]$isfraction & nrow(vrdata_subset)>1) {
      mult_start <- ssdata$year_start[i] - start
      mult_end <- 1 - (end - ssdata$year_end[i])
      if(check | complete) {
        vrdata_subset <- vrdata_subset %>%
          dplyr::mutate(env_total_to_use = ytot)
      } else {
        vrdata_subset <- vrdata_subset %>%
          dplyr::mutate(env_total_to_use = obs_env)
      }
      if(period_length > 2){
        env_start <- vrdata_subset %>%
          dplyr::filter(year_start == start) %>%
          dplyr::pull(env_total_to_use)
        env_end  <- vrdata_subset %>%
          dplyr::filter(year_end == end) %>%
          dplyr::pull(env_total_to_use)
        env_mid <- vrdata_subset %>%
          dplyr::filter(year_start != start & year_end != end) %>%
          dplyr::pull(env_total_to_use)
        env <- env_start + env_mid + env_end
        env_scaled <- env_start*mult_start + env_mid + env_end*mult_end
      }
      if(period_length >1){
        env_start <- vrdata_subset %>%
          dplyr::filter(year_start == start) %>%
          dplyr::pull(env_total_to_use)
        env_end  <- vrdata_subset %>%
          dplyr::filter(year_end == end) %>%
          dplyr::pull(env_total_to_use)
        env <- env_start +  env_end
        env_scaled <- env_start*mult_start + env_end*mult_end
      }
      if(period_length == 1) {
        env_start <- vrdata_subset %>%
          dplyr::filter(year_start == start) %>%
          dplyr::pull(env_total_to_use)
        env <- env_start 
        env_scaled <- env_start*mult_start 
      }
      multiplier <- env_scaled/env
    } else {
      multiplier <- 1
    }
    studyfinalg[[i]][["has_vr_in_reference_year"]] <- has_vr_in_ref_year
    studyfinalg[[i]][["year_mid_index"]] <- vrdata_subset$year_mid_index
    studyfinalg[[i]][["rho"]] <- vrdata_subset$rho_bmis
    studyfinalg[[i]][["ytot_vr"]] <- round(vrdata_subset$obs_env*multiplier)
    studyfinalg[[i]][["ytot"]] <- round(vrdata_subset$ytot*multiplier)
    studyfinalg[[i]][["ymat_vr"]] <- round(vrdata_subset$obs_matdeaths*multiplier)
    studyfinalg[[i]][["ynonmat_vr"]] <- round((vrdata_subset$obs_env - vrdata_subset$obs_matdeaths)*multiplier)
    studyfinalg[[i]][["icd_utilized"]] <- vrdata_subset$icd_utilized
    #below is for ssdata processing that goes before bmis and etc
    studyfinalg[[i]][["study_period_coverage"]] <- ifelse(nrow(vrdata_subset) == period_length, "all years", 
                                                          ifelse(nrow(vrdata_subset) < period_length & nrow(vrdata_subset) > 0, "some years",
                                                                 "none"
                                                          )
    )
    
    studyfinalg[[i]][["complete"]] <- complete #is the minimum rho (completeness) == 1
    #inclusion in bmis and bmat also needed
    #inclusion in bmis global is T if all years, no year is fraction, complete, if ssdata start_year > 1985 etc
    
    #this was to fix old ssdata, to do: fix old ssdata and then you can remove this line
    if(bmis_run) {
      studyfinalg[[i]]$truemat_vr = ifelse(is.na(studyfinalg[[i]]$truemat_vr) & min(studyfinalg[[i]]$rho) ==1, studyfinalg[[i]]$truemat, studyfinalg[[i]]$truemat_vr)
    }
    
    
  }
  
  
  
  return(studyfinalg)
}
