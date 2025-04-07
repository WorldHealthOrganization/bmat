process_study_data <- function(vrdata,
                               vrdata_edit_paho2019,
                               meta,
                               meta_precrisis,
                               first_year = 1985,
                               data_2019,
                               data_2021,
                               round_name) {
  
  
  # make a single dataset of raw study data
  ssdata_raw <- dplyr::bind_rows(data_2019, data_2021) %>%
    dplyr::mutate(subnational = ifelse(is.na(subnational), FALSE, subnational)) %>%
    dplyr::mutate(year_mid = 1 / 2 * (year_start + year_end))
  
  # calculate sums of vrdata for each study period e.g. mat_vr
  ssdata_w_vr_calculations <- process_ssdata_w_vr_calculations(
    ssdata = ssdata_raw,
    vrdata = vrdata,
    first_year = first_year
  )
  # write.csv(ssdata_w_vr_calculations, row.names = FALSE, here::here("ssdata_w_vr_calculations.csv"))
  # ssdata_w_vr_calculations <- read.csv(here::here("ssdata_w_vr_calculations.csv"))
  
  
  # add the S level to the check variable
  temp <- ssdata_w_vr_calculations
  #  if maternal OR check is NA is what results from this chunk - "N"
  temp <- temp %>%
    dplyr::mutate(check_outside_of_vr = ifelse(check_outside_of_vr == "NA", NA, check_outside_of_vr))
  temp <- temp %>%
    dplyr::mutate(check_outside_of_vr = ifelse((!is.na(check_outside_of_vr) & !is.na(env_mat)) , check_outside_of_vr, "N")) %>%
    #Second, assume if checkalldeaths is missing & spec study envelope (total fem deaths) is not missing then checkalldeaths = S otherwise = checkalldeaths_update. “S” indicates using total env from study not reported checkalldeaths.
    dplyr::mutate(check_outside_of_vr = ifelse(is.na(check_outside_of_vr) & !is.na(env_total) & is.na(fn) & is.na(fp), "S", check_outside_of_vr))
  
  
  
  # creation of truemat and truemat_vr from envelopes
  # GG removed something like "expect_that() something is equal from code, ask EP or check_outside_of_vr her code to see what this is
  temp <- temp %>%
    dplyr::mutate(truemat_vr = ifelse((check_outside_of_vr == "N" & is.na(up))| complete, floor(env_mat),  NA)) %>%
    dplyr::mutate(truemat = ifelse((check_outside_of_vr == "Y" & !complete) | (check_outside_of_vr == "S" & !is.na(env_mat)),  floor(env_mat), NA)) 
  #it would be odd if vrdata was complete and the study in the country check_outside_of_vred outside of vr so we might want to flag with a warning
  temp <- temp %>%
    dplyr::mutate(truemat_vr = ifelse(is.na(truemat_vr) & complete == TRUE, truemat, # this is an == S and complete case
                                      ifelse(is.na(truemat_vr) & check_outside_of_vr == "Y" &  !is.na(truemat), truemat - up,
                                             truemat_vr
                                      )
    )
    ) %>%
    dplyr::mutate(truemat = ifelse(is.na(truemat) & check_outside_of_vr == "Y" & complete == TRUE, truemat_vr, truemat))
  
  
  
  
  
  # Impute env_tot in studies based on lifetables
  temp <- temp %>% 
    dplyr::rowwise() %>%
    dplyr::mutate(env_total_imputed = ifelse(
      is.na(env_total),
      impute_env_total(
        iso_alpha_3_code = iso_alpha_3_code,
        year_start = year_start,
        year_end = year_end,
        meta = meta_precrisis
      ) %>% round(),
      env_total
    ))
  
  
  
  # MULTIPLIERS 
  # we calculate a multiplier for
  # -some years = tot_vr/env_total
  # -some years and check goes beyond vr = tot/env_total
  # -partial years = weighted based on dates
  # -all else (includes having all years of study covered) = 1
  temp <- temp %>%
    dplyr::mutate(check_or_complete = check_outside_of_vr == "Y" | complete == TRUE) %>% 
    dplyr::mutate(multiplier = ifelse(check_or_complete & study_period_coverage == "some years", min(tot/env_total_imputed, 1), #some years 
                                      ifelse(!check_or_complete & study_period_coverage == "some years", min(tot_vr/env_total_imputed, 1), #some years and check beyond vr
                                             1
                                      )))
  
  
  
  # Apply multipliers
  temp <- temp %>%
    dplyr::mutate(truemat_unscaled = truemat,
                  truemat_vr_unscaled  = truemat_vr) %>%
    dplyr::mutate_at(c("truemat", "truemat_vr"), ~ round(multiplier*.))
  
  
  
  
  # Calculations of boxes
  # The following variables are calculated from vr data in list_to_df function earlier
  # mat_vr
  # tot
  # tot_vr
  # nonmat_vr
  
  temp0 <- temp %>% 
    dplyr::filter(study_period_coverage != "all years")
  temp <- temp %>% 
    dplyr::filter(study_period_coverage == "all years")
  
  if(nrow(temp) > 0) {
    
    # TP 
    # We do this first to be able to calculate other boxes using TP.
    # If spec study reported then leave as is, else we have to calculate based on whether we have FP, or FN.
    # 1a. TP = truematvr - FN
    # 1b. If 1a gives NA then TP = matvr - FP
    # 1c. If 1a and 1b give NA, but we do have data on FP and FN, then TP = vrtot - FP-FN-TN
    temp <- temp %>%
      dplyr::mutate(tp = ifelse(!is.na(tp), tp,
                                ifelse(!is.na(truemat_vr) & !is.na(fn), truemat_vr - fn,
                                       ifelse(!is.na(mat_vr) & !is.na(fp), mat_vr - fp,
                                              ifelse(!is.na(fn) & !is.na(fp) & !is.na(tn) & !is.na(tot_vr), tot_vr - fp - fn - tn,
                                                     NA
                                              )
                                       )
                                )
      )
      )
    
    # FP 
    # If spec study reported then leave as is, otherwise we calculate based on other boxes reported if possible.
    # 1a. If not reported in ssdata, then FP = matvr - TP
    temp <- temp %>%
      dplyr::mutate(fp = ifelse(!is.na(fp), fp,
                                ifelse(!is.na(mat_vr) & !is.na(tp), mat_vr - tp,
                                       NA   
                                )     
      )  
      )
    
    # FN 
    # If ssdata reported then leave as is.
    # 1a. IF TP available after TP processing, FN = truematvr - TP.
    temp <- temp %>%
      dplyr::mutate(fn = ifelse(!is.na(fn), fn,
                                ifelse(!is.na(truemat_vr) & !is.na(tp), truemat_vr - tp,
                                       NA   
                                )     
      )  
      )
    
    # TN 
    # If reported in ssdata leave as is.
    # 1a. If not reported in ssdata, then TN = nonmatvr - FN
    temp <- temp %>%
      dplyr::mutate(tn = ifelse(!is.na(tn), tn,
                                ifelse(!is.na(nonmat_vr) & !is.na(fn), nonmat_vr - fn,
                                       NA   
                                )     
      )  
      )
    
    # UP 
    # If not reported in ssdata then
    # 1a. If study reported total mat deaths (including outside VR) then UP = truemattot - TP- FN
    temp <- temp %>%
      dplyr::mutate(up = ifelse(!is.na(up), up,
                                ifelse(!is.na(truemat) & !is.na(tp) & !is.na(fn) & check_outside_of_vr == "Y", truemat - tp - fn,
                                       NA
                                       
                                )
      )
      )
    # UN
    # 1b. UN = tot - vrtot - UP
    temp <- temp %>%
      dplyr::mutate(un = ifelse(!is.na(un), un,
                                ifelse(!is.na(tot_vr) & !is.na(tot) & !is.na(up), tot - tot_vr - up,
                                       NA)
      )
      )
    
    
    # 1c. If VR is complete, ie rhovr = 1 then UP and UN = 0
    temp <- temp %>%
      dplyr::mutate(un = ifelse(complete, 0, un)) %>%
      dplyr::mutate(up = ifelse(complete, 0, up))
  }
  
  
  
  
  # Overwrite totals with recalculations based on boxes if needed 
  # Calculate truemat and truemat_vr if NA and if We have box values
  temp_final <- dplyr::bind_rows(temp, temp0)
  temp_final <- temp_final %>%
    dplyr::mutate(conflicting_envelopes = !(is.na(fp) | fp >= 0))  #indicate if calculations are bad due to conflicting study and vr evelopes
  # tot_vr
  temp_final <- temp_final %>%
    dplyr::mutate(tot_vr = ifelse(!is.na(tp) & !is.na(tn) & !is.na(fp) & !is.na(fn), tp + tn + fp + fn, tot_vr))
  # truemat_vr 
  # if we have fn and tp we can get truemat_vr (calculate if mismatch or if missing and within vr, is within vr if up = 0)
  temp_final <- temp_final %>%
    dplyr::mutate(missing_env_before_calc = is.na(truemat) & is.na(truemat_vr)) %>%
    dplyr::mutate(mismatch_truemat_vr = truemat_vr != tp + fn | mat_vr != tp + fp | tot_vr != sum(tp, fp, fn, tn)) %>%
    dplyr::mutate(mismatch_truemat_vr = ifelse(is.na(mismatch_truemat_vr), FALSE, mismatch_truemat_vr)) %>% #impute NAs to FALSE
    dplyr::mutate(missing_truemat_vr_can_be_calculated = ifelse(is.na(truemat_vr) & up == 0 & !is.na(tp) & !is.na(fn) & !conflicting_envelopes, TRUE, FALSE)) %>%
    dplyr::mutate(truemat_vr = ifelse(
      mismatch_truemat_vr | missing_truemat_vr_can_be_calculated,
      tp + fn,
      truemat_vr)
    ) %>%
    dplyr::mutate(truemat_vr = ifelse(is.na(truemat_vr) & !conflicting_envelopes & !is.na(truemat), as.integer(truemat * (tot_vr/tot)), truemat_vr))
  # truemat
  # if up > 0 we are outside of vr, if we have up fn and tp we can then calculate truemat
  # (if we are outside of vr, indicated by truemat_vr missing, then re calculate based on tot_vr/tot)
  temp_final <- temp_final %>%
    dplyr::mutate(missing_truemat_can_be_calculated = ifelse(is.na(truemat) & up > 0 & !is.na(tp) & !is.na(fn) & !conflicting_envelopes, TRUE, FALSE)) %>%
    dplyr::mutate(truemat = ifelse(missing_truemat_can_be_calculated, tp + fn + up, truemat)) 
  
  
  # If not scaled with multiplier we can use the version that has been recalculated
  # If scaled and recalculated then scale back
  temp_final <- temp_final %>%
    dplyr::mutate(truemat_unscaled = ifelse(study_period_coverage == "all years", #this means it was not scaled
                                            truemat,
                                            ifelse(study_period_coverage == "some years" & !missing_env_before_calc,  #this means it is scaled
                                                   truemat/multiplier,
                                                   truemat_unscaled))) %>%
    dplyr::mutate(truemat_vr_unscaled  = ifelse(study_period_coverage == "all years", #this means it was not scaled
                                                truemat_vr,
                                                ifelse(study_period_coverage == "some years" & !missing_env_before_calc, #this means it is scaled
                                                       truemat_vr/multiplier,
                                                       truemat_vr_unscaled)))
  
  
  # If conflicting_envelopes remove boxes
  temp_final <- temp_final %>%
    dplyr::mutate(tp = ifelse(conflicting_envelopes, NA, tp)) %>%
    dplyr::mutate(tn = ifelse(conflicting_envelopes, NA, tn)) %>%
    dplyr::mutate(fp = ifelse(conflicting_envelopes, NA, fp)) %>%
    dplyr::mutate(fn = ifelse(conflicting_envelopes, NA, fn)) %>%
    dplyr::mutate(up = ifelse(conflicting_envelopes, NA, up)) %>%
    dplyr::mutate(un = ifelse(conflicting_envelopes, NA, un))
  
  # Removing studies which cannot be used for anything
  ##remove studies without any data --- i.e. should never have been extracted
  temp_final2 <- temp_final %>%
    dplyr::mutate(silly_extraction = 
                    is.na(tp) &
                    is.na(tn) &
                    is.na(fp) &
                    is.na(fn) &
                    (is.na(up) | up==0) &
                    (is.na(un) | un==0) &
                    is.na(truemat) &
                    is.na(truemat_vr)) %>%
    dplyr::filter(!silly_extraction)
  
  
  #remove duplicates
  temp_final2 <- temp_final2 %>% 
    # dplyr::filter(mat_deaths_recorded) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(duplicate_to_remove = process_study_duplicate_removal_indicator(
      data = temp_final2, 
      year_start = year_start, 
      year_end = year_end,
      iso_alpha_3_code = iso_alpha_3_code,
      entry_year = entry_year,
      entry_type = entry_type)) %>% 
    dplyr::filter(!duplicate_to_remove) 
  
  #check for identical duplicates --- the code above only removes duplicates from different rounds and entry dates
  ns <- temp_final2 %>% 
    dplyr::group_by(iso_alpha_3_code, year_start, year_end) %>%
    dplyr::mutate(n = dplyr::n()) %>%
    dplyr::filter(n > 1)
  if(nrow(ns)>1) {
    print(paste(ns$iso_alpha_3_code, ns$year_start,"-",ns$year_end, "has an identical duplicate."))
    stop()
  }
  
  
  ################################################################################################## 
  #################       completeness calculation                  ###############################
  ################################################################################################## 
  
  # Future to do
  # clean this code by 
  # use dplyr::mutate to create two columns
  # env_from_lifetables
  # env_from_study_imputed_with_vr_reported_total
  # env_for_comparison = either of the above depending on if study checks inside vr or goes beyong
  # completeness = env_for_comparison / env_from_lifetables
  
  # make temporary columns for calculation of completness_inq
  # write.csv(temp_final2, row.names = FALSE, "temp_final2.csv")
  # temp_final2 <- read.csv(here::here("temp_final2.csv"))
  temp_final2 <- temp_final2 %>%
    dplyr::mutate(completeness_inq = NA) %>%
    dplyr::mutate(tot2 = tot,
                  tot_vr2 = tot_vr,
                  env_total2 = env_total)
  
  ########### hack from 2019 estimation ######
  temp_final2 <- temp_final2 %>%
    dplyr::mutate(env_total2 = ifelse(iso_alpha_3_code == "PRY", NA, env_total2)) 
  ############################################
  
  choose <- seq(1, length(temp_final2$tot2))
  for (i in choose) {
    if (!is.na(temp_final2$truemat_unscaled[i])){ 
      deaths_for_env <- (meta_precrisis$deaths.ct[which(meta_precrisis$iso.c == temp_final2$iso_alpha_3_code[i]),])
      cal_years_for_env <- meta_precrisis$year.t
      deaths <- GetSums(year_start = temp_final2$year_start[i],
                        year_end = temp_final2$year_end[i],
                        tosum.t = deaths_for_env,
                        calyear.t = cal_years_for_env,
                        na.rm = TRUE )# when TRUE, does give NA if all inputs are NA (instead of 0)
      temp_final2$tot2[i] <- (deaths)
    } else {
      # check if there is a study envelope
      if (!is.na(temp_final2$env_total2[i])){
        temp_final2$tot_vr2[i] <- temp_final2$env_total2[i]
      } else { 
        vr_tmp <-
          vrdata %>%
          dplyr::filter(iso_alpha_3_code == temp_final2$iso_alpha_3_code[i])
        deaths_for_env <- (vr_tmp$obs_env)
        cal_years_for_env <- floor(vr_tmp$year_mid)
        deaths <- GetSums(year_start = temp_final2$year_start[i],
                          year_end = temp_final2$year_end[i],
                          tosum.t = deaths_for_env,
                          calyear.t = cal_years_for_env,
                          na.rm = TRUE) # when TRUE, does give NA if all inputs are NA (instead of 0)
        if (!is.na(deaths)){
          temp_final2$tot_vr2[i] <- (deaths)
        } else {
          print(paste("no VR envelope found for",
                      paste0(temp_final2[i, c("iso_alpha_3_code", "year_start", "year_end")], " - ", collapse = "")))
        }
      }
    }
    # coding taking from below (better to move)
    env_total2 <- ifelse(!is.na(temp_final2$truemat_unscaled[i]), temp_final2$tot2[i], temp_final2$tot_vr2[i])
    # get who envelope to calculate completeness
    deaths_for_env <- (meta_precrisis$deaths.ct[which(meta_precrisis$iso.c == temp_final2$iso_alpha_3_code[i]),])
    cal_years_for_env <- meta_precrisis$year.t
    env_total_lifetables <- GetSums(year_start = temp_final2$year_start[i],
                                    year_end = temp_final2$year_end[i],
                                    tosum.t = deaths_for_env,
                                    calyear.t = cal_years_for_env,
                                    # this argument is wrong way around
                                    na.rm = TRUE )# when TRUE, does give NA if all inputs are NA (instead of 0)
    temp_final2$completeness_inq[i] <- env_total2/env_total_lifetables
  }
  ##################################################################################################  
  ################################################################################################## 
  
  
  
  ################################################################################################## 
  #################       INDIA COMPLETNESS_INQ HACK                  ##############################
  ################################################################################################## 
  temp_final2 <- temp_final2 %>%
    dplyr::mutate(tot2 = ifelse(iso_alpha_3_code == "IND"|iso_alpha_3_code=="ZAF", env_total2, tot2)) %>%
    dplyr::mutate(tot_vr2 = ifelse(iso_alpha_3_code == "IND"|iso_alpha_3_code=="ZAF", env_total2, tot_vr2)) %>%
    dplyr::mutate(completeness_inq = ifelse(iso_alpha_3_code == "IND"|iso_alpha_3_code=="ZAF", 1, completeness_inq))
  
  
  ##################################################################################################  
  ################################################################################################## 
  
  
  
  ################################################################################################## 
  #########################              Inclusion criteria           ############################## 
  ##################################################################################################
  
  
  # Inclusion criteria for BMis and BMat
  temp_final2 <- temp_final2 %>%
    dplyr::mutate(include_general_criteria = !(year_mid < 1985) & iso_alpha_3_code %in% meta$iso.c) %>% #before 1985 and has lifetables data (might not need lifetables for bmis but leave this for now)
    dplyr::mutate(include_bmat = 
                    include_general_criteria & 
                    !(is.na(truemat_unscaled) & is.na(truemat_vr_unscaled))
    ) %>%
    dplyr::mutate(include_bmis_global = 
                    include_general_criteria &
                    study_period_coverage == "all years" & 
                    !(year_start < 1985 & check_outside_of_vr == "Y") 
    ) %>%
    dplyr::mutate(include_bmis = 
                    include_general_criteria & 
                    !(study_period_coverage == "none") & 
                    !(year_start < 1985 & check_outside_of_vr == "Y") &
                    has_vr_in_reference_year # since coverage is not all in some cases need to at a minimum have vr in reference year
    ) %>%
    dplyr::filter(year_mid >= 1985) %>% #we don't even need to show data before this date
    dplyr::mutate(include_bmat_reason = ifelse(!include_bmat, "No information on maternal deaths", NA)) %>%
    dplyr::mutate(include_bmat = ifelse(((!is.na(tot2) | !is.na(tot_vr2))), include_bmat, FALSE)) %>%
    dplyr::mutate(include_bmat_reason = ifelse(((!is.na(tot2) | !is.na(tot_vr2))), include_bmat_reason, "CRVS usability too low")) %>%
    # Hard coded and lacking documentation
    dplyr::mutate(include_bmat = ifelse( #EP hack for Norway FP models
      dmname == "FP" & !is.na(dmname),
      FALSE,
      include_bmat)
    ) %>%
    dplyr::mutate(include_bmat_reason = ifelse( #EP hack for Norway FP models
      dmname == "FP" & !is.na(dmname),
      "Norway FP data removed",
      include_bmat_reason)
    ) %>%
    dplyr::mutate(include_bmis_reason = ifelse(!include_bmis, "no coverage OR no coverage in reference OR years out of bounds", NA))
  
  # Hard coded and lacking documentation
  temp_final2 <- temp_final2 %>%
    dplyr::mutate(include_bmis = ifelse(
      iso_alpha_3_code == "BRA" & year_start %in% c(2000, 2005), #EP hack for BRA
      FALSE,
      include_bmis)
    ) %>%
    dplyr::mutate(include_bmis = ifelse(
      dmname == "remove.noinfo" & !is.na(dmname),
      FALSE,
      include_bmis)
    ) %>%
    dplyr::mutate(include_bmis = ifelse( #EP hack for Norway FP models
      dmname == "FP" & !is.na(dmname),
      FALSE,
      include_bmis)
    ) %>%
    dplyr::mutate(include_bmis_reason = ifelse(!include_bmis & is.na(include_bmis_reason), "HACK - see the function `process_study_data' for more info", NA))
  
  ssdata <- temp_final2
  ##################################################################################################  
  ################################################################################################## 
  
  
  
  # Calculate final PM 
  # If the study reported env is missing this implies the env from crvs was used. As such we use the (potentially) scaled maternal deaths in the numerator for these observations.
  # Such that if the study reported env was missing AND if crvs coverage was partial, the numerator is scaled maternal deaths. Where scaling is based on the amount of crvs coverage for the observation.
  ssdata <- ssdata %>%
    dplyr::mutate(year_mid = 1 / 2 * (year_start + year_end)) %>%
    dplyr::mutate(
      obs_matdeaths = ifelse(!is.na(truemat_unscaled), truemat_unscaled, 
                             ifelse(!is.na(env_total2), truemat_vr_unscaled,
                                    truemat_vr)), 
      final_env = ifelse(!is.na(truemat_unscaled), tot2, tot_vr2), #these are either observed or who values from vr dataset
      final_pm = obs_matdeaths/final_env
    ) %>%
    dplyr::mutate(
      obs_selogpm = NA,
      type = "inq",
      definition = "maternal",
    ) %>%
    # Select columns of interest and save data
    dplyr::select(iso_alpha_3_code,
                  year_start,
                  year_end,
                  year_mid,
                  env_total,
                  env_mat,
                  truemat_vr,
                  truemat,
                  truemat_vr_unscaled,
                  truemat_unscaled,
                  tp,
                  tn,
                  fp,
                  fn,
                  up,
                  un,
                  include_bmat,
                  include_bmis_global,
                  include_bmis,
                  include_bmat_reason,
                  include_bmis_reason,
                  incomplete_vr_multiplier = multiplier,
                  check_outside_of_vr,
                  study_period_coverage,
                  citation_short,
                  citation_long,
                  completeness_inq,
                  complete_vr = complete,
                  obs_matdeaths,
                  final_env,
                  final_pm,
                  obs_selogpm,
                  type,
                  definition,
                  mat_vr,
                  nonmat_vr,
                  tot,
                  tot_vr,
                  rho_ref,
                  mat_vr_ref,
                  tot_vr_ref,
                  tot_ref,
                  dmname
    )
  
  write.csv(ssdata, row.names = FALSE, here::here("output",round_name, "ssdata.csv"))
  
}



















