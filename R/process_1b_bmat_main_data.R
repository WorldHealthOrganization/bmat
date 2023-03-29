# this function dopes the following
# - removes vrdata which is overlapped by ssdata
# - joins sens and spec
# - rbinds all of the data inputs
# - crisis adjustment
process_bmat_main_data <- function(
  ssdata,
  vrdata,
  census,
  survey,
  miscellaneous,
  meta,
  meta_precrisis,
  round_first_year,
  round_last_year,
  sens_spec_global,
  sens_spec_countries_w_data,
  round_name
) {

  # Minor changes to input data before rbind
  ssdata <- ssdata  %>%
    dplyr::rename(include = include_bmat)
  
  vrdata <- vrdata %>%
    dplyr::filter(!special_case) 
  # Remove overlap
  vrdata <- remove_vrcases_ssoverlap(vrdat = vrdata, ssdat = ssdata, first_year = round_first_year)

  # Join sens and spec data
  vrdata <-
    dplyr::left_join(x = vrdata, y = sens_spec_countries_w_data, by = c("iso_alpha_3_code", "year_start"))
  
  vrdata <- vrdata %>% coalesce_join(sens_spec_global,  by =  "year_start", join = dplyr::left_join)
  
  # Rbind all datasets
  datall <- dplyr::bind_rows(ssdata, vrdata, census, survey, miscellaneous) %>%
    dplyr::filter(iso_alpha_3_code %in% meta_precrisis$iso.c)
  
  
  datall$include_reason <- ifelse(is.na(datall$final_pm) & datall$include,
                                  "PM could not be obtained",
                                  datall$include_reason)
  datall$include <- ifelse(is.na(datall$final_pm) & datall$include,
                           FALSE,
                           datall$include)
  # startdates should go last as its the main reason
  datall$include <- ifelse(datall$year_start < 1985, FALSE, datall$include)
  datall$include_reason <- ifelse(datall$year_start < 1985, "Start date before 1985",
                                  datall$include_reason)
  
  datall$include <- ifelse(datall$year_end > (1+max(meta_precrisis$year.t)), FALSE, datall$include)
  datall$include_reason <- ifelse(datall$year_end > (max(meta_precrisis$year.t)+1), "End after last year in estimates",
                                  datall$include_reason)
  
  
  
  choose <- datall$include
  print(sum(is.na(datall$final_pm)))
  # add mmr_final, used in plotting
  final_mmr <-
    GetMMRForPM(
      meta_precrisis,
      dat = data.frame(
        'iso_alpha_3_code' = datall$iso_alpha_3_code,
        'year_start' = datall$year_start,
        'year_end' = datall$year_end,
        'pm' = datall$final_pm
      )[choose, ]
    )
  datall$final_mmr <- NA
  datall$final_mmr[choose] <- final_mmr
  
  
  ## Step 2: adjust observations in crisis periods ########################
  
  # below this point is crisis adjustment
  # GG grabbed this from the vignette, putting it here for now, can always add more separation later
  dat <- datall
  dat$final_pm_before_crisis <- dat$final_pm
  dat$final_mmr_before_crisis <- dat$final_mmr
  c_exposure <- 1 # from techn report
  j <- 0
  res <- tibble::tibble(wo_crisis = NA, w_crisis = NA,
                        pregndeaths_withcrisis = NA, gfr_ave = NA, def = NA)
  indices <- NULL
  
  for (i in which(dat$include)){
    # change, don't do for haiti
    # haiti is before start of DHS so don't exclude
    c <- which(meta$iso.c==dat$iso_alpha_3_code[i])
    if (dat$iso_alpha_3_code[i] != "HTI" &
        any(seq(floor(dat$year_start[i]), ceiling(dat$year_end[i])-1) %in%
            meta$year.t[meta$crisisdeaths.ct[c,]>0])){
      #print(i)
      wo_crisis <- GetSums(year_start = dat$year_start[i],
                           year_end = dat$year_end[i],
                           tosum.t = meta$deaths.ct[c,],
                           calyear.t = meta$year.t,
                           # this argument is wrong way around
                           na.rm = TRUE )
      w_crisis <- GetSums(year_start = dat$year_start[i],
                          year_end = dat$year_end[i],
                          tosum.t = meta$deaths_incl_crisis.ct[c,],
                          calyear.t = meta$year.t,
                          # this argument is wrong way around
                          na.rm = TRUE )
      crisis_deaths <- GetSums(year_start = dat$year_start[i],
                               year_end = dat$year_end[i],
                               tosum.t = meta$crisisdeaths.ct[c,],
                               calyear.t = meta$year.t,
                               # this argument is wrong way around
                               na.rm = TRUE )
      gfr_ave <- 1/(dat$year_end[i] - dat$year_start[i])*
        GetSums(year_start = dat$year_start[i],
                year_end = dat$year_end[i],
                tosum.t = meta$gfr.ct[c,],
                calyear.t = meta$year.t,
                # this argument is wrong way around
                na.rm = TRUE )
      pregndeaths_withcrisis <- dat$final_pm_before_crisis[i]*w_crisis
      
      ## envelope could be NA!
      if (dat$definition[i] == "pregn"){
        crisis_pregn_deaths <- crisis_deaths*c_exposure*gfr_ave
        print(paste("proportion due to crisis is", crisis_pregn_deaths/pregndeaths_withcrisis))
      } else {
        crisis_pregn_deaths <- 0
      }
      dat$final_pm[i] <-
        (dat$final_pm_before_crisis[i]*w_crisis-crisis_pregn_deaths)/wo_crisis
      dat$final_mmr[i] <- dat$final_mmr_before_crisis[i]*dat$final_pm[i]/
        dat$final_pm_before_crisis[i]
    }}
  main_data <- dat
  main_data <- main_data %>%
    dplyr::rowwise() %>%
    dplyr::mutate(env_total_calculated_from_lifetables = ifelse(year_start >= round_first_year & year_end <= round_last_year + 1 & iso_alpha_3_code %in% meta$iso.c,
                                                         impute_env_total(year_start = year_start,
                                                                          year_end = year_end,
                                                                          iso_alpha_3_code = iso_alpha_3_code,
                                                                          meta = meta_precrisis),
                                                         NA)) %>%
    dplyr::mutate(live_births_calculated_from_birthsdata = ifelse(year_start >= round_first_year & year_end <= round_last_year + 1 & iso_alpha_3_code %in% meta$iso.c,
                                                           impute_live_births(year_start = year_start,
                                                                              year_end = year_end,
                                                                              iso_alpha_3_code = iso_alpha_3_code,
                                                                              meta = meta_precrisis),
                                                           NA))
  write.csv(main_data, row.names = FALSE, here::here("output", round_name, "main_data.csv"))
  return(NULL)
}