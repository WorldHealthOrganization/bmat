jags_list_wrapper <- function(main_data,
                              ssdata,
                              estimates_fixed_from_global,
                              dm_i,
                              run_setting,
                              specmin,
                              sensmin,
                              refyear,
                              first_year,
                              last_year,
                              global_run) {
  print("Compiling data inputs in list format for jags.")
  
  
  jags_list_central <- jags_list_central(ssdata = ssdata,
                                         first_year = first_year,
                                         last_year = last_year) # jags list containing general data not specific to a particular data model
  
  
  jags_list_m <- jags_list_multifarious(main_data = main_data,
                                        jags_list_central = jags_list_central,
                                        dm_i = dm_i) # jags list containing sub lists of data for each data model
  
  jags_list_blend <- jags_list_blender(jags_list_central = jags_list_central,
                                       jags_list_m = jags_list_m,
                                       specmin = specmin,
                                       sensmin = sensmin,
                                       refyear = refyear,
                                       first_year = first_year) #jags list which combines the above data into a single list
  
  
  if(!global_run){
    estimates <- estimates_fixed_from_global
    globalpars <- estimates$parameter_estimates_global
    globalpars2 <- estimates$parameter_estimates_k
    #in case we need these other paramters, E can remove if not needed
    globalpars_list <- globalpars %>%
      dplyr::select(parameter, `0.5`) %>%
      dplyr::filter(!parameter %in% c("sensworld","specworld")) %>%
      tibble::deframe()
    jags_list_blend <- append(jags_list_blend, globalpars_list)
    jags_list_blend$etaworld.k <- rep(NA, 2)
    jags_list_blend$etaworld.k[1] = globalpars2 %>% dplyr::filter(k == 1) %>% dplyr::pull(`0.5`)
    jags_list_blend$etaworld.k[2] = globalpars2 %>% dplyr::filter(k == 2) %>% dplyr::pull(`0.5`)
    jags_list_blend$sensworld <- rep(globalpars %>% dplyr::filter(parameter == "sensworld") %>% dplyr::pull(`0.5`), jags_list_blend$nyears)
    jags_list_blend$specworld <- rep(globalpars %>% dplyr::filter(parameter == "sensworld") %>% dplyr::pull(`0.5`), jags_list_blend$nyears)
    jags_list_blend$eta_se_world <- rep(globalpars2 %>% dplyr::filter(k == 1) %>% dplyr::pull(`0.5`), jags_list_blend$nyears)
    jags_list_blend$eta_spec_world <- rep(globalpars2 %>% dplyr::filter(k == 2) %>% dplyr::pull(`0.5`), jags_list_blend$nyears)
  }
  
  
  return(jags_list_blend)
}


