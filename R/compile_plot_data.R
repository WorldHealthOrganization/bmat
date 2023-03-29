compile_plot_data <- function(path_bmisonecountry, path_global, isos, bmisisos){
  jagslist = readRDS(here::here(path_global, "jags_list.RDS"))
  data.ls <- list()
  
  sens_spec_global_samples <- readRDS(here::here(path_global, "sens_spec_global_samples.rds"))
  sens.st = sens_spec_global_samples[["sens.st"]]
  spec.st = sens_spec_global_samples[["spec.st"]]

  sens_spec_global = readRDS(here::here(path_global, "sens_spec_global.RDS"))
  
  for(i in 1:length(isos)){
  
  
  if(isos[i] %in% bmisisos){
    estimates_bmis = readRDS(here::here(path_bmisonecountry, isos[i], "/estimates.RDS"))  #obtain se and sp estimates
    temp_est = estimates_bmis$parameter_estimates_ct %>%
      dplyr::mutate(version = "BMis original")
    
     
    estimates_bmis_post = readRDS(here::here(path_bmisonecountry, "/sens_spec_countries_w_data.RDS")) %>%
    dplyr::filter(iso_alpha_3_code == isos[i]) %>%
      dplyr::select(c("iso_alpha_3_code", "t", "lower_sens", "sens", "upper_sens", "lower_spec","spec", "upper_spec"))
    
      temp_se = estimates_bmis_post[, c("iso_alpha_3_code", "t", "lower_sens", "sens", "upper_sens")] %>%
        dplyr::mutate(parameter = "sens") %>%
        dplyr::rename(c('0.1' = "lower_sens", '0.5' = "sens", '0.9' = "upper_sens"))
      
      temp_sp = estimates_bmis_post[, c("iso_alpha_3_code", "t", "lower_spec", "spec", "upper_spec")] %>%
        dplyr::mutate(parameter = "spec") %>%
        dplyr::rename(c('0.1' = "lower_spec", '0.5' = "spec", '0.9' = "upper_spec"))
      
    bmis_estimates<- bind_rows(temp_se, temp_sp)%>%
      dplyr::mutate(`0.9` = as.numeric(`0.9`)) %>%
      dplyr::mutate(`0.5` = as.numeric(`0.5`)) %>%
      dplyr::mutate(`0.1` = as.numeric(`0.1`)) %>%
      dplyr::mutate(version = "Post-processed") %>%
      dplyr::bind_rows(temp_est) %>%
      dplyr::mutate(year_mid = t + 1985-1,
                    type = NA,
                    value_type = NA,
                    value = NA,
                    value_low = NA,
                    value_up = NA) %>%
      dplyr::select(-c("c", "t", "year_reference"))
    
    
    
    observed <- readRDS(here::here(path_bmisonecountry, isos[i], "/main_data_for_plots.RDS")) %>% #obtain BMis data inputs
      dplyr::filter(iso_alpha_3_code == isos[i] & 
                      parameter %in% c("gamma_mat_vr", "gamma_truemat_vr", "sens", "spec", "rho_truemat_vr")) %>%
      dplyr::mutate(value_type = "original",
                    version = NA,
                    year_mid = year_reference+0.5,
                    value_low = as.numeric(`0.1`),
                    value = as.numeric(`0.5`),
                    value_up = as.numeric(`0.9`)) %>% #add an indentifier before we rbind these data sets
      dplyr::mutate(`0.9` = NA) %>%
      dplyr::mutate(`0.5` = NA) %>%
      dplyr::mutate(`0.1` = NA) %>%
      dplyr::mutate(`0.1` = NA) %>%
      dplyr::mutate(type = ifelse(parameter %in% c("gamma_truemat_vr", "sens", "spec"), "inq", "vr")) %>%
      dplyr::select(c("iso_alpha_3_code", "year_mid", "parameter", '0.1', '0.5', '0.9', "version", "value_type",
                      "value", "value_low", "value_up", "type"))
    
    
    
    
    
    bmis_data = bind_rows(bmis_estimates, observed) %>%
      dplyr::filter(parameter %in% c("gamma_mat_vr", "gamma_truemat_vr", "sens", "spec", "rho_truemat_vr"))
    
     }else{
    
    estimates_global_se =  sens_spec_global%>%  #obtain se and sp estimates
      mutate(iso_alpha_3_code = isos[i],
             year_mid = t+1985-1,
             '0.1' = lower_sens,
             '0.5' = sens,
             '0.9' = upper_sens,
             c=NA,
             parameter = "sens") %>%
      select(c(iso_alpha_3_code, year_mid, parameter, '0.1', '0.5', '0.9'))
    
    estimates_global_sp = sens_spec_global%>% 
      mutate(iso_alpha_3_code = isos[i],
             year_mid = t+1985-1,
             '0.1' = lower_spec,
             '0.5' = spec,
             '0.9' = upper_spec,
             parameter = "spec",
             c=NA) %>%
      select(c(iso_alpha_3_code, year_mid,parameter, '0.1', '0.5', '0.9'))
    
    estimates_global = bind_rows(estimates_global_se,estimates_global_sp) %>%
      dplyr::mutate(`0.9` = as.numeric(`0.9`)) %>%
      dplyr::mutate(`0.5` = as.numeric(`0.5`)) %>%
      dplyr::mutate(`0.1` = as.numeric(`0.1`)) %>%
      dplyr::mutate(version = "Post-processed") %>%
      dplyr::mutate(
                    type = NA,
                    value_type = NA,
                    value = NA,
                    value_low = NA,
                    value_up = NA) 
      
    
    bmis_data =  estimates_global
  }
    
    

bmat_estimates = readRDS(here::here(path_bmatonecountry, isos[i], "/estimates.RDS")) %>% #obtain BMat estimates
    mutate(version = "BMat final estimate")
  
  
  main_data_adjusted <- readRDS (here::here(path_bmatonecountry, isos[i], "/main_data_adjusted.RDS")) %>%
    dplyr::filter(iso_alpha_3_code == isos[i])
  
  
  
  mmrdata <- main_data_adjusted %>%
    dplyr::select(iso_alpha_3_code, year_mid, mmr.adj.postmod, mmr.CI.up.postmod, mmr.CI.low.postmod, type) %>%
    dplyr::rename(value = mmr.adj.postmod,
                  value_low = mmr.CI.up.postmod,
                  value_up = mmr.CI.low.postmod) %>%
    tibble::add_column(parameter = "mmr") %>%
    tibble::add_column(value_type = "adjusted") %>%
    dplyr::mutate(value = value*100000) %>%
    dplyr::mutate(value_low = value_low*100000) %>%
    dplyr::mutate(value_up = value_up*100000)
  
  mmrdata2 <- main_data_adjusted %>%
    dplyr::select(iso_alpha_3_code, year_mid, final_mmr, type) %>%
    dplyr::rename(value = final_mmr) %>%
    tibble::add_column(parameter = "mmr") %>%
    tibble::add_column(value_type = "original") %>%
    dplyr::mutate(value = value*100000)
  
  
  pmdata <- main_data_adjusted %>%
    dplyr::select(iso_alpha_3_code, year_mid, pm.adj.postmod, CI.up.postmod, CI.low.postmod, type) %>%
    dplyr::rename(value = pm.adj.postmod,
                  value_low = CI.up.postmod,
                  value_up = CI.low.postmod) %>%
    tibble::add_column(parameter = "pm") %>%
    tibble::add_column(value_type = "adjusted")
  
  pmdata2 <- main_data_adjusted %>%
    dplyr::select(iso_alpha_3_code, year_mid, final_pm, type) %>%
    dplyr::rename(value = final_pm) %>%
    tibble::add_column(parameter = "pm")%>%
    tibble::add_column(value_type = "original")
  
  
  
  estimates_onecountry <- bmat_estimates %>%
    tibble::add_column(estimate_version = "country") %>%
    dplyr::mutate(estimate_version = as.factor(estimate_version))
  
  estimates <- bmat_estimates %>% dplyr::mutate(year_mid = as.character(year_mid))
  
  observed_data <- dplyr::bind_rows(mmrdata, mmrdata2, pmdata, pmdata2)
  
  
  observed_data <- observed_data %>% dplyr::mutate(year_mid = as.character(year_mid))
  
  cond_adjdat =    data.frame(array(NA, c(35,10)))
  names(cond_adjdat) = c("iso_alpha_3_code", "year_mid", "parameter", "0.1", "0.5", "0.9","value_type")
  
  cond_adjdat$year_mid =  as.numeric(unique(estimates$year_mid)[1:35])
  cond_adjdat$iso_alpha_3_code =   isos[i]
  cond_adjdat$version  ="Global convergence"
  cond_adjdat$parameter =  "CRVSadj"
  
  for(a in 1:35){
    c_index = which(jagslist$iso_alpha_3_code.c == isos[i])
    t_index = a
    if(isos[i] %in% bmisisos){
      output = calculate_sens_spec_updated(main_data_for_plots = readRDS(here::here(path_bmisonecountry, isos[i], "/main_data_for_plots.RDS")),
                                           jags_list =readRDS(here::here(path_bmisonecountry, isos[i], "/jags_list.RDS")),
                                           jags_fit =readRDS(here::here(path_bmisonecountry, isos[i], "/jags_fit.RDS")),
                                           estimates_fixed_from_global =readRDS(here::here(path_global,"estimates.RDS")),
                                           return_samples = T)
      
      cond_samples = output
      se.s = cond_samples[1,1,,t_index]
      sp.s = cond_samples[1,2,,t_index]
      
    }else{

      se.s = sens.st[, t_index]
      sp.s = spec.st[, t_index]
    }
    
    draws_w_calc =readRDS(here::here(path_bmatonecountry, isos[i],"/draws_w_calculation.rds")) %>%
      mutate(truepm = mmr * births/deaths) %>%
      filter(t == t_index)
    truepm.s = draws_w_calc$truepm
    
    
    cond_adj.s = truepm.s/ (se.s * truepm.s + (1-sp.s)* (1-truepm.s))
    cond_adj_quants = quantile(cond_adj.s, probs = c(0.1, 0.5, 0.9))
    cond_adjdat$`0.1`[a] = cond_adj_quants[1]
    cond_adjdat$`0.5`[a] = cond_adj_quants[2]
    cond_adjdat$`0.9`[a] = cond_adj_quants[3]
  }
  
  
  estimates2 <- estimates %>%
    mutate(year_mid = as.numeric(year_mid)) %>%
    dplyr::bind_rows(cond_adjdat) 
  
  observed_data$year_mid = as.numeric(observed_data$year_mid)
  
  data <- estimates2 %>%
    dplyr::mutate(year_mid = as.numeric(year_mid)) %>%
    dplyr::full_join(observed_data, by = c("iso_alpha_3_code", "value_type", "year_mid", "parameter"))  %>%
    dplyr::mutate(parameter = as.factor(parameter))
  
  
  data_all = bind_rows(data, bmis_data)
  
  data.ls[[i]] = data_all
}

  tot_data =  do.call("rbind", data.ls) %>%
    dplyr::filter(iso_alpha_3_code %in% c("BRA", "JPN", "SUR", "BEL", "EGY")) %>%
    dplyr::mutate(version = ifelse(version %in% c("BMat final estimate", "Global convergence", "Post-processed"), "Final Model Estimate", version),
                  adjustment_type = ifelse(value_type == "original", "Unadjusted input data", "Model adjusted data"),
                  data_type = ifelse(type == "inq", "Special Inquiry", ifelse(type == "vr", "CRVS", ifelse(type == "dhs", "DHS", "Miscellaneous"))),
                  casef = factor(iso_alpha_3_code, levels = c("BRA",  "JPN", "SUR", "BEL", "EGY"),
                                 labels = c("Case I", "Case II", "Case III", "Case IV", "Case V")),
                  isof = factor(iso_alpha_3_code, levels = c("BRA",  "JPN", "SUR", "BEL", "EGY"),
                                labels = c("Brazil", "Japan", "Suriname", "Belgium", "Egypt")),
                  paramf = factor(parameter, levels = c("gamma_tn", "gamma_tp", "gamma_fn", "gamma_fp", "gamma_truemat_vr", "gamma_mat_vr", "rho_tot_vr", "sens", "spec"), 
                                  labels = c("T-", "T+", "F-", "F+", "true PM", "CRVS PM", "completeness", "sens", "spec")))
  
  
  

`%!in%` = Negate(`%in%`)

exclude.ls = list()
for(i in 1:length(bmisisos)){
  years_observed <- tot_data %>%
    dplyr::filter(iso_alpha_3_code == bmisisos[i] & value_type == "original" & parameter %in% c( "rho_tot_vr", "gamma_tn", "gamma_tp", "gamma_fp", "gamma_fn", "gamma_mat_vr", "gamma_truemat_vr")) %>%
    dplyr::pull(year_mid) %>%
    unique()

  exclude.ls[[i]] = which(tot_data$version == "BMis original"  &
                            tot_data$parameter %in% c( "rho_tot_vr", "gamma_tn", "gamma_tp", "gamma_fp", "gamma_fn", "gamma_mat_vr", "gamma_truemat_vr") &
                            tot_data$iso_alpha_3_code == bmisisos[i] &
                            tot_data$year_mid %!in% floor(years_observed))

}

exclude = unlist(exclude.ls)
tot_data = tot_data[-exclude,]



return(tot_data)


}
