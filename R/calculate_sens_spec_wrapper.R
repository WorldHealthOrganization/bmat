calculate_sens_spec_wrapper <- function(
  jags_fit,
  jags_list,
  global_run,
  round_name,
  main_data_for_plots = NULL,
  estimates_fixed_from_global = NULL,
  sens_spec_global = NULL,
  iso_alpha_3_code = NULL
) {
  # global sens spec
  if (global_run) {
    sens_spec_global <- calculate_sens_spec_global( 
      jags_fit = jags_fit,
      jags_list = jags_list
    )
    saveRDS(sens_spec_global, here::here("output", round_name, "bmis_global", "sens_spec_global.rds"))
    
    sens_spec_global_samples <- calculate_sens_spec_global( 
      jags_fit = jags_fit,
      jags_list = jags_list,
      output_samples = T
    )
    
    saveRDS(sens_spec_global_samples, here::here("output", round_name, "bmis_global", "sens_spec_global_samples.rds"))
    
  }
  # one country sens spec
  if (!global_run) {
    if(nrow(main_data_for_plots) != 0) {
      sens_spec <- calculate_sens_spec_updated(
        main_data_for_plots = main_data_for_plots,
        jags_list = jags_list,
        jags_fit = jags_fit,
        estimates_fixed_from_global = estimates_fixed_from_global
      )
      sens_spec <- sens_spec %>%
        dplyr::mutate(spec = ifelse(spec > 1, 1, spec))
    } else {
      sens_spec <- NULL
    }
    
    # read and rbind data all countries with sens spec data (iterative)
    if (file.exists(here::here("output", round_name, "bmis_onecountry", "sens_spec_countries_w_data.rds"))) {
      sens_spec_pre <- readRDS(here::here("output", round_name, "bmis_onecountry", "sens_spec_countries_w_data.rds"))
      sens_spec_pre <- sens_spec_pre %>%
        dplyr::filter(iso_alpha_3_code != !!iso_alpha_3_code)
    } else {
      sens_spec_pre <- NULL
    }
    sens_spec <- sens_spec_pre %>%
      dplyr::bind_rows(sens_spec)
    saveRDS(sens_spec, here::here("output", round_name, "bmis_onecountry", "sens_spec_countries_w_data.rds"))
  }
  return(NULL)
}