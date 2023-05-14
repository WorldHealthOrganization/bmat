fit_bmis <- function(
                     global_run = FALSE,
                     estimates_fixed_from_global = NULL,
                     sens_spec_global = NULL,
                     ssdata,
                     vrdata,
                     round_name = NULL,
                     jags_settings = NULL,
                     first_year = 1985,
                     last_year = 2017,
                     refyear = 2010,
                     seed = 1234,
                     specmin = 0.995,
                     sensmin = 0.1,
                     iso_alpha_3_code = NULL,
                     percentiles = c(0.1, 0.5, 0.9),
                     server = TRUE,
                     pars_to_save =  c(
                       "rhovr.ct",
                       "gamma.truematvr.ct",
                       "rho.ctb",
                       "gamma.ctb", #updated for new BMis model.
                       "sens.ct",
                       "spec.ct",
                       "beta.ct",
                       "lambda.ctk",
                       "eta.ctk",
                       "delta.cmk", #Updated for new BMis model
                       "vradj.ct",
                       "logliketn.mj",
                       "loglikefn.mj",
                       "loglikematvr.mjt",
                       "logliketruematprop.mj",
                       "loglikefp.mj",
                       "loglikefn2.mj",
                       "loglikemultiinvr.mj",
                       "loglikemultioutvr.mj",
                       "loglikemultitnfnmatvr.mj",
                       "etaworld.k",
                       "sensworld",
                       "specworld",
                       "rho.alphabeta",
                       "rho_world",
                       "sigma.alpha",
                       "sigmaworld1",
                       "sigma.beta",
                       "sigmaworld2"
                     ))
{
  ptm <- proc.time()
  
  main_path <- make_output_directory_return_path(round_name, iso_alpha_3_code, global_run, "bmis")

  if (global_run) {
    ssdata_filtered <- ssdata %>% dplyr::filter(include_bmis_global)
  } else {
    ssdata_filtered <- ssdata %>% dplyr::filter(include_bmis)
  }
  
  main_data <-
    main_data_list(
      vrdata = vrdata,
      ssdata = ssdata_filtered,
      global_run = global_run,
      iso_alpha_3_code = c(iso_alpha_3_code),
      first_year = first_year,
      bmis_run = TRUE
    )
  dm_i <-
    dm_for_each_observation(main_data = main_data, dm_rules = dm_rules())
  

  
  
  
  # note yet ready for one country run, return to edit for one country run
  jags_list <- jags_list_wrapper(
    main_data = main_data,
    ssdata = ssdata_filtered,
    estimates_fixed_from_global = estimates_fixed_from_global,
    dm_i = dm_i,
    run_setting = run_setting,
    specmin = specmin,
    sensmin = sensmin,
    refyear = refyear,
    first_year = first_year,
    last_year = last_year,
    global_run = global_run
  )
  jags_list$settings <- jags_settings
  saveRDS(jags_list, here::here(main_path, "jags_list.rds"))
  
  write_model_new(
    dmnames = dm_i,
    jags_list = jags_list,
    global_run = global_run,
    main_path = main_path
  )
  jags_fit_inits = get_bmis_inits()
  
  
  jags_fit <- jags_call_with_settings(
    jags_list = jags_list,
    jags_fit_inits = jags_fit_inits,
    parstosave = pars_to_save,
    main_path = main_path,
    seed = seed,
    global_run = global_run,
    server = server
  )
  saveRDS(jags_fit, here::here(main_path, "jags_fit.rds"))
  
  trace_plots(jags_fit,
              main_path,
              hyperpars_to_plot =  c(
                "etaworld.k",
                "sensworld",
                "specworld",
                "rho.alphabeta",
                "rho_world",
                "sigma.alpha",
                "sigmaworld1",
                "sigma.beta",
                "sigmaworld2"
              ),
              global_run = global_run
  )
  
  # jags_fit <- readRDS(here::here(main_path, "jags_fit.rds"))
  estimates <- calculate_bmis(
    jags_fit = jags_fit,
    percentiles = percentiles,
    jags_list = jags_list,
    global_run = global_run
  )
  
  saveRDS(estimates, here::here(main_path, "estimates.rds"))
  
  main_data_for_plots <-
    calculate_data_for_plots(ssdata = ssdata_filtered,
                             main_path = main_path)
  saveRDS(main_data_for_plots, here::here(main_path, "main_data_for_plots.rds"))
  
  calculate_sens_spec_wrapper(
    main_data_for_plots = main_data_for_plots,
    jags_fit = jags_fit,
    jags_list = jags_list,
    global_run = global_run,
    round_name = round_name,
    estimates_fixed_from_global = estimates_fixed_from_global,
    sens_spec_global = sens_spec_global,
    iso_alpha_3_code = iso_alpha_3_code
  )

  plot_bmis_global_adjustment(
    round_name = round_name,
    round_last_year = last_year,
    global_run = global_run
  )

  print(paste0("Output files saved to ", main_path))
  print(paste0("Total time elapsed ", round(as.numeric((proc.time() - ptm)[3]
  )), " seconds."))
}
