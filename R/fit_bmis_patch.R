fit_bmis_patch <- function(
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
  
 
  jags_list <- readRDS(here::here(main_path, "jags_list.rds"))
  
 
  jags_fit <- readRDS(here::here(main_path, "jags_fit.rds"))
  
 
  main_data_for_plots <- readRDS(here::here(main_path, "main_data_for_plots.rds"))
  
  
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
  
  
}
