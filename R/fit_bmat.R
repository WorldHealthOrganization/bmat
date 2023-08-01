fit_bmat <- function(main_data = main_data,
                     meta = meta,
                     global_run = TRUE,
                     iso_alpha_3_code = NULL,
                     estimates_fixed_from_global_bmat = NULL,
                     round_name,
                     jags_settings,
                     referenceyear = 1990,
                     sqrtgamma0max = 0.025,
                     max.sigma.lambda = 2,
                     imputeSElogPM = 0.25,
                     getarrs = FALSE,
                     quantiles = c(0.1, 0.5, 0.9),
                     validation_settings = list(
                       validate = FALSE,
                       by_time = FALSE,
                       cutoff_year = 2008,
                       seed = 0123
                     ),
                     run_on_server = FALSE,
                     arr_periods = NULL,
                     extra_pars_to_save = NULL
                     ) {
  suppressMessages({

    main_path <- make_output_directory_return_path(round_name, iso_alpha_3_code, global_run, "bmat")
    
    
    
    if (!global_run) {
      if (length(iso_alpha_3_code) > 1) {
        stop("main_data should only have data for one country")
      }
      c_index_from_global <- which(meta$iso.c == iso_alpha_3_code)
      meta <-
        filter_global_meta(iso_alpha_3_code = iso_alpha_3_code, meta = meta)
      main_data_exl <- main_data %>% 
        dplyr::filter(iso_alpha_3_code == !!iso_alpha_3_code) %>%
        dplyr::filter(!include)
      main_data <-
        preprocess_main_data(
          main_data = main_data %>% 
            dplyr::filter(include),
          iso_alpha_3_code = iso_alpha_3_code,
          meta_filtered = meta
        )
      saveRDS(main_data_exl, here::here(main_path, "main_data_excluded.rds"))
      pars_to_save <- pars_to_save_function(global_run)
    } else {
      main_data <- main_data %>% add_validation_indicator_to_data(validation_settings = validation_settings)
      pars_to_save <- pars_to_save_function(global_run)
      main_data_exl <- main_data %>% 
        dplyr::filter(!include)
      main_data <- main_data %>% 
        dplyr::filter(include)
    }

    pars_to_save <- c(pars_to_save, extra_pars_to_save)
    
    jags_list <- jags_list_bmat(
      main_data = main_data,
      meta = meta,
      global = global_run,
      estimates_fixed_from_global_bmat = estimates_fixed_from_global_bmat,
      referenceyear = referenceyear,
      sqrtgamma0max = sqrtgamma0max,
      max.sigma.lambda = max.sigma.lambda,
      imputeSElogPM = imputeSElogPM,
      validation_settings= validation_settings
    ) 
    saveRDS(jags_list, here::here(file.path(main_path, "jags_list.rds")))
    
    if (!global_run) {
      # better if we have a single write model function but fine as two separate ones for now
      write_model_bmat_onecountry(
        jags_list = jags_list,
        main_path = main_path
      )
    } else {
      write_model_bmat_global(main_path = main_path)
    }
    fit <- jags_call_with_settings_bmat(
      global_run = global_run,
      jags_list = jags_list,
      meta = meta,
      main_path = main_path,
      jags_settings = jags_settings,
      pars_to_save = pars_to_save,
      estimates_fixed_from_global_bmat = estimates_fixed_from_global_bmat,
      iso_alpha_3_code = iso_alpha_3_code,
      c_index_from_global =c_index_from_global,
      run_on_server
    )
    saveRDS(fit, here::here(file.path(main_path, "fit.rds")))
    
    if(global_run) {
    trace_plots(jags_fit,
                round_name,
                main_path,
                hyperpars_to_plot = 
                  c("phi",
                    "sqrtgamma0", "sigma.lambda",
                    "beta.h", 
                    "nonsamplingdhs.se","nonsamplingnondhs.se",
                    "sigma.country", "sigma.region", "alpha.world"), #"alpha.r")
                jags_list = jags_list,
                global_run = global_run
    )
    }
    
    estimates <- calculate_bmat(
      fit = fit,
      meta = meta,
      jags_list = jags_list,
      quantiles = quantiles,
      main_path = main_path,
      arr_periods = arr_periods
    )

    
    
    main_data_adjusted <- adjust_main_data(
      fit = fit,
      main_data = main_data,
      meta = meta,
      jags_list = jags_list,
      estimates = estimates$estimates,
      global = global_run,
      quantiles_for_adjustment = c(0.1,0.5,0.9)
    )
    main_data_adjusted <- main_data_adjusted %>%
      dplyr::bind_rows(main_data_exl)
    
    saveRDS(main_data_adjusted, here::here(file.path(main_path, "main_data_adjusted.rds")))
  
    
      if (global_run) {
      estimates_fixed_from_global <-
        fixed_parameter_acquisition(fit = fit, meta = meta)
      saveRDS(estimates_fixed_from_global, here::here(
        file.path(main_path, "estimates_fixed_from_global.rds")
      ))
    }

    saveRDS(meta, here::here(file.path(main_path, "meta.rds")))
  })
}