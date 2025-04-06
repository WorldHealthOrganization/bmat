fit_bmat2 <- function(main_data = main_data,
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
                     extra_pars_to_save = NULL,
                     covidmodel = FALSE
                     #,
                     # rr_pars = NULL,
                     # vrcoviddatalist = NULL
) {
  suppressMessages({
    
    main_path <- make_output_directory_return_path(round_name, iso_alpha_3_code, global_run, "bmat")
    
    
    
    if (!global_run) {
      if (length(iso_alpha_3_code) > 1) {
        stop("main_data should only have data for one country")
      }
      c_index_from_global <- which(meta$iso.c == iso_alpha_3_code)
      if (!covidmodel){
        meta$deaths.ct <- meta$deathsn.ct
      }
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
      
      
      
      if (!global_run) {
        # better if we have a single write model function but fine as two separate ones for now
        if (!covidmodel){
          
          
          jags_list <- jags_list_bmat(
            main_data = main_data %>% subset(year_end<=2020),
            meta = meta,
            global = global_run,
            estimates_fixed_from_global_bmat = estimates_fixed_from_global_bmat,
            referenceyear = referenceyear,
            sqrtgamma0max = sqrtgamma0max,
            max.sigma.lambda = max.sigma.lambda,
            imputeSElogPM = imputeSElogPM,
            validation_settings= validation_settings
          ) 
          write_model_bmat_onecountry(
            jags_list = jags_list,
            main_path = main_path
          )
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
          hyperpars_to_plot = c(
            "arma.ct",
            "alpha.c",
            "mu.ct"
          )
          estimates <- calculate_bmat(
            fit = fit,
            meta = meta,
            jags_list = jags_list,
            quantiles = quantiles,
            main_path = main_path,
            arr_periods = arr_periods,
            add_covid = covidmodel
          )
          
          
          saveRDS(estimates, here::here(file.path(main_path, "estimatesnoncovid.rds")))
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
          saveRDS(main_data_adjusted, here::here(file.path(main_path, "main_data_adjustednoncovid.rds")))
          
        } else {
         
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
          # extend data with indices of covid years
          jags_list$t2023 <- which(meta$year.t == 2023)
          jags_list$t2019 <- which(meta$year.t == 2019)
          jags_list$tcovid <- which(meta$year.t %in% c(2020, 2021, 2022))
          jags_list$tnoncovid <- c(1:jags_list$t2019, jags_list$t2023)
          
          # this one not yet subsetted in filter_global_meta (could add)
          jags_list$coviddeaths.t <- c(meta$coviddeaths.ct)
          # set minimum to 1 (else log-scale doesn't work)
          jags_list$coviddeaths.t <- ifelse(jags_list$coviddeaths.t == 0, 1, jags_list$coviddeaths.t)
          # this one has been subsetted
          jags_list$gfr.ct <- meta$gfr.ct
          jags_list$k <- 1.5
          #jags_list$matcoviddeaths_precision <- 1/0.1^2
          
          pars_to_save <- c(pars_to_save, "logmatcoviddeaths.ct", 
                            #"coviddeathssample.t", 
                            "ksample.t")
          
          
          # old
          # jags_list$rr_mean <- rr_pars$mean
          # jags_list$rr_precision <- 1/rr_pars$sd^2
          
          # check for VR data in covid years 
          if (!is.null(jags_list$gett.j)){
            if (any(jags_list$gett.j %in% jags_list$tcovid)){
              print("VR data in covid years is treated differently, number of points")
              print(sum(jags_list$gett.j %in% jags_list$tcovid))
              # exclude from noncovid dm, and only include if completeness is high enough
              # just calcualte completeness first 
              iscovidyear <- (jags_list$gett.j %in% jags_list$tcovid)
              iscomplete <- jags_list$env.j/c(jags_list$E.ct[1, jags_list$gett.j]) >= 0.95
              if (sum(iscovidyear & !iscomplete) > 0){
                print("At least one VR data point excluded because completeness is less than 95%, points left")
                print(sum(iscovidyear & iscomplete))
              }
              jags_list_old <- jags_list
              # note: do make sure all terms in .j are updated, else mismatches!
              # below we do isj.d, there are a few more terms in jagslist but those are not used 
              # (would be better to delete them!)
              for (vari in c("mat.j",  "env.j", "var_sens.j", "var_spec.j", "cov_sesp.j", 
                             "varfrombeta.j", # not used 
                             # not usedL  #(sens_sq.j[j] + oneminspec_sq.j[j])
                             "getc.j", # not used
                             "gett.j", "sens.j", "spec.j",
                             "cov_sesp.j")){
                jags_list[[vari]] <- jags_list_old[[vari]][!iscovidyear]
                # replace .j with .v in vari 
                # only take the ones with high enough completeness
                jags_list[[gsub(".j", ".v", vari, fixed = TRUE)]] <- jags_list_old[[gsub(".v", ".j", vari, fixed = TRUE)]][iscovidyear & iscomplete]
              }
              jags_list$J <- length(jags_list$gett.j)
              jags_list$V <- length(jags_list$gett.v)
              
              # jags_list$isj.d updated here as well
              # the whole isj.d set up seems error prone...
              # need to not update ordering of data points!
              # but used just for plotting here so not updating right now
              
              # for VR indices, select the TRUEs and set to FALSE if in covid year
              jags_list$isj.d[which(jags_list_old$isj.d)][iscovidyear] <- FALSE
              # add .v indices for the completes only 
              jags_list$isv.d <- rep(FALSE, length(jags_list$isj.d))
              jags_list$isv.d[which(jags_list_old$isj.d)][iscovidyear & iscomplete] <- TRUE
              #return(jags_list)
              
              
              #return(jags_list)
              ## for predictions, not yet updated
              ## jags_list <- c(jags_list, vrcoviddatalist)
              ## jags_list$ncovidvr <- length(vrcoviddatalist$covidvr.t)
              ## if (!is.null(vrcoviddatalist)){
              ## to add to parameters predmatcovidfree.t
              ## pars_to_save <- c(pars_to_save, "predmatcovidfree.t")
              
            }
          }
          
          write_model_bmat_onecountry_covid(
            jags_list = jags_list,
            main_path = main_path
          )
          
          
          pars_to_save <- c(pars_to_save, "logmatcoviddeaths.ct")
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
          saveRDS(fit, here::here(file.path(main_path, "fitcovid.rds")))
          hyperpars_to_plot = c(
            "arma.ct",
            "alpha.c",
            "mu.ct"
          )
          estimates <- calculate_bmat(
            fit = fit,
            meta = meta,
            jags_list = jags_list,
            quantiles = quantiles,
            main_path = main_path,
            arr_periods = arr_periods,
            add_covid = covidmodel
          )
          
          saveRDS(estimates, here::here(file.path(main_path, "estimatescovid.rds")))
          
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
          saveRDS(main_data_adjusted, here::here(file.path(main_path, "main_data_adjustedcovid.rds")))
          
        }
        }else {
          
        write_model_bmat_global(main_path = main_path)
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
          
          pars_to_save <- c(pars_to_save, "logmatcoviddeaths.ct")
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
          hyperpars_to_plot = c("phi",
                                "sqrtgamma0", "sigma.lambda",
                                "beta.h", 
                                "nonsamplingdhs.se","nonsamplingnondhs.se",
                                "sigma.country", "sigma.region", "alpha.world", "alpha.r")
          estimates <- calculate_bmat(
            fit = fit,
            meta = meta,
            jags_list = jags_list,
            quantiles = quantiles,
            main_path = main_path,
            arr_periods = arr_periods,
            add_covid = covidmodel
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
          
        
        }
      
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
  
  
  
  