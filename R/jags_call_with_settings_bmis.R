

jags_call_with_settings <- function(jags_list,
                                    jags_fit_inits,
                                    parstosave,
                                    main_path,
                                    seed,
                                    global_run,
                                    server) {
  if(!global_run){
    jags_fit_inits <- NULL
  }

  if(server) {
  # start parallel runs, save results in steps
  library(foreach)
  library(doMC)
  registerDoMC()

  foreach(chain=1:jags_list$settings$n_chains) %dopar% {
    set.seed(chain)
    fit <- R2jags::jags(data = jags_list,
                inits = jags_fit_inits,
                parameters.to.save = parstosave,
                n.chains = 1,
                n.iter = jags_list$settings$n_burnin + jags_list$settings$n_iter,
                n.burnin = jags_list$settings$n_burnin,
                n.thin = jags_list$settings$n_thin,
                model.file=  here::here(main_path, "model.txt"),
                jags.seed = seed,
                working.directory= getwd())
    i = 1 # index for which update
    ########### REMOVING SOME DATA FROM FIT OBJECT TO REDUCE FILE SIZE ####################################

    fit[names(fit) %in% c("model")] <- NA
    fit$BUGSoutput[names(fit$BUGSoutput) %in% c("sims.list",
                                                "summary",
                                                "mean",
                                                "sd")] <- NA
    ############################################################################################################
    saveRDS(fit, here::here(file.path(main_path, paste0(chain, "chain.rds"))))
    print(paste("MCMC results for chain ", chain, "compelte"))
  } # end chains

  fit <- readRDS(here::here(file.path(main_path, paste0(1, "chain.rds", sep = ""))))
  for (chain in 2:jags_list$settings$n_chains) {
    fit_for_one_chain <- readRDS(here::here(file.path(main_path, paste0(chain, "chain.rds", sep = ""))))
    fit$BUGSoutput$sims.array <- fit$BUGSoutput$sims.array %>% abind::abind(fit_for_one_chain$BUGSoutput$sims.array, along = 2)
  }
  # now we need to hack the fit object such that it has correct meta data (as the original git object had meta data for just 1 chain)
  fit$BUGSoutput$n.chains <- jags_list$settings$n_chains
  } else {
    fit <- R2jags::jags.parallel(data = jags_list,
                        inits = jags_fit_inits,
                        parameters.to.save = parstosave,
                        n.chains = jags_list$settings$n_chains,
                        n.iter = jags_list$settings$n_burnin + jags_list$settings$n_iter,
                        n.burnin = jags_list$settings$n_burnin,
                        n.thin = jags_list$settings$n_thin,
                        model.file=  here::here(main_path, "model.txt"),
                        jags.seed = seed,
                        working.directory= getwd())

    ########### REMOVING SOME DATA FROM FIT OBJECT TO REDUCE FILE SIZE ####################################
    
    fit[names(fit) %in% c("model")] <- NA
    fit$BUGSoutput[names(fit$BUGSoutput) %in% c("sims.list",
                                                "summary",
                                                "mean",
                                                "sd")] <- NA
    ############################################################################################################
  }
  return(fit)
}#end function
