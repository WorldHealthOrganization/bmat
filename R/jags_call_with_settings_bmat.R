jags_call_with_settings_bmat <- function(global_run,
                                         iso_alpha_3_code = NULL,
                                         jags_list,
                                         meta,
                                         main_path,
                                         jags_settings,
                                         pars_to_save = pars_to_save,
                                         estimates_fixed_from_global_bmat,
                                         c_index_from_global = c_index_from_global,
                                         run_on_server) {
  
  # GG change, move this to the function that creates jags list
  dintervalbounds <- c()
  dintervalbounds[1] <- .Machine$double.eps
  dintervalbounds[2] <-  1-.Machine$double.eps
  jags_list$dintervalbounds <- dintervalbounds
  
  
  
  rnorm(1)
  set.seed(1284)
  
  if(global_run){
    AddARMA = TRUE
  } else {
    AddARMA = FALSE
  }
  
  
  
  if(run_on_server) {
    # start parallel runs, save results in steps
    library(foreach)
    library(doMC)
    registerDoMC()
    
    foreach(chain=1:jags_settings$n_chains) %dopar% {
      set.seed(chain*1239)
      mod <- R2jags::jags(data = jags_list,
                          inits = inits(AddARMA = AddARMA, meta = meta, global_run = global_run, estimates_fixed_from_global_bmat = estimates_fixed_from_global_bmat, iso_alpha_3_code = iso_alpha_3_code),
                          parameters.to.save = pars_to_save,
                          model.file = here::here(main_path, "model.txt"),
                          n.chains = 1,
                          n.burnin = jags_settings$n_burnin,
                          n.iter = jags_settings$n_burnin + jags_settings$n_iter,
                          n.thin = jags_settings$n_thin,
                          working.directory = getwd()
      )
      ########### REMOVING SOME DATA FROM FIT OBJECT TO REDUCE FILE SIZE ####################################
      
      # mod[names(mod) %in% c("model")] <- NA
      mod$BUGSoutput[names(mod$BUGSoutput) %in% c("sims.list",
                                                  "mean",
                                                  "sd")] <- NA
      saveRDS(mod, here::here(file.path(main_path, paste0(chain, "chain.rds"))))
      print(paste("MCMC results for chain ", chain, "compelte"))
    } # end chains
    
    mod <- readRDS(here::here(file.path(main_path, paste0(1, "chain.rds", sep = ""))))
    for (chain in 2:jags_settings$n_chains) {
      mod_for_one_chain <- readRDS(here::here(file.path(main_path, paste0(chain, "chain.rds", sep = ""))))
      mod$BUGSoutput$sims.array <- mod$BUGSoutput$sims.array %>% abind::abind(mod_for_one_chain$BUGSoutput$sims.array, along = 2)
    }
    # now we need to hack the fit object such that it has correct meta data (as the original git object had meta data for just 1 chain)
    mod$BUGSoutput$n.chains <- jags_settings$n_chains
  } else {
    mod <-
      R2jags::jags.parallel(
        data = jags_list,
        parameters.to.save = pars_to_save,
        model.file = here::here(main_path, "model.txt"),
        inits = inits(AddARMA = AddARMA, meta = meta, global_run = global_run, estimates_fixed_from_global_bmat = estimates_fixed_from_global_bmat, c_index_from_global=c_index_from_global, iso_alpha_3_code = iso_alpha_3_code),
        n.chains = jags_settings$n_chains,
        n.burnin = jags_settings$n_burnin,
        n.iter = jags_settings$n_burnin + jags_settings$n_iter,
        n.thin = jags_settings$n_thin,
        envir = environment()
      )
    # process_hyper(jags_list = jagslist, fit = mod)
    # mod[names(mod) %in% c("model")] <- NA
    mod$BUGSoutput[names(mod$BUGSoutput) %in% c("sims.list",
                                                "mean",
                                                "sd")] <- NA
  }
  
  
  
  return(mod) # change to fit eventually
} # end function


inits <- function(AddARMA = TRUE,  meta, global_run, estimates_fixed_from_global_bmat, c_index_from_global, iso_alpha_3_code) {
  initslist <- list()
  if (AddARMA) {
    initslist <- c(initslist, list("phi" = runif(1, 0.6, 0.9)))
  }
  #explanation of what were trying to do here
  # exp(logRstarnonaids.ct[c,t]) needs to be in 0,1 it follows
  # 0 < exp(alphac + betaz) < 1
  # -Inf < alphac + betz < 0
  # -Inf < alphac < -betaz
  # for global run we want alphacs
  # -Inf < initial_alphacs < betazstar
  # where betazstar = min(-betaz)

  if (global_run) {
    # initslist <- c(initslist, list(alpha.c = (runif(meta$C, -1, -0.2) +
    #                                             min(
    #                                               -# this is the est for Rtilde - alpha with min and max betas as defined above to get max outcome
    #                                                 (
    #                                                   -ifelse(c(meta$X.cth[, , 1]) < 0, 1.1, 0.9) * 0.4 * c(meta$X.cth[, , 1])
    #                                                   + ifelse(c(meta$X.cth[, , 2]) <
    #                                                              0, 0.9, 1.1) * 1.1 * c(meta$X.cth[, , 2])
    #                                                   - ifelse(c(meta$X.cth[, , 3]) <
    #                                                              0, 1.1, 0.9) * 0.8 * c(meta$X.cth[, , 3])
    #                                                 )
    #                                             ))))



    posterior_sd_of_alpha.c <- estimates_fixed_from_global_bmat$posterior_sd_of_alpha.c
    posterior_sd_of_beta.h <- estimates_fixed_from_global_bmat$posterior_sd_of_beta.h
    previous_beta.h <- estimates_fixed_from_global_bmat$beta.h
    previous_alpha.c <-  estimates_fixed_from_global_bmat$alpha.c

    beta.h <- c()
    for (h in 1:3) {
      beta.h[h] <- rnorm(1, mean = previous_beta.h[h], sd = 3*posterior_sd_of_beta.h[h])
    }
    betaz = min(
      -(
        -ifelse(c(meta$X.cth[, , 1]) < 0, 1.1, 0.9) * beta.h[1] * c(meta$X.cth[, , 1])
        + ifelse(c(meta$X.cth[, , 2]) < 0, 0.9, 1.1) * beta.h[2] * c(meta$X.cth[, , 2])
        - ifelse(c(meta$X.cth[, , 3]) < 0, 1.1, 0.9) * beta.h[3] * c(meta$X.cth[, , 3])
        )
    )
    alpha.c <- c()
    for(c in 1:length(meta$iso.c)) {
      alpha.c[c] <- truncnorm::rtruncnorm(1, a = previous_alpha.c[c] - 3*posterior_sd_of_alpha.c[c], b = betaz ,mean = previous_alpha.c[c], sd = 3*posterior_sd_of_alpha.c[c])
    }
    initslist <- c(initslist, list(beta.h = beta.h, alpha.c = alpha.c))
  } else {
    
    posterior_sd_of_beta.h <- estimates_fixed_from_global_bmat$posterior_sd_of_beta.h
    previous_beta.h <- estimates_fixed_from_global_bmat$beta.h
    posterior_sd_of_alpha <- estimates_fixed_from_global_bmat$posterior_sd_of_alpha.c[meta$c]
    previous_alpha <-  estimates_fixed_from_global_bmat$alpha.c[meta$c]
    
    beta.h <- c()
    for (h in 1:3) {
      beta.h[h] <- rnorm(1, mean = previous_beta.h[h], sd = 3*posterior_sd_of_beta.h[h])
    }
    betaz = min(
      -(
        -ifelse(c(meta$X.cth[1, , 1]) < 0, 1.1, 0.9) * beta.h[1] * c(meta$X.cth[1, , 1])
        + ifelse(c(meta$X.cth[1, , 2]) < 0, 0.9, 1.1) * beta.h[2] * c(meta$X.cth[1, , 2])
        - ifelse(c(meta$X.cth[1, , 3]) < 0, 1.1, 0.9) * beta.h[3] * c(meta$X.cth[1, , 3])
      )
    )
    alpha.c <- truncnorm::rtruncnorm(1, a = previous_alpha - 3*posterior_sd_of_alpha, b = betaz, mean = previous_alpha, sd = 3*posterior_sd_of_alpha)
    initslist <- c(initslist, list(alpha.c = alpha.c))
  }
  
  return(list(initslist))
} # end inits function
#-----------------
# The End!
