calculate_sens_spec_global <- function(jags_fit, jags_list, output_samples = F) {

  mcmc_array <- jags_fit$BUGSoutput$sims.array
  S <- dim(mcmc_array)[1] * dim(mcmc_array)[2]
  probitsensworld.s <- c(mcmc_array[,,"etaworld.k[1]"])
  probitspecworld.s <- c(mcmc_array[,,"etaworld.k[2]"])
  sensworld.s <- c(mcmc_array[,,"sensworld"])
  specworld.s <- c(mcmc_array[,,"specworld"])
  rhoworld.s <- c(mcmc_array[,,"rho_world"])
  rho.alphabeta.s <- c(mcmc_array[,,"rho.alphabeta"])
  sigma.beta.s <- c(mcmc_array[,,"sigma.beta"])
  sigma.alpha.s <- c(mcmc_array[,,"sigma.alpha"])
  sigmaworld1.s <- c(mcmc_array[,,"sigmaworld1"])
  sigmaworld2.s <- c(mcmc_array[,,"sigmaworld2"])
  
  
  # get global info on sens.t and spec.t
  # this is for .st, next for .ts...
  
  J = 5000 #we set number of tractejories to be 100 to increase samples.
  # get global info on sens.t and spec.t
  # this is for .st, next for .ts...
  probitsens.sjt <-  probitspec.sjt <- sens.sjt <- spec.sjt  <- array(NA, c(S, J, jags_list$nyears))
  delta.mksj = array(NA, c(jags_list$nyears-1, 2,S,J))
  
  
  #Using MVN conditioning  probit transform spec on sens
  for(s in 1:S){

    mean_probitsens.j <- rnorm(J,probitsensworld.s[s], sigmaworld1.s[s])
    condmean_probitspec.j <- probitspecworld.s[s] + sigmaworld2.s[s]/sigmaworld1.s[s] * rhoworld.s[s] * (mean_probitsens.j - probitsensworld.s[s])
    mean_probitspec.j <- rnorm(J, condmean_probitspec.j, sqrt(sigmaworld2.s[s]^2 * (1-rhoworld.s[s]^2)))
    
    #Deltas are deviations away from mean, modeled bivar normal

    
    for(tindex in  1:(jags_list$nyears-1)){
      delta.mksj[tindex,1,s,1:J] <- rnorm(J, 0, sigma.alpha.s[s])
      cond_mean_delta.sj <- sigma.beta.s[s]/sigma.alpha.s[s]*rho.alphabeta.s[s]*(delta.mksj[tindex,1,s,1:J] - 0)
      delta.mksj[tindex,2,s,1:J] <- rnorm(J, cond_mean_delta.sj,  sqrt(sigma.beta.s[s]^2 *(1-rho.alphabeta.s[s]^2)))
    }
    
    probitsens.sjt[s,1:J, 1:jags_list$nyears] = mean_probitsens.j + jags_list$Dcomb %*% delta.mksj[1:(jags_list$nyears-1),1,s,1:J]
    probitspec.sjt[s, 1:J, 1:jags_list$nyears] = mean_probitspec.j + jags_list$Dcomb %*% delta.mksj[1:(jags_list$nyears-1),2,s, 1:J]
    
    
    # sens.sjt[s,,] <- 1/(1 + exp(- probitsens.sjt[s,,]))
    # spec.sjt[s,,] <- 1/(1 + exp(- probitspec.sjt[s,,]))
    sens.sjt[s,,] <- pnorm(probitsens.sjt[s,,])
    spec.sjt[s,,] <- pnorm(probitspec.sjt[s,,])
    
  }
  
  
  
  sens.st = spec.st = probitsens.st = probitspec.st = matrix(NA, S*J, jags_list$nyears)
  
  for(t in 1:jags_list$nyears){
    sens.st[1:(S*J),t] = c(sens.sjt[,,t])
    spec.st[1:(S*J),t] = c(spec.sjt[,,t])
    probitsens.st[1:(S*J),t] = c(probitsens.sjt[,,t])
    probitspec.st[1:(S*J),t] = c(probitspec.sjt[,,t])
  }
  
  
  S =S*J
  sesp_countrynodatallyears_samples <- array(NA,c(4,S , jags_list$nyears))
  sesp_countrynodatallyears_samples[1,1:S,1:jags_list$nyears] <- sens.st
  sesp_countrynodatallyears_samples[2, 1:S, 1:jags_list$nyears] <- spec.st
  sesp_countrynodatallyears_samples[3, 1:S, 1:jags_list$nyears] <- probitsens.st
  sesp_countrynodatallyears_samples[4, 1:S, 1:jags_list$nyears] <- probitspec.st
  
  dimnames(sesp_countrynodatallyears_samples)[[1]] <- c("se", "sp", "se_trans", "sp_trans")
  
  
  #Summarize into tibble of se and sp (only) by year for country without ss data
  se_sp_country_nodata_tibble <- summarize_se_sp_tibble(se_samples.st = sens.st, sp_samples.st = spec.st)
  se_sp_country_nodata_tibble <- se_sp_country_nodata_tibble %>%
    dplyr::mutate(year_start = t + jags_list$startyear - 1)
  
  ###Update 08/08/2022 Make se and sp and vars constant across time by averaging across years to 5th significant decimal.
  #Steps: 1. For each column calculate mean, 2. Round to 5th digit, 3. Set this to be constant across years.
  sesp_tib <- data.frame(se_sp_country_nodata_tibble)
  names <- colnames(sesp_tib)
  run_names <- names[2:15]
  rounded_means <- rep(NA, length(run_names))
  for(j in 1:length(run_names)){
    rounded_means[j]<- round(mean(sesp_tib[,run_names[j]]), digits = 6)
    se_sp_country_nodata_tibble[1:jags_list$nyears,run_names[j]] <- rounded_means[j]
  }
  
  
  
  
  if(!output_samples){
  return(se_sp_country_nodata_tibble)
  }else{
    outlist  <- list()
    outlist[["sens.st"]] = sens.st
    outlist[["spec.st"]] = spec.st
    return(outlist)
  }
}
