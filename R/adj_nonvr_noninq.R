#' adjust_nonvr_noninq
#'
#' @param global 
#' @param quantiles 
#' @param datall 
#' @param meta 
#' @param jags_list 
#' @param fit 
#' @param z.q 
#'
#' @return dataframe of other data with adjustments
#'
#' @examples
adjust_nonvr_noninq <- function(global,
                                quantiles,
                                datall,
                                meta,
                                jags_list,
                                fit,
                                z.q) {
  
  if(global) {
    c_draws <- fit$BUGSoutput %>% tidybayes::spread_draws(oneminpi.c[c])
  } else {
    c_draws <- fit$BUGSoutput %>% tidybayes::spread_draws(oneminpi.c)
  }
  if(jags_list$Jnew < 2) {
    j_draws <- fit$BUGSoutput %>% tidybayes::spread_draws(logphi.jnew, tau.jnew) 
  } else {
    j_draws <- fit$BUGSoutput %>% tidybayes::spread_draws(logphi.jnew[j], tau.jnew[j]) 
  }

  # lets re-write stuff below as a single data frames
  # group
  # summarize (var1 = asl;dhaosdh, var2 = kasuhdal;s)
  # practice on data in hyperpar rmd
  
  
  
  
  
  #----
  ## data type 3 (non-vr and non-inquiry)
  # 1. store the posterior draws of oneminuspi.c = omega.c
  oneminuspi.c <- rep(NA, meta$C)
  for (c in 1:meta$C) {
    if(global) {
      oneminuspi.c[c] <- c_draws %>% 
        dplyr::filter(c == !!c) %>%
        dplyr::pull(oneminpi.c) %>%
        median
    } else {
      oneminuspi.c <- NULL
      oneminuspi.c <- c_draws %>% 
        dplyr::pull(oneminpi.c) %>%
        median
    }
  }
  # first part of variance, get total SD
  se.jnew <- logoneovergamma.jnew <- rep(NA, jags_list$Jnew)
  for (j in 1:jags_list$Jnew) {
    logoneovergamma.jnew[j] <- j_draws %>% 
      dplyr::filter(j == !!j) %>%
      dplyr::pull(logphi.jnew) %>%
      median
    # logphi = log(1/gamma)
    se.jnew[j] <- j_draws %>% 
      dplyr::filter(j == !!j) %>%
      dplyr::pull(tau.jnew) %>%
      `^`(-1) %>%
      median %>%
      sqrt
  }
  j <- which(jags_list$getc.jnew == c)
  exp(logoneovergamma.jnew[j])
  # starting CI for mean term, before adjustment for pregnancy-related obs
  CI.jnewq <-
    matrix(NA, length(jags_list$getc.jnew), length(quantiles))
  for (j in 1:length(jags_list$getc.jnew)) {
    CI.jnewq[j,] <-
      exp(jags_list$logpm.jnew[j] - logoneovergamma.jnew[j] + z.q * se.jnew[j])
  }
  # for pregnancy-related obs, steps are
  # 1. remove D(aids&pregn)/env
  # 2. multiply times omega
  # 3. add back D(aids)/env
  for (j in seq(1, jags_list$Jnew)[jags_list$ispreg.jnew == 1]) {
    va <- (meta$v.ct[jags_list$getc.jnew[j], jags_list$gett.jnew[j]]
           * meta$a.ct[jags_list$getc.jnew[j], jags_list$gett.jnew[j]])
    CI.jnewq[j,] <-
      (CI.jnewq[j,] - va) * oneminuspi.c[jags_list$getc.jnew[j]] + jags_list$uaids *
      va
  }
  datall$CI.up.postmod[jags_list$isjnew.d] <-
    CI.jnewq[, which(quantiles == max(quantiles))]
  datall$CI.low.postmod[jags_list$isjnew.d] <-
    CI.jnewq[, which(quantiles == min(quantiles))]
  datall$pm.adj.postmod[jags_list$isjnew.d] <-
    CI.jnewq[, which(quantiles == median(quantiles))]
  
  return(datall)
}