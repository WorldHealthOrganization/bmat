adjust_vr <- function(
                      datall,
                      meta,
                      jags_list,
                      estimates,
                      fit,
                      z.q) {
  #------
  # vr data, based on sens and spec and estimate of pmtrue
  # datall$pm.adj.postmod[jags_list$isj.d] <-
  # getpmtrue(pmobs = datall$final_pm[jags_list$isj.d],
  #             #jags_list$mat.j/jags_list$env.j,
  #           # based on rounded stuff as used
  # sens = jags_list$sens.j, spec = jags_list$spec.j)
  #j = 1
  #names(estimates)
  #names(estimates$pm.cqt[1,,1])
  isos <- meta$iso.c
  years <- meta$year.t
  iso_df <- data.frame(c = 1:length(isos), iso_alpha_3_code = isos)
  years_df <- data.frame(t = 1:length(years), year_mid = years)
  
  
  for (j in 1:jags_list$J) {
    pmtrue <- estimates %>% 
      dplyr::left_join(iso_df) %>%
      dplyr::left_join(years_df) %>%
      dplyr::filter(
                    c == jags_list$getc.j[j],
                    t == jags_list$gett.j[j],
                    parameter == "pm") %>% 
      dplyr::pull(`0.5`)
    vrmultiplier <- pmtrue / getpmobs(
      pmtrue = pmtrue,
      sens = jags_list$sens.j[j],
      spec = jags_list$spec.j[j]
    )
    # la 2019/1/24
    datall$multiplier[jags_list$isj.d][j] <- vrmultiplier
    datall$pm.adj.postmod[jags_list$isj.d][j] <-
      datall$final_pm[jags_list$isj.d][j] * vrmultiplier
    

    
    # use min of 0.5 death in this calc
    env <- jags_list$env.j[j]
    p <- max(0.5 / env, datall$pm.adj.postmod[jags_list$isj.d][j])
    m <- p * env
    ci <- tolerance::negbintol.int(
      x  = m,
      # failures
      n = env - m,
      # successes, aka: size, r
      m = NULL,
      alpha = 0.2,
      P = 0.99,
      side = 2,
      method = c("CB")
    ) / env
    datall$CI.low.postmod[jags_list$isj.d][j] <-
      ci[1, "2-sided.lower"]
    datall$CI.up.postmod[jags_list$isj.d][j] <-
      min(1, ci[1, "2-sided.upper"])
  } # end loop J
  return(datall)
} # end function
