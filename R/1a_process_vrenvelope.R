process_vrenvelope <- function(dat_merged_withwhoenv) {
  dat_merged_withwhoenv <- dat_merged_withwhoenv %>%
    dplyr::mutate(include_hasvrdata = !is.na(env)) #%>% ####### include a reason for exclusion in the future
    #dplyr::filter(include_hasvrdata)
  
  # step 1: calculate moving average of completeness in 5 year periods
  # to decide annual rhovr
  res3 <- with(
    dat_merged_withwhoenv,
    rhovrinfoperiod(
      isos = iso,
      vrenv = env,
      whoenv = whoenv,
      years = year,
      halfperiod = 2
    )
  )
  res4 <- data.frame(
    dat_merged_withwhoenv,
    rhovr_period = res3$rhovr,
    rhovrmax_period = res3$rhovrmax,
    rhovrmin_period = res3$rhovrmin
  )
  # rule:
  # if min of maxinperiod is greater than 0.95 for all years, set rhovr = 1 for all years
  # otherwise, use the smoothed rhovr
  minperiod_c <-
    tapply(res4$rhovrmax_period, res4$iso, min, na.rm = T)
  res4$rhovrfinal <- res4$rhovr_period
  for (c in 1:length(minperiod_c)) {
    if (minperiod_c[c] > 0.95) {
      res4$rhovrfinal[res4$iso == names(minperiod_c)[c]] <- 1
    }
  }
  # res4$rhovrfinal
  # note that this is updated to get integers, see next
  
  
  # now store what's to be used in runs
  # take care with instances of vrenv > whoenv
  # and produce integer numbers for number of deaths in envelopes
  # (rhovr was calculated based on a 5 year smoothed average
  # so its use in multiplication doesn't have to result in integers)
  
  # for crvs/bmis model runs
  res4$vrenv_vradj <- floor(res4$env)
  # ytot follows from vr and rhovr so that vr info is internally consistent
  res4$ytot_vradj <- floor(res4$vrenv_vradj / res4$rhovrfinal)
  # if rhovrfinal == 1, then floor doesn't change anything and rhovrfinal is still 1
  res4$rhovrfinal_vradj <- res4$vrenv_vradj / res4$ytot_vradj
  
  # for bmat run
  # udpate 8/23/19:
  # always use vr envelope as is
  res4$vrenv_bmat <- floor(res4$env)
  # if rho == 1, then set ytot = vrenv, otherwise, set ytot = max of who and vr env
  max_vr_who <-
    ifelse(res4$whoenv < res4$env, res4$env, res4$whoenv)
  res4$ytot_bmat <- ifelse(res4$rhovrfinal == 1,
                           res4$vrenv_bmat,
                           floor(max_vr_who))
  res4$rhovrfinal_bmat <- res4$vrenv_bmat / res4$ytot_bmat
  
  # rename res4$rhovrfinal to avoid confusion
  res4$rhointermediate <- res4$rhovrfinal
  res4$rhovrfinal <- NULL
  
  # add usability and decide on inclusion
  res4 <- res4 %>%
    dplyr::mutate(usa = (1 - propill) * rhovrfinal_vradj) %>% #bmat, envelope based
    dplyr::mutate(include = ifelse(!include | !include_hasvrdata, FALSE,
                            usa >= 0.6))
  res4 <- res4 %>%
    dplyr::mutate(include_reason = ifelse(!include_hasvrdata, "missing envelope in CRVS",
                                          ifelse(usa < 0.6, "usability less than 60%", NA)))
  
  return(res4)
}


# rhovr refers to completeness
# per 5-year period, use all obs in period +/- 2 years
rhovrinfoperiod <-
  function(isos, vrenv, whoenv, years, halfperiod = 2) {
    n <- length(isos)
    res <-
      data.frame(
        rhovr = rep(NA, n),
        rhovrmin = rep(NA, n),
        rhovrmax = rep(NA, n)
      )
    for (i in 1:n) {
      select <- which(isos == isos[i] &
                        is.element(years, seq(
                          years[i] - halfperiod, years[i] + halfperiod
                        )))
      res[i, ] <-
        rhovrinfo(vrenv =  sum(vrenv[select]), whoenv = sum(whoenv[select]))
    }
    return(res)
  }

rhovrinfo <- function(vrenv, whoenv) {
  # needs to work for vectors too
  minenv <- ifelse(whoenv > vrenv, whoenv, vrenv)
  rhovr <- (vrenv) / minenv
  rhovrmin <-
    qbinom(0.025, size = floor(minenv), prob = rhovr) / floor(minenv)
  rhovrmax <-
    qbinom(0.975, size = floor(minenv), prob = rhovr) / floor(minenv)
  return(data.frame(
    rhovr = rhovr,
    rhovrmin = rhovrmin,
    rhovrmax = rhovrmax
  ))
}
