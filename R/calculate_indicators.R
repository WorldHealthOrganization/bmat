
GetMMRForPM <- function(meta_obj,
                        dat
){
  n <- length(dat$pm)
  mmr <- rep(NA, n)
  getc.i <- Getc.i(paste(dat$iso_alpha_3_code), paste(meta_obj$iso.c))
  for (i in 1:n){
    c <- getc.i[i]
    deathswho <- GetSums(year_start = dat$year_start[i], year_end = dat$year_end[i], tosum.t = meta_obj$deaths.ct[c,], calyear.t = meta_obj$year.t)
    births <- GetSums(year_start = dat$year_start[i], year_end = dat$year_end[i], tosum.t = meta_obj$births.ct[c,], calyear.t = meta_obj$year.t)
    mmr[i] <- dat$pm[i]*deathswho/births
  }
  return(mmr)
}

# uses obs_mmr
GetPMForMMR <- function(meta_obj,
                        dat # with obs_mmr, iso_alpha_3_code, year_start, year_end
){
  n <- length(dat$obs_mmr)
  pm <- rep(NA, n)
  getc.i <- Getc.i(paste(dat$iso_alpha_3_code), paste(meta_obj$iso.c))
  for (i in 1:n){
    c <- getc.i[i]
    deathswho <- GetSums(year_start = dat$year_start[i], year_end = dat$year_end[i], tosum.t = meta_obj$deaths.ct[c,], calyear.t = meta_obj$year.t)
    births <- GetSums(year_start = dat$year_start[i], year_end = dat$year_end[i], tosum.t = meta_obj$births.ct[c,], calyear.t = meta_obj$year.t)
    pm[i] <- dat$obs_mmr[i]*births/deathswho
  }
  return(pm)
}

GetPMForMat <- function(meta_obj,
                        dat # with obs_mat
){
  n <- length(dat$obs_mat)
  pm <- rep(NA, n)
  getc.i <- Getc.i(paste(dat$iso_alpha_3_code), paste(meta_obj$iso.c))
  for (i in 1:n){
    c <- getc.i[i]
    deathswho <- GetSums(year_start = dat$year_start[i], year_end = dat$year_end[i], tosum.t = meta_obj$deaths.ct[c,], calyear.t = meta_obj$year.t)
    pm[i] <- dat$obs_mat[i]/deathswho
  }
  return(pm)
}




remove_vrcases_ssoverlap <- function(vrdat, ssdat, first_year){
  # chnage modelinclude for vr cases that have overlap country-period in ssdat
  n <- length(vrdat[,1])
  modelinclude <- rep(TRUE, n)
  modelinclude_reason <- rep("", n)
  ss2 <- ssdat %>% dplyr::filter(include) %>% dplyr::filter(year_start >= first_year)# old code is same here except also filters on  start >= 1985)
  for (isocountry in unique(vrdat$iso_alpha_3_code)){
    sscountry <- ss2 %>%
      dplyr::filter(iso_alpha_3_code==isocountry)
    for (i in which(vrdat$iso_alpha_3_code==isocountry)){
      choosess <- sum(sscountry$year_start <= vrdat$year_start[i] & sscountry$year_end >= vrdat$year_end[i])
      if (choosess > 0){
        modelinclude[i] <- FALSE
      }
    }
  }
  vrdat <- vrdat %>%
    dplyr::mutate(include = ifelse(!modelinclude, FALSE, include)) %>%
    dplyr::mutate(include_reason = ifelse(!modelinclude, "CRVS overlaps with spec. study", include_reason))
  return(vrdat)
}
