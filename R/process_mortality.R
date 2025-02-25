

process_mortality <- function(meta, mortality, mortality_to_denominate_hiv, mortality_hiv,covid_free_deaths, isos_with_aux_data){


  deathsn.ct <- deaths_to_denom_hiv.ct <- crisisdeaths.ct <-coviddeaths.ct<- crisisdeaths_covid.ct <- hiv.ct <- a.ct <- deaths.ct <- l15.ct<- t15t50.ct <- matrix(NA, meta$C, meta$nyears)
  remove <- NULL
  for (c_index in 1:meta$C){
    if (!is.element(paste(meta$iso.c[c_index]), isos_with_aux_data)){
      remove <- c(remove, c_index)
    } else {
      for (t in 1:meta$nyears){
        select <- mortality$year==meta$year.t[t] & paste(mortality$iso_alpha_3_code) == paste(meta$iso.c[c_index])
        #select2 <- mortality_to_denominate_hiv$year==meta$year.t[t] & paste(mortality_to_denominate_hiv$iso_alpha_3_code) == paste(meta$iso.c[c_index])
        select_covid <- covid_free_deaths$year==meta$year.t[t] & paste(covid_free_deaths$iso_alpha_3_code) == paste(meta$iso.c[c_index])
        select_hiv <- mortality_hiv$year==meta$year.t[t] & paste(mortality_hiv$iso_alpha_3_code) == paste(meta$iso.c[c_index])
        deaths.ct[c_index,t] <- sum(mortality$deaths[select])
        deathsn.ct[c_index,t] <- sum(covid_free_deaths$covid_free_deaths[select_covid])
        coviddeaths.ct[c_index,t] <- sum(covid_free_deaths$covid_deaths_imputed[select_covid])
        #deaths_to_denom_hiv.ct[c_index,t] <- sum(mortality_to_denominate_hiv$deaths[select2])
        hiv.ct[c_index,t] <- mortality_hiv$deaths_hiv[select_hiv]
        a.ct[c_index,t] <- hiv.ct[c_index,t]/deaths.ct[c_index,t]
        l15.ct[c_index,t] <- mortality[select & mortality$age==15,] %>% dplyr::pull(lx)
        t15t50.ct[c_index,t] <- sum(mortality$nLx[select])
        crisisdeaths.ct[c_index,t] <- sum(mortality$crisis_deaths[select])
        crisisdeaths_covid.ct[c_index,t] <- sum(mortality$crisis_deaths_COVID19[select])
      }
    }}
  
  a.ct <- ifelse(is.na(a.ct), 0 , a.ct)
  # NAs are result of current hack to
  
  # meta_precrisis$a.ct[which(is.na(meta_precrisis$a.ct),  arr.ind = TRUE)] <- 0
  
  if (!is.null(remove)){
    print("these countries are excluded because of no life table info")
    print(paste(meta$name.c[remove]))
    meta$iso.c <- meta$iso.c[-remove]
    meta$name.c <- meta$name.c[-remove]
    meta$numcode.c <- meta$numcode.c[-remove]
    meta$group.c <- meta$group.c[-remove]
    meta$reg.c <- meta$reg.c[-remove]
    meta$getr.c <- meta$getr.c[-remove]
    meta$whocode.c <- meta$whocode.c[-remove]
    meta$mdg.c <- meta$mdg.c[-remove]
    meta$C <- length(meta$iso.c)
    a.ct = a.ct[-remove,]
    l15.ct = l15.ct[-remove,]
    t15t50.ct = t15t50.ct[-remove,]
    deaths.ct = deaths.ct[-remove,]
    deathsn.ct = deathsn.ct[-remove,]
    crisisdeaths.ct = crisisdeaths.ct[-remove,]
    crisisdeaths_covid.ct = crisisdeaths_covid.ct[-remove,]
    hiv.ct = hiv.ct[-remove,]
    deaths_to_denom_hiv.ct = deaths_to_denom_hiv.ct[-remove,]
    coviddeaths.ct = coviddeaths.ct[-remove,]
  }


  m <- list(a.ct = a.ct, l15.ct = l15.ct,
            t15t50.ct = t15t50.ct, deaths.ct = deaths.ct,
            deathsn.ct = deathsn.ct, 
            crisisdeaths.ct = crisisdeaths.ct,
            crisisdeaths_covid.ct = crisisdeaths_covid.ct,
            hiv.ct = hiv.ct,
            deaths_to_denom_hiv.ct = deaths_to_denom_hiv.ct,
            coviddeaths.ct = coviddeaths.ct)
  return(c(meta,m))


}

# m$deaths.ct[1,]
