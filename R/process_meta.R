process_meta <- function(meta_precrisis,
                         crisis_years_data,
                         round_name
                         ) {
  dat <- crisis_years_data
  
  # yes this needs to be in a function
  meta_precrisis$a_nocrisis.ct <- meta_precrisis$a.ct
  meta_precrisis$crisis_years.c <- list()
  for (c in 1:meta_precrisis$C) {
    # crisis years from igme
    years <-
      unlist(dat %>% dplyr::filter(iso3 == meta_precrisis$iso.c[c]) %>% dplyr::select(year))
    # add 1 year crises with >10% deaths based on meta_precrisis
    years_add <- ifelse(
      meta_precrisis$crisisdeaths.ct[c, ] > 10 &
        meta_precrisis$crisisdeaths.ct[c, ] / meta_precrisis$deaths.ct[c, ] > 0.1,
      meta_precrisis$year.t,
      NA
    )
    years_add <- years_add[!is.na(years_add)] 
    years_remove <- NULL
    if (length(years_add) > 3) {
      for (year in years_add) {
        if (sum(years_add %in% seq(year - 2, year + 2)) > 3) {
          years_remove <- c(years_remove, seq(year - 2, year + 2))
        }
      }
    }
    
    years_add_final <- setdiff(years_add, c(years_remove))
    meta_precrisis$crisis_years.c[[c]] <- years_add_final
    crisisyears_indices <-
      which(is.element(meta_precrisis$year.t, c(years, years_add_final)))
    if (length(crisisyears_indices) == 0) {
      meta_precrisis$crisisdeaths.ct[c,] <- 0
    } else {
      meta_precrisis$crisisdeaths.ct[c,-crisisyears_indices] <- 0
      # recalculate a.ct
      meta_precrisis$a_nocrisis.ct[c, crisisyears_indices] <-
        meta_precrisis$a.ct[c, crisisyears_indices] *
        meta_precrisis$deaths.ct[c, crisisyears_indices] /
        (meta_precrisis$deaths.ct[c, crisisyears_indices] - meta_precrisis$crisisdeaths.ct[c, crisisyears_indices])
    }
  }
  meta_precrisis$deaths_incl_crisis.ct <- meta_precrisis$deaths.ct
  meta_precrisis$deaths.ct <- meta_precrisis$deaths.ct - meta_precrisis$crisisdeaths.ct
  meta_precrisis$a.ct <- meta_precrisis$a_nocrisis.ct
  
  # GG moved from script that created jags data to here
  meta_precrisis$isssa.c <- ifelse(meta_precrisis$mdg.c =="Sub-Saharan Africa",1,0)
  set.seed(123456)
  meta_precrisis$logbeta <- rnorm(1000000, 0, 1)
  # end GG edit
  
  meta <- meta_precrisis
  saveRDS(meta, here::here("output",round_name, "meta.rds"))
  
}




## the function below is similar to code above, could not tell if the one below of the code above from vignette should be used
# 
# deal_with_crisisdeaths <- function(metaobject, crisispath = "data/input/conflict_age_sex_1985_2030.dta"){
#   
#   meta <- readRDS(metaobject)
#   if (is.null(meta$deaths_inclcrisis.ct)){
#     meta$deaths_inclcrisis.ct <- meta$deaths.ct
#   } else {
#     print("Oops, did you already subtract crisis deaths?")
#     return()
#   }
#   dat <- read_dta(crisispath)
#   dat2 <- dat %>% filter(sex == 2) %>%
#     group_by(iso3, year) %>%
#     summarize(deaths = conflict15 + conflict20 + conflict25 + conflict30 + conflict35 + conflict40 + conflict45) %>%
#     rename(iso = iso3) %>%
#     select(iso, year, deaths) %>%
#     ungroup()
#   #dat2 %>% filter(iso == "ARM")
#   #View(dat2 %>% filter(iso == "VEN"))
#   dat3 <- dat2 %>% spread(key = year, value = deaths) %>%
#     select(iso, paste(meta$year.t)) %>%
#     filter(iso %in% meta$iso.c)
#   crisisdeaths.ct <- as.matrix(dat3[Getc.i(iso.i = meta$iso.c, iso.c = dat3$iso),-1])
#   # don't do anything if <10 deaths, less than 10%, >3 years
#   for (c in 1:meta$C){
#     if (sum(crisisdeaths.ct[c,]) > 0){
#       crisisdeaths.ct[c,] <- ifelse(crisisdeaths.ct[c,] > 10 & crisisdeaths.ct[c,]/meta$deaths.ct[c,] > 0.1,
#                                     crisisdeaths.ct[c,], 0)
#     }
#     # don't first exclude based on length of period (cause then even RWA disap)
#     if (sum(crisisdeaths.ct[c,])>0){
#       # easiest, just check if neighbors are crisis
#       exclude <- NULL
#       for (year in meta$year.t[crisisdeaths.ct[c,]>0]){
#         if ((year - 1) %in% meta$year.t[crisisdeaths.ct[c,]>0] &
#             (year + 1) %in% meta$year.t[crisisdeaths.ct[c,]>0] ){
#           exclude <- c(exclude, which(meta$year.t == year) + c(0,1,-1))
#         }
#       }
#       crisisdeaths.ct[c,unique(exclude)] <- 0
#     }
#   }
#   #sum(c(crisisdeaths.ct>0))
#   meta$crisisdeaths.ct <- crisisdeaths.ct
#   meta$deaths.ct <- meta$deaths.ct - crisisdeaths.ct
#   saveRDS(meta, "data/interm/meta.rds")
#   return(NULL)
# }
