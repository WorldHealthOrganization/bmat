jags_list_multifarious <- function(main_data, jags_list_central, dm_i) {
  #All overlapping studies treated with exact multinomial except for early BRAZIL years because of too many possible combinations
  #Early Brazil years will be treated with MVN. Note other brazil data in later years are multinomial. So just remove Brazil from this set-up
  DMnames = c("overlapinvr.singleyrs", "overlapoutvr.singleyrs", "overlapinvr.multipleyrs",  "overlapoutvr.multipleyrs")
  index_of_dm <- which(dm_i %in% DMnames)
  if(length(index_of_dm)>0){
    iso_alpha_3_codes = rep(NA, length(index_of_dm))
    for(s in 1:length(index_of_dm)){
      iso_alpha_3_codes[s] <- main_data[[index_of_dm[s]]]$iso_alpha_3_code
    }

    jagsdata.overlap <- jags_list_multifarious_maker(main_data_subset = main_data[index_of_dm],
                                                     subfix="overlap", index_of_dm,
                                                     jags_list_central = jags_list_central)

  }else{
    jagsdata.overlap <- NULL
  }
  # not exact deleted here

  DMname= "tn + fn"
  index_of_dm <- which(dm_i == DMname )
  if(length(index_of_dm)>0){
    jagsdata.tnbinom <- jags_list_multifarious_maker(main_data_subset= main_data[index_of_dm], #List where each element is study and vr data for study i
                                                     index_of_dm=index_of_dm,  #index_of_dmion criteria for studies predefined, ie studies that have DM model
                                                     subfix = "tnbinom", jags_list_central = jags_list_central)
  }else{
    jagsdata.tnbinom <- NULL
  }

  # tp + fp
  DMname = c("tp + fp", "fp")
  index_of_dm <- which(dm_i %in% DMname )
  if(length(index_of_dm)>0){
    jagsdata.fpbinom  <- jags_list_multifarious_maker(main_data_subset = main_data[index_of_dm],
                                                      subfix= "fpbinom", index_of_dm,
                                                      jags_list_central = jags_list_central)
  }else{
    jagsdata.fpbinom <- NULL
  }

  # fn truematvr binom
  DMname= "fn truematvr binom"
  index_of_dm <- which(dm_i == DMname )
  if(length(index_of_dm)>0){
    jagsdata.fntruematvrbinom <- jags_list_multifarious_maker(main_data_subset = main_data[index_of_dm],
                                                              subfix= "fntruematvrbinom", index_of_dm,
                                                              jags_list_central = jags_list_central)
  }else{
    jagsdata.fntruematvrbinom <- NULL
  }

  # fn truemat binom
  DMname= c("fn truemattot binom")
  index_of_dm <- which(dm_i %in% DMname )
  if(length(index_of_dm)>0){
    jagsdata.fntruematbinom <- jags_list_multifarious_maker(main_data_subset = main_data[index_of_dm],
                                                            subfix= , index_of_dm,
                                                            jags_list_central = jags_list_central)
  }else{
    jagsdata.fntruematbinom <- NULL
  }

  # Multinom out VR
  DMname = "Multinom.outVR"
  index_of_dm = which(dm_i %in% DMname )
  if(length(index_of_dm)>0){
    jagsdata.multinomoutvr <- jags_list_multifarious_maker(main_data_subset = main_data[index_of_dm],
                                                           subfix =   "multioutvr" , index_of_dm,
                                                           jags_list_central = jags_list_central)
  }else{
    jagsdata.multinomoutvr <- NULL

  }

  DMname = "Multinom.inVR"
  index_of_dm = which(dm_i %in% DMname )
  if(length(index_of_dm)>0){
    jagsdata.multinominvr <- jags_list_multifarious_maker(main_data_subset = main_data[index_of_dm],
                                                          subfix =   "multiinvr" , index_of_dm,
                                                          jags_list_central = jags_list_central)
  }else{
    jagsdata.multinominvr <- NULL

  }

  DMname = "fn tp truenonmatvr multinom"
  index_of_dm <- which(dm_i %in% DMname )

  if(length(index_of_dm)>0){
    jagsdata.fntpmultinom <- jags_list_multifarious_maker(main_data_subset = main_data[index_of_dm],
                                                          subfix =   "fntpmultinom" , index_of_dm,
                                                          jags_list_central = jags_list_central)
  }else{
    jagsdata.fntpmultinom  <- NULL
  }

  # DMname = "truematvrbinom"
  # index_of_dm <- which(dm_i %in% c("overlapinvr.singleyrs", "Multinom.inVR", "overlapinvr.multipleyrs") )
  #
  # if(length(index_of_dm)>0){
  #   jagsdata.truematvrbinom <- jags_list_multifarious_maker(main_data_subset = main_data[index_of_dm],
  #                                                           subfix =   "truematvrbinom" , index_of_dm,
  #                                                           jags_list_central = jags_list_central)
  # }else{
  #   jagsdata.truematvrbinom  <- NULL
  # }

  jagslist <- list()
  jagslist[["overlap"]] <- jagsdata.overlap
  jagslist[["tnbinom"]] <- jagsdata.tnbinom
  jagslist[["fpbinom"]] <- jagsdata.fpbinom
  jagslist[["fntruematvrbinom"]] <- jagsdata.fntruematvrbinom
  jagslist[["fntruematbinom"]] <- jagsdata.fntruematbinom
  jagslist[["multinomoutvr"]] <- jagsdata.multinomoutvr
  jagslist[["multinominvr"]] <- jagsdata.multinominvr
  jagslist[["fntpmultinom"]] <- jagsdata.fntpmultinom
  # jagslist[["truematvrbinom"]] <- jagsdata.truematvrbinom
  jagslist <- Filter(Negate(is.null), jagslist)
  return(jagslist)
}
