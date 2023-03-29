#' dm_for_each_observation
#'
#' find the data model for each observation. requires the main data to be a list
#'
#' @param main_data
#' @param dm_rules
#'
#' @return dm_name_i
dm_for_each_observation <- function(main_data, dm_rules = dm_rules()){
  dm_i <- rep(NA, length(main_data))
  for (i in 1:length(main_data)) {
    tryCatch({
      single_ss_obs = main_data[[i]] #single obserbation from the input data list
      single_ss_obs$year_start = floor(single_ss_obs$year_start)
      single_ss_obs$year_end = floor(single_ss_obs$year_end)
      single_ss_obs$ytot <- ifelse(length(single_ss_obs$ytot) == 0, NA, single_ss_obs$ytot)
      single_ss_obs$ytot_vr <- ifelse(length(single_ss_obs$ytot_vr) == 0, NA, single_ss_obs$ytot_vr)
      single_ss_obs$ymat_vr <- ifelse(length(single_ss_obs$ymat_vr) == 0, NA, single_ss_obs$ymat_vr)
      single_ss_obs$ynonmat_vr <- ifelse(length(single_ss_obs$ynonmat_vr) == 0, NA, single_ss_obs$ynonmat_vr)
      single_ss_obs$icd_utilized <- ifelse(length(single_ss_obs$icd_utilized) == 0, NA, single_ss_obs$icd_utilized)
      dm_i [i]<- dm_for_each_observation_sub(single_ss_obs = single_ss_obs, dm_rules = dm_rules)
      dm_i[i] = ifelse(dm_i[i] == "Multinom.intruemat", "Multinom.inVR", dm_i[i])
      dm_i[i] = ifelse(dm_i[i] == "Multinom.outVR", "Multinom.inVR", dm_i[i])
      dm_i[i] = ifelse(dm_i[i] == "overlapoutvr.multipleyrs" & !is.na(single_ss_obs$truemat_vr), "overlapinvr.multipleyrs", dm_i[i])
      dm_i[i] = ifelse(dm_i[i] == "overlapoutvr.singleyrs" & !is.na(single_ss_obs$truemat_vr), "overlapinvr.singleyrs", dm_i[i])
    }, error=function(e){})
  }
  return(dm_i)
}







dm_for_each_observation_sub <- function(single_ss_obs, dm_rules = dm_rules()){
  dmname <- NULL
  for (k in 1:length(dm_rules)){
    if(k %in% c(seq(1,4))){
      if (prod(is.na(single_ss_obs[dm_rules[[k]][["setofmissings"]]])) ==1 & # if the product of tn tp ... etc == 1
          prod(!is.na(single_ss_obs[dm_rules[[k]][["setofnonmissings"]]])) ==1 &
          prod((as.numeric(single_ss_obs$year_end) - as.numeric(single_ss_obs$year_start)) %in% dm_rules[[k]][["lengthyears"]])==1
      )
        dmname <- c(names(dm_rules)[k])
    }
    if(k %in% c(seq(5,(length(dm_rules)-1)))){

      if (prod(is.na(single_ss_obs[dm_rules[[k]][["setofmissings"]]])) ==1 &
          prod(!is.na(single_ss_obs[dm_rules[[k]][["setofnonmissings"]]])) ==1
      )
        dmname <- c(names(dm_rules)[k])
    }
  }
  if(k == length(dm_rules)){
    if (prod(is.na(single_ss_obs[dm_rules[[k]][["setofmissings"]]])) ==1){
      dmname = "novr"
    }
  }
  return(dmname)
}


dm_rules <- function(){
  # to define rule with its corresponding DM name and index in one place
  # output: list, k-th element list[[datamodelname]] has the rule in it

  dmrules <- list()

  ########Overlapping DMS
  dmrules[["overlapinvr.singleyrs"]]  <- list()
  dmrules[["overlapinvr.singleyrs"]][["setofmissings"]] <- c("truemat", "fn", "fp", "tp", "tn")
  dmrules[["overlapinvr.singleyrs"]][["setofnonmissings"]] <- c("truemat_vr",  "ytot_vr", "ymat_vr", "ynonmat_vr")
  dmrules[["overlapinvr.singleyrs"]][["lengthyears"]] <- 1
  dmrules[["overlapinvr.singleyrs"]][["varstouse"]] <- c("truemat_vr",  "ymat_vr", "ynonmat_vr" )
  dmrules[["overlapinvr.multipleyrs"]]  <- list()
  dmrules[["overlapinvr.multipleyrs"]][["setofmissings"]] <- c("truemat", "fn", "fp", "tp", "tn")
  dmrules[["overlapinvr.multipleyrs"]][["setofnonmissings"]] <- c("truemat_vr",  "ytot", "ytot_vr", "ymat_vr", "ynonmat_vr")
  dmrules[["overlapinvr.multipleyrs"]][["lengthyears"]] <- c(seq(2, 50))
  dmrules[["overlapinvr.multipleyrs"]][["varstouse"]] <- c("truemat_vr",  "ymat_vr", "ynonmat_vr" , "missed.t")
  dmrules[["overlapoutvr.singleyrs"]]  <- list()
  dmrules[["overlapoutvr.singleyrs"]][["setofmissings"]] <- c( "fn", "fp", "tp", "tn")
  dmrules[["overlapoutvr.singleyrs"]][["setofnonmissings"]] <- c("truemat",  "ytot", "ytot_vr", "ymat_vr", "ynonmat_vr")
  dmrules[["overlapoutvr.singleyrs"]][["lengthyears"]] <- 1
  dmrules[["overlapoutvr.singleyrs"]][["varstouse"]] <- c("truemat",  "ymat_vr", "ynonmat_vr" )
  dmrules[["overlapoutvr.multipleyrs"]]  <- list()
  dmrules[["overlapoutvr.multipleyrs"]][["setofmissings"]] <- c( "fn", "fp", "tp", "tn")
  dmrules[["overlapoutvr.multipleyrs"]][["setofnonmissings"]] <- c("truemat",  "ytot", "ytot_vr", "ymat_vr", "ynonmat_vr")
  dmrules[["overlapoutvr.multipleyrs"]][["lengthyears"]] <- c(seq(2, 50))
  dmrules[["overlapoutvr.multipleyrs"]][["varstouse"]] <- c("truemat",  "ymat_vr", "ynonmat_vr" , "missed.t")
  ######### Multinomial DMs
  dmrules[["Multinom.inVR"]]  <- list()
  dmrules[["Multinom.inVR"]][["setofmissings"]] <- c("up", "un")
  dmrules[["Multinom.inVR"]][["setofnonmissings"]] <- c("fn", "fp", "tp", "tn")
  dmrules[["Multinom.inVR"]][["lengthyears"]] <- c(seq(1, 50))
  dmrules[["Multinom.inVR"]][["varstouse"]] <- c("fn", "fp", "tp", "tn")
  dmrules[["Multinom.intruemat"]]  <- list()
  dmrules[["Multinom.intruemat"]][["setofmissings"]] <- c("un")
  dmrules[["Multinom.intruemat"]][["setofnonmissings"]] <- c("fn", "fp", "tp", "tn", "up")
  dmrules[["Multinom.intruemat"]][["lengthyears"]] <- c(seq(1, 50))
  dmrules[["Multinom.intruemat"]][["varstouse"]] <- c("fn", "fp", "tp", "tn", "up")
  dmrules[["Multinom.outVR"]]  <- list()
  dmrules[["Multinom.outVR"]][["setofmissings"]] <- NULL
  dmrules[["Multinom.outVR"]][["setofnonmissings"]] <- c("fn", "fp", "tp", "tn", "up", "un")
  dmrules[["Multinom.outVR"]][["lengthyears"]] <- c(seq(1, 50))
  dmrules[["Multinom.outVR"]][["varstouse"]] <- c("fn", "fp", "tp", "tn", "up", "un")
  dmrules[["Multinom.tp+fp+TrueNonmatVR"]]  <- list()
  dmrules[["Multinom.tp+fp+TrueNonmatVR"]][["setofmissings"]] <- c( "truemat", "fn", "tn")
  dmrules[["Multinom.tp+fp+TrueNonmatVR"]][["setofnonmissings"]] <- c("tp", "fp",  "ytot_vr", "ymat_vr", "ynonmat_vr", "truemat_vr")
  dmrules[["Multinom.tp+fp+TrueNonmatVR"]][["lengthyears"]] <- c(seq(0, 50))
  dmrules[["Multinom.tp+fp+TrueNonmatVR"]][["varstouse"]] <- c("tp", "fp", "truemat_vr")
  dmrules[["Multinom.fp+tn+truematVR"]]  <- list()
  dmrules[["Multinom.fp+tn+truematVR"]][["setofmissings"]] <- c( "truemat", "tp", "tn", "fn", "up", "un")
  dmrules[["Multinom.fp+tn+truematVR"]][["setofnonmissings"]] <- c("truemat_vr", "fp",  "ytot", "ytot_vr", "ymat_vr", "ynonmat_vr")
  dmrules[["Multinom.fp+tn+truematVR"]][["lengthyears"]] <- c(seq(0, 50))
  dmrules[["Multinom.fp+tn+truematVR"]][["varstouse"]] <- c("fp", "truemat_vr", "ymat_vr", "ynonmat_vr")
  dmrules[["fn tp truenonmatvr multinom"]]  <- list()
  dmrules[["fn tp truenonmatvr multinom"]][["setofmissings"]] <- c( "truemat","tn", "fp", "ymat_vr", "ynonmat_vr")
  dmrules[["fn tp truenonmatvr multinom"]][["setofnonmissings"]] <- c( "fn", "tp", "truemat_vr",   "ytot_vr")
  dmrules[["fn tp truenonmatvr multinom"]][["lengthyears"]] <- c(seq(0, 50))
  dmrules[["fn tp truenonmatvr multinom"]][["varstouse"]] <- c("fn", "tp", "ytot_vr")
  ######Binomial DMs
  dmrules[["truematvrbinom"]]  <- list()
  dmrules[["truematvrbinom"]][["setofmissings"]] <- c("truemat", "ymat_vr","ytot",  "ynonmat_vr", "fn", "tp", "tn", "fp")
  dmrules[["truematvrbinom"]][["setofnonmissings"]] <- c( "truemat_vr",  "ytot_vr")
  dmrules[["truematvrbinom"]][["lengthyears"]] <- c(seq(0, 50))
  dmrules[["truematvrbinom"]][["varstouse"]] <- c("truemat_vr",  "ytot_vr")
  dmrules[["truemattotbinom"]]  <- list()
  dmrules[["truemattotbinom"]][["setofmissings"]] <- c("truemat_vr", "ymat_vr","ytot_vr",  "ynonmat_vr", "fn", "tp", "tn", "fp")
  dmrules[["truemattotbinom"]][["setofnonmissings"]] <- c( "truemat",  "ytot")
  dmrules[["truemattotbinom"]][["lengthyears"]] <- c(seq(0, 50))
  dmrules[["truemattotbinom"]][["varstouse"]] <- c("truemat",  "ytot")
  dmrules[["fn truematvr binom"]]  <- list()
  dmrules[["fn truematvr binom"]][["setofmissings"]] <- c( "truemat","tn", "up", "un","tp","fp")
  dmrules[["fn truematvr binom"]][["setofnonmissings"]] <- c( "fn", "truemat_vr", "ytot",  "ytot_vr", "ymat_vr", "ynonmat_vr")
  dmrules[["fn truematvr binom"]][["lengthyears"]] <- c(seq(0, 50))
  dmrules[["fn truematvr binom"]][["varstouse"]] <- c("fn", "truemat_vr")
  dmrules[["fn truemattot binom"]]  <- list()
  dmrules[["fn truemattot binom"]][["setofmissings"]] <- c( "truemat_vr","tn", "tp","fp")
  dmrules[["fn truemattot binom"]][["setofnonmissings"]] <- c( "fn", "truemat", "ytot",  "ytot_vr", "ymat_vr", "ynonmat_vr")
  dmrules[["fn truemattot binom"]][["lengthyears"]] <- c(seq(0, 50))
  dmrules[["fn truemattot binom"]][["varstouse"]] <- c("fn", "truemat")
  dmrules[["fp"]]  <- list()
  dmrules[["fp"]][["setofmissings"]] <- c( "truemat", "truemat_vr","tp", "tn", "fn")
  dmrules[["fp"]][["setofnonmissings"]] <- c( "fp",  "ytot", "ytot_vr", "ymat_vr", "ynonmat_vr")
  dmrules[["fp"]][["lengthyears"]] <- c(seq(0, 50))
  dmrules[["fp"]][["varstouse"]] <- c("fp", "ymat_vr", "ynonmat_vr")
  dmrules[["tp + fp"]]  <- list()
  dmrules[["tp + fp"]][["setofmissings"]] <- c( "truemat","truemat_vr","tn")
  dmrules[["tp + fp"]][["setofnonmissings"]] <- c( "tp","fp",  "ytot",  "ytot_vr", "ymat_vr", "ynonmat_vr")
  dmrules[["tp + fp"]][["lengthyears"]] <- c(seq(0, 50))
  dmrules[["tp + fp"]][["varstouse"]] <- c("tp", "fp", "ymat_vr", "ynonmat_vr")
  dmrules[["tn + fn"]]  <- list()
  dmrules[["tn + fn"]] [["setofmissings"]] <- c( "fp", "tp")
  dmrules[["tn + fn"]] [["setofnonmissings"]] <- c("tn", "fn")
  dmrules[["tn + fn"]] [["lengthyears"]] <- c(seq(0, 50))
  dmrules[["tn + fn"]] [["varstouse"]] <- c("tn", "fn")
  dmrules[["fn + fp"]]  <- list()
  dmrules[["fn + fp"]][["setofmissings"]] <- c( "truemat_vr","tn", "up", "un")
  dmrules[["fn + fp"]][["lengthyears"]] <- c(seq(0, 50))
  dmrules[["fn + fp"]][["setofnonmissings"]] <- c( "fn","fp")
  dmrules[["fn + fp"]][["varstouse"]] <- c("fn", "fp", "truemat", "ymat_vr")

  ######DMS to exclude
  dmrules[["vronly"]]  <- list()
  dmrules[["vronly"]][["setofmissings"]] <- c("truemat_vr", "truemat", "fp", "tp", "fn", "tp")
  dmrules[["vronly"]][["setofnonmissings"]] <- c( "ymat_vr", "ynonmat_vr")
  dmrules[["vronly"]][["varstouse"]] <- c("ytot",  "ymat_vr", "ynonmat_vr", "ytot_vr")
  dmrules[["noinfo"]]  <- list()
  dmrules[["noinfo"]][["setofmissings"]] <- c("truemat_vr", "truemat", "ytot",  "ymat_vr", "ytot_vr", "fn", "fp")
  dmrules[["noinfo"]][["setofnonmissings"]] <-  NULL
  dmrules[["noinfo"]][["varstouse"]] <- c()
  dmrules[["novr"]]  <- list()
  dmrules[["novr"]][["setofmissings"]] <- c( "ymat_vr", "ynonmat_vr", "ytot_vr", "ytot")
  dmrules[["novr"]][["setofnonmissings"]] <- c( "truemat_vr", "fp", "tp", "fn", "tp")
  dmrules[["novr"]][["varstouse"]] <- c()
  return(dmrules)
}
