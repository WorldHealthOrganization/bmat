add_validation_indicator_to_data <- function(main_data,
                             validation_settings) {
  if (validation_settings$validate){
    if(validation_settings$by_time==TRUE){
      main_data$validate_exclude<-main_data$year >= validation_settings$cutoff_year
    } else {
      set.seed(validation_settings$seed)
      main_data$validate_exclude<-rbinom(n=nrow(main_data),1,prob=0.20)
    }
  } else {
    main_data$validate_exclude <- rep(FALSE, dim(main_data)[1])
  }
  return(main_data)
}


add_validation_index_to_jags_list <-function(
  jags_list,
  main_data,
  validation
) {

  dataother <- main_data[jags_list$isjnew.d, ]
  datavr <- main_data[jags_list$isj.d,]
  datainq_incomplete <- main_data[jags_list$isjinq_incomplete.d,]
  datainq <- main_data[jags_list$isjinq.d,]
  
  if (validation){
    getj.g = which(datavr$validate_exclude==TRUE)
    getjinq.ginq = which(datainq$validate_exclude==TRUE)
    getjnew.gnew = which(dataother$validate_exclude==TRUE)
  } else {
    getj.g = 1:jags_list$J
    getjinq.ginq = 1:jags_list$Jinq
    getjinq_incomplete.ginq = 1:jags_list$Jinq_incomplete
    getjnew.gnew = 1:jags_list$Jnew
  }
  
  datvalidation <- list(
    Gtrain = jags_list$J-sum(datavr$validate_exclude),
    Ginqtrain = jags_list$Jinq-sum(datainq$validate_exclude),
    #to change for validation
    Ginqtrain_incomplete = jags_list$Jinq_incomplete - sum(datainq_incomplete$validate_exclude),
    Gnewtrain = jags_list$Jnew-sum(dataother$validate_exclude),
    
    getj.gtrain = which(datavr$validate_exclude==FALSE),
    getjinq.ginqtrain = which(datainq$validate_exclude==FALSE),
    getjinq_incomplete.ginqtrain = which(datainq_incomplete$validate_exclude==FALSE),
    getjnew.gnewtrain = which(dataother$validate_exclude==FALSE),
    
    G= ifelse(validation, sum(datavr$validate_exclude), jags_list$J),
    Ginq= ifelse(validation, sum(datainq$validate_exclude), jags_list$Jinq),
    Ginq_incomplete = ifelse(validation, sum(datainq_incomplete$validate_exclude), jags_list$Jinq_incomplete),
    Gnew=ifelse(validation, sum(dataother$validate_exclude),jags_list$Jnew),
    
    getj.g = getj.g,
    getjinq.ginq = getjinq.ginq,
    getjinq_incomplete.ginq = getjinq_incomplete.ginq,
    getjnew.gnew = getjnew.gnew
  )
  return(c(jags_list, datvalidation))
}

