translate_specialized_incomplete <- function(
  main_data,
  meta,
  indices_by_data_model
) {
  isjinq_incomplete.d <- indices_by_data_model$isjinq_incomplete.d
  if (sum(isjinq_incomplete.d) == 0){
    datinq_incomplete <- NULL
  } else {
    datainq_incomplete <- main_data[isjinq_incomplete.d,]
    J <- dim(datainq_incomplete)[1]
    getc.j = Getc.i(iso.i = paste(datainq_incomplete$iso_alpha_3_code), iso.c = meta$iso.c)
    gett.j = Gett.i(years.i = floor(datainq_incomplete$year_mid), year.t = meta$year.t)
    # get the reference period
    start.j <- datainq_incomplete$year_start
    end.j <- datainq_incomplete$year_end
    gettstart.j <- Gett.i(years.i = floor(start.j), year.t = meta$year.t)
    gettend.j <- Gett.i(years.i = ceiling(end.j-1), year.t = meta$year.t)
    X.j <- gettend.j - gettstart.j +1
    partialtime.xj <- partialwhoenv.xj <- matrix(NA, max(X.j),J)
    for (j in 1:J){
      partialtime.xj[1:X.j[j],j] <- GetPartialTime(start = start.j[j], end = end.j[j], X = X.j[j])
      partialwhoenv.xj[1:X.j[j],j] <- partialtime.xj[1:X.j[j],j]*meta$deaths.ct[getc.j[j], gettstart.j[j]:gettend.j[j]]
    }
    #completeness
    varfrombeta.j <- rep(NA, J)
    for (j in 1:J){
      varfrombeta.j[j] <- var(1/(datainq_incomplete$completeness_inq[j] +
                                   (1-datainq_incomplete$completeness_inq[j])*exp(meta$logbeta)))
    }
    datinq_incomplete <-  list(
      Jinq_incomplete = J,
      getc_incomplete.jinq = getc.j,
      gett_incomplete.jinq = gett.j,
      gettstart_incomplete.jinq = gettstart.j,
      gettend_incomplete.jinq = gettend.j,
      env_incomplete.jinq = ceiling(datainq_incomplete$final_env),
      mat_incomplete.jinq  = floor(datainq_incomplete$final_pm*datainq_incomplete$final_env),
      X_incomplete.jinq = X.j,
      partialtime_incomplete.xjinq  = partialtime.xj,
      partialwhoenv_incomplete.xjinq  = partialwhoenv.xj,
      varfrombeta.jinq = varfrombeta.j  #completeness
    )
  }#end else
  return(datinq_incomplete)

} #end function
