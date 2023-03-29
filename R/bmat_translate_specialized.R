translate_specialized <- function(
  main_data,
  meta,
  indices_by_data_model
)
  {
  isjinq.d <- indices_by_data_model$isjinq.d
  if (sum(isjinq.d) == 0){
    datinq <- NULL
  } else {
    datainq <- main_data[isjinq.d,]
    J <- dim(datainq)[1]
    getc.j = Getc.i(iso.i = paste(datainq$iso_alpha_3_code), iso.c = meta$iso.c)
    gett.j = Gett.i(years.i = floor(datainq$year_mid), year.t = meta$year.t)
    # get the reference period
    start.j <- datainq$year_start
    end.j <- datainq$year_end
    gettstart.j <- Gett.i(years.i = floor(start.j), year.t = meta$year.t)
    gettend.j <- Gett.i(years.i = ceiling(end.j-1), year.t = meta$year.t)
    X.j <- gettend.j - gettstart.j +1
    partialtime.xj <- partialwhoenv.xj <- matrix(NA, max(X.j),J)
    for (j in 1:J){
      partialtime.xj[1:X.j[j],j] <- GetPartialTime(start = start.j[j], end = end.j[j], X = X.j[j])
      partialwhoenv.xj[1:X.j[j],j] <- partialtime.xj[1:X.j[j],j]*meta$deaths.ct[getc.j[j], gettstart.j[j]:gettend.j[j]]
    } # end loop

    datinq <-  list(
      Jinq = J,
      getc.jinq = getc.j, gett.jinq = gett.j,
      gettstart.jinq = gettstart.j,
      gettend.jinq = gettend.j,
      env.jinq = ceiling(datainq$final_env), #this env comes from vrdata
      mat.jinq  = floor(datainq$final_pm*datainq$final_env), #this is back calculating obs mat without scaling
      X.jinq = X.j,
      partialtime.xjinq  = partialtime.xj,
      partialwhoenv.xjinq  = partialwhoenv.xj
    )
  } # end else
  return(datinq)
} # end function
