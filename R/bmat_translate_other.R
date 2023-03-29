translate_other <- function(
  main_data,
  meta,
  indices_by_data_model,
  imputeSElogPM
) {
  isjnew.d <- indices_by_data_model$isjnew.d
  if (sum(isjnew.d) == 0){
    datother <- NULL
  } else {
    dataother <- main_data[isjnew.d, ]
    J <- dim(dataother)[1]
    getc.j = Getc.i(iso.i = paste(dataother$iso_alpha_3_code), iso.c = meta$iso.c)
    gett.j = Gett.i(years.i = floor(dataother$year_mid), year.t = meta$year.t)
    start.j <- dataother$year_start
    end.j <- dataother$year_end
    gettstart.j <- Gett.i(years.i = floor(start.j), year.t = meta$year.t)
    gettend.j <- Gett.i(years.i = ceiling(end.j-1), year.t = meta$year.t)
    X.j <- gettend.j - gettstart.j +1
    partialtime.xj <- partialwhoenv.xj <- matrix(NA, max(X.j),J)
    for (j in 1:J){
      partialtime.xj[1:X.j[j],j] <- GetPartialTime(start = start.j[j], end = end.j[j], X = X.j[j])
      partialwhoenv.xj[1:X.j[j],j] <- partialtime.xj[1:X.j[j],j]*meta$deaths.ct[getc.j[j], gettstart.j[j]:gettend.j[j]]
    }
    aids.jnew <- rep(NA, J)
    for (j in 1:J){
      aids.jnew[j] <- meta$v.ct[getc.j[j],gett.j[j]]*meta$a.ct[getc.j[j],gett.j[j]]
    }
    selogpm.jnew <- dataother$obs_selogpm
    selogpm.jnew[is.na(selogpm.jnew)] <- imputeSElogPM
    # data types are misc, with subtype dhs
    ismisc.jnew  <- dataother$type!="inq"
    isinq.jnew <- dataother$type=="inq"
    isdhs.jnew <- dataother$type=="dhs"
    getjnew.m <- which(ismisc.jnew)
    M <- length(getjnew.m)
    datother <-  list(
      ismisc.jnew = ismisc.jnew,
      isinq.jnew = isinq.jnew,
      isdhs.jnew = isdhs.jnew,
      tausamp.jnew  = 1/selogpm.jnew^2,
      logpm.jnew = log(ifelse(dataother$final_pm > 0, dataother$final_pm, 0.0001)),
      isssa.jnew = meta$isssa.c[getc.j],
      Jnew = J,
      getc.jnew = getc.j,
      gett.jnew = gett.j,
      ispreg.jnew = ifelse(dataother$definition=="pregn",1,0),
      gettstart.jnew = gettstart.j,
      gettend.jnew = gettend.j,
      X.jnew = X.j,
      partialtime.xjnew  = partialtime.xj,
      partialwhoenv.xjnew  = partialwhoenv.xj,
      aidsva.jnew = aids.jnew)
  }#end else
  return(datother)

} #end function
