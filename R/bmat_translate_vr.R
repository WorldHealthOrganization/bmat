#' translate_vr
#'
#' @param main_data 
#' @param meta 
#' @param indices_by_data_model 
#'
#' @return List of jags data from crvs data 
#'
translate_vr <- function(
  main_data,
  meta,
  indices_by_data_model
) {
  isj.d <- indices_by_data_model$isj.d
  if (sum(isj.d)==0){
    datvr <- NULL
  } else {
    datavr <- main_data[isj.d,]
    getj.k1 <- seq(1, dim(datavr)[1])
    K1 <- length(getj.k1)

    varfrombeta <- rep(NA, dim(datavr)[1])
    for (j in 1:dim(datavr)[1]){
      varfrombeta[j] <- var(1/(datavr$rho_bmat[j] + (1-datavr$rho_bmat[j])*exp(meta$logbeta)))
    }
    getc.j <- Getc.i(iso.i = paste(datavr$iso_alpha_3_code), iso.c = meta$iso.c)
    gett.j <- Gett.i(years.i = floor(datavr$year_mid), year.t = meta$year.t)
    datvr <- list(
      J=dim(datavr)[1],
      getc.j = getc.j,
      gett.j = gett.j,
      pmobs.j = ifelse(datavr$final_pm > 0, datavr$final_pm, 0.0001),
      logpmobs.j = log(ifelse(datavr$final_pm > 0, datavr$final_pm, 0.0001)),
      mat.j  = round(datavr$final_pm*datavr$final_env),
      env.j  = ceiling(datavr$final_env),
      var_sens.j = datavr$var_sens,
      var_spec.j = datavr$var_spec,
      cov_sesp.j = datavr$cov_sesp,
      sens.j = datavr$sens,
      spec.j = datavr$spec,
      sens_sq.j = datavr$sens_sq,
      oneminspec_sq.j = datavr$oneminspec_sq,
      rhovr.j = datavr$rho_bmat,
      input0forRstar.ct = matrix(0, meta$C, meta$nyears),
      input0forVR.ct = matrix(0, meta$C, meta$nyears),
      varfrombeta.j = varfrombeta,
      getj.k1 = getj.k1,
      K1 = K1
    )
  }#end else
  return(datvr)

} #end function
