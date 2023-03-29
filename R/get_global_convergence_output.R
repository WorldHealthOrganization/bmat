


#' get_global_convergence_output
#'
#' Takes in estimates for country c that have gone through constant extrapolation update. For years 5+ outside study period, estimates converge back to global se and sp estimates.
#' For years inside study period, these are kept as is, and for years within 4 years outside study period, linearly interpolated to global convergence estimates.
#' Output is the global convergence updates for country c estimates.
#'
#' @param sens_spec_global
#' @param indicators
#' @param constant_overwrite_output_c
#' @param tref
#'
#' @return globalconv_output_by_country
#' @export
#'
#' @examples
get_global_convergence_output <- function(sens_spec_global, indicators, constant_overwrite_output_c, tref, start_study_year){

  globalconv_output_by_country <- constant_overwrite_output_c

start_study_year <- floor(start_study_year)
#For startyear to tstart-5 we want global estimate of se and sp
if(start_study_year > 5){
  for(t in 1:(start_study_year-5)){
    for(n in 1:length(indicators)){
      globalconv_output_by_country[t,indicators[n]] <- sens_spec_global[tref, indicators[n]]
    }
  }
}

#For years between tstart-4 and tstart-1 linear interpolation
years_interp <- seq(start_study_year-4, start_study_year-1)

indices <- years_interp[years_interp > 0]

if (length(indices)>0){

  # to EP: we cannot interpolate the covariance directly so i added rho_sesp to the vector of variable names above
  # can you please update sens_spec_global and constant_overwrite_output_c to add rho_sesp there,
  # and then obtain cov from it?
  # (SO NEED TO EDIT SET-UP ABOVE SO THAT RHO IS SAVED)
  for (indicator in indicators){
    # see below for quick test of approx
    # indices could exclude earlier years but ok as long as x and y combi are for entire period
    res <- approx(x = c(start_study_year-5, start_study_year),
                  y = c(sens_spec_global[tref, indicator], constant_overwrite_output_c[start_study_year, indicator]),
                  xout = indices)
    globalconv_output_by_country[indices, indicator] <- res$y
  }

  rho_sesp <-  constant_overwrite_output_c[indices, "rho_sesp"]
  globalconv_output_by_country[indices, "rho_sesp"] <- rho_sesp

  globalconv_output_by_country[indices, "cov.t"] <-
    summarize_cov_bmat(rho = rho_sesp,
                       var_sens = constant_overwrite_output_c[indices, "var_sens"],
                       var_spec = constant_overwrite_output_c[indices, "var_spec"])
}

return(globalconv_output_by_country)

}
