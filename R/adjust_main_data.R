
#' adjust_main_data
#'
#' @param fit 
#' @param main_data 
#' @param meta 
#' @param jags_list 
#' @param estimates 
#' @param quantiles_for_adjustment 
#' @param global 
#'
#' @return data frame with adjustments applied to observed data
#'
#' @examples
adjust_main_data <- function(fit,
                             main_data,
                             meta,
                             jags_list,
                             estimates,
                             quantiles_for_adjustment = c(0.1,0.5,0.9),
                             global)
{
  z.q <- qnorm(quantiles_for_adjustment)
  datall <- main_data
  datall$pm.adj.postmod <-
    datall$multiplier <-
    datall$CI.up.postmod <-
    datall$CI.low.postmod <- rep(NA, nrow(datall))
  
  
  if (length(jags_list[["logpm.jnew"]]) > 0){
    datall <- adjust_nonvr_noninq(
      global = global,
      quantiles = quantiles_for_adjustment,
      datall = datall,
      meta = meta,
      jags_list = jags_list,
      fit = fit,
      z.q = z.q
    )
  }
  if (length(jags_list[["mat.j"]]) > 0){
    datall <- adjust_vr(
      datall = datall,
      meta = meta,
      jags_list = jags_list,
      estimates = estimates,
      fit = fit,
      z.q = z.q
    )
  }
  if (length(jags_list[["mat.jinq"]]) > 0){
    datall <- adjust_inq(
      datall = datall,
      meta = meta,
      jags_list = jags_list,
      fit = fit,
      z.q = z.q
    )
  }
  # use final pm where adjusted is
  datall$pm.adj.postmod[is.na(datall$pm.adj.postmod)] <-
    datall$CI.up.postmod[is.na(datall$pm.adj.postmod)] <-
    datall$CI.low.postmod[is.na(datall$pm.adj.postmod)] <-
    datall$final_pm[is.na(datall$pm.adj.postmod)]
  
  
  # add mmr bounds
  datall$mmr.CI.up.postmod <-
    datall$CI.up.postmod * datall$final_mmr / datall$final_pm
  datall$mmr.CI.low.postmod <-
    datall$CI.low.postmod * datall$final_mmr / datall$final_pm
  datall$mmr.adj.postmod <-
    datall$pm.adj.postmod * datall$final_mmr / datall$final_pm
  # deal with zeroes
  select <- which(datall$final_pm == 0)
  datall$mmr.CI.up.postmod[select] <-
    datall$mmr.CI.low.postmod[select] <-
    datall$mmr.adj.postmod[select] <- 0
  

  return(datall)
}
