
summarize_cov_bmat <- function(
  rho, # vector with rho_sesp for each year

  var_sens, var_spec){

  nyears =length(var_sens)

  cov_sesp <- rep(NA, nyears)

  for(t in 1:nyears){
    cov_sesp[t] <- rho[t] * sqrt(var_sens[t] * var_spec[t])
  }

  return(cov_sesp)
}



#' summarize_se_sp_tibble
#'
#' Takes in samples of se and sp for all years, summarizes into medians and vars for each, calculates correlation between se and sp by year, outputs a tibble
#' with combined median, var and correlation estimates for both se and sp
#'
#' @param se_samples.st
#' @param sp_samples.st
#' @param maxvar_se
#' @param maxvar_sp
#'
#' @return output: a tibble with median/var/corr and other measures reported for both se and sp
#' @export
#'
#' @examples
summarize_se_sp_tibble <- function(se_samples.st, sp_samples.st){

  output_se <- get_samples_bmat(samp = se_samples.st)
  output_sp <- get_samples_bmat(samp = sp_samples.st)


  rho_sesp.t <- rep(NA, length(se_samples.st[1,]))

  for(t in 1:length(se_samples.st[1,])){
    rho_sesp.t[t] <- cor(se_samples.st[,t], sp_samples.st[,t])
  }


  output <- tibble::as_tibble(merge(output_se, output_sp,  by = c("t"))) %>%
    dplyr::rename(c("sens" = "median.x", "lower_sens" = "lower.x", "upper_sens" = "upper.x", "var_sens" = "var.x", "sens_sq" = "samp_sq.x", "oneminsens_sq" = "oneminsamp_sq.x",
                    "spec" = "median.y", "lower_spec" = "lower.y", "upper_spec" = "upper.y","var_spec" = "var.y", "spec_sq" = "samp_sq.y", "oneminspec_sq" = "oneminsamp_sq.y"))
      

  output$rho_sesp <- rho_sesp.t
  output$cov_sesp <- summarize_cov_bmat(rho = rho_sesp.t,
                                             var_sens = output$var_sens,
                                             var_spec = output$var_spec)


  return(output)




}



get_samples_bmat <- function(samp # in current implementation, this is hardcoded to be dim s,t
                             
                             , maxvar = Inf){
  S = dim(samp)[1]
  nyears = dim(samp)[2]
  vars <- c("t", "median", "var",  "samp_sq", "oneminsamp_sq")
  madsq.t <- sample.s <- median_samp.t <- median_sample_sq.t <- one_minus_sample_sq.t <- lower.t <- upper.t <- rep(NA, nyears)
  
  for (t in 1:nyears){
    samples.s <- samp[, t]
    
    var_unscaled <- mad(samples.s)^2
    if (var_unscaled > maxvar){
      samples.s <- samples.s*sqrt(maxvar/var_unscaled)
    }
    madsq.t[t] <- min(mad(samples.s)^2, maxvar)
    median_samp.t[t] <- median(samples.s) #mean(sens.s) #
    median_sample_sq.t[t] <- median(samples.s^2)
    one_minus_sample_sq.t[t] <- median((1-samples.s)^2)
    lower.t[t] = quantile(samples.s, prob = c(0.1))
    upper.t[t] = quantile(samples.s, prob = c(0.9))
  }
  
  
  # median(samples.s) - 1.645 * (var(samples.s)/sqrt(S))
  # 
  
  #Put yearly data in tibble
  results <- data.frame(array(NA, c(nyears, length(vars))))
  colnames(results) <- vars
  results[["t"]] <- seq(1:nyears)
  results[["median"]] <- median_samp.t
  results[["lower"]] <- lower.t
  results[["upper"]] <- upper.t
  results[["var"]] <- madsq.t
  results[["samp_sq"]] <- median_sample_sq.t
  results[["oneminsamp_sq"]] <- one_minus_sample_sq.t
  
  output <- tibble::as_tibble(results)
  return(output)
}