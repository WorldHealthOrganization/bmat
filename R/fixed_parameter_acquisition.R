fixed_parameter_acquisition <- function(
                                        fit,
                                        meta) {


  
  # from global model parameter estiamtes
  pars_singular <- c(
    "nonsamplingdhs.se",
    "nonsamplingnondhs.se",
    "theta",
    "phi",
    "sqrtgamma0",
    "sigma.lambda",
    "sigma.country"
  )
  pars_h <- "beta.h"
  pars_r <- "alpha.r"

  percentile <- c(0.5)
  estimates <-
    fit$BUGSoutput %>% tidybayes::spread_draws(
      nonsamplingdhs.se,
      nonsamplingnondhs.se,
      theta,
      phi,
      sqrtgamma0,
      sigma.lambda,
      sigma.country,
      alpha.r[r],
      beta.h[h]
    ) %>%
    dplyr::group_by(h, r) %>%
    dplyr::summarise_all(list( ~ quantile(.x, percentile))) %>%
    dplyr::mutate(percentile = percentile)

  fixed_parameter_list <- estimates %>%
    dplyr::ungroup() %>%
    dplyr::filter(h == 1, r == 1) %>%
    dplyr::select(pars_singular) %>%
    as.list() %>%
    lapply(as.numeric)

  fixed_parameter_list <- estimates %>%
    dplyr::ungroup() %>%
    dplyr::filter(r == 1) %>%
    dplyr::select(all_of(pars_h)) %>%
    as.list() %>%
    lapply(as.numeric) %>%
    append(fixed_parameter_list)

  fixed_parameter_list <- estimates %>%
    dplyr::ungroup() %>%
    dplyr::filter(h == 1) %>%
    dplyr::select(all_of(pars_r)) %>%
    as.list() %>%
    lapply(as.numeric) %>%
    append(fixed_parameter_list)

  
  
  draws1 <- fit$BUGSoutput %>% tidybayes::spread_draws(alpha.c[c]) # warning: requires R2jags or associated package to be loaded to understand this is an mcmc object
  isos <- meta$iso.c
  iso_df <- data.frame(c = 1:length(isos), iso_alpha_3_code = isos)
  posterior_sd_of_alpha.c <- draws1 %>%
    dplyr::left_join(iso_df) %>%
    dplyr::group_by(iso_alpha_3_code) %>%
    dplyr::summarise(posterior_sd_of_alpha.c = sd(alpha.c)) %>%
    dplyr::pull(posterior_sd_of_alpha.c)
  alpha.c <- draws1 %>%
    dplyr::left_join(iso_df) %>%
    dplyr::group_by(iso_alpha_3_code) %>%
    dplyr::summarise(posterior_mean_of_alpha.c = quantile(alpha.c, .5)) %>%
    dplyr::pull(posterior_mean_of_alpha.c)
  
  draws3 <- fit$BUGSoutput %>% tidybayes::spread_draws(beta.h[h]) # warning: requires R2jags or associated package to be loaded to understand this is an mcmc object
  posterior_sd_of_beta.h <- draws3 %>% 
    dplyr::group_by(h) %>%
    dplyr::summarise(posterior_sd_of_beta.h = sd(beta.h)) %>%
    dplyr::pull(posterior_sd_of_beta.h)
  
  
  
  fixed_parameter_list <- c(fixed_parameter_list, list(posterior_sd_of_alpha.c = posterior_sd_of_alpha.c,
                                                       posterior_sd_of_beta.h = posterior_sd_of_beta.h,
                                                       alpha.c = alpha.c))
  return(fixed_parameter_list)
}
