process_hyper <- function(jags_list, fit) {
  
  hyper_list <- jags_list %>%
    .[c("referenceyear",
        "sqrtgamma0max",
        "max.sigma.lambda",
        "imputeSElogPM"     
    )]
  
  # To retrieve from a fit object
  pars_singular <- c(
    "nonsamplingdhs.se",
    "nonsamplingnondhs.se",
    "theta",
    "phi",
    "sqrtgamma0",
    "sigma.lambda",
    "sigma.country")
  pars_h <- "beta.h"
  pars_r <- "alpha.r"
  
  
  percentile <- c(0.5)
  fit2 <- fit$BUGSoutput %>% tidybayes::spread_draws(nonsamplingdhs.se,
                                                     nonsamplingnondhs.se,
                                                     theta,
                                                     phi,
                                                     sqrtgamma0,
                                                     sigma.lambda,
                                                     sigma.country,
                                                     alpha.r[r],
                                                     beta.h[h]) %>%
    dplyr::group_by(h, r) %>%
    dplyr::summarise_all(list(~ quantile(.x, percentile))) %>%
    dplyr::mutate(percentile = percentile)
  
  hyper_list <- fit2 %>% 
    dplyr::ungroup() %>%
    dplyr::filter(h == 1, r ==1) %>%
    dplyr::select(pars_singular) %>% 
    haven::zap_labels() %>%
    as.list() %>%
    append(hyper_list)
  
  hyper_list <- fit2 %>% 
    dplyr::ungroup() %>%
    dplyr::filter(r ==1) %>%
    dplyr::select(all_of(pars_h)) %>% 
    haven::zap_labels() %>%
    as.list() %>%
    append(hyper_list)
  
  hyper_list <- fit2 %>% 
    dplyr::ungroup() %>%
    dplyr::filter(h == 1) %>%
    dplyr::select(all_of(pars_r)) %>% 
    haven::zap_labels() %>%
    as.list() %>%
    append(hyper_list)
  
  usethis::use_data(hyper_list)
}