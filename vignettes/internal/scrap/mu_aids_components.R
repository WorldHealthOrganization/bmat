devtools::load_all(
)


jags_list <- readRDS(here::here("output", "quickSWZ", "SWZ", "jags_list.rds"))
fit <- readRDS(here::here("output", "quickSWZ", "SWZ", "fit.rds"))
meta <- readRDS(here::here("output", "quickSWZ", "SWZ", "meta.rds"))

calculate_bmat_mu_by_aids <- function(
  fit,
  meta,
  jags_list,
  quantiles = c(0.1, 0.5, 0.9)
) {
  
  mu_draws <- fit$BUGSoutput %>% tidybayes::spread_draws(mu.ct[c,t], munonaids.ct[c,t], muaids.ct[c,t]) # warning: requires R2jags or associated package to be loaded to understand this is an mcmc object
  
  crisis_deaths <- matrix_ct_to_df(matrix = meta$crisisdeaths.ct, C = meta$C, value_name = "crisis_deaths")
  deaths <- matrix_ct_to_df(matrix = meta$deaths.ct, C = meta$C, value_name = "deaths")
  births <- matrix_ct_to_df(matrix = meta$births.ct, C = meta$C, value_name = "births")
  aids <-  matrix_ct_to_df(matrix = jags_list$muaids.ct, C = meta$C, value_name = "aids")
  gfr <-  matrix_ct_to_df(matrix = meta$gfr.ct, C = meta$C, value_name = "gfr")
  t15t50 <- matrix_ct_to_df(matrix = meta$t15t50.ct, C = meta$C, value_name = "t15t50")
  l15 <- matrix_ct_to_df(matrix = meta$l15.ct, C = meta$C, value_name = "l15")
  
  meta_df <- crisis_deaths %>% 
    dplyr::left_join(deaths) %>% 
    dplyr::left_join(births) %>% 
    dplyr::left_join(aids) %>%
    dplyr::left_join(gfr) %>%
    dplyr::left_join(t15t50) %>%
    dplyr::left_join(l15)
  
  isos <- meta$iso.c
  years <- meta$year.t
  iso_df <- data.frame(c = 1:length(isos), iso_alpha_3_code = isos)
  years_df <- data.frame(t = 1:length(years), year = years)
  
  # join columns with readable years and isos, join covariate data
  mu_draws <- mu_draws %>% 
    dplyr::left_join(meta_df) %>%
    dplyr::left_join(iso_df) %>% 
    dplyr::left_join(years_df)
  
  # adjust for crisis deaths
  draws_w_calculation <- mu_draws %>% 
    dplyr::group_by(iso_alpha_3_code, year) %>%
    dplyr::mutate(
      mu = mu.ct,
      mu_non_aids = munonaids.ct,
      mu_aids = muaids.ct
    ) %>%
    dplyr::group_by(iso_alpha_3_code, year,`.draw`)
  rm("mu_draws")
  # above here is all generic
  ###################################
  
  
  # quantiles summarise
  estimates <- draws_w_calculation %>%
    dplyr::ungroup(`.draw`) %>%
    dplyr::summarise(mu = quantile(mu, !!quantiles, na.rm = TRUE),
                     mu_aids = quantile(mu_aids, !!quantiles, na.rm = TRUE),
                     mu_non_aids = quantile(mu_non_aids, !!quantiles, na.rm = TRUE)
    ) %>%
    dplyr::mutate(quantiles = !!quantiles) %>%
    dplyr::rename(year_mid = year) %>%
    tidyr::pivot_longer(cols = c("mu",
                                 "mu_aids",
                                 "mu_non_aids"
                                 ), names_to = "parameter", values_to = "value") %>%
    tidyr::pivot_wider(names_from = quantiles, values_from = value)
  
  return(estimates)
} 

muestimates <- calculate_bmat_mu_by_aids(jags_list = jags_list,
                          meta = meta,
                          fit = fit)













breaks = seq(1985,
             2020,
             by = 5)
library(ggplot2)
muestimates %>%
  ggplot2::ggplot(ggplot2::aes(x = year_mid)) +
    ggplot2::ggtitle("SWZ") +
    ggplot2::scale_x_continuous(name = "year", breaks = breaks) +
    ggplot2::ylab("value") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)
    ) +
    ggplot2::geom_line(ggplot2::aes(y = `0.5`, color = parameter))+
    ggplot2::geom_ribbon(ggplot2::aes(
      ymin = `0.1`,
      ymax = `0.9`,
      fill = parameter
    ), alpha = .375) 


+
    ggplot2::scale_fill_manual(values = c(cbp2[4], cbp2[8]), na.translate = F) +
    ggplot2::scale_color_manual(values = c(cbp2[4], cbp2[8]), na.translate = F) + 
    ggnewscale::new_scale_color() +
    ggplot2::geom_point(
      ggplot2::aes_string(y = "value", shape = "source_type", color = "value_type"),
      alpha = .7,
      size = 3,
      stroke = .75
    ) +
    ggplot2::scale_shape_discrete(na.translate = F) +
    ggplot2::geom_errorbar(
      ggplot2::aes_string(
        y = "value",
        ymax = "value_up",
        ymin = "value_low", color = "value_type"
      ),
      width = 0,
      alpha = 1
    ) + 
    ggplot2::scale_color_manual(values = c(cbp2[2], cbp2[3]), na.translate = F) + 
    ggplot2::facet_wrap(ggplot2::vars(parameter), scale = "free")
