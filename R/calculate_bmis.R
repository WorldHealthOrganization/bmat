calculate_bmis <- function(jags_fit, percentiles, jags_list, global_run) {
  suppressMessages({
    print("Calculating results from posterior samples.")
    indices_binary_measure <- indices_binary_measure()
    list2env(indices_binary_measure, envir = environment())
    # processing of parameters with C * T * B dimensions
    parameter_estimates_ct1 <-
      jags_fit[["BUGSoutput"]] %>%
      tidybayes::spread_draws(rho.ctb[c, t, b]) %>% # warning: requires R2jags or associated package to be loaded to understand this is an mcmc object
      dplyr::group_by(c, t, .draw) %>%
      dplyr::summarise(
        # gamma_fn = gamma.ctb[b == fn_index],
        # gamma_fp = gamma.ctb[b == fp_index],
        # gamma_tn = gamma.ctb[b == tn_index],
        # gamma_tp = gamma.ctb[b == tp_index],
        rho_fn = rho.ctb[b == fn_index],
        rho_fp = rho.ctb[b == fp_index],
        rho_tn = rho.ctb[b == tn_index],
        rho_tp = rho.ctb[b == tp_index],
        rho_up = rho.ctb[b == up_index],
        rho_un = rho.ctb[b == un_index],
        rho_truemat = calc_rho_truemat(rho.ctb, b),
        rho_truemat_vr = calc_rho_truemat_vr(rho.ctb, b),
        rho_tot_vr = calc_rho_tot_vr(rho.ctb, b),
        # rho_nonvr = calc_rho_nonvr(rho.ctb, b),
        gamma_fn = rho_fn/rho_tot_vr,
        gamma_fp = rho_fp/rho_tot_vr,
        gamma_tn = rho_tn/rho_tot_vr,
        gamma_tp = rho_tp/rho_tot_vr,
        gamma_mat_vr = calc_gamma_mat_vr(rho.ctb, b),
        gamma_truemat_vr = calc_gamma_truemat_vr(rho.ctb, b),
        vradj = gamma_truemat_vr / gamma_mat_vr,
        fn_out_truemat = calc_fn_out_truemat(rho.ctb, b),
        fn_out_truemat_vr = calc_fn_out_truemat_vr(rho.ctb, b),
        fp_out_mat_vr = calc_fp_out_mat_vr(rho.ctb, b)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(c, t) %>%
      dplyr::summarise_all(.funs = ~quantile(.x, percentiles, na.rm = TRUE)) %>%
      dplyr::mutate(percentiles = percentiles) %>%
      dplyr::relocate(c, t, percentiles) %>%
      dplyr::select(-`.draw`)
    
    # processing of parameters with C * T dimensions
    parameter_estimates_ct2 <- jags_fit[["BUGSoutput"]] %>%
      tidybayes::spread_draws(beta.ct[c, t],
                              sens.ct[c, t],
                              spec.ct[c, t],
                              vradj.ct[c,t]) %>%
      dplyr::rename(beta = beta.ct,
                    sens = sens.ct,
                    spec = spec.ct,
                    vradj = vradj.ct) %>%
      dplyr::group_by(c, t) %>%
      dplyr::summarise_all(.funs = ~quantile(.x, percentiles, na.rm = TRUE)) %>%
      dplyr::mutate(percentiles = percentiles) %>%
      dplyr::relocate(c, t, percentiles) %>%
      dplyr::select(-.draw,-.iteration,-.chain)
    
    
    # join parameter country time specific estimates
    parameter_estimates_ct <-
      dplyr::left_join(parameter_estimates_ct1, parameter_estimates_ct2, by = c("c", "t", "percentiles")) %>%
      tidyr::gather(key = "parameter", value = "value", -c, -t, -percentiles) %>%
      tidyr::spread(key = "percentiles", value = "value")
    
    # add dates and iso_alpha_3_code in place of model index
    iso_alpha_3_code_c <- jags_list$iso_alpha_3_code.c
    iso_alpha_3_code_joon <- data.frame(iso_alpha_3_code = iso_alpha_3_code_c, c = 1:jags_list$C)
    first_year <- jags_list$startyear
    parameter_estimates_ct <- parameter_estimates_ct %>%
      dplyr::mutate(year_reference = t + first_year - .5) %>%
      dplyr::left_join(iso_alpha_3_code_joon, by = "c") %>%
      dplyr::relocate(iso_alpha_3_code, year_reference)
    
    
    # processing of parameters with K dimension
    parameter_estimates_k <- jags_fit[["BUGSoutput"]] %>%
      tidybayes::spread_draws(etaworld.k[k]) %>%
      dplyr::group_by(k) %>%
      dplyr::summarise_all(.funs = ~quantile(.x, percentiles, na.rm = TRUE)) %>%
      dplyr::mutate(percentiles = percentiles) %>%
      dplyr::relocate(k, percentiles) %>%
      dplyr::select(-.draw,-.iteration,-.chain) %>%
      tidyr::spread(key = "percentiles", value = "etaworld.k") %>%
      dplyr::mutate(parameter = "etaworld.k")
    
    # processing of parameters without dimension
    if(global_run) {
      parameter_estimates_global <-
        jags_fit[["BUGSoutput"]] %>% tidybayes::spread_draws(
          rho.alphabeta,
          sigma.alpha,
          sigma.beta,
          #rrho.alphabeta,
          #rsigma.alpha,
          #rsigma.beta,
          rho_world,
          sigmaworld1,
          sigmaworld2,
          sensworld,
          specworld
        ) %>%
        dplyr::summarise_all(.funs = ~quantile(.x, percentiles, na.rm = TRUE)) %>%
        dplyr::mutate(percentiles = percentiles) %>%
        dplyr::relocate(percentiles) %>%
        dplyr::select(-.draw,-.iteration,-.chain) %>%
        tidyr::gather(key = "parameter", value = "value", -percentiles) %>%
        tidyr::spread(key = "percentiles", value = "value")
    } else {
      parameter_estimates_global <-
        jags_fit[["BUGSoutput"]] %>% tidybayes::spread_draws(
          rho.alphabeta,
          sigma.alpha,
          sigma.beta
         # rrho.alphabeta,
        #  rsigma.alpha,
        #  rsigma.beta
        ) %>%
        dplyr::summarise_all(.funs = ~quantile(.x, percentiles, na.rm = TRUE)) %>%
        dplyr::mutate(percentiles = percentiles) %>%
        dplyr::relocate(percentiles) %>%
        dplyr::select(-.draw,-.iteration,-.chain) %>%
        tidyr::gather(key = "parameter", value = "value", -percentiles) %>%
        tidyr::spread(key = "percentiles", value = "value")
    }
    
    parameter_estimates <- list(
      parameter_estimates_ct = parameter_estimates_ct,
      parameter_estimates_k = parameter_estimates_k,
      parameter_estimates_global = parameter_estimates_global
    )
  return(parameter_estimates)
  })
}
