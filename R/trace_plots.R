trace_plots <- function(fit,
                        main_path,
                        hyperpars_to_plot,
                        global_run
) {
  if(global_run) {
    summ <- MCMCvis::MCMCsummary(fit, params = hyperpars_to_plot) %>% 
      tibble::rownames_to_column(var = "pars")
    
    summ <- summ %>%
      dplyr::mutate(pars2 = pars %>% stringr::str_remove_all("[:digit:]") %>% stringr::str_remove("[,]") %>% stringr::str_remove("\\[]")) 
    
    temp <- summ %>%
      dplyr::filter(pars2 %in% hyperpars_to_plot)
    write.csv(temp, row.names = FALSE, here::here(file.path(main_path, "summary.csv")))
    
    MCMCvis::MCMCtrace(fit,
                       params = temp$pars,
                       ISB = FALSE,
                       pdf = TRUE,
                       post_zm = TRUE,
                       open_pdf = FALSE,
                       Rhat = TRUE,
                       filename = "mcmc_trace.pdf",
                       wd = main_path
    )
 
  }
} # end function