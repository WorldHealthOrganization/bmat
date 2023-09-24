
bind_bmat_onecoutnry_output_produce_spreadsheets_and_plots <- function(round_name, round_name_2, meta, country_ref, iso_alpha_3_codes) {
  # Produce maternal estimate spreadsheets
  dl <- list()
  for(iso in iso_alpha_3_codes) {
    main_path <- make_output_directory_return_path(round_name, iso, FALSE, bmis_or_bmat = "bmat")
    dl[[iso]] <- readRDS(here::here(main_path, "estimates.rds"))
  }
  # Bind each data object to a single object
  newestimates <- dplyr::bind_rows(dl) %>%
    dplyr::mutate(estimate_version = round_name)
  # Save the resulting data frame which contains all country estimates.
  write.csv(newestimates, here::here("output", round_name, "estimates.csv"), row.names = FALSE)
  newestimatesmmr <- newestimates %>% 
    dplyr::filter(parameter == "mmr") %>% 
    dplyr::filter(year_mid >= 2000) %>%
    dplyr::select(iso_alpha_3_code, year = year_mid, mmr = `0.5`) 
  write.csv(newestimatesmmr, here::here("output", round_name, "estimates_only_mmr_2000+.csv"), row.names = FALSE)
  
  # Produce ARR estimate spreadsheets
  dl <- list()
  for(iso in iso_alpha_3_codes) {
    main_path <- make_output_directory_return_path(round_name, iso, FALSE, bmis_or_bmat = "bmat")
    dl[[iso]] <- readRDS(here::here(main_path, "estimates_arr.rds")) 
  }
  # Bind each data object to a single object
  newestimatesarr <- dplyr::bind_rows(dl) %>%
    dplyr::mutate(estimate_version = round_name)
  # Save the resulting data frame which contains all country estimates.
  write.csv(newestimatesarr, here::here("output", round_name, "estimates_arr.csv"), row.names = FALSE)
  
  # Produce adjusted data spreadsheets
  dl <- list()
  for (iso in iso_alpha_3_codes) {
    main_path <- make_output_directory_return_path(round_name, global_run = FALSE, iso = iso, bmis_or_bmat = "bmat")
    dl[[iso]]<- readRDS(here::here(main_path, 'main_data_adjusted.rds'))
  }
  # Bind each data object to a single object
  main_data_adjusted <- dplyr::bind_rows(dl) 
  write.csv(main_data_adjusted, here::here("output", round_name, "main_data_adjusted.csv"), row.names = FALSE)
  
  # Produce plots of estimates and inputs
  plot_bmat_w_inputs(round_name, round_name_2, iso_alpha_3_codes)
  
}
