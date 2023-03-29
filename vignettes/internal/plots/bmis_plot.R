

round_name <- "estimates_12-19-22"
iso_alpha_3_code <- "ITA"
round_name <- "estimates_08-14-22_rev_frozen_vr"
round_name_update <- "estimates_12-19-22"


estimates <- readRDS(here::here("output", round_name_update, "bmis_onecountry", iso_alpha_3_code, "estimates.rds"))
main_data_for_plots <-
  readRDS(here::here("output", round_name_update, "bmis_onecountry", iso_alpha_3_code, "main_data_for_plots.rds"))
sens_spec <-
  readRDS(here::here("output", round_name_update, "bmis_onecountry", "sens_spec_countries_w_data.rds")) %>%
  dplyr::filter(iso_alpha_3_code == !!iso_alpha_3_code)
plot <- bmat::plot_bmis_one_country(
  estimates = estimates,
  estimates_old = NULL,
  sens_spec = sens_spec,
  main_data_for_plots = main_data_for_plots,
  iso_alpha_3_code = iso_alpha_3_code
)  
plot

devtools::load_all()
debug(calculate_sens_spec_updated)
hack_bmis_sensspec(
  global_run = FALSE,
  estimates_fixed_from_global = readRDS(
    here::here("output", round_name, "bmis_global", "estimates.rds")),
  sens_spec_global = readRDS(
    here::here("output", round_name, "bmis_global", "sens_spec_global.rds")),
  ssdata = ssdata %>% dplyr::filter(iso_alpha_3_code == !!iso_alpha_3_code),
  vrdata = vrdata_w_frozen %>% dplyr::filter(iso_alpha_3_code == !!iso_alpha_3_code),
  round_name = round_name,
  round_name_update = round_name_update,
  jags_settings = jags_settings_bmis(run_length),
  first_year = round_first_year,
  last_year = round_last_year,
  iso_alpha_3_code = iso_alpha_3_code,
  server = FALSE
)
