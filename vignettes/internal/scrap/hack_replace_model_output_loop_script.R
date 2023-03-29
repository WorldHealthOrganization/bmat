##########################################################################################################
##########################       Settings     ############################################################
##########################################################################################################
# The round_name variable is used to reference the directory in which the processed data 
# from script 1 was saved. We will also save model results in this directory. The server 
# variable should stay set to FALSE unless you are running this code on a server.
devtools::load_all()
round_name_gloabl_ref <- "estimates_08-14-22_rev_frozen_vr"
round_name <- "estimates_12-19-22"
round_first_year <- 1985
round_last_year <- 2020
run_length <- "test"
server <- FALSE
##########################################################################################################



##########################################################################################################
##########################       Read in data     ########################################################
##########################################################################################################
# Read in data and store them in objects. These objects will be provided to 
# wrapper functions which run the models.
ssdata <- read.csv(here::here("output", round_name, "ssdata.csv"))
vrdata <- read.csv(here::here("output", round_name, "vrdata.csv"))
vrdata_w_frozen <- read.csv(here::here("output", round_name, "vrdata_w_frozen.csv"))
meta <-  readRDS(here::here("output", round_name, "meta.rds"))
meta_precrisis <-  readRDS(here::here("output", round_name, "meta_precrisis.rds"))
census <- read.csv(here::here("output", round_name, "census.csv"))
survey <- read.csv(here::here("output", round_name, "survey.csv"))
miscellaneous <- read.csv(here::here("output", round_name, "miscellaneous.csv"))
##########################################################################################################



if(run_length == "test") {
  estimates_fixed_from_global_bmat = readRDS(
    here::here("default", "estimates_fixed_from_global.rds"))
} else {
  estimates_fixed_from_global_bmat = readRDS(
    here::here("output", round_name_gloabl_ref, "bmat_global", "estimates_fixed_from_global.rds"))
}
main_data = read.csv(
  here::here("output", round_name, "main_data.csv")) %>% 
  dplyr::filter(iso_alpha_3_code %in% meta$iso.c)
# For each country run the model
iso_alpha_3_codes <- meta$iso.c
for (iso_alpha_3_code in iso_alpha_3_codes) { 
  calc_arrs_hack(
    round_name = round_name,
    iso_alpha_3_code = iso_alpha_3_code,
    main_data = main_data %>% 
      dplyr::filter(iso_alpha_3_code == !!iso_alpha_3_code),
    meta = meta,
    global_run = FALSE,
    estimates_fixed_from_global_bmat = estimates_fixed_from_global_bmat,
    jags_settings = jags_settings_bmat(run_length),
    run_on_server = server,
    arr_periods= list(c(2000,2015), c(2000, 2020), c(2010, 2020), c(2000, 2005), c(2005, 2010), c(2010, 2015), c(2015, 2020), c(2016, 2020))
  )
}
# For each country read in data and add the data object to a single list
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
##########################################################################################################
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

