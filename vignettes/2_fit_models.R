


##########################################################################################################
##########################       Summary      ############################################################
##########################################################################################################
# This script produces estimates and involves fitting models.
##########################################################################################################



##########################################################################################################
##########################       Settings     ############################################################
##########################################################################################################
# The round_name variable is used to reference the directory in which the processed data 
# from script 1 was saved. We will also save model results in this directory. The server 
# variable should stay set to FALSE unless you are running this code on a server.
devtools::load_all()
round_name <- "test"
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



##########################################################################################################
##########################       Fit BMis Global     #####################################################
##########################################################################################################
# Fit BMis global. Fitting this model will produce estimates of sensitivity specificity.
fit_bmis(
  global_run = TRUE,
  ssdata = ssdata,
  vrdata = vrdata_w_frozen,
  round_name = round_name,
  jags_settings = jags_settings_bmis(run_length),
  first_year = round_first_year,
  last_year = round_last_year, 
  server = server
)
##########################################################################################################



##########################################################################################################
##########################       Fit BMis One country     ################################################
##########################################################################################################
# Fit BMis one country. Fitting this model will produce estimates of sensitivity specificity.
# This model is only run (or re-run) for countries with studies 
# indicated as "include_bmis".  As such isos are filtered by this feature.
iso_alpha_3_codes <-  ssdata %>% 
  dplyr::filter(include_bmis) %>%
  dplyr::pull(iso_alpha_3_code) %>%
  unique()
for (iso_alpha_3_code in iso_alpha_3_codes) {
  fit_bmis(
    global_run = FALSE,
    estimates_fixed_from_global = readRDS(
      here::here("output", round_name, "bmis_global", "estimates.rds")),
    sens_spec_global = readRDS(
      here::here("output", round_name, "bmis_global", "sens_spec_global.rds")),
    ssdata = ssdata %>% dplyr::filter(iso_alpha_3_code == !!iso_alpha_3_code),
    vrdata = vrdata_w_frozen %>% dplyr::filter(iso_alpha_3_code == !!iso_alpha_3_code),
    round_name = round_name,
    jags_settings = jags_settings_bmis(run_length),
    first_year = round_first_year,
    last_year = round_last_year,
    iso_alpha_3_code = iso_alpha_3_code,
    server = server
  )
}
##########################################################################################################



##########################################################################################################
##########################       Process data for BMat     ###############################################
##########################################################################################################
# Process the individual data objects and the sensitivity specificity output from BMis into one file to 
# be used by BMat. 
# debug(process_bmat_main_data)
process_bmat_main_data(
  round_name = round_name,
  ssdata = ssdata,
  vrdata = vrdata,
  census = census,
  survey = survey,
  miscellaneous = miscellaneous,
  meta = meta,
  meta_precrisis = meta_precrisis,
  round_first_year = round_first_year,
  round_last_year = round_last_year,
  sens_spec_countries_w_data = readRDS(
    here::here("output", round_name, "bmis_onecountry", "sens_spec_countries_w_data.rds")),
  sens_spec_global = readRDS(
    here::here("output", round_name, "bmis_global", "sens_spec_global.rds"))
)
##########################################################################################################


##########################################################################################################
##########################       Fit BMat Global      #####################################################
##########################################################################################################
# Fit BMat global. Fitting this model will produce estimates for maternal mortality indicators. 
fit_bmat(
  global_run = TRUE,
  round_name = round_name,
  jags_settings = jags_settings_bmat(run_length),
  main_data = read.csv(
    here::here("output", round_name, "main_data.csv")) %>% 
    dplyr::filter(iso_alpha_3_code %in% meta$iso.c), 
  meta = meta,
  arr_periods= list(c(2000, round_last_year), c(2010, round_last_year))
)
##########################################################################################################



##########################################################################################################
##########################       Fit BMat One country     ################################################
##########################################################################################################
# Fit BMat one country models for all countries with pop > 100,000 (corresponds to iso.c i.e. all 
# countries in GHE lifetables). 
# First Read in the two main data inputs before the country loop. The fixed inputs for test runs (to be 
# used for initial sampling values) are taken from a default folder which comes from the latest
# actual run. Initial values generated by a test run of BMat global would be unrealistic and may
# cause input1.ct[1,1] errors.
if(run_length == "test") {
  estimates_fixed_from_global_bmat = readRDS(
    here::here("default", "estimates_fixed_from_global.rds"))
} else {
  estimates_fixed_from_global_bmat = readRDS(
    here::here("output", round_name, "bmat_global", "estimates_fixed_from_global.rds"))
}
main_data = read.csv(
  here::here("output", round_name, "main_data.csv")) %>% 
  dplyr::filter(iso_alpha_3_code %in% meta$iso.c)
# For each country run the model
iso_alpha_3_codes <- meta$iso.c
for (iso_alpha_3_code in iso_alpha_3_codes) { 
  fit_bmat(
    round_name = round_name,
    iso_alpha_3_code = iso_alpha_3_code,
    main_data = main_data %>% 
      dplyr::filter(iso_alpha_3_code == !!iso_alpha_3_code),
    meta = meta,
    global_run = FALSE,
    estimates_fixed_from_global_bmat = estimates_fixed_from_global_bmat,
    jags_settings = jags_settings_bmat(run_length),
    run_on_server = server,
    arr_periods= list(c(2000,2015), c(2000, 2020), c(2010, 2020), c(2000, 2005), c(2005, 2010), c(2010, 2015), c(2015, 2020))
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


