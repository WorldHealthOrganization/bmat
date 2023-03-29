##########################################################################################################
##########################       Settings     ############################################################
##########################################################################################################
# The round_name variable is used to reference the directory in which the processed data 
# from script 1 was saved. We will also save model results in this directory. The server 
# variable should stay set to FALSE unless you are running this code on a server.
devtools::load_all()
round_name <- "estimates_12-19-22"
round_first_year <- 1985
round_last_year <- 2020
run_length <- "long"
server <- FALSE
##########################################################################################################

round_name_update <- round_name
##########################################################################################################
##########################       Read in data     ########################################################
##########################################################################################################
# Read in data and store them in objects. These objects will be provided to 
# wrapper functions which run the models.
# ssdata <- read.csv(here::here("output", round_name_update, "ssdata.csv"), fileEncoding="windows-1252")
# vrdata <- read.csv(here::here("output", round_name_update, "vrdata.csv"), fileEncoding="windows-1252")
# vrdata_w_frozen <- read.csv(here::here("output", round_name_update, "vrdata_w_frozen.csv"), fileEncoding="windows-1252")
# meta <-  readRDS(here::here("output", round_name_update, "meta.rds"))
# meta_precrisis <-  readRDS(here::here("output", round_name_update, "meta_precrisis.rds"))
# census <- read.csv(here::here("output", round_name_update, "census.csv"), fileEncoding="windows-1252")
# survey <- read.csv(here::here("output", round_name_update, "survey.csv"), fileEncoding="windows-1252")
# miscellaneous <- read.csv(here::here("output", round_name_update, "miscellaneous.csv"), fileEncoding="windows-1252")
# ##########################################################################################################

Sys.setlocale("LC_CTYPE", locale = paste0("English", ".UTF-8"))

ssdata <- read.csv(here::here("output", round_name_update, "ssdata.csv"))
vrdata <- read.csv(here::here("output", round_name_update, "vrdata.csv"))
vrdata_w_frozen <- read.csv(here::here("output", round_name_update, "vrdata_w_frozen.csv"))
meta <-  readRDS(here::here("output", round_name_update, "meta.rds"))
meta_precrisis <-  readRDS(here::here("output", round_name_update, "meta_precrisis.rds"))
census <- read.csv(here::here("output", round_name_update, "census.csv"))
survey <- read.csv(here::here("output", round_name_update, "survey.csv"))
miscellaneous <- read.csv(here::here("output", round_name_update, "miscellaneous.csv"))

round_name <- "estimates_08-14-22_rev_frozen_vr"
round_name_update <- "estimates_12-19-22"
#########################################################################################################
###########     Find country-years in update which require new BMis onecountry runs     #################
#########################################################################################################
process_bmat_main_data(
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
    here::here("output", round_name_update, "bmis_onecountry", "sens_spec_countries_w_data.rds")),
  sens_spec_global = readRDS(
    here::here("output", round_name, "bmis_global", "sens_spec_global.rds")),
  round_name = round_name_update
)
round_name <- round_name_update



round_name <- "estimates_08-14-22_rev_frozen_vr"
if(run_length == "test") {
  estimates_fixed_from_global_bmat = readRDS(
    here::here("default", "estimates_fixed_from_global.rds"))
} else {
  estimates_fixed_from_global_bmat = readRDS(
    here::here("output", round_name, "bmat_global", "estimates_fixed_from_global.rds"))
}
round_name <- round_name_update
main_data = read.csv(
  here::here("output", round_name, "main_data.csv")) %>% 
  dplyr::filter(iso_alpha_3_code %in% meta$iso.c)

# For each country run the model
iso_alpha_3_codes <- meta$iso.c
# error
# [1] "SLE"
for (iso_alpha_3_code in iso_alpha_3_codes) { 
  hack_main_data_adjusted(
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
