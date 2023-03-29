

##########################################################################################################
##########################       Settings     ############################################################
##########################################################################################################
# The round_name variable is used to reference the directory in which the processed data 
# from script 1 was saved. We will also save model results in this directory. The server 
# variable should stay set to FALSE unless you are running this code on a server.
devtools::load_all()
round_name <- "estimates_08-14-22_frozen_vr_rev"
round_first_year <- 1985
round_last_year <- 2020
run_length <- "long_local"
server <- FALSE
##########################################################################################################



##########################################################################################################
##########################       Read in data     ########################################################
##########################################################################################################
# Read in data and store them in objects. These objects will be provided to 
# wrapper functions which run the models.
ssdata <- read.csv(here::here("output", round_name, "ssdata.csv"))
vrdata <- read.csv(here::here("output", round_name, "vrdata.csv"))
meta <-  readRDS(here::here("output", round_name, "meta.rds"))
meta_precrisis <-  readRDS(here::here("output", round_name, "meta_precrisis.rds"))
census <- read.csv(here::here("output", round_name, "census.csv"))
survey <- read.csv(here::here("output", round_name, "survey.csv"))
miscellaneous <- read.csv(here::here("output", round_name, "miscellaneous.csv"))
##########################################################################################################


round_hack <- "estimates_08-14-22_rev_ecu"
##########################################################################################################
##########################       Process data for BMat     ###############################################
##########################################################################################################
# Process the individual data objects and the sensitivity specificity output from BMis into one file to 
# be used by BMat. 
# debug(process_bmat_main_data)
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
    here::here("output", round_hack, "bmis_onecountry", "sens_spec_countries_w_data.rds")),
  sens_spec_global = readRDS(
    here::here("output", round_hack, "bmis_global", "sens_spec_global.rds"))
)
##########################################################################################################


main_data = read.csv(
  here::here("output", round_name, "main_data.csv")) %>% 
  dplyr::filter(iso_alpha_3_code %in% meta$iso.c)

temp <- main_data %>% 
  dplyr::filter(type == "vr") %>%
  dplyr::filter(include)

# vrf <- vrdata %>%
#   dplyr::filter(updateinmat != 0 | updateinenv != 0)
# 
# temp2 <- temp %>%
#   dplyr::left_join(vrf, by = c("iso_alpha_3_code", "year_start", "year_end"))
temp3 <- temp %>%
  dplyr::filter(updateinmat != 0 | updateinenv != 0)
temp3$updateinenv
temp3$updateinmat



