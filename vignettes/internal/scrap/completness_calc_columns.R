

##########################################################################################################
##########################       Summary      ############################################################
##########################################################################################################
# This script updates estimates with one country runs
##########################################################################################################



##########################################################################################################
##########################       Settings     ############################################################
##########################################################################################################
# The round_name variable is used to reference the directory in which the processed data 
# from script 1 was saved. We will also save model results in this directory. The server 
# variable should stay set to FALSE unless you are running this code on a server.
devtools::load_all()
round_name <- "estimates_08-14-22_rev_frozen_vr"
round_first_year <- 1985
round_last_year <- 2020
run_length <- "long"
server <- TRUE
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

temp <- vrdata %>% 
  dplyr::select(iso_alpha_3_code, 
                year_start, 
                year_end, 
                allcause_1549_vr = obs_env, 
                allcause_1549_wpp = env_total_who_estimated, 
                completeness = rho_bmat, 
                usability = usability_percentage) %>%
  dplyr::mutate(`ratio_vr/wpp` = allcause_1549_vr/allcause_1549_wpp)

temp2 <- temp %>%
  dplyr::filter(iso_alpha_3_code %in% c("ARE", "IRQ"))          
write.csv(temp, "crvs_completeness_calculations.csv")
write.csv(temp2, "crvs_completeness_calculations_are_irq.csv")
