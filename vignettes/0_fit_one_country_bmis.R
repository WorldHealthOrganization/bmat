


##########################################################################################################
##########################       Summary      ############################################################
##########################################################################################################
# This script produces one country BMat estimates
##########################################################################################################



##########################################################################################################
##########################       Expected user      ######################################################
##########################################################################################################
# The expected user for this document is a person with Masters-level training in biostatistics 
# and/or quantitative demography with at least 2 years professional experience post-Master’s 
# using R statistical software). 
##########################################################################################################



##########################################################################################################
##########################       Pre reads      ############################################################
##########################################################################################################
# For more information about BMis methods please see (Peterson et al. 2022).
##########################################################################################################



##########################################################################################################
##########################       Settings     ############################################################
##########################################################################################################
# Please set the `round_name` variable is the name of the sub-directory which will be used to store your output
# Please set the `selected_country_isos` variable which specifies the country of estimation.
# The "test" run length is simply to test that the code will run on your machine.
# The minimum run length for plausible results is "quick". Official estimates use a minimum run length of "long".
devtools::load_all()
round_name <- "test_onecountry"
round_name_of_global_reference <- "global_ref"
round_first_year <- 1985
round_last_year <- 2023
selected_country_iso <- "AUT"
run_length <- "test"
server <- FALSE
##########################################################################################################



##########################################################################################################
##########################       Preparation     #########################################################
##########################################################################################################
# Move fixed pre-processed data from the default directory into the specific output folder which was named
# in the previous step `round_name`.
dir.create(here::here("output"))
dir.create(here::here("output", round_name))
flist <- list.files(here::here("default", round_name_of_global_reference, "processed_data_fixed_from_global"), all.files = TRUE, full.names = TRUE)
file.copy(from = flist,
          to = here::here("output", round_name), overwrite = TRUE, recursive = FALSE, 
          copy.mode = TRUE)
##########################################################################################################



##########################################################################################################
##########################       Fit BMis One country     ################################################
##########################################################################################################
# First read in the two main data inputs 
# 1. estimates_fixed_from_global_bmis
# 2. sens_spec_global
# 3. ssdata
# 4. vrdata_w_frozen
# The first file `estimates_fixed_from_global_bmis` contains fixed global parameters from the latest global bmis run. 
# The second file `sens_spec_global` contains fixed global sens and spec from the latest global bmis run. one country values converge to these when there is no data.
# The third file 'ssdata'  has specialized study data which is used as the gold standard
# The fourth file 'vrdata_w_frozen'  Has the crvs data. Historical values are frozen to preserve past discrepancies since the goal of bmis is to measure the historical discrepancies. 
estimates_fixed_from_global_bmis = readRDS(
  here::here("default", round_name_of_global_reference, "bmis_global", "estimates.rds"))
sens_spec_global = readRDS(
  here::here("default", round_name_of_global_reference, "bmis_global", "sens_spec_global.rds"))
ssdata <- read.csv(here::here("output", round_name, "ssdata.csv")) %>% 
  subset(year_mid <=2020|year_mid>2023)
vrdata_w_frozen <- read.csv(here::here("output", round_name, "vrdata_w_frozen.csv"))
# Fit the model
fit_bmis(
  global_run = FALSE,
  estimates_fixed_from_global = estimates_fixed_from_global_bmis,
  sens_spec_global = sens_spec_global,
  ssdata = ssdata %>% dplyr::filter(iso_alpha_3_code == selected_country_iso),
  vrdata = vrdata_w_frozen %>% dplyr::filter(iso_alpha_3_code == selected_country_iso),
  round_name = round_name,
  jags_settings = jags_settings_bmis(run_length),
  first_year = round_first_year,
  last_year = round_last_year,
  iso_alpha_3_code = selected_country_iso,
  server = server
)
##########################################################################################################



##########################################################################################################
##########################      Read/View/ estimates     ############################################
##########################################################################################################
# The following code reads in the estimates, preview them, and then saves a more accessible .csv file in your 
# output `roun_name` directory. The save directory is printed in the console
estimates <- readRDS(here::here("output", round_name, "bmis_onecountry", selected_country_iso, "estimates.rds"))
head(estimates$parameter_estimates_ct)
head(estimates$parameter_estimates_k)
head(estimates$parameter_estimates_global)
##########################################################################################################



##########################################################################################################
##########################      Plot estimates     ######################################################
##########################################################################################################
# The following function will plot the estimates from your one country run
estimates <- readRDS(here::here("output", round_name, "bmis_onecountry", selected_country_iso, "estimates.rds"))
main_data_for_plots <-
  readRDS(here::here("output", round_name, "bmis_onecountry", selected_country_iso, "main_data_for_plots.rds"))
sens_spec <-
  readRDS(here::here("output", round_name, "bmis_onecountry", "sens_spec_countries_w_data.rds")) %>%
  dplyr::filter(iso_alpha_3_code == selected_country_iso)
country_ref <- read.csv(
  here::here("output", round_name, "country_ref.csv")
)
plot <- plot_bmis_one_country(
  country_ref = country_ref,
  estimates = estimates,
  estimates_old = NULL,
  sens_spec = sens_spec,
  main_data_for_plots = main_data_for_plots,
  iso_alpha_3_code = selected_country_iso
) 
plot
##########################################################################################################






