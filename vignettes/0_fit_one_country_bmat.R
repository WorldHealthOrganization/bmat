


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
# For more information about BMat methods please see (Alkema et al. 2017).
##########################################################################################################



##########################################################################################################
##########################       Settings     ############################################################
##########################################################################################################
# Please set the `round_name` variable is the name of the sub-directory which will be used to store your output
# Please set the `selected_country_isos` variable which specifies the country of estimation.
# The "test" run length is simply to test that the code will run on your machine.
# The minimum run length for plausible results is "quick". Official estimates use a minimum run length of "long".
devtools::load_all()
round_name_of_global_reference <- "estimates_12-19-22"
round_name <- "test_onecountry"
round_first_year <- 1985
round_last_year <- 2020
selected_country_iso <- "BRA"
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
##########################       Fit BMat One country     ################################################
##########################################################################################################
# First read in the two main data inputs 
# 1. estimates_fixed_from_global_bmat
# 2. main_data
# 3. meta
# The first file `estimates_fixed_from_global_bmat` contains fixed values from the latest global run. 
# The second file `main_data` contains country observed data and BMis adjustments for the selected country.
# The third file has descriptive data such as indices and auxiliary data data such denomination (e.g. all cause deaths)
#
estimates_fixed_from_global_bmat <- readRDS(
  here::here("default", round_name_of_global_reference, "bmat_global", "estimates_fixed_from_global.rds"))
main_data <- read.csv(
  here::here("output", round_name, "main_data.csv"))
meta <- readRDS(
  here::here("output", round_name, "meta.rds"))
# Fit the model
fit_bmat(
  round_name = round_name,
  iso_alpha_3_code = selected_country_iso,
  main_data = main_data %>% 
    dplyr::filter(iso_alpha_3_code == !!selected_country_iso),
  meta = meta,
  global_run = FALSE,
  estimates_fixed_from_global_bmat = estimates_fixed_from_global_bmat,
  jags_settings = jags_settings_bmat(run_length),
  run_on_server = server,
  arr_periods= list(c(2000,2015), c(2000, 2020), c(2010, 2020), c(2000, 2005), c(2005, 2010), c(2010, 2015), c(2015, 2020))
)
##########################################################################################################



##########################################################################################################
##########################      Read/View/Save estimates     ################################################
##########################################################################################################
# The following code reads in the estimates, preview them, and then saves a more accessible .csv file in your 
# output `roun_name` directory. The save directory is printed in the console
estiamtes <- readRDS(here::here("output", round_name, "bmat_onecountry", selected_country_iso, "estimates.rds"))
head(dplyr::as_tibble(estiamtes))
write.csv(estiamtes, row.names = FALSE, here::here("output", round_name, "estimates.csv"))
print(paste0("estimates saved to ", here::here("output", round_name, "estimates.csv")))
##########################################################################################################



##########################################################################################################
##########################      Plot estimates     ################################################
##########################################################################################################
# The following function will plot the estimates from your one country run
bmat:::plot_bmat_onecountry(
  round_name =  round_name,
  iso_alpha_3_code = selected_country_iso,
  year_range = c(2000, round_last_year)
)
##########################################################################################################






