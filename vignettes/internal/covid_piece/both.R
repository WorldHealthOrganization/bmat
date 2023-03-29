



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
round_name <- "estimates_08-14-22_factual_no_covid_in_crisis_def"
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
meta <-  readRDS(here::here("output", round_name, "meta.rds"))
meta_precrisis <-  readRDS(here::here("output", round_name, "meta_precrisis.rds"))
census <- read.csv(here::here("output", round_name, "census.csv"))
survey <- read.csv(here::here("output", round_name, "survey.csv"))
miscellaneous <- read.csv(here::here("output", round_name, "miscellaneous.csv"))
##########################################################################################################



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
    here::here("output", "estimates_08-14-22", "bmis_onecountry", "sens_spec_countries_w_data.rds")),
  sens_spec_global = readRDS(
    here::here("output", "estimates_08-14-22", "bmis_global", "sens_spec_global.rds"))
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
iso_alpha_3_codes <- iso_alpha_3_codes[which(iso_alpha_3_codes == "MUS"):length(iso_alpha_3_codes)]
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
    arr_periods= list(c(2000, round_last_year), c(2010, round_last_year))
  )
}
# For each country read in data and add the data object to a single list
iso_alpha_3_codes <- meta$iso.c
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


country_ref <- read.csv( 
  here::here("output", round_name, "country_ref.csv"))
##########################################################################################################
##########################       Aggregate: Example 1     ################################################
##########################################################################################################
# Start by created a data frame which represents groups of aggregation. For example, a data frame
# with a column of iso codes and a column with who regions. The data frame must always contain 
# the iso column `iso_alpha_3_code`.
group_data <- country_ref %>%
  dplyr::select(iso_alpha_3_code, group = grp_who_region)
calculate_bmat_aggregate(round_name = round_name,
                         aggregate_group_data = group_data,
                         iso_alpha_3_codes = meta$iso.c,
                         type_of_group = "who_region")
##########################################################################################################



##########################################################################################################
##########################       Aggregate: Example 2     ################################################
##########################################################################################################
# To obtain aggregate estimates at the global level i.e. the entire worlds MMR in YYYY is equal to XXX
# the group column has only a single value. In this case we give it the value "world" to convey what
# we are attempting to aggregate
group_data <- data.frame(iso_alpha_3_code = meta$iso.c, group = "world")
calculate_bmat_aggregate(round_name = round_name,
                         aggregate_group_data = group_data,
                         iso_alpha_3_codes = meta$iso.c,
                         type_of_group = "world")
##########################################################################################################



##########################################################################################################
##########################       Aggregate: Example 2     ################################################
##########################################################################################################
# Start by created a data frame which represents groups of aggregation. For example, a data frame
# with a column of iso codes and a column with SDG1 regions. The data frame must always contain 
# the iso column `iso_alpha_3_code`.
group_data <- country_ref %>%
  dplyr::select(iso_alpha_3_code, group = sdg1)
calculate_bmat_aggregate(round_name = round_name,
                         aggregate_group_data = group_data,
                         iso_alpha_3_codes = meta$iso.c,
                         type_of_group = "sdg1")
##########################################################################################################








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
round_name <- "estimates_08-14-22_counterfactual"
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
meta <-  readRDS(here::here("output", round_name, "meta.rds"))
meta_precrisis <-  readRDS(here::here("output", round_name, "meta_precrisis.rds"))
census <- read.csv(here::here("output", round_name, "census.csv"))
survey <- read.csv(here::here("output", round_name, "survey.csv"))
miscellaneous <- read.csv(here::here("output", round_name, "miscellaneous.csv"))
##########################################################################################################



##########################################################################################################
##########################       Process data for BMat     ###############################################
##########################################################################################################
# Process the individual data objects and the sensitivity specificity output from BMis into one file to 
# be used by BMat. 
# debug(process_bmat_main_data)
# process_bmat_main_data(
#   ssdata = ssdata,
#   vrdata = vrdata,
#   census = census,
#   survey = survey,
#   miscellaneous = miscellaneous,
#   meta = meta,
#   meta_precrisis = meta_precrisis,
#   round_first_year = round_first_year,
#   round_last_year = round_last_year,
#   sens_spec_countries_w_data = readRDS(
#     here::here("output", "estimates_08-14-22", "bmis_onecountry", "sens_spec_countries_w_data.rds")),
#   sens_spec_global = readRDS(
#     here::here("output", "estimates_08-14-22", "bmis_global", "sens_spec_global.rds"))
# )
##########################################################################################################


# Observations to exclude for counterfactual
# 1 Any data source which includes the whole year 2020, i.e. year_start <= 2020 & year_end >= 2021.
# 2 Any data source which includes at least some portion of 2020 (but not necessarily the full year), i.e. year_start
# <= 2020 & year_end > 2020
# We want to exclude all 2020 so using definition 2. They are both equal currently.
main_data = read.csv(
  here::here("output", round_name, "main_data.csv")) %>% 
  dplyr::filter(iso_alpha_3_code %in% meta$iso.c) 
# 1
# temp <- main_data %>%
#   dplyr::filter(year_start == 2020) %>%
#   dplyr::filter(include)
# 2 
# temp <- main_data %>%
#   dplyr::filter(year_start <= 2020 & year_end > 2020) 
main_data <- main_data %>%
  dplyr::filter(!(year_start <= 2020 & year_end > 2020))


##########################################################################################################
##########################       Fit BMat Global      #####################################################
##########################################################################################################
# Fit BMat global. Fitting this model will produce estimates for maternal mortality indicators. 
# fit_bmat(
#   global_run = TRUE,
#   round_name = round_name,
#   jags_settings = jags_settings_bmat(run_length),
#   main_data = main_data, 
#   meta = meta,
#   arr_periods= list(c(2000, round_last_year), c(2010, round_last_year))
# )
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
iso_alpha_3_codes <- iso_alpha_3_codes[which(iso_alpha_3_codes == "DEU"):length(iso_alpha_3_codes)]
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
    arr_periods= list(c(2000, round_last_year), c(2010, round_last_year))
  )
}

iso_alpha_3_codes <- meta$iso.c
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



##########################################################################################################
##########################       Aggregate: Example 1     ################################################
##########################################################################################################
# Start by created a data frame which represents groups of aggregation. For example, a data frame
# with a column of iso codes and a column with who regions. The data frame must always contain 
# the iso column `iso_alpha_3_code`.
group_data <- country_ref %>%
  dplyr::select(iso_alpha_3_code, group = grp_who_region)
calculate_bmat_aggregate(round_name = round_name,
                         aggregate_group_data = group_data,
                         iso_alpha_3_codes = meta$iso.c,
                         type_of_group = "who_region")
##########################################################################################################



##########################################################################################################
##########################       Aggregate: Example 2     ################################################
##########################################################################################################
# To obtain aggregate estimates at the global level i.e. the entire worlds MMR in YYYY is equal to XXX
# the group column has only a single value. In this case we give it the value "world" to convey what
# we are attempting to aggregate
group_data <- data.frame(iso_alpha_3_code = meta$iso.c, group = "world")
calculate_bmat_aggregate(round_name = round_name,
                         aggregate_group_data = group_data,
                         iso_alpha_3_codes = meta$iso.c,
                         type_of_group = "world")
##########################################################################################################



##########################################################################################################
##########################       Aggregate: Example 2     ################################################
##########################################################################################################
# Start by created a data frame which represents groups of aggregation. For example, a data frame
# with a column of iso codes and a column with SDG1 regions. The data frame must always contain 
# the iso column `iso_alpha_3_code`.
group_data <- country_ref %>%
  dplyr::select(iso_alpha_3_code, group = sdg1)
calculate_bmat_aggregate(round_name = round_name,
                         aggregate_group_data = group_data,
                         iso_alpha_3_codes = meta$iso.c,
                         type_of_group = "sdg1")
##########################################################################################################
