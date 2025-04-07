


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
round_name_of_global_reference <- "estimates_11-12-2025"
round_first_year <- 1985
round_last_year <- 2023
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
country_ref <- read.csv(here::here("output", round_name, "country_ref.csv"))
##########################################################################################################



##########################################################################################################
##########################       Fit BMis Global     #####################################################
##########################################################################################################
# Fit BMis global. Fitting this model will produce estimates of sensitivity specificity.
fit_bmis(
  global_run = TRUE,
  ssdata = ssdata %>% subset(year_mid <=2020|year_mid>2023),#run without data for COVID years (2020-2022)
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
  dplyr::filter(year_end <=2020|year_end>2024) %>% 
  dplyr::pull(iso_alpha_3_code) %>%
  unique()  #run without data for COVID years (2020-2022)
for (iso_alpha_3_code in iso_alpha_3_codes) {
  fit_bmis(
    global_run = FALSE,
    estimates_fixed_from_global = readRDS(
      here::here("output", round_name, "bmis_global", "estimates.rds")),
    sens_spec_global = readRDS(
      here::here("output", round_name, "sens_spec_global.rds")),
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
# Produces one country plots and the global adjustment plot
plot_bmis(round_name, round_last_year, iso_alpha_3_codes)
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

main_data <- read.csv(
  here::here("output", round_name, "main_data.csv"))
main_data$comp = main_data$final_env/main_data$env_total_who_estimated
main_data$include <- ifelse(main_data$type=="vr"&main_data$year_start%in%2020:2022&main_data$comp<0.95, FALSE, main_data$include)
main_data$include_reason <- ifelse(main_data$type=="vr"&main_data$year_start%in%2020:2022&main_data$comp<0.95, "CRVS completeness <0.95 in a COVID-19 year", main_data$include_reason)
main_data<- main_data %>%  dplyr::select(-comp) 
write.csv(main_data, row.names = FALSE, here::here("output", round_name, "main_data.csv"))

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
    dplyr::filter(iso_alpha_3_code %in% meta$iso.c)%>%
    dplyr::filter(year_end <=2020|year_end>2024), # fit without covid data
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
# The model is run sequentially, first a non-covid one country model is used to obtain samples and estimates for 
# non-covid years (before 2020 and after 2022), and a covid model is used to obtain samples for covid years
if(run_length == "test") {
  estimates_fixed_from_global_bmat = readRDS(
    here::here("default",  round_name_of_global_reference, "bmat_global", "estimates_fixed_from_global.rds"))
} else {
  estimates_fixed_from_global_bmat = readRDS(
    here::here("output", round_name, "bmat_global", "estimates_fixed_from_global.rds"))
}
main_data = read.csv(
  here::here("output", round_name, "main_data.csv")) %>% 
  dplyr::filter(iso_alpha_3_code %in% meta$iso.c)
# For each country run the model
iso_alpha_3_codes <- meta$iso.c
meta <- readRDS(here::here("output", round_name, "meta.rds"))

main_data = read.csv(
  here::here("output", round_name, "main_data.csv")) 



for (iso_alpha_3_code in iso_alpha_3_codes){
  
  print(iso_alpha_3_code)
  main_path <- make_output_directory_return_path(round_name, iso_alpha_3_code, global_run = FALSE, bmis_or_bmat = "bmat")
  
  selected_country_iso = iso_alpha_3_code
  fit_bmat2(
    round_name = round_name,
    iso_alpha_3_code = selected_country_iso,
    main_data = main_data %>% 
      dplyr::filter(iso_alpha_3_code == !!selected_country_iso#),
      ) %>% dplyr::filter(year_end<2021|year_end>2023),
    meta = meta,
    global_run = FALSE,
    estimates_fixed_from_global_bmat = estimates_fixed_from_global_bmat,
    jags_settings = jags_settings_bmat(run_length),
    run_on_server = server,
    arr_periods= list(c(2000, 2023),c(2000, 2015),c(2016, 2023)),
    covidmodel = FALSE
    
  )
  
  fit_bmat2(
    round_name = round_name,
    iso_alpha_3_code = selected_country_iso,
    main_data = main_data %>% 
      dplyr::filter(iso_alpha_3_code == !!selected_country_iso
                    ### Note: CANNOT BE RUN WITH VR DATA YET!!!
                    # filter out just vr after 2020
                    # ,!(year_start >= 2020 & type == "vr")
      ) ,
    meta = meta,
    global_run = FALSE,
    estimates_fixed_from_global_bmat = estimates_fixed_from_global_bmat,
    jags_settings = jags_settings_bmat(run_length),
    run_on_server = server,
    arr_periods= list(c(2000, 2023),c(2000, 2015),c(2016, 2023)),
    covidmodel = TRUE
    
  )
  
  covid_est <- readRDS(here::here(file.path(main_path, "estimatescovid.rds")))
  noncovid_est <- readRDS(here::here(file.path(main_path, "estimatesnoncovid.rds")))
  
  estimates_combined <- covid_est$estimates %>% subset(year_mid%in%2020:2022) %>% 
    rbind(noncovid_est$estimates %>% subset(!year_mid%in%2020:2022))
  
  main_data_adjusted <- readRDS(here::here(file.path(main_path, "main_data_adjustedcovid.rds"))) %>% 
    subset(year_end >2020) %>% rbind(readRDS(here::here(file.path(main_path, "main_data_adjustednoncovid.rds"))) %>% 
                                       subset(year_end <=2020)) 
  
  main_data_adjusted$comp = main_data_adjusted$final_env/main_data_adjusted$env_total_who_estimated
  
  main_data_adjusted$include <- ifelse(main_data_adjusted$type=="vr"&main_data_adjusted$year_start%in%2020:2022&main_data_adjusted$comp<0.95, FALSE, main_data_adjusted$include)
  
  main_data_adjusted$include_reason <- ifelse(main_data_adjusted$type=="vr"&main_data_adjusted$year_start%in%2020:2022&main_data_adjusted$comp<0.95, "CRVS completeness <0.95 in a COVID-19 year", main_data_adjusted$include_reason)
  
  
  main_data_adjusted<- main_data_adjusted %>%  dplyr::select(-comp) 
  
  
  saveRDS(estimates_combined, here::here(file.path(main_path, "estimates.rds")))
  saveRDS(main_data_adjusted, here::here(file.path(main_path, "main_data_adjusted.rds")))
  
}

# Produce all output (bind spreadsheets from one country runs, produce all plots etc)
# add the input round_name_2 if you want plots to compare estimates and input data from the current round of estimation to the additionally supplied round of estimation
bind_bmat_onecoutnry_output_produce_spreadsheets_and_plots(round_name, round_name_2 = NULL, meta, country_ref, iso_alpha_3_codes)
##########################################################################################################



##########################################################################################################
##########################       target setting      #####################################################
##########################################################################################################
newestimates <- read.csv(here::here("output", round_name, "estimates.csv"))
group_data_wpp <- readxl::read_excel(here::here("data-raw", "country_territory_data", "territory_groups_for_report", "WPP_territory.xlsx"), skip = 0, sheet = 2)
group_data_wpp <- group_data_wpp %>%
  dplyr::filter(LocTypeName == "Country/Area") %>%
  dplyr::select(iso_alpha_3_code = ISO3_Code,
                SDG_Region = SDGRegName,
                SDG_Sub_Region = SubRegName) 

# SDG_Region
group_data <- group_data_wpp %>%
  dplyr::select(iso_alpha_3_code, SDG_Region)
# SDG_Sub_Region
group_data_sub <- group_data_wpp %>%
  dplyr::select(iso_alpha_3_code, SDG_Sub_Region)
country_ref <- country_ref %>%
  dplyr::left_join(group_data) %>%
  dplyr::left_join(group_data_sub)
target_table(newestimates, country_ref)
##########################################################################################################


