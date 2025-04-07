


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
round_name_of_global_reference <- 'estimates_11-12-2025'
round_first_year <- 1985
round_last_year <- 2023
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
  here::here("output", round_name_of_global_reference, "bmat_global", "estimates_fixed_from_global.rds"))
main_data <- read.csv(
  here::here("output", round_name, "main_data.csv"))
meta <- readRDS(
  here::here("output", round_name, "meta.rds"))
# Fit the model
meta <- readRDS(here::here("output", round_name, "meta.rds"))

main_data = read.csv(
  here::here("output", round_name, "main_data.csv")) 

for (iso_alpha_3_code in c("BRA")){
  
  print(iso_alpha_3_code)
  main_path <- make_output_directory_return_path(round_name, iso_alpha_3_code, global_run = FALSE, bmis_or_bmat = "bmat")
  
  selected_country_iso = iso_alpha_3_code
  fit_bmat2(
    round_name = round_name,
    iso_alpha_3_code = selected_country_iso,
    main_data = main_data %>% 
      dplyr::filter(iso_alpha_3_code == !!selected_country_iso
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






