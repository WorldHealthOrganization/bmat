



##########################################################################################################
##########################       Settings     ############################################################
##########################################################################################################
# The round_name variable is used to reference the directory in which the processed data 
# from script 1 was saved. We will also save model results in this directory. The server 
# variable should stay set to FALSE unless you are running this code on a server.
devtools::load_all()
round_name <- "estimates_08-14-22_rev_frozen_vr"
round_name_update <- "estimates_11-24-22_bmis_projection_patch_checkall"
round_first_year <- 1985
round_last_year <- 2020
run_length <- "long_local"
server <- FALSE
# vector of isos being updated
# iso_alpha_3_codes <-c("COL","FIN","ITA","KAZ", "MDA", "NZL","PRY","SLV","ARG")
# logical: TRUE if you wish to re do bmis onecoutnry runs for all possible specified countries. Default is FALSE
# Generally you only want to re run bmis onecountry if needed (i.e. new study within crvs coverage) so keep this set of FALSE
# TRUE is only to be used for developer purposes
overwrite_isos_bmis <- TRUE
##########################################################################################################
ssdata <- read.csv(here::here("output", round_name, "ssdata.csv"))
dbmis <-  ssdata %>% 
  dplyr::filter(include_bmis) %>%
  dplyr::select(iso_alpha_3_code, year_start, year_end)
iso_alpha_3_codes <- dbmis %>% dplyr::pull(iso_alpha_3_code)



##########################################################################################################
##########################       Setup      #############################################################
##########################################################################################################
dir.create(here::here("output", round_name_update), recursive = TRUE)
# copy over one country runs
file.copy(from = here::here("output", round_name, "bmis_onecountry"),
          to = here::here("output", round_name_update),
          recursive = TRUE)
file.copy(from = here::here("output", round_name, "bmat_onecountry"),
          to = here::here("output", round_name_update),
          recursive = TRUE)
# next grab the processed data files which are in the form of .XXX in main dir
list_of_files1 <- list.files(here::here("output", round_name), ".csv$")
list_of_files2 <- list.files(here::here("output", round_name), ".xlsx$") 
list_of_files3 <- list.files(here::here("output", round_name), ".rds$") 
list_of_files <-c(list_of_files1,list_of_files2,list_of_files3)
file.copy(from = file.path(here::here("output", round_name), list_of_files),
          to = here::here("output", round_name_update),
          recursive = TRUE)
# for checking out work to see if there are new country years after the update
main_data_preupdate = read.csv(
  here::here("output", round_name_update, "main_data.csv")) 
write.csv(main_data_preupdate, row.names = FALSE,
          here::here("output", round_name_update, "main_data_preupdate.csv")) 
##########################################################################################################





##########################################################################################################
##########################       Read in data     ########################################################
##########################################################################################################
# Read in data and store them in objects. These objects will be provided to 
# wrapper functions which run the models.
ssdata <- read.csv(here::here("output", round_name_update, "ssdata.csv"))
vrdata <- read.csv(here::here("output", round_name_update, "vrdata.csv"))
vrdata_w_frozen <- read.csv(here::here("output", round_name_update, "vrdata_w_frozen.csv"))
meta <-  readRDS(here::here("output", round_name_update, "meta.rds"))
meta_precrisis <-  readRDS(here::here("output", round_name_update, "meta_precrisis.rds"))
census <- read.csv(here::here("output", round_name_update, "census.csv"))
survey <- read.csv(here::here("output", round_name_update, "survey.csv"))
miscellaneous <- read.csv(here::here("output", round_name_update, "miscellaneous.csv"))
##########################################################################################################



##########################################################################################################
############     Find country-years in update which require new BMis onecountry runs     #################
##########################################################################################################
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
##########################################################################################################
main_data = read.csv(
  here::here("output", round_name_update, "main_data.csv")) %>% 
  dplyr::filter(iso_alpha_3_code %in% meta$iso.c) %>%
  dplyr::filter(type == "inq")
main_dataold = read.csv(
  here::here("output", round_name_update, "main_data_preupdate.csv")) %>% 
  dplyr::filter(iso_alpha_3_code %in% meta$iso.c)  %>%
  dplyr::filter(type == "inq")
d1 <- main_data %>% dplyr::select(iso_alpha_3_code, year_start, year_end)
d2 <- main_dataold %>% dplyr::select(iso_alpha_3_code, year_start, year_end)
d3 <- dplyr::anti_join(d1, d2)
##########################################################################################################



##########################################################################################################
dbmis <-  ssdata %>% 
  dplyr::filter(include_bmis) %>%
  dplyr::select(iso_alpha_3_code, year_start, year_end)
isos_bmis <- dplyr::inner_join(d3,dbmis) %>% dplyr::pull(iso_alpha_3_code)
if (overwrite_isos_bmis) {
  isos_bmis_w_ssdata <- dbmis %>% dplyr::pull(iso_alpha_3_code)
  isos_bmis <- iso_alpha_3_codes[iso_alpha_3_codes %in% isos_bmis_w_ssdata] 
}
##########################################################################################################
##########################       Fit BMis One country     ################################################
##########################################################################################################
# Fit BMis one country. Fitting this model will produce estimates of sensitivity specificity.
# This model is only run (or re-run) for countries with studies 
# indicated as "include_bmis".  As such isos are filtered by this feature.
for (iso_alpha_3_code in isos_bmis) {
  hack_bmis_sensspec(
    global_run = FALSE,
    estimates_fixed_from_global = readRDS(
      here::here("output", round_name, "bmis_global", "estimates.rds")),
    sens_spec_global = readRDS(
      here::here("output", round_name, "bmis_global", "sens_spec_global.rds")),
    ssdata = ssdata %>% dplyr::filter(iso_alpha_3_code == !!iso_alpha_3_code),
    vrdata = vrdata_w_frozen %>% dplyr::filter(iso_alpha_3_code == !!iso_alpha_3_code),
    round_name = round_name,
    round_name_update = round_name_update,
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
  round_name = round_name_update,
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
    here::here("output", round_name, "bmis_global", "sens_spec_global.rds"))
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
  here::here("output", round_name_update, "main_data.csv")) %>% 
  dplyr::filter(iso_alpha_3_code %in% meta$iso.c)
# For each country run the model
for (iso_alpha_3_code in iso_alpha_3_codes) { #iso_alpha_3_codes
  fit_bmat(
    round_name = round_name_update,
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
dl <- list()
for(iso in meta$iso.c) {
  main_path <- make_output_directory_return_path(round_name_update, iso, FALSE, bmis_or_bmat = "bmat")
  dl[[iso]] <- readRDS(here::here(main_path, "estimates.rds"))
}
# Bind each data object to a single object
newestimates <- dplyr::bind_rows(dl) %>%
  dplyr::mutate(estimate_version = round_name_update)
# Save the resulting data frame which contains all country estimates.
write.csv(newestimates, here::here("output", round_name_update, "estimates.csv"), row.names = FALSE)
##########################################################################################################
dl <- list()
for(iso in meta$iso.c) {
  main_path <- make_output_directory_return_path(round_name_update, iso, FALSE, bmis_or_bmat = "bmat")
  dl[[iso]] <- readRDS(here::here(main_path, "estimates_arr.rds")) 
}
# Bind each data object to a single object
newestimatesarr <- dplyr::bind_rows(dl) %>%
  dplyr::mutate(estimate_version = round_name_update)
# Save the resulting data frame which contains all country estimates.
write.csv(newestimatesarr, here::here("output", round_name_update, "estimates_arr.csv"), row.names = FALSE)













##########################################################################################################
##########################       Settings / Setup   ######################################################
##########################################################################################################
# The years and table years need to be edited each round.
first_reporting_year <- 2000
years <- paste0(first_reporting_year, "-", round_last_year)
table1_years <- seq(first_reporting_year, round_last_year, 5)  
devtools::load_all()
library(kableExtra)
library(float)
library(ggnewscale)
langs <- c("English", "Spanish", "French")


# Text data which has been translated is required to create the profiles.
text <- readxl::read_excel(here::here("output", round_name_update, "countryprofile_translation.xlsx"))
# Meta data
meta <-  readRDS(
  here::here("output", round_name_update, "meta.rds")
)
iso_alpha_3_codes <- "NIC"
# Territory data
territory_info <- read.csv( 
  here::here("output", round_name_update, "country_ref.csv"))
whoregions <- territory_info$grp_who_region
if (!dir.exists(here::here("output", round_name_update, "country_profile"))) dir.create(here::here("output", round_name_update, "country_profile"))
for (lang in langs) {
  if (!dir.exists(here::here("output", round_name_update, "country_profile", lang))) dir.create(here::here("output", round_name_update, "country_profile", lang))
  for (whoregion in whoregions) {
    if (!dir.exists(here::here("output", round_name_update, "country_profile", lang, whoregion))) dir.create(here::here("output", round_name_update, "country_profile", lang, whoregion))
  }
}
##########################################################################################################
# part of the hack to fix strings which the server / server excel read turned into factors and numbers
main_data <- read.csv(
  here::here("output", round_name_update, "main_data.csv")) 

main_data_hack <- read.csv(
  here::here("output", round_name_update, "main_data.csv")) %>%
  dplyr::select(year_start, year_end, iso_alpha_3_code, type, include_reason, citation_short, final_pm)






##########################################################################################################
##########################       Create profiles     #####################################################
##########################################################################################################
# Estimate run time for the printing of all profiles is a few hours. Plan accordingly.


for(lang in langs) {
  text_one_lang <- text %>%
    dplyr::select("Item name", value = lang) %>% 
    tidyr::pivot_wider(values_from = value, names_from = "Item name")
  
  text_one_lang$`table 7 column names 4 denomination` <- "(\\%)"
  for (iso_alpha_3_code in iso_alpha_3_codes) {
    ter <- territory_info %>% 
      dplyr::filter(iso_alpha_3_code == !!iso_alpha_3_code)
    
    ter <- ter %>%
      dplyr::rename(
        English = name_short_en,
        French = name_short_fr,
        Spanish = name_short_es) 
    
    
    who_name <- ter %>%
      dplyr::pull(paste0(lang))
    
    main_path <- make_output_directory_return_path(round_name_update, global_run = FALSE, iso = iso_alpha_3_code, bmis_or_bmat = "bmat")
    df <- readRDS(here::here(main_path, 'main_data_adjusted.rds'))  %>%
      dplyr::mutate(period = paste0("[",round(year_start, 2),", ", round(year_end, 2), ")")) %>%
      dplyr::relocate(period) %>%
      dplyr::filter(year_mid >= 1985) %>%
      dplyr::mutate(citation_short = gsub("&", "and", citation_short, perl=TRUE)) %>%
      # HACK INCLUDE REASON
      dplyr::select(-include_reason) %>%
      dplyr::mutate(type = as.character(type))%>%
      dplyr::mutate(env_mat_final = ifelse(type == "inq", obs_matdeaths,
                                           ifelse(!is.na(obs_matdeaths), obs_matdeaths, env_mat))) %>%
      dplyr::mutate(obs_env = ifelse(type == "inq", final_env, obs_env))
    df <- df %>%
      dplyr::left_join(main_data_hack, by = c("iso_alpha_3_code", "year_start", "year_end", "type", "citation_short", "final_pm"))
    
    meta <- readRDS(here::here(main_path, 'meta.rds'))
    estimates <- readRDS(here::here(main_path, 'estimates.rds'))
    estimates_arr <- readRDS(here::here(main_path, "estimates_arr.rds")) 
    
    df_excluded <- main_data %>%
      dplyr::filter(iso_alpha_3_code == !!iso_alpha_3_code) %>%
      dplyr::filter(!include) %>%
      dplyr::mutate(period = paste0("[",round(year_start, 2),", ", round(year_end, 2), ")")) %>%
      dplyr::relocate(period) %>%
      dplyr::filter(year_mid >= 1985) %>%
      dplyr::mutate(citation_short = gsub("&", "and", citation_short, perl=TRUE)) %>%
      dplyr::mutate(type = as.character(type))

    
    detach("package:kableExtra", unload=TRUE)
    rmarkdown::render(here::here("vignettes","country_profile.Rmd"), 
                      output_file = here::here("output", round_name_update, "country_profile", lang, paste0(iso_alpha_3_code, ".pdf")),
                      envir = environment(),
                      params = list(iso_alpha_3_code = iso_alpha_3_code,
                                    who_name = who_name,
                                    round_name = round_name_update,
                                    years = years,
                                    df = df,
                                    df_excluded = df_excluded,
                                    meta = meta,
                                    estimates = estimates,
                                    estimates_arr = estimates_arr,
                                    table1_years = table1_years,
                                    text = text_one_lang
                      )
    )
  } #end country
} #end language
##########################################################################################################

for(lang in langs) {
  for(whoregion in whoregions) {
    ter_r <- territory_info %>% 
      dplyr::filter(iso_alpha_3_code %in% iso_alpha_3_codes) %>%
      dplyr::filter(grp_who_region == whoregion) 
    
    for (iso_alpha_3_code in unique(ter_r$iso_alpha_3_code)) {
      file.copy(from = here::here("output", round_name_update, "country_profile", lang, paste0(iso_alpha_3_code, ".pdf")),
                to = here::here("output", round_name_update, "country_profile", lang, whoregion, paste0(iso_alpha_3_code, ".pdf")), overwrite = TRUE)
    }}}


