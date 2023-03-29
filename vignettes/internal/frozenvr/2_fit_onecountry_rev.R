

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
iso_alpha_3_codes <- c("CHL", "COL", "DOM", "ECU", "ISR", "JAM", "MYS", "GEO", "PRY", "SVN", "BRA")
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
##########################       Fit BMis One country     ################################################
##########################################################################################################
# Fit BMis one country. Fitting this model will produce estimates of sensitivity specificity.
# This model is only run (or re-run) for countries with studies 
# indicated as "include_bmis".  As such isos are filtered by this feature.
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
dl <- list()
for(iso in meta$iso.c) {
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
text <- readxl::read_excel(here::here("output", round_name, "countryprofile_translation.xlsx"))
# Meta data
meta <-  readRDS(
  here::here("output", round_name, "meta.rds")
)
# Territory data
territory_info <- read.csv( 
  here::here("output", round_name, "country_ref.csv"))
whoregions <- territory_info$grp_who_region
if (!dir.exists(here::here("output", round_name, "country_profile"))) dir.create(here::here("output", round_name, "country_profile"))
for (lang in langs) {
  if (!dir.exists(here::here("output", round_name, "country_profile", lang))) dir.create(here::here("output", round_name, "country_profile", lang))
  for (whoregion in whoregions) {
    if (!dir.exists(here::here("output", round_name, "country_profile", lang, whoregion))) dir.create(here::here("output", round_name, "country_profile", lang, whoregion))
  }
}
##########################################################################################################

# part of the hack to fix strings which the server / server excel read turned into factors and numbers
main_data <- read.csv(
  here::here("output", round_name, "main_data.csv")) %>%
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
    
    main_path <- make_output_directory_return_path(round_name, global_run = FALSE, iso = iso_alpha_3_code, bmis_or_bmat = "bmat")
    df <- readRDS(here::here(main_path, 'main_data_adjusted.rds'))  %>%
      dplyr::mutate(period = paste0("[",round(year_start, 2),", ", round(year_end, 2), ")")) %>%
      dplyr::relocate(period) %>%
      dplyr::filter(year_mid >= 1985) %>%
      dplyr::mutate(citation_short = gsub("&", "and", citation_short, perl=TRUE)) %>%
      # HACK INCLUDE REASON
      dplyr::select(-include_reason) %>%
      dplyr::mutate(type = as.character(type))
    df <- df %>%
      dplyr::left_join(main_data, by = c("iso_alpha_3_code", "year_start", "year_end", "type", "citation_short", "final_pm"))
    
    meta <- readRDS(here::here(main_path, 'meta.rds'))
    estimates <- readRDS(here::here(main_path, 'estimates.rds'))
    estimates_arr <- readRDS(here::here(main_path, "estimates_arr.rds")) 
    df_excluded <- readRDS(here::here(main_path, 'main_data_excluded.rds'))%>%
      dplyr::mutate(period = paste0("[",round(year_start, 2),", ", round(year_end, 2), ")")) %>%
      dplyr::relocate(period) %>%
      dplyr::filter(year_mid >= 1985) %>%
      dplyr::mutate(citation_short = gsub("&", "and", citation_short, perl=TRUE)) %>%
      # HACK INCLUDE REASON (dont know what causes yet - something in bmat where we get adjusted data?)
      dplyr::select(-include_reason) %>%
      dplyr::mutate(type = as.character(type))
    df_excluded <- df_excluded %>%
      dplyr::left_join(main_data, by = c("iso_alpha_3_code", "year_start", "year_end", "type", "citation_short", "final_pm"))
    
    
    detach("package:kableExtra", unload=TRUE)
    rmarkdown::render(here::here("vignettes","country_profile.Rmd"), 
                      output_file = here::here("output", round_name, "country_profile", lang, paste0(iso_alpha_3_code, ".pdf")),
                      envir = environment(),
                      params = list(iso_alpha_3_code = iso_alpha_3_code,
                                    who_name = who_name,
                                    round_name = round_name,
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
      file.copy(from = here::here("output", round_name, "country_profile", lang, paste0(iso_alpha_3_code, ".pdf")),
                to = here::here("output", round_name, "country_profile", lang, whoregion, paste0(iso_alpha_3_code, ".pdf")))
    }}}


