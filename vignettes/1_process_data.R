


##########################################################################################################
##########################       Summary      ############################################################
##########################################################################################################
# This script produces processed data sets in formats which are usable for the models. This involves 
# reading in data provided by extractors and external sources, processing the data, and saving the 
# processed data in a round specific folder. 
##########################################################################################################



##########################################################################################################
##########################       Settings/Setup      #####################################################
##########################################################################################################
# The round_name variable is used to give a name to the set of data and estimates you will produce as you 
# progress through the scripts.
devtools::load_all()
round_name <- "test"
round_first_year <- 1985
round_last_year <- 2023
common_na_strings <- c("NA", "")
# Create a directory based on the round name variable for all associated files to be saved.
dir.create(here::here("output"))
dir.create(here::here("output", round_name))
# Move translation file to our round-specific folder
file.copy(from = here::here("data-raw/countryprofile_translation.xlsx"),
          to = here::here("output", round_name, "countryprofile_translation.xlsx"))
##########################################################################################################



##########################################################################################################
##########################       Territory data      #####################################################
##########################################################################################################
# Processes and saves a file which contains territory information e.g. iso codes, regional groupgins, etc.
process_country_ref(
  country_ref_who_custom = readxl::read_excel(
    here::here("data-raw/country_territory_data/WHO_REF_COUNTRY_MMEIG_REGIONS_2019Update 16 August 2024.xlsx")),
  country_ref_who = readxl::read_excel(
    here::here("data-raw/country_territory_data/WHO_REF_COUNTRY_2024.08.26.xlsx"), skip = 1),
  sdg_regions = read.csv(
    here::here("data-raw/country_territory_data/sdg_regions_historical.csv")),
  round_name = round_name
)
##########################################################################################################



##########################################################################################################
##########################       FSI data      #####################################################
##########################################################################################################
process_fsi(
  country_ref = read.csv(
    here::here("output", round_name, "country_ref.csv")
    ),
  fsi <- readxl::read_excel(
    here::here("data-raw", "country_territory_data", "territory_by_fsi", "FSI-2023-DOWNLOAD.xlsx")
  ),
  namehack <- read.csv(
    here::here("data-raw", "country_territory_data", "territory_by_fsi", "fsi_custom_name_hack_for_coalesce.csv")
  )
)
##########################################################################################################



##########################################################################################################
##########################       Auxiliary (meta) data      ##############################################
##########################################################################################################
# Process and saves a file which contains precrisis auxiliary data such as covariates and denomination 
# data indexed by country and year (t). e.g. GDP, all cause deaths, etc.
process_precrisis_meta(
  country_ref = read.csv(
    here::here("output", round_name, "country_ref.csv")
  ),
  mortality = read.csv(
    here::here("data-raw", "auxiliary_data", "mortality_wpp24_crisisandlifetables.csv")) %>%
    dplyr::rename(iso_alpha_3_code = ISO3_code,
                  age = Age,
                  year = Year,
                  pop = PopExposure,
                  deaths=Deaths) %>%
    dplyr::mutate(crisis_deaths = crisis_deaths_OtherCrises + crisis_deaths_Conflict) %>%
    dplyr::mutate(crisis_deaths = ifelse(crisis_deaths < 0, 0, crisis_deaths)) %>% 
    dplyr::mutate(sex=2),
  births_gfr_data = readxl::read_excel(
    here::here("data-raw", "auxiliary_data", "WPP2024_Fertility_summary_MMEIG.xlsx"),
    sheet = 1,
    guess_max = 10000) %>% 
    dplyr::rename(iso_alpha_3_code = ISO3_code) %>%
    dplyr::mutate(GFR = as.numeric(GFR)) %>%
    dplyr::mutate(Births = Births/1000) %>% 
    dplyr::mutate(Female1549 = Female1549),
  mortality_hiv = read.csv(
    here::here("data-raw", "auxiliary_data", "hiv_2024.csv")),
  gdp_data = read.csv( here::here("data-raw", "auxiliary_data", "gdp_imputed_21092024.csv")),
  sab_data = readxl::read_excel(
    here::here("data-raw/auxiliary_data/estimates_base_0all_indicators_v2_sab_shared.xlsx")) %>% 
    dplyr::mutate(year = floor(year)) %>%
    dplyr::filter(year >= round_first_year) %>% 
    dplyr::filter(year <= round_last_year),
  first_year = round_first_year,
  last_year = round_last_year,
  round_name = round_name
)
##########################################################################################################



##########################################################################################################
##########################       Auxiliary (meta) data continued     #####################################
##########################################################################################################
# Process and saves a file which contains auxiliary data such as covariates and denomination data indexed 
# by country and year (t). e.g. GDP, all cause deaths, etc.
process_meta(
  meta_precrisis =  readRDS(
    here::here("output", round_name, "meta_precrisis.rds")),
  crisis_years_data = readxl::read_excel(
    here::here("data-raw", "auxiliary_data", "morality_crisis_years_UNIGME_2019.xx.xx.xlsx")),
  round_name = round_name
)
##########################################################################################################



##########################################################################################################
##########################       Vital registration data     #############################################
##########################################################################################################
# Process and saves a file which contains vital registration data. This process involves summing types 
# of maternal deaths by code. 
# We process two versions. One version contains frozen vrdata from when a consultation evaluated its vr system.
# BMis uses the version with frozen vrdata, BMat uses the standard current version
# without frozen
process_vr_wrapper(
  meta_precrisis =  readRDS(
    here::here("output", round_name, "meta_precrisis.rds")),
  vr_data = readxl::read_excel(
    here::here("data-raw", "dependent_data",  "mat05Aug2024.xls"),
    sheet=2)%>% 
    rbind(covid_vr = read.csv(
      here::here("data-raw", "dependent_data", "covid_vr.csv")) %>% 
        dplyr::filter(Sex==2) %>% 
        dplyr::rename(A85=A85ov) %>% 
        dplyr::mutate(A01=NA,A02=NA, A03=NA, A04=NA, A90=NA, A95ov=NA,...34=NA, 
                      date=NA) %>% 
        dplyr::select(-`A1_4`, -X )) %>%
    dplyr::select(-`...34` ) %>% 
    rbind(vr_add = readxl::read_excel(
      here::here("data-raw", "dependent_data",  "mat_new_2024_12_05.xlsx") )),
  vr_special_case = readxl::read_excel(
    here::here("data-raw", "dependent_data", "vital_registration_special_cases_MMEIG_2019.xx.xx.xlsx")),
  round_name = round_name
) 
# with frozen
process_vr_wrapper(
  with_frozen = TRUE,
  frozen_vr_data = read.csv(here::here("data-raw", "dependent_data", "vital_registration_frozen_pre_consult_MMEIG_2019.xx.xx.csv")),
  meta_precrisis =  readRDS(
    here::here("output", round_name, "meta_precrisis.rds")),
  vr_data = readxl::read_excel(
    here::here("data-raw", "dependent_data",  "mat05Aug2024.xls"),
    sheet=2)%>% 
    rbind(covid_vr = read.csv(
      here::here("data-raw", "dependent_data", "covid_vr.csv")) %>% 
        dplyr::filter(Sex==2) %>% 
        dplyr::rename(A85=A85ov) %>% 
        dplyr::mutate(A01=NA,A02=NA, A03=NA, A04=NA, A90=NA, A95ov=NA,...34=NA, 
                      date=NA) %>% 
        dplyr::select(-`A1_4`, -X ))%>%
    dplyr::select(-`...34` ) %>% 
    rbind(vr_add = readxl::read_excel(
      here::here("data-raw", "dependent_data",  "mat_new_2024_12_05.xlsx") )),
  vr_special_case = readxl::read_excel(
    here::here("data-raw", "dependent_data", "vital_registration_special_cases_MMEIG_2019.xx.xx.xlsx")),
  round_name = round_name
) 
vr_completness_data_for_steakholders(vrdata = read.csv(here::here("output", round_name, "vrdata.csv")))
##########################################################################################################



##########################################################################################################
##########################       Study data    ###########################################################
##########################################################################################################
# First combine 2022 and 2024 data sets 
data_2021 = readxl::read_excel(
  here::here("data-raw", "dependent_data", "specialised_studies_MMEIG_2022.12.19.xlsx"),  na = common_na_strings) %>%
  dplyr::rename(year_end = `year_end `) %>%
  dplyr::select(-refid, -live_births) %>%
  dplyr::mutate(entry_type = ifelse(iso_alpha_3_code == "IND", "consult", "extract")) %>%
  dplyr::rename(env_mat = env_maternal)
data_2024 = readxl::read_excel(
  here::here("data-raw", "dependent_data", "specialised_studies_MMEIG_2024.05.13v2.xlsx"),  na = common_na_strings) %>%
  dplyr::rename(year_end = `year_end `) %>%
  dplyr::select(-refid, -live_births) %>%
  dplyr::mutate(entry_type = ifelse(iso_alpha_3_code == "IND", "consult", "extract")) %>%
  dplyr::rename(env_mat = env_maternal) %>% 
  dplyr::mutate(env_total=as.double(env_total))
ssdat_combined <- dplyr:: bind_rows(data_2021, data_2024)
# Process and saves study data.
process_study_data(
  vrdata = read.csv(
    here::here("output", round_name, "vrdata_w_frozen.csv"),
    na.strings = common_na_strings
  ),
  vrdata_edit_paho2019 = readxl::read_excel(
    here::here("data-raw", "dependent_data", "vital_registration_edit_from_PAHO_2019.xx.xx.xlsx"), 
    skip = 2,
    na = common_na_strings
  ),
  meta =  readRDS(
    here::here("output", round_name, "meta.rds")
  ),
  meta_precrisis =  readRDS(
    here::here("output", round_name, "meta_precrisis.rds")
  ),
  first_year = round_first_year,
  data_2019 = readxl::read_excel(
    here::here("data-raw","dependent_data","specialized_studies_MMEIG_2019.xx.xx.xlsx"),  na = common_na_strings) %>%
    dplyr::filter(!subnational) %>%
    dplyr::mutate(tp = as.numeric(tp),
                  fp = as.numeric(fp),
                  fn = as.numeric(fn),
                  tn = as.numeric(tn),
                  env_total = as.numeric(env_total),
                  env_mat = as.numeric(env_mat),
                  up = as.numeric(up),
                  un = as.numeric(un)),
  data_2021 = ssdat_combined,
  round_name = round_name
)
##########################################################################################################



##########################################################################################################
##########################       Census data    ##########################################################
##########################################################################################################
# Process and saves census data.
process_census_data(
  census_data = readxl::read_xlsx(
    here::here("data-raw", "dependent_data", "census_WHO_2024.10.11.xlsx"), skip =1,  na = common_na_strings),
  meta =  readRDS(
    here::here("output", round_name, "meta.rds")),
  meta_precrisis =  readRDS(
    here::here("output", round_name, "meta_precrisis.rds")),
  round_first_year = round_first_year,
  round_last_year = round_last_year,
  round_name
)
##########################################################################################################



##########################################################################################################
##########################       Survey data    ##########################################################
##########################################################################################################
# Process and saves survey data.
# HACK to remove duplicates (the years are slightly different which allows them to slip through code which checks for duplicates normally)
# The versions to keep are below
# the COD observations using the country name "Democratic Republic of the Congo", with the exact years 2000.495-2007.495 and 2007.082-2014.082
# the SWZ observation using the country name "Swaziland", with the exact years 1999.83-2006.83
# to remove
# COD country.name == "Congo Democratic Republic"
# SWZ start.date == 1999.5
process_survey_data(
  survey_data =read.csv(
    here::here("data-raw", "dependent_data", "Dhs_input_2024-08-30.csv")) %>%
    dplyr::filter(definition != "Maternal-DHS") %>%
    dplyr::mutate(
      pmdf = as.numeric(gsub("[^0-9eE.-]", "", pmdf)),
      total.deaths.obs = as.numeric(gsub("[^0-9eE.-]", "", total.deaths.obs)),
      se.pmdf.calc = as.numeric(gsub("[^0-9eE.-]", "", se.pmdf.calc)),
      log.pmdf.calc = as.numeric(gsub("[^0-9eE.-]", "", log.pmdf.calc)),
      se.log.pmdf.calc = as.numeric(gsub("[^0-9eE.-]", "", se.log.pmdf.calc))), 
  round_name = round_name
)
##########################################################################################################



##########################################################################################################
##########################       Miscellaneous data    ###################################################
##########################################################################################################
# Process and saves miscellaneous data.
process_miscellaneous_data(
  misc_data =  readxl::read_excel(
    here::here("data-raw", "dependent_data", "miscellaneous_MMEIG_2019.xx.xx.xlsx"), na = common_na_strings) %>%
    dplyr::rename(final_pm = pm) %>%
    dplyr::mutate_at(c("env_mat", "env_pregrelated", "env_total", "final_pm", "mmr", "live_births"), as.numeric) %>%
    dplyr::bind_rows(readxl::read_xlsx(
      here::here("data-raw", "dependent_data", "miscellaneous_MMEIG_2022.12.19.xlsx"),  na = common_na_strings) %>% 
        dplyr::mutate(definition = "maternal") %>%
        dplyr::rename(live_births = `live births`) %>%
        dplyr::mutate(final_pm = NA)) %>% 
    dplyr::bind_rows(readxl::read_xlsx(
      here::here("data-raw", "dependent_data", "miscellaneous_MMEIG_2024.08.22.xlsx"),  na = common_na_strings) %>% 
        dplyr::mutate(definition = "maternal") %>%
        dplyr::rename(live_births = `live births`) %>%
        dplyr::mutate(final_pm = NA)),
  meta =  readRDS(
    here::here("output", round_name, "meta.rds")),
  meta_precrisis =  readRDS(
    here::here("output", round_name, "meta_precrisis.rds")),
  round_first_year = round_first_year,
  round_last_year = round_last_year,
  round_name
)

##########################################################################################################
### Exception using CRVS total deaths instead of WPP for AUS 2022, and LUX (with linear extrapolation for 2023 LUX)
vrdata <-  read.csv(here::here("output", round_name, "vrdata.csv"))
meta <-  readRDS(here::here("output", round_name, "meta.rds"))
meta$deaths.ct[which(meta$iso.c=="AUS"), which(meta$year.t==2022)] <- vrdata$final_env[vrdata$iso_alpha_3_code=="AUS"&vrdata$year_start==2022]
x <- vrdata$year_start[vrdata$iso_alpha_3_code == "LUX"]
x <- x[1:35]
y <- vrdata$final_env[vrdata$iso_alpha_3_code == "LUX"]
y <- y[1:35]
model <- lm(y ~ x)
new_x <- 2023
predicted_y <- predict(model, newdata = data.frame(x = new_x))
result <- data.frame(new_x = new_x, predicted_y = predicted_y)
y <- vrdata$final_env[vrdata$iso_alpha_3_code == "LUX"]
y[39] <- result[1,2]
meta$deaths.ct[which(meta$iso.c=="LUX"),] <- y
saveRDS(meta, here::here("output", round_name, "meta.rds"))
meta_precrisis <- readRDS(here::here("output", round_name, "meta_precrisis.rds"))
meta_precrisis$deaths.ct[which(meta_precrisis$iso.c=="LUX"),] <- y
saveRDS(meta_precrisis, here::here("output", round_name, "meta_precrisis.rds"))

