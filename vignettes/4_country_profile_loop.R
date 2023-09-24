


##########################################################################################################
##########################       Summary      ############################################################
##########################################################################################################
# This script produces country profiles.
##########################################################################################################



##########################################################################################################
##########################       SUPPLEMENTARY SOFTWARE REQUIRED     #####################################
##########################################################################################################
# The following supplementary software is needed to knit country profiles
# Run the following code in the Rstudio/R console to install supplementary software
# remotes::install_github('rstudio/rmarkdown')
# install.packages("tinytex")
# tinytex::reinstall_tinytex(repository = "illinois")
# library(tinytex)
# tlmgr_install('threeparttable')
# tlmgr_install('threeparttablex')   
# tlmgr_update() 
# install.packages("kableExtra")
# install.packages("float")
# install.packages("ggplot2")
# install.packages("ggnewscale")
##########################################################################################################




##########################################################################################################
##########################       Create profiles Settings / Setup   ######################################
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
text <- readxl::read_excel(here::here("data-raw", "countryprofile_translation.xlsx"))
# Meta data
meta <-  readRDS(
  here::here("output", round_name_update, "meta.rds")
)
##########################################################################################################
##########################################################################################################
##########################################################################################################
iso_alpha_3_codes <- meta$iso.c
##########################################################################################################
##########################################################################################################
##########################################################################################################
# Territory data
territory_info <- read.csv( 
  here::here("output", round_name_update, "country_ref.csv"), fileEncoding="latin1")
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

# main_data_hack <- read.csv(
#   here::here("output", round_name_update, "main_data.csv")) %>%
#   dplyr::select(year_start, year_end, iso_alpha_3_code, type, include_reason, citation_short, final_pm) 

##########################################################################################################
##########################       Create profiles     #####################################################
##########################################################################################################
# Estimate run time for the printing of all profiles is a few hours. Plan accordingly.


for(lang in langs) {
  Sys.setlocale("LC_CTYPE", locale = paste0(lang, ".UTF-8"))
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
      # dplyr::select(-include_reason) %>%
      dplyr::mutate(type = as.character(type))%>%
      dplyr::mutate(env_mat_final = ifelse(type == "inq", obs_matdeaths,
                                           ifelse(!is.na(obs_matdeaths), obs_matdeaths, env_mat))) %>%
      dplyr::mutate(obs_env = ifelse(type == "inq", final_env, obs_env)) 
    
    
    # df <- df %>%
    #   dplyr::left_join(main_data_hack, by = c("iso_alpha_3_code", "year_start", "year_end", "type", "citation_short", "final_pm"))
    
    meta <- readRDS(here::here(main_path, 'meta.rds'))
    estimates <- readRDS(here::here(main_path, 'estimates.rds'))
    estimates_arr <- readRDS(here::here(main_path, "estimates_arr.rds")) %>%
      dplyr::filter(period %in% c("2000, 2020", "2010, 2020"))
    
    df_excluded <- main_data %>%
      dplyr::filter(iso_alpha_3_code == !!iso_alpha_3_code) %>%
      dplyr::filter(!include) %>%
      dplyr::mutate(period = paste0("[",round(year_start, 2),", ", round(year_end, 2), ")")) %>%
      dplyr::relocate(period) %>%
      dplyr::filter(year_mid >= 1985) %>%
      dplyr::mutate(citation_short = gsub("&", "and", citation_short, perl=TRUE)) %>%
      dplyr::mutate(type = as.character(type))
    
    round_name_profile <- round_name_update
    detach("package:kableExtra", unload=TRUE)
    rmarkdown::render(here::here("vignettes","country_profile.Rmd"), 
                      output_file = here::here("output", round_name_update, "country_profile", lang, paste0(iso_alpha_3_code, ".pdf")),
                      envir = environment(),
                      params = list(iso_alpha_3_code = iso_alpha_3_code,
                                    who_name = who_name,
                                    round_name_profile = round_name_profile,
                                    years = years,
                                    df = df,
                                    df_excluded = df_excluded,
                                    meta = meta,
                                    estimates = estimates,
                                    estimates_arr = estimates_arr,
                                    table1_years = table1_years,
                                    text = text_one_lang,
                                    lang = lang
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
Sys.setlocale("LC_CTYPE", locale ="English.UTF-8")

