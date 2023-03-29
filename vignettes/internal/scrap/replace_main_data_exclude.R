devtools::load_all()
round_name <- "estimates-7-15-22"
round_first_year <- 1985
round_last_year <- 2020
run_length <- "quick_local"
server <- FALSE
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

main_data = read.csv(
  here::here("output", round_name, "main_data.csv")) %>% 
  dplyr::filter(iso_alpha_3_code %in% meta$iso.c)
# For each country run the model
iso_alpha_3_codes <- meta$iso.c
for (iso_alpha_3_code in iso_alpha_3_codes) { 
  main_path <- make_output_directory_return_path(round_name, iso_alpha_3_code, FALSE, "bmat")
  c_index_from_global <- which(meta$iso.c == iso_alpha_3_code)
  main_data_exl <- main_data %>% 
    dplyr::filter(iso_alpha_3_code == !!iso_alpha_3_code) %>%
    dplyr::filter(!include)

  saveRDS(main_data_exl, here::here(main_path, "main_data_excluded.rds"))
}


