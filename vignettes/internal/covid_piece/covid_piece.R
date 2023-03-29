##########################################################################################################
devtools::load_all()
round_name <-  "estimates_08-14-22_rev_frozen_vr"
round_first_year <- 1985
round_last_year <- 2020
run_length <- "test"
server <- FALSE
meta <-  readRDS(here::here("output", round_name, "meta.rds"))
meta_precrisis <-  readRDS(here::here("output", round_name, "meta_precrisis.rds"))
##########################################################################################################
main_data = read.csv(
  here::here("output", round_name, "main_data.csv")) %>% 
  dplyr::filter(iso_alpha_3_code %in% meta$iso.c)


#######################################################
# Exploring how many countries were in a covid crisis etc
crisis_ind <- which(lapply(meta$crisis_years.c, length) > 0)
customf <- function(x) {
  x == 2020
}
crisis_c <- lapply(lapply(meta$crisis_years.c, customf), any) %>% unlist()
crisis_c %>% sum
meta$iso.c[crisis_c]
temp <- main_data %>%
  dplyr::filter(iso_alpha_3_code %in% meta$iso.c[crisis_c]) %>%
  dplyr::filter(year_start == 2020)
nrow(temp)
temp2 <- main_data %>%
  dplyr::filter(year_start == 2020)
nrow(temp2)
#######################################################



##########################################################################################################
# remove covid crisis deaths from crisis deaths
# done in processing of mortality data
process_precrisis_meta(
  country_ref = read.csv(
    here::here("output", round_name, "country_ref.csv")
  ),
  mortality = read.csv(
    here::here("data-raw", "auxiliary_data", "mortality_WPP_2022.06.21.csv")) %>%
    dplyr::rename(iso_alpha_3_code = ISO3_code,
                  age = Age,
                  year = Year,
                  pop = Population_Exposure) %>%
    dplyr::mutate(crisis_deaths = crisis_deaths_OtherCrises) %>%
    dplyr::mutate(crisis_deaths = ifelse(crisis_deaths < 0, 0, crisis_deaths)),
  mortality_to_denominate_hiv = read.csv( 
    here::here("data-raw", "auxiliary_data", "mortality_WPP_2019.xx.xx.csv")) %>%
    dplyr::rename(iso_alpha_3_code = iso3),
  births_gfr_data = read.csv(
    here::here("data-raw", "auxiliary_data", "births_and_fertility_WPP_2022.06.21.csv")) %>%
    dplyr::rename(iso_alpha_3_code = ISO3_code) %>%
    dplyr::mutate(GFR = as.numeric(GFR)),
  mortality_hiv = read.csv(
    here::here("data-raw", "auxiliary_data", "mortality_hiv_UNAIDS_2021.07.30.csv")),
  gdp_data = readxl::read_excel(
    here::here("data-raw", "auxiliary_data", "gdp_WORLDBANK_2022.07.11.xlsx"),
    sheet = 1,
    skip = 4
  ) %>%
    dplyr::rename(iso_alpha_3_code = "Country Code"),
  sab_data = read.csv(
    here::here("data-raw/auxiliary_data/sab_MMEIG_2022.07.08.csv")) %>% 
    dplyr::mutate(year = floor(year)) %>%
    dplyr::filter(year >= round_first_year) %>% 
    dplyr::filter(year <= round_last_year),
  first_year = round_first_year,
  last_year = round_last_year,
  round_name = round_name
)
##########################################################################################################




# Interpolate data (really just 2020 <- 2019 since outside of an interval)
meta2<- meta
lastind <- length(round_first_year:round_last_year)
meta2$logGDP.ct %>% dim
meta2$logGDP.ct[,lastind] <- NA
meta2$sab.ct[,lastind] <- NA
meta2$gfr.ct[,lastind] <- NA
meta2$deaths.ct[,lastind] <- NA
# example code
# rule 2 uses the min and max value imputation outside the interpolation interval of [min(x), max(x)].
# approx(x = 1:5, y = c(1,2,5,3,NA), xout = 1:5, rule = 2)$y

for(i in 1:length(meta$iso.c)) {
  meta2$logGDP.ct[i,] <-  approx(x = 1:lastind, y = meta2$logGDP.ct[i,], xout = 1:lastind, rule = 2)$y  #rule 2 uses the min and max value imputation outside the interpolation interval of [min(x), max(x)].
  meta2$sab.ct[i,] <-  approx(x = 1:lastind, y = meta2$sab.ct[i,], xout = 1:lastind, rule = 2)$y
  meta2$gfr.ct[i,] <-  approx(x = 1:lastind, y = meta2$gfr.ct[i,], xout = 1:lastind, rule = 2)$y
  meta2$deaths.ct[i,] <-  approx(x = 1:lastind, y = meta2$deaths.ct[i,], xout = 1:lastind, rule = 2)$y
}
#######################################################



# no studies in 2020 so bmis estimates are fine to use as is
ssdata <- read.csv(here::here("output", round_name, "ssdata.csv")) %>% dplyr::filter(year_start == 2020)
