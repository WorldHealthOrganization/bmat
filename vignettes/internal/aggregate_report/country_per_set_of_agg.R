
##########################################################################################################
##########################       Settings     ############################################################
##########################################################################################################
devtools::load_all()
round_name <- "estimates_08-14-22_rev_ecu"
round_first_year <- 1985
round_last_year <- 2020
##########################################################################################################



##########################################################################################################
##########################       Read in generic data     ################################################
##########################################################################################################
country_ref <- read.csv( 
  here::here("output", round_name, "country_ref.csv"))
meta <-  readRDS(
  here::here("output", round_name, "meta.rds")
)
isos <- meta$iso.c
##########################################################################################################


##########################################################################################################
##########################       UNPD      ###############################################################
##########################################################################################################
group_data_unpd <- readxl::read_excel(here::here("data-raw", "country_territory_data", "territory_groups_for_report", "UNPD_territory.xlsx"))
group_data_unpd <- group_data_unpd %>%
  dplyr::rename(m49 = LocID) %>%
  dplyr::left_join(country_ref %>% dplyr::select(m49, iso_alpha_3_code, name_short_en), by = c("m49"))

# UNPD_Region
group_data <- group_data_unpd %>%
  dplyr::select(iso_alpha_3_code, name_short_en, group = `Geographic Name`) %>%
  tidyr::drop_na(group) %>%
  dplyr::filter(iso_alpha_3_code %in% isos) %>%
  dplyr::select(iso_alpha_3_code, name_short_en) %>%
  dplyr::distinct(iso_alpha_3_code, .keep_all = TRUE) 
N <- nrow(group_data)
write.csv(group_data, row.names = FALSE, here::here("output", round_name, "aggregates", "territories_used", paste0("UNPD_Region", "_",N,"_countries.csv")))

# UNPD_Sub_Region
group_data <- group_data_unpd %>%
  dplyr::select(iso_alpha_3_code, name_short_en, group = `Subregion Name`) %>%
  tidyr::drop_na(group) %>%
  dplyr::filter(iso_alpha_3_code %in% isos) %>%
  dplyr::select(iso_alpha_3_code, name_short_en) %>%
  dplyr::distinct(iso_alpha_3_code, .keep_all = TRUE) 
N <- nrow(group_data)
write.csv(group_data, row.names = FALSE, here::here("output", round_name, "aggregates", "territories_used", paste0("UNPD_Sub_Region", "_",N,"_countries.csv")))

# UNPD_More_Developed
group_data <- group_data_unpd %>%
  dplyr::select(iso_alpha_3_code, name_short_en, group = `More developed regions\r\n901`)%>%
  tidyr::drop_na(group) %>%
  dplyr::filter(iso_alpha_3_code %in% isos) %>%
  dplyr::select(iso_alpha_3_code, name_short_en) %>%
  dplyr::distinct(iso_alpha_3_code, .keep_all = TRUE) 
N <- nrow(group_data)
write.csv(group_data, row.names = FALSE, here::here("output", round_name, "aggregates", "territories_used", paste0("UNPD_More_Developed", "_",N,"_countries.csv")))

# UNPD_Less_Developed
group_data <- group_data_unpd %>%
  dplyr::select(iso_alpha_3_code, name_short_en, group = `Less developed regions\r\n902`)%>%
  tidyr::drop_na(group) %>%
  dplyr::filter(iso_alpha_3_code %in% isos) %>%
  dplyr::select(iso_alpha_3_code, name_short_en) %>%
  dplyr::distinct(iso_alpha_3_code, .keep_all = TRUE) 
N <- nrow(group_data)
write.csv(group_data, row.names = FALSE, here::here("output", round_name, "aggregates", "territories_used", paste0("UNPD_Less_Developed", "_",N,"_countries.csv")))

# UNPD_Least_Developed
group_data <- group_data_unpd %>%
  dplyr::select(iso_alpha_3_code, name_short_en, group = `Least developed countries\r\n941`)%>%
  tidyr::drop_na(group) %>%
  dplyr::filter(iso_alpha_3_code %in% isos) %>%
  dplyr::select(iso_alpha_3_code, name_short_en) %>%
  dplyr::distinct(iso_alpha_3_code, .keep_all = TRUE) 
N <- nrow(group_data)
write.csv(group_data, row.names = FALSE, here::here("output", round_name, "aggregates", "territories_used", paste0("UNPD_Least_Developed", "_",N,"_countries.csv")))

# UNPD_Land_Locked_Developing
group_data <- group_data_unpd %>%
  dplyr::select(iso_alpha_3_code, name_short_en, group = `Land-Locked Developing Countries (LLDC)\r\n1636`)%>%
  tidyr::drop_na(group) %>%
  dplyr::filter(iso_alpha_3_code %in% isos) %>%
  dplyr::select(iso_alpha_3_code, name_short_en) %>%
  dplyr::distinct(iso_alpha_3_code, .keep_all = TRUE) 
N <- nrow(group_data)
write.csv(group_data, row.names = FALSE, here::here("output", round_name, "aggregates", "territories_used", paste0("UNPD_Land_Locked_Developing", "_",N,"_countries.csv")))

# UNPD_Small_Island_Developing
group_data <- group_data_unpd %>%
  dplyr::select(iso_alpha_3_code, name_short_en, group = `Small Island Developing States (SIDS)\r\n1637`)%>%
  tidyr::drop_na(group) %>%
  dplyr::filter(iso_alpha_3_code %in% isos) %>%
  dplyr::select(iso_alpha_3_code, name_short_en) %>%
  dplyr::distinct(iso_alpha_3_code, .keep_all = TRUE)
N <- nrow(group_data)
write.csv(group_data, row.names = FALSE, here::here("output", round_name, "aggregates", "territories_used", paste0("UNPD_Small_Island_Developing", "_",N,"_countries.csv")))

##########################################################################################################



##########################################################################################################
##########################       SDG      ################################################################
##########################################################################################################
group_data_wpp <- readxl::read_excel(here::here("data-raw", "country_territory_data", "territory_groups_for_report", "WPP_territory.xlsx"), skip = 0, sheet = 2)
group_data_wpp <- group_data_wpp %>%
  dplyr::filter(LocTypeName == "Country/Area") %>%
  dplyr::select(iso_alpha_3_code = ISO3_Code,
                SDG_Region = SDGRegName,
                SDG_Sub_Region = SubRegName)  %>%
  dplyr::left_join(country_ref %>% dplyr::select(iso_alpha_3_code, name_short_en), by = c("iso_alpha_3_code"))

# SDG_Region
group_data <- group_data_wpp %>%
  dplyr::select(iso_alpha_3_code, name_short_en, group = SDG_Region)%>%
  tidyr::drop_na(group) %>%
  dplyr::filter(iso_alpha_3_code %in% isos) %>%
  dplyr::select(iso_alpha_3_code, name_short_en) %>%
  dplyr::distinct(iso_alpha_3_code, .keep_all = TRUE) 
N <- nrow(group_data)
write.csv(group_data, row.names = FALSE, here::here("output", round_name, "aggregates", "territories_used", paste0("SDG_Region", "_",N,"_countries.csv")))

# SDG_Sub_Region
group_data <- group_data_wpp %>%
  dplyr::select(iso_alpha_3_code, name_short_en, group = SDG_Sub_Region)%>%
  tidyr::drop_na(group) %>%
  dplyr::filter(iso_alpha_3_code %in% isos) %>%
  dplyr::select(iso_alpha_3_code, name_short_en) %>%
  dplyr::distinct(iso_alpha_3_code, .keep_all = TRUE) 
N <- nrow(group_data)
write.csv(group_data, row.names = FALSE, here::here("output", round_name, "aggregates", "territories_used", paste0("SDG_Sub_Region", "_",N,"_countries.csv")))

##########################################################################################################



##########################################################################################################
##########################       World Bank      #########################################################
##########################################################################################################
group_data_wb <- readxl::read_excel(here::here("data-raw", "country_territory_data", "territory_groups_for_report", "world_bank_territory.xlsx"), skip = 0, sheet = 1)
group_data_wb <- group_data_wb %>%
  dplyr::select(iso_alpha_3_code = Code,
                region = Region,
                income_group = `Income group`) %>%
  dplyr::left_join(country_ref %>% dplyr::select(iso_alpha_3_code, name_short_en), by = c("iso_alpha_3_code"))

# World_Bank_Region
group_data <- group_data_wb %>%
  dplyr::select(iso_alpha_3_code, name_short_en, group = region)%>%
  tidyr::drop_na(group) %>%
  dplyr::filter(iso_alpha_3_code %in% isos) %>%
  dplyr::select(iso_alpha_3_code, name_short_en) %>%
  dplyr::distinct(iso_alpha_3_code, .keep_all = TRUE) 
N <- nrow(group_data)
write.csv(group_data, row.names = FALSE, here::here("output", round_name, "aggregates", "territories_used", paste0("World_Bank_Region", "_",N,"_countries.csv")))

# World_Bank_Income
group_data <- group_data_wb %>%
  dplyr::select(iso_alpha_3_code, name_short_en, group = income_group)%>%
  tidyr::drop_na(group) %>%
  dplyr::filter(iso_alpha_3_code %in% isos) %>%
  dplyr::select(iso_alpha_3_code, name_short_en) %>%
  dplyr::distinct(iso_alpha_3_code, .keep_all = TRUE) 
N <- nrow(group_data)
write.csv(group_data, row.names = FALSE, here::here("output", round_name, "aggregates", "territories_used", paste0("World_Bank_Income", "_",N,"_countries.csv")))

##########################################################################################################



##########################################################################################################
##########################       UNFPA      ##############################################################
##########################################################################################################
group_data_UNFPA <- readxl::read_excel(here::here("data-raw", "country_territory_data", "territory_groups_for_report", "UNFPA_territory.xlsx"), skip = 0, sheet = 1)
group_data_UNFPA <- group_data_UNFPA %>%
  dplyr::select(iso_alpha_3_code = ISOCode,
                region = `UNFPA regions`) %>%
  dplyr::left_join(country_ref %>% dplyr::select(iso_alpha_3_code, name_short_en), by = c("iso_alpha_3_code"))

# UNFPA_Region
group_data <- group_data_UNFPA %>%
  dplyr::select(iso_alpha_3_code, name_short_en, group = region)%>%
  tidyr::drop_na(group) %>%
  dplyr::filter(iso_alpha_3_code %in% isos) %>%
  dplyr::select(iso_alpha_3_code, name_short_en) %>%
  dplyr::distinct(iso_alpha_3_code, .keep_all = TRUE) 
N <- nrow(group_data)
write.csv(group_data, row.names = FALSE, here::here("output", round_name, "aggregates", "territories_used", paste0("UNFPA_Region", "_",N,"_countries.csv")))

##########################################################################################################







