
##########################################################################################################
##########################       Settings     ############################################################
##########################################################################################################
devtools::load_all()
round_name <- "estimates_08-14-22_rev_frozen_vr"
round_first_year <- 1985
round_last_year <- 2020
arr_periods_list <- list(c(2000,2015), c(2000, 2020), c(2010, 2020), c(2000, 2005), c(2005, 2010), c(2010, 2015), c(2015, 2020))
##########################################################################################################



##########################################################################################################
##########################       Read in generic data     ################################################
##########################################################################################################
country_ref <- read.csv( 
  here::here("output", round_name, "country_ref.csv"))
meta <-  readRDS(
  here::here("output", round_name, "meta.rds")
)
##########################################################################################################




##########################################################################################################
##########################      UNICEF     ###############################################################
##########################################################################################################
group_data_unicef<- readxl::read_excel(here::here("data-raw", "country_territory_data", "territory_groups_for_report", "UNICEF_territory.xlsx"))
group_data_unicef_mapping <- readxl::read_excel(here::here("data-raw", "country_territory_data", "territory_groups_for_report", "UNICEF_territory_names.xlsx"))
group_data_unicef <- group_data_unicef %>%
  dplyr::left_join(group_data_unicef_mapping)
group_data_unicef <- group_data_unicef  %>%
  dplyr::select(UNICEF_Sub_Region_Name, UNICEF_Region_Name, iso_alpha_3_code = `ISO Code`) %>%
  tidyr::drop_na(iso_alpha_3_code)


##########################################################################################################
##########################       UNPD      ###############################################################
##########################################################################################################
group_data_unpd <- readxl::read_excel(here::here("data-raw", "country_territory_data", "territory_groups_for_report", "UNPD_territory.xlsx"))
group_data_unpd <- group_data_unpd %>%
  dplyr::rename(m49 = LocID) %>%
  dplyr::left_join(country_ref %>% dplyr::select(m49, iso_alpha_3_code), by = c("m49"))
group_data_unpd <- group_data_unpd %>%
  dplyr::select(iso_alpha_3_code, 
                UNPD_Region = `Geographic Name`, 
                UNPD_Sub_Region = `Subregion Name`, 
                UNPD_More_Developed = `More developed regions\r\n901`, 
                UNPD_Less_Developed = `Less developed regions\r\n902`, 
                UNPD_Least_Developed = `Least developed countries\r\n941`, 
                UNPD_Land_Locked_Developing = `Land-Locked Developing Countries (LLDC)\r\n1636`, 
                UNPD_Small_Island_Developing = `Small Island Developing States (SIDS)\r\n1637`) %>%
  tidyr::drop_na(iso_alpha_3_code)



##########################################################################################################
##########################       SDG      ################################################################
##########################################################################################################
group_data_wpp <- readxl::read_excel(here::here("data-raw", "country_territory_data", "territory_groups_for_report", "WPP_territory.xlsx"), skip = 0, sheet = 2)
group_data_wpp <- group_data_wpp %>%
  dplyr::filter(LocTypeName == "Country/Area") %>%
  dplyr::select(iso_alpha_3_code = ISO3_Code,
                SDG_Region = SDGRegName,
                SDG_Sub_Region = SubRegName) 

group_data_wb <- readxl::read_excel(here::here("data-raw", "country_territory_data", "territory_groups_for_report", "world_bank_territory.xlsx"), skip = 0, sheet = 1)
group_data_wb <- group_data_wb %>%
  dplyr::select(iso_alpha_3_code = Code,
                World_Bank_Region = Region,
                World_Bank_Income = `Income group`)



##########################################################################################################
##########################       UNFPA      ##############################################################
##########################################################################################################
group_data_UNFPA <- readxl::read_excel(here::here("data-raw", "country_territory_data", "territory_groups_for_report", "UNFPA_territory.xlsx"), skip = 0, sheet = 1)
group_data_UNFPA <- group_data_UNFPA %>%
  dplyr::select(iso_alpha_3_code = ISOCode,
                UNFPA_Region = `UNFPA regions`) 


##########################################################################################################
##########################       WHO      ##############################################################
##########################################################################################################
group_data_WHO <- country_ref %>%
  dplyr::select(iso_alpha_3_code, WHO_Region = grp_who_region)


df  <- group_data_unicef %>%
  dplyr::left_join(group_data_unpd) %>%
  dplyr::left_join(group_data_wpp)%>%
  dplyr::left_join(group_data_wb)%>%
  dplyr::left_join(group_data_UNFPA)%>%
  dplyr::left_join(group_data_WHO)

df <- df %>%
  dplyr::relocate(iso_alpha_3_code) %>%
  dplyr::arrange(iso_alpha_3_code)

# write.csv(df, row.names = FALSE, here::here("data-raw", "country_territory_data", "all_agency_territory_groups.csv"))





##########################################################################################################
####################      Hack to change region codes to names in some files     #########################
##########################################################################################################
round_name <- "estimates_08-14-22_rev_frozen_vr"
country_ref <- read.csv( 
  here::here("output", round_name, "country_ref.csv"))

group_data_who <- country_ref %>%
  dplyr::select(WHO_Region = grp_who_region) %>%
  dplyr::distinct() %>%
  dplyr::arrange(WHO_Region) %>%
  dplyr::mutate(WHO_Region_new = c("Africa", "Americas", "Eastern Mediterranean", "Europe", "South-East Asia", "Western Pacific"))
# group_data_unpd <- data.frame(group = as.numeric(c("901", "902", "941", "1636", "1637" )), 
#                               group_new = c("More Developed", "Less Developed", "Least Developed", "Land Locked Developing", "Small Island Developing States"))

df2 <- df %>%
  dplyr::left_join(group_data_who) %>%
  dplyr::select(-WHO_Region) %>%
  dplyr::rename(WHO_Region = WHO_Region_new) %>%
  dplyr::mutate(UNPD_More_Developed = ifelse(UNPD_More_Developed == 901, "More Developed", UNPD_More_Developed)) %>%
  dplyr::mutate(UNPD_Less_Developed = ifelse(UNPD_Less_Developed == 902, "Less Developed", UNPD_More_Developed)) %>%
  dplyr::mutate(UNPD_Least_Developed = ifelse(UNPD_Least_Developed == 941, "Least Developed", UNPD_More_Developed)) %>%
  dplyr::mutate(UNPD_Land_Locked_Developing = ifelse(UNPD_Land_Locked_Developing == 1636, "Land Locked Developing", UNPD_More_Developed)) %>%
  dplyr::mutate(UNPD_Small_Island_Developing = ifelse(UNPD_Small_Island_Developing == 1637, "Small Island Developing States", UNPD_More_Developed))

write.csv(df2, row.names = FALSE, here::here("data-raw", "country_territory_data", "all_agency_territory_groups.csv"))
