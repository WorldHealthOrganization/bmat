
##########################################################################################################
##########################       Settings     ############################################################
##########################################################################################################
devtools::load_all()
round_name <- "estimates_08-14-22_rev_frozen_vr"
round_first_year <- 1985
round_last_year <- 2020
arr_periods_list <- list(c(2000,2015), c(2000, 2020), c(2010, 2020), c(2000, 2005), c(2005, 2010), c(2010, 2015), c(2015, 2020), c(2016, 2020))
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

# UNICEF_Region
group_data <- group_data_unicef  %>%
  dplyr::select(group = UNICEF_Region_Name, iso_alpha_3_code = `ISO Code`)
calculate_bmat_aggregate(round_name = round_name,
                         aggregate_group_data = group_data,
                         arr_periods = arr_periods_list,
                         iso_alpha_3_codes = meta$iso.c,
                         type_of_group = "UNICEF_Region")

# UNICEF_Sub_Region
group_data <- group_data_unicef  %>%
  dplyr::select(group = UNICEF_Sub_Region_Name, iso_alpha_3_code = `ISO Code`)
calculate_bmat_aggregate(round_name = round_name,
                         aggregate_group_data = group_data,
                         arr_periods = arr_periods_list,
                         iso_alpha_3_codes = meta$iso.c,
                         type_of_group = "UNICEF_Sub_Region")



##########################################################################################################
##########################       FSI      ################################################################
##########################################################################################################
group_data <- read.csv(here::here("output", round_name, "fsi.csv"))
group_data <- group_data  %>%
  dplyr::select(group, iso_alpha_3_code)
# FSI
calculate_bmat_aggregate(round_name = round_name,
                         aggregate_group_data = group_data,
                         arr_periods = arr_periods_list,
                         iso_alpha_3_codes = meta$iso.c,
                         type_of_group = "FSI")


##########################################################################################################
##########################       UNPD      ###############################################################
##########################################################################################################
group_data_unpd <- readxl::read_excel(here::here("data-raw", "country_territory_data", "territory_groups_for_report", "UNPD_territory.xlsx"))
group_data_unpd <- group_data_unpd %>%
  dplyr::rename(m49 = LocID) %>%
  dplyr::left_join(country_ref %>% dplyr::select(m49, iso_alpha_3_code), by = c("m49"))

# UNPD_Region
group_data <- group_data_unpd %>%
  dplyr::select(iso_alpha_3_code, group = `Geographic Name`)
calculate_bmat_aggregate(round_name = round_name,
                         aggregate_group_data = group_data,
                         arr_periods = arr_periods_list,
                         iso_alpha_3_codes = meta$iso.c,
                         type_of_group = "UNPD_Region")

# UNPD_Sub_Region
group_data <- group_data_unpd %>%
  dplyr::select(iso_alpha_3_code, group = `Subregion Name`)
calculate_bmat_aggregate(round_name = round_name,
                         aggregate_group_data = group_data,
                         arr_periods = arr_periods_list,
                         iso_alpha_3_codes = meta$iso.c,
                         type_of_group = "UNPD_Sub_Region")

# UNPD_More_Developed
group_data <- group_data_unpd %>%
  dplyr::select(iso_alpha_3_code, group = `More developed regions\r\n901`)
calculate_bmat_aggregate(round_name = round_name,
                         aggregate_group_data = group_data,
                         arr_periods = arr_periods_list,
                         iso_alpha_3_codes = meta$iso.c,
                         type_of_group = "UNPD_More_Developed")

# UNPD_Less_Developed
group_data <- group_data_unpd %>%
  dplyr::select(iso_alpha_3_code, group = `Less developed regions\r\n902`)
calculate_bmat_aggregate(round_name = round_name,
                         aggregate_group_data = group_data,
                         arr_periods = arr_periods_list,
                         iso_alpha_3_codes = meta$iso.c,
                         type_of_group = "UNPD_Less_Developed")

# UNPD_Least_Developed
group_data <- group_data_unpd %>%
  dplyr::select(iso_alpha_3_code, group = `Least developed countries\r\n941`)
calculate_bmat_aggregate(round_name = round_name,
                         aggregate_group_data = group_data,
                         arr_periods = arr_periods_list,
                         iso_alpha_3_codes = meta$iso.c,
                         type_of_group = "UNPD_Least_Developed")

# UNPD_Land_Locked_Developing
group_data <- group_data_unpd %>%
  dplyr::select(iso_alpha_3_code, group = `Land-Locked Developing Countries (LLDC)\r\n1636`)
calculate_bmat_aggregate(round_name = round_name,
                         aggregate_group_data = group_data,
                         arr_periods = arr_periods_list,
                         iso_alpha_3_codes = meta$iso.c,
                         type_of_group = "UNPD_Land_Locked_Developing")

# UNPD_Small_Island_Developing
group_data <- group_data_unpd %>%
  dplyr::select(iso_alpha_3_code, group = `Small Island Developing States (SIDS)\r\n1637`)
calculate_bmat_aggregate(round_name = round_name,
                         aggregate_group_data = group_data,
                         arr_periods = arr_periods_list,
                         iso_alpha_3_codes = meta$iso.c,
                         type_of_group = "UNPD_Small_Island_Developing")
##########################################################################################################



##########################################################################################################
##########################       SDG      ################################################################
##########################################################################################################
group_data_wpp <- readxl::read_excel(here::here("data-raw", "country_territory_data", "territory_groups_for_report", "WPP_territory.xlsx"), skip = 0, sheet = 2)
group_data_wpp <- group_data_wpp %>%
  dplyr::filter(LocTypeName == "Country/Area") %>%
  dplyr::select(iso_alpha_3_code = ISO3_Code,
                SDG_Region = SDGRegName,
                SDG_Sub_Region = SubRegName) 

# SDG_Region
group_data <- group_data_wpp %>%
  dplyr::select(iso_alpha_3_code, group = SDG_Region)
calculate_bmat_aggregate(round_name = round_name,
                         aggregate_group_data = group_data,
                         arr_periods = arr_periods_list,
                         iso_alpha_3_codes = meta$iso.c,
                         type_of_group = "SDG_Region")

# SDG_Sub_Region
group_data <- group_data_wpp %>%
  dplyr::select(iso_alpha_3_code, group = SDG_Sub_Region)
calculate_bmat_aggregate(round_name = round_name,
                         aggregate_group_data = group_data,
                         arr_periods = arr_periods_list,
                         iso_alpha_3_codes = meta$iso.c,
                         type_of_group = "SDG_Sub_Region")
##########################################################################################################



##########################################################################################################
##########################       World Bank      #########################################################
##########################################################################################################
group_data_wb <- readxl::read_excel(here::here("data-raw", "country_territory_data", "territory_groups_for_report", "world_bank_territory.xlsx"), skip = 0, sheet = 1)
group_data_wb <- group_data_wb %>%
  dplyr::select(iso_alpha_3_code = Code,
                region = Region,
                income_group = `Income group`) 

# World_Bank_Region
group_data <- group_data_wb %>%
  dplyr::select(iso_alpha_3_code, group = region)
calculate_bmat_aggregate(round_name = round_name,
                         aggregate_group_data = group_data,
                         arr_periods = arr_periods_list,
                         iso_alpha_3_codes = meta$iso.c,
                         type_of_group = "World_Bank_Region")

# World_Bank_Income
group_data <- group_data_wb %>%
  dplyr::select(iso_alpha_3_code, group = income_group)
calculate_bmat_aggregate(round_name = round_name,
                         aggregate_group_data = group_data,
                         arr_periods = arr_periods_list,
                         iso_alpha_3_codes = meta$iso.c,
                         type_of_group = "World_Bank_Income")
##########################################################################################################



##########################################################################################################
##########################       UNFPA      ##############################################################
##########################################################################################################
group_data_UNFPA <- readxl::read_excel(here::here("data-raw", "country_territory_data", "territory_groups_for_report", "UNFPA_territory.xlsx"), skip = 0, sheet = 1)
group_data_UNFPA <- group_data_UNFPA %>%
  dplyr::select(iso_alpha_3_code = ISOCode,
                region = `UNFPA regions`) 

# UNFPA_Region
group_data <- group_data_UNFPA %>%
  dplyr::select(iso_alpha_3_code, group = region)
calculate_bmat_aggregate(round_name = round_name,
                         aggregate_group_data = group_data,
                         arr_periods = arr_periods_list,
                         iso_alpha_3_codes = meta$iso.c,
                         type_of_group = "UNFPA_Region")
##########################################################################################################









##########################################################################################################
##########################################################################################################
##########################################################################################################
##########################################################################################################
##########################################################################################################
##########################       AD HOC FOR JC     ################################################
##########################################################################################################
##########################################################################################################
##########################################################################################################
##########################################################################################################
##########################################################################################################





##########################################################################################################
##########################       Aggregate: Example 1     ################################################
##########################################################################################################
# Start by created a data frame which represents groups of aggregation. For example, a data frame
# with a column of iso codes and a column with who regions. The data frame must always contain 
# the iso column `iso_alpha_3_code`.
group_data <- country_ref %>%
  dplyr::select(iso_alpha_3_code, group = grp_who_region)
calculate_bmat_aggregate(round_name = round_name,
                         aggregate_group_data = group_data,
                         iso_alpha_3_codes = meta$iso.c,
                         type_of_group = "who_region")
##########################################################################################################



##########################################################################################################
##########################       Aggregate: Example 2     ################################################
##########################################################################################################
# To obtain aggregate estimates at the global level i.e. the entire worlds MMR in YYYY is equal to XXX
# the group column has only a single value. In this case we give it the value "world" to convey what
# we are attempting to aggregate
group_data <- data.frame(iso_alpha_3_code = meta$iso.c, group = "world")
calculate_bmat_aggregate(round_name = round_name,
                         aggregate_group_data = group_data,
                         iso_alpha_3_codes = meta$iso.c,
                         type_of_group = "world")
##########################################################################################################



##########################################################################################################
##########################       Aggregate: Example 2     ################################################
##########################################################################################################
# Start by created a data frame which represents groups of aggregation. For example, a data frame
# with a column of iso codes and a column with SDG1 regions. The data frame must always contain 
# the iso column `iso_alpha_3_code`.
group_data <- country_ref %>%
  dplyr::select(iso_alpha_3_code, group = sdg1)
calculate_bmat_aggregate(round_name = round_name,
                         aggregate_group_data = group_data,
                         iso_alpha_3_codes = meta$iso.c,
                         type_of_group = "sdg1")
##########################################################################################################





##########################################################################################################
####################      Hack to change region codes to names in some files     #########################
##########################################################################################################
devtools::load_all()
round_name <- "estimates_08-14-22_rev_frozen_vr"

# next grab the processed data files which are in the form of .XXX in main dir
list_of_files <- list.files(here::here("output", round_name, "aggregates"), ".csv$")

country_ref <- read.csv( 
  here::here("output", round_name, "country_ref.csv"))

group_data_who <- country_ref %>%
  dplyr::select(group = grp_who_region) %>%
  dplyr::distinct() %>%
  dplyr::arrange(group) %>%
  dplyr::mutate(group_new = c("Africa", "Americas", "Eastern Mediterranean", "Europe", "South-East Asia", "Western Pacific"))
group_data_unpd <- data.frame(group = as.numeric(c("901", "902", "941", "1636", "1637" )), 
                              group_new = c("More Developed", "Less Developed", "Least Developed", "Land Locked Developing", "Small Island Developing States"))



for(filename in list_of_files) {
  df <- read.csv(here::here("output", round_name, "aggregates", filename))
  if(!"period" %in% names(df)) {
  df <- df %>%
    dplyr::filter(year_mid >= 2000)
  }
  if (any(df$group %in% group_data_who$group)) {
    df <- df %>%
      dplyr::left_join(group_data_who) %>%
      dplyr::select(-group) %>%
      dplyr::rename(group = group_new)
  }
  if (any(df$group %in% group_data_unpd$group)) {
    df <- df %>%
      dplyr::left_join(group_data_unpd) %>%
      dplyr::select(-group) %>%
      dplyr::rename(group = group_new)
      
  }
  if("X" %in% names(df)) {
    df <- df %>% dplyr::select(-X)
  }
  df <- df %>% dplyr::relocate(group)
  write.csv(df, row.names = FALSE, here::here("output", round_name, "aggregates", filename))
}
