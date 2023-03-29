##########################################################################################################
##########################       INSTRUCTIONS      #######################################################
##########################################################################################################

# There is an excel template folder, you can look for your indicator there e.g. MMR is 3.1.1, it will have an example of final output
# The steps below are how to produce that final output

# 1 we use sheet 2 of "grouping and compositions" the "lsit view" long format simple sheet of the grouping, crosswalk of mdg, iso, region
# -we used m49 region codes
# 
# 
# 2 next read in "black template" to fill in (sheet 1)
# temp <- readxl::read_excel("blank_template.xlsx", sheet = 1) %>%
#   dplyr::select("Reference Area Code (M49)",
#                 "Reference Area Type" = "Reference Area Type [for UNSD use only]",
#                 "Reference Area Name")
# temp2 <- temp %>%
#   dplyr::filter(`Reference Area Type` == "3.0-Country") 
# 
# 
# 3 USE A SEVER obtain aggregate estimates based on the grouping in sheet 1.
# 
# 
# 4 manipulation both aggregate world, aggregates regions, and country estimates into the format for the above sheet e.g.
# 
# estimates2 <- estimates %>%
#   dplyr::rename(`Reference Area Code (M49)` = m49_r,
#                 `Time Period` = year,
#                 `Disaggregation classification` = quantiles,
#                 `Observation Value` = mmr) %>%
#   dplyr::mutate(`Time Detail` = "Calendar year",
#                 `Nature of Data` = "E",
#                 `Unit of Measurement` = "Maternal Mortality per 100,000 live births",
#                 `Reporting Type` = "G",
#                 Source = "UNMMEIG",
#                 `Disaggregation classification` = ifelse(`Disaggregation classification` == 0.1, "Uncertainty bounds: lower bound",
#                                                          ifelse(`Disaggregation classification` == 0.5, "Uncertainty bounds: mid-point",
#                                                                 "Uncertainty bounds: upper bound")
#                                                          
#                                                          
#                                                          estiamtes_blank <- estimates %>%
#                                                            dplyr::filter(`Reference Area Code (M49)` == 1) %>%
#                                                            dplyr::mutate(`Observation Value` = NA,
#                                                                          `Reference Area Code (M49)` = NA)
#                                                          
#                                                          
#                                                          4 finally left join estimates into the blank template after all the tedious manipulation


##########################################################################################################
##########################       step 1     ############################################################
##########################################################################################################
devtools::load_all()
crosswalk <- readxl::read_excel(here::here("vignettes/internal/aggregate_sdg_report/region_groupings_composition.xlsx"), sheet = 2)
crosswalk <- crosswalk %>%
  dplyr::select(
    "M49 Code",                                                             
    "ISO Code",                                                                 
    "M49 Code(region)"
  )
##########################################################################################################


  
##########################################################################################################
##########################       step 2     ############################################################
##########################################################################################################
template <- readxl::read_excel(here::here("vignettes/internal/aggregate_sdg_report/blank_data_template.xlsx"), sheet = 1)  %>%
    dplyr::filter(`Time Period` == 2000) %>%
    dplyr::select("Reference Area Code (M49)",
                  "Reference Area Type" = "Reference Area Type [for UNSD use only]",
                  "Reference Area Name")
temp2 <- template %>%
  dplyr::filter(`Reference Area Type` == "3.0-Country")
##########################################################################################################



##########################################################################################################
##########################       step 3 (SERVER COMPUTING)    ##########################################
##########################################################################################################
devtools::load_all()
round_name <- "estimates_12-19-22"
round_first_year <- 1985
round_last_year <- 2020
arr_periods_list <- list(c(2000,2015), c(2000, 2020), c(2010, 2020), c(2000, 2005), c(2005, 2010), c(2010, 2015), c(2015, 2020), c(2016, 2020))
country_ref <- read.csv( 
  here::here("output", round_name, "country_ref.csv"))
meta <-  readRDS(
  here::here("output", round_name, "meta.rds")
)
group_data <- crosswalk  %>%
  dplyr::select(group = "M49 Code(region)", iso_alpha_3_code = `ISO Code`)
calculate_bmat_aggregate(round_name = round_name,
                         aggregate_group_data = group_data,
                         iso_alpha_3_codes = meta$iso.c,
                         type_of_group = "sdg_m49")
##########################################################################################################




##########################################################################################################
##########################       step 4     ############################################################
##########################################################################################################
# world and regional
aggregates <- read.csv(here::here("output", round_name, "aggregate_estimates_rt_sdg_m49.csv"))
aggregates <- aggregates %>%
  dplyr::filter(parameter == "mmr") %>%
  dplyr::rename("Reference Area Code (M49)" = group,
                `Time Detail` = year_mid,
                `Value` = "X0.5",
                `UpperBound` = "X0.1",
                `LowerBound` = "X0.9",) %>%
  dplyr::mutate(`Indicator` = "3.1.1",
                `SeriesID` = "2870",
                `SeriesDescription` = "Maternal Mortality Ratio",
                `Nature of Data` = "E",
                `Sex` = "FEMALE",
                `Unit of Measurement` = "PER_100000_LIVE_BIRTHS",
                `Reporting Type` = "G",
                `Source` = "UNMMEIG",
                `Series Code` = "SH_STA_MORT") %>%
  dplyr::select(-X) %>%
  dplyr::select(-parameter)


aggregates2 <- aggregates %>%
  dplyr::left_join(template, by = "Reference Area Code (M49)")
#country level
estimates <- read.csv(here::here("output", round_name, "estimates.csv"))%>%
  dplyr::filter(parameter == "mmr") %>%
  dplyr::rename(`ISO Code` = iso_alpha_3_code,
                `Time Detail` = year_mid,
                `Value` = "X0.5",
                `UpperBound` = "X0.1",
                `LowerBound` = "X0.9",) %>%
  dplyr::mutate(`Indicator` = "3.1.1",
                `SeriesID` = "2870",
                `SeriesDescription` = "Maternal Mortality Ratio",
                `Nature of Data` = "E",
                `Sex` = "FEMALE",
                `Unit of Measurement` = "PER_100000_LIVE_BIRTHS",
                `Reporting Type` = "G",
                `Source` = "UNMMEIG",
                `Series Code` = "SH_STA_MORT") %>%
  dplyr::select(-parameter)
estimates <- estimates %>%
  dplyr::left_join(crosswalk %>%
                     dplyr::filter(`M49 Code(region)` == 1) %>% 
                     dplyr::rename("Reference Area Code (M49)" = "M49 Code") %>%
                     dplyr::select("Reference Area Code (M49)", "ISO Code"), 
                   by = c("ISO Code")) %>%
  dplyr::select(-"ISO Code")
estimates2 <- estimates %>%
  dplyr::left_join(template, by = "Reference Area Code (M49)")

final <- dplyr::bind_rows(estimates2, aggregates2)
write.csv(final, row.names = FALSE, "indicator_3.1.1_01-30-23")







