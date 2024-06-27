


##########################################################################################################
##########################       Summary      ############################################################
##########################################################################################################
# This script produces aggregate estimates based on one country runs.
##########################################################################################################



##########################################################################################################
##########################       Settings     ############################################################
##########################################################################################################
devtools::load_all()
round_name <- "test"
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
