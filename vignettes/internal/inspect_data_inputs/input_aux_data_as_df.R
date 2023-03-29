
##########################################################################################################
##########################       Settings     ############################################################
##########################################################################################################
# The round_name variable is used to reference the directory in which the processed data 
# from script 1 was saved. We will also save model results in this directory. The server 
# variable should stay set to FALSE unless you are running this code on a server.
devtools::load_all()
round_name <- "estimates_12-19-22"
round_first_year <- 1985
round_last_year <- 2020
run_length <- "long_local"
server <- FALSE
##########################################################################################################
meta <-  readRDS(here::here("output", round_name, "meta.rds"))

#############
# death.ct from meta raw? (raw)
# deaths_to_denom_hiv.ct (raw)
# births.ct raw? (raw)
# hiv (raw) (note denominated with historic set of deaths)
# loggdp used from meta raw? (use logGDPnotsmooth.ct instead of logGDP.ct)
# gfr.ct raw? (raw)
# sab

metanew <- meta
matrix_ct_to_df_custom <- function(metanew, value_matrixnew, name) {
  dfnew <- matrix_ct_to_df_with_isoyear(metanew, value_matrix= value_matrixnew, value_name = name)  %>%
    dplyr::filter(year > 1999) %>%
    dplyr::select(-t) %>%
    dplyr::select(-c)
  return(dfnew)
}


matrix_ct_to_df <- function(matrix, C, value_name) {
  df <- matrix %>%
    as.data.frame() %>%
    dplyr::mutate(c = 1:C) %>%
    tidyr::gather("t", {{value_name}}, -c) %>%
    dplyr::mutate(t = t %>% stringr::str_extract_all("[0-9]+") %>% as.numeric)
  return(df)
}
matrix_ct_to_df_with_isoyear <- function(meta, value_matrix, value_name = "value") {
  matrix <- matrix_ct_to_df(matrix = value_matrix, C = meta$C, value_name = !!value_name)
  isos <- meta$iso.c
  years <- meta$year.t
  iso_df <- data.frame(c = 1:length(isos), iso_alpha_3_code = isos)
  years_df <- data.frame(t = 1:length(years), year = years)
  value_df <- matrix %>%
    dplyr::left_join(years_df, by = c("t")) %>%
    dplyr::left_join(iso_df, by = c("c")) %>%
    dplyr::mutate(iso_alpha_3_code = as.character(iso_alpha_3_code))
}
# all_cause_death_f_1549 <- matrix_ct_to_df_custom(metanew = meta_precrisis,
#                                                  value_matrixnew = meta_precrisis$deaths.ct,
#                                                  name = "all_cause_death_f_1549")
all_cause_death_f_1549_incl_crisis <- matrix_ct_to_df_custom(metanew = metanew,
                                                 value_matrixnew = metanew$deaths_incl_crisis.ct,
                                                 name = "all_cause_death_f_1549_incl_crisis")

excess_covid <- matrix_ct_to_df_custom(metanew = metanew,
                                                             value_matrixnew = metanew$crisisdeaths_covid.ct,
                                                             name = "excess_covid")

all_cause_death_f_1549_w_crisis_adj <- matrix_ct_to_df_custom(metanew = metanew,
                                   value_matrixnew = metanew$deaths.ct,
                                   name = "all_cause_death_f_1549_w_crisis_adj")
all_cause_death_f_1549_historic_to_denom_hiv <- matrix_ct_to_df_custom(metanew = metanew,
                                   value_matrixnew = metanew$deaths_to_denom_hiv.ct,
                                   name = "all_cause_death_f_1549_historic_to_denom_hiv")
live_births <- matrix_ct_to_df_custom(metanew = metanew,
                              value_matrixnew = metanew$births.ct,
                              "live_births")
hiv_deaths <- matrix_ct_to_df_custom(metanew = metanew,
                              value_matrixnew = metanew$hiv.ct,
                              "hiv_deaths")
gdp <- matrix_ct_to_df_custom(metanew = metanew,
                             value_matrixnew = exp(metanew$logGDPnotsmooth.ct),
                             "gdp") 
gfr <- matrix_ct_to_df_custom(metanew = metanew,
                              value_matrixnew = metanew$gfr.ct,
                              "gfr") 
sab <- matrix_ct_to_df_custom(metanew = metanew,
                              value_matrixnew = metanew$sab.ct,
                              "sab")
  

  
meta_df <- all_cause_death_f_1549_incl_crisis %>% 
  dplyr::left_join(all_cause_death_f_1549_w_crisis_adj) %>% 
  dplyr::left_join(all_cause_death_f_1549_historic_to_denom_hiv) %>% 
  dplyr::left_join(live_births) %>% 
  dplyr::left_join(hiv_deaths) %>%
  dplyr::left_join(gdp) %>%
  dplyr::left_join(gfr) %>%
  dplyr::left_join(sab) %>%
  dplyr::relocate(iso_alpha_3_code, year)
temp <- meta_df %>%
  dplyr::filter(iso_alpha_3_code == "ARE")

write.csv(meta_df, row.names = FALSE, "input_aux_data.csv")
write.csv(temp, row.names = FALSE, "input_aux_data_ARE.csv")
