

##########################################################################################################
##########################       Settings     ############################################################
##########################################################################################################
# The round_name variable is used to reference the directory in which the processed data 
# from script 1 was saved. We will also save model results in this directory. The server 
# variable should stay set to FALSE unless you are running this code on a server.
devtools::load_all()
round_name <- "estimates_08-14-22_frozen_vr_rev"
round_first_year <- 1985
round_last_year <- 2020
##########################################################################################################



##########################################################################################################
##########################       Read in data     ########################################################
##########################################################################################################
# Read in data and store them in objects. These objects will be provided to 
# wrapper functions which run the models.
ssdata_w_frz <- read.csv(here::here("output", round_name, "ssdata.csv"))
###################


##########################################################################################################
##########################       Settings     ############################################################
##########################################################################################################
# The round_name variable is used to reference the directory in which the processed data 
# from script 1 was saved. We will also save model results in this directory. The server 
# variable should stay set to FALSE unless you are running this code on a server.
devtools::load_all()
round_name <- "estimates_08-14-22_rev_ecu"
round_first_year <- 1985
round_last_year <- 2020
##########################################################################################################



##########################################################################################################
##########################       Read in data     ########################################################
##########################################################################################################
# Read in data and store them in objects. These objects will be provided to 
# wrapper functions which run the models.
ssdata <- read.csv(here::here("output", round_name, "ssdata.csv"))
###################
ssdata_w_frz <- ssdata_w_frz %>%
  dplyr::select(truemat_frz = truemat, 
                truemat_vr_frz = truemat_vr, 
                final_pm_frz = final_pm, 
                iso_alpha_3_code, 
                year_start, 
                year_end, 
                citation_long, 
                include_bmat_frz = include_bmat, 
                include_bmis_frz = include_bmis,
                tpf = tp,
                tnf = tn,
                fpf = fp,
                fnf = fn)
temp <- ssdata %>%
  dplyr::left_join(ssdata_w_frz, by = c("iso_alpha_3_code", "year_start", "year_end", "citation_long"))
temp2 <- temp %>%
  dplyr::mutate(dif = truemat != truemat_frz | truemat_vr != truemat_vr_frz) %>%
  dplyr::filter(dif) %>%
  dplyr::select(iso_alpha_3_code, year_start, year_end, truemat, truemat_frz, truemat_vr, truemat_vr_frz, final_pm)

# we need to detect na pm? or no because of inclusion

# values differ
temp2 <- temp %>%
  dplyr::filter(final_pm != final_pm_frz) %>%
  dplyr::select(iso_alpha_3_code, year_start, year_end, truemat, truemat_frz, truemat_vr, truemat_vr_frz, final_pm, final_pm_frz, include_bmat, include_bmat_frz) %>%
  dplyr::mutate(perc_diff_in_pm = (final_pm-final_pm_frz)/final_pm_frz*100)

# include bmat differs
temp3 <- temp %>%
  dplyr::filter(include_bmat != include_bmat_frz) %>%
  dplyr::select(iso_alpha_3_code, year_start, year_end, truemat, truemat_frz, truemat_vr, truemat_vr_frz, final_pm, final_pm_frz, include_bmat, include_bmat_frz) %>%
  dplyr::mutate(perc_diff_in_pm = (final_pm-final_pm_frz)/final_pm_frz*100)

temp3 <- rbind(temp2, temp3) 


#all above and boxes
tempbmis <- temp %>%
  dplyr::filter(
                  include_bmis != include_bmis_frz |
                  tp != tpf |
                  tnf != tn |
                  fpf != fp |
                  fnf != fn) %>%
  dplyr::select(iso_alpha_3_code, year_start, year_end, include_bmis_frz, include_bmis,
                  tp,tpf,
                  tnf, tn,
                  fpf, fp,
                  fnf, fn) 

round_name <- "estimates_08-14-22_frozen_vr_rev"
write.csv(temp3, row.names = FALSE, here::here("output",round_name, "difference_between_study_mat_deaths_from_frozen_vr.csv"))
write.csv(tempbmis, row.names = FALSE, here::here("output",round_name, "difference_between_bmis_from_frozen_vr.csv"))

c(tempbmis$iso_alpha_3_code %>% unique, temp3$iso_alpha_3_code %>% unique) %>% unique
tempbmis$iso_alpha_3_code %>% unique
temp3$iso_alpha_3_code %>% unique
