#re do one country runs (only for countries with crisis in 2020)

devtools::load_all()
round_name <- "estimates_08-14-22_rev_frozen_vr"
round_name_2 <- "estimates_08-14-22_factual_no_covid_in_crisis_def"
round_name_3 <- "estimates_08-14-22_counterfactual"

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
isos_covidcrisis <- meta$iso.c[crisis_c]
temp <- main_data %>%
  # dplyr::filter(iso_alpha_3_code %in% isos) %>%
  dplyr::filter(year_start == 2020)
isos <- temp$iso_alpha_3_code %>% unique
devtools::load_all()

pl <- list()
for (iso_alpha_3_code in isos) {
  pl[[iso_alpha_3_code]] <- bmat::plot_bmat_onecountry3(
    round_name = round_name,
    round_name_2 = round_name_2,
    round_name_3 = round_name_3,
    iso_alpha_3_code = iso_alpha_3_code,
    year_range = c(2015,2020),
    caption = ""
  )
}
pdf(here::here("output", round_name_3, "comparison_plots.pdf"), 16,8)
pl
dev.off()


pl <- list()
for (iso_alpha_3_code in isos_covidcrisis) {
  pl[[iso_alpha_3_code]] <- bmat::plot_bmat_onecountry3(
    round_name = round_name,
    round_name_2 = round_name_2,
    round_name_3 = round_name_3,
    iso_alpha_3_code = iso_alpha_3_code,
    year_range = c(2015,2020),
    caption = ""
  )
}
pdf(here::here("output", round_name_3, "comparison_isos_covidcrisis_plots.pdf"), 16,8)
pl
dev.off()