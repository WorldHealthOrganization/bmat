##########################################################################################################
##########################       Settings     ############################################################
##########################################################################################################
devtools::load_all()
round_name <- "estimates_08-14-22_rev_frozen_vr"
round_first_year <- 1985
round_last_year <- 2020
##########################################################################################################



##########################################################################################################
##########################       input data     ########################################################
##########################################################################################################
ssdata <- read.csv(here::here("output", round_name, "ssdata.csv"))
vrdata <- read.csv(here::here("output", round_name, "vrdata.csv"))
vrdata_w_frozen <- read.csv(here::here("output", round_name, "vrdata_w_frozen.csv"))
meta <-  readRDS(here::here("output", round_name, "meta.rds"))
meta_precrisis <-  readRDS(here::here("output", round_name, "meta_precrisis.rds"))
census <- read.csv(here::here("output", round_name, "census.csv"))
survey <- read.csv(here::here("output", round_name, "survey.csv"))
miscellaneous <- read.csv(here::here("output", round_name, "miscellaneous.csv"))
##########################################################################################################



##########################################################################################################
##########################       estimate data     ########################################################
##########################################################################################################
estimates <- read.csv(here::here("output", round_name, "estimates.csv"))
aggregate_arr_who <- read.csv(here::here("output", round_name, "aggregates", "aggregate_arr_r_who_region.csv"))
aggregate_estimate_who <- read.csv(here::here("output", round_name, "aggregates", "aggregate_estimates_rt_who_region.csv"))
##########################################################################################################



##########################################################################################################
##########################       aux data     ########################################################
##########################################################################################################
group_data_wb <- readxl::read_excel(here::here("data-raw", "country_territory_data", "territory_groups_for_report", "world_bank_territory.xlsx"), skip = 0, sheet = 1)
group_data_wb <- group_data_wb %>%
  dplyr::select(iso_alpha_3_code = Code,
                income_group = `Income group`) %>%
  tidyr::drop_na()

group_data_fsi <- readxl::read_excel(here::here("data-raw", "country_territory_data", "territory_by_fsi", "fsi_edited.xlsx"))
group_data_fsi <- group_data_fsi %>%
  dplyr::mutate(group = ifelse(Total > 100, "Very high alert & High alert",
                               ifelse(Total > 90 & Total <= 100, "Alert",
                                      ifelse(Total > 80 & Total <= 90, "High warning",
                                             ifelse(Total > 70 & Total <= 80, "Elevated warning",
                                                    ifelse(Total > 60 & Total < 70, "Warning", "Stable")))))) %>%
  dplyr::select(group, iso_alpha_3_code)  %>%
  tidyr::drop_na()
##########################################################################################################



# check nearly all maternal deaths ~99% take place in LMICs
#-bind lmic map onto estimates.csv, 
#-get the % over time using estimated mat deaths
temp <- estimates %>%
  dplyr::filter(parameter == "maternal_deaths") %>%
  dplyr::left_join(group_data_wb, by = c("iso_alpha_3_code")) %>%
  tidyr::drop_na() %>%
  dplyr::group_by(year_mid) %>%
  dplyr::mutate(maternal_deaths_tot_annual = sum(X0.5)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(year_mid, income_group) %>%
  dplyr::summarise(maternal_deaths = sum(X0.5),
                   percent = 100*(sum(X0.5)/maternal_deaths_tot_annual[1]),
                   n = dplyr::n())
write.csv(temp, row.names = FALSE, here::here("vignettes", "internal", "communication_project", "perc_mat_by_income.csv"))
# based on our model it is around 95% estimated maternal deaths in lower/lower-mid income countries 99% if you include upper-middle 


  
#trends have stalled
#-arr 5 year periods for each region and world
#-mmr/pm aggregate region and world estimates over time
periods <- aggregate_arr_who$period %>% unique %>% .[3:6]
temp <- aggregate_arr_who %>%
  dplyr::filter(period %in% !!periods)
library(ggplot2)
library(ggrepel)
wrapper <- function(x, ...) 
{
  paste(strwrap(x, ...), collapse = "\n")
}
my_caption <- "Source: WHO analysis based on the UN Maternal Mortality Estimation Inter-argency Group (MMEIG) estimates `Trends in maternal mortality 2000 to 2020: estimates by WHO, UNICEF, UNFPA, World Bank Group and the United Nations Population Division`"
pl <- temp %>% 
  dplyr::filter(parameter == "arr") %>%
  ggplot() +
  geom_point(aes(x = period, y = `X0.5`,  col = as.factor(group)), alpha = .2, size = 3) +
  geom_errorbar(aes(x = period, ymin=`X0.1`, ymax=`X0.9`, col = as.factor(group)), width=0, alpha = .2, size = 2)  +
  geom_label_repel(aes(x = period, y = `X0.5`, label = group),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50',
                   size = 3) +
  labs(col = "WHO region") +
  labs(y = "ARR")+
  ggplot2::theme_classic() +
  ggplot2::theme(plot.caption = ggplot2::element_text(vjust= -2.5, hjust = 0,face = "italic")) +
  ggplot2::labs(caption = wrapper(my_caption, width = 80)) 
pl

pdf(here::here("vignettes", "internal", "communication_project", "arr.pdf"), 12,10)
pl
dev.off()
# temp %>% 
#   dplyr::filter(parameter == "arr") %>%
#   ggplot(aes(x = period, y = `X0.5`,  col = as.factor(group)), alpha = .2, size = 2) +
#   geom_pointrange(aes(ymin=`X0.1`, ymax=`X0.9`), position=position_jitter(width=0.2))  +
#   labs(col = "WHO region") +
#   labs(y = "ARR")+
#   ggplot2::theme_classic() +
#   ggplot2::theme(plot.caption = ggplot2::element_text(vjust= -2.5, hjust = 0,face = "italic")) +
#   ggplot2::labs(caption = wrapper(my_caption, width = 80))



# widening gap for frag states and LMIC
#-aggregates
#-fragile state categories
#-LMIC categories (should already be in aggregates for report)

