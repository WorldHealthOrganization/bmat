compare_input_data_c_wrapper <- function(
    metanew,
    metaold
) {
  

  gdp<- compare_input_data_c(metanew = metanew,
                             metaold = metaold,
                             value_matrixnew = exp(metanew$logGDP.ct),
                             value_matrixold = exp(metaold$logGDP.ct)) 
  
  
  sab <- compare_input_data_c(metanew = metanew,
                              metaold = metaold,
                              value_matrixnew = metanew$sab.ct,
                              value_matrixold = metaold$sab.ct)
  

  gfr <- compare_input_data_c(metanew = metanew,
                              metaold = metaold,
                              value_matrixnew = metanew$gfr.ct,
                              value_matrixold = metaold$gfr.ct) 
  

  alldeath <- compare_input_data_c(metanew = metanew,
                                   metaold = metaold,
                                   value_matrixnew = metanew$deaths.ct,
                                   value_matrixold = metaold$deaths.ct)
  
  alldeath_w_crisis <- compare_input_data_c(metanew = metanew,
                                   metaold = metaold,
                                   value_matrixnew = metanew$deaths_incl_crisis.ct,
                                   value_matrixold = metaold$deaths_incl_crisis.ct)

  
  dl <- list(
    gdp = gdp,
    sab = sab,
    gfr = gfr,
    alldeath = alldeath,
    alldeath_w_crisis = alldeath_w_crisis)
}


devtools::load_all()
library(dplyr)
round_name <- "estimates_08-14-22_rev_frozen_vr"
#use meta_m60.rds to compare to new meta need to filter on the right Cs
metaold <- readRDS(here::here("bmat_m60", "meta_m60.rds"))
metanew <-  readRDS(here::here("output", round_name, "meta.rds"))
country_ref <- read.csv( 
  here::here("output", round_name, "country_ref.csv"))


temp <- compare_input_data_c_wrapper(
  metanew = metanew,
  metaold = metaold
)


mortality_ghe2019 <- read.csv(here::here("data-raw","auxiliary_data", "archive", "mortality_GHE2021.csv")) %>%
  dplyr::select(year,
                iso_alpha_3_code = iso3,
                sex,
                age,
                deaths,
                deaths_crisis = crisis_deaths) %>%
  dplyr::filter(age %in% seq(15,45,5)) %>%
  dplyr::filter(sex == 2) %>%
  dplyr::group_by(iso_alpha_3_code, year, sex) %>%
  dplyr::summarise(femaledeaths_ghe2019 = sum(deaths),
                   femaledeaths_crisis_ghe2019 = sum(deaths_crisis))


temp$alldeath <- temp$alldeath %>%
  dplyr::rename(femaledeaths_wpp2019 = value_new, femaledeaths_la2019 = value_old) %>%
  dplyr::left_join(mortality_ghe2019 %>% dplyr::select(-femaledeaths_crisis_ghe2019), by = c("iso_alpha_3_code", "year"))

library(ggplot2)
# library(gridExtra)
library(ggpubr)
meta <-  readRDS(here::here("output", round_name, "meta.rds"))
iso_alpha_3_codes <- meta$iso.c
gdp <- list()
sab <- list()
gfr <- list()
death <- list()
death_w_crisis <- list()
crisis <- list()
pl <- list()
temp2 <- temp$alldeath %>% 
  dplyr::rename(value_new = femaledeaths_wpp2019,
                value_old = femaledeaths_la2019)
for(iso_alpha_3_code in iso_alpha_3_codes) {
  gdp[[iso_alpha_3_code]] <- ggplot(temp$gdp %>% dplyr::filter(iso_alpha_3_code == !!iso_alpha_3_code)) +
    geom_line(aes(x = year, y = value_new, color = "new")) +
    geom_line(aes(x = year, y = value_old, color = "old")) +
    ggtitle("gdp")
  sab[[iso_alpha_3_code]] <- ggplot(temp$sab %>% dplyr::filter(iso_alpha_3_code == !!iso_alpha_3_code)) +
    geom_line(aes(x = year, y = value_new, color = "new")) +
    geom_line(aes(x = year, y = value_old, color = "old")) +
    ggtitle("sab")
  gfr[[iso_alpha_3_code]] <- ggplot(temp$gfr %>% dplyr::filter(iso_alpha_3_code == !!iso_alpha_3_code)) +
    geom_line(aes(x = year, y = value_new, color = "new")) +
    geom_line(aes(x = year, y = value_old, color = "old")) +
    ggtitle("gfr")
  death[[iso_alpha_3_code]] <- ggplot(temp2 %>%
                                        dplyr::filter(iso_alpha_3_code == !!iso_alpha_3_code)) +
    geom_line(aes(x = year, y = value_new, color = "new")) +
    geom_line(aes(x = year, y = value_old, color = "old")) +
    ggtitle("death")
  death_w_crisis[[iso_alpha_3_code]] <- ggplot(temp$alldeath_w_crisis %>%
                                        dplyr::filter(iso_alpha_3_code == !!iso_alpha_3_code)) +
    geom_line(aes(x = year, y = value_new, color = "new")) +
    geom_line(aes(x = year, y = value_old, color = "old")) +
    ggtitle("death_w_crisis")
  pl[[iso_alpha_3_code]] <- ggarrange(gdp[[iso_alpha_3_code]],
            sab[[iso_alpha_3_code]],
            gfr[[iso_alpha_3_code]],
            death[[iso_alpha_3_code]],
            death_w_crisis[[iso_alpha_3_code]],
            ncol = 2, nrow = 3,  common.legend = TRUE, legend="bottom")
  pl[[iso_alpha_3_code]] <- pl[[iso_alpha_3_code]] %>% annotate_figure(top = text_grob(iso_alpha_3_code, 
                                           color = "black", face = "bold", size = 14))
}

pdf(here::here("output", round_name, "plot_comparison_inputs.pdf"), 16,8)
pl
dev.off()








vrdata <- read.csv(here::here("output", round_name, "vrdata.csv"))
crisis <- read.csv(
  here::here("data-raw", "auxiliary_data", "mortality_WPP_2022.06.21.csv")) %>%
  dplyr::select(iso_alpha_3_code = ISO3_code,
                age = Age,
                year = Year,
                crisis_deaths_OtherCrises_wpp2019 = crisis_deaths_OtherCrises,
                crisis_deaths_COVID19_wpp2019 = crisis_deaths_COVID19) %>%
  dplyr::mutate(crisis_deaths_total_wpp2019 = crisis_deaths_OtherCrises_wpp2019 + crisis_deaths_COVID19_wpp2019) %>%
  dplyr::mutate(crisis_deaths_total_wpp2019 = ifelse(crisis_deaths_total_wpp2019 < 0, 0, crisis_deaths_total_wpp2019)) %>%
  dplyr::mutate(crisis_deaths_COVID19_wpp2019 = ifelse(crisis_deaths_COVID19_wpp2019 < 0, 0, crisis_deaths_COVID19_wpp2019)) %>%
  dplyr::mutate(crisis_deaths_OtherCrises_wpp2019 = ifelse(crisis_deaths_OtherCrises_wpp2019 < 0, 0, crisis_deaths_OtherCrises_wpp2019)) %>%
  dplyr::group_by(year, iso_alpha_3_code) %>%
  dplyr::summarise(crisis_deaths_total_wpp2019 = sum(crisis_deaths_total_wpp2019),
                   crisis_deaths_OtherCrises_wpp2019 = sum(crisis_deaths_OtherCrises_wpp2019),
                   crisis_deaths_COVID19_wpp2019 = sum(crisis_deaths_COVID19_wpp2019)) %>%
  dplyr::filter(year > 1984) %>%
  dplyr::filter(iso_alpha_3_code %in% metanew$iso.c)

crisis_ghe2019 <- mortality_ghe2019 %>%
  dplyr::select(iso_alpha_3_code, year, crisis_deaths_ghe2019 = femaledeaths_crisis_ghe2019)
crisis_la2019 <- readstata13::read.dta13(here::here("data-raw", "auxiliary_data", "archive", "lt_mmr_30apr.dta")) %>%
  dplyr::select(year,
                iso_alpha_3_code = iso3,
                sex,
                age = x,
                crisis_deaths_la2019 = crisisdeaths) %>%
    dplyr::filter(age %in% seq(15,45,5)) %>%
    dplyr::filter(sex == 2) %>%
    dplyr::filter(year <= 2019) %>%
    dplyr::group_by(year, iso_alpha_3_code) %>%
    dplyr::summarise(crisis_deaths_la2019 = sum(crisis_deaths_la2019))

dfc <- temp$alldeath %>%
  dplyr::select(-change)%>%
  dplyr::select(-change_abs)%>%
  dplyr::select(-sex) %>%  
  dplyr::left_join(crisis) %>%
  dplyr::left_join(crisis_ghe2019) %>%
  dplyr::left_join(crisis_la2019) %>%
  dplyr::mutate(crisis_proportion_wpp2019 = crisis_deaths_total_wpp2019/femaledeaths_wpp2019) %>%
  dplyr::left_join(vrdata %>%
                     dplyr::select(iso_alpha_3_code,
                                   year = year_start,
                                   femaledeaths_vr = obs_env,
                                   ill_defined_death_count_vr = obs_ill,
                                   completeness_for_usability_calc = rho_bmat,
                                   ill_defined_death_proportion_for_usability_calc = ill_defined_death_proportion,
                                   usability_used_in_2022_model = usability_percentage)) %>%
  dplyr::mutate(`completeness_femaledeaths_vr/la2019` = femaledeaths_vr/femaledeaths_la2019) %>%
  dplyr::mutate(`completeness_femaledeaths_vr/ghe2019` = femaledeaths_vr/femaledeaths_ghe2019) %>%
  dplyr::mutate(`completeness_femaledeaths_vr/wpp2019` = femaledeaths_vr/femaledeaths_wpp2019) %>%
  dplyr::relocate(iso_alpha_3_code, year, femaledeaths_la2019, femaledeaths_ghe2019, femaledeaths_wpp2019) %>%
  dplyr::filter(year > 1999)



# custom JC request
iso <- "ARE"
temp5 <- dfc %>%
  dplyr::filter(iso_alpha_3_code == iso)
write.csv(temp5, row.names = FALSE, here::here("output", round_name, paste0(iso, "_data_for_consult.csv")))
pdf(here::here("output", round_name, paste0(iso, "_plot_comparison_inputs.pdf")), 16,8)
pl[[iso]]
dev.off()
