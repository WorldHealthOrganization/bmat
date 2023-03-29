
process_and_estimate_sab <- function(
  country_ref,
  sab,
  years,
  year_reference,
  round_name
) {


isos_arranged <- data.frame(iso = country_ref$iso_alpha_3_code) %>% 
  dplyr::arrange(iso) %>% 
  dplyr::pull(iso)

#MDV is na
# JPN has an erroneous value of .99 instead of 99
geo <- country_ref %>% dplyr::select(iso_alpha_3_code, sdg1)

sab <- dplyr::full_join(sab, geo) %>%
  dplyr::select(iso_alpha_3_code, sdg1, year_coverage_start, year_coverage_end, value) %>%
  tidyr::drop_na() %>%
  dplyr::mutate(year_coverage_start = as.numeric(year_coverage_start)) %>%
  dplyr::mutate(year_coverage_end = as.numeric(year_coverage_end)) %>%
  dplyr::mutate(value = as.numeric(value)) %>%
  dplyr::mutate(year = ifelse(year_coverage_start==year_coverage_end, year_coverage_start+.5,
                              (year_coverage_end + year_coverage_start)/2)) %>%
  tidyr::drop_na(value) %>%
  dplyr::filter(year < 3000) 

# logit regression model criteria: countries with any value of SAB < 95% *AND* 4 or more observations
# mlm criteria: at least one observation
sab <- sab %>%
  dplyr::group_by(iso_alpha_3_code) %>%
  dplyr::mutate(criteria1 = any(value < 95) &  dplyr::n() >= 4)  %>%
  dplyr::mutate(criteria1 = ifelse(is.na(criteria1), FALSE, criteria1)) %>%
  dplyr::mutate(criteria2 = dplyr::n() >= 1 & !criteria1)  %>%
  dplyr::mutate(criteria2 = ifelse(is.na(criteria2), FALSE, criteria2)) %>%
  dplyr::ungroup()

# mutate to [0,1) 
sab <- sab %>%
  dplyr::mutate(value = value/100) %>%
  dplyr::mutate(value = ifelse(value == 1, .999, value)) %>%
  dplyr::mutate(t = year - year_reference)

sab <- sab %>% 
  dplyr::ungroup() %>% 
  dplyr::select(iso_alpha_3_code, year, t, value, criteria1, criteria2) %>%
  dplyr::left_join(geo, by = c("iso_alpha_3_code"))

sab_criteria <- sab %>%
  dplyr::group_by(iso_alpha_3_code) %>%
  dplyr::summarise(criteria1 = any(criteria1),
                   criteria2 = any(criteria2))  

# logit functions
antilogit <- function(p){exp(p)/(1+exp(p))}
logit <- function(p){log(p/(1-p))}


sab_complete_empty <- geo %>%
  dplyr::mutate(year = 1980.5) %>%
  tidyr::complete(tidyr::nesting(iso_alpha_3_code, sdg1),
                  year = years)

sab_complete_empty <- dplyr::left_join(sab_complete_empty, sab_criteria) 
sab_complete_empty <- sab_complete_empty %>%
  dplyr::mutate(criteria1 = ifelse(is.na(criteria1), FALSE, criteria1)) %>%
  dplyr::mutate(criteria2 = ifelse(is.na(criteria2), FALSE, criteria2)) %>%
  dplyr::mutate(t = year - year_reference)


# mixed ef model
mod <- lme4::lmer(logit(value) ~ 1 + t + (1 + t | iso_alpha_3_code) + (1 + t | sdg1), data=sab) 
# GG what about having time trend only by country e.g.
# mod2 <- lme4::lmer(logit(value) ~ 1 + t + (1 | sdg1/iso_alpha_3_code) + (t | iso_alpha_3_code), data=sab_complete_empty)


# pulling the predicted coefficients from lmer and making columns with them
#fixed effects (intercepts)
intercept_global <- lme4::fixef(mod)[["(Intercept)"]]
t_global <- lme4::fixef(mod)[["t"]]
#random effects
iso_specific <- lme4::ranef(mod)[["iso_alpha_3_code"]] %>% 
  tibble::rownames_to_column() %>% 
  dplyr::select(iso_alpha_3_code = rowname, intercept_iso = `(Intercept)`, t_iso = t)
region_specific <- lme4::ranef(mod)[["sdg1"]]  %>% 
  tibble::rownames_to_column() %>% 
  dplyr::select(sdg1 = rowname, intercept_region = `(Intercept)`, t_region = t)



# join on the coefficient estimated B0 and B1 and calculate value estimates
sab_complete <- sab_complete_empty %>%
  dplyr::left_join(iso_specific) %>%
  dplyr::left_join(region_specific) %>%
  dplyr::mutate(intercept_global = intercept_global,
                t_global = t_global) %>%
  # value used for criteria 1 countries
  dplyr::mutate(value_ranef_model = 
                  antilogit(intercept_global + intercept_region + intercept_iso + t*(t_global + t_region + t_iso))
  ) %>%
  # value used for countries who do not fit criteria 1 or 2 (zero observed values)
  dplyr::mutate(value_ranef_model_regional = 
                  antilogit(intercept_global + intercept_region + t*(t_global + t_region))
  ) %>%
  # value used for criteria 2 countries ...
  # ...
  # imputing values
  dplyr::mutate(value_estimated = ifelse(!criteria1 & !criteria2, value_ranef_model_regional, value_ranef_model)) %>%
  dplyr::select(iso_alpha_3_code, year, t, criteria1, criteria2, value_estimated)


# Single glm for each country with one observation less than 95% and at least 4 observations
t <- years - year_reference
sab_l_i <- list()
for (iso_alpha_3_code in sab$iso_alpha_3_code %>% unique) {
  sab_i <- sab %>% 
    dplyr::filter(iso_alpha_3_code == !!iso_alpha_3_code)
  fit_iso <- glm(value ~ year, data=sab_i, family=quasibinomial)
  intercept <- fit_iso[["coefficients"]][["(Intercept)"]]
  year_coef <- fit_iso[["coefficients"]][["year"]] #multiply by t to get
  sab_i_complete <- sab_complete %>% 
    dplyr::select(iso_alpha_3_code, year) %>%
    dplyr::filter(iso_alpha_3_code == !!iso_alpha_3_code)
  sab_l_i[[iso_alpha_3_code]] <- sab_i_complete %>%
    tibble::add_column(value_single_glm = antilogit(intercept + years*year_coef)) 
}
sab_single_glm <- dplyr::bind_rows(sab_l_i)

sab_complete <- sab_complete %>% 
  dplyr::left_join(sab_single_glm, by = c("iso_alpha_3_code", "year"))

sab_complete <- sab_complete %>%
  dplyr::mutate(estimated_sab = ifelse(criteria1, value_single_glm, value_estimated)) %>%
  dplyr::select(iso_alpha_3_code, year, estimated_sab) 

write.csv(sab_complete, here::here("output", round_name, "sab_estimated_MMEIG2021.csv"), row.names = FALSE)
}

# library(ggplot2)
# pl <- list()
# for(iso_alpha_3_code in unique(sab_complete$iso_alpha_3_code)) {
#   sab_i_long <- sab_complete %>%
#     dplyr::filter(iso_alpha_3_code == !!iso_alpha_3_code)
#   # sab_i <- sab %>%
#   #   dplyr::filter(iso_alpha_3_code == !!iso_alpha_3_code)
#   pl[[iso_alpha_3_code]] <- sab_i_long %>%
#     ggplot(aes(x = year)) +
#     geom_line(aes(y = estimated_sab)) +
#     geom_point(aes(y=value), data = sab %>% dplyr::filter(iso_alpha_3_code == !!iso_alpha_3_code)) +
#     ggtitle(iso_alpha_3_code)
# }
# pdf(here::here("sab.pdf"), 12,12)
# pl
# dev.off()



