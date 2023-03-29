devtools::load_all()
temp <- read.csv(here::here("output", "estimates_12-19-22", "estimates.csv"))
temp %>% names
temp2 <- temp %>%
  dplyr::select(mmr_point_estimate = `X0.5`, iso_alpha_3_code, year = year_mid, parameter) %>%
  dplyr::filter(parameter == "mmr") %>%
  dplyr::select(-parameter) %>%
  dplyr::filter(year > 1999)

write.csv(temp2, row.names = FALSE, "estimates_only_mmr_2000+.csv")
