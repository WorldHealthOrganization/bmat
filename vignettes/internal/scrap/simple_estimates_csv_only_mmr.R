df <-  read.csv(here::here("output", round_name, "estimates.csv"))
df <- df %>% 
  dplyr::filter(parameter == "mmr") %>% 
  dplyr::filter(year_mid >= 2000) %>%
  dplyr::select(iso_alpha_3_code, year = year_mid, mmr = `X0.5`) 
write.csv(df, row.names = FALSE, "mmr_estimates.csv")
