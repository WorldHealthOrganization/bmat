process_fsi <- function(
    country_ref,
    fsi,
    namehack
) {
  country_ref <- country_ref %>%
    dplyr::select(name_short_en, iso_alpha_3_code)
  fsi  <- fsi%>%
    dplyr::select(name_short_en = Country, Total)
  fsi <- fsi %>%
    dplyr::left_join(country_ref, by = c("name_short_en")) %>%
    bmat::coalesce_join(namehack, by = c("name_short_en")) %>%
    tidyr::drop_na()
  fsi <- fsi %>%
    dplyr::mutate(group = ifelse(Total > 100, "Very high alert & High alert",
                                 ifelse(Total > 90 & Total <= 100, "Alert",
                                        ifelse(Total > 80 & Total <= 90, "High warning",
                                               ifelse(Total > 70 & Total <= 80, "Elevated warning",
                                                      ifelse(Total > 60 & Total < 70, "Warning", "Stable"))))))
  write.csv(fsi, row.names = FALSE, here::here("output", round_name, "fsi.csv"))
  print(paste("fsi file saved to ", here::here("output", round_name, "fsi.csv")))
}