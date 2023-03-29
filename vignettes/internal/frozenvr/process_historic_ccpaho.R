paho_data =  readxl::read_excel(here::here("pahoraw_2019.xlsx"), skip = 2) %>%
  dplyr::filter(is.na(exclude)) %>%
  dplyr::select(iso_alpha_3_code = iso, year_start = startdate, nonmatvr, matvr)


cc_data =  readxl::read_excel(here::here("ccraw_2019.xlsx"), skip = 1) %>%
  dplyr::rename(iso_alpha_3_code = ISO3, year_start = startdate, matvr = vrmat, nonmatvr = vrnonmat) %>%
  dplyr::mutate(vrenv = as.numeric(vrenv),
                nonmatvr = ifelse(is.na(nonmatvr), vrenv - matvr, nonmatvr),
                matvr = ifelse(is.na(matvr), vrenv - nonmatvr, matvr),
                vrenv = ifelse(is.na(vrenv), matvr + nonmatvr, vrenv)
  ) %>%
  dplyr::mutate(vrenv = as.numeric(vrenv), nonmatvr = vrenv - matvr) %>%
  dplyr::select(iso_alpha_3_code, year_start, matvr, nonmatvr) %>%
  dplyr::filter(!is.na(nonmatvr))


# processing consult/paho data
paho2 <- dplyr::anti_join(paho_data, cc_data, by = c("iso_alpha_3_code" = "iso_alpha_3_code", "year_start" = "year_start"))
dattoadd <- rbind(paho2, cc_data) %>%
  dplyr::rename(pahocc_nonmatvr = nonmatvr, pahocc_matvr = matvr)
write.csv(dattoadd, row.names = FALSE, here::here("specialized_studies_frozen_vr_MMEIG_2019.xx.xx.csv"))
