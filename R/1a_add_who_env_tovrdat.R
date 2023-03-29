add_who_env_tovrdat <- function(meta_precrisis, dat_merged) {
  colnames(meta_precrisis$deaths.ct) <- meta_precrisis$year.t
  deaths2 <- tibble::as_tibble(meta_precrisis$deaths.ct)
  deaths3 <- cbind("iso" = meta_precrisis$iso.c, deaths2)
  deaths4 <-
    deaths3 %>%
    tidyr::gather(key = "year",
                  value = "whoenv",
                  -1,
                  convert = T)
  deaths4
  names(dat_merged)
  re3 <-
    dplyr::left_join(dat_merged, deaths4, by = c("iso" = "iso", "start" = "year"))
  return(re3)
}
