
process_country_ref_for_meta <- function(country_ref_who){

    # dplyr::filter(!is.na(region))
  iso.c <- country_ref_who$iso_alpha_3_code
  numcode.c <- country_ref_who$m49
  C <- length(iso.c)
  name.c <- country_ref_who$name_short_en

  # Regions used for the modeling
  reg.c <- country_ref_who$region_for_model %>% as.factor
  regnames <- unique(reg.c) %>% as.factor
  R <- length(regnames)
  getr.c <- Getc.i(iso.i = paste(reg.c), iso.c = regnames)

  mdg.c <- country_ref_who$region_for_model_ssh  %>% as.factor
  whocode.c <- country_ref_who$who_code

  meta <- list(iso.c = iso.c, numcode.c = numcode.c,
              regnames = regnames, reg.c = reg.c,
               getr.c = getr.c, whocode.c = whocode.c, C = C, name.c = name.c, mdg.c = mdg.c, R = R)
  return(meta)
}
