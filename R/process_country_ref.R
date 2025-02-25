process_country_ref <- function(country_ref_who_custom,
                                country_ref_who,
                                sdg_regions,
                                round_name
){
  # historic custom WHO file with custom region categories
  country_ref_who_custom <- country_ref_who_custom %>%
    # dplyr::filter(in.study.15.new) %>% #those without lifetables excluded later
    dplyr::filter(`Countries to be included in the 2025 estimates`==TRUE) %>% 
    dplyr::rename(iso_alpha_3_code = iso.code,
                  m49 = num.code,
                  who_code = whocode,
                  group = group.12,
                  region_for_model = regions.for.regr,
                  region_for_model_ssh = mdg.region.small
    ) %>%
    dplyr::select(
                  iso_alpha_3_code,
                  m49,
                  who_code,
                  region_for_model,
                  region_for_model_ssh)
  
  # up to date WHO file with country names
  country_ref_who <- country_ref_who %>%
    dplyr::filter(WHO_LEGAL_STATUS == "M" | CODE_ISO_3 %in% c("PRI", "PSE", "MAC")) %>%
    dplyr::select(
      iso_alpha_3_code = CODE_ISO_3,
      NAME_SHORT_EN,
      NAME_SHORT_ES,
      NAME_SHORT_FR,
      WHO_LEGAL_STATUS,
      GRP_WHO_REGION
    ) %>%
    dplyr::rename_all(tolower)
  
  # sdg file
  colnames(sdg_regions)[1] <- "iso_alpha_3_code"
  sdg_regions <- sdg_regions %>% 
    dplyr::select(iso_alpha_3_code, sdg1, sdg2, sdg3, who_memberstate)
  
  
  
  
  country_ref <- country_ref_who_custom %>%
    dplyr::left_join(country_ref_who, by = "iso_alpha_3_code") %>%
    dplyr::left_join(sdg_regions, by = "iso_alpha_3_code")%>% 
    dplyr::filter(iso_alpha_3_code!='NIU'&iso_alpha_3_code!='HKG') # [SA 2024/08/12]: for now, not estimating for NIU
  print(paste("there are ", length(country_ref$iso_alpha_3_code), "countries in this country_ref file"))
  write.csv(country_ref, row.names = FALSE, here::here("output", round_name, "country_ref.csv"))
  print(paste("country ref file saved to ", here::here("output", round_name, "country_ref.csv")))
  
}