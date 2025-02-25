#' process_meta_precrisis
#'
#' function to process covariates and other descriptive data
#' @param info_path 
#' @param updated_names_path 
#' @param lifetable_path 
#' @param gdp_path 
#' @param sab_path 
#' @param wpp_path 
#'
#'
process_precrisis_meta <- function(
  # other files
  country_ref,
  mortality,
  mortality_to_denominate_hiv,
  mortality_hiv,
  gdp_data, 
  sab_data, 
  births_gfr_data,
  first_year,
  last_year,
  round_name
){
  # minor mortality processing
  mortality <- mortality %>%
    dplyr::filter(age %in% seq(15,45,5)) %>%
    dplyr::filter(sex == 2) %>%
    dplyr::filter(year >= first_year) %>%
    dplyr::filter(year <= last_year)
  
  # SA edit to add covidfree deaths to meta 
  mortality2 = mortality %>%
    dplyr::mutate(crisis_deaths = crisis_deaths_OtherCrises + crisis_deaths_Conflict) %>%
    dplyr::mutate(sex=2) %>% dplyr::group_by(iso_alpha_3_code, year) %>% 
    dplyr::summarise(deaths=sum(deaths),
                     crisis_deaths=sum(crisis_deaths),
                     crisis_deaths_COVID19=sum(crisis_deaths_COVID19), 
                     adjust_COVID19_mortality= adjust_COVID19_mortality[1]) %>% 
    dplyr::mutate(crisis_deaths_COVID19 = ifelse(crisis_deaths_COVID19 < 0, 0, crisis_deaths_COVID19),
                  crisis_deaths = ifelse(crisis_deaths < 0, 0, crisis_deaths))
  
  covid_free_deaths <- mortality2 %>%
    dplyr::group_by(iso_alpha_3_code) %>% 
    dplyr::mutate(
      covid_free_deaths = dplyr::case_when(
        !adjust_COVID19_mortality ~ ifelse(
          year %in% 2020:2022 & crisis_deaths_COVID19!=0,
          NA_real_,  # Placeholder for interpolation
          deaths
        ),
        TRUE ~ deaths - crisis_deaths_COVID19
      )
    ) %>%
    dplyr::mutate(
      # Interpolate for 2020-2022 where covid_free_deaths is NA
      covid_free_deaths = ifelse(
        !adjust_COVID19_mortality & is.na(covid_free_deaths),
        approx(year[c(year %in% c(2019, 2023))],
               deaths[c(year %in% c(2019, 2023))],
               xout = year)$y,
        covid_free_deaths
      )
    ) %>%
    dplyr::ungroup() %>% 
    dplyr::mutate(covid_deaths_imputed = ifelse(!adjust_COVID19_mortality & crisis_deaths_COVID19==0, 0,
                                                ifelse(year %in% 2020:2022, deaths - covid_free_deaths, 0))) %>% 
    dplyr::mutate(covid_deaths_imputed =  ifelse(covid_deaths_imputed<0, 0, covid_deaths_imputed))
  
  
  ###[SA20240813: edit to impute HIV values where hiv prop >0.9]
  
  isos <- country_ref %>% 
    dplyr::select(iso_alpha_3_code) %>% unique()
  
  
  mortality_hiv <- mortality2 %>% 
    dplyr::select(iso_alpha_3_code, year, deaths) %>% 
    dplyr::right_join(mortality_hiv, by=c('iso_alpha_3_code', 'year')) %>% 
    dplyr::filter(iso_alpha_3_code%in%isos$iso_alpha_3_code) %>% 
    dplyr::mutate(deaths_hiv=prop*deaths) 
  
  
  ## [end of SA20240813 edit]
  
  # make a complete mortality hiv dataset (i.e. has all country years with NAs imputed as 0)
  mortality_hiv <- data.frame(iso_alpha_3_code = mortality$iso_alpha_3_code %>% unique, year = 1985) %>%
    tidyr::complete(tidyr::nesting(iso_alpha_3_code),
                    year = first_year:last_year) %>%
    dplyr::mutate(deaths_hiv = NA) %>%
    bmat::coalesce_join(mortality_hiv, by = c("iso_alpha_3_code", "year"), join = dplyr::left_join) %>%
    dplyr::mutate(deaths_hiv = ifelse(is.na(deaths_hiv), 0, deaths_hiv))
  
  # geograhpic info for meta
  year.t <- seq(first_year, last_year)
  nyears <- length(year.t)
  name.h <- c("logGDP", "logGFR", "SAB")
  meta_precrisis <- list(nyears = nyears, year.t = year.t, name.h = name.h)
  geo_meta <- process_country_ref_for_meta(country_ref)
  meta_precrisis <- append(meta_precrisis, geo_meta)

  
  ############################# impute gdp for small states ############################################

  # Step 0: List of unique iso_alpha_3_code values that you want to have
  #isos_with_aux_data
  
  # Step 1: Get the value we want to use for imputation (small state median gdp)
  small_isos <- births_gfr_data %>% dplyr::filter(Female1549*100 < 500000) %>% dplyr::pull(iso_alpha_3_code) %>% unique
  gdp_data <- gdp_data %>%
    dplyr::select(dplyr::where(~ sum(is.na(.)) / nrow(gdp_data) <= .9))# %>% #drop those columns which are almost entirely missing / shouldnt even be in the excel
   # dplyr::select(-`WHO Ind`)
  long_gdp <- gdp_data %>%
    tidyr::pivot_longer(cols = starts_with("YR"), names_to = "year", values_to = "gdp") %>%
    dplyr::mutate(year = as.numeric(stringr::str_extract(year, "\\d+"))) 
  median_gdp <- long_gdp  %>%
    dplyr::filter(year > 2000) %>%
    dplyr::filter(iso_alpha_3_code %in% small_isos) %>%
    dplyr::pull(gdp) %>% 
    median(na.rm=TRUE)
  
  # Step 2: Identify missing iso_alpha_3_code values
  isos_with_wpp <- mortality %>% dplyr::filter(!iso_alpha_3_code == "") %>% dplyr::pull(iso_alpha_3_code) %>% unique()
  isos_with_gdp <- long_gdp %>%
    dplyr::filter(year > 2000) %>% 
    dplyr::filter(year < 2021) %>% 
    tidyr::drop_na() %>% 
    dplyr::pull(iso_alpha_3_code) %>% 
    unique()
  missing_iso_codes <- setdiff(isos_with_wpp, isos_with_gdp)
  missing_iso_codes <- isos$iso_alpha_3_code[which(isos$iso_alpha_3_code%in%missing_iso_codes)]
  print("The following countries required imputation of GDP: ")
  print(missing_iso_codes)
  
  # Step 3: Identify columns with years that need to be imputed
  year_columns <- grep("^YR", colnames(gdp_data), value = TRUE)
  
  # Step 4: Create a new data frame with the missing iso_alpha_3_code and fixed GDP values
  # Use `rep` and `length(missing_iso_codes)` to generate repeated values, 
  # and `setNames` to set column names dynamically based on `year_columns`
  new_rows <- data.frame(
    iso_alpha_3_code = missing_iso_codes,
    do.call(cbind, setNames(lapply(year_columns, function(x) rep(median_gdp, length(missing_iso_codes))), year_columns))
  )
  
  # Step 5: Combine the new data with the existing data
  gdp_data <- dplyr::bind_rows(long_gdp %>% tidyr::drop_na() %>% tidyr::pivot_wider(values_from = gdp, names_from = year, names_sep = "", names_prefix = "YR"), new_rows)
  #################################################################################################
  
  # isos with sufficient auxiliary data (after imputing gdp)
  isos_with_wpp <- mortality %>% dplyr::filter(!iso_alpha_3_code == "") %>% dplyr::pull(iso_alpha_3_code) %>% unique()
  isos_with_gdp <- gdp_data$iso_alpha_3_code %>% unique()
  isos_list <- list(isos_with_wpp, isos_with_gdp)
  # note we will always have SAB estimates for all isos since we produce estimates for all countries and territories internally regardless of what gets published
  isos_with_aux_data <- Reduce(intersect, isos_list)

  # mortality
  meta_precrisis <- process_mortality(meta = meta_precrisis, 
                         mortality = mortality,
                         mortality_to_denominate_hiv = mortality_to_denominate_hiv,
                         mortality_hiv =mortality_hiv,
                         covid_free_deaths = covid_free_deaths,
                         isos_with_aux_data = isos_with_aux_data)
  
  # read_covariates and births
  meta_precrisis <- append(meta_precrisis, process_covariates(gdp_data, births_gfr_data, sab_data, meta_precrisis))
  meta_precrisis <- append(meta_precrisis ,list(caids = 1, uaids = 0.3, kaids = 0.3))
  
  # information related to HIV
  meta_precrisis$v.ct <- v2015ct(meta_precrisis)
  
  # matrix of covariates
  meta_precrisis <- append(meta_precrisis, covariantMatrix(meta_precrisis))
  
  saveRDS(meta_precrisis, here::here("output",round_name, "meta_precrisis.rds"))
  print(paste("meta info saved to", here::here("output",round_name, "meta_precrisis.rds")))
}


