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
  mortality_to_denominate_hiv <- mortality_to_denominate_hiv %>%
    dplyr::filter(age %in% seq(15,45,5)) %>%
    dplyr::filter(sex == 2) %>%
    dplyr::filter(year >= first_year) %>%
    dplyr::filter(year <= last_year)
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
    dplyr::select(dplyr::where(~ sum(is.na(.)) / nrow(gdp_data) <= .9)) %>% #drop those columns which are almost entirely missing / shouldnt even be in the excel
    dplyr::select(-`WHO Ind`)
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


