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

  # isos with sufficient auxiliary data (using this older mortality dataset to obtain states with pop > 100,000)
  isos_with_aux_data <-  mortality_to_denominate_hiv %>%
    dplyr::filter(year >= first_year) %>%
    dplyr::group_by(iso_alpha_3_code) %>%
    dplyr::filter(all(!is.na(deaths))) %>%
    dplyr::pull(iso_alpha_3_code) %>% 
    unique()
  # isos_with_births <- births_gfr_data %>%
  #   dplyr::filter(Year >= first_year) %>%
  #   dplyr::filter(Year <= last_year) %>%
  #   dplyr::group_by(iso_alpha_3_code) %>%
  #   dplyr::filter(all(!is.na(GFR))) %>%#all(population_exposure != 0) & 
  #   dplyr::pull(iso_alpha_3_code) %>% 
  #   unique()
  # isos_with_aux_data <- meta_precrisis$iso.c[which(meta_precrisis$iso.c %in% iso_with_mortality & meta_precrisis$iso.c %in% isos_with_births)]
  # 
  # 
  
  # mortality
  meta_precrisis <- process_mortality(meta = meta_precrisis, 
                         mortality = mortality,
                         mortality_to_denominate_hiv = mortality_to_denominate_hiv,
                         mortality_hiv =mortality_hiv,
                         isos_with_aux_data = isos_with_aux_data)
  
  # read_covariates and births
  meta_precrisis <- append(meta_precrisis, getCovariateInfo(gdp_data, births_gfr_data, sab_data, meta_precrisis))
  meta_precrisis <- append(meta_precrisis ,list(caids = 1, uaids = 0.3, kaids = 0.3))
  
  # information related to HIV
  meta_precrisis$v.ct <- v2015ct(meta_precrisis)
  
  # matrix of covariates
  meta_precrisis <- append(meta_precrisis, covariantMatrix(meta_precrisis))
  
  saveRDS(meta_precrisis, here::here("output",round_name, "meta_precrisis.rds"))
  print(paste("meta info saved to", here::here("output",round_name, "meta_precrisis.rds")))
}


