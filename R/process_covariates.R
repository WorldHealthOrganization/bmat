#' Get Covariate Info
#'
#' function to get covariate Info
#'
#' @param gdpfile. GDP
#' @param sabfile: 2015 SAB database
#' @param wpp2015est: WPP2015 estimates
#' @param meta: list of many attributes
#'
#' @return m: list of 4 attributes
#'
#' @examples
#'
#' @family functions used to read covariates
process_covariates <- function(gdp_data, births_gfr_data, sab_data, meta){
  # meta$name.h <- c("logGDP", "logGFR", "SAB")
  births.ct <-  gfr.ct <- gfr2015.ct <- logGDP.ct <- logGDPnotsmooth.ct <-  sab.ct <- births2015.ct <- matrix(NA, meta$C, meta$nyears)
  for (c in 1:meta$C){
    for (t in 1:meta$nyears){
      births.ct[c,t] <- births_gfr_data %>% dplyr::filter(iso_alpha_3_code == meta$iso.c[c]) %>% dplyr::filter(Year == meta$year.t[t]) %>% dplyr::pull(Births) %>%"*"(1000)
      gfr.ct[c,t] <- births_gfr_data %>% dplyr::filter(iso_alpha_3_code == meta$iso.c[c]) %>% dplyr::filter(Year == meta$year.t[t]) %>% dplyr::pull(GFR)
      logGDPnotsmooth.ct[c,t] <- gdp_data %>% dplyr::filter(iso_alpha_3_code == meta$iso.c[c]) %>% dplyr::pull(paste0("YR", meta$year.t[t])) %>% log()
      gdp_sub <- gdp_data %>% dplyr::filter(iso_alpha_3_code == meta$iso.c[c])
      colselect <- which(gdp_sub %>% names() %in% paste0("YR", meta$year.t[t]+ seq(-2,2)))
      logGDP.ct[c,t] <- gdp_sub[,colselect] %>% unlist() %>% as.vector() %>% mean(na.rm=TRUE) %>% log()
      ## SA edit: 
      # Create gdp_smooth_covidfree, including 2023 but excluding 2020 to 2022
      colselect_covidfree <- which(
        gdp_sub %>% names() %in% paste0("YR", meta$year.t[t] + seq(-2,2)) &
          !gdp_sub %>% names() %in% paste0("YR", 2020:2022)
      )
      
      gdp_smooth_covidfree.ct[c,t] <- gdp_sub[,colselect_covidfree] %>%
        unlist() %>%
        as.vector() %>%
        mean(na.rm = TRUE) %>%
        log()
      
      yearselect <- meta$year.t[t]
      sab.ct[c,t]  <- sab_data %>% dplyr::filter(iso_alpha_3_code == paste(meta$iso.c[c])) %>% dplyr::filter(year == yearselect) %>% dplyr::pull(`X0.5`)
    }
  }
  m <- list(
    births.ct = births.ct, gfr.ct = gfr.ct,
    logGDP.ct=logGDP.ct,  logGDPnotsmooth.ct = logGDPnotsmooth.ct, sab.ct = sab.ct,
    logGDP_noncovid.ct = gdp_smooth_covidfree.ct
  )
  return(m)
}
