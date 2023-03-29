#' obtain_country_meta_info
#'
#' @param iso_alpha_3_code
#' @param runname
#'
#' @return
#' @export
#'
filter_global_meta <- function(iso_alpha_3_code, meta){
  if (length(iso_alpha_3_code) > 1){
    print("Use one iso code only")
    return(NULL)
  }
  if (!is.element(iso_alpha_3_code, meta$iso.c)){
    print("ISO code not found, no meta created. Please choose iso from list below:")
    print(meta$iso.c)
    return(invisible(NULL))
  }
  # update meta to contain info relevant to selected country only
  meta$C <- 1
  c <- which(meta$iso.c == iso_alpha_3_code)
  for (info in c("iso.c", "group.c", "reg.c", "getr.c", "whocode.c", "numcode.c", "isssa.c",
            "name.c", "mdg.c", "crisis_years.c")){
    meta[[info]] <- meta[[info]][c]
  }
  for (info in c( "a.ct" , "l15.ct", "t15t50.ct", "deaths.ct", "crisisdeaths.ct", "births.ct",
                  "gfr.ct", "logGDP.ct", "logGDPnotsmooth.ct", "sab.ct", "v.ct",
                  "a_nocrisis.ct", "deaths_incl_crisis.ct")){
    meta[[info]] <- matrix(c(meta[[info]][c,]), 1, meta$nyears)
  }
  meta[["X.cth"]] <- array(meta[["X.cth"]][c,,],c(1, meta$nyears, dim(meta$X.cth)[3]))
  meta$c <- c
  return(meta)
}
