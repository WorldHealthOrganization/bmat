#' v2015ct
#'
#' function to generate v2015.ct
#'
#' @param meta: list of many attributes
#'
#' @return v2015.ct: matrix of dimensions (meta$C,meta$nyears)
#'
#' @examples
#'
#' @family functions used to read covariates
v2015ct <- function(meta){
  print("generating v2015.ct...")
  #print(meta$C)
  #print(meta$nyears)
  v2015.ct <- matrix(NA,meta$C,meta$nyears)
  for (c in 1:meta$C){
    for (t in 1:meta$nyears){
      v2015.ct[c,t] <- meta$caids*meta$kaids*meta$gfr.ct[c,t]/(1+meta$caids*(meta$kaids-1)*meta$gfr.ct[c,t])
    }}
  return(v2015.ct)
}
