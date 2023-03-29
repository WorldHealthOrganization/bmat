

#' Covariant Matrix
#'
#' function to generate covariant Matrix
#'
#' @param meta: list of many attributes
#'
#' @return list of 2 elements: matrix of covariants and means (not sure)
#'
#' @examples
#'
#' @family functions used to read covariates
covariantMatrix <- function(meta){
  print("Generating Matrix of Covariants")
  H <- length(meta$name.h)
  Xunst.cth <- X.cth <- array(NA, c(meta$C, meta$nyears, H))

  dimnames(Xunst.cth)[[3]] <- meta$name.h

  Xunst.cth[,,"logGDP"] <- meta$logGDP.ct
  Xunst.cth[,,"logGFR"] <- log(meta$gfr.ct)
  Xunst.cth[,,"SAB"] <- meta$sab.ct
  # standardize X to be centered at zero
  means.h <- apply(Xunst.cth, 3, mean)
  for (h in 1:H){
    X.cth[,,h] <- Xunst.cth[,,h] - means.h[h]
  }
  dimnames(X.cth)[[3]] <- meta$name.h

  return(list(X.cth = X.cth, means.h = means.h))
}
