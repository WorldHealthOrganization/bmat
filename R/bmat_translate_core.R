translate_core <- function(meta, 
                           referenceyear,
                           sqrtgamma0max,
                           max.sigma.lambda) {
  jags_list <- list(
                   year.ref = which(meta$year.t == referenceyear),
                   sqrtgamma0max = sqrtgamma0max, # Time series parameters
                   max.sigma.lambda = max.sigma.lambda, # Time series parameters
                   caids = meta$caids, #aids parameters
                   uaids = meta$uaids, #aids parameters
                   kaids = meta$kaids, #aids parameters
                   X.cth = meta$X.cth,
                   B.ct = meta$births.ct,
                   a.ct  = meta$a.ct, #a = the proportion of HIV-related deaths among all deaths to women aged 15–49 
                   E.ct = meta$deaths.ct,
                   R = meta$R, 
                   getr.c = meta$getr.c, 
                   nyears = meta$nyears, 
                   C = meta$C,
                   pi.constant = pi,
                   input0.ct = matrix(0, meta$C, meta$nyears),# included for restriction R <1
                   Raids.ct = meta$v.ct*meta$uaids,
                   muaids.ct = meta$v.ct*meta$uaids*meta$a.ct*meta$deaths.ct/meta$births.ct, #v.ct = is the proportion of HIV-related deaths to women aged 15–49 that occur during pregnancy
                   #muaids = (hiv-related indirect maternal / births)
                   isssa.c = meta$isssa.c,
                   input1.ct = matrix(1, meta$C, meta$nyears),
                   input1again.ct = matrix(1, meta$C, meta$nyears)
  )
  return(jags_list)
}