get_bmis_inits <- function() {
  library(truncnorm)
  initslist <- list()
  initslist <- c(initslist, list("rho.alphabeta"= rtruncnorm(n=1, a= -0.95, b= 0.95, mean = -0.3390758, sd = 3 * 0.3208607) ,
                                 "sigma.alpha"= rtruncnorm(n=1, a=0, mean = 0.1194405, sd = 3 *  0.02010844) ,
                                 "sigma.beta"= rtruncnorm(n=1, a=0,mean = 0.1310584, sd = 3 * 0.02545144) ,
                                 "sigmaworld1"=rtruncnorm(n=1, a=0, mean = 0.61767, sd = 3 * 0.14407) ,
                                 "sigmaworld2" =rtruncnorm(n=1, a=0, mean = 0.1718996, sd = 3 * 0.1136006),
                                 "rho_world"= rtruncnorm(n=1, a= -1, b= 1, mean = 0.24615, sd = 3 * 0.454),
                                 "sensworld" = rtruncnorm(n=1, a=0, b=1, mean = 0.592401, sd = 3 * 0.05326) ,
                                 "specworld" = rtruncnorm(n=1, a= 0, b=1, mean = 0.9994706, sd = 3 * 0.00018699)))
  
  return(list(initslist))
} # end inits function
