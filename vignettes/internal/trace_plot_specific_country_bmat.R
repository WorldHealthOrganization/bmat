devtools::load_all()
round_name <- "estimates_08-14-22_rev_frozen_vr"
fit <- readRDS(here::here("output", round_name, "bmat_global","fit.rds"))
  

meta <-  readRDS(here::here("output", round_name, "meta.rds"))
c <- which(meta$iso.c == "ARE")
pars_to_save <- c(
  "alpha.c","arma.ct", "mu.ct")
temp <- MCMCvis::MCMCsummary(fit, params = pars_to_save) %>% 
  tibble::rownames_to_column(var = "pars")

# demo of how string split works to get c and t here
# a <- "a[1,2]"
# a %>% stringr::str_remove_all("[:alpha:]")
# a %>% stringr::str_remove_all("[:alpha:]") %>% stringr::str_remove_all("\\[")
# a %>% stringr::str_remove_all("[:alpha:]") %>% stringr::str_remove_all("\\[") %>% stringr::str_remove_all("\\]")
# a %>% stringr::str_remove_all("[:alpha:]") %>% stringr::str_remove_all("\\[") %>% stringr::str_remove_all("\\]") %>% stringr::str_split("\\,", simplify=T) %>% .[,1]
a <- "a.c[1,2]"
a %>% stringr::str_remove_all("[:alpha:]")
a %>% stringr::str_remove_all("[:alpha:]") %>% stringr::str_remove_all("\\[")
a %>% stringr::str_remove_all("[:alpha:]") %>% stringr::str_remove_all("\\[") %>% stringr::str_remove_all("\\]")
a %>% stringr::str_remove_all("[:alpha:]") %>% stringr::str_remove_all("\\[") %>% stringr::str_remove_all("\\]") %>% stringr::str_split("\\,", simplify=T) %>% .[,2]
stringr::str_split("\\,", simplify=T)
stringr::str_split(a, "\\,", simplify=T)[,2]
temp <- temp %>%
  dplyr::mutate(pars2 = pars %>% stringr::str_remove_all("[:digit:]") %>% stringr::str_remove("[,]") %>% stringr::str_remove("\\[]")) %>%
  dplyr::mutate(c = pars %>% 
                  stringr::str_remove_all("[:alpha:]") %>% 
                  stringr::str_remove_all("\\.") %>% 
                  stringr::str_remove_all("\\[") %>% 
                  stringr::str_remove_all("\\]") %>% 
                  stringr::str_split("\\,", simplify=T) %>% 
                  .[,1]) 


temp <- temp %>%
  dplyr::filter(c == !!c)


write.csv(temp, row.names = FALSE, here::here("output", round_name, "bmat_global","summary_c.csv"))

MCMCvis::MCMCtrace(fit,
                   params = temp$pars,
                   ISB = FALSE,
                   pdf = TRUE,
                   post_zm = TRUE,
                   open_pdf = FALSE,
                   Rhat = TRUE,
                   filename = "mcmc_trace_c.pdf",
                   wd = here::here("output", round_name, "bmat_global")
)





# 
# 
# 
#   # checking some parameters
#   PlotTrace <- function(#Traceplot for one parameter
#     ### Trace plot for one parameter and add loess smoother for each chain
#     parname, mcmc.array,##<< needs to be 3-dimensional array!
#     n.chains= NULL, n.sim= NULL, main = NULL){
#     if (is.null(main)) main <- parname
#     if (is.null(n.sim)) n.sim <- dim(mcmc.array)[1]
#     if (is.null(n.chains)) n.chains <- dim(mcmc.array)[2]
#     plot(c(mcmc.array[,1,parname]), type = "l", ylab = parname,  main = main,
#          ylim = c(min(mcmc.array[,,parname]),max(mcmc.array[,,parname])))
#     for (chain in 1:n.chains){
#       lines(c(mcmc.array[,chain,parname]), type = "l", col = chain)
#     }
#     for (chain in 1:n.chains){
#       curve(predict(loess(c(mcmc.array[,chain,parname])~seq(1,n.sim)),x), lty = 2, lwd = 3, add = TRUE, type = "l", col = chain)
#     }
#   }
#   
#   devtools::load_all()
#   round_name <- "estimates-7-26-22"
#   round_first_year <- 1985
#   round_last_year <- 2020
#   fit <- readRDS(here::here("output", round_name, "bmat_global","fit.rds"))
#   
#   pars_to_save <- c(
#     "phi",
#     "sqrtgamma0", "sigma.lambda",
#     "beta.h[1]",
#     "beta.h[2]",
#     "beta.h[3]",
#     "nonsamplingdhs.se","nonsamplingnondhs.se",
#     "sigma.country", "sigma.region", "alpha.r", "alpha.world")
#   globalparams <- pars_to_save
# mcmc.array <- fit$BUGSoutput$sims.array
# 
# 
# pdf(here::here("output", round_name, "bmat_global","trace.pdf"))
# par(mfrow = c(1,3))
# for(parname in globalparams){
#   PlotTrace(parname, mcmc.array)
# }
# dev.off()
# 
