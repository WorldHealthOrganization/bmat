##########################################################################################################
##########################       Settings     ############################################################
##########################################################################################################
# The round_name variable is used to reference the directory in which the processed data 
# from script 1 was saved. We will also save model results in this directory. The server 
# variable should stay set to FALSE unless you are running this code on a server.
devtools::load_all()
round_name <- "estimates_08-14-22_rev_frozen_vr"
round_first_year <- 1985
round_last_year <- 2020
iso_alpha_3_codes <- c("ARE")
##########################################################################################################


meta <-  readRDS(here::here("output", round_name, "meta.rds"))
temp <- readRDS(here::here("output", round_name, "bmat_global", "estimates_fixed_from_global.rds"))
fit <- readRDS(here::here("output", round_name, "bmat_onecountry", "ARE", "fit.rds"))
beta.h <- temp$beta.h
alpha.c <- unname(temp$alpha.c)
X.cth <- unname(meta$X.cth)
B.ct <- meta$births.ct
E.ct <- meta$deaths.ct
a.ct <- meta$a.ct
c_index <- which(meta$iso.c == iso_alpha_3_codes)
t_index <- which(meta$year.t == 2020)
alpha <- alpha.c[c_index]
covariates_centered <- X.cth[c_index,t_index,]

#print preview
covariates_centered
#model coefs
beta.h
# alpha c from global
alpha.c[c_index]

# PM below
Rstarnonaids <- exp(- beta.h[1]*X.cth[c_index,t_index,1] + 
                      beta.h[2]*X.cth[c_index,t_index,2] - 
                      beta.h[3]*X.cth[c_index,t_index,3] + 
                      alpha.c[c_index])

mmr <- Rstarnonaids/B.ct[c_index,t_index]*E.ct[c_index,t_index]*(1-a.ct[c_index,t_index])*100000
mmr


#okay scary because this is closer to what they want
#why do we get 9??
# get values from one country model
get_alpha.c <- function(
    fit,
    meta,
    c_index) {
  draws1 <- fit$BUGSoutput %>% tidybayes::spread_draws(alpha.c) # warning: requires R2jags or associated package to be loaded to understand this is an mcmc object
  iso <- meta$iso.c[c_index]
  alpha.c <- draws1 %>%
    dplyr::mutate(iso_alpha_3_code = iso) %>%
    dplyr::group_by(iso_alpha_3_code) %>%
    dplyr::summarise(posterior_mean_of_alpha.c = quantile(alpha.c, .5)) %>%
    dplyr::pull(posterior_mean_of_alpha.c)
  
  return(alpha.c)
}
alpha.c_onecountry <- get_alpha.c(fit, meta, c_index)

#alpha c from one country
alpha.c_onecountry

Rstarnonaids_onecountry <- exp(- beta.h[1]*X.cth[c_index,t_index,1] + 
                      beta.h[2]*X.cth[c_index,t_index,2] - 
                      beta.h[3]*X.cth[c_index,t_index,3] + 
                      alpha.c_onecountry)

mmr <- Rstarnonaids_onecountry/B.ct[c_index,t_index]*E.ct[c_index,t_index]*(1-a.ct[c_index,t_index])*100000
mmr


# for interpretation
eβ0 / (1 + eβ0)
exp(alpha.c[c_index])/(1+exp(alpha.c[c_index]))/B.ct[c_index,t_index]*E.ct[c_index,t_index]*(1-a.ct[c_index,t_index])*100000
exp(alpha.c_onecountry)/(1+exp(alpha.c_onecountry))/B.ct[c_index,t_index]*E.ct[c_index,t_index]*(1-a.ct[c_index,t_index])*100000


# investigations of convergence
summ <- MCMCvis::MCMCsummary(fit, params = "alpha.c") %>% 
  tibble::rownames_to_column(var = "pars")

summ <- summ %>%
  dplyr::mutate(pars2 = pars %>% stringr::str_remove_all("[:digit:]") %>% stringr::str_remove("[,]") %>% stringr::str_remove("\\[]")) 

write.csv(temp, row.names = FALSE, here::here(file.path(main_path, "summary.csv")))

MCMCvis::MCMCtrace(fit,
                   ISB = FALSE,
                   pdf = TRUE,
                   post_zm = TRUE,
                   open_pdf = FALSE,
                   Rhat = TRUE,
                   filename = "mcmc_trace.pdf",
                   wd = here::here()
)

round_name <- "estimates_08-14-22_rev_frozen_vr"
fit <- readRDS(here::here("output", round_name, "bmat_global","fit.rds"))
trace_plots(fit,
            main_path,
            hyperpars_to_plot =
              c("phi",
                "sqrtgamma0", "sigma.lambda",
                "beta.h", 
                "nonsamplingdhs.se","nonsamplingnondhs.se",
                "sigma.country", "sigma.region", "alpha.world"), #"alpha.r")
            global_run = TRUE
)

#just to confirm mmr from global
temp <- readRDS(here::here("output", round_name, "bmat_global", "estimates.rds"))
temp <- temp %>% dplyr::filter(iso_alpha_3_code == "ARE") %>% dplyr::filter(parameter == "mmr")
