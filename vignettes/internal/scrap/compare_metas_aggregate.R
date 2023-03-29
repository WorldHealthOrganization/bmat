devtools::load_all()
library(dplyr)
round_name <- "estimates-7-25-22"
#use meta_m60.rds to compare to new meta need to filter on the right Cs
metaold <- readRDS(here::here("bmat_m60", "meta_m60.rds"))
metanew <-  readRDS(here::here("output", round_name, "meta.rds"))

country_ref <- read.csv( 
  here::here("output", round_name, "country_ref.csv"))
  

#compare deaths (decrease in deaths = decrease in mmr by calculation)
death <- compare_input_data(metanew = metanew,
                   metaold = metaold,
                   value_matrixnew = metanew$deaths.ct,
                   value_matrixold = metaold$deaths.ct,
                   forweight.ct = metanew$births.ct,
                   country_ref = country_ref %>% dplyr::rename(region_who = grp_who_region))

temp <- death$r

write.csv(death, here::here("compare_GHE2016_vs_GHE2019.csv"))

####### we changed the function about for an adhoc request, make a bit more general for other indicators if we keep using this


type <- "death"
for(i in 1:length(death)) {
  write.csv(death[i], here::here("data-raw-2021/0compare/", paste0(type,"_", names(death)[i], ".csv")))
}
#compare GFR (lower gfr = lower mmr) (none this round - no new data)

#compare GDP (higher gdp = lower mmr)
gdp <- compare_input_data(metanew = metanew,
                          metaold = metaold,
                          value_matrixnew = exp(metanew$logGDP.ct),
                          value_matrixold = exp(metaold$logGDP.ct))

type <- "gdp"
for(i in 1:length(gdp)) {
  write.csv(gdp[i], here::here("data-raw-2021/0compare/", paste0(type,"_", names(gdp)[i], ".csv")))
}
#compare SBA (higher SBA = lower mmr)
sab <- compare_input_data(metanew = metanew,
                             metaold = metaold,
                             value_matrixnew = metanew$sab.ct,
                             value_matrixold = metaold$sab.ct)

type <- "sab"
for(i in 1:length(sab)) {
  write.csv(sab[i], here::here("data-raw-2021/0compare/", paste0(type,"_", names(sab)[i], ".csv")))
}



#investigatig a country
sab$change_ct %>% dplyr::filter(iso_alpha_3_code == "SWZ")
gdp$change_ct %>% dplyr::filter(iso_alpha_3_code == "SWZ")

temp <- deaths$change_ct %>% dplyr::filter(iso_alpha_3_code == "SWZ")
temp <- deaths$change_ct %>% dplyr::filter(iso_alpha_3_code == "SSD")









