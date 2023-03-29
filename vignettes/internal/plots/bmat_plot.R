devtools::load_all()
round_name <- "estimates-7-26-22"
iso_alpha_3_codes <- readRDS(
  here::here("output", round_name, "meta.rds")
)$iso.c
country_ref <- read.csv( 
  here::here("output", round_name, "country_ref.csv"))
meta_m60  <- readRDS( # temp
  here::here("bmat_m60", "meta_M60.rds"))
old_iso <- meta_m60$iso.c
isos_to_compare <- old_iso[old_iso %in% iso_alpha_3_codes]


# pl <- list()
# for (iso_alpha_3_code in iso_alpha_3_codes) {
#   pl[[iso_alpha_3_code]] <- bmat::plot_bmat_onecountry(
#     round_name = round_name,
#     iso_alpha_3_code = iso_alpha_3_code,
#     estimates_to_compare = NULL,#readRDS(here::here("archive/la1_estimates.rds")),
#     year_range = c(1985,2019),
#     caption = "",
#     country_ref = country_ref
#   )
# }
# pdf(here::here("output", round_name, "plots.pdf"), 16,8)
# pl
# dev.off()

devtools::load_all()
# HACK - AS.CHARACTER used to force factors back to strings 
# there is a bug (seems to be encoding related where strings turn to factors)
# is this why it turns to numbers on the server too?
#comparing all
pl <- list()
for (iso_to_compare in isos_to_compare) {
  pl[[iso_to_compare]] <- bmat::plot_bmat_onecountry(
    round_name = round_name,
    iso_alpha_3_code = iso_to_compare,
    estimates_to_compare_la1 = readRDS(here::here("bmat_m60/la1/la1_estimates.rds")), #la1_estimates.rds
    meta_for_compare_la1 = meta_m60,
    year_range = c(2000,2020),
    caption = ""
  )
}
library(ggplot2)
pdf(here::here("output", round_name, "plots_comparison.pdf"), 16,8)
pl
dev.off()








iso_afro <- bmat::geographic_info %>%
  dplyr::filter(region_who == "Africa") %>%
  dplyr::pull(iso_alpha_3_code)
isos <- iso_alpha_3_codes[iso_alpha_3_codes %in%iso_afro]
pl <- list()
for (iso_alpha_3_code in isos) {
  pl[[iso_alpha_3_code]] <- bmat::plot_bmat_onecountry(
    round_name = round_name,
    iso_alpha_3_code = iso_alpha_3_code,
    estimates_to_compare = readRDS(here::here("test/la1_estimates.rds")),
    year_range = c(1985,2019),
    caption = ""
  )
}
pdf(here::here("output", round_name, "plots_afro.pdf"), 16,8)
pl
dev.off()


iso_em <- bmat::geographic_info %>%
  dplyr::filter(region_who == "Eastern Mediterranean") %>%
  dplyr::pull(iso_alpha_3_code)
isos <- iso_alpha_3_codes[iso_alpha_3_codes %in%iso_afro]
pl <- list()
for (iso_alpha_3_code in isos) {
  pl[[iso_alpha_3_code]] <- bmat::plot_bmat_onecountry(
    round_name = round_name,
    iso_alpha_3_code = iso_alpha_3_code,
    estimates_to_compare = readRDS(here::here("test/la1_estimates.rds")),
    year_range = c(1985,2019),
    caption = ""
  )
}
pdf(here::here("output", round_name, "plots_eastern_med.pdf"), 16,8)
pl
dev.off()