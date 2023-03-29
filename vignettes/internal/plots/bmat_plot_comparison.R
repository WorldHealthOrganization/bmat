devtools::load_all()
round_name <- "estimates_08-14-22_rev_frozen_vr"
round_name_2 <- "estimates_11-24-22_bmis_projection_patch_checkall"

meta <- readRDS(
  here::here("output", round_name_2, "meta.rds")) 
# isos <- meta$iso.c
ssdata <- read.csv(here::here("output", round_name, "ssdata.csv"))
dbmis <-  ssdata %>% 
  dplyr::filter(include_bmis) %>%
  dplyr::select(iso_alpha_3_code, year_start, year_end)
isos <- dbmis %>% dplyr::pull(iso_alpha_3_code)
pl <- list()
for (iso_alpha_3_code in isos) {
  pl[[iso_alpha_3_code]] <- bmat::plot_bmat_onecountry(
    round_name = round_name,
    round_name_2 = round_name_2,
    iso_alpha_3_code = iso_alpha_3_code,
    year_range = c(2000,2020),
    caption = ""
  )
}
pdf(here::here("output", round_name_2, paste0("comparison_to_", round_name,"_plots.pdf")), 16,8)
pl
dev.off()


