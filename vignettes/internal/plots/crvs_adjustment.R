
devtools::load_all()
round_name <- "estimates_08-14-22_rev_frozen_vr"
round_last_year <- 2020
temp <- readRDS(here::here("output", round_name, "bmis_global", "sens_spec_global.rds"))
global_sens <- temp %>% dplyr::filter(year_start == round_last_year) %>% dplyr::pull(sens)



library(dplyr)
library(ggplot2)
truepm_vec <- seq(0,0.05,0.0001)
lengthpm <- length(truepm_vec)
spec_vec <- rep(1,lengthpm)
spec_vec2 <- rep(.9999,lengthpm)
spec_vec3 <- rep(.9993,lengthpm)
spec_vec4 <- rep(.999,lengthpm)
temp <- data.frame(truepm = rep(truepm_vec,4), 
                   spec = c(spec_vec, spec_vec2, spec_vec3, spec_vec4),
                   sens = rep(global_sens, lengthpm*4)) %>%
  dplyr::mutate(adjustment = truepm / (truepm*sens + (1-spec)*(1-truepm))) %>%
  dplyr::filter(truepm > .001) %>%
  dplyr::filter(adjustment > 1.1)
#truepm / (truepm*sens + (1-spec)*(1-truepm))
pl <- ggplot(temp, aes(x=truepm, y=adjustment, col =as.factor(spec))) +
  geom_line(size=1) +
  ylim(1, max(temp$adjustment + .1)) +
  theme_gray(base_size = 20) +
  ylab("CRVS adjustment") +
  xlab("True PM") + 
  scale_color_discrete(name = "Specificity")
pl
png("crvs_adjustment.png", width = 1000, height = 1000)
pl
dev.off()
