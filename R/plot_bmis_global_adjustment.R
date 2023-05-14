plot_bmis_global_adjustment <- function(round_name,
                                        round_last_year,
                                        global_run = TRUE) {
  if (global_run) {
    sens_spec_global <- readRDS(here::here("output", round_name, "bmis_global", "sens_spec_global.rds"))
    global_sens <- sens_spec_global %>% dplyr::filter(year_start == round_last_year) %>% dplyr::pull(sens)
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
    pl <- ggplot2::ggplot(temp, ggplot2::aes(x=truepm, y=adjustment, col =as.factor(spec))) +
      ggplot2::geom_line(size=1) +
      ggplot2::ylim(1, max(temp$adjustment + .1)) +
      ggplot2::theme_gray(base_size = 20) +
      ggplot2::ylab("CRVS adjustment") +
      ggplot2::xlab("True PM") + 
      ggplot2::scale_color_discrete(name = "Specificity")
    png(here::here("output", round_name, "bmis_global", "crvs_adjustment.png"), width = 1000, height = 1000)
    pl
    dev.off()
  } 
}