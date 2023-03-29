vrdata <- read.csv(here::here("output", round_name, "vrdata.csv"))
difference_between_frozen_and_new_vr <-
  vrdata %>%
  dplyr::filter(updateinmat != 0 | updateinenv != 0 ) %>%
  dplyr::select(iso_alpha_3_code, year_start, year_end, 
                mat_frozen = obs_matdeaths, env_frozen = obs_env, 
                mat_current = mat_fromwhovr, env_current = env_fromwhovr,
                diff_in_mat = updateinmat, diff_in_env = updateinenv) %>%
  dplyr::mutate(diff_in_env = diff_in_env*-1,
                diff_in_mat = diff_in_mat*-1)%>%
  dplyr::mutate(diff_in_env_perc_increase_since_frozen = diff_in_env/env_frozen*100,
                diff_in_mat_perc_increase_since_frozen = diff_in_mat/mat_frozen*100)

difference_between_frozen_and_new_vr
write.csv(difference_between_frozen_and_new_vr, row.names = FALSE, here::here("output",round_name, "difference_between_frozen_and_new_vr.csv"))

difference_between_frozen_and_new_vr %>%
  dplyr::filter(diff_in_mat_perc_increase_since_frozen > 5) %>%
  nrow()
difference_between_frozen_and_new_vr %>%
  dplyr::filter(diff_in_mat_perc_increase_since_frozen < -5) %>%
  nrow()
difference_between_frozen_and_new_vr %>% nrow()
