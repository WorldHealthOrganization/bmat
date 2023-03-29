# these two functions below are used to get CIs for observed parameters
custom_bintol <- function(x, n, bound) { # sometimes creates NaN, to note if problematic later
  if (bound == "lower") bound <- 4
  if (bound == "upper") bound <- 5
  b <- (tolerance::bintol.int(x = x, n = n, side = 2, alpha = 0.2, method = "CP")/n)[1,bound]
  return(b)
}
delta_method <- function(n,p,q,bound) {
  samp_mean <- p/q
  deltamethod_var <- ((1-p)/(p*n)) + ((1-q)/(q*n))
  b <- c()
  for (i in 1:length(n)) { #for loop can be removed if vectorize the mutate as with previous calculations
    b[i] <- log_vradj_samples <- rnorm(1000, mean = samp_mean[i], sd = sqrt(deltamethod_var[i])) %>%
      quantile(bound, na.rm = TRUE)
  }
  return(b)
}
calculate_data_for_plots <- function(ssdata, main_path) {
  suppressWarnings({
    suppressMessages({
      print("Calculating confidence intervals for observed data.")
      main_data_for_plotting <- ssdata %>%
        dplyr::rename_all(tolower) %>%
        # dplyr::mutate(dplyr::across(.cols = where(is.list),
        #                             .fns = unlist)) %>%
        dplyr::mutate(dplyr::across(
          .cols = c("tn", "tp", "fn", "fp", "up", "un", "truemat", "truemat_vr", "tot_vr"),
          .fns = ~ . / tot,
          .names = "rho_{.col}.point"
        )) %>%
        dplyr::mutate(dplyr::across( #tot here is summed ytot
          .cols = c("tn", "tp", "fn", "fp", "up", "un", "truemat", "truemat_vr", "tot_vr"),
          .fns = ~ mapply(custom_bintol, ., tot, "upper"), #apply here vectorizes the non-vectorized dplyr mutate
          .names = "rho_{.col}.upper"
        )) %>%
        dplyr::mutate(dplyr::across(
          .cols = c("tn", "tp", "fn", "fp", "up", "un", "truemat", "truemat_vr", "tot_vr"),
          .fns = ~ mapply(custom_bintol, ., tot, "lower"), #apply here vectorizes the non-vectorized dplyr mutate
          .names = "rho_{.col}.lower"
        )) %>%
        dplyr::mutate(dplyr::across( # tot_vr here is summed ytot_vrt
          .cols = c("tn", "tp", "fn", "fp", "up", "un", "mat_vr_ref", "truemat_vr", "mat_vr"),
          .fns = ~ . / tot_vr,
          .names = "gamma_{.col}.point"
        )) %>%
        dplyr::mutate(dplyr::across(
          .cols = c("tn", "tp", "fn", "fp", "up", "un", "mat_vr_ref", "truemat_vr", "mat_vr"),
          .fns = ~ mapply(custom_bintol, ., tot_vr, "upper"), #apply here vectorizes the non-vectorized dplyr mutate
          .names = "gamma_{.col}.upper"
        )) %>%
        dplyr::mutate(dplyr::across(
          .cols = c("tn", "tp", "fn", "fp", "up", "un", "mat_vr_ref", "truemat_vr", "mat_vr"),
          .fns = ~ mapply(custom_bintol, ., tot_vr, "lower"), #apply here vectorizes the non-vectorized dplyr mutate
          .names = "gamma_{.col}.lower"
        )) %>%
        dplyr::mutate(
          year_reference = floor((year_start + year_end - 1) / 2),
          # dattoplot[,"vradj"] <- (df$truemat.vr/df$zvr)/(df$zmatvr/df$zvr)
          # dattoplot[, "vradj"] <- ifelse(dattoplot[, "vradj"] > 20, NA, dattoplot[, "vradj"] )
          vradj.point = ifelse((gamma_truemat_vr.point) / (gamma_mat_vr.point) > 20,
                               NA,
                               (gamma_truemat_vr.point) / (gamma_mat_vr.point)
          ),
          vradj.upper = delta_method(n = tot_vr, p = gamma_truemat_vr.point, q = gamma_mat_vr.point, bound = .9),
          vradj.lower = delta_method(n = tot_vr, p = gamma_truemat_vr.point, q = gamma_mat_vr.point, bound = .1),
          sens.point =  tp / truemat_vr,
          sens.upper = mapply(custom_bintol, tp, truemat_vr, "upper"),
          sens.lower = mapply(custom_bintol, tp, truemat_vr, "lower"),
          spec.point = tn / (tn + fp),
          spec.upper = mapply(custom_bintol, tn, tn + fp, "upper"),
          spec.lower = mapply(custom_bintol, tn, tn + fp, "lower"), #different n here then in EPs version but it seems correct to match sense as above
          "fn_out_truemat_vr.point" = fn / truemat_vr,
          "fn_out_truemat_vr.upper" = mapply(custom_bintol, fn, truemat_vr, "upper"),
          "fn_out_truemat_vr.lower" = mapply(custom_bintol, fn, truemat_vr, "lower"),
          "fn_out_truemat.point" = fn / truemat,
          "fn_out_truemat.upper" = mapply(custom_bintol, fn, truemat, "upper"),
          "fn_out_truemat.lower" = mapply(custom_bintol, fn, truemat, "lower"),
          "fp_out_mat_vr.point" = fp / mat_vr,
          "fn_out_mat_vr.upper" = mapply(custom_bintol, fp, mat_vr, "upper"),
          "fn_out_mat_vr.lower" = mapply(custom_bintol, fp, mat_vr, "lower")

        )
      main_data_not_for_plots <- main_data_for_plotting %>%
        dplyr::select(iso_alpha_3_code,
                      year_end,
                      year_start,
                      year_reference,
                      include_bmat,
                      include_bmis,
                      fn,
                      fp,
                      tn,
                      tp,
                      un,
                      up,
                      truemat,
                      truemat_vr,
                      mat_vr,
                      tot,
                      tot_vr,
                      mat_vr_ref,
                      rho_ref,
                      tot_vr_ref)
      saveRDS(main_data_not_for_plots, here::here(main_path, "main_data_not_for_plots.rds"))

      main_data_for_plotting <- main_data_for_plotting %>% #not ideal coding here, should remove extra vars before gathering
        tidyr::gather(key = "parameter",
                      value = "value",
                      -check_outside_of_vr,
                      -year_end,
                      -year_start,
                      -year_reference,
                      -fn,
                      -fp,
                      -tn,
                      -tp,
                      -un,
                      -up,
                      -include_bmat,
                      -include_bmis,
                      # -incompvr,
                      # -incomvr_multiplier,
                      -iso_alpha_3_code,
                      -citation_short,
                      -env_total,
                      -truemat,
                      -truemat_vr,
                      -mat_vr,
                      -tot,
                      -tot_vr,
                      -mat_vr_ref,
                      -rho_ref,
                      -tot_vr_ref,
                      -dmname) %>%
        tidyr::separate(parameter, c("parameter", "percentile"), sep = "([.])") %>%
        dplyr::mutate(percentile = ifelse(percentile == "point", .5,
                                          ifelse(percentile == "upper", .9,
                                                 ifelse(percentile == "lower", .1,
                                                        NA)
                                          )
        )
        ) %>%
        tidyr::spread(key = "percentile", value = "value") %>%
        dplyr::relocate(iso_alpha_3_code, year_reference, parameter, `0.9`, `0.5`, `0.1`)



      # gg: refactored this but it looks historical
      # %>%
      #   dplyr::mutate(fn_proportion = ifelse(dmname == "FN truemat_vr binom",
      #                                        rho_fn/truemat_vr,
      #                                        rho_fn)
      #   ) %>%
      #   dplyr::mutate(fn_proportion = ifelse(dmname == "FN truemattot binom",
      #                                        fn_proportion/truemat,
      #                                        fn_proportion)
      #   ) %>%
    })
  })
  return(main_data_for_plotting)
}
