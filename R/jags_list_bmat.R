

jags_list_bmat <- function(
  main_data = main_data,
  meta = meta,
  global = TRUE,
  estimates_fixed_from_global_bmat = NULL,
  referenceyear = referenceyear,
  sqrtgamma0max = sqrtgamma0max,
  max.sigma.lambda = max.sigma.lambda,
  imputeSElogPM = imputeSElogPM,
  validation_settings
){
  # core parameters
  datcore <- translate_core(meta, 
                            referenceyear,
                            sqrtgamma0max,
                            max.sigma.lambda) 
  
  datcore$imputeSElogPM <- imputeSElogPM
  datcore$referenceyear <- referenceyear
  indices_by_data_model <- index_by_data_model(
    main_data
  )
  
  # specialized studies
  datinq <- translate_specialized(
    main_data,
    meta,
    indices_by_data_model
  )
  # specialized studies incomplete
  datinq_incomplete <- translate_specialized_incomplete(
    main_data,
    meta,
    indices_by_data_model
  )
  # vr
  datvr <- translate_vr(
    main_data,
    meta,
    indices_by_data_model
  )
  # other studies
  datother <- translate_other(
    main_data,
    meta,
    indices_by_data_model,
    imputeSElogPM
  )
  
  jags_list <- c(datcore, 
           datinq,
           datinq_incomplete,
           datvr, 
           datother,
           indices_by_data_model
  )
  
  if (!global) {
    jags_list_onecountry_fixed <- estimates_fixed_from_global_bmat %>% .[c(
      "nonsamplingdhs.se",
      "nonsamplingnondhs.se",
      "theta",
      "phi",
      "sqrtgamma0",
      "sigma.lambda",
      "sigma.country",
      "beta.h",
      "alpha.r"
    )]
    jags_list_onecountry_fixed["K"] <- 2  
    jags_list <- append(jags_list, jags_list_onecountry_fixed)
  } else {
    jags_list <- jags_list %>%
        add_validation_index_to_jags_list(main_data = main_data,
                                        validation = validation_settings$validate)
  }
  return(jags_list)
} # end translate_to_jagslist function

