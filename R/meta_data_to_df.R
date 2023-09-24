input_data_c_wrapper <- function(metanew, metaold = NULL, series_name_new, series_name_old = NULL, wide_format_with_differences = FALSE) {
  
  # Helper function to get attribute values
  getAttributes <- function(metanew, metaold) {
    attrs <- list(
      gdp = list(value_new = exp(metanew$logGDP.ct), value_name = "GDP"),
      sab = list(value_new = metanew$sab.ct, value_name = "SAB"),
      gfr = list(value_new = metanew$gfr.ct, value_name = "GFR"),
      births = list(value_new = metanew$births.ct, value_name = "Births"),
      alldeath = list(value_new = metanew$deaths.ct, value_name = "Deaths"),
      alldeath_w_crisis = list(value_new = metanew$deaths_incl_crisis.ct, value_name = "Deaths including crisis")
    )
    if (!is.null(metaold)) {
      attrs$gdp$value_old <- exp(metaold$logGDP.ct)
      attrs$sab$value_old <- metaold$sab.ct
      attrs$gfr$value_old <- metaold$gfr.ct
      attrs$births$value_old <- metaold$births.ct
      attrs$alldeath$value_old <- metaold$deaths.ct
      attrs$alldeath_w_crisis$value_old <- metaold$deaths_incl_crisis.ct
    }
    return(attrs)
  }
  
  attributes <- getAttributes(metanew, metaold)
  dl <- list()
  
  # Loop through each attribute and call the function
  for(attr_name in names(attributes)) {
    attr <- attributes[[attr_name]]
    
    if (is.null(metaold)) {
      func_to_call <- input_data_c
      dl[[attr_name]] <- func_to_call(
        meta = metanew,
        value_matrix = attr$value_new,
        value_name = attr$value_name
      )
    } else {
      func_to_call <- if (!wide_format_with_differences) compare_input_data_c else compare_input_data_c_wide
      dl[[attr_name]] <- func_to_call(
        metanew = metanew,
        metaold = metaold,
        value_matrixnew = attr$value_new,
        value_matrixold = attr$value_old,
        series_name_new = series_name_new,
        series_name_old = series_name_old,
        value_name = attr$value_name
      )
    }
  }
  
  df <- dplyr::bind_rows(dl)
  
  return(df)
}


# wrapper which makes series in wide format
compare_input_data_c_wide <- function(metanew, metaold, value_matrixnew, value_matrixold, series_name_new = "value", series_name_old = "value", value_name = "value") {
  
  df <- compare_input_data_c(metanew, metaold, value_matrixnew, value_matrixold, series_name_new, series_name_old, value_name) %>%
    tidyr::pivot_wider(names_from = series_name, values_from = value)
  
  
  # Create symbols from the column names
  series_name_new_sym <- rlang::sym(series_name_new)
  series_name_old_sym <- rlang::sym(series_name_old)

  df <- df %>%
    dplyr::mutate(change_abs = abs(!!series_name_old_sym - !!series_name_new_sym) / (!!series_name_new_sym)*100) %>%
    dplyr::mutate(change = (!!series_name_old_sym - !!series_name_new_sym) / (!!series_name_new_sym)*100) 
  
  return(df)
}

# inner function which processes two series of data
compare_input_data_c <- function(metanew, metaold, value_matrixnew, value_matrixold, series_name_new = "value", series_name_old = "value", value_name = "value") {
  
  series_name_new_enquo <- rlang::enquo(series_name_new)
  series_name_old_enquo <- rlang::enquo(series_name_old)
  value_name_enquo <- rlang::enquo(value_name)
  
  dfnew <- matrix_ct_to_df_with_isoyear(metanew, value_matrix = value_matrixnew) %>%
    dplyr::filter(year > 1999) %>%
    dplyr::mutate(series_name = !!series_name_new_enquo) %>%
    dplyr::mutate(value_name = !!value_name_enquo) %>%
    dplyr::select(-t) %>%
    dplyr::select(-c)
  
  dfold <- matrix_ct_to_df_with_isoyear(metaold, value_matrix = value_matrixold) %>%
    dplyr::filter(year > 1999) %>%
    dplyr::mutate(series_name = !!series_name_old_enquo) %>%
    dplyr::mutate(value_name = !!value_name_enquo) %>%
    dplyr::select(-t) %>%
    dplyr::select(-c)
  df <- dplyr::bind_rows(dfnew, dfold) 
  
  return(df)
}

# inner function to process a single series of data
input_data_c <- function(meta, value_matrix, series_name = "value", value_name = "value") {
  series_name_enquo <- rlang::enquo(series_name)
  value_name_enquo <- rlang::enquo(value_name)
  
  df <- matrix_ct_to_df_with_isoyear(meta, value_matrix= value_matrix)  %>%
    dplyr::filter(year > 1999) %>%
    dplyr::mutate(series_name = !!series_name_enquo) %>%
    dplyr::mutate(value_name = !!value_name_enquo) %>%
    dplyr::select(-t) %>%
    dplyr::select(-c)
  return(df)
  
}



