compare_input_data_c <- function(metanew, metaold, value_matrixnew, value_matrixold) {
  dfnew <- matrix_ct_to_df_with_isoyear(metanew, value_matrix= value_matrixnew, value_name = "value")  %>%
    dplyr::filter(year > 1999) %>%
    dplyr::rename(value_new = value)%>%
    dplyr::select(-t) %>%
    dplyr::select(-c)
  dfold <- matrix_ct_to_df_with_isoyear(metaold, value_matrix= value_matrixold, value_name = "value")  %>%
    dplyr::filter(year > 1999) %>%
    dplyr::rename(value_old = value) %>%
    dplyr::select(-t)%>%
    dplyr::select(-c)
  df <- dplyr::full_join(dfnew, dfold, by = c("iso_alpha_3_code", "year"))
  df <- df %>%
    dplyr::mutate(change_abs = abs(value_old - value_new) / (value_new)*100) %>%
    dplyr::mutate(change = (value_old - value_new) / (value_new)*100) 
  
  
  
  return(df)
  
}
