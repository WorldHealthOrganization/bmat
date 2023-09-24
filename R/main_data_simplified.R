main_data_simplified <- function(main_data) {
  main_data <- main_data %>% 
    dplyr::select(year_start, 
                  year_end, 
                  final_mmr, 
                  mmr, 
                  final_mmr_before_crisis, 
                  env_total_lifetables, 
                  live_births, 
                  live_births_lifetables, 
                  live_births_calculated_from_birthsdata,
                  env_total_calculated_from_lifetables)
  return(main_data)
}
