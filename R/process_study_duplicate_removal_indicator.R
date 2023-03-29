process_study_duplicate_removal_indicator <- function(data, year_start, year_end, iso_alpha_3_code, entry_year, entry_type) {
  duplicates <- data %>%
    dplyr::filter(iso_alpha_3_code == !!iso_alpha_3_code & year_start == !!year_start & year_end == !!year_end) %>%
    dplyr::filter(!(entry_year == !!entry_year & entry_type == !!entry_type)) %>%
    dplyr::mutate(entry_rank = ifelse(entry_type == "extract", 1,
                                      ifelse(entry_type == "vr_special_case", 2,
                                             ifelse(entry_type == "consult", 3, 0))))
  
  rank_of_current <- ifelse(entry_type == "extract", 1,
                            ifelse(entry_type == "vr_special_case", 2,
                                   ifelse(entry_type == "consult", 3, 0)))
  removal_logic_i <- c()
  if(nrow(duplicates) > 0) {#i.e. if there is duplicate present
    for(i in 1:nrow(duplicates)) {
      duplicate <- duplicates[i,]
      # if(duplicate$entry_year != entry_year) { #if entry type is the same use entry year to determine removal
      #   removal_logic_i[i] <- duplicate$entry_year > entry_year
      if(duplicate$entry_type == entry_type) { #if entry type is the same use entry year to determine removal
        removal_logic_i[i] <- duplicate$entry_year > entry_year
      } else { #otherwise use the type to determine removal
        removal_logic_i[i] <- duplicate$entry_rank > rank_of_current
      }
    }
  }
  removal_logic <- any(removal_logic_i)
  return(removal_logic)
}