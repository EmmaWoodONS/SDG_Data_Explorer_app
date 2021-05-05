# function to take the user arguments (currently given in config) and filter the csv data based on these.
# args: dat - the csv data file for a single indicator
# returns a filtered tibble

filter_for_selections <- function(dat){
  
  irrelevant_variables <- c("Observation.status", "Unit.multiplier", "Unit.measure", "GeoCode", "Value", "Year")
  
  selected_characteristics <- c(char1, char2, char3, char4)
  
  unselected_characteristics <- setdiff(names(dat), selected_characteristics)
  relevant_unselected_characteristics <- setdiff(unselected_characteristics, irrelevant_variables)
  
  unselected_charateristics_removed <- dat %>%
    filter_at(vars(all_of(relevant_unselected_characteristics)), all_vars(is.na(.))) 
  
  number_of_selections <- sum(!is.na(selected_characteristics))
  
  if(number_of_selections >= 1) {
    filtered_data <- unselected_charateristics_removed %>% 
      filter(!!rlang::sym(char1) %in% c(levels1))
  }
  
  if(number_of_selections >= 2) {
    filtered_data <- filtered_data %>% 
      filter(!!rlang::sym(char2) %in% c(levels2))
  }
  
  if(number_of_selections >= 3) {
    filtered_data <- filtered_data %>% 
      filter(!!rlang::sym(char3) %in% c(levels3))
  }
  
  if(number_of_selections == 4) {
    filtered_data <- filtered_data %>% 
      filter(!!rlang::sym(char4) %in% c(levels4))
  }
  
  # if(any(is.na(levels1))) {
  #   headlines_as_headline <- filtered_data %>% 
  #     mutate(sym(char1) = ifelse(is.na(!!(enquo(char1))), "1. headline", !!(enquo(char1))))
  # }
  # 
  # if(!is.na(char2) & any(is.na(levels2))) {
  #   levels2 <- replace_na(levels2, "1. headline")
  # }
  # 
  # if(!is.na(char3) & any(is.na(levels3))) {
  #   levels3 <- replace_na(levels2, "1. headline")
  # }
  # 
  # if(!is.na(char4) & any(is.na(levels4))) {
  #   levels4 <- replace_na(levels4, "1. headline")
  # }
  # 
  # headlines_specified <- filtered_data %>% 
  #   mutate()
  
  return(filtered_data)
  
}







