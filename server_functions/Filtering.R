# function to take the user arguments (currently given in config) and filter the csv data based on these.
# args: dat - the csv data file for a single indicator
# returns a filtered tibble

filter_for_selections <- function(dat){
  
  special_columns <- c("Units", "Series")
  irrelevant_variables <- c("Observation.status", 
                            "Unit.multiplier", "Unit.measure",
                            special_columns,
                            "GeoCode", "Value", "Year")
  
  selected_characteristics <- c(char1, char2, char3, char4)
  unselected_characteristics <- setdiff(names(dat), selected_characteristics)
  relevant_unselected_characteristics <- setdiff(unselected_characteristics, irrelevant_variables)
  
  if("Units" %in% names(dat)) {
    dat <- filter(dat, Units == Units_selection) 
  }
  if("Series" %in% names(dat)) {
    dat <- filter(dat, Series = Series_selection) 
  }
  
  filtered_data <- dat %>%
    filter_at(vars(all_of(relevant_unselected_characteristics)), all_vars(is.na(.))) 
  
  number_of_selections <- sum(!is.na(selected_characteristics))
  
  if(number_of_selections >= 1 & !is.na(levels1)) {
    filtered_data <- filtered_data %>% 
      filter(!!rlang::sym(char1) %in% c(levels1))
  } 

  if(number_of_selections >= 2 & !is.na(levels2)) {
    filtered_data <- filtered_data %>% 
      filter(!!rlang::sym(char2) %in% c(levels2))
  } 
  
  if(number_of_selections >= 3 & !is.na(levels3)) {
    filtered_data <- filtered_data %>% 
      filter(!!rlang::sym(char3) %in% c(levels3))
  }
  
  if(number_of_selections == 4 & !is.na(levels4)) {
    filtered_data <- filtered_data %>% 
      filter(!!rlang::sym(char4) %in% c(levels4))
  }
  
 
  return(filtered_data)
  
}







