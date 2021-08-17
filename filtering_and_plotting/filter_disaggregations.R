# function to take the user arguments (currently given in config) and filter the 
#   csv data based on these.
# args: dat - the csv data file for a single indicator
# returns a list of two dataframes


####################################################
#  dat <- dat_with_All

filter_disaggregations <- function(dat){
  
  selections <- c(char1, char2, char3, char4)
  selections <- selections[!is.na(selections)]
  number_of_selections <- length(selections)
  
  unselected_characteristics <- setdiff(names(dat), selections)
  relevant_unselected_characteristics <- setdiff(unselected_characteristics, irrelevant_variables)
  
  real_values_in_selected <- dat 
  for(i in 1:number_of_selections){
    
    selected_var <- as.name(selections[i])
    
    real_values_in_selected <- real_values_in_selected %>% 
      filter(!!selected_var != "All" &
               !is.na(!!selected_var))
  }
  
  extra_dropdowns <- c()
  only_one <- NULL
  for(i in 1:length(relevant_unselected_characteristics)){
    
    unselected_var <- as.name(relevant_unselected_characteristics[i])
    unique_entries <- distinct(real_values_in_selected, !!unselected_var) %>% 
      rename(levels = !!unselected_var) %>% 
      mutate(disaggregation = relevant_unselected_characteristics[i])
    number_of_unique_entries <- nrow(unique_entries)
    
    if(number_of_unique_entries > 1){
      extra_dropdowns <- c(extra_dropdowns, relevant_unselected_characteristics[i])
      # extra_dropdowns <- bind_rows(extra_dropdowns,
      #                              unique_entries)
    } else {
      only_one <- bind_rows(only_one,
                            unique_entries)
      }
  }
  
  # Series and Units also need to be included in the extra dropdown so add them 
  # here to keep the original data structure
  if("Series" %in% names(dat)) {
    extra_dropdowns <- c(extra_dropdowns, "Series")
  }
  if("Units" %in% names(dat)) {
    extra_dropdowns <- c(extra_dropdowns, "Units")
  }
  
  if(length(extra_dropdowns) > 0){
    
    dropdown_column_locations <- which(names(dat) %in% extra_dropdowns)
    extra_dropdown_levels <- real_values_in_selected[, dropdown_column_locations] %>% 
      distinct()

  } else { extra_dropdown_levels <- "None" }
  
  if(length(only_one) > 0){
    unselected_var_level <- only_one %>% 
      pivot_wider(names_from = disaggregation,
                  values_from = levels)
  } else {unselected_var_level <- "None"}
    
  # real_values_in_selected doesn't include headline ('All') rows,
  # so here we do almost the same filter again, but keep the 'All' rows in
  filtered <- dat 
  for(i in 1:number_of_selections){
    
    selected_var <- as.name(selections[i])
    
    filtered <- filtered %>% 
      filter(!is.na(!!selected_var)) 

  }

  # filtered currently contains rows from unselected columns, that we don;t want
  # in the final data, so this right_join gets rid of them.
  if(is.data.frame(unselected_var_level)){
    filtered <- filtered %>%
      right_join(unselected_var_level, by = 
                   c(names(unselected_var_level)))
  }  

  return(list(filtered, extra_dropdown_levels))
  
}



