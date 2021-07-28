# used to identify columns that are completely nested in another column
# e.g. Region is completely nested in Country, and only has values when Country 
# is "England".
# In such cases, if the user doesn't select Country as one of the options, Country
# will become a `relevant_unselected_characteristic` and will be filtered to be 
# NA (or All). All rows containing 'Region' values would then be filtered out.

# We don't want this to happen so need to be able to identify such column pairs


identify_complete_nesting <- function(dat){
  
  non_disaggregation_columns <- c("Observation.status", 
                                  "Unit.multiplier", "Unit.measure",
                                  "GeoCode", "Value", "Year")
  disagg_dat <- select(dat, -all_of(non_disaggregation_columns))
  
  dependent_columns <- data.frame(dependent_column = NA, dependent_on = NA)
  
  for(i in 1:ncol(disagg_dat)) {
    
    target_column <- names(disagg_dat)[i]
    
    target_column_as_NA <- disagg_dat %>% 
      filter(is.na(!!as.name(target_column)))
    
    for(j in 1:ncol(disagg_dat)) {
      
      if( i != j){
        
        comparison_column <- names(disagg_dat)[j]
        
        number_of_rows_left <- target_column_as_NA %>% 
          filter(!is.na(!!as.name(comparison_column))) %>% 
          nrow()
        
        if(number_of_rows_left == 0) {
          
          dependent_columns <- add_row(dependent_columns,
                                       'dependent_column' = comparison_column,
                                       'dependent_on' = target_column)
        }
      }
    }
  }
  
  dependent_columns <- filter(dependent_columns, !is.na(dependent_column))
  return(dependent_columns)
  
}