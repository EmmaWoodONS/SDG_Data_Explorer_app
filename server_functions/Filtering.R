# function to take the user arguments (currently given in config) and filter the 
#   csv data based on these.
# args: dat - the csv data file for a single indicator
# returns a filtered tibble

filter_for_selections <- function(dat){
  
  special_columns <- c("Units", "Series")
  
  selected_characteristics <- c(char1, char2, char3, char4, char5, char6)
  number_of_selections <- sum(!is.na(selected_characteristics))
  
  special_characteristics <- NULL
  if("Units" %in% names(dat)) {
    new_dat <- filter(dat, Units == Units_selection) 
    special_characteristics <- c(special_characteristics, "Units")
    }
  if("Series" %in% names(dat)) {
    new_dat <- filter(dat, Series = Series_selection) 
    special_characteristics <- c(special_characteristics, "Series")
  }
  
  selections <- c(selected_characteristics, special_characteristics)
    
  irrelevant_variables <- c("Observation.status", 
                            "Unit.multiplier", "Unit.measure",
                            "GeoCode", "Value", "Year")
  
  # remove any columns that are entirely NA (which may be the case as we have 
  # filtered for Series and Units already)
  NA_counts <- sapply(new_dat, function(x) sum(is.na(x)))
  row_count <- nrow(new_dat)
  NA_only_cols <- names(new_dat)[row_count - NA_counts == 0] # cols to remove completely
  
  #### TO DO:
  # Add a check: If the selected characteristicss are in the 'NA_only_cols' vector,
  # A warning must be sent to the user to say that their selected cross-disaggregation
  # is not available with that Units and Series selection.
  # This can later be improved so that only valid levels are made available in 
  # the Series and Units drop-downs given the disaggregation selections
  ######
  
  new_dat <- select(new_dat, -all_of(NA_only_cols))
  
  unselected_characteristics <- setdiff(names(new_dat), selections)
  relevant_unselected_characteristics <- setdiff(unselected_characteristics, irrelevant_variables)
  
  # some disagregations may not have been selected by the user, 
  # but are required in order to plot disagregations that have been selected.
  # For example, if region is selected but not country, and then country is 
  # filtered to only contain NAs (or Alls), there will be no data left to 
  # plot for region
  dependencies <- identify_complete_nesting(new_dat)
  
  choice_required <- dependencies %>% 
    filter(dependent_on %not_in% selections & 
             dependent_column %in% selections)

  # what do we want to do with such columns? 
  # -Remove rows that don't exist once the NAs are removed from the dependent 
  # variable (i.e. if Country is required but not selected [dependent_on column],
  # when you filter out NAs in the Region [dependent_column] only England will remain)
  # - If more than one option remains, either filter dependent_on for 'All' if
  # it exists (it shouldn't ever be there, but e.g. it accidentally is in 8-5-1),
  # or use the first level 
  # - in the latter 2 cases ('All' or first level) print a statement to say that
  # if the user wants to see plots for all levels, 
  # they will need to select that characteristic in the drop-downs
  
  # select default level for required but non-selected disaggregations
  non_selected_choices <- NULL
  for(i in 1:nrow(choice_required)) {
    
    selected <- choice_required$dependent_column[i]
    non_selected <- choice_required$dependent_on[i]
    
    available_levels <- new_dat %>% 
      filter(!is.na(!!as.name(selected)) &
               !is.na(!!as.name(non_selected))) %>% # I don't know if this is needed, but just in case
      distinct(!!as.name(non_selected)) 
    
    if(nrow(available_levels) == 1) {
      non_selected_choices <- bind_cols(available_levels)
      
    } else {
      
      if("All" %in% available_levels){
        
        chosen_level <- available_levels %>% 
          filter(!!as.name(non_selected) == "All") 
        
      } else { chosen_level <- available_levels[i, ] }
      
      non_selected_choices <- bind_cols(chosen_level)
      
    }
  }

  # get the characteristics for which we only want NA rows
  NA_characteristics <- setdiff(relevant_unselected_characteristics, 
                                c(selected_characteristics, 
                                  names(non_selected_choices)))
  
  filtered_data <- new_dat %>%
    # remove of rows with entries (levels) for non-selected characteristics that 
    # none of the selected characteristics are dependent on: 
    filter(across(all_of(NA_characteristics), function(x) is.na(x))) %>% 
    # remove rows for non-selected characteristics for which a particular entry 
    # is required (e.g. "England" for Country when Region is a selected char), or 
    # for which there are multiple possible entries (where we have set a default 
    # of 'All', or the first one alphanumerically)
    right_join(non_selected_choices, by = choice_required$dependent_on)


  
  
    # ----- still to look at and maybe edit ->
  
  
  
  
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







