# function to take the user arguments (currently given in config) and filter the csv data based on these.
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
  
  # remove any columns that are entirely NA
  NA_counts <- sapply(new_dat, function(x) sum(is.na(x)))
  row_count <- nrow(new_dat)
  NA_only_cols <- names(new_dat)[row_count - NA_counts == 0] # cols to remove completely
  
  new_dat <- select(new_dat, -all_of(NA_only_cols))
  
  # cols_to_filter_for_All <- setdiff(relevant_unselected_characteristics, NA_only_cols)
  
 
  unselected_characteristics <- setdiff(names(new_dat), selections)
  relevant_unselected_characteristics <- setdiff(unselected_characteristics, irrelevant_variables)
  
  # some disagregations may not have been selected by the user, 
  # but are required in order to plot disagregations that have been selected.
  # For example, if region is selected but not country, and then country is 
  # filtered to only contain NAs (or Alls), there will be no data left to 
  # plot for region
  dependencies <- identify_complete_nesting(new_dat)
  required_columns <- unique(dependencies$dependent_on)[selections %in% dependencies$dependent_column]
  required_column_not_chosen <- required_columns[required_columns %not_in% selections]
  
  # what do we want to do with such columns? 
  # I suggest We just take either 'All' (which shouldn't ever be there, but accidentally is in 8-5-1),
  # or the first level of each non-selected but required disaggregation
  # and print a statement to say that if they want to see plots for all levels, 
  # they will need to select that characteristic in the drop-downs
  
  # select default level for required but non-selected disaggregations
  required_non_selected <- matrix(data = NA,
                                  nrow = 2, ncol = 2)
  for(i in 1:length(required_column_not_chosen)) {
    
    available_levels <- new_dat %>% 
      distinct(!!as.name(required_column_not_chosen[i])) %>% 
      pull(!!as.name(required_column_not_chosen[i]))
    
    # We would usually filter for NAs for columns that aren't selected by the user
    # however, these columns are special cases where doing so would get rid of 
    # rows that we need, so NA mustn't be an option
    available_levels <- available_levels[!is.na(available_levels)]
    
    # create a new selection characteristic, that will function like the char
    # variables in config.R
    # i.e. creates a new variable called e.g. char3 or char4 etc depending on 
    # how many characteristics have already been assigned values:
    
    if("All" %in% available_levels){
      required_non_selected[1, i] <- required_column_not_chosen[i]
      required_non_selected[2, i] <- "All"
    } else {
      required_non_selected[1, i] <- required_column_not_chosen[i]
      required_non_selected[2, i] <- available_levels[1]
    }
   
  }
  # turn resulting matrix into a dataframe ready to be joined to new_data (as a way of filtering)
  required_non_selected_df <- as.data.frame(required_non_selected)
  colnames(required_non_selected_df) <- required_non_selected[1, ]
  required_non_selected_df <- required_non_selected_df[-1, ]

  NA_characteristics <- setdiff(relevant_unselected_characteristics, c(selected_characteristics, required_column_not_chosen))
  
  filtered_data <- new_dat %>%
    filter(across(all_of(NA_characteristics), function(x) is.na(x)))

  # try a plot to see where we've got to
  filtered_data %>%
    filter(is.na(Sex) & is.na(Age)) %>%
    ggplot(data = .,
           aes(x = Year,
               y = Value)) +
    geom_point() +
    facet_grid(.~Region)
  
  # somehow need to figure out the filtering of required non-selected columns.
  # for 8-5-1:
  # Country needs to be England but only when plotting region, otherwise it should be NA
  # Occupation needs to be All but only when plotting age
  # this info is in `dependencies`
  
  
  
  
  
  
  
  
  
  
  
  
  # ----- still to look at and edit ->
  
  
  
  
  
  
  
  
  
  
  
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







