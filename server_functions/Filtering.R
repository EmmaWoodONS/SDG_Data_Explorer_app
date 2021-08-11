# function to take the user arguments (currently given in config) and filter the 
#   csv data based on these.
# args: dat - the csv data file for a single indicator
# returns a filtered tibble

# test data for two sets of dependencies (Region dependent on country,
#   Ethnicity dependent on Ethnic Group

#########################  TEST DATA #############################
dat <- data.frame(Year = rep("2100", 12),
                  Country = c(rep("England", 5), 
                              rep("Wales", 2),
                              "Scotland",
                              rep(NA, 4)),
                  Region = c(rep("London", 2), 
                             rep("East", 2), 
                             rep(NA, 8)),
                  Ethnic_group = c(rep("Asian/Asian British", 4),
                                   NA,
                                   rep("Asian/Asian British", 2),
                                   rep(NA, 5)),
                  Ethnicity = c(rep(c("Bangladeshi", "Chinese"), 2), 
                                NA,
                                "Bangladeshi", "Chinese",
                                rep(NA, 5)),
                  Ageband = c(rep(NA, 9),
                              rep("Adult", 3)),
                  Age = c(rep(NA, 9),
                          "20 to 29",
                          "30 to 39",
                          "All"),
                  Value = c(1:4, 10, 5:7, 28, 1, 2, 3))

char1 <- "Region"
char2 <- "Ethnicity" 
char3 <- "Age"
####################################################

filter_for_selections <- function(dat){
  
  special_columns <- c("Units", "Series")
  
  selected_characteristics <- c(char1, char2, char3, char4, char5, char6)
  number_of_selections <- sum(!is.na(selected_characteristics))
  
  special_characteristics <- NULL
  new_dat <- dat
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
  # Add a check: If the selected characteristics are in the 'NA_only_cols' vector,
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
  # or use all non-NA levels. E.g. if you wanted to look at Ethnicity, you would 
  # want all the ethnic groups to be selected.
  #
  # To do at a future time (maybe):
  # - If 'All' was selected print a statement to say that is what has been done

  
  
  ############ EDIT FROM HERE! ###################
  
  # select default level for required-but-not-selected disaggregations
  # TO DO: non_selected_choices needs to be a dataframe for the test data!
  
  non_selected_choices <- NULL
  info_to_select_levels <- new_dat
  for(i in 1:nrow(choice_required)) {
    
    selected <- choice_required$dependent_column[i]
    non_selected <- choice_required$dependent_on[i]
    
    available_levels <- new_dat %>% 
      filter(!is.na(!!as.name(selected)) &
               !is.na(!!as.name(non_selected))) %>% # I don't know if this is needed, but just in case
      distinct(!!as.name(non_selected)) %>% 
      # mutate(selected = selected) %>%
      mutate(available_for_selected = TRUE)
    
    # if(i > 1) last_keep_var <- keep_var
    keep_var <- paste0("keep", i)
    # if(i > 1) all_keep_vars <- vctrs::vec_c(last_keep_var, keep_var) 
    
    info_to_select_levels <- info_to_select_levels %>% 
      left_join(available_levels, by = non_selected) %>%
      mutate(!!as.name(keep_var) := ifelse(available_for_selected == TRUE &
                             !is.na(!!as.name(selected)), 
                           1, 0)) %>% 
      select(-available_for_selected)
  }
  
  # we need to drop any rows that don't have 1 in ANY of the the 'keep..'
  # columns. If we sum those columns, any that fill that criteria will = 0
  
  keep_column_start <- ncol(dat) + 1
  keep_column_end <- ncol(dat) + nrow(choice_required)
  keep_column_locations <- c(keep_column_start:keep_column_end)

  info_to_select_levels$keep <- rowSums(
    info_to_select_levels[, keep_column_locations], na.rm = TRUE)
  
  part_filtered_data <- info_to_select_levels %>% 
    filter(keep > 0) 
  part_filtered_data[, 1:(keep_column_start - 1)]
  
   

# continue from here (10/08/2021)



  # get the characteristics for which we only want NA rows
  NA_characteristics <- setdiff(relevant_unselected_characteristics, 
                                c(selected_characteristics, 
                                  unique(choice_required$dependent_on)))
  
  if(length(NA_characteristics) > 0) {
  filtered_data <- part_filtered_data %>%
    # remove of rows with entries (levels) for non-selected characteristics that 
    # none of the selected characteristics are dependent on: 
    filter(across(all_of(NA_characteristics), function(x) is.na(x)))
  } else {
    filtered_data <- part_filtered_data
  }

  # We could turn the following into a loop, but I'm struggling with it and 
  # it's not overly mportant, so leaving as individual statements for now.

  
  if(number_of_selections >= 1 & !is.na(levels1)[1]) {
    filtered_data <- filtered_data %>% 
      filter(!!rlang::sym(char1) %in% c(levels1))
  } 

  if(number_of_selections >= 2 & !is.na(levels2)[1]) {
    filtered_data <- filtered_data %>% 
      filter(!!rlang::sym(char2) %in% c(levels2))
  } 
  
  if(number_of_selections >= 3 & !is.na(levels3[1])) {
    filtered_data <- filtered_data %>% 
      filter(!!rlang::sym(char3) %in% c(levels3))
  }
  
  if(number_of_selections == 4 & !is.na(levels4)[1]) {
    filtered_data <- filtered_data %>% 
      filter(!!rlang::sym(char4) %in% c(levels4))
  }
  
 
  return(filtered_data)
  
}







