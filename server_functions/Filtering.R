# function to take the user arguments (currently given in config) and filter the 
#   csv data based on these.
# args: dat - the csv data file for a single indicator
# returns a filtered tibble

# when I was writing this I forgot that selections will only ever be cross-disaggregations,
# so it is a little over-engineered, but will work (I think).

# test data for two sets of dependencies (Region dependent on country,
#   Ethnicity dependent on Ethnic Group

#########################  TEST DATA #############################

## I forgot that selections can only be cross-disaggs so this is 
## not a realistic example
 
# dat <- data.frame(Year = rep("2100", 14),
#                   Country = c(rep("England", 5), 
#                               rep("Wales", 2),
#                               "Scotland",
#                               rep(NA, 6)),
#                   Region = c(rep("London", 2), 
#                              rep("East", 2), 
#                              rep(NA, 10)),
#                   Ethnic_group = c(rep("Asian/Asian British", 4),
#                                    NA,
#                                    rep("Asian/Asian British", 2),
#                                    rep(NA, 7)),
#                   Ethnicity = c(rep(c("Bangladeshi", "Chinese"), 2), 
#                                 NA,
#                                 "Bangladeshi", "Chinese",
#                                 rep(NA, 7)),
#                   Ageband = c(rep(NA, 9),
#                               rep("Adult", 3),
#                               NA, NA),
#                   Age = c(rep(NA, 9),
#                           "20 to 29",
#                           "30 to 39",
#                           "All",
#                           NA, NA),
#                   Industry = c(rep(NA, 12),
#                                "this", "that"),
#                   Value = c(1:4, 10, 5:7, 28, 1, 2, 3, 10, 20))

# # test with 3 dependent (nested) variables selected when all of the 
# # variables they are dependent on are not selected
# char1 <- "Region"
# char2 <- "Ethnicity" 
# char3 <- "Age"
# char4 <- NA

# # test with 2 dependent (nested) variables selected withou those they are 
# # dependent on, AND one where the column it is dependent on is also selscted
# char1 <- "Region"
# char2 <- "Ethnicity" 
# char3 <- "Age"
# char4 <- "Ageband"

####################################################
filter_series_units <- function(){

  if("Units" %in% names(dat)) {
    dat <- filter(dat, Units == Units_selection) 
  }
  if("Series" %in% names(dat)) {
    dat <- filter(dat, Series = Series_selection) 
  }
  
  return(dat)
}

get_special_characteristics <-function(){

  special_characteristics <- NULL
  if("Units" %in% names(dat)) {
    special_characteristics <- c(special_characteristics, "Units")
  }
  if("Series" %in% names(dat)) {
    special_characteristics <- c(special_characteristics, "Series")
  }
  return(special_characteristics)
}

filter_for_selections <- function(dat){
  
  selected_characteristics <- c(char1, char2, char3, char4)
  number_of_selections <- sum(!is.na(selected_characteristics))
  special_characteristics <- get_special_characteristics()
  selections <- c(selected_characteristics, special_characteristics)
  
  new_dat <- filter_series_units()
  
  irrelevant_variables <- c("Observation.status", 
                            "Unit.multiplier", "Unit.measure",
                            "GeoCode", "Value", "Year")
  
  # # remove any columns that are entirely NA (which may be the case as we have 
  # # filtered for Series and Units already)
  # # NOTE: commented out as I think it will break something further down the 
  # # script that requires knowledge of number of characteristics
  # NA_counts <- sapply(new_dat, function(x) sum(is.na(x)))
  # row_count <- nrow(new_dat)
  # NA_only_cols <- names(new_dat)[row_count - NA_counts == 0] # cols to remove completely
  
  #### TO DO:
  # Add a check: If the selected characteristics are in the 'NA_only_cols' vector,
  # A warning must be sent to the user to say that their selected cross-disaggregation
  # is not available with that Units and Series selection.
  # This can later be improved so that only valid levels are made available in 
  # the Series and Units drop-downs given the disaggregation selections
  ######
  
  # new_dat <- select(new_dat, -all_of(NA_only_cols))
  
  unselected_characteristics <- setdiff(names(new_dat), selections)
  relevant_unselected_characteristics <- setdiff(unselected_characteristics, irrelevant_variables)
  
  all_characteristics <- c(selections, # or should this be selected_characteristics?
                           relevant_unselected_characteristics)
  all_characteristics <- all_characteristics[!is.na(all_characteristics)]
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
    
    keep_var <- paste0("keep", i)
    non_selected_var <- paste0("non_selected", i)

    info_to_select_levels <- info_to_select_levels %>% 
      left_join(available_levels, by = non_selected) %>%
      mutate(!!as.name(keep_var) := ifelse(available_for_selected == TRUE &
                                             !is.na(!!as.name(selected)), 1, 0),
             !!as.name(non_selected_var) := ifelse(!is.na(!!as.name(non_selected)),
                                                   1, 0)) %>% 
      select(-available_for_selected)
  }
  
  # we need to drop any rows that don't have 1 in ANY of the the 'keep..'
  # columns. If we sum those columns, any that fill that criteria will = 0
  keep_column_start <- ncol(dat) + 1
  keep_column_end <- ncol(dat) + (nrow(choice_required)*2)
  keep_column_locations <- c(seq(keep_column_start, keep_column_end, 2))
  
  # we only want to get rid of the columns where the keep_count is 0 if 
  # choice_required is relevant for that row. i.e. If Age is not dependent on
  # anything, it will have a keep_count of 0, but we don't want to drop it.
  non_selected_column_locations <- keep_column_locations + 1
  
  
  info_to_select_levels$keep_count <- rowSums(
    info_to_select_levels[, keep_column_locations], na.rm = TRUE)
  info_to_select_levels$non_selected_count <- rowSums(
    info_to_select_levels[, non_selected_column_locations], na.rm = TRUE)

  
  part_filtered_data <- info_to_select_levels %>% 
    filter(keep_count > 0 | 
             non_selected_count == 0) 
  part_filtered_data <- part_filtered_data[, 1:(keep_column_start - 1)]
  
   

# continue from here (10/08/2021)



  # get the characteristics for which we only want NA rows
  NA_characteristics <- setdiff(relevant_unselected_characteristics, 
                                c(selections, # or should this be selected_characteristics?
                                  unique(choice_required$dependent_on))) # 
  
  if(length(NA_characteristics) > 0) {
  filtered_data <- part_filtered_data %>%
    # remove of rows with entries (levels) for non-selected characteristics that 
    # none of the selected characteristics are dependent on: 
    filter(across(all_of(NA_characteristics), function(x) is.na(x)))
  } else {
    filtered_data <- part_filtered_data
  }

  # The headline data has not been filtered out at this point.
  # I think we should give users the choice to display this or not, so we need
  # to identify it here
  
  # I can't get dplyr rowwise to work to count NAs, so switching away from tidyverse
  # for this
  filtered_data$nas_per_row <- apply(filtered_data, MARGIN = 1, 
                                     function(x) sum(is.na(x)))
  
  filtered_data_with_headline <- filtered_data %>% 
    mutate(headline = ifelse(nas_per_row == length(all_characteristics),
                                                   TRUE, FALSE)) %>% 
    select(-nas_per_row)
  
  # DATA is now filtered for disaggregations
  # however the user may only want to plot certain levels of those disaggs
  # so now we need to filter for selected levels
  # We could turn the following into a loop, but I'm struggling with it and 
  # it's not overly important, so leaving as individual statements for now.

  
  if(number_of_selections >= 1 & !is.na(levels1)[1]) {
    filtered_data_with_headline <- filtered_data_with_headline %>% 
      filter(!!rlang::sym(char1) %in% c(levels1) |
               headline == TRUE)
  } 

  if(number_of_selections >= 2 & !is.na(levels2)[1]) {
    filtered_data_with_headline <- filtered_data_with_headline %>% 
      filter(!!rlang::sym(char2) %in% c(levels2) |
               headline == TRUE)
  } 
  
  if(number_of_selections >= 3 & !is.na(levels3[1])) {
    filtered_data_with_headline <- filtered_data_with_headline %>% 
      filter(!!rlang::sym(char3) %in% c(levels3) |
               headline == TRUE)
  }
  
  if(number_of_selections == 4 & !is.na(levels4)[1]) {
    filtered_data_with_headline <- filtered_data_with_headline %>% 
      filter(!!rlang::sym(char4) %in% c(levels4) |
               headline == TRUE)
  }
  
  # I think all the NAs that are left will generally be "All". Change to All for
  # now and test to see if it works for all indicators
  
  final_data <- filtered_data_with_headline %>% 
    replace(is.na(.), "All")

  return(final_data)
  
}

# sometimes there isn't a headline for a non-selected disaggregation
# e.g. 3-4-1 will always have multiple values for type of disease.
# In order to correctly draw the plot, the user will have to select the
# desired levels of type of disease. I think these will have to pop up
# as drop-down menus before the plot is shown.

identify_extra_disaggregations <- function(dat){
  
  irrelevant_variables <- c("Observation.status", 
                            "Unit.multiplier", "Unit.measure",
                            "GeoCode", "Value", "Year", "headline")
  # selected_characteristics <- c(char1, char2, char3, char4, char5, char6)
  # unselected_characteristics <- setdiff(names(new_dat), selections)
  # relevant_unselected_characteristics <- setdiff(unselected_characteristics, 
  #                                                irrelevant_variables)
  # all_characteristics <- c(selected_characteristics, 
  #                          relevant_unselected_characteristics)
  # all_characteristics <- all_characteristics[!is.na(all_characteristics)]
  
  
  # columns to worry about:
  # not in selected columns and more than one level
  selected_characteristics <- c(char1, char2, char3, char4)
  special_characteristics <- get_special_characteristics()
  selections <- c(selected_characteristics, special_characteristics)
  selections[!is.na(selections)]
  all_characteristics <- setdiff(names(dat), irrelevant_variables) 
  
  not_selected <- setdiff(all_characteristics, selections)
  
  levels_count <- apply(dat, 2, function(x) length(unique(x))) 
  relevant_levels_count <- levels_count[names(levels_count) %in% not_selected]
  
  extra_dropdown_names <- names(relevant_levels_count[relevant_levels_count > 1])
  extra_dropdown_locations <- which(names(dat) %in% extra_dropdown_names)
    
  extra_dropdowns <- dat[, extra_dropdown_locations]
  
  return(extra_dropdowns)
}



