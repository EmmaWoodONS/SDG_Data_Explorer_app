# Author: Emma Wood
# Date started: 23/04/2021
# Purpose: Create control sheet for Shiny app

rm(list = ls())

library(tidyr)
library(dplyr)

`%not_in%` <- Negate(`%in%`)

remove_unused_columns <- function(dat, cols_to_remove = unused_columns) {
  dat[ , colnames(csv) %not_in% cols_to_remove]
}

#----------

csv <- read.csv("Y:\\Data Collection and Reporting\\Jemalex\\CSV\\indicator_3-1-2.csv", na.strings = "") %>% 
  mutate_if(is.factor, as.character)

unused_columns <- c("Year", "Observation.status", "Unit.multiplier", "Unit.measure", "GeoCode", "Value")
all_variables <- remove_unused_columns(csv)


# is_disagg_nested? i.e. does a column require a single value in any other column (not including NAs). 
# Need to know this to know whether an NA should actually be 'All'
# For example, if 
incompletely_nested_variables <- NULL
completely_nested_variables <- NULL

for(i in 1:ncol(all_variables)) {
  
  target_column_name <- colnames(all_variables)[i]
  
  target_column_options <- all_variables %>% 
    filter(!is.na(!!as.name(target_column_name)))

  unique_target_column_options <- distinct(target_column_options, !!as.name(target_column_name))
  
  for(j in 1:ncol(all_variables)) {
    
    check_column_name <- colnames(all_variables)[j]
    
    if(target_column_name != check_column_name) {
      
      unique_check_column_options <- target_column_options %>% 
        filter(!is.na(!!as.name(check_column_name))) %>% 
        distinct(!!as.name(target_column_name)) %>% 
        rename(unique_target_column_options = all_of(target_column_name)) %>% 
        .$unique_target_column_options
      
      
      if(length(unique_check_column_options) < nrow(unique_target_column_options) &
         length(unique_check_column_options) != 0) {
        
        number_check_column_options <- length(unique_target_column_options)
        incompletely_nested <- data.frame(nested_variable = rep(check_column_name, times = number_check_column_options),
                                          nested_within = rep(target_column_name, times = number_check_column_options),
                                          values_given_for = unique_check_column_options)
        incompletely_nested_variables <- bind_rows(incompletely_nested_variables, incompletely_nested)
        
      }

    }
  }
  
}

nested_variables <- as.character(incompletely_nested_variables$nested_variable)
nested_within <- as.character(incompletely_nested_variables$nested_within)
values_given_for <- as.character(incompletely_nested_variables$values_given_for)

# where All does not make sense (e.g. All for Welsh-only Health Boards when England is selected), keep NAs
incompletely_nested_NAs_to_all <- all_variables 

for(i in 1:nrow(incompletely_nested_variables)) {
  
  nested_variable_colname <- sym(nested_variables[i])
  nested_within_colname <- sym(nested_within[i])
  
  incompletely_nested_NAs_to_all <- incompletely_nested_NAs_to_all %>% 
    # mutate(Region = ifelse(Country == "England" & is.na(Region), "England", Region)) : this is what the line below is doing as an example from 3.1.2
    mutate(!!nested_variable_colname := ifelse(!!nested_within_colname == values_given_for[i] &
                                                is.na(!!nested_variable_colname),
                                    "All", !!nested_variable_colname))

  }

final_incompletely_nested_NAs_to_all <- incompletely_nested_NAs_to_all %>% 
  select(incompletely_nested_variables$nested_variable)

all_other_NAs_to_all <- all_variables %>% 
  select(-incompletely_nested_variables$nested_variable) %>% 
  replace(is.na(.), "All")

final_NAs_to_all <- bind_cols(final_incompletely_nested_NAs_to_all, all_other_NAs_to_all)
  

# Alternative way to do the NA replacement  
# number_of_nested_variables <- nrow(distinct(nested_variables, nested_variable))
# all_variables_with_nesting <- select(all_variables, 
#                                      nested_variables$nested_within, 
#                                      nested_variables$nested_variable)
# na_to_all_for_nested <- NULL
# 
# for(variable in 1:number_of_nested_variables) {
#   
#   active_rows <- nested_variables %>% 
#     mutate_if(is.factor, as.character) %>% 
#     filter(nested_variable == nested_variable[variable])
#   
#   columns_to_select <- c(active_rows$nested_variable, 
#                          active_rows$nested_within)
#   
#   na_to_all <- all_variables_with_nesting %>% 
#     select(columns_to_select) %>% 
#     mutate(!!as.character(active_rows$nested_variable) := 
#              ifelse(is.na(!!sym(active_rows$nested_variable)) & 
#                       !!sym(active_rows$nested_within) %in% active_rows$values_given_for, 
#                     "All", !!sym(active_rows$nested_variable))) %>% 
#     select(!!sym(active_rows$nested_variable))
#     
#   na_to_all_for_nested <- bind_cols(na_to_all_for_nested, na_to_all)
# }
# 
# nas_and_alls_fixed <- bind_cols(na_to_all_for_nested, na_to_all_for_non_nested)
  
# TO DO: create the 'Characteristics' columns and work out how to put them in every possible order
# The above code just gives the 'Levels' columns shown in indicator_selection_flowchart.pptx




# # get all combinations of column names------------
# 
# number_of_columns <- ncol(all_variables)
# 
# unique_rows <- distinct(all_variables)
# 
# available_variables <- unique_rows
# 
# location_of_values <- which(!is.na(unique_rows), arr.ind = TRUE)
# available_variables[location_of_values] <- names(unique_rows)[location_of_values[, "col"]]
# unique_available_variables <- distinct(available_variables)
# 
# # # create blank dataframe to store the named disaggregations
# # top_disaggs <- data.frame(matrix(ncol = max_disaggs, nrow = 0))
# 
# # at the moment the table is 1 if present, 0 if not, this loop replaces the 1s with the name of the disaggregation
# for (i in 1:nrow(available_variables)) {
# 
#   for(j in 1:ncol(available_variables)) {
# 
#     available_variables[i,j] <- ifelse(available_variables[i,j] == 1, names(available_variables)[j], 0)
# 
#   }
# }
# 
# 
# # create a new column that tells us whether a set of disaggregations is nested within another
# # for example if we have Sex x Age, we dont also need to list Sex and Age on their own as they are nested within  Sex x Age
# # because the data are arranged by number of disaggregations with the highest at the top, the first row cannot be nested in another, so we want to keep it
# available_variables$keep <- NA
# available_variables$keep[1] <- TRUE
# 
# 
# for (i in 2:nrow(available_variables)) {
# 
#   # create a vector that gives the disaggregations available in the current row
#   new_disagg_list <- unique(unlist(available_variables[i, ], use.names=FALSE))
#   new_disagg_list <- new_disagg_list[new_disagg_list %notin% c("TRUE", "FALSE", "0", NA)]
# 
#   # set up a counter
#   tally <- 0
# 
#   for (j in 1:i-1) {
# 
#     # create a vector that gives the disaggregations available in the rows up to this point
#     existing_disagg_list <- unique(unlist(available_variables[j, ], use.names=FALSE))
#     existing_disagg_list <- existing_disagg_list[existing_disagg_list %notin% c("TRUE", "FALSE", "0")]
# 
#     # check whether the current row is a nested version of a previous row
#     matches <- new_disagg_list %in% existing_disagg_list
#     # if it is not nested add 1 to the tally
#     tally <- ifelse(FALSE %in% matches, tally + 1, tally)
# 
#     if (j == i-1) {
# 
#       # if the current row is not nested in ANY of the previous rows, we want to keep it
#       available_variables$keep[i] <- ifelse(tally == i, TRUE, FALSE)
# 
#     }
# 
#   }
# }
# 
# # remove nested rows
# dissags_of_interest <- filter(available_variables, keep == TRUE)
# 
# # create a dataframe to store the dissagregations, so that there aren't lots of unnecessary NAs
# output_table <- data.frame(matrix(ncol = max_disaggs, nrow = nrow(dissags_of_interest)))
# 
# for (i in 1:nrow(dissags_of_interest)) {
# 
#   disagg_to_output <- unique(unlist(dissags_of_interest[i, ], use.names=FALSE))
#   disagg_to_output <- disagg_to_output[disagg_to_output %notin% c("TRUE", "FALSE", "0", NA)]
# 
#   output_table[i, 1:length(disagg_to_output)] <- disagg_to_output
# 
# }
# 
# # # do the same but for all possibilities
# # output_table_all <- data.frame(matrix(ncol = max_disaggs, nrow = nrow(available_variables)))
# # for (i in 1:nrow(available_variables)) {
# #
# #  all_disagg_to_output <- unique(unlist(available_variables[i, ], use.names=FALSE))
# #  all_disagg_to_output <- all_disagg_to_output[all_disagg_to_output %notin% c("TRUE", "FALSE", "0", NA)]
# #
# #   output_table_all[i, 1:length(all_disagg_to_output)] <- all_disagg_to_output
# #
# # }
# 
# # It may be useful for a Shiny app to be able to see the columns of each row in every possible order
# # e.g. if sex is selected first X1 is filtered to only include sex, but if age is selected first, all the same subsequent options are still available
# # look up how to order an array in every possible sequence
# for (i in 1:nrow(output_table)) {
# 
# }
# 
