# Author: Emma Wood
# Date started: 23/04/2021
# Purpose: Create control sheet for Shiny app

rm(list = ls())

library(tidyr)
library(dplyr)
library(gtools)

`%not_in%` <- Negate(`%in%`)

remove_unused_columns <- function(dat, cols_to_remove = unused_columns) {
  dat[ , colnames(csv) %not_in% cols_to_remove]
}

#----------
source("config.R")

csv <- read.csv(paste0("Y:\\Data Collection and Reporting\\Jemalex\\CSV\\indicator_", indicator, ".csv"), na.strings = "") %>% 
  mutate_if(is.factor, as.character)

disagg_lookup <- read.csv('../Control_sheet/Disaggregation_group_lookup.csv')

unused_columns <- c("Year", "Observation.status", "Unit.multiplier", "Unit.measure", "GeoCode", "Value")
# all_variables <- remove_unused_columns(csv)

unused_column_numbers <- which(colnames(csv) %in% unused_columns)

all_variables <- csv

# is_disagg_nested? i.e. does a column require a single value in any other column (not including NAs). 
# Need to know this to know whether an NA should actually be 'All'
# For example, if 
incompletely_nested_variables <- NULL
completely_nested_variables <- NULL

for(i in 1:ncol(all_variables)) {
  
  if(i %not_in% unused_column_numbers){
    
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

##########################################################################


# number_of_columns <- ncol(final_NAs_to_all)
available_variables <- distinct(final_NAs_to_all)

# location_of_values <- which(!is.na(available_variables), arr.ind = TRUE)
# available_variables[location_of_values] <- names(available_variables)[location_of_values[, "col"]]
unique_available_variables <- distinct(available_variables)

# add the corresponding variable columns and variable (disaggregation) group name
disagg_lookup_cleaned <- mutate_all(disagg_lookup, .funs = tolower)
available_levels <- final_NAs_to_all %>% 
  distinct() %>% 
  rename_with( ~ tolower(gsub(".", " ", .x, fixed = TRUE)))

groups_mapped <- disagg_lookup_cleaned %>% 
  filter(Disaggregation %in% colnames(available_levels))

variables_and_levels <- available_levels %>% 
  select(groups_mapped$Disaggregation)

###################################
# build control sheet -------------

# add the corresponding variable columns and variable group name (variable = disaggregation)
control_sheet <- variables_and_levels
for (i in 1:ncol(variables_and_levels)) {
  
  variable <- colnames(variables_and_levels)[i]
  variable_colname <- sym(paste0("variable_", i))
  variable_group <- as.character(groups_mapped$Group[which(groups_mapped$Disaggregation == variable)])
  variable_group_colname <- sym(paste0("variable_group_", i))
  
  colnames(control_sheet)[i] <- paste0("level_", i)
  level_column <- sym(colnames(control_sheet)[i])
  
  control_sheet <- control_sheet %>% 
    mutate(!!variable_colname := ifelse(!is.na(!!level_column), variable, NA),
           !!variable_group_colname := ifelse(!is.na(!!level_column), variable_group, NA))
}

# get all possible variable orders ----

# all possible combinations of all possible variable groups results in a vector of size 1.6Gb, which is too big to be worth it
# so get the possible combinations for each indicator separately, as most wont have more than a few
all_combinations <- gtools::permutations(length(groups_mapped$Group), length(groups_mapped$Group), groups_mapped$Group)
all_combinations_df <- as.data.frame(all_combinations)

# TO DO: 
# - on new script: make a control sheet where we don't care about the levels (I think this would be fine - levels can be selected after indicator)
# - on this script: put the control sheet in every possible order 
