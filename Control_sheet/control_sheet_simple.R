# Author: Emma Wood
# Date started: 23/04/2021
# Purpose: Create control sheet for Shiny app that doesn't allow users to select levels as they go along

rm(list = ls())

library(tidyr)
library(dplyr)
library(gtools)

`%not_in%` <- Negate(`%in%`)

remove_unused_columns <- function(dat, cols_to_remove = unused_columns) {
  dat[ , colnames(csv) %not_in% cols_to_remove]
}


#----------
indicator_numbers <- read.csv("indicators.csv")
disagg_lookup <- read.csv('Disaggregation_group_lookup.csv')

unused_columns <- c("Year", "Observation.status", "Unit.multiplier", "Unit.measure", "GeoCode", "Value")

csv_folder_filepath <- "Y:\\Data Collection and Reporting\\Jemalex\\CSV"
available_files <- list.files(csv_folder_filepath)

control_sheet <- NULL
for(i in 1:nrow(indicator_numbers)){
  
  indicator_number <- as.character(indicator_numbers$indicator[i])
  csv_filename <- paste0("indicator_", indicator_number, ".csv")
  
  if(csv_filename %in% available_files){
    
    # because a fatal error is thrown if we try to read in a blank csv, we need to catch the error using try
    csv <- try( read.csv(paste0(csv_folder_filepath, "\\", csv_filename), na.strings = "") %>% 
                  mutate_if(is.factor, as.character),
                silent = TRUE)
    
    if(is.data.frame(csv) == TRUE) {
      
      relevant_unused_columns <- unused_columns[unused_columns %in% names(csv)]
      disaggs <- select(csv, -all_of(relevant_unused_columns))
      
      non_stats_indicator <- data.frame(Group = c("A", "B", NA, "A", "B", NA)) %>% 
        mutate(Group = as.character(Group))
      
      if(ncol(disaggs) > 0 & identical(disaggs, non_stats_indicator) == FALSE){
        
        # get a unique list of occurrences, with the name of the column given instead of the level 
        #   e.g. under the Age column, the value "20 to 25" would be replaced with "Age"
        disaggs_simplified <- disaggs %>% 
          transmute_all(function(x) (ifelse(!is.na(x), deparse(substitute(x)), NA))) %>% 
          distinct() 
        
        # add the corresponding variable columns and variable (disaggregation) group name - still to be done
        disagg_lookup_cleaned <- mutate_all(disagg_lookup, .funs = tolower)
        
        disaggs_cleaned <- disaggs_simplified %>% 
          rename_with( ~ tolower(gsub(".", " ", .x, fixed = TRUE))) %>% 
          transmute_all( ~ tolower(gsub(".", " ", .x, fixed = TRUE)))
        
        available_groups <- disagg_lookup_cleaned %>% 
          filter(Disaggregation %in% colnames(disaggs_cleaned))
        
        disagg_combos_to_keep <- disaggs_cleaned %>% 
          select(available_groups$Disaggregation) 
        
        disaggs_to_keep <- names(disagg_combos_to_keep)
        
        if(length(disaggs_to_keep) > 0){
          ###################################
          # build control sheet -------------
          # get all possible variable orders ----
          
          # TO DO: get maximum number of interactions to make permutations() faster when there are a lot of disaggs (e.g. 16.1.3)
          all_combinations <- gtools::permutations(length(disaggs_to_keep), length(disaggs_to_keep), disaggs_to_keep)
          all_combinations_df <- as.data.frame(all_combinations)
          
          control_sheet_all_orders <- NULL
          
          for(i in 1:nrow(all_combinations_df)) {
            
            order_of_vars <- c(as.matrix(all_combinations_df[i, ]))
            new_combination <- select(disagg_combos_to_keep, all_of(order_of_vars)) 
            
            # rename columns to var1, var2 etc
            for(j in 1:ncol(new_combination)) {
              colnames(new_combination)[j] <- paste0("variable", j)
            }
            
            control_sheet_all_orders <- bind_rows(control_sheet_all_orders, new_combination)
            
          }
          
          # test:
          # control_sheet_all_orders <- read.csv('to_test_row_removal.csv')
          
          # remove any instances where the preceding column is NA
          number_of_variables <- ncol(control_sheet_all_orders)
          control_sheet_cleaned <- control_sheet_all_orders
          
          if(number_of_variables > 1){
            
            for(i in 1:(number_of_variables-1)) {
              
              column_name <- sym(paste0("variable", i))
              
              if(i == 1){
                
                control_sheet_cleaned <- control_sheet_cleaned %>% 
                  filter(!is.na(!!column_name))
                
              } else {
                
                next_column_name <- sym(paste0("variable", i + 1))
                
                control_sheet_cleaned <- control_sheet_cleaned %>%
                  mutate(remove = ifelse(is.na(!!column_name) & !is.na(!!next_column_name),
                                         TRUE, FALSE)) %>% 
                  filter(remove == FALSE) %>% 
                  select(-remove)
              }
            }
            
          } else { # this is when there is only one variable, but there is still an NA row (at time of writing e.g. 10-1-1)
            
            control_sheet_cleaned <- control_sheet_cleaned %>% 
              filter(!is.na(variable1))
            
          }
          
          control_sheet_no_duplicates <- control_sheet_cleaned %>% 
            distinct() %>% 
            mutate(Indicator = indicator_number)
          
          control_sheet_with_groups <- control_sheet_no_duplicates
          for(i in 1:number_of_variables){
            
            group_column_name <- sym(paste0("variable_group_", i))
            column_name <- paste0("variable", i)
            
            control_sheet_with_groups <- control_sheet_with_groups %>% 
              left_join(available_groups, by = setNames("Disaggregation", column_name)) %>% 
              rename(!!group_column_name := Group)
          }
          
          control_sheet <- bind_rows(control_sheet, control_sheet_with_groups)
        }
      }
      
      print(indicator_number)
      
    }
  }
}

# TO DO: 
# - does it matter that 'country, NA' is a row, as well as 'country, region'?
# - add variable group

