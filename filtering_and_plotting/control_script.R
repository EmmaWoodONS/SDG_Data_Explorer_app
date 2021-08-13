library(tidyr)
library(ggplot2)
library(dplyr)
`%not_in%` <- Negate(`%in%`)

setwd("D:\\coding_repos\\SDG_Data_Explorer_app\\filtering_and_plotting")

source("config.R") 
source("user_selections.R")
source("correctly_replace_NA_with_All.R")
source("filter_disaggregations.R")

time_start <- Sys.time()

dat <- read.csv(paste0(csv_filepath, indicator, ".csv"),
                na.strings=c("","NA")) %>%
  mutate_if(is.factor, as.character)

dat_with_All <- correctly_replace_NA_with_All(dat)
filtered_data_and_extra_dropdowns <- filter_disaggregations(dat_with_All)

filtered_data <- filtered_data_and_extra_dropdowns[[1]]
extra_dropdowns <- filtered_data_and_extra_dropdowns[[2]]

# # if we want to add filtering by level un-hash:
# filtered_levels <- filter_levels(filtered_data)

# the user will then need to select the required levels from the extra drop-downs 
# Until we have these on the platform, just select the first row 
# (for testing and development purposes only)
extra_dropdown_row <- 11
if(is.data.frame(extra_dropdowns)){
  
  further_selections <- extra_dropdowns[extra_dropdown_row, ]

  # This loop and if statement are needed because for example, if region is 
  # in the data, but not being plotted, while country is being plotted,
  # the user may select NA for region, but this will lead to the filtering
  # out of all England rows, just leaving the other countries. This is because
  # for England, Region is either the name of a region or 'All', while for the
  # other countries, the correct selection would be NA
  further_selections_with_alls <- further_selections
  for(i in 1:ncol(extra_dropdowns)){
    
    current_column <- as.name(names(extra_dropdowns)[i])
    
    check_for_NA <- further_selections %>% 
      mutate(current_is_na = ifelse(is.na(!!current_column), TRUE, FALSE)) %>% 
      pull(current_is_na)
    
    if(check_for_NA == TRUE){
      replaced_na <- further_selections_with_alls %>% 
        mutate(!!current_column := ifelse(is.na(!!current_column), "All", !!current_column)) 
      
      further_selections_with_alls <- bind_rows(further_selections_with_alls, replaced_na)
    }
    
    check_for_all <- further_selections %>% 
      mutate(current_is_all = ifelse(!!current_column == "All", TRUE, FALSE)) %>% 
      mutate(current_is_all = ifelse(is.na(!!current_column), FALSE, current_is_all)) %>% 
      pull(current_is_all)
    
    if(check_for_all == TRUE){
      replaced_all <- further_selections_with_alls %>% 
        mutate(!!current_column := ifelse(!!current_column == "All", NA, !!current_column)) 
      further_selections_with_alls <- bind_rows(further_selections_with_alls, replaced_all)
      
    }
    
    
    
  }
    
  filtered_data <- filtered_data %>% 
    right_join(further_selections_with_alls, by = names(further_selections)) %>% 
    filter(!is.na(Value))
}

# create the plot:
source("plotting.R")

plot

time_end <- Sys.time()
time_end - time_start

###########
# # test data:
#
# dat <- data.frame(Year = rep("2100", 10),
#                   Country = c(rep("England", 5),
#                               rep("Wales", 2),
#                               "Scotland", 
#                               rep("England", 2)),
#                   Region = c(rep("London", 2),
#                              rep("East", 2),
#                              rep(NA, 6)),
#                   Age = c(rep(c("20 to 29", "30 to 39"), 2),
#                           "20 to 29",
#                           "20 to 29", "30 to 39",
#                           rep(NA, 3)),
#                   Sex = c(rep("M", 7),
#                           rep(NA, 3)),
#                   Industry = c(rep(NA, 8),
#                                "A", "B"))%>% 
#   mutate_if(is.factor, as.character)



