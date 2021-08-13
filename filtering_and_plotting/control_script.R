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
extra_dropdown_row <- 1
if(is.data.frame(extra_dropdowns)){
  
  further_selections <- extra_dropdowns[extra_dropdown_row, ]
    
  filtered_data <- filtered_data %>% 
    right_join(further_selections, by = names(further_selections))
  
}

# create the plot:
source("plotting.R")

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



