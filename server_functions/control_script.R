library(tidyr)
library(ggplot2)
library(dplyr)
`%not_in%` <- Negate(`%in%`)
#next things- make this function work regardless of how many characteristics the user has chosen.

setwd("D:\\coding_repos\\SDG_Data_Explorer_app\\server_functions")

source("config.R") 
source("identify_complete_nesting.R")
source("Filtering.R")
# source("plotting_functions.R")


time_start <- Sys.time()
dat <- read.csv(paste0(csv_filepath, indicator, ".csv"),
                na.strings=c("","NA")) %>% 
  mutate_if(is.factor, as.character)

filtered_data <- filter_for_selections(dat)
extra_dropdowns <- identify_extra_disaggregations(filtered_data)

# the user will then need to select the required levels from the extra drop-downs 

time_end <- Sys.time()



# this is the script for plotting the data
source("plotting_script.R")





