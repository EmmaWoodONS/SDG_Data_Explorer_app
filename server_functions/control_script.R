library(tidyr)
library(ggplot2)
library(dplyr)

#next things- make this function work regardless of how many characteristics the user has chosen.

setwd("D:\\coding_repos\\SDG_Data_Explorer_app\\server_functions")

source("config.R")   
source("Filtering.R")
# source("plotting_functions.R")

dat <- read.csv(paste0(csv_filepath, indicator, ".csv"),
                na.strings=c("","NA")) %>% 
  mutate_if(is.factor, as.character)

filtered_data <- filter_for_selections(final_NAs_to_all)

# this is the script for plotting the data
source("plotting_script.R")





