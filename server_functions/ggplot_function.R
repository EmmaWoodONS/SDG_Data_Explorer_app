library(tidyr)
library(ggplot2)
library(dplyr)

#next things- make this function work regardless of how many characteristics the user has chosen.


setwd("server_functions") 

source("config.R")   
source("Filtering.R")
# source("plotting_functions.R")

dat <- read.csv(paste0(csv_filepath, indicator), na.strings=c("","NA")) %>% 
  mutate_if(is.factor, as.character)

filtered_data <- filter_for_selections(dat)

# this is the script for plotting the data
source("if_else_plotting_script.R")





