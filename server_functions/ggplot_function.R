library(tidyr)
library(ggplot2)
library(dplyr)

#next things- make this function work regardless of how many characteristics the user has chosen.


setwd("server_functions") 

source("config.R")   
source("Filtering.R")
source("plotting_functions.R")

dat <- read.csv(paste0(csv_filepath, indicator), na.strings=c("","NA")) %>% 
  mutate_if(is.factor, as.character)

filtered_data <- filter_for_selections(dat)

#if only one facet selection the first function will run, if not the second will run.
if(is.na(facet_row)||is.na(facet_column)){
  test_facet_args(filtered_data,
                  linecolour = line_colour, 
                  facet2 = facet_row)
} else {
  test_facet_args_grid(df = filtered_data,
                       linecolour = line_colour, 
                       facet2 = facet_row,
                       facet1 = facet_column)
}




